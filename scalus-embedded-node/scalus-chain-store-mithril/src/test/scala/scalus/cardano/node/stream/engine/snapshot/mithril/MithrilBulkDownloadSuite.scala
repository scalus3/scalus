package scalus.cardano.node.stream.engine.snapshot.mithril

import com.github.luben.zstd.ZstdOutputStream
import com.sun.net.httpserver.{HttpExchange, HttpHandler, HttpServer}
import org.apache.commons.compress.archivers.tar.{TarArchiveEntry, TarArchiveOutputStream}
import org.scalatest.funsuite.AnyFunSuite
import scalus.cardano.node.stream.engine.snapshot.TestUtils

import java.io.ByteArrayOutputStream
import java.net.InetSocketAddress
import java.nio.file.{Files, Path}
import java.util.concurrent.atomic.AtomicInteger
import scala.collection.mutable
import scala.concurrent.duration.*
import scala.concurrent.{Await, ExecutionContext}

/** Exercises [[MithrilClient.downloadCardanoDatabaseV2]] against an in-process HTTP server that
  * serves synthetic `.tar.zst` archives — no aggregator, no WASM, no network. Covers:
  *
  *   - fan-out of N+2 downloads (N immutables + ancillary + digests),
  *   - progress callback shape (one event per job, with the skipped flag),
  *   - idempotent re-run via the `.extracted` marker,
  *   - `CardanoDatabaseV2Layout` population.
  */
final class MithrilBulkDownloadSuite extends AnyFunSuite {

    import MithrilBulkDownloadSuite.*

    test("downloadCardanoDatabaseV2 fans out over all archives, returns layout") {
        given ExecutionContext = ExecutionContext.global

        val server = startFixtureServer(immutableCount = 3)
        try {
            val client = stubClient()
            try {
                val destDir = Files.createTempDirectory("scalus-mithril-bulk-")
                val progress = mutable.Buffer.empty[MithrilClient.DownloadProgress]
                val layout = Await.result(
                  client.downloadCardanoDatabaseV2(
                    meta = server.meta,
                    destDir = destDir,
                    onProgress = p => progress.synchronized { progress += p }
                  ),
                  30.seconds
                )
                try {
                    assert(layout.immutableCount == 3)
                    assert(layout.immutableFiles.nonEmpty, "expected immutable files populated")
                    assert(layout.ancillaryFiles.nonEmpty, "expected ancillary files populated")
                    assert(layout.digestsFiles.nonEmpty, "expected digests files populated")

                    for n <- 1 to 3; ext <- Seq("chunk", "primary", "secondary") do
                        assert(
                          Files.exists(destDir.resolve(s"immutable/$n.$ext")),
                          s"$n.$ext missing"
                        )
                    assert(Files.exists(destDir.resolve("ledger/state")))
                    assert(Files.exists(destDir.resolve("digests/digests.json")))

                    val stages = progress.map(_.stage).toSet
                    assert(
                      stages == Set(
                        "immutable-1",
                        "immutable-2",
                        "immutable-3",
                        "ancillary",
                        "digests"
                      )
                    )
                    assert(progress.forall(!_.skipped), "first run shouldn't skip anything")
                } finally cleanup(destDir)
            } finally () // stub client has no WASM runtime to close
        } finally server.stop()
    }

    test("rerun is idempotent via .extracted marker") {
        given ExecutionContext = ExecutionContext.global

        val server = startFixtureServer(immutableCount = 2)
        try {
            val client = stubClient()
            try {
                val destDir = Files.createTempDirectory("scalus-mithril-bulk-rerun-")
                try {
                    Await.result(
                      client.downloadCardanoDatabaseV2(server.meta, destDir),
                      30.seconds
                    )
                    val firstHits = server.hits.get()

                    val progress = mutable.Buffer.empty[MithrilClient.DownloadProgress]
                    Await.result(
                      client.downloadCardanoDatabaseV2(
                        server.meta,
                        destDir,
                        onProgress = p => progress.synchronized { progress += p }
                      ),
                      30.seconds
                    )
                    assert(
                      server.hits.get() == firstHits,
                      "second run must not re-download anything"
                    )
                    assert(
                      progress.forall(_.skipped),
                      s"second run should report skipped=true; got $progress"
                    )
                } finally cleanup(destDir)
            } finally () // stub client has no WASM runtime to close
        } finally server.stop()
    }
}

object MithrilBulkDownloadSuite {

    import MithrilMessages.*

    /** HTTP-only stub — bypasses WASM init since these tests only exercise the download + extract
      * path. See `MithrilClient.forHttpOnly` for the contract.
      */
    private def stubClient()(using ExecutionContext): MithrilClient =
        MithrilClient.forHttpOnly(java.net.http.HttpClient.newHttpClient())

    private def cleanup(dir: Path): Unit = TestUtils.deleteRecursively(dir)

    final case class FixtureServer(
        port: Int,
        meta: CardanoDatabaseV2Metadata,
        hits: AtomicInteger,
        private val http: HttpServer
    ) {
        def stop(): Unit = http.stop(0)
    }

    private def startFixtureServer(immutableCount: Int): FixtureServer = {
        val hits = new AtomicInteger(0)
        val server = HttpServer.create(new InetSocketAddress("127.0.0.1", 0), 0)
        server.createContext(
          "/",
          new HttpHandler {
              def handle(ex: HttpExchange): Unit = {
                  hits.incrementAndGet()
                  val path = ex.getRequestURI.getPath
                  val bytes = path match {
                      case p if p.startsWith("/immutable/") =>
                          val n = p.stripPrefix("/immutable/").stripSuffix(".tar.zst").toLong
                          makeTarZst(
                            Seq(
                              s"immutable/$n.chunk" -> s"chunk-$n".getBytes,
                              s"immutable/$n.primary" -> s"primary-$n".getBytes,
                              s"immutable/$n.secondary" -> s"secondary-$n".getBytes
                            )
                          )
                      case "/ancillary.tar.zst" =>
                          makeTarZst(Seq("ledger/state" -> "ledger-bytes".getBytes))
                      case "/digests.tar.zst" =>
                          makeTarZst(Seq("digests/digests.json" -> "{}".getBytes))
                      case other =>
                          ex.sendResponseHeaders(404, 0); ex.close(); return
                  }
                  ex.sendResponseHeaders(200, bytes.length.toLong)
                  ex.getResponseBody.write(bytes)
                  ex.close()
              }
          }
        )
        server.start()
        val port = server.getAddress.getPort
        val base = s"http://127.0.0.1:$port"
        val meta = CardanoDatabaseV2Metadata(
          hash = "fixture",
          merkleRoot = "fixture",
          network = "testnet",
          beacon = CardanoDbBeacon(epoch = 1, immutableFileNumber = immutableCount.toLong),
          certificateHash = "fixture",
          totalDbSizeUncompressed = 0L,
          digests = DigestsLocations(
            sizeUncompressed = 16L,
            locations = Seq(DigestLocation.CloudStorage(s"$base/digests.tar.zst", None))
          ),
          immutables = ImmutablesLocations(
            averageSizeUncompressed = 32L,
            locations = Seq(
              ImmutablesLocation.CloudStorage(
                MultiFilesUri(Some(s"$base/immutable/{immutable_file_number}.tar.zst")),
                None
              )
            )
          ),
          ancillary = AncillaryLocations(
            sizeUncompressed = 64L,
            locations = Seq(AncillaryLocation.CloudStorage(s"$base/ancillary.tar.zst", None))
          ),
          cardanoNodeVersion = "test",
          createdAt = "now"
        )
        FixtureServer(port, meta, hits, server)
    }

    private def makeTarZst(entries: Seq[(String, Array[Byte])]): Array[Byte] = {
        val baos = new ByteArrayOutputStream()
        val zos = new ZstdOutputStream(baos)
        val tos = new TarArchiveOutputStream(zos)
        try {
            entries.foreach { case (name, data) =>
                val e = new TarArchiveEntry(name)
                e.setSize(data.length.toLong)
                tos.putArchiveEntry(e)
                tos.write(data)
                tos.closeArchiveEntry()
            }
            tos.finish()
        } finally {
            tos.close()
            zos.close()
        }
        baos.toByteArray
    }
}
