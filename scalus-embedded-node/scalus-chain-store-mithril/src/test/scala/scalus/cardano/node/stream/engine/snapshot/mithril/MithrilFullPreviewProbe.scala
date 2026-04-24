package scalus.cardano.node.stream.engine.snapshot.mithril

import org.scalatest.funsuite.AnyFunSuite
import scalus.cardano.node.stream.engine.snapshot.TestUtils.humanBytes

import java.net.URI
import java.net.http.{HttpClient, HttpRequest, HttpResponse}
import java.nio.file.{Files, Path}
import scala.concurrent.duration.*
import scala.concurrent.{Await, ExecutionContext}

/** Manual one-off probe: downloads the **entire** latest Cardano Database V2 snapshot from the
  * `testing-preview` aggregator and prints size/count/time stats. Useful for answering "how big is
  * it really" without committing to a restorer implementation.
  *
  * '''Not a real test.''' Tagged `[manual]` AND gated on `SCALUS_MITHRIL_FULL_PREVIEW=1`; skips
  * otherwise so `sbt test`, `sbt it`, and CI never invoke it. Expected runtime on preview is tens
  * of minutes and tens of GB of disk; don't run casually.
  *
  * Invoke with:
  * {{{
  *   SCALUS_MITHRIL_FULL_PREVIEW=1 \
  *   SCALUS_MITHRIL_DEST=/tmp/scalus-mithril-preview \
  *     sbt 'scalusChainStoreMithril/testOnly *MithrilFullPreviewProbe'
  * }}}
  *
  * `SCALUS_MITHRIL_DEST` is optional; defaults to a fresh temp dir (which the probe leaves in place
  * so you can inspect it — clean up manually).
  */
final class MithrilFullPreviewProbe extends AnyFunSuite {

    private val aggregatorUrl =
        "https://aggregator.testing-preview.api.mithril.network/aggregator"
    private val genesisVerificationKey =
        "5b3132372c37332c3132342c3136312c362c3133372c3133312c3231332c3230372c3131372c3139382c38" +
            "352c3137362c3139392c3136322c3234312c36382c3132332c3131392c3134352c31332c3233322c3234" +
            "332c34392c3232392c322c3234392c3230352c3230352c33392c3233352c34345d"

    test(
      "[manual] download the full testing-preview snapshot (requires SCALUS_MITHRIL_FULL_PREVIEW=1)"
    ) {
        val enabled = sys.env.get("SCALUS_MITHRIL_FULL_PREVIEW").contains("1")
        assume(enabled, "set SCALUS_MITHRIL_FULL_PREVIEW=1 to run")

        given ExecutionContext = ExecutionContext.global

        val destDir = sys.env
            .get("SCALUS_MITHRIL_DEST")
            .map(Path.of(_))
            .getOrElse(Files.createTempDirectory("scalus-mithril-preview-full-"))
        Files.createDirectories(destDir)
        info(s"destination: $destDir")

        val client = MithrilClient.create(aggregatorUrl, genesisVerificationKey)
        try {
            val t0 = System.nanoTime()
            val snapshots = Await.result(client.listCardanoDatabaseV2Snapshots(), 60.seconds)
            val latest = snapshots.head
            val meta = Await
                .result(client.getCardanoDatabaseV2Snapshot(latest.hash), 60.seconds)
                .getOrElse(fail(s"metadata not found for ${latest.hash}"))

            info(
              s"tip: immutable=${meta.beacon.immutableFileNumber} " +
                  s"epoch=${meta.beacon.epoch} hash=${meta.hash}"
            )
            info(
              s"advertised sizes: " +
                  s"immutables=~${meta.immutables.averageSizeUncompressed}B avg × ${meta.beacon.immutableFileNumber} " +
                  s"(≈${humanBytes(meta.immutables.averageSizeUncompressed * meta.beacon.immutableFileNumber)}), " +
                  s"ancillary=${humanBytes(meta.ancillary.sizeUncompressed)}, " +
                  s"digests=${humanBytes(meta.digests.sizeUncompressed)}, " +
                  s"total_db=${humanBytes(meta.totalDbSizeUncompressed)}"
            )

            // The testing-preview CDN retains only a rolling window of recent immutables (older
            // chunks 403). Resolve the effective lower bound: explicit env var wins, otherwise
            // binary-search with HEAD requests over the immutable URL template.
            val lower = sys.env
                .get("SCALUS_MITHRIL_FROM")
                .map(_.toLong)
                .getOrElse {
                    info("probing CDN for earliest-available immutable (HEAD requests)...")
                    val lo = findEarliestAvailable(meta)
                    info(s"earliest available = $lo (tip = ${meta.beacon.immutableFileNumber})")
                    lo
                }
            val upper = sys.env
                .get("SCALUS_MITHRIL_TO")
                .map(_.toLong)
                .getOrElse(meta.beacon.immutableFileNumber)
            info(s"downloading immutable range [$lower..$upper] (${upper - lower + 1} files)")

            val archiveCount = new java.util.concurrent.atomic.AtomicInteger(0)
            val skippedCount = new java.util.concurrent.atomic.AtomicInteger(0)
            val total = (upper - lower + 1) + 2 // +ancillary +digests
            val layout = Await.result(
              client.downloadCardanoDatabaseV2(
                meta,
                destDir,
                onProgress = p => {
                    val done = archiveCount.incrementAndGet()
                    if p.skipped then skippedCount.incrementAndGet()
                    if done % 50 == 0 || done == total then
                        info(
                          s"progress: $done / $total archives " +
                              s"(skipped=${skippedCount.get}) — latest stage=${p.stage}"
                        )
                },
                maxConcurrent = 8,
                immutableRange = MithrilClient.ImmutableFileRange.Range(lower, upper)
              ),
              12.hours
            )
            val elapsed = (System.nanoTime() - t0) / 1_000_000L

            val onDisk = dirSize(destDir)
            info(
              s"=== DONE in ${elapsed / 1000}s ===\n" +
                  s"  immutableRange = [${layout.immutableRange._1}..${layout.immutableRange._2}]\n" +
                  s"  immutableCount = ${layout.immutableCount}\n" +
                  s"  archives processed = ${archiveCount.get} (of $total)\n" +
                  s"  skipped (already extracted) = ${skippedCount.get}\n" +
                  s"  extracted immutable files listed = ${layout.immutableFiles.size}\n" +
                  s"  extracted ancillary files listed = ${layout.ancillaryFiles.size}\n" +
                  s"  extracted digests files listed = ${layout.digestsFiles.size}\n" +
                  s"  total on-disk size = ${humanBytes(onDisk)} ($onDisk bytes)\n" +
                  s"  throughput = ${humanBytes((onDisk * 1000L) / math.max(elapsed, 1L))}/s"
            )
        } finally client.close()
    }

    /** HEAD a candidate URL, return true on 2xx. 403 / 404 means "not retained". */
    private def headOk(http: HttpClient, url: String): Boolean = {
        val req = HttpRequest
            .newBuilder(URI.create(url))
            .method("HEAD", HttpRequest.BodyPublishers.noBody())
            .build()
        val resp = http.send(req, HttpResponse.BodyHandlers.discarding())
        resp.statusCode / 100 == 2
    }

    /** Binary-search the rolling-retention window: smallest `n` in `[1..tip]` whose `immutable-N`
      * URL responds 2xx. O(log N) HEAD requests.
      */
    private def findEarliestAvailable(meta: MithrilMessages.CardanoDatabaseV2Metadata): Long = {
        val http = HttpClient.newHttpClient()
        val tip = meta.beacon.immutableFileNumber
        var lo = 1L
        var hi = tip
        while lo < hi do {
            val mid = lo + (hi - lo) / 2
            if headOk(http, MithrilClient.immutableUrl(meta, mid)) then hi = mid
            else lo = mid + 1
        }
        lo
    }

    private def dirSize(p: Path): Long = {
        import scala.jdk.CollectionConverters.*
        val s = Files.walk(p)
        try s.iterator.asScala.filter(Files.isRegularFile(_)).map(Files.size).sum
        finally s.close()
    }

}
