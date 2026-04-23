package scalus.cardano.node.stream.engine.snapshot.mithril

import com.github.luben.zstd.ZstdInputStream
import com.github.plokhotnyuk.jsoniter_scala.core.{readFromString, JsonValueCodec}
import org.apache.commons.compress.archivers.tar.TarArchiveInputStream

import java.net.URI
import java.net.http.{HttpClient, HttpRequest, HttpResponse}
import java.nio.file.{Files, Path, StandardCopyOption}
import java.time.Duration
import scala.concurrent.{ExecutionContext, Future}

/** User-facing entry point for the Mithril-verified snapshot acquisition path.
  *
  * Splits responsibilities between the embedded WASM client and the JVM:
  *
  *   - **WASM**: cryptographic verification — certificate chain walk + MuSig2 threshold signature.
  *     Drives through `mithrilclient_new` + `verify_certificate_chain` on the pinned
  *     `@mithril-dev/mithril-client-wasm@0.10.4` blob. Metadata listing (`list_certificates`,
  *     `list_cardano_database_snapshots`, `get_cardano_database_v2_snapshot`) also routes through
  *     the WASM so the caller can demonstrate they're talking to the same aggregator the verifier
  *     will check against, though the JSON wire shape is decoded on our side.
  *   - **JVM**: HTTP file transport (`immutable-{N}.tar.zst`, `ancillary.tar.zst`,
  *     `digests.tar.zst`) via `java.net.http.HttpClient`, plus zstd + tar extraction via `zstd-jni`
  *     + `commons-compress`. The WASM doesn't compile the upstream `fs` feature (and
  *     `wasm32-unknown-unknown` has no filesystem anyway), so we do this half directly.
  *
  * All WASM calls serialise onto `MithrilAsyncRuntime`'s single dispatcher thread. HTTP calls run
  * on the supplied `ExecutionContext`.
  *
  * {{{
  * val client = MithrilClient.create(
  *     aggregatorUrl = "https://aggregator.testing-preview.api.mithril.network/aggregator",
  *     genesisVerificationKey = "5b3132372c37332c…"
  * )
  * try {
  *     val latest = Await.result(client.listCardanoDatabaseV2Snapshots(), 30.seconds).head
  *     val meta = Await.result(client.getCardanoDatabaseV2Snapshot(latest.hash), 30.seconds).get
  *     val files = Await.result(client.downloadImmutable(meta, latest.beacon.immutable_file_number, workDir), 60.seconds)
  * } finally client.close()
  * }}}
  */
final class MithrilClient private (
    private val runtime: MithrilWasmRuntime,
    private val asyncRuntime: MithrilAsyncRuntime,
    private val clientPtr: Long,
    private val httpClient: HttpClient
)(using ec: ExecutionContext) {

    import MithrilMessages.*

    /** `mithrilclient_list_mithril_certificates` — latest-first. */
    def listCertificates(): Future[Seq[MithrilCertificateListItem]] =
        callAsyncJson[Seq[MithrilCertificateListItem]]("mithrilclient_list_mithril_certificates")

    /** `mithrilclient_list_cardano_database_v2` — latest-first. */
    def listCardanoDatabaseV2Snapshots(): Future[Seq[CardanoDatabaseV2ListItem]] =
        callAsyncJson[Seq[CardanoDatabaseV2ListItem]]("mithrilclient_list_cardano_database_v2")

    /** `mithrilclient_get_cardano_database_v2` — full metadata including download `locations`. */
    def getCardanoDatabaseV2Snapshot(hash: String): Future[Option[CardanoDatabaseV2Metadata]] =
        withHashArg("mithrilclient_get_cardano_database_v2", hash)
            .flatMap(asyncRuntime.awaitPromise(_)(optionalJsonDecode[CardanoDatabaseV2Metadata]))

    /** Download one `immutable-{N}.tar.zst` file named by `meta.immutables.locations`, expand it
      * under `destDir` (which must exist), and return the paths of extracted files. Currently picks
      * the first `CloudStorage` location with a non-empty `Template` URI.
      */
    def downloadImmutable(
        meta: CardanoDatabaseV2Metadata,
        immutableFileNumber: Long,
        destDir: Path
    ): Future[Seq[Path]] = Future {
        val url = meta.immutables.locations
            .collectFirst {
                case ImmutablesLocation.CloudStorage(tmpl, _) if tmpl.Template.isDefined =>
                    tmpl.resolve(immutableFileNumber).get
            }
            .getOrElse(
              throw new IllegalStateException(
                "no CloudStorage Template URI in immutables.locations — " +
                    "aggregator returned unexpected shape"
              )
            )
        val archive = httpGet(url)
        extractTarZst(archive, destDir)
    }

    /** Release the underlying WASM runtime + dispatcher. Subsequent calls will fail. */
    def close(): Unit = asyncRuntime.close()

    // -------- internals --------

    private def callAsyncJson[T: JsonValueCodec](exportName: String): Future[T] =
        asyncRuntime
            .submit { _ => runtime.exportFn(exportName).apply(clientPtr)(0).toInt }
            .flatMap(asyncRuntime.awaitPromise(_)(jsonDecode[T]))

    private def withHashArg(exportName: String, hash: String): Future[Int] =
        asyncRuntime.submit { _ =>
            val (ptr, len) = runtime.passString(hash)
            runtime
                .exportFn(exportName)
                .apply(clientPtr, ptr.toLong, len.toLong)(0)
                .toInt
        }

    private def jsonDecode[T: JsonValueCodec](v: AnyRef | Null): T = {
        val json = WbindgenAbi.jsonStringify(v)
        readFromString[T](json)
    }

    private def optionalJsonDecode[T: JsonValueCodec](v: AnyRef | Null): Option[T] = v match {
        case null                  => None
        case WbindgenAbi.Undefined => None
        case other                 => Some(jsonDecode[T](other))
    }

    private def httpGet(url: String): Array[Byte] = {
        val req = HttpRequest
            .newBuilder(URI.create(url))
            .timeout(Duration.ofSeconds(MithrilClient.DownloadTimeoutSeconds))
            .GET()
            .build()
        val resp = httpClient.send(req, HttpResponse.BodyHandlers.ofByteArray())
        if resp.statusCode != 200 then throw new RuntimeException(s"GET $url → ${resp.statusCode}")
        resp.body
    }

    private def extractTarZst(bytes: Array[Byte], destDir: Path): Seq[Path] = {
        Files.createDirectories(destDir)
        val written = scala.collection.mutable.ArrayBuffer.empty[Path]
        val zin = new ZstdInputStream(new java.io.ByteArrayInputStream(bytes))
        val tin = new TarArchiveInputStream(zin)
        try {
            var e = tin.getNextEntry
            while e != null do {
                if !e.isDirectory then {
                    val rel = sanitiseTarName(e.getName)
                    val out = destDir.resolve(rel)
                    Files.createDirectories(out.getParent)
                    Files.copy(tin, out, StandardCopyOption.REPLACE_EXISTING)
                    written += out
                }
                e = tin.getNextEntry
            }
        } finally {
            tin.close()
            zin.close()
        }
        written.toSeq
    }

    /** Reject absolute / parent-escape entries — a cardano-node immutable tar should only ever
      * contain `immutable/NNNN.*` paths, but a hostile aggregator could ship `../../etc/passwd`.
      * Strip absolute prefixes and bail on `..` components.
      */
    private def sanitiseTarName(name: String): String = {
        val p = java.nio.file.Paths.get(name).normalize()
        require(!p.isAbsolute && !p.startsWith(".."), s"unsafe tar entry: $name")
        p.toString
    }
}

object MithrilClient {

    /** Timeout for per-file snapshot downloads. An immutable chunk is ~500 KB compressed; the
      * ancillary tar can approach 1 GB on mainnet and may need a higher ceiling.
      */
    val DownloadTimeoutSeconds: Long = 600L

    /** Construct a client around a fresh WASM instance wired to `aggregatorUrl`. Caller owns the
      * returned client and must [[MithrilClient.close]] it when done.
      */
    def create(
        aggregatorUrl: String,
        genesisVerificationKey: String,
        hashes: MithrilAsyncRuntime.ClosureHashes = MithrilAsyncRuntime.ClosureHashes.Release0_10_4
    )(using ec: ExecutionContext): MithrilClient = {
        val abi = new WbindgenAbi(hashes)
        val asyncRt = new MithrilAsyncRuntime(abi, hashes)
        val imports = abi.defaultImports ++ abi.pinnedImports ++ asyncRt.asyncImports
        val (rt, _) = MithrilWasmRuntime.instantiate(imports)
        asyncRt.attach(rt.instance)

        val (aggPtr, aggLen) = rt.passString(aggregatorUrl)
        val (keyPtr, keyLen) = rt.passString(genesisVerificationKey)
        val clientPtr = rt
            .exportFn("mithrilclient_new")
            .apply(
              aggPtr.toLong,
              aggLen.toLong,
              keyPtr.toLong,
              keyLen.toLong,
              1L
            )(0)

        new MithrilClient(rt, asyncRt, clientPtr, HttpClient.newHttpClient())
    }
}
