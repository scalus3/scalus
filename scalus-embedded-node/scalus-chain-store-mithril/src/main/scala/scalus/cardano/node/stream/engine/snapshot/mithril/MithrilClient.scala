package scalus.cardano.node.stream.engine.snapshot.mithril

import com.github.luben.zstd.ZstdInputStream
import com.github.plokhotnyuk.jsoniter_scala.core.{readFromString, JsonValueCodec}
import org.apache.commons.compress.archivers.tar.TarArchiveInputStream

import java.io.InputStream
import java.net.URI
import java.net.http.{HttpClient, HttpRequest, HttpResponse}
import java.nio.file.{Files, Path, StandardCopyOption}
import java.time.Duration
import java.util.concurrent.Semaphore
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
  *     val files = Await.result(client.downloadImmutable(meta, latest.beacon.immutableFileNumber, workDir), 60.seconds)
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
        val url = immutableUrl(meta, immutableFileNumber)
        httpGetStream(url) { in => extractTarZst(in, destDir) }
    }

    /** Bulk-download the full Cardano Database V2 artifact referenced by `meta`: every
      * `immutable-{1..N}.tar.zst` chunk, plus `ancillary.tar.zst` (ledger state + the last
      * immutable) and `digests.tar.zst` (per-file hashes for later Merkle verification). Each
      * archive is streamed directly from HTTP into the zstd+tar extractor — no multi-GB byte arrays
      * are held in memory.
      *
      * Layout on disk mirrors cardano-node's: `destDir/immutable/{n}.chunk|primary|secondary`,
      * `destDir/ledger/...` (from ancillary), plus whatever the digests archive contains.
      *
      * **Resumability.** An archive is skipped when a sibling marker file
      * `<name>.tar.zst.extracted` already exists next to the extracted output — we drop the marker
      * only after extraction finishes cleanly, so a crash mid-extract forces a fresh re-download on
      * the next run.
      *
      * **Concurrency.** Up to `maxConcurrent` archives are downloaded in parallel on the supplied
      * `ExecutionContext`. Per-archive IO remains sequential (one HTTP connection, one zstd+tar
      * stream). Default 4 matches what `mithril-client` uses as its soft default.
      *
      * **Progress.** `onProgress` fires once per archive as it completes, with the archive's stage
      * label and byte counts. Byte counts reflect what the server advertised in
      * `meta.{immutables.averageSizeUncompressed, ancillary.sizeUncompressed, digests.sizeUncompressed}`
      * rather than metered HTTP bytes — cheap to surface, good enough for UX.
      *
      * No Merkle root verification yet — that lands with the cert-chain walk in a follow-up.
      */
    def downloadCardanoDatabaseV2(
        meta: CardanoDatabaseV2Metadata,
        destDir: Path,
        onProgress: MithrilClient.DownloadProgress => Unit = _ => (),
        maxConcurrent: Int = MithrilClient.DefaultMaxConcurrent,
        immutableRange: MithrilClient.ImmutableFileRange = MithrilClient.ImmutableFileRange.Full
    ): Future[MithrilClient.CardanoDatabaseV2Layout] = {
        require(maxConcurrent >= 1, s"maxConcurrent must be >= 1, got $maxConcurrent")
        Files.createDirectories(destDir)
        val total = meta.beacon.immutableFileNumber
        require(total >= 1, s"beacon.immutableFileNumber must be >= 1, got $total")
        val range = immutableRange.resolve(total)

        val ancillaryUrl0 = meta.ancillary.locations.collectFirst {
            case AncillaryLocation.CloudStorage(uri, _) => uri
        }
        val digestsUrl0 = meta.digests.locations.collectFirst {
            case DigestLocation.CloudStorage(uri, _) => uri
            case DigestLocation.Aggregator(uri)      => uri
        }

        val immutableJobs = (range.start to range.end).map { n =>
            MithrilClient.ArchiveJob(
              stage = s"immutable-$n",
              url = immutableUrl(meta, n),
              advertisedSize = Some(meta.immutables.averageSizeUncompressed)
            )
        }
        val ancillaryJob = ancillaryUrl0.map { u =>
            MithrilClient.ArchiveJob("ancillary", u, Some(meta.ancillary.sizeUncompressed))
        }
        val digestsJob = digestsUrl0.map { u =>
            MithrilClient.ArchiveJob("digests", u, Some(meta.digests.sizeUncompressed))
        }
        val allJobs = (immutableJobs ++ ancillaryJob ++ digestsJob).toVector

        val sem = new Semaphore(maxConcurrent)
        val perJob = allJobs.map { job =>
            Future {
                sem.acquire()
                try runArchiveJob(job, destDir, onProgress)
                finally sem.release()
            }
        }
        Future.sequence(perJob).map { written =>
            val byStage = allJobs.zip(written).toMap
            MithrilClient.CardanoDatabaseV2Layout(
              root = destDir,
              immutableRange = (range.start, range.end),
              immutableFiles = immutableJobs.flatMap(byStage(_)),
              ancillaryFiles = ancillaryJob.map(byStage(_)).getOrElse(Seq.empty),
              digestsFiles = digestsJob.map(byStage(_)).getOrElse(Seq.empty)
            )
        }
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

    private def immutableUrl(meta: CardanoDatabaseV2Metadata, n: Long): String =
        MithrilClient.immutableUrl(meta, n)

    /** Download one archive (idempotent via sibling `.extracted` marker), stream-extract it into
      * `destDir`, fire `onProgress` with the advertised size, return the extracted paths. Caller
      * holds the concurrency permit.
      */
    private def runArchiveJob(
        job: MithrilClient.ArchiveJob,
        destDir: Path,
        onProgress: MithrilClient.DownloadProgress => Unit
    ): Seq[Path] = {
        val markerName = job.stage + ".extracted"
        val marker = destDir.resolve(markerName)
        if Files.exists(marker) then {
            onProgress(
              MithrilClient.DownloadProgress(job.stage, 0L, job.advertisedSize, skipped = true)
            )
            return Seq.empty
        }
        val written = httpGetStream(job.url) { in => extractTarZst(in, destDir) }
        Files.writeString(marker, job.url)
        onProgress(
          MithrilClient.DownloadProgress(
            job.stage,
            job.advertisedSize.getOrElse(0L),
            job.advertisedSize,
            skipped = false
          )
        )
        written
    }

    private def httpGetStream[T](url: String)(f: InputStream => T): T = {
        val req = HttpRequest
            .newBuilder(URI.create(url))
            .timeout(Duration.ofSeconds(MithrilClient.DownloadTimeoutSeconds))
            .GET()
            .build()
        val resp = httpClient.send(req, HttpResponse.BodyHandlers.ofInputStream())
        if resp.statusCode != 200 then {
            // Drain up to 4 KB of the body so the caller sees *why* — a 403 from the CDN
            // typically carries an XML <Error><Code>…</Code><Message>…</Message></Error> payload,
            // and a plain `→ 403` is almost useless without it. Cap the read so we don't slurp a
            // multi-MB HTML error page into the exception string.
            val snippet = scala.util
                .Try {
                    val buf = resp.body.readNBytes(4096)
                    new String(buf, java.nio.charset.StandardCharsets.UTF_8)
                }
                .getOrElse("<unavailable>")
            scala.util.Try(resp.body.close())
            throw new RuntimeException(
              s"GET $url → ${resp.statusCode}; body: ${snippet.take(4096)}"
            )
        }
        val body = resp.body
        try f(body)
        finally body.close()
    }

    private def extractTarZst(in: InputStream, destDir: Path): Seq[Path] = {
        Files.createDirectories(destDir)
        val written = scala.collection.mutable.ArrayBuffer.empty[Path]
        val zin = new ZstdInputStream(in)
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

    /** Default parallelism for [[MithrilClient.downloadCardanoDatabaseV2]] — matches upstream
      * `mithril-client`'s default; bounded so we don't saturate the CDN or the host's file handles.
      */
    val DefaultMaxConcurrent: Int = 4

    /** Progress record emitted by [[MithrilClient.downloadCardanoDatabaseV2]] as each archive
      * finishes (or is skipped because a prior run's marker is present).
      *
      * @param stage
      *   `"immutable-N"`, `"ancillary"`, or `"digests"` — stable enough to key resumability
      *   markers.
      * @param bytesDownloaded
      *   advertised-size bytes attributed to this archive (0 if skipped).
      * @param totalExpected
      *   the aggregator's announced size, when available.
      * @param skipped
      *   `true` when the `.extracted` marker shortcut fired.
      */
    final case class DownloadProgress(
        stage: String,
        bytesDownloaded: Long,
        totalExpected: Option[Long],
        skipped: Boolean
    )

    /** Output of [[MithrilClient.downloadCardanoDatabaseV2]] — enumerates what was extracted so
      * downstream parsers can iterate without scanning the directory.
      *
      * Note: when a previous run already extracted an archive, the corresponding paths are
      * **omitted** from these lists — the `.extracted` marker skips re-materialisation and we don't
      * pay the cost of re-walking the tar just to rebuild the path list. Callers that need the full
      * on-disk inventory should scan `root` directly.
      */
    final case class CardanoDatabaseV2Layout(
        root: Path,
        immutableRange: (Long, Long),
        immutableFiles: Seq[Path],
        ancillaryFiles: Seq[Path],
        digestsFiles: Seq[Path]
    ) {
        def immutableCount: Long = immutableRange._2 - immutableRange._1 + 1
    }

    /** Selects which immutable chunks of a snapshot to pull. Mirrors upstream Mithril's
      * `ImmutableFileRange`. Indices are inclusive and 1-based to match cardano-node's immutable
      * file numbering. `Full` is the "download everything" default; the others are needed when a
      * CDN retains only a rolling window of recent immutables (as `testing-preview` does), or for
      * test fixtures / incremental restores.
      */
    sealed trait ImmutableFileRange {
        def resolve(last: Long): ResolvedRange = this match {
            case ImmutableFileRange.Full        => ResolvedRange(1L, last)
            case ImmutableFileRange.From(from)  => ResolvedRange(from, last)
            case ImmutableFileRange.UpTo(to)    => ResolvedRange(1L, to)
            case ImmutableFileRange.Range(a, b) => ResolvedRange(a, b)
        }
    }
    object ImmutableFileRange {
        case object Full extends ImmutableFileRange
        final case class From(from: Long) extends ImmutableFileRange
        final case class UpTo(to: Long) extends ImmutableFileRange
        final case class Range(from: Long, to: Long) extends ImmutableFileRange
    }

    /** Internal inclusive-bounds range descriptor returned by `ImmutableFileRange.resolve`. */
    final case class ResolvedRange(start: Long, end: Long) {
        require(start >= 1L && end >= start, s"invalid immutable range [$start..$end]")
    }

    private final case class ArchiveJob(
        stage: String,
        url: String,
        advertisedSize: Option[Long]
    )

    /** Resolve the HTTP URL for a single immutable-chunk archive by substituting
      * `{immutable_file_number}` in the first `CloudStorage` template in `meta.immutables`.
      */
    def immutableUrl(meta: MithrilMessages.CardanoDatabaseV2Metadata, n: Long): String =
        meta.immutables.locations
            .collectFirst {
                case MithrilMessages.ImmutablesLocation.CloudStorage(tmpl, _)
                    if tmpl.template.isDefined =>
                    tmpl.resolve(n).get
            }
            .getOrElse(
              throw new IllegalStateException(
                "no CloudStorage Template URI in immutables.locations — " +
                    "aggregator returned unexpected shape"
              )
            )

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

    /** Build a client whose only live dependency is the JVM `HttpClient` — no WASM, no
      * async-runtime dispatcher. Intended for unit-testing the HTTP + tar+zstd path without
      * standing up the full cryptographic client. `close()` on the returned instance is a no-op
      * because there's nothing to release (calling the WASM/dispatcher-bound methods will NPE).
      */
    private[mithril] def forHttpOnly(httpClient: HttpClient)(using
        ExecutionContext
    ): MithrilClient =
        new MithrilClient(null, null, 0L, httpClient)
}
