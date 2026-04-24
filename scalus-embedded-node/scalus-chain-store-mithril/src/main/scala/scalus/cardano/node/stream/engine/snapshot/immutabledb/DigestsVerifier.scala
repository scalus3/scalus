package scalus.cardano.node.stream.engine.snapshot.immutabledb

import com.github.plokhotnyuk.jsoniter_scala.core.*
import com.github.plokhotnyuk.jsoniter_scala.macros.{CodecMakerConfig, JsonCodecMaker}
import scalus.utils.Hex.toHex

import java.nio.file.{Files, Path}
import java.security.MessageDigest

/** Per-file SHA-256 verification against the digests manifest shipped in Mithril Cardano DB V2
  * `digests.tar.zst`. Mirrors upstream Rust's `CardanoDatabaseDigestListMessage`: an array of
  * `{ immutable_file_name, digest }` entries where `digest` is a lowercase-hex SHA-256.
  *
  * This is file-level integrity only — it catches corruption and truncation but does NOT prove the
  * files match what Mithril signers actually signed. The latter needs a Merkle root of these
  * leaves, checked against the certificate's signed message via the WASM verifier. That's a
  * follow-up; until then, treat `DigestsVerifier.verify` as a "what I have on disk matches the
  * manifest that came with the snapshot" check, not a cryptographic authenticity proof.
  */
object DigestsVerifier {

    final case class DigestEntry(immutableFileName: String, digest: String)

    /** Parse result — we keep entries as a map for O(1) name-lookup during verification. */
    final case class DigestManifest(entries: Map[String, String]) {
        def get(name: String): Option[String] = entries.get(name)
        def size: Int = entries.size
    }

    /** One mismatch observed during verification. */
    final case class Mismatch(fileName: String, expectedHex: String, actualHex: String)

    /** Outcome of a verification run.
      *
      *   - `verified` — files on disk whose SHA-256 matched the manifest.
      *   - `mismatches` — files whose SHA-256 didn't match — always a hard failure.
      *   - `missingOnDisk` — manifest entries we couldn't find; usually means the CDN aged out the
      *     archives before we downloaded them, not corruption.
      *   - `unexpectedOnDisk` — files in the dir not in the manifest. Common artefacts from
      *     `ancillary.tar.zst` (it ships a chunk past the signed tip) land here benignly.
      */
    final case class VerificationResult(
        verified: Int,
        mismatches: Seq[Mismatch],
        missingOnDisk: Seq[String],
        unexpectedOnDisk: Seq[String]
    ) {

        /** True when everything we *do* have on disk matches the manifest — the normal gate for
          * snapshot integrity on a partial (window-limited) download.
          */
        def presentMatchesManifest: Boolean = mismatches.isEmpty

        /** True when the manifest is fully covered on disk. Stricter than
          * [[presentMatchesManifest]]; only useful after a full (not window-limited) download.
          */
        def isComplete: Boolean = presentMatchesManifest && missingOnDisk.isEmpty
    }

    private given JsonValueCodec[DigestEntry] = JsonCodecMaker.make(
      CodecMakerConfig
          .withSkipUnexpectedFields(true)
          .withFieldNameMapper(JsonCodecMaker.enforce_snake_case2)
    )
    private given JsonValueCodec[Seq[DigestEntry]] = JsonCodecMaker.make(
      CodecMakerConfig
          .withSkipUnexpectedFields(true)
          .withFieldNameMapper(JsonCodecMaker.enforce_snake_case2)
    )

    /** Load a digest manifest from a JSON file produced by `digests.tar.zst` extraction. Duplicate
      * entries (same file_name) are resolved "last wins" — shouldn't happen in valid Mithril
      * output, but tolerating it keeps us forward-compatible with future aggregator changes.
      */
    def loadManifest(jsonPath: Path): DigestManifest = {
        val arr = readFromArray[Seq[DigestEntry]](Files.readAllBytes(jsonPath))
        DigestManifest(arr.iterator.map(e => e.immutableFileName -> e.digest).toMap)
    }

    /** Find and load the digests-manifest JSON that Mithril's `digests.tar.zst` extracts. The
      * archive ships a single filename-tagged JSON (e.g. `preview-e1277-i25543.digests.json`), and
      * in practice puts it at the snapshot root — not under a `digests/` subdir. We search the
      * given directory for any `*digests*.json`; fallback to `*.json` if none match.
      *
      * Accepts either the snapshot root or a legacy `digests/` subdir — whichever you hand it,
      * first `.json` wins.
      */
    def loadManifestFromDir(dir: Path): Path = {
        import scala.jdk.CollectionConverters.*
        val all = scala.util.Using.resource(Files.list(dir)) { s =>
            s.iterator.asScala.filter(p => p.getFileName.toString.endsWith(".json")).toVector
        }
        val preferred = all.find(_.getFileName.toString.contains("digests"))
        preferred
            .orElse(all.headOption)
            .getOrElse(
              throw new IllegalArgumentException(s"no *digests*.json or *.json in $dir")
            )
    }

    /** Load a manifest discovered under `dir` via [[loadManifestFromDir]]. */
    def loadManifestAt(dir: Path): DigestManifest = loadManifest(loadManifestFromDir(dir))

    /** Hash every file under `immutableDir` and compare against the manifest. Returns a structured
      * result so callers can decide how to surface partial failure (some apps tolerate a
      * tail-missing chunk because a concurrent run was still extracting).
      */
    def verify(immutableDir: Path, manifest: DigestManifest): VerificationResult = {
        import scala.jdk.CollectionConverters.*
        val mismatches = collection.mutable.ArrayBuffer.empty[Mismatch]
        val onDisk = collection.mutable.Set.empty[String]
        var verified = 0
        scala.util.Using.resource(Files.list(immutableDir)) { stream =>
            stream.iterator.asScala.foreach { p =>
                if Files.isRegularFile(p) then {
                    val name = p.getFileName.toString
                    onDisk += name
                    manifest.get(name) match {
                        case Some(expected) =>
                            val actual = sha256Hex(p)
                            if !actual.equalsIgnoreCase(expected) then
                                mismatches += Mismatch(name, expected, actual)
                            else verified += 1
                        case None => // unexpectedOnDisk handled below
                    }
                }
            }
        }
        val missing = manifest.entries.keySet.diff(onDisk).toVector.sorted
        val unexpected = onDisk.diff(manifest.entries.keySet).toVector.sorted
        VerificationResult(verified, mismatches.toSeq, missing, unexpected)
    }

    /** Hex-encoded SHA-256 of a file, streamed in 64 KB blocks so we don't peak memory on large
      * chunk files (~2 MB uncompressed is typical, but defensible across sizes).
      */
    def sha256Hex(path: Path): String = {
        val md = MessageDigest.getInstance("SHA-256")
        val buf = Array.ofDim[Byte](64 * 1024)
        scala.util.Using.resource(Files.newInputStream(path)) { in =>
            var read = in.read(buf)
            while read >= 0 do {
                if read > 0 then md.update(buf, 0, read)
                read = in.read(buf)
            }
        }
        md.digest().toHex
    }
}
