package scalus.cardano.node.stream.engine.snapshot.mithril

import com.github.luben.zstd.ZstdInputStream
import com.github.plokhotnyuk.jsoniter_scala.core.*
import com.github.plokhotnyuk.jsoniter_scala.macros.*
import org.apache.commons.compress.archivers.tar.TarArchiveInputStream
import org.scalatest.funsuite.AnyFunSuite

import java.net.URI
import java.net.http.{HttpClient, HttpRequest, HttpResponse}
import scala.collection.mutable

/** Feasibility test for the Scala-side download path.
  *
  * The pinned Mithril WASM client only covers the certificate + metadata side of the protocol (the
  * `fs` feature is gated off in `mithril-client-wasm`'s Cargo.toml, and `wasm32-unknown-unknown`
  * has no filesystem regardless). Actual file transfer is plain HTTP to the aggregator / its CDN —
  * see `mithril-client/src/file_downloader/http.rs` for the upstream reference. This suite
  * exercises that path end-to-end without the WASM:
  *
  *   1. fetch the cardano-database snapshot list,
  *   2. pick the latest snapshot, fetch its v2 metadata with `locations`,
  *   3. materialise one immutable chunk from the `Template` URI, zstd-decompress + untar,
  *   4. assert the conventional three-file layout (`{n}.chunk`, `{n}.primary`, `{n}.secondary`).
  *
  * No cryptographic verification here — that step belongs in the facade once the download + extract
  * path is proven. Tagged `[network]` so CI can skip it.
  */
final class MithrilDownloadSuite extends AnyFunSuite {

    import MithrilDownloadSuite.*

    private val aggregatorBase =
        "https://aggregator.testing-preview.api.mithril.network/aggregator"

    test("[network] list → metadata → download + untar one immutable chunk") {
        val client = HttpClient.newHttpClient()

        val list = getJson[Seq[CardanoDatabaseListItem]](
          client,
          s"$aggregatorBase/artifact/cardano-database"
        )
        assert(list.nonEmpty, "aggregator returned empty cardano-database list")
        val head = list.head
        info(s"latest snapshot: hash=${head.hash} immutable=${head.beacon.immutable_file_number}")

        val meta = getJson[CardanoDatabaseV2Metadata](
          client,
          s"$aggregatorBase/artifact/cardano-database/${head.hash}"
        )
        info(
          s"immutables ~${meta.immutables.average_size_uncompressed}B each, " +
              s"${meta.immutables.locations.size} location(s); " +
              s"digests.size=${meta.digests.size_uncompressed}, ancillary.size=${meta.ancillary.size_uncompressed}"
        )

        val template = meta.immutables.locations
            .collectFirst { case ImmutablesLocation.CloudStorage(MultiFilesUri(Some(tmpl)), _) =>
                tmpl
            }
            .getOrElse(fail("no CloudStorage Template URI in immutables.locations"))

        val url =
            template.replace("{immutable_file_number}", head.beacon.immutable_file_number.toString)
        info(s"downloading $url")

        val req = HttpRequest.newBuilder(URI.create(url)).GET().build()
        val resp = client.send(req, HttpResponse.BodyHandlers.ofByteArray())
        assert(resp.statusCode == 200, s"immutable download got ${resp.statusCode}")
        val compressed = resp.body
        info(s"downloaded ${compressed.length} bytes (.tar.zst)")

        val entries = untarZst(compressed)
        entries.foreach { (name, size) => info(f"  $name%-40s $size%10d bytes") }

        val names = entries.map(_._1).toSet
        val n = head.beacon.immutable_file_number
        assert(names.contains(s"immutable/$n.chunk"), s"missing $n.chunk")
        assert(names.contains(s"immutable/$n.primary"), s"missing $n.primary")
        assert(names.contains(s"immutable/$n.secondary"), s"missing $n.secondary")
    }
}

object MithrilDownloadSuite {

    private def getJson[T: JsonValueCodec](client: HttpClient, url: String): T = {
        val req = HttpRequest.newBuilder(URI.create(url)).GET().build()
        val resp = client.send(req, HttpResponse.BodyHandlers.ofByteArray())
        require(resp.statusCode == 200, s"GET $url → ${resp.statusCode}")
        readFromArray[T](resp.body)
    }

    private def untarZst(bytes: Array[Byte]): Seq[(String, Long)] = {
        val zin = new ZstdInputStream(new java.io.ByteArrayInputStream(bytes))
        val tin = new TarArchiveInputStream(zin)
        val out = mutable.ArrayBuffer.empty[(String, Long)]
        try {
            var e = tin.getNextEntry
            while e != null do {
                if !e.isDirectory then out += (e.getName -> e.getSize)
                e = tin.getNextEntry
            }
        } finally {
            tin.close()
            zin.close()
        }
        out.toSeq
    }

    final case class CardanoDatabaseListItem(
        hash: String,
        merkle_root: String,
        beacon: CardanoDbBeacon,
        certificate_hash: String
    )

    final case class CardanoDbBeacon(epoch: Long, immutable_file_number: Long)

    final case class CardanoDatabaseV2Metadata(
        hash: String,
        merkle_root: String,
        beacon: CardanoDbBeacon,
        certificate_hash: String,
        digests: DigestsLocations,
        immutables: ImmutablesLocations,
        ancillary: AncillaryLocations
    )

    final case class DigestsLocations(size_uncompressed: Long, locations: Seq[DigestLocation])
    final case class ImmutablesLocations(
        average_size_uncompressed: Long,
        locations: Seq[ImmutablesLocation]
    )
    final case class AncillaryLocations(size_uncompressed: Long, locations: Seq[AncillaryLocation])

    // `type`-tagged enums — matches Rust's `#[serde(tag = "type", rename_all = "snake_case")]`.
    // `@named` sets the discriminator value per case; `unknown` stays as the Scala name.
    sealed trait DigestLocation
    object DigestLocation {
        @named("cloud_storage")
        case class CloudStorage(uri: String, compression_algorithm: Option[String])
            extends DigestLocation
        @named("aggregator")
        case class Aggregator(uri: String) extends DigestLocation
        @named("unknown") case object Unknown extends DigestLocation
    }

    sealed trait ImmutablesLocation
    object ImmutablesLocation {
        @named("cloud_storage")
        case class CloudStorage(uri: MultiFilesUri, compression_algorithm: Option[String])
            extends ImmutablesLocation
        @named("unknown") case object Unknown extends ImmutablesLocation
    }

    sealed trait AncillaryLocation
    object AncillaryLocation {
        @named("cloud_storage")
        case class CloudStorage(uri: String, compression_algorithm: Option[String])
            extends AncillaryLocation
        @named("unknown") case object Unknown extends AncillaryLocation
    }

    /** Mirror of Rust's `MultiFilesUri` —
      * `{"Template": "https://…/{immutable_file_number}.tar.zst"}`.
      */
    final case class MultiFilesUri(Template: Option[String])

    given JsonValueCodec[CardanoDbBeacon] = JsonCodecMaker.make
    given JsonValueCodec[MultiFilesUri] = JsonCodecMaker.make

    given JsonValueCodec[DigestLocation] = JsonCodecMaker.make(
      CodecMakerConfig.withDiscriminatorFieldName(Some("type"))
    )
    given JsonValueCodec[ImmutablesLocation] = JsonCodecMaker.make(
      CodecMakerConfig.withDiscriminatorFieldName(Some("type"))
    )
    given JsonValueCodec[AncillaryLocation] = JsonCodecMaker.make(
      CodecMakerConfig.withDiscriminatorFieldName(Some("type"))
    )

    given JsonValueCodec[DigestsLocations] = JsonCodecMaker.make
    given JsonValueCodec[ImmutablesLocations] = JsonCodecMaker.make
    given JsonValueCodec[AncillaryLocations] = JsonCodecMaker.make

    given JsonValueCodec[CardanoDatabaseListItem] = JsonCodecMaker.make
    given JsonValueCodec[Seq[CardanoDatabaseListItem]] = JsonCodecMaker.make
    given JsonValueCodec[CardanoDatabaseV2Metadata] = JsonCodecMaker.make
}
