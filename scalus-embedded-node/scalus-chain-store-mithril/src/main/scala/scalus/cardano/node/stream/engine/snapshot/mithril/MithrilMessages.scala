package scalus.cardano.node.stream.engine.snapshot.mithril

import com.github.plokhotnyuk.jsoniter_scala.core.JsonValueCodec
import com.github.plokhotnyuk.jsoniter_scala.macros.{named, CodecMakerConfig, JsonCodecMaker}

/** Typed Scala mirrors of the JSON shapes the Mithril aggregator returns — matches
  * `mithril-common/src/messages/` and `mithril-common/src/entities/cardano_database.rs` on the Rust
  * side. Field names use `snake_case` where that's what the wire format emits.
  *
  * Scope: just enough to drive the restore path. Fields whose Rust type is a polymorphic enum with
  * variant-specific payloads (`SignedEntityType`, `ProtocolMessage`) are held as raw JSON text —
  * typing them properly is doable but orthogonal to getting download + verify working.
  */
object MithrilMessages {

    final case class CardanoDbBeacon(
        epoch: Long,
        immutable_file_number: Long
    )

    /** Protocol parameters as emitted under `metadata.parameters` on every certificate. */
    final case class ProtocolParameters(k: Long, m: Long, phi_f: Double)

    /** An entry in `GET /certificates` — the restore path uses this to find a certificate that
      * covers the snapshot it wants to verify.
      */
    final case class MithrilCertificateListItem(
        hash: String,
        previous_hash: String,
        epoch: Long,
        metadata: CertificateMetadata,
        signed_message: String,
        aggregate_verification_key: String
    )

    final case class CertificateMetadata(
        network: String,
        version: String,
        parameters: ProtocolParameters,
        initiated_at: String,
        sealed_at: String,
        total_signers: Long
    )

    /** An entry in `GET /artifact/cardano-database`. */
    final case class CardanoDatabaseV2ListItem(
        hash: String,
        merkle_root: String,
        beacon: CardanoDbBeacon,
        certificate_hash: String,
        total_db_size_uncompressed: Long,
        cardano_node_version: String,
        created_at: String
    )

    /** Full v2 metadata for one snapshot — the response of `GET /artifact/cardano-database/{hash}`.
      * Carries the digest/immutable/ancillary location URIs the downloader consumes.
      */
    final case class CardanoDatabaseV2Metadata(
        hash: String,
        merkle_root: String,
        network: String,
        beacon: CardanoDbBeacon,
        certificate_hash: String,
        total_db_size_uncompressed: Long,
        digests: DigestsLocations,
        immutables: ImmutablesLocations,
        ancillary: AncillaryLocations,
        cardano_node_version: String,
        created_at: String
    )

    final case class DigestsLocations(
        size_uncompressed: Long,
        locations: Seq[DigestLocation]
    )

    final case class ImmutablesLocations(
        average_size_uncompressed: Long,
        locations: Seq[ImmutablesLocation]
    )

    final case class AncillaryLocations(
        size_uncompressed: Long,
        locations: Seq[AncillaryLocation]
    )

    // `type`-tagged enums — match Rust's `#[serde(tag = "type", rename_all = "snake_case")]` with
    // a `#[serde(other)]` fallback. `@named` sets the discriminator value per case.
    sealed trait DigestLocation
    object DigestLocation {
        @named("cloud_storage")
        final case class CloudStorage(
            uri: String,
            compression_algorithm: Option[String]
        ) extends DigestLocation
        @named("aggregator")
        final case class Aggregator(uri: String) extends DigestLocation
        @named("unknown") case object Unknown extends DigestLocation
    }

    sealed trait ImmutablesLocation
    object ImmutablesLocation {
        @named("cloud_storage")
        final case class CloudStorage(
            uri: MultiFilesUri,
            compression_algorithm: Option[String]
        ) extends ImmutablesLocation
        @named("unknown") case object Unknown extends ImmutablesLocation
    }

    sealed trait AncillaryLocation
    object AncillaryLocation {
        @named("cloud_storage")
        final case class CloudStorage(
            uri: String,
            compression_algorithm: Option[String]
        ) extends AncillaryLocation
        @named("unknown") case object Unknown extends AncillaryLocation
    }

    /** Rust's `MultiFilesUri` — `{"Template": "https://…/{immutable_file_number}.tar.zst"}`. */
    final case class MultiFilesUri(Template: Option[String]) {
        def resolve(immutableFileNumber: Long): Option[String] =
            Template.map(_.replace("{immutable_file_number}", immutableFileNumber.toString))
    }

    // -------- jsoniter codecs --------

    given JsonValueCodec[CardanoDbBeacon] = JsonCodecMaker.make
    given JsonValueCodec[ProtocolParameters] = JsonCodecMaker.make
    given JsonValueCodec[CertificateMetadata] = JsonCodecMaker.make
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

    // `withSkipUnexpectedFields` — jsoniter silently drops unknown fields; we want that so
    // the presence of polymorphic wire fields (`signed_entity_type`, `protocol_message`, …)
    // doesn't force us to type them while we only care about the restore-path data. The
    // config must be passed inline: jsoniter's macro rejects `val`-bound configs as
    // non-constant expressions.

    given JsonValueCodec[MithrilCertificateListItem] = JsonCodecMaker.make(
      CodecMakerConfig.withSkipUnexpectedFields(true)
    )
    given JsonValueCodec[CardanoDatabaseV2ListItem] = JsonCodecMaker.make(
      CodecMakerConfig.withSkipUnexpectedFields(true)
    )
    given JsonValueCodec[CardanoDatabaseV2Metadata] = JsonCodecMaker.make(
      CodecMakerConfig.withSkipUnexpectedFields(true)
    )

    // Seq codecs collide on JVM erasure — name them explicitly.
    given certificateListCodec: JsonValueCodec[Seq[MithrilCertificateListItem]] =
        JsonCodecMaker.make(CodecMakerConfig.withSkipUnexpectedFields(true))
    given cardanoDatabaseV2ListCodec: JsonValueCodec[Seq[CardanoDatabaseV2ListItem]] =
        JsonCodecMaker.make(CodecMakerConfig.withSkipUnexpectedFields(true))
}
