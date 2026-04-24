package scalus.cardano.node.stream.engine.snapshot.mithril

import com.github.plokhotnyuk.jsoniter_scala.core.JsonValueCodec
import com.github.plokhotnyuk.jsoniter_scala.macros.{named, CodecMakerConfig, JsonCodecMaker}

/** Typed Scala mirrors of the JSON shapes the Mithril aggregator returns — matches
  * `mithril-common/src/messages/` and `mithril-common/src/entities/cardano_database.rs` on the Rust
  * side.
  *
  * Field names are camelCase in Scala; jsoniter's `enforce_snake_case2` mapper translates to the
  * aggregator's snake_case wire shape on the fly. One exception is [[MultiFilesUri.Template]],
  * which Rust's `serde` leaves capitalised — `@named("Template")` pins the wire name against the
  * mapper.
  *
  * Scope: just enough to drive the restore path. Fields whose Rust type is a polymorphic enum with
  * variant-specific payloads (`SignedEntityType`, `ProtocolMessage`) are held as raw JSON text —
  * typing them properly is doable but orthogonal to getting download + verify working.
  */
object MithrilMessages {

    final case class CardanoDbBeacon(
        epoch: Long,
        immutableFileNumber: Long
    )

    /** Protocol parameters as emitted under `metadata.parameters` on every certificate. */
    final case class ProtocolParameters(k: Long, m: Long, phiF: Double)

    /** An entry in `GET /certificates` — the restore path uses this to find a certificate that
      * covers the snapshot it wants to verify.
      */
    final case class MithrilCertificateListItem(
        hash: String,
        previousHash: String,
        epoch: Long,
        metadata: CertificateMetadata,
        signedMessage: String,
        aggregateVerificationKey: String
    )

    final case class CertificateMetadata(
        network: String,
        version: String,
        parameters: ProtocolParameters,
        initiatedAt: String,
        sealedAt: String,
        totalSigners: Long
    )

    /** An entry in `GET /artifact/cardano-database`. */
    final case class CardanoDatabaseV2ListItem(
        hash: String,
        merkleRoot: String,
        beacon: CardanoDbBeacon,
        certificateHash: String,
        totalDbSizeUncompressed: Long,
        cardanoNodeVersion: String,
        createdAt: String
    )

    /** Full v2 metadata for one snapshot — the response of `GET /artifact/cardano-database/{hash}`.
      * Carries the digest/immutable/ancillary location URIs the downloader consumes.
      */
    final case class CardanoDatabaseV2Metadata(
        hash: String,
        merkleRoot: String,
        network: String,
        beacon: CardanoDbBeacon,
        certificateHash: String,
        totalDbSizeUncompressed: Long,
        digests: DigestsLocations,
        immutables: ImmutablesLocations,
        ancillary: AncillaryLocations,
        cardanoNodeVersion: String,
        createdAt: String
    )

    final case class DigestsLocations(
        sizeUncompressed: Long,
        locations: Seq[DigestLocation]
    )

    final case class ImmutablesLocations(
        averageSizeUncompressed: Long,
        locations: Seq[ImmutablesLocation]
    )

    final case class AncillaryLocations(
        sizeUncompressed: Long,
        locations: Seq[AncillaryLocation]
    )

    // `type`-tagged enums — match Rust's `#[serde(tag = "type", rename_all = "snake_case")]` with
    // a `#[serde(other)]` fallback. `@named` pins the discriminator value per case; the
    // field-name mapper doesn't apply to discriminator values, so we keep them explicit.
    sealed trait DigestLocation
    object DigestLocation {
        @named("cloud_storage")
        final case class CloudStorage(
            uri: String,
            compressionAlgorithm: Option[String]
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
            compressionAlgorithm: Option[String]
        ) extends ImmutablesLocation
        @named("unknown") case object Unknown extends ImmutablesLocation
    }

    sealed trait AncillaryLocation
    object AncillaryLocation {
        @named("cloud_storage")
        final case class CloudStorage(
            uri: String,
            compressionAlgorithm: Option[String]
        ) extends AncillaryLocation
        @named("unknown") case object Unknown extends AncillaryLocation
    }

    /** Rust's `MultiFilesUri` — `{"Template": "https://…/{immutable_file_number}.tar.zst"}`. The
      * wire field is capitalised `Template` (Rust serde default), hence the explicit `@named`.
      */
    final case class MultiFilesUri(@named("Template") template: Option[String]) {
        def resolve(immutableFileNumber: Long): Option[String] =
            template.map(_.replace("{immutable_file_number}", immutableFileNumber.toString))
    }

    // -------- jsoniter codecs --------
    //
    // `withFieldNameMapper(enforce_snake_case2)` — `totalDbSize` ↔ `total_db_size`; preserves
    // acronym groups correctly (as opposed to `enforce_snake_case`).
    // `withSkipUnexpectedFields(true)` — the aggregator returns polymorphic wire fields
    // (`signed_entity_type`, `protocol_message`, …) that we don't model here; drop them silently.
    //
    // The config must be inlined per `given` (jsoniter's macro rejects `val`-bound configs as
    // non-constant expressions), so `snakeCase` here is only a readability helper used inside
    // each inline expression — it never leaks out as a shared value.

    given JsonValueCodec[CardanoDbBeacon] = JsonCodecMaker.make(
      CodecMakerConfig.withFieldNameMapper(JsonCodecMaker.enforce_snake_case2)
    )
    given JsonValueCodec[ProtocolParameters] = JsonCodecMaker.make(
      CodecMakerConfig.withFieldNameMapper(JsonCodecMaker.enforce_snake_case2)
    )
    given JsonValueCodec[CertificateMetadata] = JsonCodecMaker.make(
      CodecMakerConfig.withFieldNameMapper(JsonCodecMaker.enforce_snake_case2)
    )
    given JsonValueCodec[MultiFilesUri] = JsonCodecMaker.make(
      CodecMakerConfig.withFieldNameMapper(JsonCodecMaker.enforce_snake_case2)
    )

    given JsonValueCodec[DigestLocation] = JsonCodecMaker.make(
      CodecMakerConfig
          .withDiscriminatorFieldName(Some("type"))
          .withFieldNameMapper(JsonCodecMaker.enforce_snake_case2)
    )
    given JsonValueCodec[ImmutablesLocation] = JsonCodecMaker.make(
      CodecMakerConfig
          .withDiscriminatorFieldName(Some("type"))
          .withFieldNameMapper(JsonCodecMaker.enforce_snake_case2)
    )
    given JsonValueCodec[AncillaryLocation] = JsonCodecMaker.make(
      CodecMakerConfig
          .withDiscriminatorFieldName(Some("type"))
          .withFieldNameMapper(JsonCodecMaker.enforce_snake_case2)
    )

    given JsonValueCodec[DigestsLocations] = JsonCodecMaker.make(
      CodecMakerConfig.withFieldNameMapper(JsonCodecMaker.enforce_snake_case2)
    )
    given JsonValueCodec[ImmutablesLocations] = JsonCodecMaker.make(
      CodecMakerConfig.withFieldNameMapper(JsonCodecMaker.enforce_snake_case2)
    )
    given JsonValueCodec[AncillaryLocations] = JsonCodecMaker.make(
      CodecMakerConfig.withFieldNameMapper(JsonCodecMaker.enforce_snake_case2)
    )

    given JsonValueCodec[MithrilCertificateListItem] = JsonCodecMaker.make(
      CodecMakerConfig
          .withSkipUnexpectedFields(true)
          .withFieldNameMapper(JsonCodecMaker.enforce_snake_case2)
    )
    given JsonValueCodec[CardanoDatabaseV2ListItem] = JsonCodecMaker.make(
      CodecMakerConfig
          .withSkipUnexpectedFields(true)
          .withFieldNameMapper(JsonCodecMaker.enforce_snake_case2)
    )
    given JsonValueCodec[CardanoDatabaseV2Metadata] = JsonCodecMaker.make(
      CodecMakerConfig
          .withSkipUnexpectedFields(true)
          .withFieldNameMapper(JsonCodecMaker.enforce_snake_case2)
    )

    // Seq codecs collide on JVM erasure — name them explicitly.
    given certificateListCodec: JsonValueCodec[Seq[MithrilCertificateListItem]] =
        JsonCodecMaker.make(
          CodecMakerConfig
              .withSkipUnexpectedFields(true)
              .withFieldNameMapper(JsonCodecMaker.enforce_snake_case2)
        )
    given cardanoDatabaseV2ListCodec: JsonValueCodec[Seq[CardanoDatabaseV2ListItem]] =
        JsonCodecMaker.make(
          CodecMakerConfig
              .withSkipUnexpectedFields(true)
              .withFieldNameMapper(JsonCodecMaker.enforce_snake_case2)
        )
}
