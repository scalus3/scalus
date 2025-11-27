package scalus.cardano.ledger
package rules

import org.scalatest.funsuite.AnyFunSuite
import scalus.builtin.{platform, ByteString}

class MetadataValidatorTest extends AnyFunSuite, ValidatorRulesTestKit {
    test("MetadataValidator success when both hash and data are absent") {
        val context = Context()
        val state = State()
        val transaction = Transaction(
          body = KeepRaw(
            TransactionBody(
              inputs = TaggedSortedSet.empty,
              outputs = IndexedSeq.empty,
              fee = Coin.zero,
              auxiliaryDataHash = None
            )
          ),
          witnessSet = TransactionWitnessSet(),
          auxiliaryData = None
        )

        val result = MetadataValidator.validate(context, state, transaction)
        assert(result.isRight)
    }

    test("MetadataValidator failure when hash present but data absent") {
        val context = Context()
        val state = State()
        val auxiliaryDataHash = AuxiliaryDataHash.fromByteString(
          ByteString.fromArray(Array.fill(32)(0.toByte))
        )

        val transaction = Transaction(
          body = KeepRaw(
            TransactionBody(
              inputs = TaggedSortedSet.empty,
              outputs = IndexedSeq.empty,
              fee = Coin.zero,
              auxiliaryDataHash = Some(auxiliaryDataHash)
            )
          ),
          witnessSet = TransactionWitnessSet(),
          auxiliaryData = None
        )

        val result = MetadataValidator.validate(context, state, transaction)
        assert(result.isLeft)
        assert(
          result.left.exists(
            _.isInstanceOf[TransactionException.MetadataException.MissingAuxiliaryDataException]
          )
        )
    }

    test("MetadataValidator failure when data present but hash absent") {
        val context = Context()
        val state = State()
        val metadata = Map(Word64(0L) -> Metadatum.Int(42L))
        val auxiliaryData = AuxiliaryData.Metadata(metadata)
        val keepRawAuxiliaryData = KeepRaw(auxiliaryData)

        val transaction = Transaction(
          body = KeepRaw(
            TransactionBody(
              inputs = TaggedSortedSet.empty,
              outputs = IndexedSeq.empty,
              fee = Coin.zero,
              auxiliaryDataHash = None
            )
          ),
          witnessSet = TransactionWitnessSet(),
          auxiliaryData = Some(keepRawAuxiliaryData)
        )

        val result = MetadataValidator.validate(context, state, transaction)
        assert(result.isLeft)
        assert(
          result.left.exists(
            _.isInstanceOf[TransactionException.MetadataException.MissingAuxiliaryDataHashException]
          )
        )
    }

    test(
      "MetadataValidator success with valid metadata containing various types with exact values and matching hash"
    ) {
        val context = Context()
        val state = State()
        val exactBytes = ByteString.fromArray(Array.fill(64)(1.toByte))
        val exactText = "a" * 64
        val metadata = Map(
          Word64(0L) -> Metadatum.Int(42L),
          Word64(1L) -> Metadatum.Bytes(exactBytes),
          Word64(2L) -> Metadatum.Text(exactText),
          Word64(3L) -> Metadatum.List(
            IndexedSeq(Metadatum.Bytes(exactBytes), Metadatum.Text(exactText))
          ),
          Word64(4L) -> Metadatum.Map(
            Map(Metadatum.Bytes(exactBytes) -> Metadatum.Text(exactText))
          )
        )
        val auxiliaryData = AuxiliaryData.Metadata(metadata)
        val keepRawAuxiliaryData = KeepRaw(auxiliaryData)
        val cborAuxiliaryData = keepRawAuxiliaryData.raw
        val auxiliaryDataHash = AuxiliaryDataHash.fromByteString(
          platform.blake2b_256(ByteString.unsafeFromArray(cborAuxiliaryData))
        )

        val transaction = Transaction(
          body = KeepRaw(
            TransactionBody(
              inputs = TaggedSortedSet.empty,
              outputs = IndexedSeq.empty,
              fee = Coin.zero,
              auxiliaryDataHash = Some(auxiliaryDataHash)
            )
          ),
          witnessSet = TransactionWitnessSet(),
          auxiliaryData = Some(keepRawAuxiliaryData)
        )

        val result = MetadataValidator.validate(context, state, transaction)
        assert(result.isRight)
    }

    test(
      "MetadataValidator failure with valid metadata containing various types with exact values but mismatched hash"
    ) {
        val context = Context()
        val state = State()
        val exactBytes = ByteString.fromArray(Array.fill(64)(1.toByte))
        val exactText = "a" * 64
        val metadata = Map(
          Word64(0L) -> Metadatum.Int(42L),
          Word64(1L) -> Metadatum.Bytes(exactBytes),
          Word64(2L) -> Metadatum.Text(exactText),
          Word64(3L) -> Metadatum.List(
            IndexedSeq(Metadatum.Bytes(exactBytes), Metadatum.Text(exactText))
          ),
          Word64(4L) -> Metadatum.Map(
            Map(Metadatum.Bytes(exactBytes) -> Metadatum.Text(exactText))
          )
        )
        val auxiliaryData = AuxiliaryData.Metadata(metadata)
        val keepRawAuxiliaryData = KeepRaw(auxiliaryData)
        val wrongAuxiliaryDataHash = AuxiliaryDataHash.fromByteString(
          ByteString.fromArray(Array.fill(32)(0.toByte))
        )

        val transaction = Transaction(
          body = KeepRaw(
            TransactionBody(
              inputs = TaggedSortedSet.empty,
              outputs = IndexedSeq.empty,
              fee = Coin.zero,
              auxiliaryDataHash = Some(wrongAuxiliaryDataHash)
            )
          ),
          witnessSet = TransactionWitnessSet(),
          auxiliaryData = Some(keepRawAuxiliaryData)
        )

        val result = MetadataValidator.validate(context, state, transaction)
        assert(result.isLeft)
        assert(
          result.left.exists(
            _.isInstanceOf[TransactionException.MetadataException.InvalidAuxiliaryDataHashException]
          )
        )
    }

    test("MetadataValidator failure with bytes exceeding max size and matching hash") {
        val context = Context()
        val state = State()
        val oversizedBytes = ByteString.fromArray(Array.fill(65)(1.toByte))
        val metadata = Map(Word64(0L) -> Metadatum.Bytes(oversizedBytes))
        val auxiliaryData = AuxiliaryData.Metadata(metadata)
        val keepRawAuxiliaryData = KeepRaw(auxiliaryData)
        val cborAuxiliaryData = keepRawAuxiliaryData.raw
        val auxiliaryDataHash = AuxiliaryDataHash.fromByteString(
          platform.blake2b_256(ByteString.unsafeFromArray(cborAuxiliaryData))
        )

        val transaction = Transaction(
          body = KeepRaw(
            TransactionBody(
              inputs = TaggedSortedSet.empty,
              outputs = IndexedSeq.empty,
              fee = Coin.zero,
              auxiliaryDataHash = Some(auxiliaryDataHash)
            )
          ),
          witnessSet = TransactionWitnessSet(),
          auxiliaryData = Some(keepRawAuxiliaryData)
        )

        val result = MetadataValidator.validate(context, state, transaction)
        assert(result.isLeft)
        assert(
          result.left.exists(
            _.isInstanceOf[TransactionException.MetadataException.InvalidAuxiliaryDataException]
          )
        )
    }

    test("MetadataValidator failure with text exceeding max size and matching hash") {
        val context = Context()
        val state = State()
        val oversizedText = "a" * 65
        val metadata = Map(Word64(0L) -> Metadatum.Text(oversizedText))
        val auxiliaryData = AuxiliaryData.Metadata(metadata)
        val keepRawAuxiliaryData = KeepRaw(auxiliaryData)
        val cborAuxiliaryData = keepRawAuxiliaryData.raw
        val auxiliaryDataHash = AuxiliaryDataHash.fromByteString(
          platform.blake2b_256(ByteString.unsafeFromArray(cborAuxiliaryData))
        )

        val transaction = Transaction(
          body = KeepRaw(
            TransactionBody(
              inputs = TaggedSortedSet.empty,
              outputs = IndexedSeq.empty,
              fee = Coin.zero,
              auxiliaryDataHash = Some(auxiliaryDataHash)
            )
          ),
          witnessSet = TransactionWitnessSet(),
          auxiliaryData = Some(keepRawAuxiliaryData)
        )

        val result = MetadataValidator.validate(context, state, transaction)
        assert(result.isLeft)
        assert(
          result.left.exists(
            _.isInstanceOf[TransactionException.MetadataException.InvalidAuxiliaryDataException]
          )
        )
    }

    test("MetadataValidator failure with List invalid values and matching hash") {
        val context = Context()
        val state = State()
        val oversizedBytes = ByteString.fromArray(Array.fill(65)(1.toByte))
        val oversizedText = "a" * 65
        val metadata = Map(
          Word64(0L) -> Metadatum.List(
            IndexedSeq(Metadatum.Bytes(oversizedBytes), Metadatum.Text(oversizedText))
          )
        )
        val auxiliaryData = AuxiliaryData.Metadata(metadata)
        val keepRawAuxiliaryData = KeepRaw(auxiliaryData)
        val cborAuxiliaryData = keepRawAuxiliaryData.raw
        val auxiliaryDataHash = AuxiliaryDataHash.fromByteString(
          platform.blake2b_256(ByteString.unsafeFromArray(cborAuxiliaryData))
        )

        val transaction = Transaction(
          body = KeepRaw(
            TransactionBody(
              inputs = TaggedSortedSet.empty,
              outputs = IndexedSeq.empty,
              fee = Coin.zero,
              auxiliaryDataHash = Some(auxiliaryDataHash)
            )
          ),
          witnessSet = TransactionWitnessSet(),
          auxiliaryData = Some(keepRawAuxiliaryData)
        )

        val result = MetadataValidator.validate(context, state, transaction)
        assert(result.isLeft)
        assert(
          result.left.exists(
            _.isInstanceOf[TransactionException.MetadataException.InvalidAuxiliaryDataException]
          )
        )
    }

    test("MetadataValidator failure with Map invalid key and matching hash") {
        val context = Context()
        val state = State()
        val oversizedBytes = ByteString.fromArray(Array.fill(65)(1.toByte))
        val metadata = Map(
          Word64(0L) -> Metadatum.Map(
            Map(Metadatum.Bytes(oversizedBytes) -> Metadatum.Int(0L))
          )
        )
        val auxiliaryData = AuxiliaryData.Metadata(metadata)
        val keepRawAuxiliaryData = KeepRaw(auxiliaryData)
        val cborAuxiliaryData = keepRawAuxiliaryData.raw
        val auxiliaryDataHash = AuxiliaryDataHash.fromByteString(
          platform.blake2b_256(ByteString.unsafeFromArray(cborAuxiliaryData))
        )

        val transaction = Transaction(
          body = KeepRaw(
            TransactionBody(
              inputs = TaggedSortedSet.empty,
              outputs = IndexedSeq.empty,
              fee = Coin.zero,
              auxiliaryDataHash = Some(auxiliaryDataHash)
            )
          ),
          witnessSet = TransactionWitnessSet(),
          auxiliaryData = Some(keepRawAuxiliaryData)
        )

        val result = MetadataValidator.validate(context, state, transaction)
        assert(result.isLeft)
        assert(
          result.left.exists(
            _.isInstanceOf[TransactionException.MetadataException.InvalidAuxiliaryDataException]
          )
        )
    }

    test("MetadataValidator failure with Map invalid value and matching hash") {
        val context = Context()
        val state = State()
        val oversizedBytes = ByteString.fromArray(Array.fill(65)(1.toByte))
        val metadata = Map(
          Word64(0L) -> Metadatum.Map(
            Map(Metadatum.Int(0L) -> Metadatum.Bytes(oversizedBytes))
          )
        )
        val auxiliaryData = AuxiliaryData.Metadata(metadata)
        val keepRawAuxiliaryData = KeepRaw(auxiliaryData)
        val cborAuxiliaryData = keepRawAuxiliaryData.raw
        val auxiliaryDataHash = AuxiliaryDataHash.fromByteString(
          platform.blake2b_256(ByteString.unsafeFromArray(cborAuxiliaryData))
        )

        val transaction = Transaction(
          body = KeepRaw(
            TransactionBody(
              inputs = TaggedSortedSet.empty,
              outputs = IndexedSeq.empty,
              fee = Coin.zero,
              auxiliaryDataHash = Some(auxiliaryDataHash)
            )
          ),
          witnessSet = TransactionWitnessSet(),
          auxiliaryData = Some(keepRawAuxiliaryData)
        )

        val result = MetadataValidator.validate(context, state, transaction)
        assert(result.isLeft)
        assert(
          result.left.exists(
            _.isInstanceOf[TransactionException.MetadataException.InvalidAuxiliaryDataException]
          )
        )
    }

    test("MetadataValidator success for <= ShelleyPV and matching hash") {
        val context = Context(
          env = UtxoEnv.default.copy(
            params = UtxoEnv.default.params.copy(
              protocolVersion = ProtocolVersion.shelleyPV
            )
          )
        )
        val state = State()
        val oversizedBytes = ByteString.fromArray(Array.fill(65)(1.toByte))
        val oversizedText = "a" * 65
        val metadata = Map(
          Word64(0L) -> Metadatum.Int(42L),
          Word64(1L) -> Metadatum.Bytes(oversizedBytes),
          Word64(2L) -> Metadatum.Text(oversizedText),
          Word64(3L) -> Metadatum.List(
            IndexedSeq(Metadatum.Bytes(oversizedBytes), Metadatum.Text(oversizedText))
          ),
          Word64(4L) -> Metadatum.Map(
            Map(Metadatum.Bytes(oversizedBytes) -> Metadatum.Text(oversizedText))
          )
        )
        val auxiliaryData = AuxiliaryData.Metadata(metadata)
        val keepRawAuxiliaryData = KeepRaw(auxiliaryData)
        val cborAuxiliaryData = keepRawAuxiliaryData.raw
        val auxiliaryDataHash = AuxiliaryDataHash.fromByteString(
          platform.blake2b_256(ByteString.unsafeFromArray(cborAuxiliaryData))
        )

        val transaction = Transaction(
          body = KeepRaw(
            TransactionBody(
              inputs = TaggedSortedSet.empty,
              outputs = IndexedSeq.empty,
              fee = Coin.zero,
              auxiliaryDataHash = Some(auxiliaryDataHash)
            )
          ),
          witnessSet = TransactionWitnessSet(),
          auxiliaryData = Some(keepRawAuxiliaryData)
        )

        val result = MetadataValidator.validate(context, state, transaction)
        assert(result.isRight)
    }

    test("MetadataValidator success with empty metadata map") {
        val context = Context()
        val state = State()
        val auxiliaryData = AuxiliaryData.Metadata(Map.empty)
        val keepRawAuxiliaryData = KeepRaw(auxiliaryData)
        val cborAuxiliaryData = keepRawAuxiliaryData.raw
        val auxiliaryDataHash = AuxiliaryDataHash.fromByteString(
          platform.blake2b_256(ByteString.unsafeFromArray(cborAuxiliaryData))
        )

        val transaction = Transaction(
          body = KeepRaw(
            TransactionBody(
              inputs = TaggedSortedSet.empty,
              outputs = IndexedSeq.empty,
              fee = Coin.zero,
              auxiliaryDataHash = Some(auxiliaryDataHash)
            )
          ),
          witnessSet = TransactionWitnessSet(),
          auxiliaryData = Some(keepRawAuxiliaryData)
        )

        val result = MetadataValidator.validate(context, state, transaction)
        assert(result.isRight)
    }
}
