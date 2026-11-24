package scalus.cardano.ledger
package rules

import org.scalatest.funsuite.AnyFunSuite
import scalus.testing.kit.Party.Alice
import scalus.uplc.builtin.{ByteString, Data}
import TransactionWitnessSet.given

class MissingRequiredDatumsValidatorTest extends AnyFunSuite, ValidatorRulesTestKit {

    // The rule reads only the hash and the language of a script, so any fixed bytes serve.
    private val plutusV1 = Script.PlutusV1(ByteString.fromHex("01"))
    private val plutusV3 = Script.PlutusV3(ByteString.fromHex("03"))

    private val datum: Data = Data.I(42)
    private val datumHash = DataHash.fromByteString(datum.dataHash)
    private val datums: KeepRaw[TaggedSortedMap[DataHash, KeepRaw[Data]]] =
        KeepRaw(TaggedSortedMap.from(Set(KeepRaw(datum))))

    private val v1Witness = TransactionWitnessSet(plutusV1Scripts = TaggedSortedStrictMap(plutusV1))
    private val v3Witness = TransactionWitnessSet(plutusV3Scripts = TaggedSortedStrictMap(plutusV3))

    /** A key output of `coin` lovelace with `datumHash`, a supplemental datum source. */
    private def outputWithDatumHash(coin: Long): TransactionOutput =
        Output(keyAddress(1), Value(Coin(coin)), datumHash)

    /** The UTxO of `input(0)` at a key address, so the rule needs no datum to spend it. */
    private val keyUtxo = input(0) -> Output(keyAddress(1), Value(Coin(1000000L)))

    test("MissingRequiredDatumsValidator success with no scripts") {
        val transaction = txSpending(input(0))(TransactionWitnessSet.empty)

        val result = validate(MissingRequiredDatumsValidator, Map(keyUtxo), transaction)

        assert(result.isRight)
    }

    test("MissingRequiredDatumsValidator success with matching datum") {
        val utxo = Map(
          scriptUtxo(input(0), plutusV3.scriptHash, Some(DatumOption.Hash(datumHash)))
        )
        val transaction = txSpending(input(0))(v3Witness.copy(plutusData = datums))

        val result = validate(MissingRequiredDatumsValidator, utxo, transaction)
        assert(result.isRight)
    }

    test("MissingRequiredDatumsValidator failure with missing datum") {
        val utxo = Map(
          scriptUtxo(input(0), plutusV1.scriptHash, Some(DatumOption.Hash(datumHash)))
        )
        val transaction = txSpending(input(0))(v1Witness)

        val result = validate(MissingRequiredDatumsValidator, utxo, transaction)
        assert(result.isLeft)
    }

    test("MissingRequiredDatumsValidator failure with unspendable UTxO no datum hash") {
        val utxo = Map(scriptUtxo(input(0), plutusV1.scriptHash))
        val transaction = txSpending(input(0))(v1Witness)

        val result = validate(MissingRequiredDatumsValidator, utxo, transaction)
        assert(result.isLeft)
    }

    test("MissingRequiredDatumsValidator success with no datum hash for PlutusV3") {
        val utxo = Map(scriptUtxo(input(0), plutusV3.scriptHash))
        val transaction = txSpending(input(0))(v3Witness)

        val result = validate(MissingRequiredDatumsValidator, utxo, transaction)
        assert(result.isRight)
    }

    test("MissingRequiredDatumsValidator success with supplemental datum from outputs") {
        val transaction = txSpending(input(0))(TransactionWitnessSet(plutusData = datums))
            .withBody(_.copy(outputs = IndexedSeq(Sized(outputWithDatumHash(500000L)))))

        val result = validate(MissingRequiredDatumsValidator, Map(keyUtxo), transaction)

        assert(result.isRight)
    }

    test("MissingRequiredDatumsValidator success with supplemental datum from reference inputs") {
        val utxo = Map(keyUtxo, input(1) -> outputWithDatumHash(500000L))
        val transaction = txSpending(input(0))(TransactionWitnessSet(plutusData = datums))
            .withBody(_.copy(referenceInputs = TaggedSortedSet(input(1))))

        val result = validate(MissingRequiredDatumsValidator, utxo, transaction)

        assert(result.isRight)
    }

    test(
      "MissingRequiredDatumsValidator success with supplemental datum from collateralReturnOutput"
    ) {
        val transaction = txSpending(input(0))(TransactionWitnessSet(plutusData = datums))
            .withBody(_.copy(collateralReturnOutput = Some(Sized(outputWithDatumHash(500000L)))))

        val result = validate(MissingRequiredDatumsValidator, Map(keyUtxo), transaction)

        assert(result.isRight)
    }

    test(
      "MissingRequiredDatumsValidator success when datum is used in outputs, reference inputs and collateral return but skipped in witness"
    ) {
        val utxo = Map(keyUtxo, input(1) -> outputWithDatumHash(500000L))
        val transaction = txSpending(input(0))(TransactionWitnessSet.empty).withBody(
          _.copy(
            outputs = IndexedSeq(Sized(outputWithDatumHash(500000L))),
            referenceInputs = TaggedSortedSet(input(1)),
            collateralReturnOutput = Some(Sized(outputWithDatumHash(300000L)))
          )
        )

        val result = validate(MissingRequiredDatumsValidator, utxo, transaction)

        assert(result.isRight)
    }

    test("MissingRequiredDatumsValidator failure with not allowed supplemental datum") {
        val transaction = txSpending(input(0))(TransactionWitnessSet(plutusData = datums))

        val result = validate(MissingRequiredDatumsValidator, Map(keyUtxo), transaction)
        assert(result.isLeft)
    }

    test("MissingRequiredDatumsValidator success with native script") {
        val nativeScript = Timelock.Signature(Alice.addrKeyHash)
        val utxo = Map(scriptUtxo(input(0), nativeScript.scriptHash))
        val transaction = txSpending(input(0))(
          TransactionWitnessSet(nativeScripts = TaggedSortedMap(Script.Native(nativeScript)))
        )

        val result = validate(MissingRequiredDatumsValidator, utxo, transaction)

        assert(result.isRight)
    }

    // Tests with inline datums
    test("MissingRequiredDatumsValidator success with inline datum in inputs") {
        val utxo = Map(scriptUtxo(input(0), plutusV3.scriptHash, Some(DatumOption.Inline(datum))))
        val transaction = txSpending(input(0))(v3Witness)

        val result = validate(MissingRequiredDatumsValidator, utxo, transaction)
        assert(result.isRight)
    }
}
