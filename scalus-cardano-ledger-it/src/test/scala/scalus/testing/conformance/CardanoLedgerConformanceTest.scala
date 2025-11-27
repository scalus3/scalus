package scalus.testing.conformance

import org.scalatest.Tag
import org.scalatest.funsuite.AnyFunSuite
import scalus.cardano.ledger.*
import scalus.cardano.ledger.rules.*
import scalus.testing.conformance.CardanoLedgerVectors.*
import scalus.utils.Hex

import scala.util.Try

/** Cardano Ledger Conformance Test Suite
  *
  * Runs conformance tests from cardano-ledger test vectors to validate Scalus ledger implementation
  * against reference implementation.
  */
class CardanoLedgerConformanceTest extends AnyFunSuite {

    val TestTag = Tag("conformance")

    test("MetadataValidator Conway.Imp.AllegraImpSpec.UTXOW.InvalidMetadata", TestTag) {
        assume(vectorsExist, "Conformance test vectors directory not found")
        val vectorName = "Conway.Imp.AllegraImpSpec.UTXOW.InvalidMetadata"
        for case (path, vector) <- loadAllVectors(vectorName) do {
            val ledgerState = LedgerState.fromCbor(Hex.hexToBytes(vector.oldLedgerState))
            println(pprint(ledgerState))
            val transaction = Transaction.fromCbor(Hex.hexToBytes(vector.cbor))
            println(pprint(transaction))
            val validation = MetadataValidator.validate(
              Context.testMainnet(),
              ledgerState.ruleState,
              transaction
            )
            println(pprint(validation))
            assert(vector.success === validation.isRight)
        }
    }

    test(
      "Conway.Imp.ConwayImpSpec - Version 10.RATIFY.Voting.Active voting stake.StakePool.Proposal deposits contribute to active voting stake.After switching delegations/9",
      TestTag
    ) {
        assume(vectorsExist, "Conformance test vectors directory not found")
        val vectorName =
            "Conway.Imp.ConwayImpSpec - Version 10.RATIFY.Voting.Active voting stake.StakePool.Proposal deposits contribute to active voting stake.After switching delegations/9"
        for case (path, vector) <- loadAllVectors(vectorName) do {
            val ledgerState = LedgerState.fromCbor(Hex.hexToBytes(vector.oldLedgerState))
            println(pprint(ledgerState))
            val transaction = Transaction.fromCbor(Hex.hexToBytes(vector.cbor))
            println(pprint(transaction))
        }
    }

    val validators = STS.Validator(
      List(
        AllInputsMustBeInUtxoValidator,
        EmptyInputsValidator,
        ExactSetOfRedeemersValidator,
        ExUnitsTooBigValidator,
        FeesOkValidator,
        InputsAndReferenceInputsDisjointValidator,
        MetadataValidator,
        MissingKeyHashesValidator,
        MissingOrExtraScriptHashesValidator,
        MissingRequiredDatumsValidator,
        NativeScriptsValidator,
        OutsideForecastValidator,
        OutsideValidityIntervalValidator,
        OutputBootAddrAttrsSizeValidator,
        OutputsHaveNotEnoughCoinsValidator,
        OutputsHaveTooBigValueStorageSizeValidator,
        ProtocolParamsViewHashesMatchValidator,
        ScriptsWellFormedValidator,
        TooManyCollateralInputsValidator,
        TransactionSizeValidator,
        ValueNotConservedUTxOValidator,
        VerifiedSignaturesInWitnessesValidator,
        WrongNetworkInTxBodyValidator,
        WrongNetworkValidator,
        WrongNetworkWithdrawalValidator
      )
    )

    private def validateVector(vectorName: String) =
        for case (path, vector) <- loadAllVectors(vectorName) yield
            val transaction = Transaction.fromCbor(Hex.hexToBytes(vector.cbor))
            val state = LedgerState.fromCbor(Hex.hexToBytes(vector.oldLedgerState)).ruleState
            (
              path.getFileName.toFile.getName,
              vector.success,
              validators.validate(Context(), state, transaction)
            )

    private def failureVectors(
        results: List[(String, Try[List[(String, Boolean, validators.Result)]])]
    ) = for
        case (vectorName, result) <- results
        x <- result.toOption.toList
        case (n, success, result) <- x if success != result.isRight
    yield (vectorName, n, success, result)

    test("Conformance ledger rules test") {
        val results =
            for vectorName <- vectorNames()
            yield vectorName -> Try(validateVector(vectorName))

        val failed = failureVectors(results)

        for case (name, index, success, result) <- failed do
            println(s"$name/$index ($success) ${pprint(result)}")

        // assert(failed.isEmpty)
    }

    test("Conway.Imp.AlonzoImpSpec.UTXO.PlutusV1.Insufficient collateral") {
        val vectorName = "Conway.Imp.AlonzoImpSpec.UTXO.PlutusV1.Insufficient collateral"
        val result = validateVector(vectorName)
        println(pprint(result))
    }

    test("Conway.Imp.AlonzoImpSpec.UTXOS.PlutusV1.Invalid plutus script fails in phase 2") {
        val vectorName =
            "Conway.Imp.AlonzoImpSpec.UTXOS.PlutusV1.Invalid plutus script fails in phase 2"
        val result = validateVector(vectorName)
        println(pprint(result))
    }

    test(
      "Conway.Imp.ConwayImpSpec - Version 10.RATIFY.When CC expired.SPOs alone can't enact hard-fork"
    ) {
        val vectorName =
            "Conway.Imp.ConwayImpSpec - Version 10.RATIFY.When CC expired.SPOs alone can't enact hard-fork"
        val result = validateVector(vectorName)
        println(pprint(result))
    }

}
