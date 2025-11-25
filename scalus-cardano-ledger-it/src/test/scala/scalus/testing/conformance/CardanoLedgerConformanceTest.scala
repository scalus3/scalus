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

    test("Conformance ledger rules test") {
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

        val results = for
            vector <- vectorNames()
            context = Context.testMainnet()
        yield vector -> Try(for case (_, vector) <- loadAllVectors(vector) yield {
            val transaction = Transaction.fromCbor(Hex.hexToBytes(vector.cbor))
            val state = LedgerState.fromCbor(Hex.hexToBytes(vector.oldLedgerState)).ruleState
            vector.success -> validators.validate(context, state, transaction)
        })

        for case (a, b) <- results do {
            println(a)
            println(pprint(b))
            println()
        }

    }

}
