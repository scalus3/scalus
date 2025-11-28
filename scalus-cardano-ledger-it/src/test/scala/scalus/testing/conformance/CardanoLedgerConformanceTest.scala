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

        println(s"\n=== Summary ===")
        println(s"Total vectors: ${results.map(_._2.map(_.length).getOrElse(0)).sum}")
        println(s"Failed vectors: ${failed.length}")
        println(s"Success rate: ${100.0 * (results.map(_._2.map(_.length).getOrElse(0)).sum - failed.length) / results.map(_._2.map(_.length).getOrElse(0)).sum}%")

        // Group by exception type
        val byException = failed.groupBy {
            case (_, _, _, Left(ex)) => ex.getClass.getSimpleName
            case _ => "Unexpected success"
        }
        println(s"\nFailures by exception type:")
        byException.toSeq.sortBy(-_._2.length).foreach { case (exType, failures) =>
            println(s"  $exType: ${failures.length}")
        }

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

    test("Addr28Extra mempack parsing uses little-endian byte order", TestTag) {
        assume(vectorsExist, "Conformance test vectors directory not found")
        val vectorName =
            "Conway.Imp.AlonzoImpSpec.UTXOW.Valid transactions.PlutusV1.Validating MINT script"
        for case (path, vector) <- loadAllVectors(vectorName) do {
            if path.getFileName.toString == "1" then
                val ledgerState = LedgerState.fromCbor(Hex.hexToBytes(vector.oldLedgerState))
                val transaction = Transaction.fromCbor(Hex.hexToBytes(vector.cbor))

                // Get all witness key hashes
                val witnessKeyHashes = transaction.witnessSet.vkeyWitnesses.toSet.map(_.vkeyHash.toHex)

                // Verify that input addresses have matching witnesses
                transaction.body.value.inputs.toSeq.foreach { input =>
                    ledgerState.ruleState.utxos.get(input) match {
                        case Some(output) =>
                            output.address.keyHashOption match {
                                case Some(kh) =>
                                    val khHex = kh.toHex
                                    // This test verifies the Addr28Extra parsing fix:
                                    // With correct little-endian parsing, the payment key hash
                                    // should match one of the witness key hashes
                                    assert(
                                      witnessKeyHashes.contains(khHex),
                                      s"Payment key hash $khHex should have a witness"
                                    )
                                case None => // Script address, no witness needed
                            }
                        case None =>
                            fail(s"Input $input not found in UTxO set")
                    }
                }
        }
    }
}
