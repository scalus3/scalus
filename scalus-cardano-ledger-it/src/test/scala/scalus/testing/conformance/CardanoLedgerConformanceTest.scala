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

    // Set to true to use extracted pparams, false to use default
    private val useExtractedPParams = true

    private def validateVector(vectorName: String) =
        for case (path, vector) <- loadAllVectors(vectorName) yield
            val transaction = Transaction.fromCbor(Hex.hexToBytes(vector.cbor))
            val state = LedgerState.fromCbor(Hex.hexToBytes(vector.oldLedgerState)).ruleState
            // Extract protocol parameters from test vector
            val extractedParams =
                if useExtractedPParams then
                    ConwayProtocolParams
                        .extractPparamsHash(vector.oldLedgerState, pparamsDir)
                        .flatMap(hash => ConwayProtocolParams.loadFromHash(pparamsDir, hash))
                        .map(_.toProtocolParams)
                else None

            val params =
                extractedParams.getOrElse(Context().env.params) // Fallback to default params
            val context =
                Context(env =
                    rules.UtxoEnv(
                      0,
                      params,
                      state.certState,
                      scalus.cardano.address.Network.Testnet
                    )
                )
            (
              path.getFileName.toFile.getName,
              vector.success,
              validators.validate(context, state, transaction)
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

        // Group by exception type
        val groups = failed
            .groupBy {
                case (_, _, _, Left(ex)) => ex.getClass.getSimpleName
                case _                   => "Unexpected success"
            }
            .toSeq
            .sortBy(-_._2.length)
        println(s"\nFailures by exception type:")
        groups.foreach { case (exType, failures) =>
            println(s"  $exType: ${failures.length}")
        }
        println()
        groups.foreach { case (exType, failures) =>
            println(exType -> pprint(failures))
        }

        println(s"\n=== Summary ===")
        println(s"Vector names: ${results.length}")
        println(s"Total vectors: ${results.map(_._2.map(_.length).getOrElse(0)).sum}")
        println(s"Failed vectors: ${failed.length}")
        println(
          s"Success rate: ${100.0 * (results.map(_._2.map(_.length).getOrElse(0)).sum - failed.length) / results.map(_._2.map(_.length).getOrElse(0)).sum}%"
        )

        // assert(failed.isEmpty)
    }

    test("MissingOrExtraScriptHashesException") {
        for vectorName <- List(
              "Conway.Imp.AlonzoImpSpec.UTXOW.Valid transactions.PlutusV1.Validating scripts everywhere",
              "Conway.Imp.AlonzoImpSpec.UTXOW.Valid transactions.PlutusV2.Validating scripts everywhere",
              "Conway.Imp.AlonzoImpSpec.UTXOW.Valid transactions.PlutusV3.Validating scripts everywhere"
            )
        do println(vectorName -> pprint(validateVector(vectorName)))
    }

    test("ExactSetOfRedeemersException") {
        for vectorName <- List(
              "Conway.Imp.ConwayImpSpec - Version 10.UTXOS.can use reference scripts",
              "Conway.Imp.ConwayImpSpec - Version 10.UTXOS.can use regular inputs for reference"
            )
        do println(vectorName -> pprint(validateVector(vectorName)))
    }

}
