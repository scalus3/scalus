package scalus.cardano.node

import org.scalatest.funsuite.AnyFunSuite
import scalus.uplc.builtin.ByteString
import scalus.cardano.ledger.*

class SubmitErrorTest extends AnyFunSuite {

    val sampleTxHash: TransactionHash =
        TransactionHash.fromByteString(ByteString.fromHex("a" * 64))

    // HTTP Response Classification Tests

    test("fromHttpResponse classifies HTTP 402 as RateLimited") {
        val error = SubmitError.fromHttpResponse(402, "Usage limit exceeded")
        assert(error.isInstanceOf[SubmitError.RateLimited])
        assert(error.asInstanceOf[SubmitError.RateLimited].message == "Usage limit exceeded")
    }

    test("fromHttpResponse classifies HTTP 429 as RateLimited") {
        val error = SubmitError.fromHttpResponse(429, "Too many requests")
        assert(error.isInstanceOf[SubmitError.RateLimited])
    }

    test("fromHttpResponse classifies HTTP 403 as AuthenticationError") {
        val error = SubmitError.fromHttpResponse(403, "Invalid API key")
        assert(error.isInstanceOf[SubmitError.AuthenticationError])
    }

    test("fromHttpResponse classifies HTTP 418 as Banned") {
        val error = SubmitError.fromHttpResponse(418, "IP auto-banned")
        assert(error.isInstanceOf[SubmitError.Banned])
    }

    test("fromHttpResponse classifies HTTP 425 as MempoolFull") {
        val error = SubmitError.fromHttpResponse(425, "Mempool full")
        assert(error.isInstanceOf[SubmitError.MempoolFull])
    }

    test("fromHttpResponse classifies HTTP 500 as InternalError") {
        val error = SubmitError.fromHttpResponse(500, "Internal server error")
        assert(error.isInstanceOf[SubmitError.InternalError])
    }

    test("fromHttpResponse classifies HTTP 503 as InternalError") {
        val error = SubmitError.fromHttpResponse(503, "Service unavailable")
        assert(error.isInstanceOf[SubmitError.InternalError])
    }

    // Validation Error Parsing Tests

    test("parseValidationError classifies BadInputsUTxO as UtxoNotAvailable") {
        val message = "BadInputsUTxO (fromList [])"
        val error = SubmitError.parseValidationError(message)
        assert(error.isInstanceOf[SubmitError.UtxoNotAvailable])
    }

    test("parseValidationError classifies 'bad inputs' as UtxoNotAvailable") {
        val message = "Transaction has bad inputs"
        val error = SubmitError.parseValidationError(message)
        assert(error.isInstanceOf[SubmitError.UtxoNotAvailable])
    }

    test("parseValidationError extracts transaction inputs from error message") {
        val txHash = "a" * 64
        val message = s"BadInputsUTxO ($txHash#0, $txHash#1)"
        val error = SubmitError.parseValidationError(message)

        error match
            case SubmitError.UtxoNotAvailable(_, inputs) =>
                assert(inputs.size == 2)
                assert(inputs.exists(_.index == 0))
                assert(inputs.exists(_.index == 1))
            case _ => fail(s"Expected UtxoNotAvailable, got $error")
    }

    test("parseValidationError classifies OutsideValidityInterval as TransactionExpired") {
        val message = "OutsideValidityIntervalUTxO (ValidityInterval ...)"
        val error = SubmitError.parseValidationError(message)
        assert(error.isInstanceOf[SubmitError.TransactionExpired])
    }

    test("parseValidationError classifies 'expired' as TransactionExpired") {
        val message = "Transaction has expired"
        val error = SubmitError.parseValidationError(message)
        assert(error.isInstanceOf[SubmitError.TransactionExpired])
    }

    test("parseValidationError classifies 'outside validity' as TransactionExpired") {
        val message = "Transaction is outside the validity interval"
        val error = SubmitError.parseValidationError(message)
        assert(error.isInstanceOf[SubmitError.TransactionExpired])
    }

    test("parseValidationError classifies ValueNotConserved as ValueNotConserved") {
        val message = "ValueNotConservedUTxO (consumed: 10 ADA, produced: 15 ADA)"
        val error = SubmitError.parseValidationError(message)
        assert(error.isInstanceOf[SubmitError.ValueNotConserved])
    }

    test("parseValidationError classifies 'value not conserved' as ValueNotConserved") {
        val message = "value not conserved in transaction"
        val error = SubmitError.parseValidationError(message)
        assert(error.isInstanceOf[SubmitError.ValueNotConserved])
    }

    test("parseValidationError classifies script failure as ScriptFailure") {
        val message = "script failed: evaluation error"
        val error = SubmitError.parseValidationError(message)
        assert(error.isInstanceOf[SubmitError.ScriptFailure])
    }

    test("parseValidationError classifies script error as ScriptFailure") {
        val message = "plutus script error"
        val error = SubmitError.parseValidationError(message)
        assert(error.isInstanceOf[SubmitError.ScriptFailure])
    }

    test("parseValidationError classifies unknown errors as ValidationError") {
        val message = "Some unknown validation error"
        val error = SubmitError.parseValidationError(message)
        assert(error.isInstanceOf[SubmitError.ValidationError])
    }

    // TransactionException Mapping Tests

    test("fromException maps BadAllInputsUTxOException to UtxoNotAvailable with inputs") {
        val missingInputs = Set(
          TransactionInput(sampleTxHash, 0),
          TransactionInput(sampleTxHash, 1)
        )
        val ex = TransactionException.BadAllInputsUTxOException(
          sampleTxHash,
          missingInputs,
          missingCollateralInputs = Set.empty,
          missingReferenceInputs = Set.empty
        )

        val error = SubmitError.fromException(ex)
        error match
            case SubmitError.UtxoNotAvailable(_, inputs) =>
                assert(inputs == missingInputs)
            case _ => fail(s"Expected UtxoNotAvailable, got $error")
    }

    test("fromException maps BadAllInputsUTxOException with collateral inputs") {
        val missingCollateral = Set(TransactionInput(sampleTxHash, 2))
        val ex = TransactionException.BadAllInputsUTxOException(
          sampleTxHash,
          missingInputs = Set.empty,
          missingCollateralInputs = missingCollateral,
          missingReferenceInputs = Set.empty
        )

        val error = SubmitError.fromException(ex)
        error match
            case SubmitError.UtxoNotAvailable(_, inputs) =>
                assert(inputs == missingCollateral)
            case _ => fail(s"Expected UtxoNotAvailable, got $error")
    }

    test("fromException maps BadInputsUTxOException to UtxoNotAvailable") {
        val ex = TransactionException.BadInputsUTxOException(sampleTxHash)
        val error = SubmitError.fromException(ex)
        assert(error.isInstanceOf[SubmitError.UtxoNotAvailable])
    }

    test("fromException maps BadCollateralInputsUTxOException to UtxoNotAvailable") {
        val ex = TransactionException.BadCollateralInputsUTxOException(sampleTxHash)
        val error = SubmitError.fromException(ex)
        assert(error.isInstanceOf[SubmitError.UtxoNotAvailable])
    }

    test("fromException maps BadReferenceInputsUTxOException to UtxoNotAvailable") {
        val ex = TransactionException.BadReferenceInputsUTxOException(sampleTxHash)
        val error = SubmitError.fromException(ex)
        assert(error.isInstanceOf[SubmitError.UtxoNotAvailable])
    }

    test("fromException maps OutsideValidityIntervalException to TransactionExpired") {
        val ex = TransactionException.OutsideValidityIntervalException(
          sampleTxHash,
          ValidityInterval(Some(0), Some(100)),
          slot = 200
        )
        val error = SubmitError.fromException(ex)
        assert(error.isInstanceOf[SubmitError.TransactionExpired])
    }

    test("fromException maps ValueNotConservedUTxOException to ValueNotConserved") {
        val ex = TransactionException.ValueNotConservedUTxOException(
          sampleTxHash,
          consumed = Value.ada(100),
          produced = Value.ada(150)
        )
        val error = SubmitError.fromException(ex)
        assert(error.isInstanceOf[SubmitError.ValueNotConserved])
    }

    test("fromException maps NativeScriptsException to ScriptFailure") {
        val ex = TransactionException.NativeScriptsException(
          sampleTxHash,
          invalidWitnessesNativeScripts = Set.empty,
          invalidProvidedReferenceNativeScripts = Set.empty
        )
        val error = SubmitError.fromException(ex)
        assert(error.isInstanceOf[SubmitError.ScriptFailure])
    }

    test("fromException maps unknown TransactionException to ValidationError") {
        val ex = TransactionException.EmptyInputsException(sampleTxHash)
        val error = SubmitError.fromException(ex)
        assert(error.isInstanceOf[SubmitError.ValidationError])
    }

    // Integration with Emulator

    test("Emulator returns UtxoNotAvailable for double spend") {
        import scalus.testing.kit.Party.{Alice, Bob}
        import scalus.cardano.txbuilder.TxBuilder
        import scalus.cardano.ledger.rules.AllInputsMustBeInUtxoValidator
        import scalus.utils.await

        given testEnv: CardanoInfo = CardanoInfo.mainnet
        val genesisHash = TransactionHash.fromByteString(ByteString.fromHex("0" * 64))

        val initialUtxos = Map(
          TransactionInput(genesisHash, 0) -> TransactionOutput.Babbage(
            Alice.address,
            Value.ada(100)
          )
        )

        // Use AllInputsMustBeInUtxoValidator to properly detect missing inputs
        val emulator = Emulator(
          initialUtxos = initialUtxos,
          validators = Set(AllInputsMustBeInUtxoValidator),
          mutators = Emulator.defaultMutators
        )

        val tx = TxBuilder(testEnv)
            .payTo(Bob.address, Value.ada(10))
            .complete(emulator, Alice.address)
            .await()
            .transaction

        // First submit should succeed
        val submit1 = emulator.submit(tx).await()
        assert(submit1.isRight, s"First transaction should succeed: $submit1")

        // Second submit (double spend) should return UtxoNotAvailable
        val submit2 = emulator.submit(tx).await()
        submit2 match
            case Left(SubmitError.UtxoNotAvailable(_, inputs)) =>
                // The spent input should be in the unavailable inputs set
                assert(inputs.nonEmpty, "Should report unavailable inputs")
            case Left(other) =>
                fail(s"Expected UtxoNotAvailable, got $other")
            case Right(_) =>
                fail("Double spend should be rejected")
    }
}
