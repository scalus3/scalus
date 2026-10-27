package scalus.cardano.ledger

import org.scalatest.funsuite.AnyFunSuite
import scalus.cardano.address.Network
import scalus.cardano.ledger.rules.{Context, PlutusScriptsTransactionMutator, State, UtxoEnv, ValidatorRulesTestKit}
import scalus.serialization.cbor.Cbor
import scalus.serialization.flat.FlatDecodingError
import scalus.uplc.Term.*
import scalus.uplc.builtin.ByteString
import scalus.uplc.{Constant, Program, ProgramFlatCodec}

/** A script the ledger cannot deserialise fails phase 1 from Babbage on, before any script runs:
  * the evaluator names the script and does not throw a [[PlutusScriptEvaluationException]], which
  * its callers read as a phase 2 failure. Before Babbage the ledger had no phase 1 check, and such
  * a script failed in phase 2.
  */
class PlutusScriptDecodeTest extends AnyFunSuite, ValidatorRulesTestKit {

    private val unit = Const(Constant.Unit)

    /** `\ctx -> ()` */
    private val v3Flat = ProgramFlatCodec.unsafeEncodeFlat(Program.plutusV3(LamAbs("ctx", unit)))

    /** `\redeemer ctx -> ()` */
    private val v1Flat =
        ProgramFlatCodec.unsafeEncodeFlat(Program.plutusV1(LamAbs("redeemer", LamAbs("ctx", unit))))

    private def exactly(flat: Array[Byte]): ByteString = ByteString.fromArray(Cbor.encode(flat))

    /** The program and one byte after its filler, in a CBOR byte string. */
    private def withByteAfter(flat: Array[Byte]): ByteString =
        ByteString.fromArray(Cbor.encode(flat :+ 0.toByte))

    private def evaluate(plutusScript: PlutusScript, protocolMajor: Int): Seq[Redeemer] = {
        val (tx, utxos) = mintTx(plutusScript)
        PlutusScriptEvaluator(
          slotConfig = CardanoInfo.mainnet.slotConfig,
          initialBudget = CardanoInfo.mainnet.protocolParams.maxTxExecutionUnits,
          protocolMajorVersion = MajorProtocolVersion(protocolMajor),
          costModels = CardanoInfo.mainnet.protocolParams.costModels
        ).evalPlutusScripts(tx, utxos)
    }

    /** Runs the Plutus scripts rule on the minting transaction, marked invalid. */
    private def applyMarkedInvalid(plutusScript: PlutusScript, protocolMajor: Int) = {
        val (tx, utxos) = mintTx(plutusScript)
        val params = CardanoInfo.mainnet.protocolParams
        val context = Context(
          env = UtxoEnv(
            0,
            params.copy(protocolVersion = ProtocolVersion(protocolMajor, 0)),
            CertState.empty,
            Network.Mainnet,
            Coin.zero
          ),
          slotConfig = CardanoInfo.mainnet.slotConfig
        )
        PlutusScriptsTransactionMutator.transit(
          context,
          State(utxos = utxos),
          tx.copy(isValid = false)
        )
    }

    test("a script that decodes evaluates, before and after Babbage") {
        assert(evaluate(Script.PlutusV3(exactly(v3Flat)), 10).size == 1)
        assert(evaluate(Script.PlutusV1(exactly(v1Flat)), 6).size == 1)
    }

    test("from Babbage on, a script with a byte after its program fails to decode, in phase 1") {
        val script = Script.PlutusV3(withByteAfter(v3Flat))
        val e = intercept[Exception](evaluate(script, 10))
        assert(e.isInstanceOf[IllegalArgumentException])
        assert(!e.isInstanceOf[PlutusScriptEvaluationException])
        assert(e.getMessage.contains(script.scriptHash.toHex))
        assert(e.getCause.isInstanceOf[FlatDecodingError])
        // a transaction marked invalid is rejected, so its collateral is not collected
        applyMarkedInvalid(script, 10) match
            case Left(e: TransactionException.IllegalArgumentException) =>
                assert(e.getMessage.contains(script.scriptHash.toHex))
            case other => fail(s"expected a phase 1 failure, got $other")
    }

    test("before Babbage, a script with a byte after its program fails in phase 2") {
        val script = Script.PlutusV1(withByteAfter(v1Flat))
        val e = intercept[PlutusScriptEvaluationException](evaluate(script, 6))
        assert(e.failedScriptHash == script.scriptHash)
        // a transaction marked invalid is applied, with its collateral collected
        assert(applyMarkedInvalid(script, 6).isRight)
    }
}
