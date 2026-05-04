package scalus.utxocells

import org.scalatest.funsuite.AnyFunSuite
import scalus.compiler.Compile
import scalus.compiler.Options
import scalus.uplc.builtin.Data
import scalus.uplc.builtin.Data.toData
import scalus.uplc.builtin.ByteString
import scalus.uplc.builtin.ByteString.utf8
import scalus.uplc.builtin.{FromData, ToData}
import scalus.uplc.PlutusV3
import scalus.cardano.onchain.plutus.v1.{PolicyId, PubKeyHash, Value}
import scalus.cardano.onchain.plutus.v2.{OutputDatum, TxOut}
import scalus.cardano.onchain.plutus.v3.*
import scalus.cardano.onchain.plutus.prelude.*

// -- Counter state --
case class CounterState(count: BigInt) derives FromData, ToData

@Compile
object CounterState {
    given Eq[CounterState] = (a: CounterState, b: CounterState) => a.count === b.count
}

// -- Counter action --
enum CounterAction:
    case Increment
    case Decrement
    case Reset

@Compile
object CounterAction {
    given FromData[CounterAction] = FromData.derived
    given ToData[CounterAction] = ToData.derived
}

// -- Counter cell: extends Validator directly, uses UtxoCellLib helpers --
@Compile
object CounterCell extends Validator {

    val beaconName: ByteString = utf8"counter"

    def transition(
        state: CounterState,
        action: CounterAction
    ): UtxoCellTransition[CounterState] = {
        val nextState = action match
            case CounterAction.Increment =>
                Option.Some(CounterState(state.count + BigInt(1)))
            case CounterAction.Decrement =>
                require(state.count > BigInt(0), "Counter: cannot decrement below zero")
                Option.Some(CounterState(state.count - BigInt(1)))
            case CounterAction.Reset =>
                Option.None
        UtxoCellTransition(nextState, List.Nil)
    }

    inline override def spend(
        datum: Option[Data],
        redeemer: Data,
        tx: TxInfo,
        ownRef: TxOutRef
    ): Unit = {
        val state = datum.getOrFail("CounterCell: missing datum").to[CounterState]
        val action = redeemer.to[CounterAction]
        val result = transition(state, action)
        UtxoCellLib.verifySpendResult(result, tx, ownRef)
        result.nextState match
            case Option.None =>
                val ownInput = tx.findOwnInputOrFail(ownRef)
                val policyId = ownInput.resolved.address.credential match
                    case Credential.ScriptCredential(hash) => hash
                    case _ => fail("CounterCell: input is not a script")
                UtxoCellLib.verifyBurnBeacon(beaconName, policyId, tx)
            case _ => ()
    }

    inline override def mint(
        redeemer: Data,
        policyId: PolicyId,
        tx: TxInfo
    ): Unit = {
        val qty = tx.mint.quantityOf(policyId, beaconName)
        if qty === BigInt(1) then
            val initialState = CounterState(BigInt(0))
            UtxoCellLib.verifyMintResult(initialState, beaconName, policyId, tx)
        else if qty === BigInt(-1) then UtxoCellLib.verifyBurnBeacon(beaconName, policyId, tx)
        else fail("CounterCell: invalid beacon mint quantity")
    }
}

// -- Compilation --
object CounterCellCompilation {
    given Options = Options.debug

    lazy val compiled = PlutusV3.compile(CounterCell.validate)
}

// -- Counter cell V2: uses CellValidator + transitionSpend/transitionMint helpers --
@Compile
object CounterCellV2 extends CellValidator {

    val beaconName: ByteString = utf8"counter"

    def initialState(redeemer: Data): CounterState = CounterState(BigInt(0))

    def transition(
        state: CounterState,
        action: CounterAction,
        ctx: CellContext
    ): Option[CounterState] =
        action match
            case CounterAction.Increment =>
                Option.Some(CounterState(state.count + BigInt(1)))
            case CounterAction.Decrement =>
                require(state.count > BigInt(0), "Counter: cannot decrement below zero")
                Option.Some(CounterState(state.count - BigInt(1)))
            case CounterAction.Reset =>
                Option.None

    inline override def spendCell(
        datum: Option[Data],
        redeemer: Data,
        sc: ScriptContext,
        ownRef: TxOutRef
    ): Unit = UtxoCellLib.transitionSpend(beaconName, transition, datum, redeemer, sc, ownRef)

    inline override def mintCell(
        redeemer: Data,
        policyId: PolicyId,
        sc: ScriptContext
    ): Unit = UtxoCellLib.transitionMint(beaconName, initialState, redeemer, policyId, sc)
}

object CounterCellV2Compilation {
    given Options = Options.debug

    lazy val compiled = PlutusV3.compile(CounterCellV2.validate)
}

class CounterCellSpec extends AnyFunSuite {

    private given scalus.uplc.eval.PlutusVM = scalus.uplc.eval.PlutusVM.makePlutusV3VM()

    private val scriptHash =
        ByteString.fromHex("aabbccddaabbccddaabbccddaabbccddaabbccddaabbccddaabbccdd")
    private val ownOutRef = TxOutRef(
      id = TxId(
        ByteString.fromHex("1234567890abcdef1234567890abcdef1234567890abcdef1234567890abcdef")
      ),
      idx = 0
    )

    test("CounterCell compiles to UPLC") {
        val compiled = CounterCellCompilation.compiled
        assert(compiled.program.term != null)
        info(s"UPLC program version: ${compiled.program.version}")
        info(s"Script size: ${compiled.script.script.size} bytes")
    }

    test("CounterCell transition logic works off-chain") {
        val state = CounterState(BigInt(5))
        val result = CounterCell.transition(state, CounterAction.Increment)
        assert(result.nextState == Option.Some(CounterState(BigInt(6))))

        val result2 = CounterCell.transition(state, CounterAction.Reset)
        assert(result2.nextState == Option.None)

        val result3 = CounterCell.transition(state, CounterAction.Decrement)
        assert(result3.nextState == Option.Some(CounterState(BigInt(4))))
    }

    test("CounterCell UPLC evaluates correctly with valid ScriptContext") {
        val compiled = CounterCellCompilation.compiled.withErrorTraces
        val currentState = CounterState(BigInt(5))
        val expectedNextState = CounterState(BigInt(6))

        val scriptContext = makeSpendingContext(
          datum = currentState.toData,
          redeemer = CounterAction.Increment.toData,
          nextStateDatum = expectedNextState.toData
        )

        val applied = compiled.program $ scriptContext.toData
        val result = applied.evaluateDebug
        assert(result.isSuccess, s"UPLC evaluation failed: $result")
    }

    test("CounterCell UPLC fails with wrong next state datum") {
        val compiled = CounterCellCompilation.compiled.withErrorTraces
        val currentState = CounterState(BigInt(5))
        val wrongNextState = CounterState(BigInt(99)) // wrong!

        val scriptContext = makeSpendingContext(
          datum = currentState.toData,
          redeemer = CounterAction.Increment.toData,
          nextStateDatum = wrongNextState.toData
        )

        val applied = compiled.program $ scriptContext.toData
        val result = applied.evaluateDebug
        assert(!result.isSuccess, "Should have failed with wrong datum")
    }

    test("CounterCell UPLC fails on decrement below zero") {
        val compiled = CounterCellCompilation.compiled.withErrorTraces
        val currentState = CounterState(BigInt(0))

        val scriptContext = makeSpendingContext(
          datum = currentState.toData,
          redeemer = CounterAction.Decrement.toData,
          nextStateDatum = CounterState(BigInt(-1)).toData
        )

        val applied = compiled.program $ scriptContext.toData
        val result = applied.evaluateDebug
        assert(!result.isSuccess, "Should have failed on decrement below zero")
    }

    // -- Mint tests --

    test("CounterCell mint UPLC evaluates correctly") {
        val compiled = CounterCellCompilation.compiled.withErrorTraces
        val initialState = CounterState(BigInt(0))

        val scriptContext = makeMintingContext(
          redeemer = Data.I(0),
          initialStateDatum = initialState.toData,
          mintBeacon = true
        )

        val applied = compiled.program $ scriptContext.toData
        val result = applied.evaluateDebug
        assert(result.isSuccess, s"UPLC mint evaluation failed: $result")
    }

    test("CounterCell mint fails without beacon") {
        val compiled = CounterCellCompilation.compiled.withErrorTraces
        val initialState = CounterState(BigInt(0))

        val scriptContext = makeMintingContext(
          redeemer = Data.I(0),
          initialStateDatum = initialState.toData,
          mintBeacon = false
        )

        val applied = compiled.program $ scriptContext.toData
        val result = applied.evaluateDebug
        assert(!result.isSuccess, "Should have failed without beacon mint")
    }

    test("CounterCell mint fails with wrong initial datum") {
        val compiled = CounterCellCompilation.compiled.withErrorTraces
        val wrongState = CounterState(BigInt(42))

        val scriptContext = makeMintingContext(
          redeemer = Data.I(0),
          initialStateDatum = wrongState.toData,
          mintBeacon = true
        )

        val applied = compiled.program $ scriptContext.toData
        val result = applied.evaluateDebug
        assert(!result.isSuccess, "Should have failed with wrong initial datum")
    }

    // -- Spend terminal (Reset) tests --

    test("CounterCell UPLC Reset burns beacon and terminates") {
        val compiled = CounterCellCompilation.compiled.withErrorTraces
        val currentState = CounterState(BigInt(5))

        val scriptContext = makeTerminalSpendContext(
          datum = currentState.toData,
          redeemer = CounterAction.Reset.toData
        )

        val applied = compiled.program $ scriptContext.toData
        val result = applied.evaluateDebug
        assert(result.isSuccess, s"UPLC terminal spend failed: $result")
    }

    private def makeSpendingContext(
        datum: Data,
        redeemer: Data,
        nextStateDatum: Data
    ): ScriptContext = {
        val scriptAddress = Address(Credential.ScriptCredential(scriptHash), Option.None)
        val ownInput = TxInInfo(
          outRef = ownOutRef,
          resolved = TxOut(
            address = scriptAddress,
            value = Value.lovelace(BigInt(10_000_000))
          )
        )
        val continuingOutput = TxOut(
          address = scriptAddress,
          value = Value.lovelace(BigInt(10_000_000)),
          datum = OutputDatum.OutputDatum(nextStateDatum)
        )
        ScriptContext(
          txInfo = TxInfo(
            inputs = List(ownInput),
            outputs = List(continuingOutput),
            fee = 200_000,
            id = TxId(
              ByteString.fromHex(
                "deadbeefdeadbeefdeadbeefdeadbeefdeadbeefdeadbeefdeadbeefdeadbeef"
              )
            )
          ),
          redeemer = redeemer,
          scriptInfo = ScriptInfo.SpendingScript(ownOutRef, Option.Some(datum))
        )
    }

    private def makeMintingContext(
        redeemer: Data,
        initialStateDatum: Data,
        mintBeacon: Boolean
    ): ScriptContext = {
        val policyId = scriptHash
        val scriptAddress = Address(Credential.ScriptCredential(policyId), Option.None)
        val beaconName = CounterCell.beaconName

        val mintValue =
            if mintBeacon then Value(policyId, beaconName, BigInt(1))
            else Value.zero

        val output = TxOut(
          address = scriptAddress,
          value = Value.lovelace(BigInt(10_000_000)) + Value(policyId, beaconName, BigInt(1)),
          datum = OutputDatum.OutputDatum(initialStateDatum)
        )

        ScriptContext(
          txInfo = TxInfo(
            inputs = List.Nil,
            outputs = List(output),
            fee = 200_000,
            mint = mintValue,
            id = TxId(
              ByteString.fromHex(
                "deadbeefdeadbeefdeadbeefdeadbeefdeadbeefdeadbeefdeadbeefdeadbeef"
              )
            )
          ),
          redeemer = redeemer,
          scriptInfo = ScriptInfo.MintingScript(policyId)
        )
    }

    private def makeTerminalSpendContext(
        datum: Data,
        redeemer: Data
    ): ScriptContext = {
        val scriptAddress = Address(Credential.ScriptCredential(scriptHash), Option.None)
        val ownInput = TxInInfo(
          outRef = ownOutRef,
          resolved = TxOut(
            address = scriptAddress,
            value = Value.lovelace(BigInt(10_000_000))
          )
        )
        ScriptContext(
          txInfo = TxInfo(
            inputs = List(ownInput),
            outputs = List.Nil,
            fee = 200_000,
            mint = Value(scriptHash, CounterCell.beaconName, BigInt(-1)),
            id = TxId(
              ByteString.fromHex(
                "deadbeefdeadbeefdeadbeefdeadbeefdeadbeefdeadbeefdeadbeefdeadbeef"
              )
            )
          ),
          redeemer = redeemer,
          scriptInfo = ScriptInfo.SpendingScript(ownOutRef, Option.Some(datum))
        )
    }

    // -- UtxoCellOutput verification tests --

    private val recipientPkh = PubKeyHash(
      ByteString.fromHex(
        "1122334411223344112233441122334411223344112233441122334411223344".take(56)
      )
    )
    private val recipientAddress = Address.fromPubKeyHash(recipientPkh)

    private val dummyTxId = TxId(
      ByteString.fromHex("deadbeefdeadbeefdeadbeefdeadbeefdeadbeefdeadbeefdeadbeefdeadbeef")
    )

    private def txInfoWithOutputs(outputs: List[TxOut]): TxInfo =
        TxInfo(inputs = List.Nil, outputs = outputs, fee = 200_000, id = dummyTxId)

    test("UtxoCellLib.verifyOutputs passes when outputs match exactly") {
        val expected = UtxoCellOutput(recipientAddress, Value.lovelace(BigInt(5_000_000)))
        val txOutput = TxOut(address = recipientAddress, value = Value.lovelace(BigInt(5_000_000)))
        UtxoCellLib.verifyOutputs(List(expected), txInfoWithOutputs(List(txOutput)))
    }

    test("UtxoCellLib.verifyOutputs passes when output has more value than expected") {
        val expected = UtxoCellOutput(recipientAddress, Value.lovelace(BigInt(5_000_000)))
        val txOutput = TxOut(address = recipientAddress, value = Value.lovelace(BigInt(10_000_000)))
        UtxoCellLib.verifyOutputs(List(expected), txInfoWithOutputs(List(txOutput)))
    }

    test("UtxoCellLib.verifyOutputs fails when output is missing") {
        val expected = UtxoCellOutput(recipientAddress, Value.lovelace(BigInt(5_000_000)))
        val ex = intercept[Exception] {
            UtxoCellLib.verifyOutputs(List(expected), txInfoWithOutputs(List.Nil))
        }
        assert(ex.getMessage.contains("expected output not found"))
    }

    test("UtxoCellLib.verifyOutputs fails when output has wrong address") {
        val expected = UtxoCellOutput(recipientAddress, Value.lovelace(BigInt(5_000_000)))
        val wrongAddress = Address.fromScriptHash(scriptHash)
        val txOutput = TxOut(address = wrongAddress, value = Value.lovelace(BigInt(5_000_000)))
        val ex = intercept[Exception] {
            UtxoCellLib.verifyOutputs(List(expected), txInfoWithOutputs(List(txOutput)))
        }
        assert(ex.getMessage.contains("expected output not found"))
    }

    test("UtxoCellLib.verifyOutputs fails when output has insufficient value") {
        val expected = UtxoCellOutput(recipientAddress, Value.lovelace(BigInt(5_000_000)))
        val txOutput = TxOut(address = recipientAddress, value = Value.lovelace(BigInt(3_000_000)))
        val ex = intercept[Exception] {
            UtxoCellLib.verifyOutputs(List(expected), txInfoWithOutputs(List(txOutput)))
        }
        assert(ex.getMessage.contains("expected output not found"))
    }

    test("UtxoCellLib.verifyOutputs passes with empty outputs list") {
        UtxoCellLib.verifyOutputs(List.Nil, txInfoWithOutputs(List.Nil))
    }

    test("UtxoCellTransition carries outputs") {
        val output = UtxoCellOutput(recipientAddress, Value.lovelace(BigInt(5_000_000)))
        val transition = UtxoCellTransition(
          nextState = Option.Some(CounterState(BigInt(1))),
          outputs = List(output)
        )
        assert(transition.outputs.length === BigInt(1))
        assert(transition.outputs.head.address === recipientAddress)
    }

    // -- CellValidator (V2) tests --

    test("CounterCellV2 compiles to UPLC") {
        val compiled = CounterCellV2Compilation.compiled
        assert(compiled.program.term != null)
        info(s"V2 script size: ${compiled.script.script.size} bytes")
    }

    test("CounterCellV2 UPLC evaluates correctly with valid ScriptContext") {
        val compiled = CounterCellV2Compilation.compiled.withErrorTraces
        val currentState = CounterState(BigInt(5))
        val expectedNextState = CounterState(BigInt(6))

        val scriptContext = makeSpendingContext(
          datum = currentState.toData,
          redeemer = CounterAction.Increment.toData,
          nextStateDatum = expectedNextState.toData
        )

        val applied = compiled.program $ scriptContext.toData
        val result = applied.evaluateDebug
        assert(result.isSuccess, s"V2 UPLC evaluation failed: $result")
    }

    test("CounterCellV2 UPLC fails with wrong next state datum") {
        val compiled = CounterCellV2Compilation.compiled.withErrorTraces
        val currentState = CounterState(BigInt(5))
        val wrongNextState = CounterState(BigInt(99))

        val scriptContext = makeSpendingContext(
          datum = currentState.toData,
          redeemer = CounterAction.Increment.toData,
          nextStateDatum = wrongNextState.toData
        )

        val applied = compiled.program $ scriptContext.toData
        val result = applied.evaluateDebug
        assert(!result.isSuccess, "V2 should have failed with wrong datum")
    }

    test("CounterCellV2 UPLC Reset burns beacon and terminates") {
        val compiled = CounterCellV2Compilation.compiled.withErrorTraces
        val currentState = CounterState(BigInt(5))

        val scriptContext = makeTerminalSpendContext(
          datum = currentState.toData,
          redeemer = CounterAction.Reset.toData
        )

        val applied = compiled.program $ scriptContext.toData
        val result = applied.evaluateDebug
        assert(result.isSuccess, s"V2 terminal spend failed: $result")
    }

    test("CounterCellV2 mint UPLC evaluates correctly") {
        val compiled = CounterCellV2Compilation.compiled.withErrorTraces
        val initialState = CounterState(BigInt(0))

        val scriptContext = makeMintingContext(
          redeemer = Data.I(0),
          initialStateDatum = initialState.toData,
          mintBeacon = true
        )

        val applied = compiled.program $ scriptContext.toData
        val result = applied.evaluateDebug
        assert(result.isSuccess, s"V2 mint evaluation failed: $result")
    }
}
