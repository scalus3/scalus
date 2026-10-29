package scalus.examples.usdcx

import org.scalatest.funsuite.AnyFunSuite
import scalus.cardano.ledger.MajorProtocolVersion
import scalus.cardano.onchain.plutus.prelude.*
import scalus.cardano.onchain.plutus.v3.*
import scalus.uplc.Program
import scalus.uplc.builtin.ByteString.hex
import scalus.uplc.builtin.Data.toData
import scalus.uplc.eval.{PlutusVM, Result}

import scala.io.Source

/** Compares [[UsdcxMintingPolicy]] with the script it is recreated from: the Plinth-compiled
  * `minting_policy.flat` of IOG's CardanoLedgerApiBlaster tests (Apache-2.0), kept as
  * `minting_policy_plinth.hex`. Both run on the same contexts, at protocol versions 10 and 11, and
  * must agree on whether they accept them, and on what they trace.
  *
  * They are not the same term. Besides how the two compilers hoist builtins and encode recursion,
  * the original looks the minting logic up again for each withdrawal it compares, where the
  * recreation looks it up once, first. So they accept the same contexts, the original costs more
  * when the minting logic's withdrawal is not the first, and where there is no withdrawal and no
  * protocol parameters, only the original traces (the last test).
  *
  * What is proved of the recreation is in [[UsdcxMintingPolicyVerificationTest]].
  */
class UsdcxMintingPolicyTest extends AnyFunSuite {
    import UsdcxMintingPolicyTest.*

    /** The VMs both scripts run on: the mainnet's, and that of protocol version 10, which the
      * recreation is compiled for.
      */
    private val vms = Seq(
      "PV11" -> PlutusVM.makePlutusV3VM(),
      "PV10" -> PlutusVM.makePlutusV3VM(MajorProtocolVersion.plominPV)
    )

    private lazy val original: Program = Program.fromCborHex(resource("minting_policy_plinth.hex"))

    private lazy val recreated: Program = UsdcxMintingPolicyContract.compiled.program

    private def run(program: Program, ctx: ScriptContext, vm: PlutusVM): Result = {
        given PlutusVM = vm
        (program $ pparamsCs.toData $ ctx.toData).evaluateDebug
    }

    test("the recreation is no larger than the original") {
        info(s"original:  ${original.flatEncoded.length} bytes")
        info(s"recreated: ${recreated.flatEncoded.length} bytes")
        assert(recreated.flatEncoded.length <= original.flatEncoded.length)
    }

    private def agree(name: String, ctx: ScriptContext, accepted: Boolean): Unit = {
        test(s"both ${if accepted then "accept" else "reject"}: $name") {
            for (version, vm) <- vms do
                val expected = run(original, ctx, vm)
                val actual = run(recreated, ctx, vm)
                info(
                  s"$version original:  ${expected.budget}, logs ${expected.logs.mkString("; ")}"
                )
                info(s"$version recreated: ${actual.budget}, logs ${actual.logs.mkString("; ")}")
                assert(expected.isSuccess == accepted, s"$version: $expected")
                assert(actual.isSuccess == accepted, s"$version: $actual")
                assert(actual.logs == expected.logs, version)
        }
    }

    agree("a mint that withdraws from the minting logic", context(), accepted = true)

    agree(
      "the protocol parameters are the last reference input",
      context(referenceInputs = List(otherInput(0), otherInput(1), pparamsInput())),
      accepted = true
    )

    agree(
      "no withdrawal from the minting logic",
      context(withdrawals = List(Credential.ScriptCredential(otherHash))),
      accepted = false
    )

    agree(
      "a withdrawal from a key of the minting logic's hash",
      context(withdrawals = List(Credential.PubKeyCredential(PubKeyHash(mintingLogicHash)))),
      accepted = false
    )

    agree(
      "run as a spending script",
      context(scriptInfo = ScriptInfo.SpendingScript(seedRef)),
      accepted = false
    )

    agree(
      "neither withdrawn from nor minting: both are traced",
      context(withdrawals = List.Nil, scriptInfo = ScriptInfo.SpendingScript(seedRef)),
      accepted = false
    )

    agree(
      "no reference input holds the protocol parameters",
      context(referenceInputs = List(otherInput(0))),
      accepted = false
    )

    agree(
      "the protocol parameters are in a spent input, not a reference input",
      context(inputs = List(seedInput, pparamsInput()), referenceInputs = List(otherInput(0))),
      accepted = false
    )

    agree(
      "the protocol parameters' datum is a hash",
      context(referenceInputs = List(pparamsInput(OutputDatum.OutputDatumHash(datumHash)))),
      accepted = false
    )

    agree(
      "the first input with the protocol parameters has a datum hash, the second the right datum",
      context(referenceInputs =
          List(pparamsInput(OutputDatum.OutputDatumHash(datumHash), 0), pparamsInput(index = 1))
      ),
      accepted = false
    )

    agree(
      "the first input with the protocol parameters names another minting logic",
      context(referenceInputs =
          List(
            pparamsInput(OutputDatum.OutputDatum(pparamsDatum(otherHash)), 0),
            pparamsInput(index = 1)
          )
      ),
      accepted = false
    )

    test("the original looks the minting logic up for each withdrawal it compares") {
        // The ledger sorts the withdrawals: here the minting logic's is the last of three.
        val ctx = context(withdrawals =
            List(lowerHashes(0), lowerHashes(1), mintingLogicHash).map(
              Credential.ScriptCredential(_)
            )
        )
        for (version, vm) <- vms do
            val expected = run(original, ctx, vm)
            val actual = run(recreated, ctx, vm)
            info(s"$version original:  ${expected.budget}")
            info(s"$version recreated: ${actual.budget}")
            assert(expected.isSuccess && actual.isSuccess, version)
            assert(actual.budget.steps < expected.budget.steps, version)
            assert(actual.budget.memory < expected.budget.memory, version)
    }

    test("with no withdrawal and no protocol parameters, only the original traces") {
        // Both reject. The original finds no withdrawal to compare, so never looks for the
        // protocol parameters, and traces that the minting logic is not withdrawn from. The
        // recreation looks for them first, finds none, and fails without a trace.
        val ctx = context(referenceInputs = List(otherInput(0)), withdrawals = List.Nil)
        for (version, vm) <- vms do
            val expected = run(original, ctx, vm)
            val actual = run(recreated, ctx, vm)
            assert(expected.isFailure && actual.isFailure, version)
            assert(expected.logs == Seq(UsdcxMintingPolicy.NoMintingLogicInWithdrawals), version)
            assert(actual.logs.isEmpty, version)
    }
}

object UsdcxMintingPolicyTest {
    val pparamsCs: ByteString = ByteString.fromHex("11" * 28)
    val mintingLogicHash: ByteString = ByteString.fromHex("22" * 28)
    val otherHash: ByteString = ByteString.fromHex("33" * 28)
    val ownPolicy: ByteString = ByteString.fromHex("44" * 28)

    /** Script hashes that sort before the minting logic's. */
    val lowerHashes: Seq[ByteString] = Seq("0a", "0b").map(b => ByteString.fromHex(b * 28))

    val datumHash: ByteString = ByteString.fromHex("00" * 32)

    private val address =
        Address(Credential.PubKeyCredential(PubKeyHash(ByteString.fromHex("66" * 28))), Option.None)

    val seedRef: TxOutRef = TxOutRef(TxId(ByteString.fromHex("55" * 32)), 0)

    val seedInput: TxInInfo = TxInInfo(seedRef, TxOut(address, Value.lovelace(10_000_000)))

    /** The protocol parameters, as a datum: the minting logic's hash first. */
    def pparamsDatum(mintingLogic: ByteString): Data =
        Data.Constr(0, List(Data.B(mintingLogic), Data.I(42)))

    /** An output with the protocol parameters NFT. Its reference sorts after [[otherInput]]'s. */
    def pparamsInput(
        datum: OutputDatum = OutputDatum.OutputDatum(pparamsDatum(mintingLogicHash)),
        index: BigInt = 0
    ): TxInInfo = {
        TxInInfo(
          TxOutRef(TxId(ByteString.fromHex("a7" * 32)), index),
          TxOut(
            address,
            Value.lovelace(2_000_000) + Value(pparamsCs, ByteString.fromHex(""), 1),
            datum
          )
        )
    }

    /** An output with a token of another policy. */
    def otherInput(index: BigInt): TxInInfo = {
        TxInInfo(
          TxOutRef(TxId(ByteString.fromHex("88" * 32)), index),
          TxOut(address, Value.lovelace(5_000_000) + Value(otherHash, hex"01", 7))
        )
    }

    /** A context of the policy's own mint. The withdrawals are sorted as the ledger sorts them; the
      * inputs and the reference inputs are given in their order.
      */
    def context(
        inputs: List[TxInInfo] = List(seedInput),
        referenceInputs: List[TxInInfo] = List(pparamsInput()),
        withdrawals: List[Credential] = List(Credential.ScriptCredential(mintingLogicHash)),
        scriptInfo: ScriptInfo = ScriptInfo.MintingScript(ownPolicy)
    ): ScriptContext = {
        ScriptContext(
          txInfo = TxInfo(
            inputs = inputs,
            referenceInputs = referenceInputs,
            mint = Value(ownPolicy, hex"55534443", 100),
            withdrawals =
                SortedMap.fromList(withdrawals.map(credential => (credential, BigInt(0)))),
            id = TxId(ByteString.fromHex("99" * 32))
          ),
          scriptInfo = scriptInfo
        )
    }

    def resource(name: String): String = {
        val source = Source.fromResource(s"scalus/examples/usdcx/$name")
        try source.mkString.trim
        finally source.close()
    }
}
