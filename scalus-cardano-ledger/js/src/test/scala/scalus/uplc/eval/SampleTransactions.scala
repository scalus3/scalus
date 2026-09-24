package scalus.uplc.eval

import scalus.cardano.address.{Address, Network, StakeAddress, StakePayload}
import scalus.cardano.ledger.*
import scalus.cardano.txbuilder.{ScriptSource, TwoArgumentPlutusScriptWitness, TxBuilder}
import scalus.testing.kit.Party.Alice
import scalus.uplc.Constant.given
import scalus.uplc.DefaultFun.Trace
import scalus.uplc.Term
import scalus.uplc.Term.*
import scalus.uplc.TermDSL.given
import scalus.uplc.builtin.{ByteString, Data}
import scalus.uplc.builtin.Data.toData

import scala.language.implicitConversions

/** Transactions the facade tests evaluate, built the way `EmulatorJsTest` builds its failing one.
  * The scripts are hand-written UPLC so their bytes do not move between the Scala versions this
  * repository cross-builds on.
  */
object SampleTransactions {
    given CardanoInfo = CardanoInfo.mainnet

    private val genesisHash = TransactionHash.fromByteString(ByteString.fromHex("0" * 64))
    private val alice = Alice.address(Network.Mainnet)
    private val paymentInput = Input(genesisHash, 0)
    private val paymentOutput = Output(alice, Value.ada(5000))

    /** `λ… . force (trace "boom" (delay error))`: traces once, then fails. */
    private def tracingFailure(params: String*): Term =
        λ(params.head, params.tail*)(!(!Trace $ "boom".asTerm $ ~Error()))

    val failingV3: Script.PlutusV3 =
        Script.PlutusV3(tracingFailure("ctx").plutusV3.cborByteString)
    val failingV2: Script.PlutusV2 =
        Script.PlutusV2(tracingFailure("datum", "redeemer", "ctx").plutusV2.cborByteString)
    val succeedingV3: Script.PlutusV3 =
        Script.PlutusV3(λ("ctx")(()).plutusV3.cborByteString)

    /** Runs and returns `42` instead of unit: valid UPLC, invalid under CIP-117. */
    val returning42V3: Script.PlutusV3 =
        Script.PlutusV3(λ("ctx")(42.asTerm).plutusV3.cborByteString)

    /** A zero-value withdrawal from the stake address `script` controls: one `Reward[0]`. */
    def withdrawal(script: Script.PlutusV3): (Transaction, Utxos) = {
        val stakeAddress = StakeAddress(Network.Mainnet, StakePayload.Script(script.scriptHash))
        val witness =
            TwoArgumentPlutusScriptWitness(ScriptSource.PlutusScriptValue(script), Data.unit)
        // `.draft`, not `.complete`: completing evaluates the scripts to price them.
        val tx = TxBuilder(summon[CardanoInfo])
            .spend(Map(paymentInput -> paymentOutput))
            .withdrawRewards(stakeAddress, Coin.zero, witness)
            .draft
        (tx, Map(paymentInput -> paymentOutput))
    }

    /** Spends an output `failingV2` locks with an inline datum: one `Spend[n]` whose script takes
      * datum, redeemer and context.
      */
    def failingV2Spend: (Transaction, Utxos) = {
        val scriptInput = Input(genesisHash, 1)
        val scriptOutput = Output(
          address = Address(Network.Mainnet, Credential.ScriptHash(failingV2.scriptHash)),
          value = Value.ada(10),
          inlineDatum = 42.toData
        )
        val tx = TxBuilder(summon[CardanoInfo])
            .spend(Map(paymentInput -> paymentOutput))
            .spend(Utxo(scriptInput, scriptOutput), Data.unit, failingV2)
            .draft
        (tx, Map(paymentInput -> paymentOutput, scriptInput -> scriptOutput))
    }
}
