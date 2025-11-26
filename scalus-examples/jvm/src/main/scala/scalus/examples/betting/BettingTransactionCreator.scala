package scalus.examples.betting

import scalus.builtin.ByteString.{hex, utf8}
import scalus.builtin.Data
import scalus.builtin.Data.toData
import scalus.cardano.address.{Address, ShelleyAddress, ShelleyDelegationPart, ShelleyPaymentPart}
import scalus.cardano.blueprint.PlutusV3CompiledContract
import scalus.cardano.ledger.*
import scalus.cardano.txbuilder.{Environment, TransactionSigner, TxBuilder}
import scalus.ledger.api.v1.{PosixTime, PubKeyHash}

case class BettingTransactionCreator(
    env: Environment,
    evaluator: PlutusScriptEvaluator,
    signer: TransactionSigner,
    compiledContract: PlutusV3CompiledContract = BettingContract.defaultCompiledContract
) {
    def script: Script.PlutusV3 = compiledContract.script
    val scriptAddress: Address = compiledContract.address(env.network)

    def deploy(
        utxos: Utxos,
        deploymentAddress: Address,
        changeAddress: Address
    ): Transaction = {
        TxBuilder
            .withCustomEvaluator(env, evaluator)
            .spend(utxos)
            .output(
              TransactionOutput(
                deploymentAddress,
                Value.zero,
                None,
                Some(ScriptRef(script))
              )
            )
            .changeTo(changeAddress)
            .build()
            .sign(signer)
            .transaction
    }

    def init(
        utxos: Utxos,
        scriptUtxo: Utxo,
        bet: Coin,
        player1: PubKeyHash,
        oracle: PubKeyHash,
        expiration: PosixTime,
        changeAddress: Address,
        beforeTime: Long,
        player2: PubKeyHash = PubKeyHash(hex""),
        token: AssetName = AssetName(utf8"lucky_number_slevin"),
        amount: Long = 1L
    ): Transaction = {
        val config = Config(player1, player2, oracle, expiration)

        TxBuilder
            .withCustomEvaluator(env, evaluator)
            .spend(utxos)
            .references(scriptUtxo)
            .mint(Data.unit, scala.collection.Map(token -> amount), script)
            .payTo(
              scriptAddress,
              Value.asset(script.scriptHash, token, amount, bet),
              config
            )
            .validTo(java.time.Instant.ofEpochMilli(beforeTime))
            .changeTo(changeAddress)
            .build()
            .sign(signer)
            .transaction
    }

    def join(
        utxos: Utxos,
        scriptUtxo: Utxo,
        betUtxo: Utxo,
        bet: Coin,
        player1: PubKeyHash,
        player2: PubKeyHash,
        player2Pkh: AddrKeyHash,
        oracle: PubKeyHash,
        expiration: PosixTime,
        changeAddress: Address,
        beforeTime: Long
    ): Transaction = {
        val lovelace = Value(bet)
        val config = Config(player1, player2, oracle, expiration)

        TxBuilder
            .withCustomEvaluator(env, evaluator)
            .spend(utxos)
            .references(scriptUtxo)
            .spend(betUtxo, Action.Join, script, Set(player2Pkh))
            .payTo(scriptAddress, betUtxo.output.value + lovelace, config)
            .validTo(java.time.Instant.ofEpochMilli(beforeTime))
            .changeTo(changeAddress)
            .build()
            .sign(signer)
            .transaction
    }

    def win(
        utxos: Utxos,
        scriptUtxo: Utxo,
        betUtxo: Utxo,
        isJoinWin: Boolean,
        player1: PubKeyHash,
        player2: PubKeyHash,
        oracle: PubKeyHash,
        oraclePkh: AddrKeyHash,
        changeAddress: Address,
        afterTime: Long
    ): Transaction = {
        val payout = if isJoinWin then player2 else player1
        val payoutAddress = ShelleyAddress(
          network = env.network,
          payment = ShelleyPaymentPart.Key(AddrKeyHash.fromByteString(payout.hash)),
          delegation = ShelleyDelegationPart.Null
        )

        TxBuilder
            .withCustomEvaluator(env, evaluator)
            .spend(utxos)
            .references(scriptUtxo)
            .spend(betUtxo, Action.AnnounceWinner(payout), script, Set(oraclePkh))
            .payTo(payoutAddress, betUtxo.output.value)
            .validFrom(java.time.Instant.ofEpochMilli(afterTime))
            .changeTo(changeAddress)
            .build()
            .sign(signer)
            .transaction
    }
}
