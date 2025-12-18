package scalus.examples.betting

import scalus.builtin.ByteString.{hex, utf8}
import scalus.builtin.Data
import scalus.cardano.address.{Address, ShelleyAddress, ShelleyDelegationPart, ShelleyPaymentPart}
import scalus.cardano.blueprint.PlutusV3CompiledContract
import scalus.cardano.ledger.*
import scalus.cardano.txbuilder.{TransactionSigner, TxBuilder}
import scalus.ledger.api.v1.{PosixTime, PubKeyHash}

case class BettingTransactionCreator(
    env: CardanoInfo,
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
        TxBuilder(env, evaluator)
            .spend(utxos)
            .output(
              TransactionOutput(
                deploymentAddress,
                Value.zero,
                None,
                Some(ScriptRef(script))
              )
            )
            .build(changeTo = changeAddress)
            .sign(signer)
            .transaction
    }

    def init(
        utxos: Utxos,
        collateralUtxo: Utxo,
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
        val player1Pkh = AddrKeyHash.fromByteString(player1.hash)

        TxBuilder(env, evaluator)
            .spend(utxos)
            .collaterals(collateralUtxo)
            .references(scriptUtxo)
            .mint(
              script.scriptHash,
              scala.collection.Map(token -> amount),
              Data.unit,
              Set(player1Pkh)
            )
            .payTo(
              scriptAddress,
              Value.asset(script.scriptHash, token, amount, bet),
              config
            )
            .validTo(java.time.Instant.ofEpochMilli(beforeTime))
            .build(changeTo = changeAddress)
            .sign(signer)
            .transaction
    }

    def join(
        utxos: Utxos,
        collateralUtxo: Utxo,
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

        TxBuilder(env, evaluator)
            .spend(utxos)
            .collaterals(collateralUtxo)
            .references(scriptUtxo)
            .spend(betUtxo, Action.Join, Set(player2Pkh))
            .payTo(scriptAddress, betUtxo.output.value + lovelace, config)
            .validTo(java.time.Instant.ofEpochMilli(beforeTime))
            .build(changeTo = changeAddress)
            .sign(signer)
            .transaction
    }

    def win(
        utxos: Utxos,
        collateralUtxo: Utxo,
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

        TxBuilder(env, evaluator)
            .spend(utxos)
            .collaterals(collateralUtxo)
            .references(scriptUtxo)
            .spend(betUtxo, Action.AnnounceWinner(payout), Set(oraclePkh))
            .payTo(payoutAddress, betUtxo.output.value)
            .validFrom(java.time.Instant.ofEpochMilli(afterTime))
            .build(changeTo = changeAddress)
            .sign(signer)
            .transaction
    }
}
