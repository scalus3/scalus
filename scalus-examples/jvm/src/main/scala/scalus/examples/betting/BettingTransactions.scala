package scalus.examples.betting

import scalus.uplc.builtin.ByteString.{hex, utf8}
import scalus.uplc.builtin.Data
import scalus.cardano.address.{Address, ShelleyAddress, ShelleyDelegationPart, ShelleyPaymentPart}
import scalus.cardano.ledger.*
import scalus.cardano.txbuilder.{TransactionSigner, TxBuilder}
import scalus.cardano.onchain.plutus.v1.{PosixTime, PubKeyHash}
import scalus.uplc.PlutusV3

case class BettingTransactions(
    env: CardanoInfo,
    evaluator: PlutusScriptEvaluator,
    signer: TransactionSigner,
    contract: PlutusV3[Data => Unit]
) {
    def script: Script.PlutusV3 = contract.script
    val scriptAddress: Address = contract.address(env.network)

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
              Data.unit
            )
            .requireSignature(player1Pkh)
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
            .spend(betUtxo, Action.Join)
            .requireSignature(player2Pkh)
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

        // Payout output will be at index 0 (first output added)
        val payoutOutputIdx = BigInt(0)
        TxBuilder(env, evaluator)
            .spend(utxos)
            .collaterals(collateralUtxo)
            .references(scriptUtxo)
            .spend(betUtxo, Action.AnnounceWinner(payout, payoutOutputIdx))
            // Burn the bet NFT so it can't be re-locked; the winner gets only the pot lovelace.
            .mint(script.scriptHash, scala.collection.Map(betToken(betUtxo) -> -1L), Data.unit)
            .requireSignature(oraclePkh)
            .payTo(payoutAddress, Value(betUtxo.output.value.coin))
            .validFrom(java.time.Instant.ofEpochMilli(afterTime))
            .build(changeTo = changeAddress)
            .sign(signer)
            .transaction
    }

    /** The bet NFT's asset name — the single token under this script's policy in the bet UTxO. */
    private def betToken(utxo: Utxo): AssetName =
        utxo.output.value.assets.assets
            .get(script.scriptHash)
            .flatMap(_.keys.headOption)
            .getOrElse(throw IllegalStateException("Bet UTxO must hold a bet token"))

    /** Reclaim the bet after expiration when the oracle never announced a winner.
      *
      * If `player2` has joined, the doubled pot is split back to both players (the beacon token
      * rides with player1's output); otherwise the whole pot is refunded to player1. A player must
      * sign (`signerPkh`).
      */
    def timeout(
        utxos: Utxos,
        collateralUtxo: Utxo,
        scriptUtxo: Utxo,
        betUtxo: Utxo,
        bet: Coin,
        player1: PubKeyHash,
        player2: PubKeyHash,
        signerPkh: AddrKeyHash,
        changeAddress: Address,
        afterTime: Long
    ): Transaction = {
        def enterprise(pkh: PubKeyHash): Address = ShelleyAddress(
          network = env.network,
          payment = ShelleyPaymentPart.Key(AddrKeyHash.fromByteString(pkh.hash)),
          delegation = ShelleyDelegationPart.Null
        )

        val potLovelace = Value(betUtxo.output.value.coin)
        val base = TxBuilder(env, evaluator)
            .spend(utxos)
            .collaterals(collateralUtxo)
            .references(scriptUtxo)
            .spend(betUtxo, Action.Timeout)
            // Burn the bet NFT so a reclaimed bet's token can't be re-locked; refund lovelace only.
            .mint(script.scriptHash, scala.collection.Map(betToken(betUtxo) -> -1L), Data.unit)
            .requireSignature(signerPkh)
            .validFrom(java.time.Instant.ofEpochMilli(afterTime))

        val withPayouts =
            if player2.hash == hex"" then base.payTo(enterprise(player1), potLovelace)
            else
                base
                    // Each player gets their stake back; the NFT is burned, not handed out.
                    .payTo(enterprise(player1), Value(bet))
                    .payTo(enterprise(player2), Value(bet))

        withPayouts
            .build(changeTo = changeAddress)
            .sign(signer)
            .transaction
    }
}
