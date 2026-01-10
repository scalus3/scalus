package scalus.examples.lottery

import scalus.builtin.Data
import scalus.cardano.address.Address
import scalus.cardano.ledger.*
import scalus.cardano.txbuilder.*
import scalus.ledger.api.v1.PosixTime
import scalus.uplc.PlutusV3

import java.time.Instant

// TODO:
//   - enforce secret lengths to protect from brute force against the secrets
//   - add multisig for the initiation
//   - look for optimisations and refactor opportunities in the validator code
case class LotteryTransactionCreator(
    env: CardanoInfo,
    evaluator: PlutusScriptEvaluator,
    contract: PlutusV3[Data => Unit]
) {
    def script: Script.PlutusV3 = contract.script
    val scriptAddress: Address = contract.address(env.network)

    /** Create initial lottery UTXO. Both players contribute equal bet amounts and both must sign.
      */
    def initiateLottery(
        playerOneUtxos: Utxos,
        playerTwoUtxos: Utxos,
        betAmount: Coin,
        playerOnePkh: AddrKeyHash,
        playerTwoPkh: AddrKeyHash,
        secret1: Secret,
        secret2: Secret,
        revealDeadline: PosixTime,
        changeAddress: Address,
        playerOneSigner: TransactionSigner,
        playerTwoSigner: TransactionSigner
    ): Transaction = {
        val datum = State(
          playerOneSecret = secret1,
          playerTwoSecret = secret2,
          revealDeadline = revealDeadline,
          lotteryState = LotteryState.Empty
        )

        val playerOneUtxo = Utxo(playerOneUtxos.head)
        val playerTwoUtxo = Utxo(playerTwoUtxos.head)

        val allUtxos = playerOneUtxos ++ playerTwoUtxos

        TxBuilder(env)
            .spend(playerOneUtxo)
            .spend(playerTwoUtxo)
            .payTo(scriptAddress, Value.lovelace(betAmount.value * 2), datum)
            .complete(availableUtxos = allUtxos, sponsor = changeAddress)
            .sign(playerOneSigner)
            .sign(playerTwoSigner)
            .transaction
    }

    def revealPlayerOne(
        utxos: Utxos,
        lotteryUtxo: Utxo,
        preimage: Preimage,
        playerOnePkh: AddrKeyHash,
        playerOneSecret: Secret,
        playerTwoSecret: Secret,
        revealDeadline: PosixTime,
        sponsor: Address,
        validTo: Instant,
        signer: TransactionSigner
    ): Transaction = {
        val redeemer = Action.RevealPlayerOne(preimage)

        // Construct new state with PlayerOneRevealed
        val newLotteryState = LotteryState.PlayerOneRevealed(
          BigInt(preimage.bytes.length),
          scalus.ledger.api.v1.PubKeyHash(playerOnePkh)
        )
        val newState = State(
          playerOneSecret = playerOneSecret,
          playerTwoSecret = playerTwoSecret,
          revealDeadline = revealDeadline,
          lotteryState = newLotteryState
        )

        TxBuilder(env, evaluator)
            .spend(lotteryUtxo, redeemer, script, Set(playerOnePkh))
            .payTo(scriptAddress, lotteryUtxo.output.value, newState)
            .validTo(validTo)
            .complete(availableUtxos = utxos, sponsor)
            .sign(signer)
            .transaction
    }

    def revealPlayerTwo(
        utxos: Utxos,
        lotteryUtxo: Utxo,
        preimage: Preimage,
        playerTwoPkh: AddrKeyHash,
        playerOneSecret: Secret,
        playerTwoSecret: Secret,
        revealDeadline: PosixTime,
        sponsor: Address,
        validTo: Instant,
        signer: TransactionSigner
    ): Transaction = {
        val redeemer = Action.RevealPlayerTwo(preimage)

        val newLotteryState = LotteryState.PlayerTwoRevealed(
          BigInt(preimage.bytes.length),
          scalus.ledger.api.v1.PubKeyHash(playerTwoPkh)
        )
        val newState = State(
          playerOneSecret = playerOneSecret,
          playerTwoSecret = playerTwoSecret,
          revealDeadline = revealDeadline,
          lotteryState = newLotteryState
        )

        TxBuilder(env, evaluator)
            .spend(lotteryUtxo, redeemer, script, Set(playerTwoPkh))
            .payTo(scriptAddress, lotteryUtxo.output.value, newState)
            .validTo(validTo)
            .complete(availableUtxos = utxos, sponsor)
            .sign(signer)
            .transaction
    }

    /** Non-revealing player claims pot after deadline when opponent failed to reveal in time. */
    def timeout(
        utxos: Utxos,
        lotteryUtxo: Utxo,
        preimage: Preimage,
        claimantPkh: AddrKeyHash,
        payeeAddress: Address,
        sponsor: Address,
        validFrom: Instant,
        signer: TransactionSigner
    ): Transaction = {
        val redeemer = Action.Timeout(preimage)

        TxBuilder(env, evaluator)
            .spend(lotteryUtxo, redeemer, script, Set(claimantPkh))
            .payTo(payeeAddress, lotteryUtxo.output.value)
            .validFrom(validFrom)
            .complete(availableUtxos = utxos, sponsor)
            .sign(signer)
            .transaction
    }

    /** Losing player concedes by revealing their preimage and directing pot to winner. */
    def lose(
        utxos: Utxos,
        lotteryUtxo: Utxo,
        preimage: Preimage,
        loserPkh: AddrKeyHash,
        winnerAddress: Address,
        winnerOutputIdx: BigInt,
        sponsor: Address,
        validTo: Instant,
        signer: TransactionSigner
    ): Transaction = {
        val redeemer = Action.Lose(preimage, winnerOutputIdx)

        TxBuilder(env, evaluator)
            .spend(lotteryUtxo, redeemer, script, Set(loserPkh))
            .payTo(winnerAddress, lotteryUtxo.output.value)
            .validTo(validTo)
            .complete(availableUtxos = utxos, sponsor)
            .sign(signer)
            .transaction
    }
}
