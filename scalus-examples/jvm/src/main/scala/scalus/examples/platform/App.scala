package scalus.examples.platform

import scalus.cardano.ledger.{AssetName, ScriptHash, Transaction, TransactionOutput}
import scalus.cardano.onchain.plutus.v1.PubKeyHash
import scalus.uplc.builtin.ByteString

import scala.concurrent.duration.*

/** A prototype **Scalus Platform Application** built on the Application Runtime (ARS).
  *
  * `CollateralGuardAgent` is a personal bot that watches *your own* lending position and tops it up
  * before it can be liquidated. It shows how little application code the runtime requires:
  *
  *   1. A rich on-chain subscription — pay-to-script + carries an NFT + datum names my pubkey — in a
  *      single fluent expression.
  *   2. A reaction transaction submitted under a durable resubmission policy, reacting to its
  *      terminal outcome.
  *
  * The runtime API it builds on lives in `ScalusPlatform.scala` (stubbed). This file is the user code.
  */

/** Inline datum carried by a lending-vault UTxO. */
final case class VaultDatum(owner: PubKeyHash, collateral: BigInt, debt: BigInt) {

    /** Collateral-to-debt ratio; below ~1.5 the position is at risk of liquidation. */
    def healthFactor: Double =
        if debt == 0 then Double.PositiveInfinity
        else collateral.toDouble / debt.toDouble
}

/** Watches the caller's own lending position and tops it up before liquidation. */
object CollateralGuardAgent extends ScalusApp {

    // --- configuration (provided at startup) ---
    private val myPubKey: PubKeyHash =
        PubKeyHash(ByteString.fromHex("22c06a8c86a716745a6861913a2ba92932d6e94cff0f47cd7b27c1fd"))
    private val vaultScript: ScriptHash =
        ScriptHash.fromHex("3cd1b62822d323989a0548af6e410af29db2286be935d82572c5e042")
    private val positionNft: Asset = Asset(
      policy = ScriptHash.fromHex("62d5d51e72dfe4ec7f81e0e7916b1201267bf414bb62df229985c466"),
      name = AssetName.fromString("VaultPosition")
    )
    private val minHealthFactor = 1.5

    def run(): Unit = {
        // One fluent expression encodes the whole filter:
        //   pay-to script-hash  &&  carries the position NFT  &&  datum.owner == my pubkey
        runtime.watchUtxos
            .atScript(vaultScript)
            .containingAsset(positionNft)
            .withInlineDatum[VaultDatum]
            .where(_.owner == myPubKey)
            .onEvent {
                case UtxoEvent.Produced(output, datum, _) =>
                    if datum.healthFactor < minHealthFactor then topUp(output, datum)
                case UtxoEvent.Spent(ref) =>
                    println(s"position $ref closed — nothing to guard")
            }
    }

    /** Build and submit a top-up transaction, reacting to its terminal outcome. */
    private def topUp(position: TransactionOutput, datum: VaultDatum): Unit = {
        val tx = buildTopUpTx(position, datum)

        // Resubmission policy: back off exponentially up to an hour, and treat the tx as done only
        // after 10 confirmations. The runtime owns the retry loop, mempool tracking, and rollback
        // detection — the application just handles the three terminal outcomes.
        val outcome = runtime.submit(
          tx,
          retry = Backoff.exponential(start = 2.seconds, cap = 1.hour),
          confirmations = 10
        )

        outcome match
            case SubmitOutcome.Confirmed(h) =>
                println(s"position topped up — confirmed in $h")
            case SubmitOutcome.RolledBack(h) =>
                // The position UTxO reappears, so the next Produced event re-triggers the guard.
                println(s"top-up $h rolled back — will be retried on the next event")
            case SubmitOutcome.GaveUp(reason) =>
                alertOperator(reason)
    }

    /** Construct the top-up transaction. Stubbed — building it with the Scalus `TxBuilder` (spend the
      * vault UTxO, add collateral, return it with an updated datum) is orthogonal to this demo.
      */
    private def buildTopUpTx(position: TransactionOutput, datum: VaultDatum): Transaction = ???

    private def alertOperator(reason: String): Unit =
        println(s"ALERT: collateral top-up gave up after retrying for 1h: $reason")
}

@main def runCollateralGuard(): Unit = CollateralGuardAgent.run()
