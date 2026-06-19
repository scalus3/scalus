package scalus.examples

import scalus.cardano.address.{Address, ShelleyAddress}
import scalus.cardano.ledger.*
import scalus.cardano.node.BlockfrostProvider
import scalus.cardano.txbuilder.{TransactionSigner, TxBuilder}
import scalus.cardano.wallet.hd.HdAccount
import scalus.crypto.ed25519.given
import scalus.uplc.builtin.Data.toData
import scalus.uplc.builtin.{platform, ByteString, given}
import scalus.utils.await

import scala.concurrent.ExecutionContext.Implicits.global

/** Off-chain runner for the hash-preimage lock contract on the Cardano preview testnet.
  *
  * Uses the native Scalus TxBuilder stack together with [[BlockfrostProvider]] for UTxO querying and
  * transaction submission. Replaces the previous bloxbean-based implementation.
  *
  * The contract ([[PreimageValidatorContract]], PlutusV3) locks funds with an inline datum
  * `(sha2_256(preimage), pubKeyHash)`. The funds can be claimed by a transaction that reveals the
  * preimage as the redeemer and is signed by the matching public key hash.
  *
  * Flow:
  *   1. Lock: pay 10 ADA to the script with the preimage-hash datum.
  *   2. Spend: reveal the preimage and claim the funds back.
  *
  * Required environment variables:
  *   - `BLOCKFROST_API_KEY` — Blockfrost project id for the preview network
  *   - `MNEMONIC` — BIP-39 mnemonic for the wallet that locks and claims the funds
  */
object SendTx {

    private val mnemonic = sys.env("MNEMONIC")
    private val blockfrostApiKey = sys.env("BLOCKFROST_API_KEY")

    // HD wallet derived from the mnemonic (payment key, index 0).
    private val sender: HdAccount = HdAccount.fromMnemonic(mnemonic, "", 0)
    private val senderSigner = new TransactionSigner(Set(sender.paymentKeyPair))

    // Blockfrost provider for the preview testnet. Its `cardanoInfo` carries the preview
    // protocol params and SlotConfig, so we reuse it as the build environment.
    private val provider: BlockfrostProvider = BlockfrostProvider.preview(blockfrostApiKey).await()
    private given env: CardanoInfo = provider.cardanoInfo

    private val contract = PreimageValidatorContract
    private val scriptAddress: Address = contract.address(env.network)
    private val senderAddress: ShelleyAddress = sender.baseAddress(env.network)

    private def senderUtxos: Utxos =
        provider.findUtxos(senderAddress).await() match
            case Right(found) => found
            case Left(error)  => sys.error(s"Failed to query sender UTxOs: $error")

    /** Submit a transaction and wait for it to be confirmed. */
    private def submitAndConfirm(label: String, tx: Transaction): Unit = {
        println(s"Submitting $label TX (${tx.id.toHex})...")
        provider.submit(tx).await() match
            case Right(txHash) =>
                println(s"$label tx submitted successfully: ${txHash.toHex}")
                val status = provider.pollForConfirmation(txHash).await()
                println(s"$label tx status: $status")
            case Left(error) =>
                sys.error(s"$label tx submission failed: $error")
    }

    /** Lock 10 ADA at the script address with the inline preimage-hash datum. */
    private def publishLockingTx(datum: (ByteString, ByteString)): Unit = {
        val tx = TxBuilder(env)
            .payTo(contract, Value.ada(10), datum)
            .complete(availableUtxos = senderUtxos, sponsor = senderAddress)
            .sign(senderSigner)
            .transaction
        submitAndConfirm("Lock", tx)
    }

    /** Spend the locked UTxO by revealing the preimage and claiming the funds back. */
    private def spendLockedTx(datum: (ByteString, ByteString), redeemer: ByteString): Unit = {
        val datumData = datum.toData
        val scriptUtxo = provider.findUtxos(scriptAddress).await() match
            case Right(found) =>
                found
                    .find { case (_, output) => output.inlineDatum.contains(datumData) }
                    .map(Utxo(_))
                    .getOrElse(
                      sys.error(s"No locked UTxO with matching datum found at $scriptAddress")
                    )
            case Left(error) => sys.error(s"Failed to query script UTxOs: $error")

        // The validator imposes no output constraints, so the locked value flows back to the
        // sender as change (sponsor == senderAddress).
        val tx = TxBuilder(env)
            .spend(scriptUtxo, redeemer, contract)
            .requireSignature(sender.paymentKeyHash)
            .complete(availableUtxos = senderUtxos, sponsor = senderAddress)
            .sign(senderSigner)
            .transaction
        submitAndConfirm("Spend", tx)
    }

    def main(args: Array[String]): Unit = {
        val preimage = "Scalus rocks!"
        val preimageBytes = ByteString.fromString(preimage)
        val preimageHash = platform.sha2_256(preimageBytes)
        val pkh: ByteString = sender.paymentKeyHash
        val datum = (preimageHash, pkh)
        val redeemer = preimageBytes

        println(s"Script $env Address: $scriptAddress")
        println(s"Sender Address: $senderAddress")
        println(s"Preimage: $preimage, Hex: ${preimageBytes.toHex}, Hash: ${preimageHash.toHex}")
        println(s"PubKeyHash: ${pkh.toHex}")

        def lock(): Unit = publishLockingTx(datum)
        def spend(): Unit = spendLockedTx(datum, redeemer)

//        lock()
//        spend()
    }
}
