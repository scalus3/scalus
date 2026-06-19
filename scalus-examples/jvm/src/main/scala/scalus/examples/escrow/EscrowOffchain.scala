package scalus.examples.escrow

import scalus.cardano.address.{Address, ShelleyAddress}
import scalus.cardano.ledger.*
import scalus.cardano.node.BlockfrostProvider
import scalus.cardano.txbuilder.TransactionSigner
import scalus.cardano.wallet.hd.HdAccount
import scalus.crypto.ed25519.given
import scalus.uplc.builtin.Data.toData
import scalus.utils.await

import scala.concurrent.ExecutionContext.Implicits.global

/** Off-chain runner for the Escrow contract on the Cardano preview testnet.
  *
  * This runner uses the native Scalus TxBuilder stack (via [[EscrowTransactions]]) together with
  * the [[BlockfrostProvider]] for UTxO querying and transaction submission. It replaces the
  * previous bloxbean-based implementation.
  *
  * Flow:
  *   1. Seller initializes the escrow with the initialization amount.
  *   2. Buyer deposits the escrow amount (contract now holds escrow + initialization).
  *   3. Buyer releases the payment to the seller.
  *
  * Refund (seller returns funds to buyer) is available as [[refund]] but left out of the main flow.
  *
  * Required environment variables:
  *   - `BLOCKFROST_API_KEY` — Blockfrost project id for the preview network
  *   - `SELLER_MNEMONIC` — BIP-39 mnemonic for the seller wallet
  *   - `BUYER_MNEMONIC` — BIP-39 mnemonic for the buyer wallet
  */
object EscrowOffChain {

    private val blockfrostApiKey = sys.env("BLOCKFROST_API_KEY")
    private val sellerMnemonic = sys.env("SELLER_MNEMONIC")
    private val buyerMnemonic = sys.env("BUYER_MNEMONIC")

    // HD accounts derived from mnemonics (payment key, index 0).
    private val seller: HdAccount = HdAccount.fromMnemonic(sellerMnemonic, "", 0)
    private val buyer: HdAccount = HdAccount.fromMnemonic(buyerMnemonic, "", 0)

    private val sellerSigner = new TransactionSigner(Set(seller.paymentKeyPair))
    private val buyerSigner = new TransactionSigner(Set(buyer.paymentKeyPair))

    // Blockfrost provider for the preview testnet. Its `cardanoInfo` carries the preview
    // protocol params and SlotConfig, so we reuse it as the build environment.
    private val provider: BlockfrostProvider = BlockfrostProvider.preview(blockfrostApiKey).await()
    private given env: CardanoInfo = provider.cardanoInfo

    private val contract = EscrowContract.compiled
    private val scriptAddress: Address = contract.address(env.network)

    private val txCreator = EscrowTransactions(env = env, contract = contract)

    private val sellerAddress: ShelleyAddress = seller.baseAddress(env.network)
    private val buyerAddress: ShelleyAddress = buyer.baseAddress(env.network)

    /** Poll the script address for the escrow UTxO matching the given datum and lovelace amount.
      *
      * Replaces the bloxbean `waitForUtxoWithAmount` polling helper. The escrow flow keeps a single
      * UTxO at the script address whose lovelace amount changes between phases (initialization-only
      * after `initialize`, full amount after `deposit`), so matching on both datum and amount is
      * enough to locate the correct UTxO.
      */
    private def waitForEscrowUtxo(
        datum: Config,
        expectedLovelace: Long,
        maxAttempts: Int = 20,
        delayMs: Long = 15000
    ): Utxo = {
        val expectedDatumData = datum.toData

        def attempt(n: Int): Utxo = {
            println(
              s"Searching for escrow UTxO with $expectedLovelace lovelace... (Attempt $n/$maxAttempts)"
            )
            val utxos = provider.findUtxos(scriptAddress).await() match
                case Right(found) => found
                case Left(error)  => sys.error(s"Failed to query script UTxOs: $error")

            val matching = utxos.find { case (_, output) =>
                val datumMatches = output.inlineDatum.contains(expectedDatumData)
                val amountMatches = output.value.coin.value == expectedLovelace
                datumMatches && amountMatches
            }

            matching match
                case Some(pair) =>
                    println("Found escrow UTxO!")
                    Utxo(pair)
                case None if n < maxAttempts =>
                    Thread.sleep(delayMs)
                    attempt(n + 1)
                case None =>
                    sys.error(
                      s"Escrow UTxO with datum and $expectedLovelace lovelace not found at " +
                          s"$scriptAddress after $maxAttempts attempts."
                    )
        }

        attempt(1)
    }

    /** Submit a transaction and wait for it to be confirmed. */
    private def submitAndConfirm(label: String, tx: Transaction): Unit = {
        println(s"Submitting $label TX (${tx.id.toHex})...")
        provider.submit(tx).await() match
            case Right(txHash) =>
                println(s"$label tx submitted successfully: ${txHash.toHex}")
                println(s"Waiting for $label tx confirmation...")
                val status = provider.pollForConfirmation(txHash).await()
                println(s"$label tx status: $status")
            case Left(error) =>
                sys.error(s"$label tx submission failed: $error")
    }

    /** Seller initializes the escrow contract with the initialization amount. */
    def initialize(escrowAmount: Long, initializationAmount: Long): Unit = {
        val utxos = provider.findUtxos(sellerAddress).await() match
            case Right(found) => found
            case Left(error)  => sys.error(s"Failed to query seller UTxOs: $error")

        val tx = txCreator.initialize(
          utxos = utxos,
          sponsor = sellerAddress,
          seller = seller.paymentKeyHash,
          buyer = buyer.paymentKeyHash,
          escrowAmount = escrowAmount,
          initializationAmount = initializationAmount,
          signer = sellerSigner
        )
        submitAndConfirm("Initialize", tx)
    }

    /** Buyer deposits the escrow amount onto the initialized contract.
      *
      * The buyer is the fee sponsor (matching the test), so the buyer change output satisfies the
      * validator's required buyer-output constraint.
      */
    def deposit(datum: Config): Unit = {
        // Before deposit the contract holds only the initialization amount.
        val escrowUtxo = waitForEscrowUtxo(datum, datum.initializationAmount.toLong)

        val utxos = provider.findUtxos(buyerAddress).await() match
            case Right(found) => found
            case Left(error)  => sys.error(s"Failed to query buyer UTxOs: $error")

        val tx = txCreator.deposit(
          utxos = utxos,
          escrowUtxo = escrowUtxo,
          buyerAddress = buyerAddress,
          sponsor = buyerAddress,
          buyer = buyer.paymentKeyHash,
          signer = buyerSigner
        )
        submitAndConfirm("Deposit", tx)
    }

    /** Buyer releases the payment to the seller.
      *
      * The buyer is the fee sponsor so the buyer change output is present, and the seller receives
      * exactly escrowAmount + initializationAmount as the validator requires.
      */
    def pay(datum: Config): Unit = {
        // After deposit the contract holds the full amount.
        val fullAmount = (datum.escrowAmount + datum.initializationAmount).toLong
        val escrowUtxo = waitForEscrowUtxo(datum, fullAmount)

        val utxos = provider.findUtxos(buyerAddress).await() match
            case Right(found) => found
            case Left(error)  => sys.error(s"Failed to query buyer UTxOs: $error")

        val tx = txCreator.pay(
          utxos = utxos,
          escrowUtxo = escrowUtxo,
          sellerAddress = sellerAddress,
          buyerAddress = buyerAddress,
          sponsor = buyerAddress,
          buyer = buyer.paymentKeyHash,
          signer = buyerSigner
        )
        submitAndConfirm("Pay", tx)
    }

    /** Seller refunds the buyer.
      *
      * The seller is the fee sponsor here (mirroring the test): the validator enforces that the
      * buyer receives exactly the escrow amount and the seller receives exactly the initialization
      * amount, so the seller — not the buyer — must pay the fee out of its own change.
      */
    def refund(datum: Config): Unit = {
        val fullAmount = (datum.escrowAmount + datum.initializationAmount).toLong
        val escrowUtxo = waitForEscrowUtxo(datum, fullAmount)

        val utxos = provider.findUtxos(sellerAddress).await() match
            case Right(found) => found
            case Left(error)  => sys.error(s"Failed to query seller UTxOs: $error")

        val tx = txCreator.refund(
          utxos = utxos,
          escrowUtxo = escrowUtxo,
          sellerAddress = sellerAddress,
          buyerAddress = buyerAddress,
          sponsor = sellerAddress,
          seller = seller.paymentKeyHash,
          signer = sellerSigner
        )
        submitAndConfirm("Refund", tx)
    }

    def main(args: Array[String]): Unit = {
        val escrowAmount = 10_000_000L // 10 ADA
        val initializationAmount = 2_000_000L // 2 ADA (min UTxO)

        val datum = Config(
          scalus.cardano.onchain.plutus.v1.PubKeyHash(seller.paymentKeyHash),
          scalus.cardano.onchain.plutus.v1.PubKeyHash(buyer.paymentKeyHash),
          BigInt(escrowAmount),
          BigInt(initializationAmount)
        )

        println("Escrow Datum:")
        println(s"  Seller: ${datum.seller}")
        println(s"  Buyer: ${datum.buyer}")
        println(s"  Escrow Amount: $escrowAmount lovelace")
        println(s"  Initialization Amount: $initializationAmount lovelace")
        println(s"  Script address: $scriptAddress")

        println("Step 1: Seller initializes escrow contract")
        initialize(escrowAmount, initializationAmount)

        println("Step 2: Buyer deposits payment")
        deposit(datum)

        println("Step 3: Buyer releases payment to seller")
        pay(datum)

        // println("Step 3 (alt): Seller refunds to buyer")
        // refund(datum)
    }
}
