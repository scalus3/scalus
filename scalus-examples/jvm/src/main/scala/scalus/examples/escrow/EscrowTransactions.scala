package scalus.examples.escrow

import scalus.uplc.builtin.Data
import scalus.cardano.address.Address
import scalus.cardano.ledger.*
import scalus.cardano.txbuilder.*
import scalus.cardano.onchain.plutus.v1.PubKeyHash
import scalus.uplc.PlutusV3

/** Transaction builder for Escrow contract operations.
  *
  * The escrow flow is:
  *   1. Initialize: Seller creates UTxO with initializationAmount
  *   2. Deposit: Buyer adds escrowAmount
  *   3. Pay: Buyer releases payment to seller OR
  *   4. Refund: Seller returns funds to buyer
  */
case class EscrowTransactions(
    env: CardanoInfo,
    contract: PlutusV3[Data => Unit]
) {
    private val script: Script.PlutusV3 = contract.script
    private val scriptAddress: Address = contract.address(env.network)

    private val builder = TxBuilder(env)

    /** Seller initializes the escrow contract with the initialization amount.
      *
      * @param utxos
      *   Available UTxOs for the transaction
      * @param sponsor
      *   Address to pay fees from
      * @param seller
      *   Seller's public key hash
      * @param buyer
      *   Buyer's public key hash
      * @param escrowAmount
      *   The amount buyer will deposit
      * @param initializationAmount
      *   The initial amount locked by seller (typically min UTxO)
      * @param signer
      *   Transaction signer
      * @return
      *   Signed transaction
      */
    def initialize(
        utxos: Utxos,
        sponsor: Address,
        seller: AddrKeyHash,
        buyer: AddrKeyHash,
        escrowAmount: Long,
        initializationAmount: Long,
        signer: TransactionSigner
    ): Transaction = {
        val datum = Config(
          PubKeyHash(seller),
          PubKeyHash(buyer),
          BigInt(escrowAmount),
          BigInt(initializationAmount)
        )

        builder
            .payTo(scriptAddress, Value.lovelace(initializationAmount), datum)
            .complete(availableUtxos = utxos, sponsor = sponsor)
            .sign(signer)
            .transaction
    }

    /** Buyer deposits the escrow amount.
      *
      * The validator requires:
      *   - Buyer signature
      *   - Exactly one buyer output
      *   - Contract output with escrowAmount + initializationAmount
      *   - Preserved datum
      *
      * @param utxos
      *   Available UTxOs for the transaction
      * @param escrowUtxo
      *   The existing escrow UTxO with initialization amount
      * @param buyerAddress
      *   Buyer's address for the required output
      * @param sponsor
      *   Address to pay fees from
      * @param buyer
      *   Buyer's public key hash (must sign)
      * @param signer
      *   Transaction signer
      * @return
      *   Signed transaction
      */
    def deposit(
        utxos: Utxos,
        escrowUtxo: Utxo,
        buyerAddress: Address,
        sponsor: Address,
        buyer: AddrKeyHash,
        signer: TransactionSigner
    ): Transaction = {
        val datum = escrowUtxo.output.requireInlineDatum
        val escrowDatum = datum.to[Config]
        val totalAmount = (escrowDatum.escrowAmount + escrowDatum.initializationAmount).toLong

        // Note: The buyer output is created as change by the TxBuilder when sponsor == buyerAddress.
        // If sponsor != buyerAddress, an explicit buyer output would need to be added.
        builder
            .spend(escrowUtxo, Action.Deposit, script, Set(buyer))
            .payTo(
              scriptAddress,
              Value.lovelace(totalAmount),
              datum
            ) // Continue contract with preserved datum
            .complete(availableUtxos = utxos, sponsor)
            .sign(signer)
            .transaction
    }

    /** Buyer releases payment to seller.
      *
      * The validator requires:
      *   - Buyer signature
      *   - Contract fully funded
      *   - Seller receives escrowAmount + initializationAmount
      *   - Both buyer and seller outputs present
      *
      * @param utxos
      *   Available UTxOs for the transaction
      * @param escrowUtxo
      *   The funded escrow UTxO
      * @param sellerAddress
      *   Seller's address to receive payment
      * @param buyerAddress
      *   Buyer's address for the required output
      * @param sponsor
      *   Address to pay fees from
      * @param buyer
      *   Buyer's public key hash (must sign)
      * @param signer
      *   Transaction signer
      * @return
      *   Signed transaction
      */
    def pay(
        utxos: Utxos,
        escrowUtxo: Utxo,
        sellerAddress: Address,
        buyerAddress: Address,
        sponsor: Address,
        buyer: AddrKeyHash,
        signer: TransactionSigner
    ): Transaction = {
        val datum = escrowUtxo.output.requireInlineDatum
        val escrowDatum = datum.to[Config]
        val paymentAmount = (escrowDatum.escrowAmount + escrowDatum.initializationAmount).toLong

        // Note: The buyer output is created as change by the TxBuilder when sponsor == buyerAddress.
        builder
            .spend(escrowUtxo, Action.Pay, script, Set(buyer))
            .payTo(sellerAddress, Value.lovelace(paymentAmount))
            .complete(availableUtxos = utxos, sponsor)
            .sign(signer)
            .transaction
    }

    /** Seller refunds to buyer.
      *
      * The validator requires:
      *   - Seller signature
      *   - Contract fully funded
      *   - Buyer receives escrowAmount
      *   - Both buyer and seller outputs present
      *
      * @param utxos
      *   Available UTxOs for the transaction
      * @param escrowUtxo
      *   The funded escrow UTxO
      * @param sellerAddress
      *   Seller's address for the required output
      * @param buyerAddress
      *   Buyer's address to receive refund
      * @param sponsor
      *   Address to pay fees from
      * @param seller
      *   Seller's public key hash (must sign)
      * @param signer
      *   Transaction signer
      * @return
      *   Signed transaction
      */
    def refund(
        utxos: Utxos,
        escrowUtxo: Utxo,
        sellerAddress: Address,
        buyerAddress: Address,
        sponsor: Address,
        seller: AddrKeyHash,
        signer: TransactionSigner
    ): Transaction = {
        val datum = escrowUtxo.output.requireInlineDatum
        val escrowDatum = datum.to[Config]

        builder
            .spend(escrowUtxo, Action.Refund, script, Set(seller))
            .payTo(buyerAddress, Value.lovelace(escrowDatum.escrowAmount.toLong))
            .payTo(sellerAddress, Value.lovelace(escrowDatum.initializationAmount.toLong))
            .complete(availableUtxos = utxos, sponsor)
            .sign(signer)
            .transaction
    }
}
