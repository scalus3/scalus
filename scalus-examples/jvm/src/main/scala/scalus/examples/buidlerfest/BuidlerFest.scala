package scalus.examples.buidlerfest

import scalus.bloxbean.Interop.??
import scalus.uplc.builtin.Data.{toData, FromData, ToData}
import scalus.cardano.address.Address
import scalus.cardano.address.Address.addr
import scalus.cardano.ledger.*
import scalus.cardano.node.BlockfrostProvider
import scalus.cardano.txbuilder.TxBuilder
import scalus.utils.Hex.toHex
import scalus.utils.await
import sttp.client4.DefaultFutureBackend

import java.time.Instant
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Future

/** Off-chain transaction builder for BuidlerFest 2026 ticket registration.
  *
  * This module builds transactions for purchasing tickets to BuidlerFest 2026, integrating with an
  * Aiken smart contract deployed on Cardano mainnet.
  *
  * ==How it works==
  *
  * The ticket system uses a beacon token pattern:
  *   - An issuer UTxO holds a beacon token (BUIDLERFEST2026) and a counter datum
  *   - Each ticket purchase spends this UTxO and creates a new one with incremented counter
  *   - A unique ticket NFT (TICKET0, TICKET1, ...) is minted for each purchase
  *   - Payment goes to the treasury address
  *
  * ==Usage==
  *
  * {{{
  * // Set BLOCKFROST_API_KEY environment variable, then:
  * sbt "scalusExamplesJVM/runMain scalus.examples.buidlerfest.main addr1..."
  * }}}
  *
  * The returned transaction is unsigned and needs to be signed by the buyer's wallet.
  */
object BuidlerFest {
    given sttp.client4.Backend[Future] = DefaultFutureBackend()

    // ============================================================================
    // Contract addresses and identifiers (mainnet deployment)
    // ============================================================================

    /** Policy ID for the issuer beacon token that identifies the valid issuer UTxO */
    val issuerBeaconPolicy: PolicyId =
        ScriptHash.fromHex("e1ddde8138579e255482791d9fba0778cb1f5c7b435be7b3e42069de")

    /** Asset name of the beacon token */
    val issuerBeaconName: AssetName = AssetName.fromString("BUIDLERFEST2026")

    /** Treasury address where ticket payments are sent */
    val treasury: Address =
        addr"addr1qx0decp93g2kwym5cz0p68thamd2t9pehlxqe02qae5r6nycv42qmjppm2rr8fj6qlzfhm6ljkd5f0tjlgudtmt5kzyqmy8x82"

    /** UTxO containing the reference script (validator + minting policy) */
    val issuerScriptRef: TransactionInput = Input(
      TransactionHash.fromHex("31596ecbdcf102c8e5c17e75c65cf9780996285879d18903f035964f3a7499a8"),
      0
    )

    /** Policy ID for minting ticket NFTs */
    val ticketPolicy: PolicyId =
        ScriptHash.fromHex("1d9c0b541adc300c19ddc6b9fb63c0bfe32b1508305ba65b8762dc7b")

    /** Address holding the issuer UTxO (beacon token + counter datum) */
    val issuer: Address = addr"addr1wywecz65rtwrqrqemhrtn7mrczl7x2c4pqc9hfjmsa3dc7cr5pvqw"

    // ============================================================================
    // Pricing configuration
    // ============================================================================

    /** Early bird price (before switch slot) */
    val blindPrice: Value = Value.ada(400)

    /** Regular price (after switch slot) */
    val normalPrice: Value = Value.ada(500)

    // ============================================================================
    // Data types matching the Aiken contract
    // These types are serialized to Plutus Data for on-chain validation
    // ============================================================================

    /** Datum stored in the issuer UTxO tracking the number of tickets issued.
      *
      * Plutus Data encoding: `Constr 0 [issued]`
      */
    case class TicketerDatum(issued: BigInt) derives ToData, FromData

    /** Redeemer for spending the issuer UTxO.
      *
      * Plutus Data encoding:
      *   - `BuyTicket`: `Constr 0 []` - purchase a ticket
      *   - `AdminWithdraw`: `Constr 1 []` - admin withdrawal
      */
    enum TicketerRedeemer derives ToData, FromData {
        case BuyTicket
        case AdminWithdraw
    }

    /** Redeemer for the ticket minting policy.
      *
      * Plutus Data encoding:
      *   - `Mint`: `Constr 0 []` - mint a new ticket NFT
      *   - `Burn`: `Constr 1 []` - burn a ticket NFT
      */
    enum TicketPolicyRedeemer derives ToData, FromData {
        case Mint
        case Burn
    }

    /** Finds the issuer UTxO by looking for the beacon token.
      *
      * The issuer UTxO is identified by containing the BUIDLERFEST2026 beacon token. This UTxO also
      * holds the datum with the current ticket counter.
      *
      * @param utxos
      *   UTxOs to search through (typically from the issuer address)
      * @return
      *   The issuer UTxO if found, None otherwise
      */
    def findIssuerUtxo(utxos: Utxos): Option[Utxo] = {
        utxos
            .find { case (_, output) =>
                output.value.assets.assets
                    .get(issuerBeaconPolicy)
                    .flatMap(_.get(issuerBeaconName))
                    .exists(_ > 0)
            }
            .map(Utxo.apply)
    }

    /** Builds a ticket registration transaction for BuidlerFest 2026.
      *
      * This transaction performs the following:
      *   1. Spends the issuer UTxO (containing beacon token and counter datum)
      *   2. Increments the ticket counter in the datum
      *   3. Mints a new ticket NFT named "TICKET{n}" where n is the ticket number
      *   4. Sends the ticket price to the treasury
      *   5. Sends the ticket NFT to the buyer
      *
      * The transaction uses a reference script to avoid including the full validator, reducing
      * transaction size and fees.
      *
      * @param provider
      *   Blockfrost provider for fetching UTxOs and protocol parameters
      * @param buyerAddress
      *   Address of the ticket buyer (receives the ticket NFT and pays for the transaction)
      * @return
      *   Unsigned balanced transaction ready for signing
      * @throws RuntimeException
      *   if the issuer UTxO or reference script cannot be found
      */
    def buildRegistrationTx(
        provider: BlockfrostProvider,
        buyerAddress: Address,
    ): Transaction = {
        given env: CardanoInfo = CardanoInfo.mainnet

        // Fetch issuer UTxO (contains the ticket counter)
        val issuerUtxos = provider.findUtxos(issuer).await().toOption.get
        val issuerUtxo = findIssuerUtxo(issuerUtxos)
            .getOrElse(sys.error("Issuer UTxO not found"))

        // Fetch the reference script UTxO
        val refScriptUtxo = provider
            .findUtxo(issuerScriptRef)
            .await()
            .getOrElse(sys.error(s"Reference script UTxO not found: $issuerScriptRef"))

        // Parse current datum to get ticket counter
        val currentDatum = issuerUtxo.output.requireInlineDatum.to[TicketerDatum]
        val ticketNumber = currentDatum.issued

        // Create new datum with incremented counter
        val newDatum = TicketerDatum(ticketNumber + 1)

        // Ticket asset name: "TICKET" + number
        val ticketName = AssetName.fromString(s"TICKET$ticketNumber")

        // Use early bird pricing
        val price = blindPrice

        // Build the transaction
        val now = Instant.now()
        val tx = TxBuilder(env)
            // Reference the script UTxO (contains the validator)
            .references(refScriptUtxo)
            // Spend the issuer UTxO with BuyTicket redeemer
            .spend(issuerUtxo, TicketerRedeemer.BuyTicket)
            // Mint the ticket NFT
            .mint(
              ticketPolicy,
              Map(ticketName -> 1L),
              TicketPolicyRedeemer.Mint
            )
            // Output 1: Updated issuer UTxO with new datum
            .output(
              TransactionOutput(
                address = issuer,
                value = issuerUtxo.output.value, // Same value (beacon token + min ADA)
                datumOption = DatumOption.Inline(newDatum.toData)
              )
            )
            // Output 2: Payment to treasury
            .payTo(treasury, price)
            // Output 3: Ticket to buyer (will be added via change handling)
            .payTo(
              buyerAddress,
              Value.asset(ticketPolicy, ticketName, 1) + Value.ada(2) // min ADA
            )
            // Set validity interval (10 minutes from now)
            .validTo(now.plusSeconds(600))
            // Complete the transaction (fetch buyer UTxOs for funding)
            .complete(provider = provider, sponsor = buyerAddress)
            .await()
            .transaction

        println(s"Built transaction for ticket #$ticketNumber, ticket name: $ticketName")

        tx
    }

    /** Entry point for building a ticket registration transaction.
      *
      * Requires `BLOCKFROST_API_KEY` environment variable to be set. Prints the unsigned
      * transaction in both human-readable and CBOR hex format.
      *
      * @param buyerAddress
      *   Bech32-encoded buyer address (e.g., addr1...)
      */
    @main
    def main(buyerAddress: String): Unit = {
        val apiKey = System.getenv("BLOCKFROST_API_KEY") ?? sys.error(
          "BLOCKFROST_API_KEY is not set, please set it before running the test"
        )
        val provider = BlockfrostProvider.mainnet(apiKey).await()

        // Print loaded constants for verification
        println(s"Issuer Beacon Policy: ${issuerBeaconPolicy.toHex}")
        println(s"Issuer Beacon Name: ${issuerBeaconName.bytes.toHex}")
        println(s"Treasury: ${treasury.encode.get}")
        println(
          s"Issuer Script Ref: ${issuerScriptRef.transactionId.toHex}#${issuerScriptRef.index}"
        )
        println(s"Ticket Policy: ${ticketPolicy.toHex}")
        println(s"Issuer: ${issuer.encode.get}")

        val buyer = Address.fromBech32(buyerAddress)
        val tx = buildRegistrationTx(provider, buyer)
        println(tx)
        println("Registraton tx cbor hex:")
        println(tx.toCbor.toHex)
    }
}
