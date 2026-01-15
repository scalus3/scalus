package scalus.examples.auction

import org.scalatest.funsuite.AnyFunSuite
import scalus.builtin.ByteString
import scalus.builtin.ByteString.*
import scalus.builtin.Data.toData
import scalus.cardano.ledger.*
import scalus.cardano.ledger.rules.{Context, PlutusScriptsTransactionMutator}
import scalus.cardano.node.Emulator
import scalus.cardano.txbuilder.{RedeemerPurpose, TxBuilder}
import scalus.compiler.Options
import scalus.ledger.api.v1.{PosixTime, PubKeyHash}
import scalus.prelude.Option as ScalusOption
import scalus.testing.kit.TestUtil.{genesisHash, getScriptContextV3}
import scalus.testing.kit.{ScalusTest, TestUtil}
import scalus.testing.kit.Party.{Alice, Bob}
import scalus.uplc.PlutusV3
import scalus.utils.await

import java.time.Instant
import scala.concurrent.ExecutionContext.Implicits.global

/** Test demonstrating the Double Satisfaction vulnerability (V005).
  *
  * Attack: Bob wins two auctions from Alice, ends both in one tx paying Alice only once.
  */
class DoubleSatisfactionAttackTest extends AnyFunSuite, ScalusTest {
    private given env: CardanoInfo = TestUtil.testEnvironment
    private given Options = Options.release.copy(generateErrorTraces = true)

    private val vulnerableContract = PlutusV3.compile(UnfixedAuctionValidator.validate)
    private val fixedContract = AuctionContract.withErrorTraces

    private val seller = Alice
    private val buyer = Bob
    private val sellerPkh = PubKeyHash(ByteString.fromArray(seller.addrKeyHash.bytes))
    private val buyerPkh = PubKeyHash(ByteString.fromArray(buyer.addrKeyHash.bytes))
    private val itemIdA = utf8"item-A"
    private val itemIdB = utf8"item-B"
    private val bidAmount = 10_000_000L
    private val auctionEndTime: PosixTime = BigInt(env.slotConfig.slotToTime(100))

    test("VULNERABILITY: Double satisfaction attack SUCCEEDS on UnfixedAuctionValidator") {
        val script = vulnerableContract.script
        val policyId = script.scriptHash
        val scriptAddress = vulnerableContract.address(env.network)

        // Create two won auction UTxOs
        val datumA =
            Datum(
              sellerPkh,
              ScalusOption.Some(buyerPkh),
              BigInt(bidAmount),
              auctionEndTime,
              itemIdA
            )
        val datumB =
            Datum(
              sellerPkh,
              ScalusOption.Some(buyerPkh),
              BigInt(bidAmount),
              auctionEndTime,
              itemIdB
            )

        val auctionUtxoA = Utxo(
          Input(genesisHash, 10),
          TransactionOutput.Babbage(
            address = scriptAddress,
            value = Value
                .lovelace(bidAmount + 2_000_000L) + Value.asset(policyId, AssetName(itemIdA), 1L),
            datumOption = Some(DatumOption.Inline(datumA.toData))
          )
        )
        val auctionUtxoB = Utxo(
          Input(genesisHash, 11),
          TransactionOutput.Babbage(
            address = scriptAddress,
            value = Value
                .lovelace(bidAmount + 2_000_000L) + Value.asset(policyId, AssetName(itemIdB), 1L),
            datumOption = Some(DatumOption.Inline(datumB.toData))
          )
        )

        val provider = Emulator(
          initialUtxos = Map(
            auctionUtxoA.input -> auctionUtxoA.output,
            auctionUtxoB.input -> auctionUtxoB.output,
            Input(genesisHash, 0) -> TransactionOutput
                .Babbage(buyer.address, Value.lovelace(100_000_000L))
          ),
          initialContext = Context.testMainnet(slot = 200),
          mutators = Set(PlutusScriptsTransactionMutator)
        )

        // Build ATTACK transaction: spend both auctions, pay seller only ONCE
        val tx = TxBuilder(env)
            .spend(
              auctionUtxoA,
              redeemerBuilder = (tx: Transaction) => {
                  val inputIdx = tx.body.value.inputs.toSeq.indexOf(auctionUtxoA.input)
                  val sellerIdx =
                      tx.body.value.outputs.indexWhere(_.value.address == seller.address)
                  val winnerIdx = tx.body.value.outputs.indexWhere(_.value.address == buyer.address)
                  Action.End(BigInt(inputIdx), BigInt(sellerIdx), BigInt(winnerIdx)).toData
              },
              script
            )
            .spend(
              auctionUtxoB,
              redeemerBuilder = (tx: Transaction) => {
                  val inputIdx = tx.body.value.inputs.toSeq.indexOf(auctionUtxoB.input)
                  // ATTACK: Use SAME output indices as auction A!
                  val sellerIdx =
                      tx.body.value.outputs.indexWhere(_.value.address == seller.address)
                  val winnerIdx = tx.body.value.outputs.indexWhere(_.value.address == buyer.address)
                  Action.End(BigInt(inputIdx), BigInt(sellerIdx), BigInt(winnerIdx)).toData
              },
              script
            )
            // Pay seller only ONCE (should be twice!)
            .payTo(seller.address, Value.lovelace(bidAmount))
            // Winner gets BOTH NFTs in one output
            .payTo(
              buyer.address,
              Value.lovelace(2_000_000L) +
                  Value.asset(policyId, AssetName(itemIdA), 1L) +
                  Value.asset(policyId, AssetName(itemIdB), 1L)
            )
            .validFrom(Instant.ofEpochMilli(auctionEndTime.toLong + 1000))
            .complete(provider, buyer.address)
            .map(_.sign(buyer.signer).transaction)
            .await()

        // Get UTxOs for script context creation
        val utxos = Map(
          auctionUtxoA.input -> auctionUtxoA.output,
          auctionUtxoB.input -> auctionUtxoB.output
        )

        // Run validator for auction A
        val scriptContextA =
            tx.getScriptContextV3(utxos, RedeemerPurpose.ForSpend(auctionUtxoA.input))
        val resultA = vulnerableContract.program.runWithDebug(scriptContextA)
        assert(
          resultA.isSuccess,
          s"Vulnerable validator A should pass attack: ${resultA.logs.mkString(", ")}"
        )

        // Run validator for auction B - this is the DOUBLE SATISFACTION
        val scriptContextB =
            tx.getScriptContextV3(utxos, RedeemerPurpose.ForSpend(auctionUtxoB.input))
        val resultB = vulnerableContract.program.runWithDebug(scriptContextB)
        assert(
          resultB.isSuccess,
          s"Vulnerable validator B should pass attack: ${resultB.logs.mkString(", ")}"
        )
    }

    test("FIX VERIFICATION: Double satisfaction attack FAILS on fixed AuctionValidator") {
        val script = fixedContract.script
        val policyId = script.scriptHash
        val scriptAddress = fixedContract.address(env.network)

        val datumA =
            Datum(
              sellerPkh,
              ScalusOption.Some(buyerPkh),
              BigInt(bidAmount),
              auctionEndTime,
              itemIdA
            )
        val datumB =
            Datum(
              sellerPkh,
              ScalusOption.Some(buyerPkh),
              BigInt(bidAmount),
              auctionEndTime,
              itemIdB
            )

        val auctionUtxoA = Utxo(
          Input(genesisHash, 10),
          TransactionOutput.Babbage(
            address = scriptAddress,
            value = Value
                .lovelace(bidAmount + 2_000_000L) + Value.asset(policyId, AssetName(itemIdA), 1L),
            datumOption = Some(DatumOption.Inline(datumA.toData))
          )
        )
        val auctionUtxoB = Utxo(
          Input(genesisHash, 11),
          TransactionOutput.Babbage(
            address = scriptAddress,
            value = Value
                .lovelace(bidAmount + 2_000_000L) + Value.asset(policyId, AssetName(itemIdB), 1L),
            datumOption = Some(DatumOption.Inline(datumB.toData))
          )
        )

        val provider = Emulator(
          initialUtxos = Map(
            auctionUtxoA.input -> auctionUtxoA.output,
            auctionUtxoB.input -> auctionUtxoB.output,
            Input(genesisHash, 0) -> TransactionOutput
                .Babbage(buyer.address, Value.lovelace(100_000_000L))
          ),
          initialContext = Context.testMainnet(slot = 200),
          mutators = Set(PlutusScriptsTransactionMutator)
        )

        // Try to build attack transaction - fixed validator should reject during complete()
        val txResult = scala.util.Try {
            TxBuilder(env)
                .spend(
                  auctionUtxoA,
                  redeemerBuilder = (tx: Transaction) => {
                      val inputIdx = tx.body.value.inputs.toSeq.indexOf(auctionUtxoA.input)
                      val sellerIdx =
                          tx.body.value.outputs.indexWhere(_.value.address == seller.address)
                      val winnerIdx =
                          tx.body.value.outputs.indexWhere(_.value.address == buyer.address)
                      Action.End(BigInt(inputIdx), BigInt(sellerIdx), BigInt(winnerIdx)).toData
                  },
                  script
                )
                .spend(
                  auctionUtxoB,
                  redeemerBuilder = (tx: Transaction) => {
                      val inputIdx = tx.body.value.inputs.toSeq.indexOf(auctionUtxoB.input)
                      val sellerIdx =
                          tx.body.value.outputs.indexWhere(_.value.address == seller.address)
                      val winnerIdx =
                          tx.body.value.outputs.indexWhere(_.value.address == buyer.address)
                      Action.End(BigInt(inputIdx), BigInt(sellerIdx), BigInt(winnerIdx)).toData
                  },
                  script
                )
                .payTo(seller.address, Value.lovelace(bidAmount))
                .payTo(
                  buyer.address,
                  Value.lovelace(2_000_000L) +
                      Value.asset(policyId, AssetName(itemIdA), 1L) +
                      Value.asset(policyId, AssetName(itemIdB), 1L)
                )
                .validFrom(Instant.ofEpochMilli(auctionEndTime.toLong + 1000))
                .complete(provider, buyer.address)
                .await()
        }

        // The fixed validator should reject the attack during script evaluation in complete()
        assert(
          txResult.isFailure,
          "Fixed validator should reject double satisfaction attack during transaction building"
        )

        // Verify it's a script evaluation failure (not some other error)
        val errorMessage = txResult.failed.get.getMessage
        assert(
          errorMessage.contains("script") || errorMessage.contains("Error evaluated"),
          s"Expected script evaluation failure, but got: $errorMessage"
        )
    }
}
