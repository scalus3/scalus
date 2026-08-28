package scalus.cardano.node

import org.scalatest.funsuite.AnyFunSuite
import scalus.cardano.address.ShelleyAddress
import scalus.cardano.ledger.*
import scalus.testing.kit.Party
import scalus.uplc.builtin.ByteString

/** Which inputs a transaction actually consumed, as `/txs/{hash}/utxos` reports it.
  *
  * The interesting part is collateral. Blockfrost describes a successful transaction and one whose
  * script phase failed through the same two arrays, distinguished only by a flag — and getting the
  * flag wrong is invisible to a subscriber: it either keeps a UTxO the chain has consumed or drops
  * one it still holds, with nothing downstream able to notice either.
  */
class BlockfrostTxEffectsTest extends AnyFunSuite {

    private given CardanoInfo = CardanoInfo.mainnet

    private def bech32(p: Party): String =
        p.address.asInstanceOf[ShelleyAddress].toBech32.get

    private val txHash: TransactionHash =
        TransactionHash.fromByteString(ByteString.fromHex("ab" * 32))

    private def hashOf(byte: String): TransactionHash =
        TransactionHash.fromByteString(ByteString.fromHex(byte * 32))

    private def ada(n: Long): ujson.Arr =
        ujson.Arr(ujson.Obj("unit" -> "lovelace", "quantity" -> (n * 1000000L).toString))

    private def in(
        party: Party,
        source: String,
        index: Int,
        collateral: Boolean = false,
        reference: Boolean = false
    ): ujson.Obj = ujson.Obj(
      "address" -> bech32(party),
      "amount" -> ada(10),
      "tx_hash" -> hashOf(source).toHex,
      "output_index" -> index,
      "collateral" -> collateral,
      "reference" -> reference
    )

    private def out(party: Party, index: Int, collateral: Boolean = false): ujson.Obj = ujson.Obj(
      "address" -> bech32(party),
      "amount" -> ada(9),
      "output_index" -> index,
      "collateral" -> collateral
    )

    private def utxos(inputs: Seq[ujson.Obj], outputs: Seq[ujson.Obj]): ujson.Value =
        ujson.Obj("inputs" -> ujson.Arr.from(inputs), "outputs" -> ujson.Arr.from(outputs))

    test("a successful transaction spends its inputs and not its collateral") {
        val json = utxos(
          Seq(
            in(Party.Alice, "01", 0),
            // Declared as collateral and not touched, because the scripts succeeded.
            in(Party.Bob, "02", 1, collateral = true)
          ),
          Seq(out(Party.Bob, 0))
        )
        val effects = BlockfrostProvider.parseTransactionEffects(txHash, json)
        assert(
          effects.spent.map(_.input) == Seq(TransactionInput(hashOf("01"), 0)),
          "unused collateral is still on chain; reporting it as spent would make a subscriber " +
              s"drop a UTxO it still holds — got ${effects.spent.map(_.input)}"
        )
        assert(effects.created.map(_.input) == Seq(TransactionInput(txHash, 0)))
    }

    test("a failed transaction spends its collateral and not its inputs") {
        val json = utxos(
          Seq(in(Party.Alice, "01", 0), in(Party.Bob, "02", 1, collateral = true)),
          // A collateral *return* is the only output a phase-2 failure leaves, and its presence is
          // how this response says the scripts failed.
          Seq(out(Party.Bob, 3, collateral = true))
        )
        val effects = BlockfrostProvider.parseTransactionEffects(txHash, json)
        assert(
          effects.spent.map(_.input) == Seq(TransactionInput(hashOf("02"), 1)),
          "the collateral was consumed and the ordinary inputs were not; a subscriber told the " +
              s"opposite is wrong about both — got ${effects.spent.map(_.input)}"
        )
    }

    test("reference inputs are never spent, whether the transaction succeeded or failed") {
        val referenced = in(Party.Carol, "03", 2, reference = true)
        val succeeded = utxos(Seq(in(Party.Alice, "01", 0), referenced), Seq(out(Party.Bob, 0)))
        val failed = utxos(
          Seq(in(Party.Bob, "02", 1, collateral = true), referenced),
          Seq(out(Party.Bob, 3, collateral = true))
        )
        for json <- Seq(succeeded, failed) do
            val spent = BlockfrostProvider.parseTransactionEffects(txHash, json).spent
            assert(
              !spent.map(_.input).contains(TransactionInput(hashOf("03"), 2)),
              s"a reference input is read, not consumed — got ${spent.map(_.input)}"
            )
    }

    test("output_index places an output, rather than its position in the array") {
        // The one case where the two differ: a failed transaction's collateral return sits at the
        // end of the body's output list, and is the only entry in this array.
        val json = utxos(
          Seq(in(Party.Bob, "02", 1, collateral = true)),
          Seq(out(Party.Bob, 7, collateral = true))
        )
        val created = BlockfrostProvider.parseTransactionEffects(txHash, json).created
        assert(
          created.map(_.input) == Seq(TransactionInput(txHash, 7)),
          "an index taken from the array position would name a UTxO that does not exist, and miss " +
              s"the one that does — got ${created.map(_.input)}"
        )
    }

    test("a missing output_index falls back to the array position") {
        val json = ujson.Obj(
          "inputs" -> ujson.Arr(in(Party.Alice, "01", 0)),
          "outputs" -> ujson.Arr(
            ujson.Obj("address" -> bech32(Party.Bob), "amount" -> ada(5)),
            ujson.Obj("address" -> bech32(Party.Alice), "amount" -> ada(4))
          )
        )
        val created = BlockfrostProvider.parseTransactionEffects(txHash, json).created
        assert(
          created.map(_.input) ==
              Seq(TransactionInput(txHash, 0), TransactionInput(txHash, 1)),
          s"outputs are dense and in order when the field is absent — got ${created.map(_.input)}"
        )
    }

    test("absent collateral and reference flags read as false") {
        // Older responses omit them entirely; treating a missing flag as `true` would silently
        // report a transaction as spending nothing.
        val json = ujson.Obj(
          "inputs" -> ujson.Arr(
            ujson.Obj(
              "address" -> bech32(Party.Alice),
              "amount" -> ada(10),
              "tx_hash" -> hashOf("01").toHex,
              "output_index" -> 0
            )
          ),
          "outputs" -> ujson.Arr(ujson.Obj("address" -> bech32(Party.Bob), "amount" -> ada(9)))
        )
        val effects = BlockfrostProvider.parseTransactionEffects(txHash, json)
        assert(effects.spent.map(_.input) == Seq(TransactionInput(hashOf("01"), 0)))
        assert(effects.created.size == 1)
    }

    test("the resolved outputs carry address and value, not just references") {
        // What makes `spent` usable at all: a subscriber watching an address needs to know which of
        // its UTxOs disappeared, and an input reference alone does not say.
        val json = utxos(Seq(in(Party.Alice, "01", 0)), Seq(out(Party.Bob, 0)))
        val spent = BlockfrostProvider.parseTransactionEffects(txHash, json).spent
        assert(spent.head.output.address == Party.Alice.address)
        assert(spent.head.output.value == Value.ada(10))
    }
}
