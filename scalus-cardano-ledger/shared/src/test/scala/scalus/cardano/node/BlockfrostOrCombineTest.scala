package scalus.cardano.node

import org.scalatest.funsuite.AnyFunSuite
import scalus.cardano.address.{Address, Network, ShelleyAddress, ShelleyDelegationPart, ShelleyPaymentPart}
import scalus.cardano.ledger.*
import scalus.uplc.builtin.ByteString

/** `Or` is documented as the union of two sources, so it needs both of them.
  *
  * It used to answer with whichever half succeeded, as a `Right`. That made
  * `FromPaymentCredential(c) || FromAddress(a)` return `a`'s UTxOs and report success - and
  * `FromPaymentCredential` *always* fails against Blockfrost, which has no reverse index from a
  * payment credential to the addresses carrying it. So that query silently answered a different
  * question than the one asked, every time, with nothing in the result to say so.
  */
class BlockfrostOrCombineTest extends AnyFunSuite {

    private def addressAt(seed: String): Address = ShelleyAddress(
      Network.Testnet,
      ShelleyPaymentPart.Key(Hash[Blake2b_224, HashPurpose.KeyHash](ByteString.fromHex(seed * 28))),
      ShelleyDelegationPart.Null
    )

    private def utxoAt(seed: String, index: Int, coin: Long): Utxos = Map(
      TransactionInput(TransactionHash.fromHex(seed * 32), index) ->
          TransactionOutput(addressAt(seed), Value(Coin(coin)))
    )

    private val left = utxoAt("11", 0, 1_000_000L)
    private val right = utxoAt("22", 1, 2_000_000L)

    private val notSupported = UtxoQueryError.NotSupported(
      UtxoQuery(
        UtxoSource.FromPaymentCredential(Credential.KeyHash(AddrKeyHash.fromHex("33" * 28)))
      ),
      "Blockfrost has no reverse index from a payment credential to its addresses"
    )
    private val networkError = UtxoQueryError.NetworkError("connection reset", None)

    test("both sides succeeding gives the union") {
        val combined = BlockfrostProvider.combineOr(Right(left), Right(right))
        assert(combined == Right(left ++ right))
        assert(combined.toOption.get.size == 2)
    }

    test("an unsupported left side fails the query instead of answering with the right side") {
        // The case Ruslan reported: `credential || address` used to come back as the address's
        // UTxOs, indistinguishable from a genuine union that happened to hold only those.
        val combined = BlockfrostProvider.combineOr(Left(notSupported), Right(right))
        assert(combined == Left(notSupported))
    }

    test("an unsupported right side fails the query too") {
        val combined = BlockfrostProvider.combineOr(Right(left), Left(notSupported))
        assert(combined == Left(notSupported))
    }

    test("a transient failure on either side also fails, rather than halving the answer") {
        // A network error is not "this side is empty" either. Returning the reachable half as a
        // success would leave a caller doing coin selection against an arbitrary subset of its
        // own UTxOs, and no way to notice.
        assert(BlockfrostProvider.combineOr(Left(networkError), Right(right)) == Left(networkError))
        assert(BlockfrostProvider.combineOr(Right(left), Left(networkError)) == Left(networkError))
    }

    test("when both sides fail the left error is the one reported") {
        assert(
          BlockfrostProvider.combineOr(Left(notSupported), Left(networkError)) == Left(notSupported)
        )
    }

    test("the union is by input, so a UTxO present on both sides appears once") {
        val combined = BlockfrostProvider.combineOr(Right(left), Right(left))
        assert(combined == Right(left))
        assert(combined.toOption.get.size == 1)
    }
}
