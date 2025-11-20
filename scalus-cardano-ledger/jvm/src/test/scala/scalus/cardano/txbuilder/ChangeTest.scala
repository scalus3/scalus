package scalus.cardano.txbuilder

import org.scalacheck.Arbitrary
import org.scalatest.funsuite.AnyFunSuite
import scalus.builtin.ByteString
import scalus.cardano.ledger.ArbitraryInstances.given_Arbitrary_Hash
import scalus.cardano.ledger.*

class ChangeTest extends AnyFunSuite {

    val testEnv: CardanoInfo = CardanoInfo.mainnet

    test("does not kill the multiassets") {
        val asset = MultiAsset.asset(
          Arbitrary.arbitrary[ScriptHash].sample.get,
          AssetName(ByteString.fromString("co2")),
          1
        )

        val inputAda = Coin.ada(100_000L)
        val outputAda = Coin.ada(80_000L)
        // ensure change = 0
        val generousFee = inputAda - outputAda

        val output = TransactionOutput(genPubkeyAddr().sample.get, Value(outputAda))
        val changeOutput = TransactionOutput(genPubkeyAddr().sample.get, Value(Coin.zero, asset))

        val input = genTransactionInput.sample.get
        val tx = makeSimpleTx(
          // A single random input is enough
          ins = Seq(input),
          outs = Seq(output, changeOutput),
          fee = generousFee
        )

        Change.handleChange(diff = 0L, tx, changeOutput.address, testEnv.protocolParams) match {
            case Right(value) =>
                val outs = value.body.value.outputs.map(_.value)
                assert(outs.size == 2)
                val o = outs.find(_.address == changeOutput.address).get
                assert(o.value.assets == asset)
            case Left(value) => fail("Expected transaction to balance successfully")
        }
    }

    private def makeSimpleTx(
        ins: Seq[TransactionInput],
        outs: Seq[TransactionOutput],
        fee: Coin
    ) = {
        val body = TransactionBody(
          TaggedSortedSet(ins*),
          outs.toIndexedSeq.map(Sized.apply),
          fee
        )
        Transaction(
          body,
          TransactionWitnessSet.empty
        )
    }
}
