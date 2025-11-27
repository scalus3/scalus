package scalus.cardano.txbuilder

import org.scalacheck.Arbitrary
import org.scalatest.funsuite.AnyFunSuite
import scalus.builtin.ByteString.utf8
import scalus.cardano.ledger.*
import scalus.cardano.ledger.ArbitraryInstances.given_Arbitrary_Hash
import scalus.cardano.txbuilder.TxBalancingError.InsufficientFunds

class ChangeTest extends AnyFunSuite {

    val testEnv: CardanoInfo = CardanoInfo.mainnet

    test("does not kill the multiassets") {
        val asset = MultiAsset.asset(
          Arbitrary.arbitrary[ScriptHash].sample.get,
          AssetName(utf8"co2"),
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

        Change.handleChange(
          diff = Value.zero,
          tx,
          changeOutput.address,
          testEnv.protocolParams
        ) match {
            case Right(value) =>
                val outs = value.body.value.outputs.map(_.value)
                assert(outs.size == 2)
                val o = outs.find(_.address == changeOutput.address).get
                assert(o.value.assets == asset)
            case Left(value) => fail("Expected transaction to balance successfully")
        }
    }

    test(
      "should fail when trying to remove more tokens than exist in change output"
    ) {
        val policyId = Arbitrary.arbitrary[ScriptHash].sample.get
        val assetName = AssetName(utf8"token")

        val changeAssets = MultiAsset.asset(policyId, assetName, 50)
        val changeAddr = genPubkeyAddr().sample.get
        val changeOutput = TransactionOutput(changeAddr, Value(Coin.ada(2), changeAssets))

        val otherOutput = TransactionOutput(genPubkeyAddr().sample.get, Value(Coin.ada(3)))

        val input = genTransactionInput.sample.get
        val tx = makeSimpleTx(
          ins = Seq(input),
          outs = Seq(otherOutput, changeOutput),
          fee = Coin.ada(1)
        )

        val diff = Value(
          Coin.zero,
          MultiAsset.asset(policyId, assetName, -100)
        )

        val result = Change.handleChange(
          diff,
          tx,
          changeAddr,
          testEnv.protocolParams
        )

        result match {
            case Right(updatedTx) =>
                val changeOut = updatedTx.body.value.outputs
                    .find(_.value.address == changeAddr)
                    .get
                    .value
                    .value

                val finalTokenAmount = changeOut.assets.assets(policyId)(assetName)

                fail(
                  s"Expected InsufficientFunds error but got success with $finalTokenAmount tokens"
                )

            case Left(InsufficientFunds(_, _)) => succeed
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
