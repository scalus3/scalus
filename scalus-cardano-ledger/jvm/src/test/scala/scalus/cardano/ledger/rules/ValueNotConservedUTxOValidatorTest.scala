package scalus.cardano.ledger
package rules

import org.scalacheck.Arbitrary.arbitrary
import org.scalatest.funsuite.AnyFunSuite
import scalus.cardano.address.Address

import scala.collection.immutable.SortedMap

class ValueNotConservedUTxOValidatorTest extends AnyFunSuite, ArbitraryInstances {

    private def createInput(): TransactionInput = arbitrary[TransactionInput].sample.get
    private def createAddress(): Address = arbitrary[Address].sample.get
    private def createPolicyId(): PolicyId = arbitrary[PolicyId].sample.get
    private def createAssetName(): AssetName = arbitrary[AssetName].sample.get

    private val defaultContext = Context()
    private val defaultState = State()

    // ============ Basic Value Conservation Tests ============

    test("empty transaction preserves value") {
        val tx = Transaction(
          body = TransactionBody(
            inputs = TaggedSortedSet.empty,
            outputs = IndexedSeq.empty,
            fee = Coin.zero
          ),
          witnessSet = TransactionWitnessSet.empty
        )

        val result = ValueNotConservedUTxOValidator.validate(defaultContext, defaultState, tx)
        assert(result.isRight)
    }

    test("single input equals single output plus fee") {
        val input = createInput()
        val resolvedOutput = TransactionOutput(
          address = createAddress(),
          value = Value(coin = Coin(1_000_000))
        )

        val output = TransactionOutput(
          address = createAddress(),
          value = Value(coin = Coin(900_000))
        )

        val state = State(utxos = Map(input -> resolvedOutput))
        val tx = Transaction(
          body = TransactionBody(
            inputs = TaggedSortedSet.from(Set(input)),
            outputs = IndexedSeq(Sized(output)),
            fee = Coin(100_000)
          ),
          witnessSet = TransactionWitnessSet.empty
        )

        val result = ValueNotConservedUTxOValidator.validate(defaultContext, state, tx)
        assert(result.isRight)
    }

    test("fails when output exceeds input") {
        val input = createInput()
        val resolvedOutput = TransactionOutput(
          address = createAddress(),
          value = Value(coin = Coin(500_000))
        )

        val output = TransactionOutput(
          address = createAddress(),
          value = Value(coin = Coin(600_000))
        )

        val state = State(utxos = Map(input -> resolvedOutput))
        val tx = Transaction(
          body = TransactionBody(
            inputs = TaggedSortedSet.from(Set(input)),
            outputs = IndexedSeq(Sized(output)),
            fee = Coin.zero
          ),
          witnessSet = TransactionWitnessSet.empty
        )

        val result = ValueNotConservedUTxOValidator.validate(defaultContext, state, tx)
        assert(result.isLeft)
    }

    // ============ Multiple Inputs Tests ============

    test("multiple inputs aggregate correctly") {
        val input1 = createInput()
        val input2 = createInput()
        val input3 = createInput()

        val resolvedOutput1 = TransactionOutput(
          address = createAddress(),
          value = Value(coin = Coin(300_000))
        )
        val resolvedOutput2 = TransactionOutput(
          address = createAddress(),
          value = Value(coin = Coin(400_000))
        )
        val resolvedOutput3 = TransactionOutput(
          address = createAddress(),
          value = Value(coin = Coin(300_000))
        )

        val output = TransactionOutput(
          address = createAddress(),
          value = Value(coin = Coin(900_000))
        )

        val state = State(utxos =
            Map(
              input1 -> resolvedOutput1,
              input2 -> resolvedOutput2,
              input3 -> resolvedOutput3
            )
        )
        val tx = Transaction(
          body = TransactionBody(
            inputs = TaggedSortedSet.from(Set(input1, input2, input3)),
            outputs = IndexedSeq(Sized(output)),
            fee = Coin(100_000)
          ),
          witnessSet = TransactionWitnessSet.empty
        )

        val result = ValueNotConservedUTxOValidator.validate(defaultContext, state, tx)
        assert(result.isRight)
    }

    // ============ Multiple Outputs Tests ============

    test("single input distributes to multiple outputs") {
        val input = createInput()
        val resolvedOutput = TransactionOutput(
          address = createAddress(),
          value = Value(coin = Coin(1_000_000))
        )

        val output1 = TransactionOutput(
          address = createAddress(),
          value = Value(coin = Coin(300_000))
        )
        val output2 = TransactionOutput(
          address = createAddress(),
          value = Value(coin = Coin(400_000))
        )
        val output3 = TransactionOutput(
          address = createAddress(),
          value = Value(coin = Coin(250_000))
        )

        val state = State(utxos = Map(input -> resolvedOutput))
        val tx = Transaction(
          body = TransactionBody(
            inputs = TaggedSortedSet.from(Set(input)),
            outputs = IndexedSeq(Sized(output1), Sized(output2), Sized(output3)),
            fee = Coin(50_000)
          ),
          witnessSet = TransactionWitnessSet.empty
        )

        val result = ValueNotConservedUTxOValidator.validate(defaultContext, state, tx)
        assert(result.isRight)
    }

    test("multiple inputs to multiple outputs") {
        val input1 = createInput()
        val input2 = createInput()

        val resolvedOutput1 = TransactionOutput(
          address = createAddress(),
          value = Value(coin = Coin(600_000))
        )
        val resolvedOutput2 = TransactionOutput(
          address = createAddress(),
          value = Value(coin = Coin(400_000))
        )

        val output1 = TransactionOutput(
          address = createAddress(),
          value = Value(coin = Coin(450_000))
        )
        val output2 = TransactionOutput(
          address = createAddress(),
          value = Value(coin = Coin(500_000))
        )

        val state = State(utxos =
            Map(
              input1 -> resolvedOutput1,
              input2 -> resolvedOutput2
            )
        )
        val tx = Transaction(
          body = TransactionBody(
            inputs = TaggedSortedSet.from(Set(input1, input2)),
            outputs = IndexedSeq(Sized(output1), Sized(output2)),
            fee = Coin(50_000)
          ),
          witnessSet = TransactionWitnessSet.empty
        )

        val result = ValueNotConservedUTxOValidator.validate(defaultContext, state, tx)
        assert(result.isRight)
    }

    // ============ Fee Tests ============

    test("zero fee is valid") {
        val input = createInput()
        val resolvedOutput = TransactionOutput(
          address = createAddress(),
          value = Value(coin = Coin(1_000_000))
        )

        val output = TransactionOutput(
          address = createAddress(),
          value = Value(coin = Coin(1_000_000))
        )

        val state = State(utxos = Map(input -> resolvedOutput))
        val tx = Transaction(
          body = TransactionBody(
            inputs = TaggedSortedSet.from(Set(input)),
            outputs = IndexedSeq(Sized(output)),
            fee = Coin.zero
          ),
          witnessSet = TransactionWitnessSet.empty
        )

        val result = ValueNotConservedUTxOValidator.validate(defaultContext, state, tx)
        assert(result.isRight)
    }

    test("entire input becomes fee") {
        val input = createInput()
        val resolvedOutput = TransactionOutput(
          address = createAddress(),
          value = Value(coin = Coin(1_000_000))
        )

        val state = State(utxos = Map(input -> resolvedOutput))
        val tx = Transaction(
          body = TransactionBody(
            inputs = TaggedSortedSet.from(Set(input)),
            outputs = IndexedSeq.empty,
            fee = Coin(1_000_000)
          ),
          witnessSet = TransactionWitnessSet.empty
        )

        val result = ValueNotConservedUTxOValidator.validate(defaultContext, state, tx)
        assert(result.isRight)
    }

    // ============ Minting Tests ============

    test("minting native assets increases consumed value") {
        val input = createInput()
        val policyId = createPolicyId()
        val assetName = createAssetName()

        val resolvedOutput = TransactionOutput(
          address = createAddress(),
          value = Value(coin = Coin(2_000_000))
        )

        val mintedAssets = Map(assetName -> 100L)
        val output = TransactionOutput(
          address = createAddress(),
          value = Value(
            coin = Coin(1_900_000),
            assets = MultiAsset.fromPolicy(policyId, mintedAssets)
          )
        )

        val state = State(utxos = Map(input -> resolvedOutput))
        val tx = Transaction(
          body = TransactionBody(
            inputs = TaggedSortedSet.from(Set(input)),
            outputs = IndexedSeq(Sized(output)),
            fee = Coin(100_000),
            mint = Some(Mint(MultiAsset.fromPolicy(policyId, mintedAssets)))
          ),
          witnessSet = TransactionWitnessSet.empty
        )

        val result = ValueNotConservedUTxOValidator.validate(defaultContext, state, tx)
        assert(result.isRight)
    }

    test("minting multiple asset types") {
        val input = createInput()
        val policyId1 = createPolicyId()
        val policyId2 = createPolicyId()
        val assetName1 = createAssetName()
        val assetName2 = createAssetName()

        val resolvedOutput = TransactionOutput(
          address = createAddress(),
          value = Value(coin = Coin(2_000_000))
        )

        val mintedAssets1 = Map(assetName1 -> 50L)
        val mintedAssets2 = Map(assetName2 -> 75L)
        val output = TransactionOutput(
          address = createAddress(),
          value = Value(
            coin = Coin(1_900_000),
            assets =
                MultiAsset.fromAssets(Map(policyId1 -> mintedAssets1, policyId2 -> mintedAssets2))
          )
        )

        val state = State(utxos = Map(input -> resolvedOutput))
        val tx = Transaction(
          body = TransactionBody(
            inputs = TaggedSortedSet.from(Set(input)),
            outputs = IndexedSeq(Sized(output)),
            fee = Coin(100_000),
            mint = Some(
              Mint(
                MultiAsset.fromAssets(
                  Map(
                    policyId1 -> mintedAssets1,
                    policyId2 -> mintedAssets2
                  )
                )
              )
            )
          ),
          witnessSet = TransactionWitnessSet.empty
        )

        val result = ValueNotConservedUTxOValidator.validate(defaultContext, state, tx)
        assert(result.isRight)
    }

    // ============ Burning Tests ============

    test("burning native assets decreases produced value") {
        val input = createInput()
        val policyId = createPolicyId()
        val assetName = createAssetName()

        val inputAssets = Map(assetName -> 100L)
        val resolvedOutput = TransactionOutput(
          address = createAddress(),
          value = Value(
            coin = Coin(2_000_000),
            assets = MultiAsset.fromPolicy(policyId, inputAssets)
          )
        )

        val output = TransactionOutput(
          address = createAddress(),
          value = Value(coin = Coin(1_900_000))
        )

        val state = State(utxos = Map(input -> resolvedOutput))
        val tx = Transaction(
          body = TransactionBody(
            inputs = TaggedSortedSet.from(Set(input)),
            outputs = IndexedSeq(Sized(output)),
            fee = Coin(100_000),
            mint = Some(Mint(MultiAsset.fromAssets(Map(policyId -> Map(assetName -> -100L)))))
          ),
          witnessSet = TransactionWitnessSet.empty
        )

        val result = ValueNotConservedUTxOValidator.validate(defaultContext, state, tx)
        assert(result.isRight)
    }

    test("partial burning preserves remaining assets") {
        val input = createInput()
        val policyId = createPolicyId()
        val assetName = createAssetName()

        val inputAssets = Map(assetName -> 100L)
        val resolvedOutput = TransactionOutput(
          address = createAddress(),
          value = Value(
            coin = Coin(2_000_000),
            assets = MultiAsset.fromPolicy(policyId, inputAssets)
          )
        )

        val outputAssets = Map(assetName -> 50L)
        val output = TransactionOutput(
          address = createAddress(),
          value = Value(
            coin = Coin(1_900_000),
            assets = MultiAsset.fromPolicy(policyId, outputAssets)
          )
        )

        val state = State(utxos = Map(input -> resolvedOutput))
        val tx = Transaction(
          body = TransactionBody(
            inputs = TaggedSortedSet.from(Set(input)),
            outputs = IndexedSeq(Sized(output)),
            fee = Coin(100_000),
            mint = Some(Mint(MultiAsset.fromAssets(Map(policyId -> Map(assetName -> -50L)))))
          ),
          witnessSet = TransactionWitnessSet.empty
        )

        val result = ValueNotConservedUTxOValidator.validate(defaultContext, state, tx)
        assert(result.isRight)
    }

    test("mint and burn same asset") {
        val input = createInput()
        val policyId = createPolicyId()
        val assetName1 = createAssetName()
        val assetName2 = createAssetName()

        val inputAssets = Map(assetName1 -> 100L)
        val resolvedOutput = TransactionOutput(
          address = createAddress(),
          value = Value(
            coin = Coin(2_000_000),
            assets = MultiAsset.fromPolicy(policyId, inputAssets)
          )
        )

        val outputAssets = Map(assetName2 -> 50L)
        val output = TransactionOutput(
          address = createAddress(),
          value = Value(
            coin = Coin(1_900_000),
            assets = MultiAsset.fromPolicy(policyId, outputAssets)
          )
        )

        val state = State(utxos = Map(input -> resolvedOutput))
        val tx = Transaction(
          body = TransactionBody(
            inputs = TaggedSortedSet.from(Set(input)),
            outputs = IndexedSeq(Sized(output)),
            fee = Coin(100_000),
            mint = Some(
              Mint(
                MultiAsset.fromAssets(
                  Map(
                    policyId -> Map(
                      assetName1 -> -100L,
                      assetName2 -> 50L
                    )
                  )
                )
              )
            )
          ),
          witnessSet = TransactionWitnessSet.empty
        )

        val result = ValueNotConservedUTxOValidator.validate(defaultContext, state, tx)
        assert(result.isRight)
    }

    // ============ Withdrawal Tests ============

    test("withdrawal increases consumed value") {
        val input = createInput()
        val rewardAccount = arbitrary[RewardAccount].sample.get

        val resolvedOutput = TransactionOutput(
          address = createAddress(),
          value = Value(coin = Coin(1_000_000))
        )

        val output = TransactionOutput(
          address = createAddress(),
          value = Value(coin = Coin(1_400_000))
        )

        val state = State(utxos = Map(input -> resolvedOutput))
        val tx = Transaction(
          body = TransactionBody(
            inputs = TaggedSortedSet.from(Set(input)),
            outputs = IndexedSeq(Sized(output)),
            fee = Coin(100_000),
            withdrawals = Some(Withdrawals(SortedMap(rewardAccount -> Coin(500_000))))
          ),
          witnessSet = TransactionWitnessSet.empty
        )

        val result = ValueNotConservedUTxOValidator.validate(defaultContext, state, tx)
        assert(result.isRight)
    }

    test("multiple withdrawals aggregate") {
        val input = createInput()
        val rewardAccount1 = arbitrary[RewardAccount].sample.get
        val rewardAccount2 = arbitrary[RewardAccount].sample.get

        val resolvedOutput = TransactionOutput(
          address = createAddress(),
          value = Value(coin = Coin(1_000_000))
        )

        val output = TransactionOutput(
          address = createAddress(),
          value = Value(coin = Coin(1_600_000))
        )

        val state = State(utxos = Map(input -> resolvedOutput))
        val tx = Transaction(
          body = TransactionBody(
            inputs = TaggedSortedSet.from(Set(input)),
            outputs = IndexedSeq(Sized(output)),
            fee = Coin(100_000),
            withdrawals = Some(
              Withdrawals(
                SortedMap(
                  rewardAccount1 -> Coin(300_000),
                  rewardAccount2 -> Coin(400_000)
                )
              )
            )
          ),
          witnessSet = TransactionWitnessSet.empty
        )

        val result = ValueNotConservedUTxOValidator.validate(defaultContext, state, tx)
        assert(result.isRight)
    }

    test("zero withdrawal has no effect") {
        val input = createInput()
        val rewardAccount = arbitrary[RewardAccount].sample.get

        val resolvedOutput = TransactionOutput(
          address = createAddress(),
          value = Value(coin = Coin(1_000_000))
        )

        val output = TransactionOutput(
          address = createAddress(),
          value = Value(coin = Coin(900_000))
        )

        val state = State(utxos = Map(input -> resolvedOutput))
        val tx = Transaction(
          body = TransactionBody(
            inputs = TaggedSortedSet.from(Set(input)),
            outputs = IndexedSeq(Sized(output)),
            fee = Coin(100_000),
            withdrawals = Some(Withdrawals(SortedMap(rewardAccount -> Coin.zero)))
          ),
          witnessSet = TransactionWitnessSet.empty
        )

        val result = ValueNotConservedUTxOValidator.validate(defaultContext, state, tx)
        assert(result.isRight)
    }

    // ============ Donation Tests ============

    test("donation increases produced value") {
        val input = createInput()
        val resolvedOutput = TransactionOutput(
          address = createAddress(),
          value = Value(coin = Coin(1_000_000))
        )

        val output = TransactionOutput(
          address = createAddress(),
          value = Value(coin = Coin(400_000))
        )

        val state = State(utxos = Map(input -> resolvedOutput))
        val tx = Transaction(
          body = TransactionBody(
            inputs = TaggedSortedSet.from(Set(input)),
            outputs = IndexedSeq(Sized(output)),
            fee = Coin(100_000),
            donation = Some(Coin(500_000)),
            currentTreasuryValue = Some(Coin(0))
          ),
          witnessSet = TransactionWitnessSet.empty
        )

        val result = ValueNotConservedUTxOValidator.validate(defaultContext, state, tx)
        assert(result.isRight)
    }

    test("zero donation has no effect") {
        val input = createInput()
        val resolvedOutput = TransactionOutput(
          address = createAddress(),
          value = Value(coin = Coin(1_000_000))
        )

        val output = TransactionOutput(
          address = createAddress(),
          value = Value(coin = Coin(900_000))
        )

        val state = State(utxos = Map(input -> resolvedOutput))
        val tx = Transaction(
          body = TransactionBody(
            inputs = TaggedSortedSet.from(Set(input)),
            outputs = IndexedSeq(Sized(output)),
            fee = Coin(100_000),
            donation = Some(Coin.zero),
            currentTreasuryValue = Some(Coin.zero)
          ),
          witnessSet = TransactionWitnessSet.empty
        )

        val result = ValueNotConservedUTxOValidator.validate(defaultContext, state, tx)
        assert(result.isRight)
    }

    // ============ Combined Scenarios Tests ============

    test("complex transaction with minting, burning, withdrawals, and donation") {
        val input = createInput()
        val policyId = createPolicyId()
        val assetName1 = createAssetName()
        val assetName2 = createAssetName()
        val rewardAccount = arbitrary[RewardAccount].sample.get

        val inputAssets = Map(assetName1 -> 100L)
        val resolvedOutput = TransactionOutput(
          address = createAddress(),
          value = Value(
            coin = Coin(5_000_000),
            assets = MultiAsset.fromPolicy(policyId, inputAssets)
          )
        )

        val outputAssets = Map(assetName1 -> 50L, assetName2 -> 75L)
        val output = TransactionOutput(
          address = createAddress(),
          value = Value(
            coin = Coin(3_000_000),
            assets = MultiAsset.fromPolicy(policyId, outputAssets)
          )
        )

        val state = State(utxos = Map(input -> resolvedOutput))
        val tx = Transaction(
          body = TransactionBody(
            inputs = TaggedSortedSet.from(Set(input)),
            outputs = IndexedSeq(Sized(output)),
            fee = Coin(200_000),
            mint = Some(
              Mint(
                MultiAsset.fromAssets(
                  Map(
                    policyId -> Map(
                      assetName1 -> -50L,
                      assetName2 -> 75L
                    )
                  )
                )
              )
            ),
            withdrawals = Some(Withdrawals(SortedMap(rewardAccount -> Coin(1_000_000)))),
            donation = Some(Coin(2_800_000)),
            currentTreasuryValue = Some(Coin(0))
          ),
          witnessSet = TransactionWitnessSet.empty
        )

        val result = ValueNotConservedUTxOValidator.validate(defaultContext, state, tx)
        assert(result.isRight)
    }

    test("all components contribute to balance equation") {
        val input1 = createInput()
        val input2 = createInput()
        val policyId = createPolicyId()
        val assetName = createAssetName()
        val rewardAccount1 = arbitrary[RewardAccount].sample.get
        val rewardAccount2 = arbitrary[RewardAccount].sample.get

        val resolvedOutput1 = TransactionOutput(
          address = createAddress(),
          value = Value(coin = Coin(3_000_000))
        )
        val resolvedOutput2 = TransactionOutput(
          address = createAddress(),
          value = Value(coin = Coin(2_000_000))
        )

        val mintedAssets = Map(assetName -> 200L)
        val output1 = TransactionOutput(
          address = createAddress(),
          value = Value(
            coin = Coin(3_000_000),
            assets = MultiAsset.fromPolicy(policyId, mintedAssets)
          )
        )
        val output2 = TransactionOutput(
          address = createAddress(),
          value = Value(coin = Coin(1_500_000))
        )

        val state = State(utxos =
            Map(
              input1 -> resolvedOutput1,
              input2 -> resolvedOutput2
            )
        )
        val tx = Transaction(
          body = TransactionBody(
            inputs = TaggedSortedSet.from(Set(input1, input2)),
            outputs = IndexedSeq(Sized(output1), Sized(output2)),
            fee = Coin(300_000),
            mint = Some(Mint(MultiAsset.fromPolicy(policyId, mintedAssets))),
            withdrawals = Some(
              Withdrawals(
                SortedMap(
                  rewardAccount1 -> Coin(500_000),
                  rewardAccount2 -> Coin(300_000)
                )
              )
            ),
            donation = Some(Coin(1_000_000)),
            currentTreasuryValue = Some(Coin(0))
          ),
          witnessSet = TransactionWitnessSet.empty
        )

        val result = ValueNotConservedUTxOValidator.validate(defaultContext, state, tx)
        assert(result.isRight)
    }
}
