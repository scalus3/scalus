package scalus.cardano.ledger
package rules

import org.scalacheck.Arbitrary.arbitrary
import org.scalatest.funsuite.AnyFunSuite
import scalus.cardano.address.{Address, Network}
import scalus.cardano.node.Emulator
import scalus.testing.kit.Party.{Alice, Bob}

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
        val resolvedOutput = Output(
          address = createAddress(),
          value = Value(coin = Coin(1_000_000))
        )

        val output = Output(
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
        val resolvedOutput = Output(
          address = createAddress(),
          value = Value(coin = Coin(500_000))
        )

        val output = Output(
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

        val resolvedOutput1 = Output(
          address = createAddress(),
          value = Value(coin = Coin(300_000))
        )
        val resolvedOutput2 = Output(
          address = createAddress(),
          value = Value(coin = Coin(400_000))
        )
        val resolvedOutput3 = Output(
          address = createAddress(),
          value = Value(coin = Coin(300_000))
        )

        val output = Output(
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
        val resolvedOutput = Output(
          address = createAddress(),
          value = Value(coin = Coin(1_000_000))
        )

        val output1 = Output(
          address = createAddress(),
          value = Value(coin = Coin(300_000))
        )
        val output2 = Output(
          address = createAddress(),
          value = Value(coin = Coin(400_000))
        )
        val output3 = Output(
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

        val resolvedOutput1 = Output(
          address = createAddress(),
          value = Value(coin = Coin(600_000))
        )
        val resolvedOutput2 = Output(
          address = createAddress(),
          value = Value(coin = Coin(400_000))
        )

        val output1 = Output(
          address = createAddress(),
          value = Value(coin = Coin(450_000))
        )
        val output2 = Output(
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
        val resolvedOutput = Output(
          address = createAddress(),
          value = Value(coin = Coin(1_000_000))
        )

        val output = Output(
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
        val resolvedOutput = Output(
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

        val resolvedOutput = Output(
          address = createAddress(),
          value = Value(coin = Coin(2_000_000))
        )

        val mintedAssets = Map(assetName -> 100L)
        val output = Output(
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

        val resolvedOutput = Output(
          address = createAddress(),
          value = Value(coin = Coin(2_000_000))
        )

        val mintedAssets1 = Map(assetName1 -> 50L)
        val mintedAssets2 = Map(assetName2 -> 75L)
        val output = Output(
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
        val resolvedOutput = Output(
          address = createAddress(),
          value = Value(
            coin = Coin(2_000_000),
            assets = MultiAsset.fromPolicy(policyId, inputAssets)
          )
        )

        val output = Output(
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
        val resolvedOutput = Output(
          address = createAddress(),
          value = Value(
            coin = Coin(2_000_000),
            assets = MultiAsset.fromPolicy(policyId, inputAssets)
          )
        )

        val outputAssets = Map(assetName -> 50L)
        val output = Output(
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
        val resolvedOutput = Output(
          address = createAddress(),
          value = Value(
            coin = Coin(2_000_000),
            assets = MultiAsset.fromPolicy(policyId, inputAssets)
          )
        )

        val outputAssets = Map(assetName2 -> 50L)
        val output = Output(
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

        val resolvedOutput = Output(
          address = createAddress(),
          value = Value(coin = Coin(1_000_000))
        )

        val output = Output(
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

        val resolvedOutput = Output(
          address = createAddress(),
          value = Value(coin = Coin(1_000_000))
        )

        val output = Output(
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

        val resolvedOutput = Output(
          address = createAddress(),
          value = Value(coin = Coin(1_000_000))
        )

        val output = Output(
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
        val resolvedOutput = Output(
          address = createAddress(),
          value = Value(coin = Coin(1_000_000))
        )

        val output = Output(
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
        val resolvedOutput = Output(
          address = createAddress(),
          value = Value(coin = Coin(1_000_000))
        )

        val output = Output(
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
        val resolvedOutput = Output(
          address = createAddress(),
          value = Value(
            coin = Coin(5_000_000),
            assets = MultiAsset.fromPolicy(policyId, inputAssets)
          )
        )

        val outputAssets = Map(assetName1 -> 50L, assetName2 -> 75L)
        val output = Output(
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

        val resolvedOutput1 = Output(
          address = createAddress(),
          value = Value(coin = Coin(3_000_000))
        )
        val resolvedOutput2 = Output(
          address = createAddress(),
          value = Value(coin = Coin(2_000_000))
        )

        val mintedAssets = Map(assetName -> 200L)
        val output1 = Output(
          address = createAddress(),
          value = Value(
            coin = Coin(3_000_000),
            assets = MultiAsset.fromPolicy(policyId, mintedAssets)
          )
        )
        val output2 = Output(
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

    // ============ Pool Deposit Tests ============

    private val poolOperator = AddrKeyHash.fromHex("1" * 56)
    private val poolRegistration: Certificate.PoolRegistration = Certificate.PoolRegistration(
      operator = poolOperator,
      vrfKeyHash = VrfKeyHash.fromHex("a" * 64),
      pledge = Coin.ada(100),
      cost = Coin(defaultContext.env.params.minPoolCost),
      margin = UnitInterval.one,
      rewardAccount = arbitrary[RewardAccount].sample.get,
      poolOwners = Set(poolOperator),
      relays = IndexedSeq.empty,
      poolMetadata = None
    )
    private val poolDeposit = defaultContext.env.params.stakePoolDeposit

    /** A tx that spends 10 ADA, pays `deposit` lovelace, the 0.1 ADA fee, and the rest to an
      * output, and carries `certificates`.
      */
    private def poolTx(deposit: Long, certificates: Certificate*): (State, Transaction) = {
        val input = createInput()
        val inputCoin = 10_000_000L + poolDeposit
        val output = Output(createAddress(), Value(Coin(inputCoin - 100_000 - deposit)))
        val tx = Transaction(
          body = TransactionBody(
            inputs = TaggedSortedSet.from(Set(input)),
            outputs = IndexedSeq(Sized(output)),
            fee = Coin(100_000),
            certificates = TaggedOrderedStrictSet.from(certificates)
          ),
          witnessSet = TransactionWitnessSet.empty
        )
        (State(utxos = Map(input -> Output(createAddress(), Value(Coin(inputCoin))))), tx)
    }

    test("a new pool registration pays the pool deposit") {
        val (state, tx) = poolTx(poolDeposit, poolRegistration)
        assert(ValueNotConservedUTxOValidator.validate(defaultContext, state, tx).isRight)
    }

    test("a re-registration of a registered pool pays no deposit") {
        val (state, tx) = poolTx(0, poolRegistration)
        val registered = state.copy(certState =
            CertState.empty.copy(pstate =
                PoolsState(stakePools =
                    Map(PoolKeyHash.fromByteString(poolOperator) -> poolRegistration)
                )
            )
        )
        assert(ValueNotConservedUTxOValidator.validate(defaultContext, registered, tx).isRight)
    }

    test("two registrations of one new pool in a tx pay one deposit") {
        val (state, tx) =
            poolTx(poolDeposit, poolRegistration, poolRegistration.copy(pledge = Coin.ada(200)))
        assert(ValueNotConservedUTxOValidator.validate(defaultContext, state, tx).isRight)
    }

    // ============ Sums beyond Long.MaxValue ============
    //
    // The ledger sums both sides with an unbounded `Integer`. A `Long` sum wraps around, and a
    // transaction can choose amounts that wrap its produced value onto its consumed value.

    private val token = createPolicyId()
    private val tokenName = createAssetName()
    private def tokens(amount: Long): MultiAsset = MultiAsset.asset(token, tokenName, amount)

    /** A tx spending one input per `inputs` into `outputs`, paying `fee`. */
    private def balanceTx(
        inputs: Seq[Value],
        outputs: Seq[Value],
        fee: Coin,
        mint: Option[Mint] = None,
        certificates: Seq[Certificate] = Seq.empty
    ): (State, Transaction) = {
        val utxos = inputs.map(value => createInput() -> Output(createAddress(), value)).toMap
        val tx = Transaction(
          body = TransactionBody(
            inputs = TaggedSortedSet.from(utxos.keySet),
            outputs = outputs.map(value => Sized(Output(createAddress(), value))).toIndexedSeq,
            fee = fee,
            mint = mint,
            certificates =
                if certificates.isEmpty then TaggedOrderedStrictSet.empty
                else TaggedOrderedStrictSet.from(certificates)
          ),
          witnessSet = TransactionWitnessSet.empty
        )
        (State(utxos = utxos), tx)
    }

    private def notConserved(state: State, tx: Transaction) =
        ValueNotConservedUTxOValidator.validate(defaultContext, state, tx) match
            case Left(e: TransactionException.ValueNotConservedUTxOException) => e
            case other => fail(s"expected ValueNotConservedUTxOException, got $other")

    test("outputs whose lovelace wraps around to the consumed amount do not conserve value") {
        // 2 * Long.MaxValue + 9_800_002 + 200_000 = 2^64 + 10_000_000, which wraps to 10 ADA
        val (state, tx) = balanceTx(
          inputs = Seq(Value.ada(10)),
          outputs = Seq(
            Value.lovelace(Long.MaxValue),
            Value.lovelace(Long.MaxValue),
            Value.lovelace(9_800_002)
          ),
          fee = Coin(200_000)
        )
        val error = notConserved(state, tx)
        assert(error.consumed == Value.ada(10))
        assert(error.produced == Value.lovelace(Long.MaxValue), "clamped to the Long range")
        assertThrows[ArithmeticException](utils.TxBalance.produced(tx, defaultContext.env.params))
    }

    test("outputs whose tokens wrap around to the consumed amount do not conserve value") {
        // 2 * Long.MaxValue + 3 = 2^64 + 1, which wraps to the 1 token consumed
        val (state, tx) = balanceTx(
          inputs = Seq(Value(Coin.ada(10), tokens(1))),
          outputs = Seq(
            Value(Coin.ada(2), tokens(Long.MaxValue)),
            Value(Coin.ada(2), tokens(Long.MaxValue)),
            Value(Coin(5_800_000), tokens(3))
          ),
          fee = Coin(200_000)
        )
        val error = notConserved(state, tx)
        assert(error.produced.assets == tokens(Long.MaxValue), "clamped to the Long range")
    }

    test("inputs whose tokens wrap around to zero do not conserve value") {
        // 4 * 2^62 = 2^64, which wraps to 0: the tokens would vanish without a burn
        val (state, tx) = balanceTx(
          inputs = Seq.fill(4)(Value(Coin.ada(2), tokens(1L << 62))),
          outputs = Seq(Value(Coin(7_800_000))),
          fee = Coin(200_000)
        )
        val error = notConserved(state, tx)
        assert(error.consumed.assets == tokens(Long.MaxValue), "clamped to the Long range")
        assert(error.produced == Value(Coin.ada(8)))
    }

    test("declared deposits whose sum wraps around do not conserve value") {
        // 2 * Long.MaxValue of deposits wraps to -2 lovelace, which the 2 extra lovelace output
        // would cancel
        def registration(n: Int) = Certificate.RegCert(
          Credential.KeyHash(AddrKeyHash.fromHex(n.toString * 56)),
          Some(Coin(Long.MaxValue))
        )
        val (state, tx) = balanceTx(
          inputs = Seq(Value.ada(10)),
          outputs = Seq(Value.lovelace(9_800_002)),
          fee = Coin(200_000),
          certificates = Seq(registration(2), registration(3))
        )
        notConserved(state, tx)
    }

    test("tokens summing past Long.MaxValue on both sides conserve value") {
        val amount = (1L << 62) + 1 // two of them exceed Long.MaxValue
        val (state, tx) = balanceTx(
          inputs = Seq.fill(2)(Value(Coin.ada(5), tokens(amount))),
          outputs = Seq(Value(Coin.ada(5), tokens(amount)), Value(Coin(4_800_000), tokens(amount))),
          fee = Coin(200_000)
        )
        assert(ValueNotConservedUTxOValidator.validate(defaultContext, state, tx).isRight)
    }

    test("a burn of 2^63 tokens conserves value") {
        // negating the mint's Long.MinValue does not fit in a Long
        val (state, tx) = balanceTx(
          inputs = Seq.fill(2)(Value(Coin.ada(5), tokens(1L << 62))),
          outputs = Seq(Value(Coin(9_800_000))),
          fee = Coin(200_000),
          mint = Some(Mint(tokens(Long.MinValue)))
        )
        assert(ValueNotConservedUTxOValidator.validate(defaultContext, state, tx).isRight)
    }

    test("the emulator rejects outputs whose lovelace wraps around to the consumed amount") {
        val alice = Alice.address(Network.Mainnet)
        val bob = Bob.address(Network.Mainnet)
        val genesis = TransactionInput(TransactionHash.fromHex("0" * 64), 0)
        val emulator = Emulator(initialUtxos = Map(genesis -> Output(alice, Value.ada(10))))
        val tx = Alice.signer.sign(
          Transaction(
            body = TransactionBody(
              inputs = TaggedSortedSet(genesis),
              outputs = IndexedSeq(
                Sized(Output(bob, Value.lovelace(Long.MaxValue))),
                Sized(Output(bob, Value.lovelace(Long.MaxValue))),
                Sized(Output(alice, Value.lovelace(9_000_002)))
              ),
              fee = Coin(1_000_000)
            ),
            witnessSet = TransactionWitnessSet.empty
          )
        )
        emulator.submitSync(tx) match
            case Left(error) => assert(error.rule == "ValueNotConserved", error)
            case Right(_)    => fail("a transaction creating ADA was applied")
    }
}
