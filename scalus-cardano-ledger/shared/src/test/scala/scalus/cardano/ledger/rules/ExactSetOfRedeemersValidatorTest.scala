package scalus.cardano.ledger
package rules

import org.scalacheck.Arbitrary
import org.scalatest.funsuite.AnyFunSuite
import scalus.cardano.address.*
import scalus.uplc.builtin.Data

import TransactionWitnessSet.given
import scala.collection.immutable.SortedMap

class ExactSetOfRedeemersValidatorTest extends AnyFunSuite, ValidatorRulesTestKit {

    test("ExactSetOfRedeemersValidator success with no scripts") {
        val input = Arbitrary.arbitrary[TransactionInput].sample.get
        val utxo = Map(
          input -> Output(
            Arbitrary.arbitrary[ShelleyAddress].sample.get,
            Value(Coin(1000000L))
          )
        )
        val transaction = Transaction(
          body = TransactionBody(
            inputs = TaggedSortedSet.from(Set(input)),
            outputs = IndexedSeq.empty,
            fee = Coin.zero
          ),
          witnessSet = TransactionWitnessSet.empty
        )
        val context = Context()
        val state = State(utxos = utxo)
        val result = ExactSetOfRedeemersValidator.validate(context, state, transaction)
        assert(result.isRight)
    }

    test("ExactSetOfRedeemersValidator success with matching spend redeemer") {
        val plutusScript = Arbitrary.arbitrary[Script.PlutusV1].sample.get
        val input = Arbitrary.arbitrary[TransactionInput].sample.get
        val utxo = Map(
          input -> Output(
            Address(Network.Testnet, Credential.ScriptHash(plutusScript.scriptHash)),
            Value(Coin(1000000L))
          )
        )
        val dummyData = Arbitrary.arbitrary[Data].sample.get
        val dummyExUnits = Arbitrary.arbitrary[ExUnits].sample.get
        val transaction = Transaction(
          TransactionBody(
            inputs = TaggedSortedSet.from(Set(input)),
            outputs = IndexedSeq.empty,
            fee = Coin.zero,
          ),
          TransactionWitnessSet(
            scripts = Seq(plutusScript),
            redeemers = Some(Redeemers(Redeemer(RedeemerTag.Spend, 0, dummyData, dummyExUnits))),
            vkeyWitnesses = Set.empty,
            plutusData = Seq(dummyData)
          )
        )
        val context = Context()
        val state = State(utxos = utxo)
        val result = ExactSetOfRedeemersValidator.validate(context, state, transaction)
        assert(result.isRight)
    }

    test("ExactSetOfRedeemersValidator failure with missing spend redeemer") {
        val plutusScript = Arbitrary.arbitrary[Script.PlutusV1].sample.get
        val input = Arbitrary.arbitrary[TransactionInput].sample.get
        val utxo = Map(
          input -> Output(
            Address(Network.Testnet, Credential.ScriptHash(plutusScript.scriptHash)),
            Value(Coin(1000000L))
          )
        )

        val dummyData = Arbitrary.arbitrary[Data].sample.get
        // Create a redeemer for a different index to test missing redeemer
        val wrongRedeemer = Redeemer(
          tag = RedeemerTag.Spend,
          index = 1, // Wrong index!
          data = dummyData,
          exUnits = ExUnits(100000, 100000)
        )
        val transaction = Transaction(
          TransactionBody(
            inputs = TaggedSortedSet.from(Set(input)),
            outputs = IndexedSeq.empty,
            fee = Coin.zero,
          ),
          TransactionWitnessSet(
            scripts = Seq(plutusScript),
            redeemers = Some(Redeemers.from(Seq(wrongRedeemer))),
            vkeyWitnesses = Set.empty,
            plutusData = Seq(dummyData)
          )
        )
        val context = Context()
        val state = State(utxos = utxo)
        val result = ExactSetOfRedeemersValidator.validate(context, state, transaction)
        assert(result.isLeft)
    }

    test("ExactSetOfRedeemersValidator failure with extra spend redeemer") {
        val plutusScript = Arbitrary.arbitrary[Script.PlutusV1].sample.get
        val input = Arbitrary.arbitrary[TransactionInput].sample.get
        val utxo = Map(
          input -> Output(
            Arbitrary.arbitrary[ShelleyAddress].sample.get,
            Value(Coin(1000000L))
          )
        )
        val dummyData = Arbitrary.arbitrary[Data].sample.get
        val dummyExUnits = Arbitrary.arbitrary[ExUnits].sample.get
        val transaction = Transaction(
          TransactionBody(
            inputs = TaggedSortedSet.from(Set(input)),
            outputs = IndexedSeq.empty,
            fee = Coin.zero,
          ),
          TransactionWitnessSet(
            scripts = Seq(plutusScript),
            redeemers = Some(Redeemers(Redeemer(RedeemerTag.Spend, 0, dummyData, dummyExUnits))),
            vkeyWitnesses = Set.empty,
            plutusData = Seq(dummyData)
          )
        )
        val context = Context()
        val state = State(utxos = utxo)
        val result = ExactSetOfRedeemersValidator.validate(context, state, transaction)
        assert(result.isLeft)
    }

    test("ExactSetOfRedeemersValidator success with matching mint redeemer") {
        val plutusScript = Arbitrary.arbitrary[Script.PlutusV2].sample.get
        val input = Arbitrary.arbitrary[TransactionInput].sample.get
        val utxo = Map(
          input -> Output(
            Arbitrary.arbitrary[ShelleyAddress].sample.get,
            Value(Coin(1000000L))
          )
        )
        val dummyData = Arbitrary.arbitrary[Data].sample.get
        val dummyExUnits = Arbitrary.arbitrary[ExUnits].sample.get
        val transaction = Transaction(
          TransactionBody(
            inputs = TaggedSortedSet.from(Set(input)),
            outputs = IndexedSeq.empty,
            fee = Coin.zero,
            mint = Some(
              Mint(
                MultiAsset(
                  SortedMap(
                    plutusScript.scriptHash -> SortedMap(AssetName.empty -> 1)
                  )
                )
              )
            ),
          ),
          TransactionWitnessSet(
            scripts = Seq(plutusScript),
            redeemers = Some(Redeemers(Redeemer(RedeemerTag.Mint, 0, dummyData, dummyExUnits))),
            vkeyWitnesses = Set.empty,
            plutusData = Seq(dummyData)
          )
        )
        val context = Context()
        val state = State(utxos = utxo)

        val result = ExactSetOfRedeemersValidator.validate(context, state, transaction)
        assert(result.isRight)
    }

    test("ExactSetOfRedeemersValidator failure with missing mint redeemer") {
        val plutusScript = Arbitrary.arbitrary[Script.PlutusV2].sample.get
        val input = Arbitrary.arbitrary[TransactionInput].sample.get
        val utxo = Map(
          input -> Output(
            Arbitrary.arbitrary[ShelleyAddress].sample.get,
            Value(Coin(1000000L))
          )
        )
        val dummyData = Arbitrary.arbitrary[Data].sample.get
        val transaction = Transaction(
          TransactionBody(
            inputs = TaggedSortedSet.from(Set(input)),
            outputs = IndexedSeq.empty,
            fee = Coin.zero,
            mint = Some(
              Mint(
                MultiAsset(
                  SortedMap(
                    plutusScript.scriptHash -> SortedMap(AssetName.empty -> 1)
                  )
                )
              )
            ),
          ),
          TransactionWitnessSet(
            scripts = Seq(plutusScript),
            redeemers = None,
            vkeyWitnesses = Set.empty,
            plutusData = Seq(dummyData)
          )
        )
        val context = Context()
        val state = State(utxos = utxo)

        val result = ExactSetOfRedeemersValidator.validate(context, state, transaction)
        assert(result.isLeft)
    }

    test("ExactSetOfRedeemersValidator failure with extra mint redeemer") {
        val plutusScript = Arbitrary.arbitrary[Script.PlutusV2].sample.get
        val input = Arbitrary.arbitrary[TransactionInput].sample.get
        val utxo = Map(
          input -> Output(
            Arbitrary.arbitrary[ShelleyAddress].sample.get,
            Value(Coin(1000000L))
          )
        )
        val dummyData = Arbitrary.arbitrary[Data].sample.get
        val dummyExUnits = Arbitrary.arbitrary[ExUnits].sample.get
        val transaction = Transaction(
          TransactionBody(
            inputs = TaggedSortedSet.from(Set(input)),
            outputs = IndexedSeq.empty,
            fee = Coin.zero,
            mint = None,
          ),
          TransactionWitnessSet(
            scripts = Seq(plutusScript),
            redeemers = Some(Redeemers(Redeemer(RedeemerTag.Mint, 0, dummyData, dummyExUnits))),
            vkeyWitnesses = Set.empty,
            plutusData = Seq(dummyData)
          )
        )
        val context = Context()
        val state = State(utxos = utxo)

        val result = ExactSetOfRedeemersValidator.validate(context, state, transaction)
        assert(result.isLeft)
    }

    test("ExactSetOfRedeemersValidator success with matching Cert redeemer") {
        val plutusScript = Arbitrary.arbitrary[Script.PlutusV1].sample.get
        val input = Arbitrary.arbitrary[TransactionInput].sample.get
        val utxo = Map(
          input -> Output(
            Arbitrary.arbitrary[ShelleyAddress].sample.get,
            Value(Coin(1000000L))
          )
        )
        val dummyData = Arbitrary.arbitrary[Data].sample.get
        val dummyExUnits = Arbitrary.arbitrary[ExUnits].sample.get
        val transaction = Transaction(
          TransactionBody(
            inputs = TaggedSortedSet.from(Set(input)),
            outputs = IndexedSeq.empty,
            fee = Coin.zero,
            certificates = TaggedOrderedStrictSet(
              Certificate.UnregCert(Credential.ScriptHash(plutusScript.scriptHash), None)
            )
          ),
          TransactionWitnessSet(
            scripts = Seq(plutusScript),
            redeemers = Some(Redeemers(Redeemer(RedeemerTag.Cert, 0, dummyData, dummyExUnits))),
            vkeyWitnesses = Set.empty,
            plutusData = Seq(dummyData)
          )
        )
        val context = Context()
        val state = State(utxos = utxo)
        val result = ExactSetOfRedeemersValidator.validate(context, state, transaction)
        assert(result.isRight)
    }

    test("ExactSetOfRedeemersValidator failure with missing Cert redeemer") {
        val plutusScript = Arbitrary.arbitrary[Script.PlutusV1].sample.get
        val input = Arbitrary.arbitrary[TransactionInput].sample.get
        val utxo = Map(
          input -> Output(
            Arbitrary.arbitrary[ShelleyAddress].sample.get,
            Value(Coin(1000000L))
          )
        )
        val dummyData = Arbitrary.arbitrary[Data].sample.get
        val transaction = Transaction(
          TransactionBody(
            inputs = TaggedSortedSet.from(Set(input)),
            outputs = IndexedSeq.empty,
            fee = Coin.zero,
            certificates = TaggedOrderedStrictSet(
              Certificate.UnregCert(Credential.ScriptHash(plutusScript.scriptHash), None)
            )
          ),
          TransactionWitnessSet(
            scripts = Seq(plutusScript),
            redeemers = None,
            vkeyWitnesses = Set.empty,
            plutusData = Seq(dummyData)
          )
        )
        val context = Context()
        val state = State(utxos = utxo)
        val result = ExactSetOfRedeemersValidator.validate(context, state, transaction)
        assert(result.isLeft)
    }

    test("ExactSetOfRedeemersValidator failure with extra Cert redeemer") {
        val plutusScript = Arbitrary.arbitrary[Script.PlutusV1].sample.get
        val input = Arbitrary.arbitrary[TransactionInput].sample.get
        val utxo = Map(
          input -> Output(
            Arbitrary.arbitrary[ShelleyAddress].sample.get,
            Value(Coin(1000000L))
          )
        )
        val dummyData = Arbitrary.arbitrary[Data].sample.get
        val dummyExUnits = Arbitrary.arbitrary[ExUnits].sample.get
        val transaction = Transaction(
          TransactionBody(
            inputs = TaggedSortedSet.from(Set(input)),
            outputs = IndexedSeq.empty,
            fee = Coin.zero,
            certificates = TaggedOrderedStrictSet.empty
          ),
          TransactionWitnessSet(
            scripts = Seq(plutusScript),
            redeemers = Some(Redeemers(Redeemer(RedeemerTag.Cert, 0, dummyData, dummyExUnits))),
            vkeyWitnesses = Set.empty,
            plutusData = Seq(dummyData)
          )
        )
        val context = Context()
        val state = State(utxos = utxo)
        val result = ExactSetOfRedeemersValidator.validate(context, state, transaction)
        assert(result.isLeft)
    }

    test("ExactSetOfRedeemersValidator success with matching Reward redeemer") {
        val plutusScript = Arbitrary.arbitrary[Script.PlutusV1].sample.get
        val input = Arbitrary.arbitrary[TransactionInput].sample.get
        val utxo = Map(
          input -> Output(
            Arbitrary.arbitrary[ShelleyAddress].sample.get,
            Value(Coin(1000000L))
          )
        )
        val dummyData = Arbitrary.arbitrary[Data].sample.get
        val dummyExUnits = Arbitrary.arbitrary[ExUnits].sample.get
        val transaction = Transaction(
          TransactionBody(
            inputs = TaggedSortedSet.from(Set(input)),
            outputs = IndexedSeq.empty,
            fee = Coin.zero,
            withdrawals = Some(
              Withdrawals(
                SortedMap(
                  RewardAccount(
                    StakeAddress(
                      Network.Testnet,
                      StakePayload.Script(plutusScript.scriptHash)
                    )
                  ) -> Coin.zero
                )
              )
            )
          ),
          TransactionWitnessSet(
            scripts = Seq(plutusScript),
            redeemers = Some(Redeemers(Redeemer(RedeemerTag.Reward, 0, dummyData, dummyExUnits))),
            vkeyWitnesses = Set.empty,
            plutusData = Seq(dummyData)
          )
        )
        val context = Context()
        val state = State(utxos = utxo)
        val result = ExactSetOfRedeemersValidator.validate(context, state, transaction)
        assert(result.isRight)
    }

    test("ExactSetOfRedeemersValidator failure with missing Reward redeemer") {
        val plutusScript = Arbitrary.arbitrary[Script.PlutusV1].sample.get
        val input = Arbitrary.arbitrary[TransactionInput].sample.get
        val utxo = Map(
          input -> Output(
            Arbitrary.arbitrary[ShelleyAddress].sample.get,
            Value(Coin(1000000L))
          )
        )
        val dummyData = Arbitrary.arbitrary[Data].sample.get
        val transaction = Transaction(
          TransactionBody(
            inputs = TaggedSortedSet.from(Set(input)),
            outputs = IndexedSeq.empty,
            fee = Coin.zero,
            withdrawals = Some(
              Withdrawals(
                SortedMap(
                  RewardAccount(
                    StakeAddress(
                      Network.Testnet,
                      StakePayload.Script(plutusScript.scriptHash)
                    )
                  ) -> Coin.zero
                )
              )
            )
          ),
          TransactionWitnessSet(
            scripts = Seq(plutusScript),
            redeemers = None,
            vkeyWitnesses = Set.empty,
            plutusData = Seq(dummyData)
          )
        )
        val context = Context()
        val state = State(utxos = utxo)
        val result = ExactSetOfRedeemersValidator.validate(context, state, transaction)
        assert(result.isLeft)
    }

    test("ExactSetOfRedeemersValidator failure with extra Reward redeemer") {
        val plutusScript = Arbitrary.arbitrary[Script.PlutusV1].sample.get
        val input = Arbitrary.arbitrary[TransactionInput].sample.get
        val utxo = Map(
          input -> Output(
            Arbitrary.arbitrary[ShelleyAddress].sample.get,
            Value(Coin(1000000L))
          )
        )
        val dummyData = Arbitrary.arbitrary[Data].sample.get
        val dummyExUnits = Arbitrary.arbitrary[ExUnits].sample.get
        val transaction = Transaction(
          TransactionBody(
            inputs = TaggedSortedSet.from(Set(input)),
            outputs = IndexedSeq.empty,
            fee = Coin.zero,
            withdrawals = None
          ),
          TransactionWitnessSet(
            scripts = Seq(plutusScript),
            redeemers = Some(Redeemers(Redeemer(RedeemerTag.Reward, 0, dummyData, dummyExUnits))),
            vkeyWitnesses = Set.empty,
            plutusData = Seq(dummyData)
          )
        )
        val context = Context()
        val state = State(utxos = utxo)
        val result = ExactSetOfRedeemersValidator.validate(context, state, transaction)
        assert(result.isLeft)
    }

    test("ExactSetOfRedeemersValidator success with matching Voting redeemer") {
        val plutusScript = Arbitrary.arbitrary[Script.PlutusV1].sample.get
        val input = Arbitrary.arbitrary[TransactionInput].sample.get
        val utxo = Map(
          input -> Output(
            Arbitrary.arbitrary[ShelleyAddress].sample.get,
            Value(Coin(1000000L))
          )
        )
        val dummyData = Arbitrary.arbitrary[Data].sample.get
        val dummyExUnits = Arbitrary.arbitrary[ExUnits].sample.get
        val transaction = Transaction(
          TransactionBody(
            inputs = TaggedSortedSet.from(Set(input)),
            outputs = IndexedSeq.empty,
            fee = Coin.zero,
            votingProcedures = Some(
              VotingProcedures(
                SortedMap(
                  Voter.DRepScript(plutusScript.scriptHash) -> SortedMap.empty
                )
              )
            )
          ),
          TransactionWitnessSet(
            scripts = Seq(plutusScript),
            redeemers = Some(Redeemers(Redeemer(RedeemerTag.Voting, 0, dummyData, dummyExUnits))),
            vkeyWitnesses = Set.empty,
            plutusData = Seq(dummyData)
          )
        )
        val context = Context()
        val state = State(utxos = utxo)
        val result = ExactSetOfRedeemersValidator.validate(context, state, transaction)
        assert(result.isRight)
    }

    test("ExactSetOfRedeemersValidator failure with missing Voting redeemer") {
        val plutusScript = Arbitrary.arbitrary[Script.PlutusV1].sample.get
        val input = Arbitrary.arbitrary[TransactionInput].sample.get
        val utxo = Map(
          input -> Output(
            Arbitrary.arbitrary[ShelleyAddress].sample.get,
            Value(Coin(1000000L))
          )
        )
        val dummyData = Arbitrary.arbitrary[Data].sample.get
        val transaction = Transaction(
          TransactionBody(
            inputs = TaggedSortedSet.from(Set(input)),
            outputs = IndexedSeq.empty,
            fee = Coin.zero,
            votingProcedures = Some(
              VotingProcedures(
                SortedMap(
                  Voter.DRepScript(plutusScript.scriptHash) -> SortedMap.empty
                )
              )
            )
          ),
          TransactionWitnessSet(
            scripts = Seq(plutusScript),
            redeemers = None,
            vkeyWitnesses = Set.empty,
            plutusData = Seq(dummyData)
          )
        )
        val context = Context()
        val state = State(utxos = utxo)
        val result = ExactSetOfRedeemersValidator.validate(context, state, transaction)
        assert(result.isLeft)
    }

    test("ExactSetOfRedeemersValidator failure with extra Voting redeemer") {
        val plutusScript = Arbitrary.arbitrary[Script.PlutusV1].sample.get
        val input = Arbitrary.arbitrary[TransactionInput].sample.get
        val utxo = Map(
          input -> Output(
            Arbitrary.arbitrary[ShelleyAddress].sample.get,
            Value(Coin(1000000L))
          )
        )
        val dummyData = Arbitrary.arbitrary[Data].sample.get
        val dummyExUnits = Arbitrary.arbitrary[ExUnits].sample.get
        val transaction = Transaction(
          TransactionBody(
            inputs = TaggedSortedSet.from(Set(input)),
            outputs = IndexedSeq.empty,
            fee = Coin.zero,
            votingProcedures = None
          ),
          TransactionWitnessSet(
            scripts = Seq(plutusScript),
            redeemers = Some(Redeemers(Redeemer(RedeemerTag.Voting, 0, dummyData, dummyExUnits))),
            vkeyWitnesses = Set.empty,
            plutusData = Seq(dummyData)
          )
        )
        val context = Context()
        val state = State(utxos = utxo)
        val result = ExactSetOfRedeemersValidator.validate(context, state, transaction)
        assert(result.isLeft)
    }

    test("ExactSetOfRedeemersValidator success with matching Proposing redeemer") {
        val plutusScript = Arbitrary.arbitrary[Script.PlutusV1].sample.get
        val input = Arbitrary.arbitrary[TransactionInput].sample.get
        val utxo = Map(
          input -> Output(
            Arbitrary.arbitrary[ShelleyAddress].sample.get,
            Value(Coin(1000000L))
          )
        )
        val dummyData = Arbitrary.arbitrary[Data].sample.get
        val dummyExUnits = Arbitrary.arbitrary[ExUnits].sample.get
        val transaction = Transaction(
          TransactionBody(
            inputs = TaggedSortedSet.from(Set(input)),
            outputs = IndexedSeq.empty,
            fee = Coin.zero,
            proposalProcedures = TaggedOrderedSet(
              ProposalProcedure(
                deposit = Arbitrary.arbitrary[Coin].sample.get,
                rewardAccount = Arbitrary.arbitrary[RewardAccount].sample.get,
                govAction = GovAction.TreasuryWithdrawals(Map.empty, Some(plutusScript.scriptHash)),
                anchor = Arbitrary.arbitrary[Anchor].sample.get,
              )
            )
          ),
          TransactionWitnessSet(
            scripts = Seq(plutusScript),
            redeemers =
                Some(Redeemers(Redeemer(RedeemerTag.Proposing, 0, dummyData, dummyExUnits))),
            vkeyWitnesses = Set.empty,
            plutusData = Seq(dummyData)
          )
        )
        val context = Context()
        val state = State(utxos = utxo)
        val result = ExactSetOfRedeemersValidator.validate(context, state, transaction)
        assert(result.isRight)
    }

    test("ExactSetOfRedeemersValidator failure with missing Proposing redeemer") {
        val plutusScript = Arbitrary.arbitrary[Script.PlutusV1].sample.get
        val input = Arbitrary.arbitrary[TransactionInput].sample.get
        val utxo = Map(
          input -> Output(
            Arbitrary.arbitrary[ShelleyAddress].sample.get,
            Value(Coin(1000000L))
          )
        )
        val dummyData = Arbitrary.arbitrary[Data].sample.get
        val transaction = Transaction(
          TransactionBody(
            inputs = TaggedSortedSet.from(Set(input)),
            outputs = IndexedSeq.empty,
            fee = Coin.zero,
            proposalProcedures = TaggedOrderedSet(
              ProposalProcedure(
                deposit = Arbitrary.arbitrary[Coin].sample.get,
                rewardAccount = Arbitrary.arbitrary[RewardAccount].sample.get,
                govAction = GovAction.TreasuryWithdrawals(Map.empty, Some(plutusScript.scriptHash)),
                anchor = Arbitrary.arbitrary[Anchor].sample.get,
              )
            )
          ),
          TransactionWitnessSet(
            scripts = Seq(plutusScript),
            redeemers = None,
            vkeyWitnesses = Set.empty,
            plutusData = Seq(dummyData)
          )
        )
        val context = Context()
        val state = State(utxos = utxo)
        val result = ExactSetOfRedeemersValidator.validate(context, state, transaction)
        assert(result.isLeft)
    }

    test("ExactSetOfRedeemersValidator failure with extra Proposing redeemer") {
        val plutusScript = Arbitrary.arbitrary[Script.PlutusV1].sample.get
        val input = Arbitrary.arbitrary[TransactionInput].sample.get
        val utxo = Map(
          input -> Output(
            Arbitrary.arbitrary[ShelleyAddress].sample.get,
            Value(Coin(1000000L))
          )
        )
        val dummyData = Arbitrary.arbitrary[Data].sample.get
        val dummyExUnits = Arbitrary.arbitrary[ExUnits].sample.get
        val transaction = Transaction(
          TransactionBody(
            inputs = TaggedSortedSet.from(Set(input)),
            outputs = IndexedSeq.empty,
            fee = Coin.zero,
            proposalProcedures = TaggedOrderedSet.empty
          ),
          TransactionWitnessSet(
            scripts = Seq(plutusScript),
            redeemers =
                Some(Redeemers(Redeemer(RedeemerTag.Proposing, 0, dummyData, dummyExUnits))),
            vkeyWitnesses = Set.empty,
            plutusData = Seq(dummyData)
          )
        )
        val context = Context()
        val state = State(utxos = utxo)
        val result = ExactSetOfRedeemersValidator.validate(context, state, transaction)
        assert(result.isLeft)
    }

    test("ExactSetOfRedeemersValidator success with native scripts (no redeemers needed)") {
        val (privateKey, publicKey) = generateKeyPair()
        val nativeScript =
            Timelock.Signature(Hash(scalus.uplc.builtin.platform.blake2b_224(publicKey)))
        val input = Arbitrary.arbitrary[TransactionInput].sample.get
        val utxo = Map(
          input -> Output(
            Address(Network.Testnet, Credential.ScriptHash(nativeScript.scriptHash)),
            Value(Coin(1000000L))
          )
        )
        val transaction = Transaction(
          TransactionBody(
            inputs = TaggedSortedSet.from(Set(input)),
            outputs = IndexedSeq.empty,
            fee = Coin.zero,
            mint = Some(
              Mint(
                MultiAsset(
                  SortedMap(
                    nativeScript.scriptHash -> SortedMap(AssetName.empty -> 1)
                  )
                )
              )
            ),
          ),
          TransactionWitnessSet(
            nativeScripts = TaggedSortedMap(Script.Native(nativeScript)),
            redeemers = None
          ),
        )
        val context = Context()
        val state = State(utxos = utxo)

        val result = ExactSetOfRedeemersValidator.validate(context, state, transaction)
        assert(result.isRight)
    }

    test("ExactSetOfRedeemersValidator success with mixed native and Plutus scripts") {
        val (privateKey, publicKey) = generateKeyPair()
        val nativeScript =
            Timelock.Signature(Hash(scalus.uplc.builtin.platform.blake2b_224(publicKey)))
        val plutusScript = Arbitrary.arbitrary[Script.PlutusV2].sample.get

        val input1 = Input(
          TransactionHash.fromHex(
            "a000000000000000000000000000000000000000000000000000000000000000"
          ),
          0
        )
        val input2 = Input(
          TransactionHash.fromHex(
            "b000000000000000000000000000000000000000000000000000000000000000"
          ),
          0
        )

        val utxo = Map(
          input1 -> Output(
            Address(Network.Testnet, Credential.ScriptHash(nativeScript.scriptHash)),
            Value(Coin(1000000L))
          ),
          input2 -> Output(
            Address(Network.Testnet, Credential.ScriptHash(plutusScript.scriptHash)),
            Value(Coin(1000000L))
          )
        )

        val dummyData = Arbitrary.arbitrary[Data].sample.get
        val dummyExUnits = Arbitrary.arbitrary[ExUnits].sample.get

        // Create inputs set and determine the actual index of the Plutus script input
        val inputsSet = Set(input1, input2)
        val sortedInputs = inputsSet.toSeq.sorted
        val plutusInputIndex = sortedInputs.zipWithIndex
            .find { case (input, _) =>
                utxo.get(input).exists(_.address.scriptHashOption.contains(plutusScript.scriptHash))
            }
            .get
            ._2

        val transaction = Transaction(
          TransactionBody(
            inputs = TaggedSortedSet.from(inputsSet),
            outputs = IndexedSeq.empty,
            fee = Coin.zero,
          ),
          TransactionWitnessSet(
            scripts = Seq(Script.Native(nativeScript), plutusScript),
            redeemers = Some(
              Redeemers(
                // calculate the index manually for a tx where n of inputs > 1
                Redeemer(RedeemerTag.Spend, plutusInputIndex, dummyData, dummyExUnits)
              )
            ),
            vkeyWitnesses = Set.empty,
            plutusData = Seq(dummyData)
          )
        )
        val context = Context()
        val state = State(utxos = utxo)

        val result = ExactSetOfRedeemersValidator.validate(context, state, transaction)
        assert(result.isRight)
    }
}
