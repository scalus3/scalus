package scalus.cardano.ledger
package rules

import org.scalacheck.Arbitrary
import org.scalatest.funsuite.AnyFunSuite
import scalus.cardano.address.{Address, Network, StakeAddress, StakePayload}
import scala.collection.immutable.SortedMap

class MissingOrExtraScriptHashesValidatorTest extends AnyFunSuite, ValidatorRulesTestKit {
    test("MissingOrExtraScriptHashesValidator success with no scripts") {
        val transaction = Transaction(
          TransactionBody(
            inputs = TaggedSortedSet.empty,
            outputs = IndexedSeq.empty,
            fee = Coin.zero,
          ),
          TransactionWitnessSet(
            scripts = Seq.empty,
            redeemers = None,
            vkeyWitnesses = Set.empty,
            plutusData = Seq.empty
          )
        )
        val context = Context()
        val state = State()
        val result = MissingOrExtraScriptHashesValidator.validate(context, state, transaction)
        assert(result.isRight)
    }

    test("MissingOrExtraScriptHashesValidator success for inputs") {
        val plutusScript = Arbitrary.arbitrary[Script.PlutusV1].sample.get
        val input = Arbitrary.arbitrary[TransactionInput].sample.get
        val utxo = Map(
          input -> TransactionOutput(
            Address(Network.Testnet, Credential.ScriptHash(plutusScript.scriptHash)),
            Value(Coin(1000000L))
          )
        )

        // Create a redeemer for the spending input
        val redeemer = Redeemer(
          tag = RedeemerTag.Spend,
          index = 0,
          data = scalus.builtin.Data.I(0),
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
            redeemers = Some(Redeemers.from(Seq(redeemer))),
            vkeyWitnesses = Set.empty,
            plutusData = Seq.empty
          )
        )
        val context = Context()
        val state = State(utxos = utxo)
        val result = MissingOrExtraScriptHashesValidator.validate(context, state, transaction)
        assert(result.isRight)
    }

    test("MissingOrExtraScriptHashesValidator failure with missing inputs") {
        val plutusScript = Arbitrary.arbitrary[Script.PlutusV1].sample.get
        val input = Arbitrary.arbitrary[TransactionInput].sample.get
        val utxo = Map(
          input -> TransactionOutput(
            Address(Network.Testnet, Credential.ScriptHash(plutusScript.scriptHash)),
            Value(Coin(1000000L))
          )
        )

        // Create a redeemer for the spending input
        val redeemer = Redeemer(
          tag = RedeemerTag.Spend,
          index = 0,
          data = scalus.builtin.Data.I(0),
          exUnits = ExUnits(100000, 100000)
        )

        val transaction = Transaction(
          TransactionBody(
            inputs = TaggedSortedSet.from(Set(input)),
            outputs = IndexedSeq.empty,
            fee = Coin.zero,
          ),
          TransactionWitnessSet(
            scripts = Seq.empty,
            redeemers = Some(Redeemers.from(Seq(redeemer))),
            vkeyWitnesses = Set.empty,
            plutusData = Seq.empty
          )
        )
        val context = Context()
        val state = State(utxos = utxo)
        val result = MissingOrExtraScriptHashesValidator.validate(context, state, transaction)
        assert(result.isLeft)
    }

    test("MissingOrExtraScriptHashesValidator success for Mint") {
        val plutusScript = Arbitrary.arbitrary[Script.PlutusV1].sample.get

        val transaction = Transaction(
          TransactionBody(
            inputs = TaggedSortedSet.empty,
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
            )
          ),
          TransactionWitnessSet(
            scripts = Seq(plutusScript),
            redeemers = None,
            vkeyWitnesses = Set.empty,
            plutusData = Seq.empty
          )
        )
        val context = Context()
        val state = State()
        val result = MissingOrExtraScriptHashesValidator.validate(context, state, transaction)
        assert(result.isRight)
    }

    test("MissingOrExtraScriptHashesValidator failure with missing Mint") {
        val plutusScript = Arbitrary.arbitrary[Script.PlutusV1].sample.get

        val transaction = Transaction(
          TransactionBody(
            inputs = TaggedSortedSet.empty,
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
            )
          ),
          TransactionWitnessSet(
            scripts = Seq.empty,
            redeemers = None,
            vkeyWitnesses = Set.empty,
            plutusData = Seq.empty
          )
        )
        val context = Context()
        val state = State()
        val result = MissingOrExtraScriptHashesValidator.validate(context, state, transaction)
        assert(result.isLeft)
    }

    test("MissingOrExtraScriptHashesValidator success for Voting") {
        val plutusScript = Arbitrary.arbitrary[Script.PlutusV1].sample.get

        val transaction = Transaction(
          TransactionBody(
            inputs = TaggedSortedSet.empty,
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
            plutusData = Seq.empty
          )
        )
        val context = Context()
        val state = State()
        val result = MissingOrExtraScriptHashesValidator.validate(context, state, transaction)
        assert(result.isRight)
    }

    test("MissingOrExtraScriptHashesValidator failure with missing Voting") {
        val plutusScript = Arbitrary.arbitrary[Script.PlutusV1].sample.get

        val transaction = Transaction(
          TransactionBody(
            inputs = TaggedSortedSet.empty,
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
            scripts = Seq.empty,
            redeemers = None,
            vkeyWitnesses = Set.empty,
            plutusData = Seq.empty
          )
        )
        val context = Context()
        val state = State()
        val result = MissingOrExtraScriptHashesValidator.validate(context, state, transaction)
        assert(result.isLeft)
    }

    test("MissingOrExtraScriptHashesValidator success for Withdrawals") {
        val plutusScript = Arbitrary.arbitrary[Script.PlutusV1].sample.get

        val transaction = Transaction(
          TransactionBody(
            inputs = TaggedSortedSet.empty,
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
            plutusData = Seq.empty
          )
        )
        val context = Context()
        val state = State()
        val result = MissingOrExtraScriptHashesValidator.validate(context, state, transaction)
        assert(result.isRight)
    }

    test("MissingOrExtraScriptHashesValidator failure with missing Withdrawals") {
        val plutusScript = Arbitrary.arbitrary[Script.PlutusV1].sample.get

        val transaction = Transaction(
          TransactionBody(
            inputs = TaggedSortedSet.empty,
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
            scripts = Seq.empty,
            redeemers = None,
            vkeyWitnesses = Set.empty,
            plutusData = Seq.empty
          )
        )
        val context = Context()
        val state = State()
        val result = MissingOrExtraScriptHashesValidator.validate(context, state, transaction)
        assert(result.isLeft)
    }

    test("MissingOrExtraScriptHashesValidator success for Proposal Procedures") {
        val plutusScript = Arbitrary.arbitrary[Script.PlutusV1].sample.get

        val transaction = Transaction(
          TransactionBody(
            inputs = TaggedSortedSet.empty,
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
            plutusData = Seq.empty
          )
        )
        val context = Context()
        val state = State()
        val result = MissingOrExtraScriptHashesValidator.validate(context, state, transaction)
        assert(result.isRight)
    }

    test("MissingOrExtraScriptHashesValidator failure with missing Proposal Procedures") {
        val plutusScript = Arbitrary.arbitrary[Script.PlutusV1].sample.get

        val transaction = Transaction(
          TransactionBody(
            inputs = TaggedSortedSet.empty,
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
            scripts = Seq.empty,
            redeemers = None,
            vkeyWitnesses = Set.empty,
            plutusData = Seq.empty
          )
        )
        val context = Context()
        val state = State()
        val result = MissingOrExtraScriptHashesValidator.validate(context, state, transaction)
        assert(result.isLeft)
    }

    test("MissingOrExtraScriptHashesValidator success for Certificates") {
        val plutusScript = Arbitrary.arbitrary[Script.PlutusV1].sample.get

        val transaction = Transaction(
          TransactionBody(
            inputs = TaggedSortedSet.empty,
            outputs = IndexedSeq.empty,
            fee = Coin.zero,
            certificates = TaggedOrderedStrictSet(
              Certificate.UnregCert(
                Credential.ScriptHash(plutusScript.scriptHash),
                None
              )
            )
          ),
          TransactionWitnessSet(
            scripts = Seq(plutusScript),
            redeemers = None,
            vkeyWitnesses = Set.empty,
            plutusData = Seq.empty
          )
        )
        val context = Context()
        val state = State()
        val result = MissingOrExtraScriptHashesValidator.validate(context, state, transaction)
        assert(result.isRight)
    }

    test("MissingOrExtraScriptHashesValidator failure with missing Certificates") {
        val plutusScript = Arbitrary.arbitrary[Script.PlutusV1].sample.get

        val transaction = Transaction(
          TransactionBody(
            inputs = TaggedSortedSet.empty,
            outputs = IndexedSeq.empty,
            fee = Coin.zero,
            certificates = TaggedOrderedStrictSet(
              Certificate.UnregCert(
                Credential.ScriptHash(plutusScript.scriptHash),
                None
              )
            )
          ),
          TransactionWitnessSet(
            scripts = Seq.empty,
            redeemers = None,
            vkeyWitnesses = Set.empty,
            plutusData = Seq.empty
          )
        )
        val context = Context()
        val state = State()
        val result = MissingOrExtraScriptHashesValidator.validate(context, state, transaction)
        assert(result.isLeft)
    }

    test("MissingOrExtraScriptHashesValidator failure with extra") {
        val plutusScript = Arbitrary.arbitrary[Script.PlutusV1].sample.get

        val transaction = Transaction(
          TransactionBody(
            inputs = TaggedSortedSet.empty,
            outputs = IndexedSeq.empty,
            fee = Coin.zero,
            mint = None
          ),
          TransactionWitnessSet(
            scripts = Seq(plutusScript),
            redeemers = None,
            vkeyWitnesses = Set.empty,
            plutusData = Seq.empty
          )
        )
        val context = Context()
        val state = State()
        val result = MissingOrExtraScriptHashesValidator.validate(context, state, transaction)
        assert(result.isLeft)
    }
}
