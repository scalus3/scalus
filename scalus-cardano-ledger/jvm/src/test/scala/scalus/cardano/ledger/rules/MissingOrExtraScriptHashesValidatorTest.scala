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

    test("MissingOrExtraScriptHashesValidator success for inputs in witnesses") {
        val plutusScript = Arbitrary.arbitrary[Script.PlutusV1].sample.get
        val input = Arbitrary.arbitrary[TransactionInput].sample.get
        val utxo = Map(
          input -> TransactionOutput(
            Address(Network.Testnet, Credential.ScriptHash(plutusScript.scriptHash)),
            Value(Coin(1000000L))
          )
        )

        val transaction = Transaction(
          TransactionBody(
            inputs = TaggedSortedSet.from(Set(input)),
            outputs = IndexedSeq.empty,
            fee = Coin.zero,
          ),
          TransactionWitnessSet(
            scripts = Seq(plutusScript),
            redeemers = None,
            vkeyWitnesses = Set.empty,
            plutusData = Seq.empty
          )
        )
        val context = Context()
        val state = State(utxos = utxo)
        val result = MissingOrExtraScriptHashesValidator.validate(context, state, transaction)
        assert(result.isRight)
    }

    test("MissingOrExtraScriptHashesValidator failure with missing inputs in witnesses") {
        val plutusScript = Arbitrary.arbitrary[Script.PlutusV1].sample.get
        val input = Arbitrary.arbitrary[TransactionInput].sample.get
        val utxo = Map(
          input -> TransactionOutput(
            Address(Network.Testnet, Credential.ScriptHash(plutusScript.scriptHash)),
            Value(Coin(1000000L))
          )
        )

        val transaction = Transaction(
          TransactionBody(
            inputs = TaggedSortedSet.from(Set(input)),
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
        val state = State(utxos = utxo)
        val result = MissingOrExtraScriptHashesValidator.validate(context, state, transaction)
        assert(result.isLeft)
    }

    test("MissingOrExtraScriptHashesValidator success for Mint in witnesses") {
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

    test("MissingOrExtraScriptHashesValidator failure with missing Mint in witnesses") {
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

    test("MissingOrExtraScriptHashesValidator success for Voting in witnesses") {
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

    test("MissingOrExtraScriptHashesValidator failure with missing Voting in witnesses") {
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

    test("MissingOrExtraScriptHashesValidator success for Withdrawals in witnesses") {
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

    test("MissingOrExtraScriptHashesValidator failure with missing Withdrawals in witnesses") {
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

    test("MissingOrExtraScriptHashesValidator success for Proposal Procedures in witnesses") {
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

    test(
      "MissingOrExtraScriptHashesValidator failure with missing Proposal Procedures in witnesses"
    ) {
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

    test("MissingOrExtraScriptHashesValidator success for Certificates in witnesses") {
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

    test("MissingOrExtraScriptHashesValidator failure with missing Certificates in witnesses") {
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

    test("MissingOrExtraScriptHashesValidator failure with extra in witnesses") {
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

    test("MissingOrExtraScriptHashesValidator success for Inputs with reference script in inputs") {
        val plutusScript = Arbitrary.arbitrary[Script.PlutusV1].sample.get
        val input = Arbitrary.arbitrary[TransactionInput].sample.get
        val utxo = Map(
          input -> TransactionOutput.Babbage(
            Address(Network.Testnet, Credential.ScriptHash(plutusScript.scriptHash)),
            Value(Coin(1000000L)),
            datumOption = None,
            scriptRef = Some(ScriptRef(plutusScript))
          )
        )

        val transaction = Transaction(
          TransactionBody(
            inputs = TaggedSortedSet.from(Set(input)),
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
        val state = State(utxos = utxo)
        val result = MissingOrExtraScriptHashesValidator.validate(context, state, transaction)
        assert(result.isRight)
    }

    test(
      "MissingOrExtraScriptHashesValidator success for Inputs with reference script in referenceInputs"
    ) {
        val plutusScript = Arbitrary.arbitrary[Script.PlutusV1].sample.get
        val input = Arbitrary.arbitrary[TransactionInput].sample.get
        val refInput = Arbitrary.arbitrary[TransactionInput].sample.get
        val utxo = Map(
          input -> TransactionOutput.Babbage(
            Address(Network.Testnet, Credential.ScriptHash(plutusScript.scriptHash)),
            Value(Coin(1000000L)),
            datumOption = None,
            scriptRef = None
          ),
          refInput -> TransactionOutput.Babbage(
            Arbitrary.arbitrary[Address].sample.get,
            Value(Coin(1000000L)),
            datumOption = None,
            scriptRef = Some(ScriptRef(plutusScript))
          )
        )

        val transaction = Transaction(
          TransactionBody(
            inputs = TaggedSortedSet.from(Set(input)),
            outputs = IndexedSeq.empty,
            fee = Coin.zero,
            referenceInputs = TaggedSortedSet.from(Set(refInput))
          ),
          TransactionWitnessSet(
            scripts = Seq.empty,
            redeemers = None,
            vkeyWitnesses = Set.empty,
            plutusData = Seq.empty
          )
        )
        val context = Context()
        val state = State(utxos = utxo)
        val result = MissingOrExtraScriptHashesValidator.validate(context, state, transaction)
        assert(result.isRight)
    }

    test("MissingOrExtraScriptHashesValidator success for Mint with reference script in inputs") {
        val plutusScript = Arbitrary.arbitrary[Script.PlutusV1].sample.get
        val input = Arbitrary.arbitrary[TransactionInput].sample.get
        val utxo = Map(
          input -> TransactionOutput.Babbage(
            Address(
              Network.Testnet,
              Credential.KeyHash(Arbitrary.arbitrary[AddrKeyHash].sample.get)
            ),
            Value(Coin(1000000L)),
            datumOption = None,
            scriptRef = Some(ScriptRef(plutusScript))
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
        val state = State(utxos = utxo)
        val result = MissingOrExtraScriptHashesValidator.validate(context, state, transaction)
        assert(result.isRight)
    }

    test(
      "MissingOrExtraScriptHashesValidator success for Mint with reference script in referenceInputs"
    ) {
        val plutusScript = Arbitrary.arbitrary[Script.PlutusV1].sample.get
        val refInput = Arbitrary.arbitrary[TransactionInput].sample.get
        val utxo = Map(
          refInput -> TransactionOutput.Babbage(
            Address(
              Network.Testnet,
              Credential.KeyHash(Arbitrary.arbitrary[AddrKeyHash].sample.get)
            ),
            Value(Coin(1000000L)),
            datumOption = None,
            scriptRef = Some(ScriptRef(plutusScript))
          )
        )

        val transaction = Transaction(
          TransactionBody(
            inputs = TaggedSortedSet.empty,
            outputs = IndexedSeq.empty,
            fee = Coin.zero,
            referenceInputs = TaggedSortedSet.from(Set(refInput)),
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
        val state = State(utxos = utxo)
        val result = MissingOrExtraScriptHashesValidator.validate(context, state, transaction)
        assert(result.isRight)
    }

    test("MissingOrExtraScriptHashesValidator success for Voting with reference script in inputs") {
        val plutusScript = Arbitrary.arbitrary[Script.PlutusV1].sample.get
        val input = Arbitrary.arbitrary[TransactionInput].sample.get
        val utxo = Map(
          input -> TransactionOutput.Babbage(
            Address(
              Network.Testnet,
              Credential.KeyHash(Arbitrary.arbitrary[AddrKeyHash].sample.get)
            ),
            Value(Coin(1000000L)),
            datumOption = None,
            scriptRef = Some(ScriptRef(plutusScript))
          )
        )

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
            scripts = Seq.empty,
            redeemers = None,
            vkeyWitnesses = Set.empty,
            plutusData = Seq.empty
          )
        )
        val context = Context()
        val state = State(utxos = utxo)
        val result = MissingOrExtraScriptHashesValidator.validate(context, state, transaction)
        assert(result.isRight)
    }

    test(
      "MissingOrExtraScriptHashesValidator success for Voting with reference script in referenceInputs"
    ) {
        val plutusScript = Arbitrary.arbitrary[Script.PlutusV1].sample.get
        val refInput = Arbitrary.arbitrary[TransactionInput].sample.get
        val utxo = Map(
          refInput -> TransactionOutput.Babbage(
            Address(
              Network.Testnet,
              Credential.KeyHash(Arbitrary.arbitrary[AddrKeyHash].sample.get)
            ),
            Value(Coin(1000000L)),
            datumOption = None,
            scriptRef = Some(ScriptRef(plutusScript))
          )
        )

        val transaction = Transaction(
          TransactionBody(
            inputs = TaggedSortedSet.empty,
            outputs = IndexedSeq.empty,
            fee = Coin.zero,
            referenceInputs = TaggedSortedSet.from(Set(refInput)),
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
        val state = State(utxos = utxo)
        val result = MissingOrExtraScriptHashesValidator.validate(context, state, transaction)
        assert(result.isRight)
    }

    test(
      "MissingOrExtraScriptHashesValidator success for Withdrawals with reference script in inputs"
    ) {
        val plutusScript = Arbitrary.arbitrary[Script.PlutusV1].sample.get
        val input = Arbitrary.arbitrary[TransactionInput].sample.get
        val utxo = Map(
          input -> TransactionOutput.Babbage(
            Address(
              Network.Testnet,
              Credential.KeyHash(Arbitrary.arbitrary[AddrKeyHash].sample.get)
            ),
            Value(Coin(1000000L)),
            datumOption = None,
            scriptRef = Some(ScriptRef(plutusScript))
          )
        )

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
            scripts = Seq.empty,
            redeemers = None,
            vkeyWitnesses = Set.empty,
            plutusData = Seq.empty
          )
        )
        val context = Context()
        val state = State(utxos = utxo)
        val result = MissingOrExtraScriptHashesValidator.validate(context, state, transaction)
        assert(result.isRight)
    }

    test(
      "MissingOrExtraScriptHashesValidator success for Withdrawals with reference script in referenceInputs"
    ) {
        val plutusScript = Arbitrary.arbitrary[Script.PlutusV1].sample.get
        val refInput = Arbitrary.arbitrary[TransactionInput].sample.get
        val utxo = Map(
          refInput -> TransactionOutput.Babbage(
            Address(
              Network.Testnet,
              Credential.KeyHash(Arbitrary.arbitrary[AddrKeyHash].sample.get)
            ),
            Value(Coin(1000000L)),
            datumOption = None,
            scriptRef = Some(ScriptRef(plutusScript))
          )
        )

        val transaction = Transaction(
          TransactionBody(
            inputs = TaggedSortedSet.empty,
            outputs = IndexedSeq.empty,
            fee = Coin.zero,
            referenceInputs = TaggedSortedSet.from(Set(refInput)),
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
        val state = State(utxos = utxo)
        val result = MissingOrExtraScriptHashesValidator.validate(context, state, transaction)
        assert(result.isRight)
    }

    test(
      "MissingOrExtraScriptHashesValidator success for Proposal Procedures with reference script in inputs"
    ) {
        val plutusScript = Arbitrary.arbitrary[Script.PlutusV1].sample.get
        val input = Arbitrary.arbitrary[TransactionInput].sample.get
        val utxo = Map(
          input -> TransactionOutput.Babbage(
            Address(
              Network.Testnet,
              Credential.KeyHash(Arbitrary.arbitrary[AddrKeyHash].sample.get)
            ),
            Value(Coin(1000000L)),
            datumOption = None,
            scriptRef = Some(ScriptRef(plutusScript))
          )
        )

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
            scripts = Seq.empty,
            redeemers = None,
            vkeyWitnesses = Set.empty,
            plutusData = Seq.empty
          )
        )
        val context = Context()
        val state = State(utxos = utxo)
        val result = MissingOrExtraScriptHashesValidator.validate(context, state, transaction)
        assert(result.isRight)
    }

    test(
      "MissingOrExtraScriptHashesValidator success for Proposal Procedures with reference script in referenceInputs"
    ) {
        val plutusScript = Arbitrary.arbitrary[Script.PlutusV1].sample.get
        val refInput = Arbitrary.arbitrary[TransactionInput].sample.get
        val utxo = Map(
          refInput -> TransactionOutput.Babbage(
            Address(
              Network.Testnet,
              Credential.KeyHash(Arbitrary.arbitrary[AddrKeyHash].sample.get)
            ),
            Value(Coin(1000000L)),
            datumOption = None,
            scriptRef = Some(ScriptRef(plutusScript))
          )
        )

        val transaction = Transaction(
          TransactionBody(
            inputs = TaggedSortedSet.empty,
            outputs = IndexedSeq.empty,
            fee = Coin.zero,
            referenceInputs = TaggedSortedSet.from(Set(refInput)),
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
        val state = State(utxos = utxo)
        val result = MissingOrExtraScriptHashesValidator.validate(context, state, transaction)
        assert(result.isRight)
    }

    test(
      "MissingOrExtraScriptHashesValidator success for Certificates with reference script in inputs"
    ) {
        val plutusScript = Arbitrary.arbitrary[Script.PlutusV1].sample.get
        val input = Arbitrary.arbitrary[TransactionInput].sample.get
        val utxo = Map(
          input -> TransactionOutput.Babbage(
            Address(
              Network.Testnet,
              Credential.KeyHash(Arbitrary.arbitrary[AddrKeyHash].sample.get)
            ),
            Value(Coin(1000000L)),
            datumOption = None,
            scriptRef = Some(ScriptRef(plutusScript))
          )
        )

        val transaction = Transaction(
          TransactionBody(
            inputs = TaggedSortedSet.from(Set(input)),
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
        val state = State(utxos = utxo)
        val result = MissingOrExtraScriptHashesValidator.validate(context, state, transaction)
        assert(result.isRight)
    }

    test(
      "MissingOrExtraScriptHashesValidator success for Certificates with reference script in referenceInputs"
    ) {
        val plutusScript = Arbitrary.arbitrary[Script.PlutusV1].sample.get
        val refInput = Arbitrary.arbitrary[TransactionInput].sample.get
        val utxo = Map(
          refInput -> TransactionOutput.Babbage(
            Address(
              Network.Testnet,
              Credential.KeyHash(Arbitrary.arbitrary[AddrKeyHash].sample.get)
            ),
            Value(Coin(1000000L)),
            datumOption = None,
            scriptRef = Some(ScriptRef(plutusScript))
          )
        )

        val transaction = Transaction(
          TransactionBody(
            inputs = TaggedSortedSet.empty,
            outputs = IndexedSeq.empty,
            fee = Coin.zero,
            referenceInputs = TaggedSortedSet.from(Set(refInput)),
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
        val state = State(utxos = utxo)
        val result = MissingOrExtraScriptHashesValidator.validate(context, state, transaction)
        assert(result.isRight)
    }

    test(
      "MissingOrExtraScriptHashesValidator failure for Inputs with missing reference script in inputs"
    ) {
        val plutusScript = Arbitrary.arbitrary[Script.PlutusV1].sample.get
        val input = Arbitrary.arbitrary[TransactionInput].sample.get
        val utxo = Map(
          input -> TransactionOutput.Babbage(
            Address(Network.Testnet, Credential.ScriptHash(plutusScript.scriptHash)),
            Value(Coin(1000000L)),
            datumOption = None,
            scriptRef = None
          )
        )

        val transaction = Transaction(
          TransactionBody(
            inputs = TaggedSortedSet.from(Set(input)),
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
        val state = State(utxos = utxo)
        val result = MissingOrExtraScriptHashesValidator.validate(context, state, transaction)
        assert(result.isLeft)
    }

    test(
      "MissingOrExtraScriptHashesValidator failure for Inputs with missing reference script in referenceInputs"
    ) {
        val plutusScript = Arbitrary.arbitrary[Script.PlutusV1].sample.get
        val input = Arbitrary.arbitrary[TransactionInput].sample.get
        val refInput = Arbitrary.arbitrary[TransactionInput].sample.get
        val utxo = Map(
          input -> TransactionOutput.Babbage(
            Address(Network.Testnet, Credential.ScriptHash(plutusScript.scriptHash)),
            Value(Coin(1000000L)),
            datumOption = None,
            scriptRef = None
          ),
          refInput -> TransactionOutput.Babbage(
            Arbitrary.arbitrary[Address].sample.get,
            Value(Coin(1000000L)),
            datumOption = None,
            scriptRef = None
          )
        )

        val transaction = Transaction(
          TransactionBody(
            inputs = TaggedSortedSet.from(Set(input)),
            outputs = IndexedSeq.empty,
            fee = Coin.zero,
            referenceInputs = TaggedSortedSet.from(Set(refInput))
          ),
          TransactionWitnessSet(
            scripts = Seq.empty,
            redeemers = None,
            vkeyWitnesses = Set.empty,
            plutusData = Seq.empty
          )
        )
        val context = Context()
        val state = State(utxos = utxo)
        val result = MissingOrExtraScriptHashesValidator.validate(context, state, transaction)
        assert(result.isLeft)
    }

    test(
      "MissingOrExtraScriptHashesValidator failure for Mint with missing reference script in inputs"
    ) {
        val plutusScript = Arbitrary.arbitrary[Script.PlutusV1].sample.get
        val input = Arbitrary.arbitrary[TransactionInput].sample.get
        val utxo = Map(
          input -> TransactionOutput.Babbage(
            Address(
              Network.Testnet,
              Credential.KeyHash(Arbitrary.arbitrary[AddrKeyHash].sample.get)
            ),
            Value(Coin(1000000L)),
            datumOption = None,
            scriptRef = None
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
        val state = State(utxos = utxo)
        val result = MissingOrExtraScriptHashesValidator.validate(context, state, transaction)
        assert(result.isLeft)
    }

    test(
      "MissingOrExtraScriptHashesValidator failure for Mint with missing reference script in referenceInputs"
    ) {
        val plutusScript = Arbitrary.arbitrary[Script.PlutusV1].sample.get
        val refInput = Arbitrary.arbitrary[TransactionInput].sample.get
        val utxo = Map(
          refInput -> TransactionOutput.Babbage(
            Address(
              Network.Testnet,
              Credential.KeyHash(Arbitrary.arbitrary[AddrKeyHash].sample.get)
            ),
            Value(Coin(1000000L)),
            datumOption = None,
            scriptRef = None
          )
        )

        val transaction = Transaction(
          TransactionBody(
            inputs = TaggedSortedSet.empty,
            outputs = IndexedSeq.empty,
            fee = Coin.zero,
            referenceInputs = TaggedSortedSet.from(Set(refInput)),
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
        val state = State(utxos = utxo)
        val result = MissingOrExtraScriptHashesValidator.validate(context, state, transaction)
        assert(result.isLeft)
    }

    test(
      "MissingOrExtraScriptHashesValidator failure for Voting with missing reference script in inputs"
    ) {
        val plutusScript = Arbitrary.arbitrary[Script.PlutusV1].sample.get
        val input = Arbitrary.arbitrary[TransactionInput].sample.get
        val utxo = Map(
          input -> TransactionOutput.Babbage(
            Address(
              Network.Testnet,
              Credential.KeyHash(Arbitrary.arbitrary[AddrKeyHash].sample.get)
            ),
            Value(Coin(1000000L)),
            datumOption = None,
            scriptRef = None
          )
        )

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
            scripts = Seq.empty,
            redeemers = None,
            vkeyWitnesses = Set.empty,
            plutusData = Seq.empty
          )
        )
        val context = Context()
        val state = State(utxos = utxo)
        val result = MissingOrExtraScriptHashesValidator.validate(context, state, transaction)
        assert(result.isLeft)
    }

    test(
      "MissingOrExtraScriptHashesValidator failure for Voting with missing reference script in referenceInputs"
    ) {
        val plutusScript = Arbitrary.arbitrary[Script.PlutusV1].sample.get
        val refInput = Arbitrary.arbitrary[TransactionInput].sample.get
        val utxo = Map(
          refInput -> TransactionOutput.Babbage(
            Address(
              Network.Testnet,
              Credential.KeyHash(Arbitrary.arbitrary[AddrKeyHash].sample.get)
            ),
            Value(Coin(1000000L)),
            datumOption = None,
            scriptRef = None
          )
        )

        val transaction = Transaction(
          TransactionBody(
            inputs = TaggedSortedSet.empty,
            outputs = IndexedSeq.empty,
            fee = Coin.zero,
            referenceInputs = TaggedSortedSet.from(Set(refInput)),
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
        val state = State(utxos = utxo)
        val result = MissingOrExtraScriptHashesValidator.validate(context, state, transaction)
        assert(result.isLeft)
    }

    test(
      "MissingOrExtraScriptHashesValidator failure for Withdrawals with missing reference script in inputs"
    ) {
        val plutusScript = Arbitrary.arbitrary[Script.PlutusV1].sample.get
        val input = Arbitrary.arbitrary[TransactionInput].sample.get
        val utxo = Map(
          input -> TransactionOutput.Babbage(
            Address(
              Network.Testnet,
              Credential.KeyHash(Arbitrary.arbitrary[AddrKeyHash].sample.get)
            ),
            Value(Coin(1000000L)),
            datumOption = None,
            scriptRef = None
          )
        )

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
            scripts = Seq.empty,
            redeemers = None,
            vkeyWitnesses = Set.empty,
            plutusData = Seq.empty
          )
        )
        val context = Context()
        val state = State(utxos = utxo)
        val result = MissingOrExtraScriptHashesValidator.validate(context, state, transaction)
        assert(result.isLeft)
    }

    test(
      "MissingOrExtraScriptHashesValidator failure for Withdrawals with missing reference script in referenceInputs"
    ) {
        val plutusScript = Arbitrary.arbitrary[Script.PlutusV1].sample.get
        val refInput = Arbitrary.arbitrary[TransactionInput].sample.get
        val utxo = Map(
          refInput -> TransactionOutput.Babbage(
            Address(
              Network.Testnet,
              Credential.KeyHash(Arbitrary.arbitrary[AddrKeyHash].sample.get)
            ),
            Value(Coin(1000000L)),
            datumOption = None,
            scriptRef = None
          )
        )

        val transaction = Transaction(
          TransactionBody(
            inputs = TaggedSortedSet.empty,
            outputs = IndexedSeq.empty,
            fee = Coin.zero,
            referenceInputs = TaggedSortedSet.from(Set(refInput)),
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
        val state = State(utxos = utxo)
        val result = MissingOrExtraScriptHashesValidator.validate(context, state, transaction)
        assert(result.isLeft)
    }

    test(
      "MissingOrExtraScriptHashesValidator failure for Proposal Procedures with missing reference script in inputs"
    ) {
        val plutusScript = Arbitrary.arbitrary[Script.PlutusV1].sample.get
        val input = Arbitrary.arbitrary[TransactionInput].sample.get
        val utxo = Map(
          input -> TransactionOutput.Babbage(
            Address(
              Network.Testnet,
              Credential.KeyHash(Arbitrary.arbitrary[AddrKeyHash].sample.get)
            ),
            Value(Coin(1000000L)),
            datumOption = None,
            scriptRef = None
          )
        )

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
            scripts = Seq.empty,
            redeemers = None,
            vkeyWitnesses = Set.empty,
            plutusData = Seq.empty
          )
        )
        val context = Context()
        val state = State(utxos = utxo)
        val result = MissingOrExtraScriptHashesValidator.validate(context, state, transaction)
        assert(result.isLeft)
    }

    test(
      "MissingOrExtraScriptHashesValidator failure for Proposal Procedures with missing reference script in referenceInputs"
    ) {
        val plutusScript = Arbitrary.arbitrary[Script.PlutusV1].sample.get
        val refInput = Arbitrary.arbitrary[TransactionInput].sample.get
        val utxo = Map(
          refInput -> TransactionOutput.Babbage(
            Address(
              Network.Testnet,
              Credential.KeyHash(Arbitrary.arbitrary[AddrKeyHash].sample.get)
            ),
            Value(Coin(1000000L)),
            datumOption = None,
            scriptRef = None
          )
        )

        val transaction = Transaction(
          TransactionBody(
            inputs = TaggedSortedSet.empty,
            outputs = IndexedSeq.empty,
            fee = Coin.zero,
            referenceInputs = TaggedSortedSet.from(Set(refInput)),
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
        val state = State(utxos = utxo)
        val result = MissingOrExtraScriptHashesValidator.validate(context, state, transaction)
        assert(result.isLeft)
    }

    test(
      "MissingOrExtraScriptHashesValidator failure for Certificates with missing reference script in inputs"
    ) {
        val plutusScript = Arbitrary.arbitrary[Script.PlutusV1].sample.get
        val input = Arbitrary.arbitrary[TransactionInput].sample.get
        val utxo = Map(
          input -> TransactionOutput.Babbage(
            Address(
              Network.Testnet,
              Credential.KeyHash(Arbitrary.arbitrary[AddrKeyHash].sample.get)
            ),
            Value(Coin(1000000L)),
            datumOption = None,
            scriptRef = None
          )
        )

        val transaction = Transaction(
          TransactionBody(
            inputs = TaggedSortedSet.from(Set(input)),
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
        val state = State(utxos = utxo)
        val result = MissingOrExtraScriptHashesValidator.validate(context, state, transaction)
        assert(result.isLeft)
    }

    test(
      "MissingOrExtraScriptHashesValidator failure for Certificates with missing reference script in referenceInputs"
    ) {
        val plutusScript = Arbitrary.arbitrary[Script.PlutusV1].sample.get
        val refInput = Arbitrary.arbitrary[TransactionInput].sample.get
        val utxo = Map(
          refInput -> TransactionOutput.Babbage(
            Address(
              Network.Testnet,
              Credential.KeyHash(Arbitrary.arbitrary[AddrKeyHash].sample.get)
            ),
            Value(Coin(1000000L)),
            datumOption = None,
            scriptRef = None
          )
        )

        val transaction = Transaction(
          TransactionBody(
            inputs = TaggedSortedSet.empty,
            outputs = IndexedSeq.empty,
            fee = Coin.zero,
            referenceInputs = TaggedSortedSet.from(Set(refInput)),
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
        val state = State(utxos = utxo)
        val result = MissingOrExtraScriptHashesValidator.validate(context, state, transaction)
        assert(result.isLeft)
    }
}
