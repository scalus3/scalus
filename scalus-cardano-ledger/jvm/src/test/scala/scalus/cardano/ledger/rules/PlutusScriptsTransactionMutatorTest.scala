package scalus.cardano.ledger
package rules

import org.scalacheck.Arbitrary.arbitrary
import org.scalatest.funsuite.AnyFunSuite
import scalus.compiler.compile
import scalus.uplc.builtin.Data
import scalus.uplc.{PlutusV1, PlutusV2, PlutusV3, Term}
import scalus.cardano.address.*
import scalus.cardano.address.Network.Testnet
import scalus.cardano.node.SubmitError

import scala.collection.immutable.SortedMap

class PlutusScriptsTransactionMutatorTest extends AnyFunSuite, ValidatorRulesTestKit {

    test(
      "PlutusScriptsTransactionMutator success with isValid = true and valid scripts for Plutus V1"
    ) {
        val script = validPlutusV1Script

        val input = arbitrary[TransactionInput].sample.get
        val datum = arbitrary[Data].sample.get

        val output = Output(
          address = Address(Testnet, Credential.ScriptHash(script.scriptHash)),
          value = Value.lovelace(100000),
        )

        val tx = Transaction(
          body = TransactionBody(
            inputs = TaggedSortedSet(input),
            outputs = IndexedSeq(Sized(output)),
            fee = Coin(1000L)
          ),
          witnessSet = TransactionWitnessSet(
            scripts = Seq(script),
            redeemers = Some(
              Redeemers.Array(
                IndexedSeq(
                  Redeemer(
                    tag = RedeemerTag.Spend,
                    index = 0,
                    data = scalus.uplc.builtin.Data.unit,
                    exUnits = ExUnits(100000000, 100000000)
                  )
                )
              )
            ),
            vkeyWitnesses = Set.empty,
            plutusData = Seq(datum)
          )
        )

        val state = State(
          utxos = Map(
            input -> Output(
              address = Address(Testnet, Credential.ScriptHash(script.scriptHash)),
              value = Value.zero,
              datumHash = DataHash.fromByteString(datum.dataHash)
            )
          )
        )

        val result = PlutusScriptsTransactionMutator.transit(Context(), state, tx)
        assert(result.isRight)
    }

    test(
      "PlutusScriptsTransactionMutator success with isValid = true and valid scripts for Plutus V2"
    ) {
        val script = validPlutusV2Script

        val input = arbitrary[TransactionInput].sample.get
        val datum = arbitrary[Data].sample.get

        val output = Output(
          address = Address(Testnet, Credential.ScriptHash(script.scriptHash)),
          value = Value.lovelace(100000),
        )

        val tx = Transaction(
          body = TransactionBody(
            inputs = TaggedSortedSet(input),
            outputs = IndexedSeq(Sized(output)),
            fee = Coin(1000L)
          ),
          witnessSet = TransactionWitnessSet(
            scripts = Seq(script),
            redeemers = Some(
              Redeemers.Array(
                IndexedSeq(
                  Redeemer(
                    tag = RedeemerTag.Spend,
                    index = 0,
                    data = scalus.uplc.builtin.Data.unit,
                    exUnits = ExUnits(100000000, 100000000)
                  )
                )
              )
            ),
            vkeyWitnesses = Set.empty,
            plutusData = Seq(datum)
          )
        )

        val state = State(
          utxos = Map(
            input -> Output(
              address = Address(Testnet, Credential.ScriptHash(script.scriptHash)),
              value = Value.zero,
              datumHash = DataHash.fromByteString(datum.dataHash)
            )
          )
        )

        val result = PlutusScriptsTransactionMutator.transit(Context(), state, tx)
        assert(result.isRight)
    }

    test("PlutusScriptsTransactionMutator success with isValid = true and valid scripts") {
        val script = validPlutusV3Script

        val input = arbitrary[TransactionInput].sample.get
        val datum = arbitrary[Data].sample.get

        val output = Output(
          address = Address(Testnet, Credential.ScriptHash(script.scriptHash)),
          value = Value.lovelace(100000),
        )

        val transaction = Transaction(
          TransactionBody(
            inputs = TaggedSortedSet(input),
            outputs = IndexedSeq(Sized(output)),
            fee = Coin(1000L),
            donation = Some(Coin(500L)),
            currentTreasuryValue = Some(Coin(200000L))
          ),
          TransactionWitnessSet(
            scripts = Seq(script),
            redeemers = Some(
              Redeemers.Array(
                IndexedSeq(
                  Redeemer(
                    tag = RedeemerTag.Spend,
                    index = 0,
                    data = scalus.uplc.builtin.Data.unit,
                    exUnits = ExUnits(100000000, 100000000)
                  )
                )
              )
            ),
            vkeyWitnesses = Set.empty,
            plutusData = Seq(datum)
          )
        )

        val state = State(
          utxos = Map(
            input -> Output(
              address = Address(Testnet, Credential.ScriptHash(script.scriptHash)),
              value = Value.zero,
              datumHash = DataHash.fromByteString(datum.dataHash)
            )
          )
        )

        val result = PlutusScriptsTransactionMutator.transit(Context(), state, transaction)
        assert(result.isRight, result)

        val newState = result.toOption.get
        val newInput = Input(
          transactionId = transaction.id,
          index = 0
        )
        assert(
          state.utxos.contains(input) && !newState.utxos.contains(input),
          "Input UTxO should be consumed"
        )
        assert(
          !state.utxos.contains(newInput) && newState.utxos(newInput) == output,
          "Output UTxO should be added"
        )
        assert(
          state.fees == Coin(0) && newState.fees == transaction.body.value.fee,
          "Fees should be updated"
        )
        assert(
          state.donation == Coin(0) && newState.donation == transaction.body.value.donation.get,
          "Donation should be updated"
        )
    }

    test("PlutusScriptsTransactionMutator success with isValid = false and invalid scripts") {
        val script = invalidPlutusV3Script

        val input = arbitrary[TransactionInput].sample.get
        val collateralInput = arbitrary[TransactionInput].sample.get
        val datum = arbitrary[Data].sample.get

        val transaction = Transaction(
          body = KeepRaw(
            TransactionBody(
              inputs = TaggedSortedSet(input),
              collateralInputs = TaggedSortedSet(collateralInput),
              outputs = IndexedSeq.empty,
              fee = Coin(0),
              collateralReturnOutput = Some(
                Sized(
                  Output(
                    address = arbitrary[Address].sample.get,
                    value = Value.lovelace(80000)
                  )
                )
              )
            )
          ),
          witnessSetRaw = KeepRaw(
            TransactionWitnessSet(
              scripts = Seq(script),
              redeemers = Some(
                Redeemers.Array(
                  IndexedSeq(
                    Redeemer(
                      tag = RedeemerTag.Spend,
                      index = 0,
                      data = scalus.uplc.builtin.Data.unit,
                      exUnits = ExUnits(100000000, 100000000)
                    )
                  )
                )
              ),
              vkeyWitnesses = Set.empty,
              plutusData = Seq(datum)
            )
          ),
          isValid = false,
          auxiliaryData = None
        )

        val state = State(
          utxos = Map(
            input -> Output(
              address = Address(Testnet, Credential.ScriptHash(script.scriptHash)),
              value = Value.zero,
              datumHash = DataHash.fromByteString(datum.dataHash)
            ),
            collateralInput -> Output(
              address = arbitrary[Address].sample.get,
              value = Value.lovelace(100000)
            )
          )
        )

        val result = PlutusScriptsTransactionMutator.transit(Context(), state, transaction)
        assert(result.isRight, result)

        val newState = result.toOption.get
        val newInput = Input(
          transactionId = transaction.id,
          index = 0
        )
        assert(
          state.utxos.contains(collateralInput) && !newState.utxos.contains(collateralInput),
          "Collateral input UTxO should be consumed"
        )
        assert(
          !state.utxos.contains(newInput) && newState.utxos(
            newInput
          ) == transaction.body.value.collateralReturnOutput.get.value,
          "Collateral return output UTxO should be added"
        )
        assert(
          state.fees == Coin(0) && newState.fees == (
            state
                .utxos(collateralInput)
                .value
                .coin - transaction.body.value.collateralReturnOutput.get.value.value.coin
          ),
          "Fees should be updated"
        )
    }

    test(
      "PlutusScriptsTransactionMutator collects collateral whose inputs sum past Long.MaxValue"
    ) {
        val script = invalidPlutusV3Script

        val input = arbitrary[TransactionInput].sample.get
        val collateralInput1 = arbitrary[TransactionInput].sample.get
        val collateralInput2 = arbitrary[TransactionInput].sample.get
        val datum = arbitrary[Data].sample.get

        // The inputs hold 2 * Long.MaxValue lovelace and the return takes Long.MaxValue back, so
        // Long.MaxValue lovelace is collected.
        val transaction = Transaction(
          body = KeepRaw(
            TransactionBody(
              inputs = TaggedSortedSet(input),
              collateralInputs = TaggedSortedSet(collateralInput1, collateralInput2),
              outputs = IndexedSeq.empty,
              fee = Coin(0),
              collateralReturnOutput = Some(
                Sized(
                  Output(
                    address = arbitrary[Address].sample.get,
                    value = Value.lovelace(Long.MaxValue)
                  )
                )
              )
            )
          ),
          witnessSetRaw = KeepRaw(
            TransactionWitnessSet(
              scripts = Seq(script),
              redeemers = Some(
                Redeemers.Array(
                  IndexedSeq(
                    Redeemer(
                      tag = RedeemerTag.Spend,
                      index = 0,
                      data = scalus.uplc.builtin.Data.unit,
                      exUnits = ExUnits(100000000, 100000000)
                    )
                  )
                )
              ),
              vkeyWitnesses = Set.empty,
              plutusData = Seq(datum)
            )
          ),
          isValid = false,
          auxiliaryData = None
        )

        val state = State(
          utxos = Map(
            input -> Output(
              address = Address(Testnet, Credential.ScriptHash(script.scriptHash)),
              value = Value.zero,
              datumHash = DataHash.fromByteString(datum.dataHash)
            ),
            collateralInput1 -> Output(
              arbitrary[Address].sample.get,
              Value.lovelace(Long.MaxValue)
            ),
            collateralInput2 -> Output(arbitrary[Address].sample.get, Value.lovelace(Long.MaxValue))
          )
        )

        val result = PlutusScriptsTransactionMutator.transit(Context(), state, transaction)
        assert(result.map(_.fees) == Right(Coin(Long.MaxValue)), result)
    }

    test("PlutusScriptsTransactionMutator success with isValid = true and empty scripts") {
        val transaction = Transaction(
          TransactionBody(
            inputs = TaggedSortedSet.empty,
            outputs = IndexedSeq.empty,
            fee = Coin(0),
          )
        )

        val result = PlutusScriptsTransactionMutator.transit(Context(), State(), transaction)
        assert(result.isRight, result)
    }

    test("PlutusScriptsTransactionMutator failure with isValid = false and valid scripts") {
        val script = validPlutusV3Script

        val input = arbitrary[TransactionInput].sample.get
        val datum = arbitrary[Data].sample.get

        val transaction = Transaction(
          body = KeepRaw(
            TransactionBody(
              inputs = TaggedSortedSet(input),
              outputs = IndexedSeq.empty,
              fee = Coin(0),
            )
          ),
          witnessSetRaw = KeepRaw(
            TransactionWitnessSet(
              scripts = Seq(script),
              redeemers = Some(
                Redeemers.Array(
                  IndexedSeq(
                    Redeemer(
                      tag = RedeemerTag.Spend,
                      index = 0,
                      data = scalus.uplc.builtin.Data.unit,
                      exUnits = ExUnits(100000000, 100000000)
                    )
                  )
                )
              ),
              vkeyWitnesses = Set.empty,
              plutusData = Seq(datum)
            )
          ),
          isValid = false,
          auxiliaryData = None
        )

        val state = State(
          utxos = Map(
            input -> Output(
              address = Address(Testnet, Credential.ScriptHash(script.scriptHash)),
              value = Value.zero,
              datumHash = DataHash.fromByteString(datum.dataHash)
            )
          )
        )

        val result = PlutusScriptsTransactionMutator.transit(Context(), state, transaction)
        // ValidationTagMismatch (IsValid False) PassedUnexpectedly, B/Rules/Utxos.hs:255-260,
        // spec [FL-9]
        assert(
          result == Left(TransactionException.PassedUnexpectedlyException(transaction.id)),
          result
        )
        assert(
          result.left.map(SubmitError.fromException(_).rule) == Left("PassedUnexpectedly"),
          result
        )
    }

    test("PlutusScriptsTransactionMutator failure with isValid = true and invalid scripts") {
        val script = invalidPlutusV3Script

        val input = arbitrary[TransactionInput].sample.get
        val datum = arbitrary[Data].sample.get

        val transaction = Transaction(
          TransactionBody(
            inputs = TaggedSortedSet(input),
            outputs = IndexedSeq.empty,
            fee = Coin(0),
          ),
          TransactionWitnessSet(
            scripts = Seq(script),
            redeemers = Some(
              Redeemers.Array(
                IndexedSeq(
                  Redeemer(
                    tag = RedeemerTag.Spend,
                    index = 0,
                    data = scalus.uplc.builtin.Data.unit,
                    exUnits = ExUnits(100000000, 100000000)
                  )
                )
              )
            ),
            vkeyWitnesses = Set.empty,
            plutusData = Seq(datum)
          )
        )

        val state = State(
          utxos = Map(
            input -> Output(
              address = Address(Testnet, Credential.ScriptHash(script.scriptHash)),
              value = Value.zero,
              datumHash = DataHash.fromByteString(datum.dataHash)
            )
          )
        )

        val result = PlutusScriptsTransactionMutator.transit(Context(), state, transaction)
        assert(result.isLeft, result)
    }

    // ============ Conway Features Guard Tests for Plutus V1 ============

    test("PlutusV1 rejects transaction with voting procedures") {
        val script = validPlutusV1Script

        val input = arbitrary[TransactionInput].sample.get
        val datum = arbitrary[Data].sample.get

        val output = Output(
          address = Address(Testnet, Credential.ScriptHash(script.scriptHash)),
          value = Value.lovelace(100000),
        )

        val tx = Transaction(
          body = TransactionBody(
            inputs = TaggedSortedSet(input),
            outputs = IndexedSeq(Sized(output)),
            fee = Coin(1000L),
            votingProcedures = Some(
              VotingProcedures(
                arbitrary[SortedMap[Voter, SortedMap[GovActionId, VotingProcedure]]]
                    .retryUntil(_.nonEmpty)
                    .sample
                    .get
              )
            )
          ),
          witnessSet = TransactionWitnessSet(
            scripts = Seq(script),
            redeemers = Some(
              Redeemers.Array(
                IndexedSeq(
                  Redeemer(
                    tag = RedeemerTag.Spend,
                    index = 0,
                    data = scalus.uplc.builtin.Data.unit,
                    exUnits = ExUnits(100000000, 100000000)
                  )
                )
              )
            ),
            vkeyWitnesses = Set.empty,
            plutusData = Seq(datum)
          )
        )

        val state = State(
          utxos = Map(
            input -> Output(
              address = Address(Testnet, Credential.ScriptHash(script.scriptHash)),
              value = Value.zero,
              datumHash = DataHash.fromByteString(datum.dataHash)
            )
          )
        )

        val result = PlutusScriptsTransactionMutator.transit(Context(), state, tx)
        assertBadTranslation(result, "VotingProceduresFieldNotSupported")
    }

    test("PlutusV1 rejects transaction with proposal procedures") {
        val script = validPlutusV1Script

        val input = arbitrary[TransactionInput].sample.get
        val datum = arbitrary[Data].sample.get

        val output = Output(
          address = Address(Testnet, Credential.ScriptHash(script.scriptHash)),
          value = Value.lovelace(100000),
        )

        val tx = Transaction(
          body = TransactionBody(
            inputs = TaggedSortedSet(input),
            outputs = IndexedSeq(Sized(output)),
            fee = Coin(1000L),
            proposalProcedures = TaggedOrderedSet(arbitrary[ProposalProcedure].sample.get)
          ),
          witnessSet = TransactionWitnessSet(
            scripts = Seq(script),
            redeemers = Some(
              Redeemers.Array(
                IndexedSeq(
                  Redeemer(
                    tag = RedeemerTag.Spend,
                    index = 0,
                    data = scalus.uplc.builtin.Data.unit,
                    exUnits = ExUnits(100000000, 100000000)
                  )
                )
              )
            ),
            vkeyWitnesses = Set.empty,
            plutusData = Seq(datum)
          )
        )

        val state = State(
          utxos = Map(
            input -> Output(
              address = Address(Testnet, Credential.ScriptHash(script.scriptHash)),
              value = Value.zero,
              datumHash = DataHash.fromByteString(datum.dataHash)
            )
          )
        )

        val result = PlutusScriptsTransactionMutator.transit(Context(), state, tx)
        assertBadTranslation(result, "ProposalProceduresFieldNotSupported")
    }

    test("PlutusV1 rejects transaction with non-zero donation") {
        val script = validPlutusV1Script

        val input = arbitrary[TransactionInput].sample.get
        val datum = arbitrary[Data].sample.get

        val output = Output(
          address = Address(Testnet, Credential.ScriptHash(script.scriptHash)),
          value = Value.lovelace(100000),
        )

        val tx = Transaction(
          body = TransactionBody(
            inputs = TaggedSortedSet(input),
            outputs = IndexedSeq(Sized(output)),
            fee = Coin(1000L),
            donation = Some(Coin(500000)),
            currentTreasuryValue = Some(Coin.zero)
          ),
          witnessSet = TransactionWitnessSet(
            scripts = Seq(script),
            redeemers = Some(
              Redeemers.Array(
                IndexedSeq(
                  Redeemer(
                    tag = RedeemerTag.Spend,
                    index = 0,
                    data = scalus.uplc.builtin.Data.unit,
                    exUnits = ExUnits(100000000, 100000000)
                  )
                )
              )
            ),
            vkeyWitnesses = Set.empty,
            plutusData = Seq(datum)
          )
        )

        val state = State(
          utxos = Map(
            input -> Output(
              address = Address(Testnet, Credential.ScriptHash(script.scriptHash)),
              value = Value.zero,
              datumHash = DataHash.fromByteString(datum.dataHash)
            )
          )
        )

        val result = PlutusScriptsTransactionMutator.transit(Context(), state, tx)
        assertBadTranslation(result, "TreasuryDonationFieldNotSupported")
    }

    test("PlutusV1 rejects transaction with current treasury value") {
        val script = validPlutusV1Script

        val input = arbitrary[TransactionInput].sample.get
        val datum = arbitrary[Data].sample.get

        val output = Output(
          address = Address(Testnet, Credential.ScriptHash(script.scriptHash)),
          value = Value.lovelace(100000),
        )

        val tx = Transaction(
          body = TransactionBody(
            inputs = TaggedSortedSet(input),
            outputs = IndexedSeq(Sized(output)),
            fee = Coin(1000L),
            donation = Some(Coin.zero),
            currentTreasuryValue = Some(Coin(1000000000))
          ),
          witnessSet = TransactionWitnessSet(
            scripts = Seq(script),
            redeemers = Some(
              Redeemers.Array(
                IndexedSeq(
                  Redeemer(
                    tag = RedeemerTag.Spend,
                    index = 0,
                    data = scalus.uplc.builtin.Data.unit,
                    exUnits = ExUnits(100000000, 100000000)
                  )
                )
              )
            ),
            vkeyWitnesses = Set.empty,
            plutusData = Seq(datum)
          )
        )

        val state = State(
          utxos = Map(
            input -> Output(
              address = Address(Testnet, Credential.ScriptHash(script.scriptHash)),
              value = Value.zero,
              datumHash = DataHash.fromByteString(datum.dataHash)
            )
          )
        )

        val result = PlutusScriptsTransactionMutator.transit(Context(), state, tx)
        assertBadTranslation(result, "CurrentTreasuryFieldNotSupported")
    }

    // ============ Conway Features Guard Tests for Plutus V2 ============

    test("PlutusV2 rejects transaction with voting procedures") {
        val script = validPlutusV2Script

        val input = arbitrary[TransactionInput].sample.get
        val datum = arbitrary[Data].sample.get

        val output = Output(
          address = Address(Testnet, Credential.ScriptHash(script.scriptHash)),
          value = Value.lovelace(100000),
        )

        val tx = Transaction(
          body = TransactionBody(
            inputs = TaggedSortedSet(input),
            outputs = IndexedSeq(Sized(output)),
            fee = Coin(1000L),
            votingProcedures = Some(
              VotingProcedures(
                arbitrary[SortedMap[Voter, SortedMap[GovActionId, VotingProcedure]]]
                    .retryUntil(_.nonEmpty)
                    .sample
                    .get
              )
            )
          ),
          witnessSet = TransactionWitnessSet(
            scripts = Seq(script),
            redeemers = Some(
              Redeemers.Array(
                IndexedSeq(
                  Redeemer(
                    tag = RedeemerTag.Spend,
                    index = 0,
                    data = scalus.uplc.builtin.Data.unit,
                    exUnits = ExUnits(100000000, 100000000)
                  )
                )
              )
            ),
            vkeyWitnesses = Set.empty,
            plutusData = Seq(datum)
          )
        )

        val state = State(
          utxos = Map(
            input -> Output(
              address = Address(Testnet, Credential.ScriptHash(script.scriptHash)),
              value = Value.zero,
              datumHash = DataHash.fromByteString(datum.dataHash)
            )
          )
        )

        val result = PlutusScriptsTransactionMutator.transit(Context(), state, tx)
        assertBadTranslation(result, "VotingProceduresFieldNotSupported")
    }

    test("PlutusV2 rejects transaction with proposal procedures") {
        val script = validPlutusV2Script

        val input = arbitrary[TransactionInput].sample.get
        val datum = arbitrary[Data].sample.get

        val output = Output(
          address = Address(Testnet, Credential.ScriptHash(script.scriptHash)),
          value = Value.lovelace(100000),
        )

        val tx = Transaction(
          body = TransactionBody(
            inputs = TaggedSortedSet(input),
            outputs = IndexedSeq(Sized(output)),
            fee = Coin(1000L),
            proposalProcedures = TaggedOrderedSet(arbitrary[ProposalProcedure].sample.get)
          ),
          witnessSet = TransactionWitnessSet(
            scripts = Seq(script),
            redeemers = Some(
              Redeemers.Array(
                IndexedSeq(
                  Redeemer(
                    tag = RedeemerTag.Spend,
                    index = 0,
                    data = scalus.uplc.builtin.Data.unit,
                    exUnits = ExUnits(100000000, 100000000)
                  )
                )
              )
            ),
            vkeyWitnesses = Set.empty,
            plutusData = Seq(datum)
          )
        )

        val state = State(
          utxos = Map(
            input -> Output(
              address = Address(Testnet, Credential.ScriptHash(script.scriptHash)),
              value = Value.zero,
              datumHash = DataHash.fromByteString(datum.dataHash)
            )
          )
        )

        val result = PlutusScriptsTransactionMutator.transit(Context(), state, tx)
        assertBadTranslation(result, "ProposalProceduresFieldNotSupported")
    }

    test("PlutusV2 rejects transaction with non-zero donation") {
        val script = validPlutusV2Script

        val input = arbitrary[TransactionInput].sample.get
        val datum = arbitrary[Data].sample.get

        val output = Output(
          address = Address(Testnet, Credential.ScriptHash(script.scriptHash)),
          value = Value.lovelace(100000),
        )

        val tx = Transaction(
          body = TransactionBody(
            inputs = TaggedSortedSet(input),
            outputs = IndexedSeq(Sized(output)),
            fee = Coin(1000L),
            donation = Some(Coin(500000)),
            currentTreasuryValue = Some(Coin.zero)
          ),
          witnessSet = TransactionWitnessSet(
            scripts = Seq(script),
            redeemers = Some(
              Redeemers.Array(
                IndexedSeq(
                  Redeemer(
                    tag = RedeemerTag.Spend,
                    index = 0,
                    data = scalus.uplc.builtin.Data.unit,
                    exUnits = ExUnits(100000000, 100000000)
                  )
                )
              )
            ),
            vkeyWitnesses = Set.empty,
            plutusData = Seq(datum)
          )
        )

        val state = State(
          utxos = Map(
            input -> Output(
              address = Address(Testnet, Credential.ScriptHash(script.scriptHash)),
              value = Value.zero,
              datumHash = DataHash.fromByteString(datum.dataHash)
            )
          )
        )

        val result = PlutusScriptsTransactionMutator.transit(Context(), state, tx)
        assertBadTranslation(result, "TreasuryDonationFieldNotSupported")
    }

    test("PlutusV2 rejects transaction with current treasury value") {
        val script = validPlutusV2Script

        val input = arbitrary[TransactionInput].sample.get
        val datum = arbitrary[Data].sample.get

        val output = Output(
          address = Address(Testnet, Credential.ScriptHash(script.scriptHash)),
          value = Value.lovelace(100000),
        )

        val tx = Transaction(
          body = TransactionBody(
            inputs = TaggedSortedSet(input),
            outputs = IndexedSeq(Sized(output)),
            fee = Coin(1000L),
            donation = Some(Coin.zero),
            currentTreasuryValue = Some(Coin(1000000000))
          ),
          witnessSet = TransactionWitnessSet(
            scripts = Seq(script),
            redeemers = Some(
              Redeemers.Array(
                IndexedSeq(
                  Redeemer(
                    tag = RedeemerTag.Spend,
                    index = 0,
                    data = scalus.uplc.builtin.Data.unit,
                    exUnits = ExUnits(100000000, 100000000)
                  )
                )
              )
            ),
            vkeyWitnesses = Set.empty,
            plutusData = Seq(datum)
          )
        )

        val state = State(
          utxos = Map(
            input -> Output(
              address = Address(Testnet, Credential.ScriptHash(script.scriptHash)),
              value = Value.zero,
              datumHash = DataHash.fromByteString(datum.dataHash)
            )
          )
        )

        val result = PlutusScriptsTransactionMutator.transit(Context(), state, tx)
        assertBadTranslation(result, "CurrentTreasuryFieldNotSupported")
    }

    // ============ Inline Datum and Byron Address Guard Tests for Plutus V1 ============

    test("PlutusV1 rejects transaction with inline datum in input") {
        val script = validPlutusV1Script

        val input = arbitrary[TransactionInput].sample.get
        val datum = Data.unit

        val output = TransactionOutput.Babbage(
          address = Address(Testnet, Credential.ScriptHash(script.scriptHash)),
          value = Value.lovelace(100000),
          datumOption = Some(DatumOption.Hash(DataHash.fromByteString(datum.dataHash))),
          scriptRef = None
        )

        val tx = Transaction(
          body = TransactionBody(
            inputs = TaggedSortedSet(input),
            outputs = IndexedSeq(Sized(output)),
            fee = Coin(1000L)
          ),
          witnessSet = TransactionWitnessSet(
            scripts = Seq(script),
            redeemers = Some(
              Redeemers.Array(
                IndexedSeq(
                  Redeemer(
                    tag = RedeemerTag.Spend,
                    index = 0,
                    data = scalus.uplc.builtin.Data.unit,
                    exUnits = ExUnits(100000000, 100000000)
                  )
                )
              )
            ),
            vkeyWitnesses = Set.empty,
            plutusData = Seq(datum)
          )
        )

        val state = State(
          utxos = Map(
            input -> Output(
              address = Address(Testnet, Credential.ScriptHash(script.scriptHash)),
              value = Value.zero,
              inlineDatum = datum
            )
          )
        )

        val result = PlutusScriptsTransactionMutator.transit(Context(), state, tx)
        assertBadTranslation(result, "InlineDatumsNotSupported")
    }

    test("PlutusV1 rejects transaction with inline datum in reference input") {
        val script = validPlutusV1Script

        val input = arbitrary[TransactionInput].sample.get
        val referenceInput = arbitrary[TransactionInput].sample.get
        val datum = Data.unit

        val output = TransactionOutput.Babbage(
          address = Address(Testnet, Credential.ScriptHash(script.scriptHash)),
          value = Value.lovelace(100000),
          datumOption = Some(DatumOption.Hash(DataHash.fromByteString(datum.dataHash))),
          scriptRef = None
        )

        val tx = Transaction(
          body = TransactionBody(
            inputs = TaggedSortedSet(input),
            referenceInputs = TaggedSortedSet(referenceInput),
            outputs = IndexedSeq(Sized(output)),
            fee = Coin(1000L)
          ),
          witnessSet = TransactionWitnessSet(
            scripts = Seq(script),
            redeemers = Some(
              Redeemers.Array(
                IndexedSeq(
                  Redeemer(
                    tag = RedeemerTag.Spend,
                    index = 0,
                    data = scalus.uplc.builtin.Data.unit,
                    exUnits = ExUnits(100000000, 100000000)
                  )
                )
              )
            ),
            vkeyWitnesses = Set.empty,
            plutusData = Seq(datum)
          )
        )

        val state = State(
          utxos = Map(
            input -> Output(
              address = Address(Testnet, Credential.ScriptHash(script.scriptHash)),
              value = Value.zero,
              datumOption = DatumOption.Hash(DataHash.fromByteString(datum.dataHash))
            ),
            referenceInput -> Output(
              address = Address(Testnet, Credential.ScriptHash(script.scriptHash)),
              value = Value.zero,
              inlineDatum = datum
            )
          )
        )

        val result = PlutusScriptsTransactionMutator.transit(Context(), state, tx)
        assertBadTranslation(result, "InlineDatumsNotSupported")
    }

    test("PlutusV1 rejects transaction with inline datum in output") {
        val script = validPlutusV1Script

        val input = arbitrary[TransactionInput].sample.get
        val datum = Data.unit

        val outputWithInlineDatum = TransactionOutput.Babbage(
          address = Address(Testnet, Credential.ScriptHash(script.scriptHash)),
          value = Value.lovelace(100000),
          datumOption = Some(DatumOption.Inline(datum)),
          scriptRef = None
        )

        val tx = Transaction(
          body = TransactionBody(
            inputs = TaggedSortedSet(input),
            outputs = IndexedSeq(Sized(outputWithInlineDatum)),
            fee = Coin(1000L)
          ),
          witnessSet = TransactionWitnessSet(
            scripts = Seq(script),
            redeemers = Some(
              Redeemers.Array(
                IndexedSeq(
                  Redeemer(
                    tag = RedeemerTag.Spend,
                    index = 0,
                    data = scalus.uplc.builtin.Data.unit,
                    exUnits = ExUnits(100000000, 100000000)
                  )
                )
              )
            ),
            vkeyWitnesses = Set.empty,
            plutusData = Seq(datum)
          )
        )

        val state = State(
          utxos = Map(
            input -> Output(
              address = Address(Testnet, Credential.ScriptHash(script.scriptHash)),
              value = Value.zero,
              datumHash = DataHash.fromByteString(datum.dataHash)
            )
          )
        )

        val result = PlutusScriptsTransactionMutator.transit(Context(), state, tx)
        assertBadTranslation(result, "InlineDatumsNotSupported")
    }

    test("PlutusV1 rejects transaction with Byron address in input") {
        val script = validPlutusV1Script

        val input1 = arbitrary[TransactionInput].sample.get
        val input2 = arbitrary[TransactionInput].sample.get
        val datum = Data.unit
        val byronAddress = arbitrary[ByronAddress].sample.get

        val output = TransactionOutput.Babbage(
          address = Address(Testnet, Credential.ScriptHash(script.scriptHash)),
          value = Value.lovelace(100000),
          datumOption = Some(DatumOption.Hash(DataHash.fromByteString(datum.dataHash))),
          scriptRef = None
        )

        val tx = Transaction(
          body = TransactionBody(
            inputs = TaggedSortedSet(input1, input2),
            outputs = IndexedSeq(Sized(output)),
            fee = Coin(1000L)
          ),
          witnessSet = TransactionWitnessSet(
            scripts = Seq(script),
            redeemers = Some(
              Redeemers.Array(
                IndexedSeq(
                  Redeemer(
                    tag = RedeemerTag.Spend,
                    index = 0,
                    data = scalus.uplc.builtin.Data.unit,
                    exUnits = ExUnits(100000000, 100000000)
                  )
                )
              )
            ),
            vkeyWitnesses = Set.empty,
            plutusData = Seq(datum)
          )
        )

        val state = State(
          utxos = Map(
            tx.body.value.inputs.toSet.head -> TransactionOutput.Babbage(
              address = Address(Testnet, Credential.ScriptHash(script.scriptHash)),
              value = Value.zero,
              datumOption = Some(DatumOption.Hash(DataHash.fromByteString(datum.dataHash))),
              scriptRef = None
            ),
            tx.body.value.inputs.toSet.tail.head -> TransactionOutput.Babbage(
              address = byronAddress,
              value = Value.zero
            )
          )
        )

        val result = PlutusScriptsTransactionMutator.transit(Context(), state, tx)
        assertBadTranslation(result, "ByronTxOutInContext")
    }

    test("PlutusV1 rejects transaction with Byron address in reference input") {
        val script = validPlutusV1Script

        val input = arbitrary[TransactionInput].sample.get
        val referenceInput = arbitrary[TransactionInput].sample.get
        val datum = Data.unit
        val byronAddress = arbitrary[ByronAddress].sample.get

        val output = TransactionOutput.Babbage(
          address = Address(Testnet, Credential.ScriptHash(script.scriptHash)),
          value = Value.lovelace(100000),
          datumOption = Some(DatumOption.Hash(DataHash.fromByteString(datum.dataHash))),
          scriptRef = None
        )

        val tx = Transaction(
          body = TransactionBody(
            inputs = TaggedSortedSet(input),
            referenceInputs = TaggedSortedSet(referenceInput),
            outputs = IndexedSeq(Sized(output)),
            fee = Coin(1000L)
          ),
          witnessSet = TransactionWitnessSet(
            scripts = Seq(script),
            redeemers = Some(
              Redeemers.Array(
                IndexedSeq(
                  Redeemer(
                    tag = RedeemerTag.Spend,
                    index = 0,
                    data = scalus.uplc.builtin.Data.unit,
                    exUnits = ExUnits(100000000, 100000000)
                  )
                )
              )
            ),
            vkeyWitnesses = Set.empty,
            plutusData = Seq(datum)
          )
        )

        val state = State(
          utxos = Map(
            input -> TransactionOutput.Babbage(
              address = Address(Testnet, Credential.ScriptHash(script.scriptHash)),
              value = Value.zero,
              datumOption = Some(DatumOption.Hash(DataHash.fromByteString(datum.dataHash))),
              scriptRef = None
            ),
            referenceInput -> TransactionOutput.Babbage(
              address = byronAddress,
              value = Value.zero,
              datumOption = Some(DatumOption.Hash(DataHash.fromByteString(datum.dataHash))),
              scriptRef = None
            )
          )
        )

        val result = PlutusScriptsTransactionMutator.transit(Context(), state, tx)
        assertBadTranslation(result, "ByronTxOutInContext")
    }

    test("PlutusV1 rejects transaction with Byron address in output") {

        val script = validPlutusV1Script

        val input = arbitrary[TransactionInput].sample.get
        val datum = Data.unit
        val byronAddress = arbitrary[ByronAddress].sample.get

        val outputWithByronAddress = TransactionOutput.Babbage(
          address = byronAddress,
          value = Value.lovelace(100000),
          datumOption = Some(DatumOption.Hash(DataHash.fromByteString(datum.dataHash))),
          scriptRef = None
        )

        val tx = Transaction(
          body = TransactionBody(
            inputs = TaggedSortedSet(input),
            outputs = IndexedSeq(Sized(outputWithByronAddress)),
            fee = Coin(1000L)
          ),
          witnessSet = TransactionWitnessSet(
            scripts = Seq(script),
            redeemers = Some(
              Redeemers.Array(
                IndexedSeq(
                  Redeemer(
                    tag = RedeemerTag.Spend,
                    index = 0,
                    data = scalus.uplc.builtin.Data.unit,
                    exUnits = ExUnits(100000000, 100000000)
                  )
                )
              )
            ),
            vkeyWitnesses = Set.empty,
            plutusData = Seq(datum)
          )
        )

        val state = State(
          utxos = Map(
            input -> TransactionOutput.Babbage(
              address = Address(Testnet, Credential.ScriptHash(script.scriptHash)),
              value = Value.zero,
              datumOption = Some(DatumOption.Hash(DataHash.fromByteString(datum.dataHash))),
              scriptRef = None
            )
          )
        )

        val result = PlutusScriptsTransactionMutator.transit(Context(), state, tx)
        assertBadTranslation(result, "ByronTxOutInContext")
    }

    test("PlutusV2 rejects transaction with a Conway certificate, spec [FL-10]") {
        // transTxCertV1V2 (eras/conway/impl/src/Cardano/Ledger/Conway/TxInfo.hs:390-397):
        // CollectErrors [BadTranslation (CertificateNotSupported txCert)]
        val (spend, state, _, _) = spendAndReference(validPlutusV2Script, isValid = true)
        val drep = arbitrary[Credential].sample.get
        val tx = spend.withBody(
          _.copy(
            referenceInputs = TaggedSortedSet.empty,
            certificates =
                TaggedOrderedStrictSet(Certificate.RegDRepCert(drep, Coin(500_000_000L), None))
          )
        )
        val result = PlutusScriptsTransactionMutator.transit(Context(), state, tx)
        assertBadTranslation(result, "CertificateNotSupported")
    }

    /** A PlutusV1 spend, whose reference input and only output are `reference` and `output`. */
    private def plutusV1With(
        reference: TransactionOutput,
        output: TransactionOutput
    ): (Transaction, State) = {
        val (spend, state, _, _) = spendAndReference(validPlutusV1Script, isValid = true)
        val referenceInput = arbitrary[TransactionInput].sample.get
        val tx = spend.withBody(
          _.copy(
            referenceInputs = TaggedSortedSet(referenceInput),
            outputs = IndexedSeq(Sized(output))
          )
        )
        (tx, state.copy(utxos = state.utxos + (referenceInput -> reference)))
    }

    private def babbageOutput(address: Address, inline: Boolean) =
        TransactionOutput.Babbage(
          address = address,
          value = Value.lovelace(100000),
          datumOption = Option.when(inline)(DatumOption.Inline(Data.unit)),
          scriptRef = None
        )

    test("PlutusV1 checks the reference inputs before the outputs, spec [FL-10]") {
        // The Conway PlutusV1 TxInfo translates the inputs, the reference inputs, then the outputs
        // (eras/conway/impl/src/Cardano/Ledger/Conway/TxInfo.hs:405-412)
        val byron = arbitrary[ByronAddress].sample.get
        val shelley = Address(Testnet, Credential.ScriptHash(validPlutusV1Script.scriptHash))
        val (tx, state) = plutusV1With(
          reference = babbageOutput(byron, inline = false),
          output = babbageOutput(shelley, inline = true)
        )
        val result = PlutusScriptsTransactionMutator.transit(Context(), state, tx)
        assertBadTranslation(result, "ByronTxOutInContext")
    }

    test("PlutusV1 checks the inline datum of an output before its address, spec [FL-10]") {
        // transTxOutV1 (eras/conway/impl/src/Cardano/Ledger/Conway/TxInfo.hs:315-321)
        val byron = arbitrary[ByronAddress].sample.get
        val shelley = Address(Testnet, Credential.ScriptHash(validPlutusV1Script.scriptHash))
        val (tx, state) = plutusV1With(
          reference = babbageOutput(shelley, inline = false),
          output = babbageOutput(byron, inline = true)
        )
        val result = PlutusScriptsTransactionMutator.transit(Context(), state, tx)
        assertBadTranslation(result, "InlineDatumsNotSupported")
    }

    /** Asserts that `result` is one `BadTranslation` collect error whose message starts with the
      * ledger `ContextError` constructor `constructor`, spec [FL-10].
      */
    private def assertBadTranslation(
        result: PlutusScriptsTransactionMutator.Result,
        constructor: String
    ) =
        result match
            case Left(
                  e @ TransactionException.CollectErrorsException(
                    _,
                    Seq(CollectError.BadTranslation(message))
                  )
                ) =>
                assert(message.startsWith(constructor + ": "), e)
                assert(SubmitError.fromException(e).rule == "CollectErrors")
            case other =>
                fail(s"expected CollectErrors [BadTranslation ($constructor ...)], got $other")

    /** A tx that spends `input`, locked by `script`, and also has it in its reference inputs. */
    private def spendAndReference(
        script: PlutusScript,
        isValid: Boolean
    ): (Transaction, State, TransactionInput, TransactionInput) = {
        val input = arbitrary[TransactionInput].sample.get
        val collateralInput = arbitrary[TransactionInput].sample.get
        val datum = arbitrary[Data].sample.get
        val tx = Transaction(
          body = KeepRaw(
            TransactionBody(
              inputs = TaggedSortedSet(input),
              referenceInputs = TaggedSortedSet(input),
              collateralInputs = TaggedSortedSet(collateralInput),
              outputs = IndexedSeq.empty,
              fee = Coin(0)
            )
          ),
          witnessSetRaw = KeepRaw(
            TransactionWitnessSet(
              scripts = Seq(script),
              redeemers = Some(
                Redeemers.Array(
                  IndexedSeq(
                    Redeemer(
                      tag = RedeemerTag.Spend,
                      index = 0,
                      data = Data.unit,
                      exUnits = ExUnits(100000000, 100000000)
                    )
                  )
                )
              ),
              vkeyWitnesses = Set.empty,
              plutusData = Seq(datum)
            )
          ),
          isValid = isValid,
          auxiliaryData = None
        )
        val state = State(
          utxos = Map(
            input -> Output(
              address = Address(Testnet, Credential.ScriptHash(script.scriptHash)),
              value = Value.zero,
              datumHash = DataHash.fromByteString(datum.dataHash)
            ),
            collateralInput -> Output(
              address = arbitrary[Address].sample.get,
              value = Value.lovelace(100000)
            )
          )
        )
        (tx, state, input, collateralInput)
    }

    private def assertRejectedForOverlap(
        result: PlutusScriptsTransactionMutator.Result,
        input: TransactionInput
    ) =
        result match
            // CollectErrors [BadTranslation (ReferenceInputsNotDisjointFromInputs ...)], spec [FL-10]
            case Left(
                  e @ TransactionException.CollectErrorsException(
                    _,
                    Seq(CollectError.BadTranslation(message))
                  )
                ) =>
                assert(message.contains("ReferenceInputsNotDisjointFromInputs"), e)
                assert(message.contains(input.toString), e)
                assert(SubmitError.fromException(e).rule == "CollectErrors")
            case other => fail(s"expected a rejection for the overlap, got $other")

    test(
      "a PlutusV3 spend of a UTxO that is also a reference input is rejected at PV 11, spec [TX-1]"
    ) {
        // CollectErrors (ReferenceInputsNotDisjointFromInputs), C/TxInfo.hs:488-493
        assert(Context().env.params.protocolVersion.major == 11)
        val (tx, state, input, _) = spendAndReference(validPlutusV3Script, isValid = true)
        assertRejectedForOverlap(
          PlutusScriptsTransactionMutator.transit(Context(), state, tx),
          input
        )
    }

    test(
      "the PV 11 overlap of a PlutusV3 tx with isValid = false is rejected, not collected, spec [TX-1]"
    ) {
        // The script fails, but the context cannot be built, so UTXOS fails in phase 1 and no
        // collateral is taken, whatever isValid says.
        val (tx, state, input, _) = spendAndReference(invalidPlutusV3Script, isValid = false)
        assertRejectedForOverlap(
          PlutusScriptsTransactionMutator.transit(Context(), state, tx),
          input
        )
    }

    test("a PlutusV2 spend of a UTxO that is also a reference input passes at PV 11, spec [TX-1]") {
        val (tx, state, input, _) = spendAndReference(validPlutusV2Script, isValid = true)
        val result = PlutusScriptsTransactionMutator.transit(Context(), state, tx)
        assert(result.map(_.utxos.contains(input)) == Right(false), result)
    }

    test(
      "the PV 11 overlap is rejected before an earlier PlutusV2 script of a tx with isValid = false fails, spec [TX-1]"
    ) {
        // collectPlutusScriptsWithContext builds every script context before any script runs
        // (eras/alonzo/impl/src/Cardano/Ledger/Alonzo/Plutus/Evaluate.hs:157-214), so the PlutusV3
        // collect error wins over the PlutusV2 failure that would collect the collateral. Spend
        // redeemers run before Mint ones, so the PlutusV2 spend would fail first.
        val (spend, state, input, _) = spendAndReference(invalidPlutusV2Script, isValid = false)
        val mint = validPlutusV3Script
        import TransactionWitnessSet.given
        val tx = spend
            .withBody(
              _.copy(mint =
                  Some(
                    Mint(MultiAsset(SortedMap(mint.scriptHash -> SortedMap(AssetName.empty -> 1))))
                  )
              )
            )
            .withWitness(ws =>
                ws.copy(
                  plutusV3Scripts =
                      TaggedSortedStrictMap.from[ScriptHash, Script.PlutusV3](Seq(mint)),
                  redeemers = ws.redeemers.map(redeemers =>
                      KeepRaw[Redeemers](
                        Redeemers.Array(
                          redeemers.value.toIndexedSeq :+ Redeemer(
                            RedeemerTag.Mint,
                            0,
                            Data.unit,
                            ExUnits(100000000, 100000000)
                          )
                        )
                      )
                  )
                )
            )
        assertRejectedForOverlap(
          PlutusScriptsTransactionMutator.transit(Context(), state, tx),
          input
        )
    }

    test("a PlutusV3 script with no cost model is a collect error, spec [FL-10]") {
        // Alonzo UtxosSpec "No cost model" (eras/alonzo/impl/testlib/Test/Cardano/Ledger/Alonzo/
        // Imp/UtxosSpec.hs:202-208): CollectErrors [NoCostModel lang]
        val (tx, state, _, _) = spendAndReference(validPlutusV3Script, isValid = true)
        val unshared = tx.withBody(_.copy(referenceInputs = TaggedSortedSet.empty))
        val result = PlutusScriptsTransactionMutator.transit(noCostModels, state, unshared)
        assert(
          result == Left(
            TransactionException.CollectErrorsException(
              unshared.id,
              Seq(CollectError.NoCostModel(Language.PlutusV3))
            )
          ),
          result
        )
    }

    test("the collect errors follow merge of collectPlutusScriptsWithContext, spec [FL-10]") {
        // Two spends of a PlutusV3 script without a cost model, then a mint without a redeemer.
        // After the first error only NoRedeemer errors are added, each in front
        // (eras/alonzo/impl/src/Cardano/Ledger/Alonzo/Plutus/Evaluate.hs:196-214).
        val (spend, state, input, _) = spendAndReference(validPlutusV3Script, isValid = true)
        val second = arbitrary[TransactionInput].sample.get
        val mint = invalidPlutusV3Script
        import TransactionWitnessSet.given
        val tx = spend
            .withBody(
              _.copy(
                inputs = TaggedSortedSet(input, second),
                referenceInputs = TaggedSortedSet.empty,
                mint = Some(
                  Mint(MultiAsset(SortedMap(mint.scriptHash -> SortedMap(AssetName.empty -> 1))))
                )
              )
            )
            .withWitness(ws =>
                ws.copy(
                  plutusV3Scripts = TaggedSortedStrictMap
                      .from[ScriptHash, Script.PlutusV3](Seq(validPlutusV3Script, mint)),
                  redeemers = Some(
                    KeepRaw[Redeemers](
                      Redeemers.Array(
                        IndexedSeq(0, 1).map(i =>
                            Redeemer(RedeemerTag.Spend, i, Data.unit, ExUnits(1000, 1000))
                        )
                      )
                    )
                  )
                )
            )
        val withSecond = state.copy(utxos = state.utxos + (second -> state.utxos(input)))
        val result = PlutusScriptsTransactionMutator.transit(noCostModels, withSecond, tx)
        assert(
          result == Left(
            TransactionException.CollectErrorsException(
              tx.id,
              Seq(
                CollectError.NoRedeemer(RedeemerTag.Mint, 0),
                CollectError.NoCostModel(Language.PlutusV3)
              )
            )
          ),
          result
        )
    }

    test("the spend collect errors follow the input index order for many inputs, spec [FL-10]") {
        // Eight script-locked inputs, none with a redeemer. The ledger folds the spend scripts
        // in sorted-input order (eras/alonzo/impl/src/Cardano/Ledger/Alonzo/UTxO.hs:290-296)
        // and merge adds each NoRedeemer in front, so the list is index 7 down to 0. A Set of
        // more than 4 elements is a HashSet, which loses that order.
        val (spend, state, input, _) = spendAndReference(validPlutusV3Script, isValid = true)
        val inputs = input +: Seq.fill(7)(arbitrary[TransactionInput].sample.get)
        val tx = spend
            .withBody(
              _.copy(inputs = TaggedSortedSet.from(inputs), referenceInputs = TaggedSortedSet.empty)
            )
            .withWitness(_.copy(redeemers = None))
        val utxos = state.utxos ++ inputs.map(_ -> state.utxos(input))
        val result =
            PlutusScriptsTransactionMutator.transit(noCostModels, state.copy(utxos = utxos), tx)
        assert(
          result == Left(
            TransactionException.CollectErrorsException(
              tx.id,
              (7 to 0 by -1).map(CollectError.NoRedeemer(RedeemerTag.Spend, _))
            )
          ),
          result
        )
    }

    test("the collect errors follow the ledger order of the needed scripts, spec [FL-10]") {
        // A spend, a withdrawal, a certificate, two mints, a vote and a proposal, all without a
        // redeemer. The ledger collects Spend, Reward, Cert, Mint, Voting, Proposing
        // (eras/conway/impl/src/Cardano/Ledger/Conway/UTxO.hs:64-70), in index order inside a
        // tag, and merge adds each NoRedeemer in front, so the list is the reverse.
        val (spend, state, _, _) = spendAndReference(validPlutusV3Script, isValid = true)
        val mint = invalidPlutusV3Script
        val reward = validPlutusV2Script
        val cert = validPlutusV1Script
        val account = RewardAccount(StakeAddress(Testnet, StakePayload.Script(reward.scriptHash)))
        import TransactionWitnessSet.given
        val tx = spend
            .withBody(
              _.copy(
                referenceInputs = TaggedSortedSet.empty,
                mint = Some(
                  Mint(
                    MultiAsset(
                      SortedMap(
                        mint.scriptHash -> SortedMap(AssetName.empty -> 1),
                        validPlutusV3Script.scriptHash -> SortedMap(AssetName.empty -> 1)
                      )
                    )
                  )
                ),
                certificates = TaggedOrderedStrictSet(
                  Certificate.UnregCert(Credential.ScriptHash(cert.scriptHash), None)
                ),
                withdrawals = Some(Withdrawals(SortedMap(account -> Coin.zero))),
                votingProcedures = Some(
                  VotingProcedures(
                    SortedMap(
                      Voter.DRepScript(validPlutusV3Script.scriptHash) -> SortedMap(
                        arbitrary[GovActionId].sample.get -> VotingProcedure(Vote.Yes, None)
                      )
                    )
                  )
                ),
                proposalProcedures = TaggedOrderedSet(
                  ProposalProcedure(
                    Coin.zero,
                    account,
                    GovAction.TreasuryWithdrawals(
                      Map.empty,
                      Some(validPlutusV3Script.scriptHash)
                    ),
                    arbitrary[Anchor].sample.get
                  )
                )
              )
            )
            .withWitness(ws =>
                ws.copy(
                  redeemers = None,
                  plutusV1Scripts =
                      TaggedSortedStrictMap.from[ScriptHash, Script.PlutusV1](Seq(cert)),
                  plutusV2Scripts =
                      TaggedSortedStrictMap.from[ScriptHash, Script.PlutusV2](Seq(reward)),
                  plutusV3Scripts = TaggedSortedStrictMap
                      .from[ScriptHash, Script.PlutusV3](Seq(validPlutusV3Script, mint))
                )
            )
        val result = PlutusScriptsTransactionMutator.transit(Context(), state, tx)
        assert(
          result == Left(
            TransactionException.CollectErrorsException(
              tx.id,
              Seq(
                CollectError.NoRedeemer(RedeemerTag.Proposing, 0),
                CollectError.NoRedeemer(RedeemerTag.Voting, 0),
                CollectError.NoRedeemer(RedeemerTag.Mint, 1),
                CollectError.NoRedeemer(RedeemerTag.Mint, 0),
                CollectError.NoRedeemer(RedeemerTag.Cert, 0),
                CollectError.NoRedeemer(RedeemerTag.Reward, 0),
                CollectError.NoRedeemer(RedeemerTag.Spend, 0)
              )
            )
          ),
          result
        )
    }

    private def noCostModels: Context = {
        val context = Context()
        context.copy(env =
            context.env.copy(params = context.env.params.copy(costModels = CostModels(Map.empty)))
        )
    }

    private def validPlutusV1Script = PlutusV1.alwaysOk.withErrorTraces.script

    private def validPlutusV2Script = PlutusV2.alwaysOk.withErrorTraces.script

    /** A PlutusV2 script that fails on any argument. */
    private def invalidPlutusV2Script = Script.PlutusV2(Term.Error().plutusV2.cborByteString)

    private def validPlutusV3Script = PlutusV3.alwaysOk.withErrorTraces.script

    private def invalidPlutusV3Script = {
        import scalus.*
        val program = compile((data: Data) => scalus.cardano.onchain.plutus.prelude.fail())
            .toUplc(true)
            .plutusV3
        Script.PlutusV3(program.cborByteString)
    }
}
