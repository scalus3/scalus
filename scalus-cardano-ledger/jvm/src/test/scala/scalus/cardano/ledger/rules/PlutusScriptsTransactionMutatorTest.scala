package scalus.cardano.ledger
package rules

import org.scalacheck.Arbitrary.arbitrary
import org.scalatest.funsuite.AnyFunSuite
import scalus.builtin.Data
import scalus.cardano.address.*
import scalus.cardano.address.Network.Testnet

import scala.collection.immutable.SortedMap

class PlutusScriptsTransactionMutatorTest extends AnyFunSuite, ValidatorRulesTestKit {

    test(
      "PlutusScriptsTransactionMutator success with isValid = true and valid scripts for Plutus V1"
    ) {
        val script = validPlutusV1Script

        val input = arbitrary[TransactionInput].sample.get
        val datum = arbitrary[Data].sample.get

        val output = TransactionOutput(
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
                    data = scalus.builtin.Data.unit,
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
            input -> TransactionOutput(
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

        val output = TransactionOutput(
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
                    data = scalus.builtin.Data.unit,
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
            input -> TransactionOutput(
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

        val output = TransactionOutput(
          address = Address(Testnet, Credential.ScriptHash(script.scriptHash)),
          value = Value.lovelace(100000),
        )

        val transaction = Transaction(
          body = KeepRaw(
            TransactionBody(
              inputs = TaggedSortedSet(input),
              outputs = IndexedSeq(Sized(output)),
              fee = Coin(1000L),
              donation = Some(Coin(500L)),
              currentTreasuryValue = Some(Coin(200000L))
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
                    data = scalus.builtin.Data.unit,
                    exUnits = ExUnits(100000000, 100000000)
                  )
                )
              )
            ),
            vkeyWitnesses = Set.empty,
            plutusData = Seq(datum)
          ),
          isValid = true,
          auxiliaryData = None
        )

        val state = State(
          utxos = Map(
            input -> TransactionOutput(
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
                  TransactionOutput(
                    address = arbitrary[Address].sample.get,
                    value = Value.lovelace(80000)
                  )
                )
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
                    data = scalus.builtin.Data.unit,
                    exUnits = ExUnits(100000000, 100000000)
                  )
                )
              )
            ),
            vkeyWitnesses = Set.empty,
            plutusData = Seq(datum)
          ),
          isValid = false,
          auxiliaryData = None
        )

        val state = State(
          utxos = Map(
            input -> TransactionOutput(
              address = Address(Testnet, Credential.ScriptHash(script.scriptHash)),
              value = Value.zero,
              datumHash = DataHash.fromByteString(datum.dataHash)
            ),
            collateralInput -> TransactionOutput(
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

    test("PlutusScriptsTransactionMutator success with isValid = true and empty scripts") {
        val transaction = Transaction(
          body = KeepRaw(
            TransactionBody(
              inputs = TaggedSortedSet.empty,
              outputs = IndexedSeq.empty,
              fee = Coin(0),
            )
          ),
          witnessSet = TransactionWitnessSet.empty,
          isValid = true,
          auxiliaryData = None
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
          witnessSet = TransactionWitnessSet(
            scripts = Seq(script),
            redeemers = Some(
              Redeemers.Array(
                IndexedSeq(
                  Redeemer(
                    tag = RedeemerTag.Spend,
                    index = 0,
                    data = scalus.builtin.Data.unit,
                    exUnits = ExUnits(100000000, 100000000)
                  )
                )
              )
            ),
            vkeyWitnesses = Set.empty,
            plutusData = Seq(datum)
          ),
          isValid = false,
          auxiliaryData = None
        )

        val state = State(
          utxos = Map(
            input -> TransactionOutput(
              address = Address(Testnet, Credential.ScriptHash(script.scriptHash)),
              value = Value.zero,
              datumHash = DataHash.fromByteString(datum.dataHash)
            )
          )
        )

        val result = PlutusScriptsTransactionMutator.transit(Context(), state, transaction)
        assert(result.isLeft, result)
    }

    test("PlutusScriptsTransactionMutator failure with isValid = true and invalid scripts") {
        val script = invalidPlutusV3Script

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
          witnessSet = TransactionWitnessSet(
            scripts = Seq(script),
            redeemers = Some(
              Redeemers.Array(
                IndexedSeq(
                  Redeemer(
                    tag = RedeemerTag.Spend,
                    index = 0,
                    data = scalus.builtin.Data.unit,
                    exUnits = ExUnits(100000000, 100000000)
                  )
                )
              )
            ),
            vkeyWitnesses = Set.empty,
            plutusData = Seq(datum)
          ),
          isValid = true,
          auxiliaryData = None
        )

        val state = State(
          utxos = Map(
            input -> TransactionOutput(
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

        val output = TransactionOutput(
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
                arbitrary[SortedMap[Voter, SortedMap[GovActionId, VotingProcedure]]].sample.get
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
                    data = scalus.builtin.Data.unit,
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
            input -> TransactionOutput(
              address = Address(Testnet, Credential.ScriptHash(script.scriptHash)),
              value = Value.zero,
              datumHash = DataHash.fromByteString(datum.dataHash)
            )
          )
        )

        val result = PlutusScriptsTransactionMutator.transit(Context(), state, tx)
        assert(result.isLeft)
    }

    test("PlutusV1 rejects transaction with proposal procedures") {
        val script = validPlutusV1Script

        val input = arbitrary[TransactionInput].sample.get
        val datum = arbitrary[Data].sample.get

        val output = TransactionOutput(
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
                    data = scalus.builtin.Data.unit,
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
            input -> TransactionOutput(
              address = Address(Testnet, Credential.ScriptHash(script.scriptHash)),
              value = Value.zero,
              datumHash = DataHash.fromByteString(datum.dataHash)
            )
          )
        )

        val result = PlutusScriptsTransactionMutator.transit(Context(), state, tx)
        assert(result.isLeft)
    }

    test("PlutusV1 rejects transaction with non-zero donation") {
        val script = validPlutusV1Script

        val input = arbitrary[TransactionInput].sample.get
        val datum = arbitrary[Data].sample.get

        val output = TransactionOutput(
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
                    data = scalus.builtin.Data.unit,
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
            input -> TransactionOutput(
              address = Address(Testnet, Credential.ScriptHash(script.scriptHash)),
              value = Value.zero,
              datumHash = DataHash.fromByteString(datum.dataHash)
            )
          )
        )

        val result = PlutusScriptsTransactionMutator.transit(Context(), state, tx)
        assert(result.isLeft)
    }

    test("PlutusV1 rejects transaction with current treasury value") {
        val script = validPlutusV1Script

        val input = arbitrary[TransactionInput].sample.get
        val datum = arbitrary[Data].sample.get

        val output = TransactionOutput(
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
                    data = scalus.builtin.Data.unit,
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
            input -> TransactionOutput(
              address = Address(Testnet, Credential.ScriptHash(script.scriptHash)),
              value = Value.zero,
              datumHash = DataHash.fromByteString(datum.dataHash)
            )
          )
        )

        val result = PlutusScriptsTransactionMutator.transit(Context(), state, tx)
        assert(result.isLeft)
    }

    // ============ Conway Features Guard Tests for Plutus V2 ============

    test("PlutusV2 rejects transaction with voting procedures") {
        val script = validPlutusV2Script

        val input = arbitrary[TransactionInput].sample.get
        val datum = arbitrary[Data].sample.get

        val output = TransactionOutput(
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
                arbitrary[SortedMap[Voter, SortedMap[GovActionId, VotingProcedure]]].sample.get
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
                    data = scalus.builtin.Data.unit,
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
            input -> TransactionOutput(
              address = Address(Testnet, Credential.ScriptHash(script.scriptHash)),
              value = Value.zero,
              datumHash = DataHash.fromByteString(datum.dataHash)
            )
          )
        )

        val result = PlutusScriptsTransactionMutator.transit(Context(), state, tx)
        assert(result.isLeft)
    }

    test("PlutusV2 rejects transaction with proposal procedures") {
        val script = validPlutusV2Script

        val input = arbitrary[TransactionInput].sample.get
        val datum = arbitrary[Data].sample.get

        val output = TransactionOutput(
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
                    data = scalus.builtin.Data.unit,
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
            input -> TransactionOutput(
              address = Address(Testnet, Credential.ScriptHash(script.scriptHash)),
              value = Value.zero,
              datumHash = DataHash.fromByteString(datum.dataHash)
            )
          )
        )

        val result = PlutusScriptsTransactionMutator.transit(Context(), state, tx)
        assert(result.isLeft)
    }

    test("PlutusV2 rejects transaction with non-zero donation") {
        val script = validPlutusV2Script

        val input = arbitrary[TransactionInput].sample.get
        val datum = arbitrary[Data].sample.get

        val output = TransactionOutput(
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
                    data = scalus.builtin.Data.unit,
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
            input -> TransactionOutput(
              address = Address(Testnet, Credential.ScriptHash(script.scriptHash)),
              value = Value.zero,
              datumHash = DataHash.fromByteString(datum.dataHash)
            )
          )
        )

        val result = PlutusScriptsTransactionMutator.transit(Context(), state, tx)
        assert(result.isLeft)
    }

    test("PlutusV2 rejects transaction with current treasury value") {
        val script = validPlutusV2Script

        val input = arbitrary[TransactionInput].sample.get
        val datum = arbitrary[Data].sample.get

        val output = TransactionOutput(
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
                    data = scalus.builtin.Data.unit,
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
            input -> TransactionOutput(
              address = Address(Testnet, Credential.ScriptHash(script.scriptHash)),
              value = Value.zero,
              datumHash = DataHash.fromByteString(datum.dataHash)
            )
          )
        )

        val result = PlutusScriptsTransactionMutator.transit(Context(), state, tx)
        assert(result.isLeft)
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
                    data = scalus.builtin.Data.unit,
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
            input -> TransactionOutput(
              address = Address(Testnet, Credential.ScriptHash(script.scriptHash)),
              value = Value.zero,
              datumOption = DatumOption.Inline(datum)
            )
          )
        )

        val result = PlutusScriptsTransactionMutator.transit(Context(), state, tx)
        assert(result.isLeft, "PlutusV1 should reject transaction with inline datum in inputs")
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
                    data = scalus.builtin.Data.unit,
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
            input -> TransactionOutput(
              address = Address(Testnet, Credential.ScriptHash(script.scriptHash)),
              value = Value.zero,
              datumOption = DatumOption.Hash(DataHash.fromByteString(datum.dataHash))
            ),
            referenceInput -> TransactionOutput(
              address = Address(Testnet, Credential.ScriptHash(script.scriptHash)),
              value = Value.zero,
              datumOption = DatumOption.Inline(datum)
            )
          )
        )

        val result = PlutusScriptsTransactionMutator.transit(Context(), state, tx)
        assert(
          result.isLeft,
          "PlutusV1 should reject transaction with inline datum in reference inputs"
        )
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
                    data = scalus.builtin.Data.unit,
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
            input -> TransactionOutput(
              address = Address(Testnet, Credential.ScriptHash(script.scriptHash)),
              value = Value.zero,
              datumHash = DataHash.fromByteString(datum.dataHash)
            )
          )
        )

        val result = PlutusScriptsTransactionMutator.transit(Context(), state, tx)
        assert(result.isLeft, "PlutusV1 should reject transaction with inline datum in output")
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
                    data = scalus.builtin.Data.unit,
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
        assert(result.isLeft, "PlutusV1 should reject transaction with Byron address in inputs")
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
                    data = scalus.builtin.Data.unit,
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
        assert(
          result.isLeft,
          "PlutusV1 should reject transaction with Byron address in reference inputs"
        )
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
                    data = scalus.builtin.Data.unit,
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
        assert(result.isLeft, "PlutusV1 should reject transaction with Byron address in output")
    }

    private def validPlutusV1Script = {
        import scalus.*
        val program = scalus.Compiler
            .compile((data1: Data, data2: Data, data3: Data) => ())
            .toUplc(true)
            .plutusV1
        Script.PlutusV1(program.cborByteString)
    }

    private def validPlutusV2Script = {
        import scalus.*
        val program = scalus.Compiler
            .compile((data1: Data, data2: Data, data3: Data) => ())
            .toUplc(true)
            .plutusV2
        Script.PlutusV2(program.cborByteString)
    }

    private def validPlutusV3Script = {
        import scalus.*
        val program = scalus.Compiler.compile((data: Data) => ()).toUplc(true).plutusV3
        Script.PlutusV3(program.cborByteString)
    }

    private def invalidPlutusV3Script = {
        import scalus.*
        val program =
            scalus.Compiler.compile((data: Data) => scalus.prelude.fail()).toUplc(true).plutusV3
        Script.PlutusV3(program.cborByteString)
    }
}
