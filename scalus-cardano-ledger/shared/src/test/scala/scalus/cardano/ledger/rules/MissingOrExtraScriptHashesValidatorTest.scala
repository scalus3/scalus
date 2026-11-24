package scalus.cardano.ledger
package rules

import org.scalatest.funsuite.AnyFunSuite
import scalus.uplc.builtin.ByteString

import scala.collection.immutable.SortedMap

class MissingOrExtraScriptHashesValidatorTest extends AnyFunSuite, ValidatorRulesTestKit {

    // The rule reads only the hash of a script, so any fixed bytes serve.
    private val plutusScript = Script.PlutusV1(ByteString.fromHex("01"))
    private val refScript = Some(ScriptRef(plutusScript))

    // Each body field below needs `plutusScript`.

    private val mint = Some(mintOne(plutusScript.scriptHash))

    private val votingProcedures = Some(
      // Both kinds of script voter, so that each reaches `Voter.scriptHashOption`.
      VotingProcedures(
        SortedMap(
          Voter.ConstitutionalCommitteeHotScript(plutusScript.scriptHash) -> SortedMap.empty,
          Voter.DRepScript(plutusScript.scriptHash) -> SortedMap.empty
        )
      )
    )

    private val withdrawals = Some(
      Withdrawals(SortedMap(scriptRewardAccount(plutusScript.scriptHash) -> Coin.zero))
    )

    private val proposalProcedures: TaggedOrderedSet[ProposalProcedure] = TaggedOrderedSet(
      proposal(
        GovAction.TreasuryWithdrawals(Map.empty, Some(plutusScript.scriptHash)),
        rewardAccount(1)
      )
    )

    private val certificates: TaggedOrderedStrictSet[Certificate] = TaggedOrderedStrictSet(
      Certificate.UnregCert(Credential.ScriptHash(plutusScript.scriptHash), None)
    )

    /** A UTxO at a key address: it needs no script, and it can only provide `scriptRef`. */
    private def keyUtxo(
        input: TransactionInput,
        scriptRef: Option[ScriptRef]
    ): (TransactionInput, TransactionOutput) =
        input -> TransactionOutput(keyAddress(1), Value.ada(1), None, scriptRef)

    private def witnesses(scripts: Script*): TransactionWitnessSet =
        TransactionWitnessSet(
          scripts = scripts,
          redeemers = None,
          vkeyWitnesses = Set.empty,
          plutusData = Seq.empty
        )

    test("MissingOrExtraScriptHashesValidator success with no scripts") {
        val transaction = txSpending()(witnesses())
        assert(validate(MissingOrExtraScriptHashesValidator, Map.empty, transaction).isRight)
    }

    test("MissingOrExtraScriptHashesValidator success for inputs in witnesses") {
        val spent = input(0)
        val utxos = Map(scriptUtxo(spent, plutusScript.scriptHash))
        val transaction = txSpending(spent)(witnesses(plutusScript))
        assert(validate(MissingOrExtraScriptHashesValidator, utxos, transaction).isRight)
    }

    test("MissingOrExtraScriptHashesValidator failure with missing inputs in witnesses") {
        val spent = input(0)
        val utxos = Map(scriptUtxo(spent, plutusScript.scriptHash))
        val transaction = txSpending(spent)(witnesses())
        assert(validate(MissingOrExtraScriptHashesValidator, utxos, transaction).isLeft)
    }

    test("MissingOrExtraScriptHashesValidator success for Mint in witnesses") {
        val transaction = Transaction(emptyBody.copy(mint = mint), witnesses(plutusScript))
        assert(validate(MissingOrExtraScriptHashesValidator, Map.empty, transaction).isRight)
    }

    test("MissingOrExtraScriptHashesValidator failure with missing Mint in witnesses") {
        val transaction = Transaction(emptyBody.copy(mint = mint), witnesses())
        assert(validate(MissingOrExtraScriptHashesValidator, Map.empty, transaction).isLeft)
    }

    test("MissingOrExtraScriptHashesValidator success for Voting in witnesses") {
        val transaction = Transaction(
          emptyBody.copy(votingProcedures = votingProcedures),
          witnesses(plutusScript)
        )
        assert(validate(MissingOrExtraScriptHashesValidator, Map.empty, transaction).isRight)
    }

    test("MissingOrExtraScriptHashesValidator failure with missing Voting in witnesses") {
        val transaction =
            Transaction(emptyBody.copy(votingProcedures = votingProcedures), witnesses())
        assert(validate(MissingOrExtraScriptHashesValidator, Map.empty, transaction).isLeft)
    }

    test("MissingOrExtraScriptHashesValidator success for Withdrawals in witnesses") {
        val transaction =
            Transaction(emptyBody.copy(withdrawals = withdrawals), witnesses(plutusScript))
        assert(validate(MissingOrExtraScriptHashesValidator, Map.empty, transaction).isRight)
    }

    test("MissingOrExtraScriptHashesValidator failure with missing Withdrawals in witnesses") {
        val transaction = Transaction(emptyBody.copy(withdrawals = withdrawals), witnesses())
        assert(validate(MissingOrExtraScriptHashesValidator, Map.empty, transaction).isLeft)
    }

    test("MissingOrExtraScriptHashesValidator success for Proposal Procedures in witnesses") {
        val transaction = Transaction(
          emptyBody.copy(proposalProcedures = proposalProcedures),
          witnesses(plutusScript)
        )
        assert(validate(MissingOrExtraScriptHashesValidator, Map.empty, transaction).isRight)
    }

    test(
      "MissingOrExtraScriptHashesValidator failure with missing Proposal Procedures in witnesses"
    ) {
        val transaction =
            Transaction(emptyBody.copy(proposalProcedures = proposalProcedures), witnesses())
        assert(validate(MissingOrExtraScriptHashesValidator, Map.empty, transaction).isLeft)
    }

    test("MissingOrExtraScriptHashesValidator success for Certificates in witnesses") {
        val transaction =
            Transaction(emptyBody.copy(certificates = certificates), witnesses(plutusScript))
        assert(validate(MissingOrExtraScriptHashesValidator, Map.empty, transaction).isRight)
    }

    test("MissingOrExtraScriptHashesValidator failure with missing Certificates in witnesses") {
        val transaction = Transaction(emptyBody.copy(certificates = certificates), witnesses())
        assert(validate(MissingOrExtraScriptHashesValidator, Map.empty, transaction).isLeft)
    }

    test("MissingOrExtraScriptHashesValidator failure with extra in witnesses") {
        val transaction = txSpending()(witnesses(plutusScript))
        assert(validate(MissingOrExtraScriptHashesValidator, Map.empty, transaction).isLeft)
    }

    test("MissingOrExtraScriptHashesValidator success for Inputs with reference script in inputs") {
        val spent = input(0)
        val utxos = Map(scriptUtxo(spent, plutusScript.scriptHash, scriptRef = refScript))
        val transaction = txSpending(spent)(witnesses())
        assert(validate(MissingOrExtraScriptHashesValidator, utxos, transaction).isRight)
    }

    test(
      "MissingOrExtraScriptHashesValidator success for Inputs with reference script in referenceInputs"
    ) {
        val spent = input(0)
        val refInput = input(1)
        val utxos = Map(scriptUtxo(spent, plutusScript.scriptHash), keyUtxo(refInput, refScript))
        val transaction = txSpending(spent)(witnesses()).withBody(
          _.copy(referenceInputs = TaggedSortedSet(refInput))
        )
        assert(validate(MissingOrExtraScriptHashesValidator, utxos, transaction).isRight)
    }

    test("MissingOrExtraScriptHashesValidator success for Mint with reference script in inputs") {
        val spent = input(0)
        val utxos = Map(keyUtxo(spent, refScript))
        val transaction = txSpending(spent)(witnesses()).withBody(_.copy(mint = mint))
        assert(validate(MissingOrExtraScriptHashesValidator, utxos, transaction).isRight)
    }

    test(
      "MissingOrExtraScriptHashesValidator success for Mint with reference script in referenceInputs"
    ) {
        val refInput = input(1)
        val utxos = Map(keyUtxo(refInput, refScript))
        val transaction = Transaction(
          emptyBody.copy(referenceInputs = TaggedSortedSet(refInput), mint = mint),
          witnesses()
        )
        assert(validate(MissingOrExtraScriptHashesValidator, utxos, transaction).isRight)
    }

    test("MissingOrExtraScriptHashesValidator success for Voting with reference script in inputs") {
        val spent = input(0)
        val utxos = Map(keyUtxo(spent, refScript))
        val transaction =
            txSpending(spent)(witnesses()).withBody(_.copy(votingProcedures = votingProcedures))
        assert(validate(MissingOrExtraScriptHashesValidator, utxos, transaction).isRight)
    }

    test(
      "MissingOrExtraScriptHashesValidator success for Voting with reference script in referenceInputs"
    ) {
        val refInput = input(1)
        val utxos = Map(keyUtxo(refInput, refScript))
        val transaction = Transaction(
          emptyBody.copy(
            referenceInputs = TaggedSortedSet(refInput),
            votingProcedures = votingProcedures
          ),
          witnesses()
        )
        assert(validate(MissingOrExtraScriptHashesValidator, utxos, transaction).isRight)
    }

    test(
      "MissingOrExtraScriptHashesValidator success for Withdrawals with reference script in inputs"
    ) {
        val spent = input(0)
        val utxos = Map(keyUtxo(spent, refScript))
        val transaction = txSpending(spent)(witnesses()).withBody(_.copy(withdrawals = withdrawals))
        assert(validate(MissingOrExtraScriptHashesValidator, utxos, transaction).isRight)
    }

    test(
      "MissingOrExtraScriptHashesValidator success for Withdrawals with reference script in referenceInputs"
    ) {
        val refInput = input(1)
        val utxos = Map(keyUtxo(refInput, refScript))
        val transaction = Transaction(
          emptyBody.copy(referenceInputs = TaggedSortedSet(refInput), withdrawals = withdrawals),
          witnesses()
        )
        assert(validate(MissingOrExtraScriptHashesValidator, utxos, transaction).isRight)
    }

    test(
      "MissingOrExtraScriptHashesValidator success for Proposal Procedures with reference script in inputs"
    ) {
        val spent = input(0)
        val utxos = Map(keyUtxo(spent, refScript))
        val transaction =
            txSpending(spent)(witnesses()).withBody(_.copy(proposalProcedures = proposalProcedures))
        assert(validate(MissingOrExtraScriptHashesValidator, utxos, transaction).isRight)
    }

    test(
      "MissingOrExtraScriptHashesValidator success for Proposal Procedures with reference script in referenceInputs"
    ) {
        val refInput = input(1)
        val utxos = Map(keyUtxo(refInput, refScript))
        val transaction = Transaction(
          emptyBody.copy(
            referenceInputs = TaggedSortedSet(refInput),
            proposalProcedures = proposalProcedures
          ),
          witnesses()
        )
        assert(validate(MissingOrExtraScriptHashesValidator, utxos, transaction).isRight)
    }

    test(
      "MissingOrExtraScriptHashesValidator success for Certificates with reference script in inputs"
    ) {
        val spent = input(0)
        val utxos = Map(keyUtxo(spent, refScript))
        val transaction =
            txSpending(spent)(witnesses()).withBody(_.copy(certificates = certificates))
        assert(validate(MissingOrExtraScriptHashesValidator, utxos, transaction).isRight)
    }

    test(
      "MissingOrExtraScriptHashesValidator success for Certificates with reference script in referenceInputs"
    ) {
        val refInput = input(1)
        val utxos = Map(keyUtxo(refInput, refScript))
        val transaction = Transaction(
          emptyBody.copy(referenceInputs = TaggedSortedSet(refInput), certificates = certificates),
          witnesses()
        )
        assert(validate(MissingOrExtraScriptHashesValidator, utxos, transaction).isRight)
    }

    test(
      "MissingOrExtraScriptHashesValidator failure for Inputs with missing reference script in inputs"
    ) {
        val spent = input(0)
        val utxos = Map(scriptUtxo(spent, plutusScript.scriptHash, scriptRef = None))
        val transaction = txSpending(spent)(witnesses())
        assert(validate(MissingOrExtraScriptHashesValidator, utxos, transaction).isLeft)
    }

    test(
      "MissingOrExtraScriptHashesValidator failure for Inputs with missing reference script in referenceInputs"
    ) {
        val spent = input(0)
        val refInput = input(1)
        val utxos = Map(scriptUtxo(spent, plutusScript.scriptHash), keyUtxo(refInput, None))
        val transaction = txSpending(spent)(witnesses()).withBody(
          _.copy(referenceInputs = TaggedSortedSet(refInput))
        )
        assert(validate(MissingOrExtraScriptHashesValidator, utxos, transaction).isLeft)
    }

    test(
      "MissingOrExtraScriptHashesValidator failure for Mint with missing reference script in inputs"
    ) {
        val spent = input(0)
        val utxos = Map(keyUtxo(spent, None))
        val transaction = txSpending(spent)(witnesses()).withBody(_.copy(mint = mint))
        assert(validate(MissingOrExtraScriptHashesValidator, utxos, transaction).isLeft)
    }

    test(
      "MissingOrExtraScriptHashesValidator failure for Mint with missing reference script in referenceInputs"
    ) {
        val refInput = input(1)
        val utxos = Map(keyUtxo(refInput, None))
        val transaction = Transaction(
          emptyBody.copy(referenceInputs = TaggedSortedSet(refInput), mint = mint),
          witnesses()
        )
        assert(validate(MissingOrExtraScriptHashesValidator, utxos, transaction).isLeft)
    }

    test(
      "MissingOrExtraScriptHashesValidator failure for Voting with missing reference script in inputs"
    ) {
        val spent = input(0)
        val utxos = Map(keyUtxo(spent, None))
        val transaction =
            txSpending(spent)(witnesses()).withBody(_.copy(votingProcedures = votingProcedures))
        assert(validate(MissingOrExtraScriptHashesValidator, utxos, transaction).isLeft)
    }

    test(
      "MissingOrExtraScriptHashesValidator failure for Voting with missing reference script in referenceInputs"
    ) {
        val refInput = input(1)
        val utxos = Map(keyUtxo(refInput, None))
        val transaction = Transaction(
          emptyBody.copy(
            referenceInputs = TaggedSortedSet(refInput),
            votingProcedures = votingProcedures
          ),
          witnesses()
        )
        assert(validate(MissingOrExtraScriptHashesValidator, utxos, transaction).isLeft)
    }

    test(
      "MissingOrExtraScriptHashesValidator failure for Withdrawals with missing reference script in inputs"
    ) {
        val spent = input(0)
        val utxos = Map(keyUtxo(spent, None))
        val transaction = txSpending(spent)(witnesses()).withBody(_.copy(withdrawals = withdrawals))
        assert(validate(MissingOrExtraScriptHashesValidator, utxos, transaction).isLeft)
    }

    test(
      "MissingOrExtraScriptHashesValidator failure for Withdrawals with missing reference script in referenceInputs"
    ) {
        val refInput = input(1)
        val utxos = Map(keyUtxo(refInput, None))
        val transaction = Transaction(
          emptyBody.copy(referenceInputs = TaggedSortedSet(refInput), withdrawals = withdrawals),
          witnesses()
        )
        assert(validate(MissingOrExtraScriptHashesValidator, utxos, transaction).isLeft)
    }

    test(
      "MissingOrExtraScriptHashesValidator failure for Proposal Procedures with missing reference script in inputs"
    ) {
        val spent = input(0)
        val utxos = Map(keyUtxo(spent, None))
        val transaction =
            txSpending(spent)(witnesses()).withBody(_.copy(proposalProcedures = proposalProcedures))
        assert(validate(MissingOrExtraScriptHashesValidator, utxos, transaction).isLeft)
    }

    test(
      "MissingOrExtraScriptHashesValidator failure for Proposal Procedures with missing reference script in referenceInputs"
    ) {
        val refInput = input(1)
        val utxos = Map(keyUtxo(refInput, None))
        val transaction = Transaction(
          emptyBody.copy(
            referenceInputs = TaggedSortedSet(refInput),
            proposalProcedures = proposalProcedures
          ),
          witnesses()
        )
        assert(validate(MissingOrExtraScriptHashesValidator, utxos, transaction).isLeft)
    }

    test(
      "MissingOrExtraScriptHashesValidator failure for Certificates with missing reference script in inputs"
    ) {
        val spent = input(0)
        val utxos = Map(keyUtxo(spent, None))
        val transaction =
            txSpending(spent)(witnesses()).withBody(_.copy(certificates = certificates))
        assert(validate(MissingOrExtraScriptHashesValidator, utxos, transaction).isLeft)
    }

    test(
      "MissingOrExtraScriptHashesValidator failure for Certificates with missing reference script in referenceInputs"
    ) {
        val refInput = input(1)
        val utxos = Map(keyUtxo(refInput, None))
        val transaction = Transaction(
          emptyBody.copy(referenceInputs = TaggedSortedSet(refInput), certificates = certificates),
          witnesses()
        )
        assert(validate(MissingOrExtraScriptHashesValidator, utxos, transaction).isLeft)
    }
}
