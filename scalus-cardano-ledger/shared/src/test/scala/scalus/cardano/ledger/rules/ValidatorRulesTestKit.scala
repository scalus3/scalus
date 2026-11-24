package scalus.cardano.ledger
package rules

import scalus.cardano.ledger.*
import scalus.uplc.builtin.Data

trait ValidatorRulesTestKit extends ArbitraryInstances, RuleTestValues {

    /** Spec `[kit.exunits.bounded-given]` in `docs/internal/test-refactor/d6-rules-kits.md`: a
      * transaction under test fails only the rule under test, not `ExUnitsTooBigValidator`.
      * `Arbitrary[Redeemers]` draws at most 3 redeemers, so their sum stays within
      * `maxTxExecutionUnits`. A test that needs the wide units takes `genExUnits` by name, spec
      * `[kit.exunits.wide-by-name]`.
      */
    given RedeemerUnits = RedeemerUnits.perTx(3)

    /** A UTxO of 2 ada at `scriptAddress(hash)`, spec `[kit.rules.script-utxo]`. It returns a pair,
      * so that `Map(scriptUtxo(...))` gives the `Utxos` of `validate`.
      */
    protected def scriptUtxo(
        input: TransactionInput,
        hash: ScriptHash,
        datum: Option[DatumOption] = None,
        scriptRef: Option[ScriptRef] = None
    ): (TransactionInput, TransactionOutput) =
        input -> TransactionOutput(scriptAddress(hash), Value.ada(2), datum, scriptRef)

    /** A transaction that spends `inputs`, with no outputs and fee 0, spec
      * `[kit.rules.tx-spending]`.
      */
    protected def txSpending(
        inputs: TransactionInput*
    )(witnessSet: TransactionWitnessSet): Transaction =
        Transaction(emptyBody.copy(inputs = TaggedSortedSet.from(inputs)), witnessSet)

    /** Runs `validator` on `Context()` and `State(utxos)`, spec `[kit.rules.validate]`. */
    protected def validate(
        validator: STS.Validator,
        utxos: Utxos,
        tx: Transaction
    ): validator.Result = validator.validate(Context(), State(utxos = utxos), tx)

    /** A transaction that mints one token with `plutusScript`, and the UTxO it spends. */
    protected def mintTx(plutusScript: PlutusScript): (Transaction, Utxos) = {
        val spent = input(0)
        // A key address: a script address would need a spending script in the transaction.
        val utxos = Map(spent -> TransactionOutput(keyAddress(0), Value.lovelace(1_000_000L)))
        val tx = Transaction(
          emptyBody.copy(
            inputs = TaggedSortedSet.from(Set(spent)),
            mint = Some(mintOne(plutusScript.scriptHash))
          ),
          TransactionWitnessSet(
            scripts = Seq(plutusScript),
            redeemers = Some(Redeemers(Redeemer(RedeemerTag.Mint, 0, Data.unit, ExUnits.zero))),
            vkeyWitnesses = Set.empty,
            plutusData = Seq.empty
          )
        )
        (tx, utxos)
    }
}
