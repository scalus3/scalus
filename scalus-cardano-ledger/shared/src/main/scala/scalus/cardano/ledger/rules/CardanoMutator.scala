package scalus.cardano.ledger
package rules

import scalus.cardano.ledger.utils.TraitObjectScanner

// WIP: It's a composition of Cardano ledger rules that mutate the state of the L1 ledger
object CardanoMutator extends STS.Mutator {
    override final type Error = TransactionException

    override def transit(context: Context, state: State, event: Event): Result = {
        STS.Mutator.transit[Error](allValidators.values, allMutators.values, context, state, event)
    }

    private val packageName = getClass.getPackage.getName

    val allValidators: Map[String, STS.Validator] =
        TraitObjectScanner
            .findImplementors[STS.Validator](packageName)
            .view
            .map(v => v.name -> v)
            .toMap

    val allMutators: Map[String, STS.Mutator] = {
        // FIXME: The TraitObjectScanner is non-deterministic; it can return the mutators in different orders.
        // The mutators aren't commutative, so this is an issue.
//        TraitObjectScanner
//            .findImplementors[STS.Mutator](packageName)
//            .view
//            .filter(_.name != this.name) // Exclude self to prevent infinite recursion
//            .map(v => v.name -> v)
//            .toMap
        // QUESTION: Should we add the outputs before or after running the plutus scripts transaction mutator?
        // Does it matter?
        Map(
          "AddOutputsToUtxoMutator" -> AddOutputsToUtxoMutator,
          "PlutusScriptsTransactionMutator" -> PlutusScriptsTransactionMutator,
          "FeeMutator" -> FeeMutator,
          "RemoveInputsFromUtxoMutator" -> RemoveInputsFromUtxoMutator
        )
    }

    val allSTSs: Map[String, STS] = allValidators ++ allMutators
}
