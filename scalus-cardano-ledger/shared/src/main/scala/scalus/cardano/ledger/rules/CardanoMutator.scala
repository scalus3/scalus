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
        TraitObjectScanner.findImplementors[STS.Validator](packageName)

    val allMutators: Map[String, STS.Mutator] =
        TraitObjectScanner.findImplementors[STS.Mutator](packageName) - this.name

    val allSTSs: Map[String, STS] = allValidators ++ allMutators
}
