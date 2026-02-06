package scalus.cardano.ledger
package rules

import scalus.cardano.ledger.utils.TraitObjectScanner

// WIP: It's a composition of Cardano ledger rules that mutate the state of the L1 ledger
object CardanoMutator extends STS.Mutator {
    override final type Error = TransactionException

    override def transit(context: Context, state: State, event: Event): Result = {
        STS.Mutator
            .transit[Error](DefaultValidators.all, DefaultMutators.all, context, state, event)
    }

    private val packageName = getClass.getPackage.getName

    @deprecated("Use `DefaultValidators.all` instead", "0.14")
    val allValidators: Map[String, STS.Validator] =
        TraitObjectScanner
            .findImplementors[STS.Validator](packageName)
            .view
            .map(v => v.name -> v)
            .toMap

    @deprecated("Use `DefaultMutators.all` instead.", "0.14")
    val allMutators: Map[String, STS.Mutator] =
        TraitObjectScanner
            .findImplementors[STS.Mutator](packageName)
            .view
            .filter(_.name != this.name) // Exclude self to prevent infinite recursion
            .map(v => v.name -> v)
            .toMap

    val defaultSTSs: Map[String, STS] =
        (DefaultValidators.all.toSeq ++ DefaultMutators.all.toSeq).map(v => v.name -> v).toMap

    @deprecated("Use `defaultSTSs` instead", "0.14")
    def allSTSs: Map[String, STS] = defaultSTSs
}
