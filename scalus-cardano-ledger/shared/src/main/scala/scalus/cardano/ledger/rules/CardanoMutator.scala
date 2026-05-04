package scalus.cardano.ledger
package rules

// WIP: It's a composition of Cardano ledger rules that mutate the state of the L1 ledger
object CardanoMutator extends STS.Mutator {
    override final type Error = TransactionException

    override def transit(context: Context, state: State, event: Event): Result = {
        STS.Mutator
            .transit[Error](DefaultValidators.all, DefaultMutators.all, context, state, event)
    }

    val defaultSTSs: Map[String, STS] =
        (DefaultValidators.all.toSeq ++ DefaultMutators.all.toSeq).map(v => v.name -> v).toMap
}
