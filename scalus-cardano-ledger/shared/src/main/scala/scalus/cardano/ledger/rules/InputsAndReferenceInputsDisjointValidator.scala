package scalus.cardano.ledger
package rules

// inputs and refInputs are disjoint at protocol versions 9 and 10
// It's Babbage.disjointRefInputs in cardano-ledger
// {- inputs ∩ refInputs = ∅ -}
object InputsAndReferenceInputsDisjointValidator extends STS.Validator {
    override final type Error = TransactionException.NonDisjointInputsAndReferenceInputsException

    override def validate(context: Context, state: State, event: Event): Result = {
//        ( pvMajor (pp ^. ppProtocolVersionL) > eraProtVerHigh @BabbageEra
//            && pvMajor (pp ^. ppProtocolVersionL) < natVersion @11
//        )
        val protocolVersionMajor = context.env.params.protocolVersion.major
        if protocolVersionMajor <= babbageProtVerHigh || protocolVersionMajor >= protocolVersionMajor11
        then return success

        val transactionId = event.id
        val body = event.body.value

        val inputs = body.inputs
        val referenceInputs = body.referenceInputs
        val intersection = inputs.toSet.intersect(referenceInputs.toSet)

        if intersection.nonEmpty then
            return failure(
              TransactionException
                  .NonDisjointInputsAndReferenceInputsException(transactionId, intersection)
            )

        success
    }

    // ProtVerHigh BabbageEra in cardano-ledger: the check starts at PV 9
    private val babbageProtVerHigh = 8
    private val protocolVersionMajor11 = 11
}
