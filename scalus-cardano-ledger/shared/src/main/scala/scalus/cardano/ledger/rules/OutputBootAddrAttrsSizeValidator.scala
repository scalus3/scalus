package scalus.cardano.ledger
package rules

import scalus.cardano.address.{Address, ByronAddress}

// validateOutputBootAddrAttrsTooBig in cardano-ledger
//
// Bootstrap (i.e. Byron) addresses have variable sized attributes in them.
// It is important to limit their overall size.
//
// The attributes size is calculated as: derivationPathLength + unknownAttributesLength
// where derivation path is key 1 and unknown attributes are any keys other than 1 (derivation path)
// and 2 (network magic). Network magic is NOT counted in the size.
object OutputBootAddrAttrsSizeValidator extends STS.Validator {
    override final type Error = TransactionException.OutputBootAddrAttrsTooBigException

    override def validate(context: Context, state: State, event: Event): Result = {
        val transaction = event
        val outputs = transaction.body.value.outputs

        val outputsWithOversizedBootAttrs = findOutputsWithOversizedBootAttrs(outputs)

        if outputsWithOversizedBootAttrs.nonEmpty then
            failure(
              TransactionException.OutputBootAddrAttrsTooBigException(
                transaction.id,
                outputsWithOversizedBootAttrs,
                maxBootstrapAttrsSize
              )
            )
        else success
    }

    def findOutputsWithOversizedBootAttrs(
        outputs: IndexedSeq[Sized[TransactionOutput]]
    ): IndexedSeq[Address] = {
        outputs.flatMap { case SizedValue(output) =>
            output.address match
                case byronAddr: ByronAddress if byronAddr.attributesSize > maxBootstrapAttrsSize =>
                    Some(output.address)
                case _ => None
        }
    }

    private val maxBootstrapAttrsSize = 64
}
