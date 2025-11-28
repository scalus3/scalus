package scalus.cardano.ledger.rules

import scalus.cardano.address.ByronAddress
import scalus.cardano.ledger.*

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

    private val maxBootstrapAttrsSize = 64

    override def validate(context: Context, state: State, event: Event): Result = {
        val transaction = event
        val txBody = transaction.body.value

        val outputsWithOversizedBootAttrs = txBody.outputs.filter { output =>
            output.value.address match {
                case byronAddr: ByronAddress =>
                    byronAddr.attributesSize > maxBootstrapAttrsSize
                case _ => false
            }
        }

        if outputsWithOversizedBootAttrs.nonEmpty then {
            failure(
              TransactionException.OutputBootAddrAttrsTooBigException(
                transaction.id,
                outputsWithOversizedBootAttrs.map(_.value.address).toList,
                maxBootstrapAttrsSize
              )
            )
        } else {
            success
        }
    }
}
