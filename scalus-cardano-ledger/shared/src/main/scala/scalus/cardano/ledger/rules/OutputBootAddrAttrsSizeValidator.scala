package scalus.cardano.ledger
package rules

import scalus.cardano.address.{Address, ByronAddress}

// validateOutputBootAddrAttrsTooBig in cardano-ledger
//
// Bootstrap (i.e. Byron) addresses have variable sized attributes in them.
// It is important to limit their overall size.
//
// The attributes size is `ByronAddress.attributesSize`, the ledger's `bootstrapAddressAttrsSize`
// (libs/cardano-ledger-core/src/Cardano/Ledger/Address.hs:334-340): the HD payload length of
// attribute 1 plus the bytes of the unknown attributes (keys other than 1 and 2). The network
// magic (attribute 2) is not counted.
object OutputBootAddrAttrsSizeValidator extends STS.Validator {
    override final type Error = TransactionException.OutputBootAddrAttrsTooBigException

    override def validate(context: Context, state: State, event: Event): Result = {
        val transaction = event
        val body = transaction.body.value
        // spec [TX-4]: `allSizedOutputsTxBodyF`, the outputs and the collateral return
        // (B/Rules/Utxo.hs:406-414)
        val outputs = body.outputs ++ body.collateralReturnOutput

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
