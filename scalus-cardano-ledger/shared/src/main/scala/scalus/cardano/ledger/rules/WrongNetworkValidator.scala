package scalus.cardano.ledger
package rules

import scalus.cardano.address.{Address, Network}

// It's part of Babbage.validateWrongNetwork in cardano-ledger
object WrongNetworkValidator extends STS.Validator {
    override final type Error = TransactionException.WrongNetworkAddress

    override def validate(context: Context, state: State, event: Event): Result = {
        val transactionId = event.id
        val expectedNetwork = context.env.network
        val outputs = event.body.value.outputs
        val collateralReturnOutput = event.body.value.collateralReturnOutput.toIndexedSeq

        val invalidOutputAddresses = findInvalidAddresses(outputs, expectedNetwork)

        val invalidCollateralReturnAddresses =
            findInvalidAddresses(collateralReturnOutput, expectedNetwork).headOption

        if invalidOutputAddresses.nonEmpty || invalidCollateralReturnAddresses.nonEmpty then
            failure(
              TransactionException.WrongNetworkAddress(
                transactionId,
                invalidOutputAddresses,
                invalidCollateralReturnAddresses
              )
            )
        else success
    }

    private def findInvalidAddresses(
        outputs: IndexedSeq[Sized[TransactionOutput]],
        expectedNetwork: Network
    ): IndexedSeq[Address] = {
        (
          for
              SizedValue(output) <- outputs.view
              address = output.address
              if !address.getNetwork.contains(
                expectedNetwork
              ) // TODO: getNetwork returns Option[Network] instead of Network due to Byron addresses issue, so we need to handle None case as invalid address
          yield address
        ).toIndexedSeq
    }
}
