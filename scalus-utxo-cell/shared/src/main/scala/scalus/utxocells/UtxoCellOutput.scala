package scalus.utxocells

import scalus.cardano.onchain.plutus.v1.{Address, Value}

/** An additional output that a UtxoCell transition requires in the transaction.
  *
  * On-chain: `UtxoCellLib.verifyOutputs` checks that a matching output exists in `tx.outputs` with
  * the specified address and at least the specified value. Off-chain: the builder converts these to
  * `TxBuilder.payTo` calls.
  *
  * @param address
  *   the on-chain destination address
  * @param value
  *   the minimum value that must be sent to this address
  */
case class UtxoCellOutput(
    address: Address,
    value: Value
)
