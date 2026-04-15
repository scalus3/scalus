package scalus.cardano.node.stream.engine

import scalus.cardano.address.Address
import scalus.cardano.ledger.{AssetName, PolicyId, TransactionHash, TransactionInput, TransactionOutput}

/** Pre-computable partition key over the UTxO set.
  *
  * For any `(TransactionInput, TransactionOutput)` pair the engine can decide in O(1) (plus
  * multi-asset lookup) which [[UtxoKey]]s it belongs to — no external lookup. Each
  * [[scalus.cardano.node.UtxoSource]] variant maps onto exactly one [[UtxoKey]] constructor;
  * [[UtxoQuery]] decomposition walks its sources to produce the set of keys that must be indexed
  * for a subscription.
  *
  * The engine holds one [[Bucket]] per active key, refcounted by the subscriptions that need it.
  * When the last subscription leaves, the bucket is dropped.
  */
sealed trait UtxoKey
object UtxoKey {

    case class Addr(address: Address) extends UtxoKey

    case class Asset(policyId: PolicyId, assetName: AssetName) extends UtxoKey

    /** All outputs produced by a specific transaction. */
    case class TxOuts(transactionId: TransactionHash) extends UtxoKey

    /** A fixed enumerated set of inputs. Typical use: collateral lookup, spot reads of a known UTxO
      * by its input. The "index" is trivial — membership test against the set itself.
      */
    case class Inputs(inputs: Set[TransactionInput]) extends UtxoKey

    /** True if this UTxO belongs to the partition named by `key`. Cheap membership test.
      */
    def matches(key: UtxoKey, input: TransactionInput, output: TransactionOutput): Boolean =
        key match {
            case Addr(address) => output.address == address
            case Asset(policyId, assetName) =>
                output.value.assets.assets.get(policyId).exists(_.contains(assetName))
            case TxOuts(transactionId) => input.transactionId == transactionId
            case Inputs(inputs)        => inputs.contains(input)
        }
}
