package scalus.cardano.node.stream

import scalus.cardano.address.Address
import scalus.cardano.ledger.{AssetName, PolicyId, ScriptHash, SlotNo, TransactionInput}

/** Which transactions a subscriber wants to observe.
  *
  * The shape deliberately mirrors UTxORPC's `TxPredicate` (`match` / `not` / `all_of` / `any_of`),
  * so a UTxORPC-backed provider lowers a query onto the wire predicate instead of translating
  * between two different algebras — and so anything expressible here can be pushed down by the
  * backends that support server-side filtering.
  */
enum TransactionQuery {

    /** Every transaction. */
    case All

    /** Transactions with an input or an output at this address. */
    case InvolvesAddress(address: Address)

    /** Transactions minting or burning any asset under this policy. */
    case MintsPolicy(policy: PolicyId)

    /** Transactions minting or burning this exact asset. */
    case MintsAsset(policy: PolicyId, name: AssetName)

    /** Transactions spending this specific input. */
    case SpendsInput(input: TransactionInput)

    /** Transactions running this script — spending from it, minting under it, or referencing it. */
    case InvolvesScript(script: ScriptHash)

    case Not(query: TransactionQuery)
    case AllOf(queries: Seq[TransactionQuery])
    case AnyOf(queries: Seq[TransactionQuery])
}

object TransactionQuery {
    extension (self: TransactionQuery) {
        def &&(other: TransactionQuery): TransactionQuery = AllOf(Seq(self, other))
        def ||(other: TransactionQuery): TransactionQuery = AnyOf(Seq(self, other))
        def unary_! : TransactionQuery = Not(self)
    }
}

/** Which blocks a subscriber wants to observe.
  *
  * Simpler than the other two algebras on purpose: most subscribers want every block, or every
  * block in a range. Content-based selection belongs on [[TransactionQuery]], where it can be
  * pushed down.
  */
enum BlockQuery {

    /** Every block. */
    case All

    /** Blocks whose slot is in `[from, to]`; `to = None` means open-ended. The subscription
      * completes once the chain passes `to`.
      */
    case InSlotRange(from: SlotNo, to: Option[SlotNo])
}
