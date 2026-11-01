package scalus.cardano.node.emulator

import scalus.cardano.ledger.SlotNo
import scalus.cardano.node.SubmitError
import scalus.cardano.node.stream.ChainPoint

/** Why `produceBlock(on, …)` forged no block, spec section 8.2. The state does not change, spec
  * [FG-17].
  */
enum ForgeError {

    /** The forge slot is not after the slot of `on`, spec [FG-12]. */
    case SlotNotAfterParent(slot: SlotNo, parentSlot: SlotNo)

    /** `on` is not in the block tree, spec [FG-13]. Genesis is [[ChainPoint.origin]]. */
    case UnknownPoint(point: ChainPoint)

    /** `on`, or the fork point of the branch it is on, is deeper than k, spec [FG-14]. */
    case TooDeep(depth: Int, securityParam: Long)

    /** The tx at `index` fails the ledger rules, spec [FG-16]. */
    case InvalidTx(index: Int, error: SubmitError)

    /** The txs exceed the block limits, spec [FG-18]. */
    case BlockTooBig

    /** What went wrong, in prose, for a person to read. Match on the case in code. */
    def message: String = this match
        case SlotNotAfterParent(slot, parentSlot) =>
            s"slot $slot is not after slot $parentSlot of the parent block"
        case UnknownPoint(point) =>
            s"no block at slot ${point.slot} with hash ${point.blockHash.toHex} in the block tree"
        case TooDeep(depth, securityParam) =>
            s"the fork point is at depth $depth, deeper than securityParam $securityParam"
        case InvalidTx(index, error) => s"the tx at index $index is invalid: ${error.message}"
        case BlockTooBig => "the txs exceed the block body size or execution unit limits"
}

/** Why `rollback(n)` removed no block, spec section 11.3. */
enum RollbackError {

    /** `n` is below 1, spec [RB-1]. */
    case NotPositive(n: Int)

    /** `n` is above k, spec [RB-2], or the snapshot of the block `n` back is gone: an earlier
      * rollback already went back to the oldest snapshot the Emulator keeps.
      */
    case TooDeep(n: Int, securityParam: Long)

    /** `n` is above the tip height, spec [RB-3]. */
    case PastGenesis(n: Int, tipHeight: Long)

    /** What went wrong, in prose, for a person to read. Match on the case in code. */
    def message: String = this match
        case NotPositive(n) => s"rollback($n): n must be at least 1"
        case TooDeep(n, securityParam) =>
            s"rollback($n): n is above securityParam $securityParam, or past the oldest snapshot kept"
        case PastGenesis(n, tipHeight) => s"rollback($n): n is above the tip height $tipHeight"
}
