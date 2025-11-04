package scalus.examples

import scalus.Compiler.compile
import scalus.Compiler.compileWithOptions
import scalus.builtin.Builtins
import scalus.builtin.Data
import scalus.builtin.Data.FromData
import scalus.builtin.Data.ToData
import scalus.cardano.blueprint.Application
import scalus.cardano.blueprint.Blueprint
import scalus.ledger.api.v1.Address
import scalus.ledger.api.v3.*
import scalus.patterns.{Config, Cons}
import scalus.prelude.*
import scalus.{show as _, *}

import scalus.patterns.OrderedLinkedList.*

/** Actions that can be performed on the linked list
  *
  * @note
  *   No actions designed to perform in batches.
  */
enum OrderedNodeAction derives FromData, ToData:
    /** Initialize linked list head reference cell with current [[scalus.patterns.Config]]
      */
    case Init

    /** Burn an empty linked list head reference, pay royalties
      */
    case Deinit

    /** Insert new linked list node by `key` at `covering` cell.
      *
      * Covering cell could be parent's cell ex reference, but not necessary.
      *
      * @param key
      *   A `key` the new cell be added at.
      * @param covering
      *   A pair of `key`'s parent key, that expected to be at linked list, and a reference to the
      *   new tail.
      */
    case Insert(key: PubKeyHash, covering: Cons)

    /** Prepend a new node to the beginning of the list.
      *
      * Covering cell is expected to be the head of the linked list.
      *
      * @see
      *   [[scalus.examples.OrderedNodeAction.Insert]]
      */
    case Prepend(key: PubKeyHash, covering: Cons)

    /** Append a new node to the end of the list.
      *
      * Covering cell is expected to be the latest at the tail of the linked list.
      *
      * @see
      *   [[scalus.examples.OrderedNodeAction.Insert]]
      */
    case Append(key: PubKeyHash, covering: Cons)

    /** Remove a linked list node by `key` at `covering` cell.
      *
      * Covering cell must be original parent's cell reference.
      *
      * @param key
      *   A `key` the cell be removed by.
      * @param covering
      *   A pair of `key`'s original parent key, that expected to be at linked list, and a reference
      *   to the original tail, that remains unchanged.
      */
    case Remove(key: PubKeyHash, covering: Cons)

@Compile
object OrderedNodeAction

/** Linked List is an on-chain, sorted linked list solution designed for blockchain environments,
  * specifically utilizing NFTs (Non-Fungible Tokens) and datums. It provides a structured and
  * efficient way to store and manipulate a list of key/value pairs on-chain.
  *
  * {{{
  *   ╭──────╮  ╭───────╮  ╭────────╮  ╭────────╮  ╭───────╮
  *   │•Head•├─>│ Apple ├─>│ Banana ├─>│ Orange ├─>│ Peach │
  *   ╰──────╯  ╰───────╯  ╰────────╯  ╰────────╯  ╰───────╯
  * }}}
  * &nbsp;
  * ===Entry Structure&#10;===
  * &nbsp;
  *   - Each entry in the list comprises: &#10;&nbsp;
  *     - [[scalus.ledger.api.v1.TokenName]] '''NFT''': A unique identifier for each entry.
  *     - [[scalus.patterns.Cons]] '''Datum''': A data structure containing the key/value pair, a
  *       reference to the entry's NFT, and a pointer to the next NFT in the list.
  *
  * ===Operations===
  * &nbsp;
  * ====Inserting an Entry====
  * &nbsp;
  *   - Insertion involves: &#10;&nbsp;
  *     - '''Inputs''': Two adjacent list entries.
  *     - '''Outputs''':
  *       - The first input entry, modified to point to the new entry.
  *       - The newly inserted entry, pointing to the second input entry.
  *       - The second input entry, unchanged.
  *   - Validation Rules &#10;&nbsp;
  *     - Keys must maintain the order: `a < b < c`, where `a` is the lowest, `b` is the new key,
  *       and `c` is the highest.
  *     - The pointers must be correctly updated to maintain list integrity.
  *
  * {{{
  * ╭──────╮  ╭───────╮  ╭────────╮  ╭────────╮  ╭───────╮
  * │•Head•├─>│ Apple ├─>│ Banana ├─>│ Orange ├─>│ Peach │
  * ╰──────╯  ╰───────╯  ╰─────┬──╯  ╰────┬───╯  ╰───────╯
  *                            │          │
  *                        ┏━━━V━━━━━━━━━━V━━━━━━━━━━━━━━┓
  *                        ┃▓█▓▒░ Insert Transaction ░▒▓█┃
  *                        ┗━━━┯━━━━━━━━━━┯━━━━━━━━━━┯━━━┛
  *                            │          │          │
  *   ╭──────╮  ╭───────╮  ╭───V────╮  ╭──V───╮  ╭───V────╮  ╭───────╮
  *   │•Head•├─>│ Apple ├─>│:Banana:├─>│~Kiwi~├─>│ Orange ├─>│ Peach │
  *   ╰──────╯  ╰───────╯  ╰────────╯  ╰──────╯  ╰────────╯  ╰───────╯
  * }}}
  *
  * ====Removing an Entry====
  * &nbsp;
  *   - To remove an entry: &#10;&nbsp;
  *     - '''Inputs''': The entry to remove and its preceding entry.
  *     - '''Output''': The preceding entry is modified to point to what the removed entry was
  *       pointing to.
  *
  * {{{
  *   ╭──────╮  ╭───────╮  ╭────────╮  ╭──────╮  ╭────────╮  ╭───────╮
  *   │•Head•├─>│ Apple ├─>│ Banana ├─>│~Kiwi~├─>│ Orange ├─>│ Peach │
  *   ╰──────╯  ╰───────╯  ╰───┬────╯  ╰──┬───╯  ╰────────╯  ╰───────╯
  *                            │          │
  *                        ┏━━━V━━━━━━━━━━V━━━━━━━━━━━━━━┓
  *                        ┃▓█▓▒░ Delete Transaction ░▒▓█┃
  *                        ┗━━━┯━━━━━━━━━━━━━━━━━━━━━━━━━┛
  *                            │
  * ╭──────╮  ╭───────╮  ╭─────V──╮  ╭────────╮  ╭───────╮
  * │•Head•├─>│ Apple ├─>│:Banana:├─>│ Orange ├─>│ Peach │
  * ╰──────╯  ╰───────╯  ╰────────╯  ╰────────╯  ╰───────╯
  * }}}
  *
  * @see
  *   [[https://github.com/Anastasia-Labs/aiken-linked-list/tree/0.0.1?tab=readme-ov-file#linked-list Aiken Linked List]]
  * @see
  *   [[https://github.com/Anastasia-Labs/data-structures/blob/2bbe6e7388d3fb0fa5c0e5cbfcaad98294869655/pages/linked_list.mdx#introduction-to-linked-list Plutarch Linked List Guide]]
  */
@Compile
object OrderedLinkedList extends DataParameterizedValidator:

    /** Minting validator.
      */
    inline override def mint(
        cfgData: Data,
        redeemer: Data,
        policy: PolicyId,
        tx: TxInfo
    ): Unit =
        val cfg = cfgData.to[Config]
        val (common, inputs, outputs, signatories, range) = mkCommon(policy, tx)

        redeemer.to[OrderedNodeAction] match
            case OrderedNodeAction.Init =>
                require(
                  inputs.exists(cfg.init === _.outRef),
                  "The head must be unique: the initial UTxO must be spent"
                )
                init(common)
            case OrderedNodeAction.Deinit =>
                deinit(common)
            case OrderedNodeAction.Insert(key, covering) =>
                require(
                  range.isEntirelyBefore(cfg.deadline),
                  "Must be before the deadline"
                )
                require(
                  signatories.contains(key),
                  "Must be signed by a node key"
                )
                val _ = insert(common, key, covering)
            case OrderedNodeAction.Prepend(key, covering) =>
                require(
                  range.isEntirelyBefore(cfg.deadline),
                  "Must be before the deadline"
                )
                require(
                  signatories.contains(key),
                  "Must be signed by a node key"
                )
                prepend(common, key, covering)
            case OrderedNodeAction.Append(key, covering) =>
                require(
                  range.isEntirelyBefore(cfg.deadline),
                  "Must be before the deadline"
                )
                require(
                  signatories.contains(key),
                  "Must be signed by a node key"
                )
                append(common, key, covering)
            case OrderedNodeAction.Remove(key, covering) =>
                require(
                  signatories.contains(key),
                  "Must be signed by a node key"
                )
                val removed = remove(common, key, covering)
                val fee = -Builtins.divideInteger(removed.value.getLovelace, -4)
                require(
                  range.isEntirelyBefore(cfg.deadline) || outputs.exists(out =>
                      out.address === cfg.penalty && fee < out.value.getLovelace
                  ),
                  "Must satisfy removal broke phase rules"
                )

object OrderedLinkedListContract:

    given scalus.Compiler.Options = scalus.Compiler.Options(
      targetLoweringBackend = scalus.Compiler.TargetLoweringBackend.SirToUplcV3Lowering,
      generateErrorTraces = true,
      optimizeUplc = true,
      debug = false
    )

    inline def make(param: Config)(using scalus.Compiler.Options) =
        import scalus.builtin.ToData.toData
        compile(OrderedLinkedList.validate).toUplc().plutusV3 $ param.toData

    inline def compiled(using options: scalus.Compiler.Options) =
        compileWithOptions(options, OrderedLinkedList.validate)

    def application: Application = Application
        .ofSingleValidator[Config, OrderedNodeAction](
          "OrderedLinkedList validator",
          "Linked list structures leverage the EUTXO model to enhancing scalability and throughput significantly. By linking multiple UTXOs together through a series of minting policies and validators, it can improve the user experience interacting with smart contract concurrently.",
          "1.0.0",
          OrderedLinkedList.validate
        )

    def blueprint: Blueprint = application.blueprint
