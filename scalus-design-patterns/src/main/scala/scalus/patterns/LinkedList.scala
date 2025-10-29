package scalus.patterns

import scalus.builtin.Builtins
import scalus.builtin.ByteString
import scalus.builtin.ByteString.fromString
import scalus.builtin.ByteString.hex
import scalus.builtin.Data
import scalus.builtin.Data.FromData
import scalus.builtin.Data.ToData
import scalus.ledger.api.v1.Address
import scalus.ledger.api.v2.OutputDatum
import scalus.ledger.api.v3.*
import scalus.prelude.*
import scalus.prelude.Option.*
import scalus.prelude.Ord.*
import scalus.{show as _, *}

/** Linked list configuration parameter
  *
  * @param init
  *   An input reference that makes each initialized linked list unique.
  * @param deadline
  *   The deadline to which royalties could be payed.
  * @param penalty
  *   A payment address to pay royalties.
  */
case class Config(
    init: TxOutRef,
    deadline: PosixTime,
    penalty: Address
) derives FromData,
      ToData

@Compile
object Config

type NodeKey = Option[TokenName]

/** Node key and reference:
  *
  * Passed at redeemer, supposed to be stored in tx's datum.
  *
  * @param key
  *   Has unique key (unless it's the root node).
  *   - [[scalus.prelude.Option.None]] means a head of the linked list
  *   - [[scalus.prelude.Option.Some]] means a node of the linked list.
  * @param ref
  *   Has a unique link to another node's key (unless it's the last node of the list)
  *   - [[scalus.prelude.Option.None]] means a end of the linked list (or empty head)
  *   - [[scalus.prelude.Option.Some]] contains a reference key to the next node.
  *
  * @note
  *   Token present as a value asset name and as key at datum.
  *
  * @todo
  *   `data: Data` field to store user data.
  */
case class Cons(
    key: NodeKey,
    ref: NodeKey = None
) derives FromData,
      ToData

@Compile
object Cons:
    /** Constructor of the [[scalus.patterns.Cons]] for a head of the linked list.
      */
    inline def head(ref: NodeKey = None): Cons = Cons(key = None, ref)

    /** Constructor of the [[scalus.patterns.Cons]] for a node of the linked list.
      */
    inline def cons(key: TokenName, ref: NodeKey = None): Cons = Cons(key = Some(key), ref)

    given Eq[Cons] = (x, y) => x.key === y.key && x.ref === y.ref

    given Ord[Cons] = by: node =>
        node match
            case Cons(key, _) => key

    extension (self: Cons)

        /** Create a nested node
          *
          * @param at
          *   New node key, the head of linked list at current node.
          */
        inline def chKey(at: TokenName): Cons = cons(at, self.ref)

        /** Create a parent node
          *
          * @param by
          *   New node key, the head of tail of linked list at current node.
          */
        inline def chRef(by: TokenName): Cons = Cons(self.key, Some(by))

/** Every linked list node UTxO:
  *
  * Representation of an [[scalus.ledger.api.v2.TxOut]] of the node for the
  * [[scalus.patterns.Common]] argument with tx's inputs and outputs.
  *
  * @param value
  *   Holds node's token key.
  * @param cell
  *   Node state, [[scalus.patterns.Cons]] keys.
  */
case class Node(
    value: Value,
    cell: Cons
) derives FromData,
      ToData

@Compile
object Node:

    given Eq[Node] = (x, y) => x.value === y.value && x.cell === y.cell

    given Ord[Node] = by: cons =>
        cons match
            case Node(_, cell) => cell

    extension (self: Node)

        def sort(other: Node): (Node, Node) = if self < other then (self, other) else (other, self)
        def sortByKey(other: Node, key: TokenName): (Node, Node) =
            if self.cell.key !== Some(key) then (self, other) else (other, self)

/** Common information shared between all redeemers:
  *
  * @param policy
  *   state token (own) policy id.
  * @param mint
  *   value minted in current Tx, meant to contain a policy's node token.
  * @param inputs
  *   current Tx inputs, the [[scalus.patterns.NodePair]] of [[scalus.ledger.api.v2.TxOut]].
  * @param outputs
  *   current Tx outputs, the [[scalus.patterns.NodePair]] of [[scalus.ledger.api.v3.TxInInfo]].
  */
case class Common(
    policy: PolicyId,
    mint: Value,
    inputs: List[Node],
    outputs: List[Node]
) derives FromData,
      ToData

@Compile
object Common

@Compile
object OrderedLinkedList:

    /** Linked list node token.
      *
      * @param key
      *   Optional node key argument for the non-head nodes.
      */
    def nodeToken(key: TokenName = hex""): TokenName =
        fromString("LAN") ++ key // FIXME: utf8"LAN" syntax

        /** Node token validation.
          *
          * @param token
          *   A node token to check.
          */
    def isNodeToken(token: TokenName): Boolean = token.take(nodeToken().length) === nodeToken()

    /** Node key by a node token.
      *
      * @param token
      *   A node token.
      */
    def nodeKey(token: TokenName): NodeKey =
        Some(token.drop(nodeToken().length)).filter(_.nonEmpty)

        /** Common invariants validation, collect node's inputs and outputs.
          *
          * @param policy
          *   A policy of the linked list.
          * @param tx
          *   Current transaction metadata.
          */
    def mkCommon(
        policy: PolicyId,
        tx: TxInfo
    ): (Common, List[TxInInfo], List[TxOut], List[PubKeyHash], Interval) =
        def withPolicy(outs: List[TxOut]) = outs.filter:
            _.value.toSortedMap.contains(policy)

        val policyInOuts = withPolicy(tx.inputs.map(_.resolved))
        val policyOutputs = withPolicy(tx.outputs)
        val nodeOuts = policyInOuts ++ policyOutputs

        nodeOuts.headOption match
            case Some(head) =>
                require(
                  nodeOuts.forall(head.address === _.address),
                  "All node outputs must have same address"
                )
                val inputs = policyInOuts.map(getNode)
                val outputs = policyOutputs.map(out =>
                    val node = getNode(out)
                    validateNode(policy, node)
                    node
                )
                val common = Common(policy, tx.mint, inputs, outputs)
                (common, tx.inputs, tx.outputs, tx.signatories, tx.validRange)
            case _ => fail("There's must be at least one output")

    /** Collect node output.
      *
      * @param out
      *   An output to collect.
      */
    def getNode(out: TxOut): Node = Node(
      out.value,
      out.datum match
          case OutputDatum.OutputDatum(nodeDatum) => nodeDatum.to[Cons]
          case _                                  => fail("Node datum must be inline")
    )

    /** Validatie a node output invariants.
      *
      * @param policy
      *   A policy of the linked list.
      * @param node
      *   A node to validate.
      */
    def validateNode(policy: PolicyId, node: Node): Unit =
        val Node(value, cell) = node
        require(
          cell.key.forall(key => cell.ref.forall(key < _)),
          "Nodes must be ordered by keys"
        )
        value.flatten match
            case List.Cons(
                  (adaPolicy, adaToken, _amount),
                  List.Cons((policyId, token, amount), List.Nil)
                ) =>
                require(
                  adaPolicy === Value.adaPolicyId && adaToken === Value.adaTokenName,
                  "There's must be a lovelace value for each policy output"
                )
                require(
                  policyId === policy,
                  "There's must be a token key for the policy output"
                )
                require(
                  amount == BigInt(1),
                  "Minted token be exactly one per output"
                )
                require(
                  isNodeToken(token),
                  "Must be valid node token"
                )
                require(
                  nodeKey(token) === cell.key,
                  "Datum must contain a valid node key"
                )
            case _ =>
                fail(
                  "There's must be only a token key and a lovelace values for each policy output"
                )

    // MARK: State transition handlers (used in list minting policy)
    //       aka list natural transformations

    /** Initialize an empty unordered list.
      *
      * Application code must ensure that this action can happen only once.
      */
    def init(common: Common): Unit =
        require( // NEW: not checked
          common.inputs.isEmpty,
          "Must not spend nodes"
        )
        require(
          common.outputs.length == BigInt(1),
          "Must a single linked list head node output"
        )
        require(
          common.mint.flatten === List.single((common.policy, nodeToken(), BigInt(1))),
          "Must mint an node token NFT value for this linked list"
        )

        /** Deinitialize an empty unordered list.
          */
    def deinit(common: Common): Unit = common.inputs match
        case List.Cons(Node(_value, Cons(_key, None)), List.Nil) =>
            require(
              common.outputs.isEmpty,
              "Must not produce nodes"
            )
            require( // NEW: _value.quantityOf(common.policy, nodeToken()) == BigInt(1)
              common.mint.flatten === List.single(
                (common.policy, nodeToken(), BigInt(-1))
              ),
              "Must burn an node token NFT value for this linked list"
            )
        case _ =>
            fail(
              "There must be a single head node input,\n" +
                  "the linked list must be empty"
            )

    /** Insert a node `insertKey` at covering `cell` at the linked list.
      *
      * Covering cell could be parent's cell ex reference, but not necessary.
      *
      * @param insertKey
      *   A key the new cell be added at.
      * @param cell
      *   A pair of key's parent key, that expected to be at linked list, and a reference to the new
      *   tail.
      * @return
      *   Parent node.
      */
    def insert(common: Common, insertKey: PubKeyHash, cell: Cons): Node = common.inputs match
        case List.Cons(parentIn, List.Nil) =>
            common.outputs match
                case List.Cons(fstOut, List.Cons(sndOut, List.Nil)) =>
                    val PubKeyHash(key) = insertKey
                    val (parentOut, insertNode) = fstOut.sort(sndOut)
                    require(
                      cell.chKey(key) === insertNode.cell,
                      "The covering cell must be preserved by inserted key at outputs,\n" +
                          "the inserted key must be present at the cell of inserted node"
                    )
                    require(
                      parentOut === Node(parentIn.value, cell.chRef(key)),
                      "The inserted key must be referenced by the parent's key at outputs,\n" +
                          "the parent node's value must be preserved"
                    ) // NEW: cell.key === parentIn.cell.key
                    require(
                      common.mint.flatten === List.single(
                        (common.policy, nodeToken(key), BigInt(1))
                      ),
                      "Must mint an NFT value for the inserted key for this linked list"
                    )
                    parentIn
                case _ => fail("There must be only a parent and an inserted node outputs")
        case _ => fail("There must be a single covering node input")

    // FIXME: linking
    //
    // (common.inputs, common.outputs) match
    //     case (List.Cons(fstIn, List.Cons(sndIn, List.Nil)), List.Cons(parentOut, List.Nil)) =>
    //

    /** Remove a non-root node `removeKey` at covering `cell` at the linked list.
      *
      * Covering cell must be original parent's cell reference.
      *
      * @param removeKey
      *   A key the cell be removed by.
      * @param cell
      *   A pair of key's original parent key, that expected to be at linked list, and a reference
      *   to the original tail, that remains unchanged.
      * @return
      *   Removed node.
      */
    def remove(common: Common, removeKey: PubKeyHash, cell: Cons): Node = common.inputs match
        case List.Cons(fstIn, List.Cons(sndIn, List.Nil)) =>
            common.outputs match
                case List.Cons(parentOut, List.Nil) =>
                    val PubKeyHash(key) = removeKey
                    val (parentIn, removeNode) = fstIn.sort(sndIn)
                    require(
                      cell.chKey(key) === removeNode.cell,
                      "The covering cell must be referenced by removed key at inputs,\n" +
                          "the removed key must be present at the cell of removed node"
                    )
                    require(
                      cell.chRef(key) === parentIn.cell,
                      "The remove key must be referenced by parent's cell at inputs,\n" +
                          "the parent key must be present at the covering cell"
                    )
                    require(
                      parentOut === Node(parentIn.value, cell),
                      "The covering cell must be referenced by the parent's key at outputs,\n" +
                          "the parent node's value must be kept unchanged"
                    )
                    require(
                      common.mint.flatten === List.single(
                        (common.policy, nodeToken(key), BigInt(-1))
                      ),
                      "Must burn an NFT value for the removed key for this linked list"
                    )
                    removeNode
                case _ => fail("There must be a single parent output")
        case _ => fail("There must be parent and remove node inputs only")

    /** Prepend a new node to the beginning of the list.
      *
      * Covering cell is expected to be the head of the linked list.
      *
      * @param prependKey
      *   A key the new cell be added at.
      * @param cell
      *   A pair of key's parent key, that expected to empty as a marker of a head of the linked
      *   list, and a reference to the new tail.
      * @note
      *   Same as [[scalus.patterns.OrderedLinkedList.insert]] with a boundary extra check.
      */
    def prepend(common: Common, prependKey: PubKeyHash, cell: Cons): Unit =
        require(
          cell.key.isEmpty,
          "A covering cell must be the head of the linked list"
        )
        val _ = insert(common, prependKey, cell)

    /** Append a new node to the end of the list.
      *
      * Covering cell is expected to be the latest at the tail of the linked list.
      *
      * @param appendKey
      *   A key the new cell be added at.
      * @param cell
      *   A pair of key's parent key, that expected to be at linked list, and a reference to the new
      *   tail, that expected to be empty as a marker of the end of the linked list.
      * @note
      *   Same as [[scalus.patterns.OrderedLinkedList.insert]] with a boundary extra check.
      */
    def append(common: Common, appendKey: PubKeyHash, cell: Cons): Unit =
        require(
          cell.ref.isEmpty,
          "A covering cell must be a latest at the tail of the linked list"
        )
        val parentIn = insert(common, appendKey, cell)
        require(
          parentIn.cell.ref.isEmpty,
          "A parent cell cell must be a latest at the tail of the linked list"
        )

// assumptions: keys must be unique
@Compile
object UnorderedLinkedList:

    /** Linked list node token.
      *
      * @param key
      *   Optional node key argument for the non-head nodes.
      */
    def nodeToken(key: TokenName = hex""): TokenName =
        fromString("LAN") ++ key // FIXME: utf8"LAN" syntax

        /** Node token validation.
          *
          * @param token
          *   A node token to check.
          */
    def isNodeToken(token: TokenName): Boolean = token.take(nodeToken().length) === nodeToken()

    /** Node key by a node token.
      *
      * @param token
      *   A node token.
      */
    def nodeKey(token: TokenName): NodeKey =
        Some(token.drop(nodeToken().length)).filter(_.nonEmpty)

        /** Common invariants validation, collect node's inputs and outputs.
          *
          * @param policy
          *   A policy of the linked list.
          * @param tx
          *   Current transaction metadata.
          */
    def mkCommon(
        policy: PolicyId,
        tx: TxInfo
    ): (Common, List[TxInInfo], List[TxOut], List[PubKeyHash], Interval) =
        def withPolicy(outs: List[TxOut]) = outs.filter:
            _.value.toSortedMap.contains(policy)

        val policyInOuts = withPolicy(tx.inputs.map(_.resolved))
        val policyOutputs = withPolicy(tx.outputs)
        val nodeOuts = policyInOuts ++ policyOutputs

        nodeOuts.headOption match
            case Some(head) =>
                require(
                  nodeOuts.forall(head.address === _.address),
                  "All node outputs must have same address"
                )
                val inputs = policyInOuts.map(getNode)
                val outputs = policyOutputs.map(out =>
                    val node = getNode(out)
                    validateNode(policy, node)
                    node
                )
                val common = Common(policy, tx.mint, inputs, outputs)
                (common, tx.inputs, tx.outputs, tx.signatories, tx.validRange)
            case _ => fail("There's must be at least one output")

    /** Collect node output.
      *
      * @param out
      *   An output to collect.
      */
    def getNode(out: TxOut): Node = Node(
      out.value,
      out.datum match
          case OutputDatum.OutputDatum(nodeDatum) => nodeDatum.to[Cons]
          case _                                  => fail("Node datum must be inline")
    )

    /** Validatie a node output invariants.
      *
      * @param policy
      *   A policy of the linked list.
      * @param node
      *   A node to validate.
      */
    def validateNode(policy: PolicyId, node: Node): Unit =
        val Node(value, cell) = node
        value.flatten match
            case List.Cons(
                  (adaPolicy, adaToken, _amount),
                  List.Cons((policyId, token, amount), List.Nil)
                ) =>
                require(
                  adaPolicy === Value.adaPolicyId && adaToken === Value.adaTokenName,
                  "There's must be a lovelace value for each policy output"
                )
                require(
                  policyId === policy,
                  "There's must be a token key for the policy output"
                )
                require(
                  amount == BigInt(1),
                  "Minted token be exactly one per output"
                )
                require(
                  isNodeToken(token),
                  "Must be valid node token"
                )
                require(
                  nodeKey(token) === cell.key,
                  "Datum must contain a valid node key"
                )
            case _ =>
                fail(
                  "There's must be only a token key and a lovelace values for each policy output"
                )

    // MARK: State transition handlers (used in list minting policy)
    //       aka list natural transformations

    /** Initialize an empty unordered list.
      *
      * Application code must ensure that this action can happen only once.
      */
    def init(common: Common): Unit =
        require( // NEW: not checked
          common.inputs.isEmpty,
          "Must not spend nodes"
        )
        require(
          common.outputs.length == BigInt(1),
          "Must a single linked list head node output"
        )
        require(
          common.mint.flatten === List.single((common.policy, nodeToken(), BigInt(1))),
          "Must mint an node token NFT value for this linked list"
        )

        /** Deinitialize an empty unordered list.
          */
    def deinit(common: Common): Unit = common.inputs match
        case List.Cons(Node(_value, Cons(_key, None)), List.Nil) =>
            require(
              common.outputs.isEmpty,
              "Must not produce nodes"
            )
            require( // NEW: _value.quantityOf(common.policy, nodeToken()) == BigInt(1)
              common.mint.flatten === List.single(
                (common.policy, nodeToken(), BigInt(-1))
              ),
              "Must burn an node token NFT value for this linked list"
            )
        case _ =>
            fail(
              "There must be a single head node input,\n" +
                  "the linked list must be empty"
            )

    /** Insert a node `insertKey` at covering `cell` at the linked list.
      *
      * Covering cell could be parent's cell ex reference, but not necessary.
      *
      * @param insertKey
      *   A key the new cell be added at.
      * @param cell
      *   A pair of key's parent key, that expected to be at linked list, and a reference to the new
      *   tail.
      * @return
      *   Parent node.
      */
    private inline def insert(common: Common, insertKey: PubKeyHash, cell: Cons): Node =
        common.inputs match
            case List.Cons(parentIn, List.Nil) =>
                common.outputs match
                    case List.Cons(fstOut, List.Cons(sndOut, List.Nil)) =>
                        val PubKeyHash(key) = insertKey
                        val (parentOut, insertNode) = fstOut.sortByKey(sndOut, key)
                        require(
                          cell.chKey(key) === insertNode.cell,
                          "The covering cell must be preserved by inserted key at outputs,\n" +
                              "the inserted key must be present at the cell of inserted node"
                        )
                        require(
                          parentOut === Node(parentIn.value, cell.chRef(key)),
                          "The inserted key must be referenced by the parent's key at outputs,\n" +
                              "the parent node's value must be preserved"
                        ) // NEW: cell.key === parentIn.cell.key
                        require(
                          common.mint.flatten === List.single(
                            (common.policy, nodeToken(key), BigInt(1))
                          ),
                          "Must mint an NFT value for the inserted key for this linked list"
                        )
                        parentIn
                    case _ => fail("There must be only a parent and an inserted node outputs")
            case _ => fail("There must be a single covering node input")

    // FIXME: linking
    //
    // (common.inputs, common.outputs) match
    //     case (List.Cons(fstIn, List.Cons(sndIn, List.Nil)), List.Cons(parentOut, List.Nil)) =>
    //

    /** Remove a non-root node `removeKey` at covering `cell` at the linked list.
      *
      * Covering cell must be original parent's cell reference.
      *
      * @param removeKey
      *   A key the cell be removed by.
      * @param cell
      *   A pair of key's original parent key, that expected to be at linked list, and a reference
      *   to the original tail, that remains unchanged.
      * @return
      *   Removed node.
      */
    def remove(common: Common, removeKey: PubKeyHash, cell: Cons): Node = common.inputs match
        case List.Cons(fstIn, List.Cons(sndIn, List.Nil)) =>
            common.outputs match
                case List.Cons(parentOut, List.Nil) =>
                    val PubKeyHash(key) = removeKey
                    val (parentIn, removeNode) = fstIn.sortByKey(sndIn, key)
                    require(
                      cell.chKey(key) === removeNode.cell,
                      "The covering cell must be referenced by removed key at inputs,\n" +
                          "the removed key must be present at the cell of removed node"
                    )
                    require(
                      cell.chRef(key) === parentIn.cell,
                      "The remove key must be referenced by parent's cell at inputs,\n" +
                          "the parent key must be present at the covering cell"
                    )
                    require(
                      parentOut === Node(parentIn.value, cell),
                      "The covering cell must be referenced by the parent's key at outputs,\n" +
                          "the parent node's value must be kept unchanged"
                    )
                    require(
                      common.mint.flatten === List.single(
                        (common.policy, nodeToken(key), BigInt(-1))
                      ),
                      "Must burn an NFT value for the removed key for this linked list"
                    )
                    removeNode
                case _ => fail("There must be a single parent output")
        case _ => fail("There must be parent and remove node inputs only")

    /** Prepend a new node to the beginning of the list.
      *
      * Covering cell is expected to be the head of the linked list.
      *
      * @param prependKey
      *   A key the new cell be added at.
      * @param cell
      *   A pair of key's parent key, that expected to empty as a marker of a head of the linked
      *   list, and a reference to the new tail.
      * @note
      *   Same as [[scalus.patterns.UnorderedLinkedList.insert]] with a boundary extra check.
      */
    def prepend(common: Common, prependKey: PubKeyHash, cell: Cons): Unit =
        require(
          cell.key.isEmpty,
          "A covering cell must be the head of the linked list"
        )
        val _ = insert(common, prependKey, cell)

    /** Append a new node to the end of the list.
      *
      * Covering cell is expected to be the latest at the tail of the linked list.
      *
      * @param appendKey
      *   A key the new cell be added at.
      * @param cell
      *   A pair of key's parent key, that expected to be at linked list, and a reference to the new
      *   tail, that expected to be empty as a marker of the end of the linked list.
      * @note
      *   Same as [[scalus.patterns.UnorderedLinkedList.insert]] with a boundary extra check.
      */
    def append(common: Common, appendKey: PubKeyHash, cell: Cons): Unit =
        require(
          cell.ref.isEmpty,
          "A covering cell must be a latest at the tail of the linked list"
        )
        val parentIn = insert(common, appendKey, cell)
        require(
          parentIn.cell.ref.isEmpty,
          "A parent cell cell must be a latest at the tail of the linked list"
        )
