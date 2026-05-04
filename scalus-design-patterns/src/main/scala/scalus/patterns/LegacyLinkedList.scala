package scalus.patterns

import scalus.compiler.Compile

import scalus.uplc.builtin.Builtins
import scalus.uplc.builtin.ByteString
import scalus.uplc.builtin.ByteString.hex
import scalus.uplc.builtin.ByteString.utf8
import scalus.uplc.builtin.Data
import scalus.uplc.builtin.Data.FromData
import scalus.uplc.builtin.Data.ToData
import scalus.cardano.onchain.plutus.v1.Address
import scalus.cardano.onchain.plutus.v2.OutputDatum
import scalus.cardano.onchain.plutus.v3.*
import scalus.cardano.onchain.plutus.prelude.*
import scalus.cardano.onchain.plutus.prelude.Option.*
import scalus.cardano.onchain.plutus.prelude.Ord.*
import scalus.{show as _, *}

/** Linked list configuration parameter
  *
  * @param init
  *   An input reference that makes each initialized linked list unique.
  * @param deadline
  *   The deadline to which royalties could be paid.
  * @param penalty
  *   A payment address to pay royalties.
  * @deprecated
  *   Use [[LinkedList]] instead.
  */
@deprecated("Use scalus.patterns.LinkedList instead", "0.15.1")
case class Config(
    init: TxOutRef,
    deadline: PosixTime,
    penalty: Address
) derives FromData,
      ToData

@Compile
@deprecated("Use scalus.patterns.LinkedList instead", "0.15.1")
object Config

/** Node key and reference.
  *
  * @deprecated
  *   Use [[LinkedList]] instead.
  */
@deprecated("Use scalus.patterns.LinkedList instead", "0.15.1")
case class Cons(
    key: Option[TokenName],
    ref: Option[TokenName] = None,
    data: Data = Data.unit
) derives FromData,
      ToData

@Compile
@deprecated("Use scalus.patterns.LinkedList instead", "0.15.1")
object Cons:
    inline def head(ref: Option[TokenName] = None, data: Data = Data.unit): Cons =
        Cons(key = None, ref, data)

    inline def cons(key: TokenName, ref: Option[TokenName] = None, data: Data = Data.unit): Cons =
        Cons(key = Some(key), ref, data)

    given Eq[Cons] = (x, y) => x.key === y.key && x.ref === y.ref && x.data === y.data

    given Ord[Cons] = by { case Cons(key, _, _) =>
        key
    }

    extension (self: Cons)

        inline def chKey(at: TokenName): Cons = cons(at, self.ref, self.data)

        inline def chRef(by: TokenName): Cons = Cons(self.key, Some(by), self.data)

/** @deprecated Use [[LinkedList]] instead. */
@deprecated("Use scalus.patterns.LinkedList instead", "0.15.1")
case class Node(
    value: Value,
    cell: Cons
) derives FromData,
      ToData

@Compile
@deprecated("Use scalus.patterns.LinkedList instead", "0.15.1")
object Node:

    given Eq[Node] = (x, y) => x.value === y.value && x.cell === y.cell

    given Ord[Node] = by { case Node(_, cell) =>
        cell
    }

    extension (self: Node)

        def sort(other: Node): (Node, Node) = if self < other then (self, other) else (other, self)
        def sortByKey(other: Node, key: TokenName): (Node, Node) =
            if self.cell.key !== Some(key) then (self, other) else (other, self)

/** @deprecated Use [[LinkedList]] instead. */
@deprecated("Use scalus.patterns.LinkedList instead", "0.15.1")
case class Common(
    policy: PolicyId,
    mint: Value,
    inputs: List[Node],
    outputs: List[Node]
) derives FromData,
      ToData

@Compile
@deprecated("Use scalus.patterns.LinkedList instead", "0.15.1")
object Common

/** @deprecated Use [[LinkedList]] instead. */
@Compile
@deprecated("Use scalus.patterns.LinkedList instead", "0.15.1")
object OrderedLinkedList:

    def nodeToken(key: TokenName = hex""): TokenName =
        utf8"LAN" ++ key

    def isNodeToken(token: TokenName): Boolean = token.take(nodeToken().length) === nodeToken()

    def nodeKey(token: TokenName): Option[TokenName] =
        Some(token.drop(nodeToken().length)).filter(_.nonEmpty)

    def mkCommon(
        policy: PolicyId,
        tx: TxInfo
    ): (Common, List[TxInInfo], List[TxOut], List[PubKeyHash], Interval) =
        def withPolicy(outs: List[TxOut]) = outs.filter:
            _.value.toSortedMap.contains(policy)

        val policyInOuts = withPolicy(tx.inputs.map(_.resolved))
        val policyOutputs = withPolicy(tx.outputs)
        val nodeOuts = policyInOuts ++ policyOutputs

        val head = nodeOuts.headOption.getOrFail("There's must be at least one output")
        require(
          nodeOuts.forall(head.address === _.address),
          "All node outputs must have same address"
        )
        val inputs = policyInOuts.map(getNode)
        val outputs = policyOutputs.map: out =>
            val node = getNode(out)
            validateNode(policy, node)
            node
        val common = Common(policy, tx.mint, inputs, outputs)
        (common, tx.inputs, tx.outputs, tx.signatories, tx.validRange)

    def getNode(out: TxOut): Node = Node(
      out.value,
      out.datum match
          case OutputDatum.OutputDatum(nodeDatum) => nodeDatum.to[Cons]
          case _                                  => fail("Node datum must be inline")
    )

    def validateNode(policy: PolicyId, node: Node): Unit =
        val Node(value, cell) = node
        require(
          cell.key.forall(key => cell.ref.forall(key < _)),
          "Nodes must be ordered by keys"
        )
        val (adaPolicy, adaToken, policyId, token, amount) = value.flatten match
            case List.Cons(
                  (adaPolicy, adaToken, _),
                  List.Cons((policyId, token, amount), List.Nil)
                ) =>
                (adaPolicy, adaToken, policyId, token, amount)
            case _ =>
                fail(
                  "There's must be only a token key and a lovelace values for each policy output"
                )
        require(
          adaPolicy === Value.adaPolicyId && adaToken === Value.adaTokenName,
          "There's must be a lovelace value for each policy output"
        )
        require(policyId === policy, "There's must be a token key for the policy output")
        require(amount == BigInt(1), "Minted token be exactly one per output")
        require(isNodeToken(token), "Must be valid node token")
        require(nodeKey(token) === cell.key, "Datum must contain a valid node key")

    def init(common: Common): Unit =
        require(common.inputs.isEmpty, "Must not spend nodes")
        require(
          common.outputs.length == BigInt(1),
          "Must a single linked list head node output"
        )
        require(
          common.mint.flatten === List.single((common.policy, nodeToken(), BigInt(1))),
          "Must mint an node token NFT value for this linked list"
        )

    def deinit(common: Common): Unit = common.inputs match
        case List.Cons(Node(_value, Cons(_key, None, _)), List.Nil) =>
            require(common.outputs.isEmpty, "Must not produce nodes")
            require(
              common.mint.flatten === List.single((common.policy, nodeToken(), BigInt(-1))),
              "Must burn an node token NFT value for this linked list"
            )
        case _ =>
            fail("There must be a single head node input,\nthe linked list must be empty")

    def insert(common: Common, insertKey: PubKeyHash, cell: Cons): Node =
        val parentIn = common.inputs match
            case List.Cons(parentIn, List.Nil) => parentIn
            case _                             => fail("There must be a single covering node input")
        val PubKeyHash(key) = insertKey
        val (parentOut, insertNode) = common.outputs match
            case List.Cons(fstOut, List.Cons(sndOut, List.Nil)) => fstOut.sort(sndOut)
            case _ => fail("There must be only a parent and an inserted node outputs")

        require(
          insertNode.cell.key === Some(key),
          "The inserted key must be present at the cell of inserted node"
        )
        require(insertNode.cell.ref === cell.ref, "The inserted node must have the expected ref")
        require(parentOut.cell.key === cell.key, "Parent key must be preserved")
        require(
          parentOut.cell.ref === Some(key),
          "The inserted key must be referenced by the parent's key at outputs"
        )
        require(parentOut.cell.data === parentIn.cell.data, "Parent data must be preserved")
        require(parentOut.value === parentIn.value, "Parent node's value must be preserved")
        require(
          common.mint.flatten === List.single((common.policy, nodeToken(key), BigInt(1))),
          "Must mint an NFT value for the inserted key for this linked list"
        )
        parentIn

    def remove(common: Common, removeKey: PubKeyHash, cell: Cons): Node =
        val PubKeyHash(key) = removeKey
        val (parentIn, removeNode) = common.inputs match
            case List.Cons(fstIn, List.Cons(sndIn, List.Nil)) => fstIn.sort(sndIn)
            case _ => fail("There must be parent and remove node inputs only")
        val parentOut = common.outputs match
            case List.Cons(parentOut, List.Nil) => parentOut
            case _                              => fail("There must be a single parent output")
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
          common.mint.flatten === List.single((common.policy, nodeToken(key), BigInt(-1))),
          "Must burn an NFT value for the removed key for this linked list"
        )
        removeNode

    def prepend(common: Common, prependKey: PubKeyHash, cell: Cons): Unit =
        require(cell.key.isEmpty, "A covering cell must be the head of the linked list")
        val _ = insert(common, prependKey, cell)

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

/** @deprecated Use [[LinkedList]] instead. */
@Compile
@deprecated("Use scalus.patterns.LinkedList instead", "0.15.1")
object UnorderedLinkedList:

    def nodeToken(key: TokenName = hex""): TokenName =
        utf8"LAN" ++ key

    def isNodeToken(token: TokenName): Boolean = token.take(nodeToken().length) === nodeToken()

    def nodeKey(token: TokenName): Option[TokenName] =
        Some(token.drop(nodeToken().length)).filter(_.nonEmpty)

    def mkCommon(
        policy: PolicyId,
        tx: TxInfo
    ): (Common, List[TxInInfo], List[TxOut], List[PubKeyHash], Interval) =
        def withPolicy(outs: List[TxOut]) = outs.filter:
            _.value.toSortedMap.contains(policy)

        val policyInOuts = withPolicy(tx.inputs.map(_.resolved))
        val policyOutputs = withPolicy(tx.outputs)
        val nodeOuts = policyInOuts ++ policyOutputs

        val head = nodeOuts.headOption.getOrFail("There's must be at least one output")
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

    def getNode(out: TxOut): Node = Node(
      out.value,
      out.datum match
          case OutputDatum.OutputDatum(nodeDatum) => nodeDatum.to[Cons]
          case _                                  => fail("Node datum must be inline")
    )

    def validateNode(policy: PolicyId, node: Node): Unit =
        val Node(value, cell) = node
        val (adaPolicy, adaToken, policyId, token, amount) = value.flatten match
            case List.Cons(
                  (adaPolicy, adaToken, _),
                  List.Cons((policyId, token, amount), List.Nil)
                ) =>
                (adaPolicy, adaToken, policyId, token, amount)
            case _ =>
                fail(
                  "There's must be only a token key and a lovelace values for each policy output"
                )
        require(
          adaPolicy === Value.adaPolicyId && adaToken === Value.adaTokenName,
          "There's must be a lovelace value for each policy output"
        )
        require(policyId === policy, "There's must be a token key for the policy output")
        require(amount == BigInt(1), "Minted token be exactly one per output")
        require(isNodeToken(token), "Must be valid node token")
        require(nodeKey(token) === cell.key, "Datum must contain a valid node key")

    def init(common: Common): Unit =
        require(common.inputs.isEmpty, "Must not spend nodes")
        require(
          common.outputs.length == BigInt(1),
          "Must a single linked list head node output"
        )
        require(
          common.mint.flatten === List.single((common.policy, nodeToken(), BigInt(1))),
          "Must mint an node token NFT value for this linked list"
        )

    def deinit(common: Common): Unit = common.inputs match
        case List.Cons(Node(_value, Cons(_key, None, _)), List.Nil) =>
            require(common.outputs.isEmpty, "Must not produce nodes")
            require(
              common.mint.flatten === List.single((common.policy, nodeToken(), BigInt(-1))),
              "Must burn an node token NFT value for this linked list"
            )
        case _ =>
            fail("There must be a single head node input,\nthe linked list must be empty")

    def insert(common: Common, insertKey: PubKeyHash, cell: Cons): Node =
        val parentIn = common.inputs match
            case List.Cons(parentIn, List.Nil) => parentIn
            case _                             => fail("There must be a single covering node input")
        val PubKeyHash(key) = insertKey
        val (parentOut, insertNode) = common.outputs match
            case List.Cons(fstOut, List.Cons(sndOut, List.Nil)) => fstOut.sortByKey(sndOut, key)
            case _ => fail("There must be only a parent and an inserted node outputs")

        require(
          insertNode.cell.key === Some(key),
          "The inserted key must be present at the cell of inserted node"
        )
        require(insertNode.cell.ref === cell.ref, "The inserted node must have the expected ref")
        require(parentOut.cell.key === cell.key, "Parent key must be preserved")
        require(
          parentOut.cell.ref === Some(key),
          "The inserted key must be referenced by the parent's key at outputs"
        )
        require(parentOut.cell.data === parentIn.cell.data, "Parent data must be preserved")
        require(parentOut.value === parentIn.value, "Parent node's value must be preserved")
        require(
          common.mint.flatten === List.single((common.policy, nodeToken(key), BigInt(1))),
          "Must mint an NFT value for the inserted key for this linked list"
        )
        parentIn

    def remove(common: Common, removeKey: PubKeyHash, cell: Cons): Node =
        val PubKeyHash(key) = removeKey
        val (parentIn, removeNode) = common.inputs match
            case List.Cons(fstIn, List.Cons(sndIn, List.Nil)) => fstIn.sortByKey(sndIn, key)
            case _ => fail("There must be parent and remove node inputs only")
        val parentOut = common.outputs match
            case List.Cons(parentOut, List.Nil) => parentOut
            case _                              => fail("There must be a single parent output")
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
          common.mint.flatten === List.single((common.policy, nodeToken(key), BigInt(-1))),
          "Must burn an NFT value for the removed key for this linked list"
        )
        removeNode

    def prepend(common: Common, prependKey: PubKeyHash, cell: Cons): Unit =
        require(cell.key.isEmpty, "A covering cell must be the head of the linked list")
        val _ = insert(common, prependKey, cell)

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
