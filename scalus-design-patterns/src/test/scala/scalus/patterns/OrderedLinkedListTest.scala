package scalus.patterns

import org.scalatest.funsuite.AnyFunSuite
import scalus.*
import scalus.builtin.ByteString.*
import scalus.builtin.Data
import scalus.builtin.Data.toData
import scalus.cardano.ledger.ExUnits
import scalus.compiler.Options
import scalus.compiler.sir.TargetLoweringBackend
import scalus.examples.{OrderedLinkedListContract, OrderedNodeAction}
import scalus.ledger.api.v2.{OutputDatum, TxOut}
import scalus.ledger.api.v3.*
import scalus.patterns.OrderedLinkedList as LinkedList
import scalus.prelude.*
import scalus.prelude.Option.*
import scalus.testing.kit.{ScalusTest, TestUtil}
import scalus.uplc.eval.Result

import scala.language.implicitConversions

class OrderedLinkedListTest extends AnyFunSuite, ScalusTest:
    given Options = Options(
      targetLoweringBackend = TargetLoweringBackend.SirToUplcV3Lowering,
      generateErrorTraces = true,
      optimizeUplc = true,
      debug = false
    )
    import Cons.{cons, head}

    extension (self: TxOut) def token: Value = self.value.withoutLovelace

    val policyId = TestUtil.mockScriptHash(1)
    val initRef = TestUtil.mockTxOutRef(1, 1)
    val removeRef = TestUtil.mockTxOutRef(3, 2)
    val parentRef = TestUtil.mockTxOutRef(3, 1)
    val royalty = TestUtil.mockScriptHash(2)
    val scriptAddress = Address.fromScriptHash(policyId)
    val config = Config(
      init = initRef,
      deadline = 86_400_000L,
      penalty = Address.fromScriptHash(royalty)
    )
    // hashes must be ordered
    val user1 = TestUtil.mockPubKeyHash(8).hash
    val user2 = TestUtil.mockPubKeyHash(16).hash
    val user3 = TestUtil.mockPubKeyHash(32).hash
    val emptyKey = hex""

    def nodeToken(key: TokenName = hex""): TokenName = LinkedList.nodeToken() ++ key

    def txOut(
        node: Option[(TokenName, Cons)],
        burn: Boolean,
        lovelace: BigInt
    ): TxOut =
        val (token, datum) = node match
            case Some(token -> datum) =>
                Value(cs = policyId, tn = token, v = if burn then -1 else 1) ->
                    OutputDatum.OutputDatum(datum.toData)
            case _ => Value.zero -> OutputDatum.NoOutputDatum
        TxOut(
          address = scriptAddress,
          value = Value.lovelace(lovelace) + token,
          datum = datum
        )

    def node(
        cell: Cons,
        key: TokenName = hex"",
        burn: Boolean = false,
        lovelace: BigInt = 0
    ): TxOut = txOut(Some(nodeToken(key) -> cell), burn, lovelace)

    def empty(
        burn: Boolean = false,
        lovelace: BigInt = 0
    ): TxOut = txOut(None, burn, lovelace)

    case class TestCase(
        budget: ExUnits,
        inputs: List[TxInInfo],
        outputs: List[TxOut] = List.Nil,
        mint: Value,
        action: OrderedNodeAction,
        validRange: Interval = Interval.always,
        signedBy: Option[TokenName] = None,
        fails: Boolean = false
    ):
        val tx = TxInfo.placeholder.copy(
          inputs,
          outputs = outputs,
          mint = mint,
          validRange = validRange,
          signatories = signedBy.map(key => List.single(PubKeyHash(key))).getOrElse(List.Nil),
          id = TestUtil.mockTxOutRef(2, 1).id
        )
        val result = OrderedLinkedListContract
            .make(config)
            .runWithDebug(
              scriptContext = ScriptContext(
                txInfo = tx,
                redeemer = action.toData,
                scriptInfo = ScriptInfo.MintingScript(policyId)
              )
            )
        if fails != result.isSuccess then
            if result.budget.steps > budget.steps || result.budget.memory > budget.memory
            then
                fail:
                    s"""Performance regression,
                        |expected: $budget,
                        |but got: ${result.budget};
                        |costs: ${result.costs}""".stripMargin
        else
            result.logs.foreach(println)
            val reason = result match
                case Result.Failure(exception, _, _, _) =>
                    s"Script failed with exception: ${exception.getMessage}"
                case _ => "Script should fail, but didn't"
            fail(reason)

    test("Verify that a linked list can be properly initialized"):
        val nodeIn = empty(lovelace = 4_000_000)
        val nodeOut = node(head(), lovelace = nodeIn.value.getLovelace)
        TestCase(
          budget = ExUnits(memory = 1089434, steps = 315866792),
          inputs = List.single(TxInInfo(initRef, nodeIn)),
          outputs = List.single(nodeOut),
          mint = nodeOut.token,
          action = OrderedNodeAction.Init
        )

    test("Verify that a linked list can be properly de-initialized (burn)"):
        val nodeIn = node(head(), burn = true, lovelace = 4_000_000)
        TestCase(
          budget = ExUnits(memory = 777264, steps = 228692388),
          inputs = List.single(TxInInfo(initRef, nodeIn)),
          mint = nodeIn.token,
          action = OrderedNodeAction.Deinit
        )

    test("Verify that de-initialization fails if the list is not empty"):
        val nonEmptyCell = cons(user1, Some(user2))
        val nodeIn = node(nonEmptyCell, user2, burn = true, lovelace = 9_000_000)
        val nodeOut = empty(burn = true)
        TestCase(
          budget = ExUnits(memory = 1683061, steps = 495962460),
          inputs = List.single(TxInInfo(initRef, nodeIn)),
          mint = nodeOut.token,
          action = OrderedNodeAction.Deinit,
          fails = true,
        )

    test("Verify that the first node can be inserted into the linked list"):
        val parentCell: Cons = head()
        val parentIn = node(parentCell, lovelace = 4_000_000)
        val newCell = cons(user1)
        val newOutput = node(newCell, user1, lovelace = 9_000_000)
        val updatedCell = parentCell.copy(ref = newCell.key)
        val parentOut = parentIn.copy(datum = OutputDatum.OutputDatum(updatedCell.toData))
        val covering = parentCell.copy(ref = newCell.ref)
        TestCase(
          budget = ExUnits(memory = 1703570, steps = 501785103),
          inputs = List.single(TxInInfo(parentRef, parentIn)),
          outputs = List(parentOut, newOutput),
          mint = newOutput.token,
          action = OrderedNodeAction.Insert(PubKeyHash(user1), covering),
          validRange = Interval.entirelyBetween(1000L, 2000L),
          signedBy = newCell.key
        )

    test("Verify that a new node can be inserted into the linked list"):
        val parentCell = cons(user1)
        val parentIn = node(parentCell, user1, lovelace = 9_000_000)
        val newCell = cons(user2)
        val newOutput = node(newCell, user2, lovelace = parentIn.value.getLovelace)
        val updatedCell = parentCell.copy(ref = newCell.key)
        val parentOut = parentIn.copy(datum = OutputDatum.OutputDatum(updatedCell.toData))
        val covering = parentCell.copy(ref = newCell.ref)
        TestCase(
          budget = ExUnits(memory = 1717932, steps = 505711679),
          inputs = List.single(TxInInfo(parentRef, parentIn)),
          outputs = List(parentOut, newOutput),
          mint = newOutput.token,
          action = OrderedNodeAction.Insert(PubKeyHash(user2), covering),
          validRange = Interval.entirelyBetween(1000L, 2000L),
          signedBy = newCell.key
        )

    test("Verify that a new node insertion fails with an irrelevant covering key"):
        val parentCell = cons(user1)
        val parentIn = node(parentCell, user1, lovelace = 9_000_000)
        val newCell = cons(user3)
        val newOutput = node(newCell, user3, lovelace = parentIn.value.getLovelace)
        val newParent = cons(user2)
        val updatedCell = newParent.copy(ref = newCell.key)
        val parentOut = parentIn.copy(datum = OutputDatum.OutputDatum(updatedCell.toData))
        val covering = newParent.copy(ref = newCell.ref)
        TestCase(
          budget = ExUnits(memory = 1703570, steps = 501785103),
          inputs = List.single(TxInInfo(parentRef, parentIn)),
          outputs = List(parentOut, newOutput),
          mint = newOutput.token,
          action = OrderedNodeAction.Insert(PubKeyHash(user3), covering),
          validRange = Interval.entirelyBetween(1000L, 2000L),
          signedBy = newCell.key,
          fails = true
        )

    test("Verify that a new node can be inserted into the linked list for a non-empty covering"):
        val parentCell = cons(user1)
        val parentIn = node(parentCell, user1, lovelace = 9_000_000)
        val newCell = cons(user2, Some(user3))
        val newOutput = node(newCell, user2, lovelace = parentIn.value.getLovelace)
        val updatedCell = parentCell.copy(ref = newCell.key)
        val parentOut = parentIn.copy(datum = OutputDatum.OutputDatum(updatedCell.toData))
        val covering = parentCell.copy(ref = newCell.ref)
        TestCase(
          budget = ExUnits(memory = 1717932, steps = 505711679),
          inputs = List.single(TxInInfo(parentRef, parentIn)),
          outputs = List(parentOut, newOutput),
          mint = newOutput.token,
          action = OrderedNodeAction.Insert(PubKeyHash(user2), covering),
          validRange = Interval.entirelyBetween(1000L, 2000L),
          signedBy = newCell.key
        )

    test("Verify that a new node insertion fails for a duplicate key"):
        // TODO: test in ledger with keys not utxo but not at tx inputs
        val parentCell = cons(user2)
        val parentIn = node(parentCell, user2, lovelace = 9_000_000)
        val newCell = cons(user2)
        val newOutput = node(newCell, user2, lovelace = parentIn.value.getLovelace)
        val updatedCell = parentCell.copy(ref = newCell.key)
        val parentOut = parentIn.copy(datum = OutputDatum.OutputDatum(updatedCell.toData))
        val covering = parentCell.copy(ref = newCell.ref)
        TestCase(
          budget = ExUnits(memory = 1717932, steps = 505711679),
          inputs = List.single(TxInInfo(parentRef, parentIn)),
          outputs = List(parentOut, newOutput),
          mint = newOutput.token,
          action = OrderedNodeAction.Insert(PubKeyHash(user2), covering),
          validRange = Interval.entirelyBetween(1000L, 2000L),
          signedBy = newCell.key,
          fails = true
        )

    test("Verify that a new node insertion fails with unordered keys"):
        val parentCell = cons(user2)
        val parentIn = node(parentCell, user2, lovelace = 9_000_000)
        val newCell = cons(user1)
        val newOutput = node(newCell, user1, lovelace = parentIn.value.getLovelace)
        val updatedCell = parentCell.copy(ref = newCell.key)
        val parentOut = parentIn.copy(datum = OutputDatum.OutputDatum(updatedCell.toData))
        val covering = parentCell.copy(ref = newCell.ref)
        TestCase(
          budget = ExUnits(memory = 1458044, steps = 429416754),
          inputs = List.single(TxInInfo(parentRef, parentIn)),
          outputs = List(parentOut, newOutput),
          mint = newOutput.token,
          action = OrderedNodeAction.Insert(PubKeyHash(user1), covering),
          validRange = Interval.entirelyBetween(1000L, 2000L),
          signedBy = newCell.key,
          fails = true
        )

    test("Verify that a new node insertion fails for an empty key"):
        val parentCell = head()
        val parentIn = node(parentCell, lovelace = 4_000_000)
        val newCell = cons(emptyKey)
        val newOutput = node(newCell, lovelace = 9_000_000)
        val updatedCell = parentCell.copy(ref = newCell.key)
        val parentOut = parentIn.copy(datum = OutputDatum.OutputDatum(updatedCell.toData))
        val covering = parentCell.copy(ref = newCell.ref)
        TestCase(
          budget = ExUnits(memory = 2004736, steps = 592887819),
          inputs = List.single(TxInInfo(parentRef, parentIn)),
          outputs = List(parentOut, newOutput),
          mint = newOutput.token,
          action = OrderedNodeAction.Insert(PubKeyHash(emptyKey), covering),
          validRange = Interval.entirelyBetween(1000L, 2000L),
          signedBy = newCell.key,
          fails = true
        )

    test("Verify that a node can be removed from the linked list"):
        val parentCell = cons(user1, Some(user2))
        val parentIn = node(parentCell, user1, lovelace = 9_000_000)
        val remCell = cons(user2)
        val removeIn = node(remCell, user2, burn = true, lovelace = parentIn.value.getLovelace)
        val updatedCell = parentCell.copy(ref = remCell.ref)
        val parentOut = parentIn.copy(datum = OutputDatum.OutputDatum(updatedCell.toData))
        TestCase(
          budget = ExUnits(memory = 2004736, steps = 592887819),
          inputs = List(TxInInfo(removeRef, removeIn), TxInInfo(parentRef, parentIn)),
          outputs = List.single(parentOut),
          mint = removeIn.token,
          action = OrderedNodeAction.Remove(PubKeyHash(user2), updatedCell),
          validRange = Interval.entirelyBetween(1000L, 2000L),
          signedBy = remCell.key
        )

    test("Verify that a node removing fails for an empty key"):
        val parentCell = cons(user1, Some(emptyKey))
        val parentIn = node(parentCell, user1, lovelace = 9_000_000)
        val remCell = cons(emptyKey)
        val removeIn = node(remCell, emptyKey, burn = true, lovelace = parentIn.value.getLovelace)
        val updatedCell = parentCell.copy(ref = remCell.ref)
        val parentOut = parentIn.copy(datum = OutputDatum.OutputDatum(updatedCell.toData))
        TestCase(
          budget = ExUnits(memory = 2004736, steps = 592887819),
          inputs = List(TxInInfo(removeRef, removeIn), TxInInfo(parentRef, parentIn)),
          outputs = List.single(parentOut),
          mint = removeIn.token,
          action = OrderedNodeAction.Remove(PubKeyHash(emptyKey), updatedCell),
          validRange = Interval.entirelyBetween(1000L, 2000L),
          signedBy = remCell.key,
          fails = true
        )

    test("Verify that the first node can be removed with a non-empty covering"):
        val parentCell = head(Some(user1))
        val parentIn = node(parentCell, lovelace = 9_000_000)
        val remCell = cons(user1, Some(user2))
        val removeIn = node(remCell, user1, burn = true, lovelace = parentIn.value.getLovelace)
        val updatedCell = parentCell.copy(ref = remCell.ref)
        val parentOut = parentIn.copy(datum = OutputDatum.OutputDatum(updatedCell.toData))
        TestCase(
          budget = ExUnits(memory = 1996487, steps = 590553133),
          inputs = List(TxInInfo(removeRef, removeIn), TxInInfo(parentRef, parentIn)),
          outputs = List.single(parentOut),
          mint = removeIn.token,
          action = OrderedNodeAction.Remove(PubKeyHash(user1), updatedCell),
          validRange = Interval.entirelyBetween(1000L, 2000L),
          signedBy = remCell.key
        )

    test("Verify that a non-first node can be removed with an empty covering"):
        val parentCell = cons(user1, Some(user2))
        val remCell = cons(user2)
        val parentIn = node(parentCell, user1, lovelace = 9_000_000)
        val removeIn = node(remCell, user2, burn = true, lovelace = parentIn.value.getLovelace)
        val removeRef = TestUtil.mockTxOutRef(3, 2)
        val updatedCell = parentCell.copy(ref = remCell.ref)
        val parentOut = parentIn.copy(datum = OutputDatum.OutputDatum(updatedCell.toData))
        TestCase(
          budget = ExUnits(memory = 2004736, steps = 592887819),
          inputs = List(TxInInfo(removeRef, removeIn), TxInInfo(parentRef, parentIn)),
          outputs = List.single(parentOut),
          mint = removeIn.token,
          action = OrderedNodeAction.Remove(PubKeyHash(user2), updatedCell),
          validRange = Interval.entirelyBetween(1000L, 2000L),
          signedBy = remCell.key
        )

    test("Verify that a node removing fails for a non-relevant covering"):
        val parentCell = head(Some(user1))
        val parentIn = node(parentCell, lovelace = 9_000_000)
        val remCell = cons(user1)
        val removeIn = node(remCell, user1, burn = true, lovelace = parentIn.value.getLovelace)
        val updatedCell = parentCell.copy(ref = Some(user2))
        val parentOut = parentIn.copy(datum = OutputDatum.OutputDatum(updatedCell.toData))
        TestCase(
          budget = ExUnits(memory = 1730487, steps = 509647608),
          inputs = List(TxInInfo(removeRef, removeIn), TxInInfo(parentRef, parentIn)),
          outputs = List.single(parentOut),
          mint = removeIn.token,
          action = OrderedNodeAction.Remove(PubKeyHash(user1), updatedCell),
          validRange = Interval.entirelyBetween(1000L, 2000L),
          signedBy = remCell.key,
          fails = true
        )

    test("Verify that a node can be prepended to the head of the linked list"):
        val parentCell = head(Some(user2))
        val parentIn = node(parentCell, lovelace = 4_000_000)
        val newCell = cons(user1, Some(user2))
        val newOutput = node(newCell, user1, lovelace = 9_000_000)
        val updatedCell = parentCell.copy(ref = newCell.key)
        val parentOut = parentIn.copy(datum = OutputDatum.OutputDatum(updatedCell.toData))
        val covering = parentCell.copy(ref = newCell.ref)
        TestCase(
          budget = ExUnits(memory = 2016535, steps = 593743028),
          inputs = List.single(TxInInfo(parentRef, parentIn)),
          outputs = List(parentOut, newOutput),
          mint = newOutput.token,
          action = OrderedNodeAction.Prepend(PubKeyHash(user1), covering),
          validRange = Interval.entirelyBetween(1000L, 2000L),
          signedBy = newCell.key
        )

    test("Verify that prepend fails when the covering is not the head node"):
        val parentCell = cons(key = user1, ref = Some(user3))
        val parentIn = node(parentCell, user1, lovelace = 9_000_000)
        val newCell = cons(user2, Some(user3))
        val newOutput = node(newCell, user2, lovelace = parentIn.value.getLovelace)
        val updatedCell = parentCell.copy(ref = newCell.key)
        val parentOut = parentIn.copy(datum = OutputDatum.OutputDatum(updatedCell.toData))
        val covering = parentCell.copy(ref = newCell.ref)
        TestCase(
          budget = ExUnits(memory = 2017393, steps = 593973078),
          inputs = List.single(TxInInfo(parentRef, parentIn)),
          outputs = List(parentOut, newOutput),
          mint = newOutput.token,
          action = OrderedNodeAction.Prepend(PubKeyHash(user2), covering),
          validRange = Interval.entirelyBetween(1000L, 2000L),
          signedBy = newCell.key,
          fails = true
        )

    test("Verify that a node can be appended to the tail of the linked list"):
        val parentCell = cons(user1)
        val parentIn = node(parentCell, user1, lovelace = 9_000_000)
        val newCell = cons(user2)
        val newOutput = node(newCell, user2, lovelace = parentIn.value.getLovelace)
        val updatedCell = parentCell.copy(ref = newCell.key)
        val parentOut = parentIn.copy(datum = OutputDatum.OutputDatum(updatedCell.toData))
        val covering = parentCell.copy(ref = newCell.ref)
        TestCase(
          budget = ExUnits(memory = 2016535, steps = 593743028),
          inputs = List.single(TxInInfo(parentRef, parentIn)),
          outputs = List(parentOut, newOutput),
          mint = newOutput.token,
          action = OrderedNodeAction.Append(PubKeyHash(user2), covering),
          validRange = Interval.entirelyBetween(1000L, 2000L),
          signedBy = newCell.key
        )

    test("Verify that append fails when the covering is not the tail node"):
        val parentCell = cons(user1, ref = Some(user3))
        val parentIn = node(parentCell, user1, lovelace = 9_000_000)
        val newCell = cons(user2)
        val newOutput = node(newCell, user2, lovelace = parentIn.value.getLovelace)
        val updatedCell = parentCell.copy(ref = newCell.key)
        val parentOut = parentIn.copy(datum = OutputDatum.OutputDatum(updatedCell.toData))
        val covering = parentCell.copy(ref = newCell.ref)
        TestCase(
          budget = ExUnits(memory = 2017393, steps = 593973078),
          inputs = List.single(TxInInfo(parentRef, parentIn)),
          outputs = List(parentOut, newOutput),
          mint = newOutput.token,
          action = OrderedNodeAction.Append(PubKeyHash(user2), covering),
          validRange = Interval.entirelyBetween(1000L, 2000L),
          signedBy = newCell.key,
          fails = true
        )
