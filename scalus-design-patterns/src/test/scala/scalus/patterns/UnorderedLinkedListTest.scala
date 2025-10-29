package scalus.patterns

import org.scalatest.funsuite.AnyFunSuite
import scalus.*
import scalus.builtin.ByteString.*
import scalus.builtin.Data
import scalus.builtin.Data.toData
import scalus.ledger.api.v2.OutputDatum
import scalus.ledger.api.v2.TxOut
import scalus.ledger.api.v3.*
import scalus.prelude.*
import scalus.prelude.Option.*
import scalus.testkit.Mock
import scalus.testkit.ScalusTest
import scalus.uplc.eval.Result

import scalus.examples.{UnorderedLinkedListContract, UnorderedNodeAction}
import scalus.patterns.UnorderedLinkedList as LinkedList

import scala.language.implicitConversions

class UnorderedLinkedListTest extends AnyFunSuite, ScalusTest:
    given scalus.Compiler.Options = scalus.Compiler.Options(
      targetLoweringBackend = scalus.Compiler.TargetLoweringBackend.SirToUplcV3Lowering,
      generateErrorTraces = true,
      optimizeUplc = true,
      debug = false
    )
    import Cons.{cons, head}
    import scalus.uplc.eval.ExBudget

    extension (self: TxOut) def token: Value = self.value.withoutLovelace

    val policyId = Mock.mockScriptHash(1)
    val initRef = Mock.mockTxOutRef(1, 1)
    val removeRef = Mock.mockTxOutRef(3, 2)
    val parentRef = Mock.mockTxOutRef(3, 1)
    val royalty = Mock.mockScriptHash(2)
    val scriptAddress = Address.fromScriptHash(policyId)
    val config = Config(
      init = initRef,
      deadline = 86_400_000L,
      penalty = Address.fromScriptHash(royalty)
    )
    // hashes must be ordered
    val user1 = Mock.mockPubKeyHash(8).hash
    val user2 = Mock.mockPubKeyHash(16).hash
    val user3 = Mock.mockPubKeyHash(32).hash
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
        budget: ExBudget,
        inputs: List[TxInInfo],
        outputs: List[TxOut] = List.Nil,
        mint: Value,
        action: UnorderedNodeAction,
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
          id = Mock.mockTxOutRef(2, 1).id
        )
        val result = UnorderedLinkedListContract
            .make(config)
            .runWithDebug(
              scriptContext = ScriptContext(
                txInfo = tx,
                redeemer = action.toData,
                scriptInfo = ScriptInfo.MintingScript(policyId)
              )
            )
        if fails != result.isSuccess then
            if result.budget.cpu > budget.cpu || result.budget.memory > budget.memory then
                println(result.budget)
                println("Costs: " + result.costs)
                fail("Performance regression")
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
          budget = ExBudget.fromCpuAndMemory(315866792, 1089434),
          inputs = List.single(TxInInfo(initRef, nodeIn)),
          outputs = List.single(nodeOut),
          mint = nodeOut.token,
          action = UnorderedNodeAction.Init
        )

    test("Verify that a linked list can be properly de-initialized (burn)"):
        val nodeIn = node(head(), burn = true, lovelace = 4_000_000)
        TestCase(
          budget = ExBudget.fromCpuAndMemory(228692388, 777264),
          inputs = List.single(TxInInfo(initRef, nodeIn)),
          mint = nodeIn.token,
          action = UnorderedNodeAction.Deinit
        )

    test("Verify that de-initialization fails if the list is not empty"):
        val nonEmptyCell = cons(user1, Some(user2))
        val nodeIn = node(nonEmptyCell, user2, burn = true, lovelace = 9_000_000)
        val nodeOut = empty(burn = true)
        TestCase(
          budget = ExBudget.fromCpuAndMemory(495962460, 1683061),
          inputs = List.single(TxInInfo(initRef, nodeIn)),
          mint = nodeOut.token,
          action = UnorderedNodeAction.Deinit,
          fails = true,
        )

    test("Verify that a node can be removed from the linked list"):
        val parentCell = cons(user1, Some(user2))
        val parentIn = node(parentCell, user1, lovelace = 9_000_000)
        val remCell = cons(user2)
        val removeIn = node(remCell, user2, burn = true, lovelace = parentIn.value.getLovelace)
        val updatedCell = parentCell.copy(ref = remCell.ref)
        val parentOut = parentIn.copy(datum = OutputDatum.OutputDatum(updatedCell.toData))
        TestCase(
          budget = ExBudget.fromCpuAndMemory(592887819, 2004736),
          inputs = List(TxInInfo(removeRef, removeIn), TxInInfo(parentRef, parentIn)),
          outputs = List.single(parentOut),
          mint = removeIn.token,
          action = UnorderedNodeAction.Remove(PubKeyHash(user2), updatedCell),
          validRange = Interval.entirelyBetween(1000L, 2000L),
          signedBy = remCell.key
        )

    test("Verify that the first node can be removed with a non-empty covering"):
        val parentCell = head(Some(user1))
        val parentIn = node(parentCell, lovelace = 9_000_000)
        val remCell = cons(user1, Some(user2))
        val removeIn = node(remCell, user1, burn = true, lovelace = parentIn.value.getLovelace)
        val updatedCell = parentCell.copy(ref = remCell.ref)
        val parentOut = parentIn.copy(datum = OutputDatum.OutputDatum(updatedCell.toData))
        TestCase(
          budget = ExBudget.fromCpuAndMemory(590553133, 1996487),
          inputs = List(TxInInfo(removeRef, removeIn), TxInInfo(parentRef, parentIn)),
          outputs = List.single(parentOut),
          mint = removeIn.token,
          action = UnorderedNodeAction.Remove(PubKeyHash(user1), updatedCell),
          validRange = Interval.entirelyBetween(1000L, 2000L),
          signedBy = remCell.key
        )

    test("Verify that a non-first node can be removed with an empty covering"):
        val parentCell = cons(user1, Some(user2))
        val remCell = cons(user2)
        val parentIn = node(parentCell, user1, lovelace = 9_000_000)
        val removeIn = node(remCell, user2, burn = true, lovelace = parentIn.value.getLovelace)
        val removeRef = Mock.mockTxOutRef(3, 2)
        val updatedCell = parentCell.copy(ref = remCell.ref)
        val parentOut = parentIn.copy(datum = OutputDatum.OutputDatum(updatedCell.toData))
        TestCase(
          budget = ExBudget.fromCpuAndMemory(592887819, 2004736),
          inputs = List(TxInInfo(removeRef, removeIn), TxInInfo(parentRef, parentIn)),
          outputs = List.single(parentOut),
          mint = removeIn.token,
          action = UnorderedNodeAction.Remove(PubKeyHash(user2), updatedCell),
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
          budget = ExBudget.fromCpuAndMemory(509647608, 1730487),
          inputs = List(TxInInfo(removeRef, removeIn), TxInInfo(parentRef, parentIn)),
          outputs = List.single(parentOut),
          mint = removeIn.token,
          action = UnorderedNodeAction.Remove(PubKeyHash(user1), updatedCell),
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
          budget = ExBudget.fromCpuAndMemory(593743028, 2016535),
          inputs = List.single(TxInInfo(parentRef, parentIn)),
          outputs = List(parentOut, newOutput),
          mint = newOutput.token,
          action = UnorderedNodeAction.Prepend(PubKeyHash(user1), covering),
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
          budget = ExBudget.fromCpuAndMemory(593973078, 2017393),
          inputs = List.single(TxInInfo(parentRef, parentIn)),
          outputs = List(parentOut, newOutput),
          mint = newOutput.token,
          action = UnorderedNodeAction.Prepend(PubKeyHash(user2), covering),
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
          budget = ExBudget.fromCpuAndMemory(593743028, 2016535),
          inputs = List.single(TxInInfo(parentRef, parentIn)),
          outputs = List(parentOut, newOutput),
          mint = newOutput.token,
          action = UnorderedNodeAction.Append(PubKeyHash(user2), covering),
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
          budget = ExBudget.fromCpuAndMemory(593973078, 2017393),
          inputs = List.single(TxInInfo(parentRef, parentIn)),
          outputs = List(parentOut, newOutput),
          mint = newOutput.token,
          action = UnorderedNodeAction.Append(PubKeyHash(user2), covering),
          validRange = Interval.entirelyBetween(1000L, 2000L),
          signedBy = newCell.key,
          fails = true
        )
