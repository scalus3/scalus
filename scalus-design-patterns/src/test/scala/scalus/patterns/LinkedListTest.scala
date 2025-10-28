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

import scala.language.implicitConversions

class LinkedListTest extends AnyFunSuite, ScalusTest:
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
    val user1 = Mock.mockPubKeyHash(1).hash
    val user2 = Mock.mockPubKeyHash(2).hash
    val user3 = Mock.mockPubKeyHash(3).hash
    val emptyKey = hex""

    def key(token: TokenName = hex""): TokenName = LinkedList.nodeToken() ++ token

    def node(
        cell: Cons,
        key: TokenName = key(),
        burn: Boolean = false,
        lovelace: BigInt = 0
    ): TxOut = TxOut(
      address = scriptAddress,
      value = Value.lovelace(lovelace) + Value(cs = policyId, tn = key, v = if burn then -1 else 1),
      datum = OutputDatum.OutputDatum(cell.toData)
    )

    case class TestCase(
        budget: ExBudget,
        inputs: List[TxInInfo],
        outputs: List[TxOut] = List.Nil,
        mint: Value,
        action: NodeAction,
        validRange: Interval = Interval.always,
        signedBy: Option[PubKeyHash] = None,
        shouldFail: Boolean = false
    ):
        def check: Unit =
            val tx = TxInfo.placeholder.copy(
              inputs,
              outputs = outputs,
              mint = mint,
              validRange = validRange,
              signatories = signedBy.map(List.single).getOrElse(List.Nil),
              id = Mock.mockTxOutRef(2, 1).id
            )
            val result = LinkedListContract
                .make(config)
                .runWithDebug(
                  scriptContext = ScriptContext(
                    txInfo = tx,
                    redeemer = action.toData,
                    scriptInfo = ScriptInfo.MintingScript(policyId)
                  )
                )
            if shouldFail != result.isSuccess then
                if result.budget.cpu <= budget.cpu && result.budget.memory <= budget.memory then
                    println(result.budget)
                    println(result.costs)
                    fail("Performance regression")
            else
                result.logs.foreach(println)
                val reason = result match
                    case Result.Failure(exception, _, _, _) =>
                        s"Script failed with exception: ${exception.getMessage}"
                    case _ => "Script should fail, but didn't"
                fail(reason)

    test("Verify that a linked list can be properly initialized"):
        val headOutput = node(head(), lovelace = 4_000_000)
        val headInput = headOutput.copy(datum = OutputDatum.NoOutputDatum)
        TestCase(
          budget = ExBudget.fromCpuAndMemory(143129594, 515159),
          inputs = List(
            TxInInfo(
              outRef = initRef,
              resolved = headInput
            )
          ),
          outputs = List.single(headOutput),
          mint = headOutput.token,
          action = NodeAction.Init
        )

    test("Verify that a linked list can be properly de-initialized (burn)"):
        val nodeOut = node(head(), burn = true, lovelace = 4_000_000)
        TestCase(
          budget = ExBudget.fromCpuAndMemory(85976699, 303776),
          inputs = List(TxInInfo(initRef, nodeOut)),
          mint = nodeOut.token,
          action = NodeAction.Deinit
        )

    test("Verify that de-initialization fails if the list is not empty"):
        val nonEmptyCell = cons(
          key = user1,
          ref = Some(user2)
        )
        val nodeValue = Value.lovelace(9_000_000) + Value(cs = policyId, tn = key(user2), v = -1)
        val nodeOut = TxOut(
          address = scriptAddress,
          value = nodeValue,
          datum = OutputDatum.OutputDatum(nonEmptyCell.toData)
        )
        val burnValue = Value(cs = policyId, tn = key(), v = -1)
        TestCase(
          budget = ExBudget.fromCpuAndMemory(63261784, 222384),
          inputs = List(TxInInfo(initRef, nodeOut)),
          mint = burnValue,
          action = NodeAction.Deinit,
          shouldFail = true,
        )

    test("Verify that the first node can be inserted into the linked list"):
        val parentCell: Cons = head()
        val parentValue =
            Value.lovelace(4_000_000) + Value(cs = policyId, tn = key(), v = 1)
        val parentOutput = TxOut(
          scriptAddress,
          parentValue,
          OutputDatum.OutputDatum(parentCell.toData)
        )
        val newCell = cons(user1)
        val insertValue = Value.lovelace(9_000_000) + Value(cs = policyId, tn = key(user1), v = 1)
        val newOutput = TxOut(
          scriptAddress,
          insertValue,
          OutputDatum.OutputDatum(newCell.toData)
        )
        TestCase(
          budget = ExBudget.fromCpuAndMemory(303439106, 1054709),
          inputs = List.single(TxInInfo(parentRef, parentOutput)),
          outputs = List(
            parentOutput.copy(datum = OutputDatum.OutputDatum(head(newCell.key).toData)),
            newOutput
          ),
          mint = Value(cs = policyId, tn = key(user1), v = 1),
          action = NodeAction.Insert(PubKeyHash(user1), parentCell),
          validRange = Interval.entirelyBetween(1000L, 2000L),
          signedBy = Some(PubKeyHash(user1))
        )

    test("Verify that a new node can be inserted into the linked list"):
        val parentCell = cons(user1)
        val parentValue = Value.lovelace(9_000_000) + Value(cs = policyId, tn = key(user1), v = 1)
        val parentOutput = TxOut(
          scriptAddress,
          parentValue,
          OutputDatum.OutputDatum(parentCell.toData)
        )
        val newCell = cons(user2)
        val insertValue = Value.lovelace(9_000_000) + Value(cs = policyId, tn = key(user2), v = 1)
        val newOutput = TxOut(
          scriptAddress,
          insertValue,
          OutputDatum.OutputDatum(newCell.toData)
        )
        TestCase(
          budget = ExBudget.fromCpuAndMemory(305468667, 1062506),
          inputs = List.single(TxInInfo(parentRef, parentOutput)),
          outputs = List(
            parentOutput.copy(datum =
                OutputDatum.OutputDatum(cons(key = user1, ref = Some(user2)).toData)
            ),
            newOutput
          ),
          mint = Value(cs = policyId, tn = key(user1), v = 1),
          action = NodeAction.Insert(PubKeyHash(user2), parentCell),
          validRange = Interval.entirelyBetween(1000L, 2000L),
          signedBy = Some(PubKeyHash(user2))
        )

    test("Verify that a new node can be inserted into the linked list for a non-empty covering"):
        val parentCell = cons(user1)
        val parentValue = Value.lovelace(9_000_000) + Value(cs = policyId, tn = key(user1), v = 1)
        val parentOutput = TxOut(
          scriptAddress,
          parentValue,
          OutputDatum.OutputDatum(parentCell.toData)
        )
        val newCell = cons(user2)
        val insertValue = Value.lovelace(9_000_000) + Value(cs = policyId, tn = key(user2), v = 1)
        val newOutput = TxOut(
          scriptAddress,
          insertValue,
          OutputDatum.OutputDatum(newCell.toData)
        )
        TestCase(
          budget = ExBudget.fromCpuAndMemory(305468667, 1062506),
          inputs = List.single(TxInInfo(parentRef, parentOutput)),
          outputs = List(
            parentOutput.copy(datum =
                OutputDatum.OutputDatum(cons(key = user1, ref = Some(user2)).toData)
            ),
            newOutput
          ),
          mint = Value(cs = policyId, tn = key(user2), v = 1),
          action = NodeAction.Insert(PubKeyHash(user2), parentCell.copy()),
          validRange = Interval.entirelyBetween(1000L, 2000L),
          signedBy = Some(PubKeyHash(user2))
        )

    test("Verify that a new node insertion fails for a duplicate key"):
        // TODO: test in ledger with keys not at inputs
        val parentCell = cons(user2)
        val parentValue = Value.lovelace(9_000_000) + Value(cs = policyId, tn = key(user2), v = 1)
        val parentOutput = TxOut(
          scriptAddress,
          parentValue,
          OutputDatum.OutputDatum(parentCell.toData)
        )
        val newCell = cons(user2)
        val insertValue = Value.lovelace(9_000_000) + Value(cs = policyId, tn = key(user2), v = 1)
        val newOutput = TxOut(
          scriptAddress,
          insertValue,
          OutputDatum.OutputDatum(newCell.toData)
        )
        TestCase(
          budget = ExBudget.fromCpuAndMemory(305468667, 1062506),
          inputs = List.single(TxInInfo(parentRef, parentOutput)),
          outputs = List(
            parentOutput.copy(datum =
                OutputDatum.OutputDatum(cons(key = user2, ref = Some(user2)).toData)
            ),
            newOutput
          ),
          mint = Value(cs = policyId, tn = key(user2), v = 1),
          action = NodeAction.Insert(PubKeyHash(user2), parentCell),
          validRange = Interval.entirelyBetween(1000L, 2000L),
          signedBy = Some(PubKeyHash(user2)),
          shouldFail = true
        )

    test("Verify that a new node insertion fails for an unordered key"):
        val parentCell = cons(user2)
        val parentValue = Value.lovelace(9_000_000) + Value(cs = policyId, tn = key(user2), v = 1)
        val parentOutput = TxOut(
          scriptAddress,
          parentValue,
          OutputDatum.OutputDatum(parentCell.toData)
        )
        val newCell = cons(user1)
        val insertValue = Value.lovelace(9_000_000) + Value(cs = policyId, tn = key(user1), v = 1)
        val newOutput = TxOut(
          scriptAddress,
          insertValue,
          OutputDatum.OutputDatum(newCell.toData)
        )
        TestCase(
          budget = ExBudget.fromCpuAndMemory(305468667, 1062506),
          inputs = List.single(TxInInfo(parentRef, parentOutput)),
          outputs = List(
            parentOutput.copy(datum =
                OutputDatum.OutputDatum(cons(key = user2, ref = Some(user1)).toData)
            ),
            newOutput
          ),
          mint = Value(cs = policyId, tn = key(user1), v = 1),
          action = NodeAction.Insert(PubKeyHash(user1), parentCell),
          validRange = Interval.entirelyBetween(1000L, 2000L),
          signedBy = Some(PubKeyHash(user1)),
          shouldFail = true
        )

    test("Verify that a new node insertion fails for an empty key"):
        val parentCell: Cons = head()
        val parentValue =
            Value.lovelace(4_000_000) + Value(cs = policyId, tn = key(), v = 1)
        val parentOutput = TxOut(
          scriptAddress,
          parentValue,
          OutputDatum.OutputDatum(parentCell.toData)
        )
        val newCell = cons(emptyKey)
        val insertValue =
            Value.lovelace(9_000_000) + Value(cs = policyId, tn = key(emptyKey), v = 1)
        val newOutput = TxOut(
          scriptAddress,
          insertValue,
          OutputDatum.OutputDatum(newCell.toData)
        )
        TestCase(
          budget = ExBudget.fromCpuAndMemory(146423245, 522047),
          inputs = List.single(TxInInfo(parentRef, parentOutput)),
          outputs = List(
            parentOutput.copy(datum = OutputDatum.OutputDatum(head(newCell.key).toData)),
            newOutput
          ),
          mint = Value(cs = policyId, tn = key(emptyKey), v = 1),
          action = NodeAction.Insert(PubKeyHash(emptyKey), parentCell),
          validRange = Interval.entirelyBetween(1000L, 2000L),
          signedBy = Some(PubKeyHash(emptyKey)),
          shouldFail = true
        )

    test("Verify that a node can be removed from the linked list"):
        val parentCell = cons(key = user1, ref = Some(user2))
        val removeCell = cons(key = user2)
        val parentValue = Value.lovelace(9_000_000) + Value(cs = policyId, tn = key(user1), v = 1)
        val removeValue = Value.lovelace(9_000_000) + Value(cs = policyId, tn = key(user2), v = 1)
        val parentOutput = TxOut(
          scriptAddress,
          parentValue,
          OutputDatum.OutputDatum(parentCell.toData)
        )
        val removeOutput = TxOut(
          scriptAddress,
          removeValue,
          OutputDatum.OutputDatum(removeCell.toData)
        )
        val removeRef = Mock.mockTxOutRef(3, 2)
        val updatedCell = cons(user1)
        val updatedOutput = TxOut(
          scriptAddress,
          parentValue,
          OutputDatum.OutputDatum(updatedCell.toData)
        )
        val burnValue = Value(cs = policyId, tn = key(user2), v = -1)
        TestCase(
          budget = ExBudget.fromCpuAndMemory(338398996, 1165209),
          inputs = List(
            TxInInfo(removeRef, removeOutput),
            TxInInfo(parentRef, parentOutput)
          ),
          outputs = List.single(updatedOutput),
          mint = burnValue,
          action = NodeAction.Remove(PubKeyHash(user2), updatedCell),
          validRange = Interval.entirelyBetween(1000L, 2000L),
          signedBy = Some(PubKeyHash(user2))
        )

    test("Verify that a node removing fails for an empty key"):
        val parentCell = cons(key = user1, ref = Some(emptyKey))
        val removeCell = cons(key = emptyKey)
        val parentValue = Value.lovelace(9_000_000) + Value(cs = policyId, tn = key(user1), v = 1)
        val removeValue =
            Value.lovelace(9_000_000) + Value(cs = policyId, tn = key(emptyKey), v = 1)
        val parentOutput = TxOut(
          scriptAddress,
          parentValue,
          OutputDatum.OutputDatum(parentCell.toData)
        )
        val removeOutput = TxOut(
          scriptAddress,
          removeValue,
          OutputDatum.OutputDatum(removeCell.toData)
        )
        val removeRef = Mock.mockTxOutRef(3, 2)
        val updatedCell = cons(user1)
        val updatedOutput = TxOut(
          scriptAddress,
          parentValue,
          OutputDatum.OutputDatum(updatedCell.toData)
        )
        val burnValue = Value(cs = policyId, tn = key(emptyKey), v = -1)
        TestCase(
          budget = ExBudget.fromCpuAndMemory(338398996, 1165209),
          inputs = List(
            TxInInfo(removeRef, removeOutput),
            TxInInfo(parentRef, parentOutput)
          ),
          outputs = List.single(updatedOutput),
          mint = burnValue,
          action = NodeAction.Remove(PubKeyHash(emptyKey), updatedCell),
          validRange = Interval.entirelyBetween(1000L, 2000L),
          signedBy = Some(PubKeyHash(emptyKey))
        )

    test("Verify that the first node removing fails for a non-empty covering"):
        val parentCell = head(Some(user1))
        val removeCell = cons(user1)
        val parentValue = Value.lovelace(9_000_000) + Value(cs = policyId, tn = key(), v = 1)
        val removeValue = Value.lovelace(9_000_000) + Value(cs = policyId, tn = key(user1), v = 1)
        val parentOutput = TxOut(
          scriptAddress,
          parentValue,
          OutputDatum.OutputDatum(parentCell.toData)
        )
        val removeOutput = TxOut(
          scriptAddress,
          removeValue,
          OutputDatum.OutputDatum(removeCell.toData)
        )
        val updatedCell = head(Some(user2))
        val updatedOutput = TxOut(
          scriptAddress,
          parentValue,
          OutputDatum.OutputDatum(updatedCell.toData)
        )
        val burnValue = Value(cs = policyId, tn = key(user1), v = -1)
        TestCase(
          budget = ExBudget.fromCpuAndMemory(338398996, 1165209),
          inputs = List(
            TxInInfo(removeRef, removeOutput),
            TxInInfo(parentRef, parentOutput)
          ),
          outputs = List.single(updatedOutput),
          mint = burnValue,
          action = NodeAction.Remove(PubKeyHash(user1), updatedCell),
          validRange = Interval.entirelyBetween(1000L, 2000L),
          signedBy = Some(PubKeyHash(user1))
        )

    test("Verify that a non-first node removing fails for an empty covering"):
        val parentCell = cons(key = user1, ref = Some(user2))
        val removeCell = cons(key = user2)
        val parentValue = Value.lovelace(9_000_000) + Value(cs = policyId, tn = key(user1), v = 1)
        val removeValue = Value.lovelace(9_000_000) + Value(cs = policyId, tn = key(user2), v = 1)
        val parentOutput = TxOut(
          scriptAddress,
          parentValue,
          OutputDatum.OutputDatum(parentCell.toData)
        )
        val removeOutput = TxOut(
          scriptAddress,
          removeValue,
          OutputDatum.OutputDatum(removeCell.toData)
        )
        val removeRef = Mock.mockTxOutRef(3, 2)
        val updatedCell = head()
        val updatedOutput = TxOut(
          scriptAddress,
          parentValue,
          OutputDatum.OutputDatum(updatedCell.toData)
        )
        val burnValue = Value(cs = policyId, tn = key(user2), v = -1)
        TestCase(
          budget = ExBudget.fromCpuAndMemory(338398996, 1165209),
          inputs = List(
            TxInInfo(removeRef, removeOutput),
            TxInInfo(parentRef, parentOutput)
          ),
          outputs = List.single(updatedOutput),
          mint = burnValue,
          action = NodeAction.Remove(PubKeyHash(user2), updatedCell),
          validRange = Interval.entirelyBetween(1000L, 2000L),
          signedBy = Some(PubKeyHash(user2))
        )

    test("Verify that a node can be prepended to the head of the linked list"):
        val headCell = head(Some(user1))
        val headValue =
            Value.lovelace(4_000_000) + Value(cs = policyId, tn = key(), v = 1)
        val headOutput = TxOut(
          scriptAddress,
          headValue,
          OutputDatum.OutputDatum(headCell.toData)
        )
        val newCell = cons(user2, ref = Some(user1))
        val newValue = Value.lovelace(9_000_000) + Value(cs = policyId, tn = key(user2), v = 1)
        val newOutput = TxOut(
          scriptAddress,
          newValue,
          OutputDatum.OutputDatum(newCell.toData)
        )
        TestCase(
          budget = ExBudget.fromCpuAndMemory(305000000, 1060000),
          inputs = List.single(TxInInfo(parentRef, headOutput)),
          outputs = List(
            // Updated head now points to user2
            headOutput.copy(
              datum = OutputDatum.OutputDatum(cons(key = emptyKey, ref = Some(user2)).toData)
            ),
            newOutput
          ),
          mint = Value(cs = policyId, tn = key(user2), v = 1),
          action = NodeAction.Prepend(PubKeyHash(user2), headCell),
          validRange = Interval.entirelyBetween(1000L, 2000L),
          signedBy = Some(PubKeyHash(user2))
        )

    test("Verify that prepend fails when the anchor is not the head node"):
        val midCell = cons(key = user1, ref = Some(user2))
        val midValue =
            Value.lovelace(9_000_000) + Value(cs = policyId, tn = key(user1), v = 1)
        val midOutput = TxOut(
          scriptAddress,
          midValue,
          OutputDatum.OutputDatum(midCell.toData)
        )
        val newCell = cons(user3, ref = Some(user1))
        val newValue = Value.lovelace(9_000_000) + Value(cs = policyId, tn = key(user3), v = 1)
        val newOutput = TxOut(
          scriptAddress,
          newValue,
          OutputDatum.OutputDatum(newCell.toData)
        )
        TestCase(
          budget = ExBudget.fromCpuAndMemory(305000000, 1060000),
          inputs = List.single(TxInInfo(parentRef, midOutput)),
          outputs = List(midOutput, newOutput),
          mint = Value(cs = policyId, tn = key(user3), v = 1),
          action = NodeAction.Prepend(PubKeyHash(user3), midCell),
          validRange = Interval.entirelyBetween(1000L, 2000L),
          signedBy = Some(PubKeyHash(user3)),
          shouldFail = true
        )

    test("Verify that a node can be appended to the tail of the linked list"):
        val parentCell = cons(user1)
        val parentOutput = node(parentCell, user1, lovelace = 9_000_000)
        val newCell = cons(user2)
        val newOutput = node(newCell, user2, lovelace = parentOutput.value.getLovelace)
        TestCase(
          budget = ExBudget.fromCpuAndMemory(305000000, 1060000),
          inputs = List.single(TxInInfo(parentRef, parentOutput)),
          outputs = List(
            parentOutput.copy(
              datum = OutputDatum.OutputDatum(parentCell.copy(ref = Some(user2)).toData)
            ),
            newOutput
          ),
          mint = newOutput.token,
          action = NodeAction.Append(PubKeyHash(user2), parentCell),
          validRange = Interval.entirelyBetween(1000L, 2000L),
          signedBy = Some(PubKeyHash(user2))
        )

    test("Verify that append fails when the anchor is not the tail node"):
        val midCell = cons(user1, ref = Some(user2))
        val midValue =
            Value.lovelace(9_000_000) + Value(cs = policyId, tn = key(user1), v = 1)
        val midOutput = TxOut(
          scriptAddress,
          midValue,
          OutputDatum.OutputDatum(midCell.toData)
        )
        val newCell = cons(user3)
        val newValue = Value.lovelace(9_000_000) + Value(cs = policyId, tn = key(user3), v = 1)
        val newOutput = TxOut(
          scriptAddress,
          newValue,
          OutputDatum.OutputDatum(newCell.toData)
        )
        TestCase(
          budget = ExBudget.fromCpuAndMemory(305000000, 1060000),
          inputs = List.single(TxInInfo(parentRef, midOutput)),
          outputs = List(midOutput, newOutput),
          mint = Value(cs = policyId, tn = key(user3), v = 1),
          action = NodeAction.Append(PubKeyHash(user3), midCell),
          validRange = Interval.entirelyBetween(1000L, 2000L),
          signedBy = Some(PubKeyHash(user3)),
          shouldFail = true
        )
