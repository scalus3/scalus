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

    val policyId = Mock.mockScriptHash(1)
    val txRef = Mock.mockTxOutRef(2, 1)
    val initRef = Mock.mockTxOutRef(1, 1)
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
    val emptyKey = hex""

    def key(token: TokenName = hex""): TokenName = LinkedList.nodeToken() ++ token

    case class TestCase(
        exBudget: ExBudget
    ):

        def check: Unit =
            val mintedValue = Value(cs = policyId, tn = key(), v = 1)
            val cell: Cons = head()
            val headOutput = TxOut(
              address = scriptAddress,
              value = Value.lovelace(4_000_000) + mintedValue,
              datum = OutputDatum.OutputDatum(cell.toData)
            )
            val tx = TxInfo.placeholder.copy(
              inputs = List(
                TxInInfo(
                  outRef = initRef,
                  resolved = TxOut(
                    address = scriptAddress,
                    value = Value.lovelace(4_000_000),
                    datum = OutputDatum.NoOutputDatum
                  )
                )
              ),
              outputs = List.single(headOutput),
              mint = mintedValue,
              id = txRef.id
            )
            LinkedListContract
                .make(config)
                .runWithDebug(
                  scriptContext = ScriptContext(
                    txInfo = tx,
                    redeemer = NodeAction.Init.toData,
                    scriptInfo = ScriptInfo.MintingScript(policyId)
                  )
                ) match
                case Result.Success(term, budget, costs, logs) =>
                    if budget.cpu <= exBudget.cpu && budget.memory <= exBudget.memory then
                        println(budget)
                        fail("Performance regression")
                case Result.Failure(exception, budget, costs, logs) =>
                    logs.foreach(println)
                    fail(s"Script failed with exception: ${exception.getMessage}")

    test("Verify that a linked list can be properly initialized"):
        val mintedValue = Value(cs = policyId, tn = key(), v = 1)
        val cell: Cons = head()
        val headOutput = TxOut(
          address = scriptAddress,
          value = Value.lovelace(4_000_000) + mintedValue,
          datum = OutputDatum.OutputDatum(cell.toData)
        )
        val tx = TxInfo.placeholder.copy(
          inputs = List(
            TxInInfo(
              outRef = initRef,
              resolved = TxOut(
                address = scriptAddress,
                value = Value.lovelace(4_000_000),
                datum = OutputDatum.NoOutputDatum
              )
            )
          ),
          outputs = List.single(headOutput),
          mint = mintedValue,
          id = txRef.id
        )
        val result = LinkedListContract
            .make(config)
            .runWithDebug(
              scriptContext = ScriptContext(
                txInfo = tx,
                redeemer = NodeAction.Init.toData,
                scriptInfo = ScriptInfo.MintingScript(policyId)
              )
            )
        if result.isFailure then
            result.logs.foreach(println)
            println(result)
        else println(result.budget)
        val budget = ExBudget.fromCpuAndMemory(143129594, 515159)
        assert(
          result.budget.cpu <= budget.cpu && result.budget.memory <= budget.memory,
          "Performance regression"
        )
        assert(result.isSuccess, "Linked list initialization should succeed")

    test("Verify that a linked list can be properly de-initialized (burn)"):
        val burnValue = Value(cs = policyId, tn = key(), v = -1)
        val cell: Cons = head()
        val nodeOut = TxOut(
          address = scriptAddress,
          value = Value.lovelace(4_000_000) + burnValue,
          datum = OutputDatum.OutputDatum(cell.toData)
        )
        val tx = TxInfo.placeholder.copy(
          inputs = List(TxInInfo(initRef, nodeOut)),
          mint = burnValue,
          id = txRef.id
        )
        val result = LinkedListContract
            .make(config)
            .runWithDebug(
              scriptContext = ScriptContext(
                txInfo = tx,
                redeemer = NodeAction.Deinit.toData,
                scriptInfo = ScriptInfo.MintingScript(policyId)
              )
            )
        if result.isFailure then
            result.logs.foreach(println)
            println(result)
        else println(result.budget)
        val budget = ExBudget.fromCpuAndMemory(85976699, 303776)
        assert(
          result.budget.cpu <= budget.cpu && result.budget.memory <= budget.memory,
          "Performance regression"
        )
        assert(result.isSuccess, "Linked list de-initialization should succeed")

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
        val tx = TxInfo.placeholder.copy(
          inputs = List(TxInInfo(initRef, nodeOut)),
          mint = burnValue,
          id = txRef.id
        )
        val result = LinkedListContract
            .make(config)
            .runWithDebug(
              scriptContext = ScriptContext(
                txInfo = tx,
                redeemer = NodeAction.Deinit.toData,
                scriptInfo = ScriptInfo.MintingScript(policyId)
              )
            )
        if result.isSuccess then
            result.logs.foreach(println)
            println(result)
        else println(result.budget)
        val budget = ExBudget.fromCpuAndMemory(63261784, 222384)
        assert(
          result.budget.cpu <= budget.cpu && result.budget.memory <= budget.memory,
          "Performance regression"
        )
        assert(result.isFailure, "De-initialization should fail if the list is not empty")

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
        val tx = TxInfo.placeholder.copy(
          inputs = List.single(TxInInfo(parentRef, parentOutput)),
          outputs = List(
            parentOutput.copy(datum = OutputDatum.OutputDatum(head(newCell.key).toData)),
            newOutput
          ),
          mint = Value(cs = policyId, tn = key(user1), v = 1),
          validRange = Interval.entirelyBetween(1000L, 2000L),
          signatories = List.single(PubKeyHash(user1)),
          id = txRef.id
        )
        val result = LinkedListContract
            .make(config)
            .runWithDebug(
              scriptContext = ScriptContext(
                txInfo = tx,
                redeemer = NodeAction.Insert(PubKeyHash(user1), parentCell).toData,
                scriptInfo = ScriptInfo.MintingScript(policyId)
              )
            )
        if result.isFailure then
            result.logs.foreach(println)
            println(result)
        else println(result.budget)
        val budget = ExBudget.fromCpuAndMemory(303439106, 1054709)
        assert(
          result.budget.cpu <= budget.cpu && result.budget.memory <= budget.memory,
          "Performance regression"
        )
        assert(result.isSuccess, "Linked list insertion should succeed")

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
        val tx = TxInfo.placeholder.copy(
          inputs = List.single(TxInInfo(parentRef, parentOutput)),
          outputs = List(
            parentOutput.copy(datum =
                OutputDatum.OutputDatum(cons(key = user1, ref = Some(user2)).toData)
            ),
            newOutput
          ),
          mint = Value(cs = policyId, tn = key(user2), v = 1),
          validRange = Interval.entirelyBetween(1000L, 2000L),
          signatories = List.single(PubKeyHash(user2)),
          id = txRef.id
        )
        val result = LinkedListContract
            .make(config)
            .runWithDebug(
              scriptContext = ScriptContext(
                txInfo = tx,
                redeemer = NodeAction.Insert(PubKeyHash(user2), parentCell).toData,
                scriptInfo = ScriptInfo.MintingScript(policyId)
              )
            )
        if result.isFailure then
            result.logs.foreach(println)
            println(result)
        else println(result.budget)
        val budget = ExBudget.fromCpuAndMemory(305468667, 1062506)
        assert(
          result.budget.cpu <= budget.cpu && result.budget.memory <= budget.memory,
          "Performance regression"
        )
        assert(result.isSuccess, "Linked list insertion should succeed")

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
        val tx = TxInfo.placeholder.copy(
          inputs = List.single(TxInInfo(parentRef, parentOutput)),
          outputs = List(
            parentOutput.copy(datum =
                OutputDatum.OutputDatum(cons(key = user1, ref = Some(user2)).toData)
            ),
            newOutput
          ),
          mint = Value(cs = policyId, tn = key(user2), v = 1),
          validRange = Interval.entirelyBetween(1000L, 2000L),
          signatories = List.single(PubKeyHash(user2)),
          id = txRef.id
        )
        val user3 = Mock.mockPubKeyHash(3).hash
        val updatedCell = cons(user2, Some(user3))
        val result = LinkedListContract
            .make(config)
            .runWithDebug(
              scriptContext = ScriptContext(
                txInfo = tx,
                redeemer = NodeAction.Insert(PubKeyHash(user2), parentCell.copy()).toData,
                scriptInfo = ScriptInfo.MintingScript(policyId)
              )
            )
        if result.isFailure then
            result.logs.foreach(println)
            println(result)
        else println(result.budget)
        val budget = ExBudget.fromCpuAndMemory(305468667, 1062506)
        assert(
          result.budget.cpu <= budget.cpu && result.budget.memory <= budget.memory,
          "Performance regression"
        )
        assert(result.isSuccess, "Linked list insertion should succeed if covering isn't empty")

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
        val tx = TxInfo.placeholder.copy(
          inputs = List.single(TxInInfo(parentRef, parentOutput)),
          outputs = List(
            parentOutput.copy(datum =
                OutputDatum.OutputDatum(cons(key = user2, ref = Some(user2)).toData)
            ),
            newOutput
          ),
          mint = Value(cs = policyId, tn = key(user2), v = 1),
          validRange = Interval.entirelyBetween(1000L, 2000L),
          signatories = List.single(PubKeyHash(user2)),
          id = txRef.id
        )
        val result = LinkedListContract
            .make(config)
            .runWithDebug(
              scriptContext = ScriptContext(
                txInfo = tx,
                redeemer = NodeAction.Insert(PubKeyHash(user2), parentCell).toData,
                scriptInfo = ScriptInfo.MintingScript(policyId)
              )
            )
        if result.isSuccess then
            result.logs.foreach(println)
            println(result)
        else println(result.budget)
        val budget = ExBudget.fromCpuAndMemory(305468667, 1062506)
        assert(
          result.budget.cpu <= budget.cpu && result.budget.memory <= budget.memory,
          "Performance regression"
        )
        assert(result.isFailure, "Linked list insertion should fail for a duplicate key")

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
        val tx = TxInfo.placeholder.copy(
          inputs = List.single(TxInInfo(parentRef, parentOutput)),
          outputs = List(
            parentOutput.copy(datum =
                OutputDatum.OutputDatum(cons(key = user2, ref = Some(user1)).toData)
            ),
            newOutput
          ),
          mint = Value(cs = policyId, tn = key(user1), v = 1),
          validRange = Interval.entirelyBetween(1000L, 2000L),
          signatories = List.single(PubKeyHash(user1)),
          id = txRef.id
        )
        val result = LinkedListContract
            .make(config)
            .runWithDebug(
              scriptContext = ScriptContext(
                txInfo = tx,
                redeemer = NodeAction.Insert(PubKeyHash(user1), parentCell).toData,
                scriptInfo = ScriptInfo.MintingScript(policyId)
              )
            )
        if result.isSuccess then
            result.logs.foreach(println)
            println(result)
        else println(result.budget)
        val budget = ExBudget.fromCpuAndMemory(305468667, 1062506)
        assert(
          result.budget.cpu <= budget.cpu && result.budget.memory <= budget.memory,
          "Performance regression"
        )
        assert(result.isFailure, "Linked list insertion should fail for an unordered key")

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
        val tx = TxInfo.placeholder.copy(
          inputs = List.single(TxInInfo(parentRef, parentOutput)),
          outputs = List(
            parentOutput.copy(datum = OutputDatum.OutputDatum(head(newCell.key).toData)),
            newOutput
          ),
          mint = Value(cs = policyId, tn = key(emptyKey), v = 1),
          validRange = Interval.entirelyBetween(1000L, 2000L),
          signatories = List.single(PubKeyHash(emptyKey)),
          id = txRef.id
        )
        val result = LinkedListContract
            .make(config)
            .runWithDebug(
              scriptContext = ScriptContext(
                txInfo = tx,
                redeemer = NodeAction.Insert(PubKeyHash(emptyKey), parentCell).toData,
                scriptInfo = ScriptInfo.MintingScript(policyId)
              )
            )
        if result.isSuccess then
            result.logs.foreach(println)
            println(result)
        else println(result.budget)
        val budget = ExBudget.fromCpuAndMemory(146423245, 522047)
        assert(
          result.budget.cpu <= budget.cpu && result.budget.memory <= budget.memory,
          "Performance regression"
        )
        assert(result.isFailure, "Linked list insertion should fail if key is empty")

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
        val tx = TxInfo.placeholder.copy(
          inputs = List(
            TxInInfo(removeRef, removeOutput),
            TxInInfo(parentRef, parentOutput)
          ),
          outputs = List.single(updatedOutput),
          mint = burnValue,
          validRange = Interval.entirelyBetween(1000L, 2000L),
          signatories = List.single(PubKeyHash(user2)),
          id = txRef.id
        )
        val result = LinkedListContract
            .make(config)
            .runWithDebug(
              scriptContext = ScriptContext(
                txInfo = tx,
                redeemer = NodeAction
                    .Remove(PubKeyHash(user2), updatedCell)
                    .toData,
                scriptInfo = ScriptInfo.MintingScript(policyId)
              )
            )
        if result.isFailure then
            result.logs.foreach(println)
            println(result)
        else println(result.budget)
        val budget = ExBudget.fromCpuAndMemory(338398996, 1165209)
        assert(
          result.budget.cpu <= budget.cpu && result.budget.memory <= budget.memory,
          "Performance regression"
        )
        assert(result.isSuccess, "Linked list removal should succeed")

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
        val tx = TxInfo.placeholder.copy(
          inputs = List(
            TxInInfo(removeRef, removeOutput),
            TxInInfo(parentRef, parentOutput)
          ),
          outputs = List.single(updatedOutput),
          mint = burnValue,
          validRange = Interval.entirelyBetween(1000L, 2000L),
          signatories = List.single(PubKeyHash(emptyKey)),
          id = txRef.id
        )
        val result = LinkedListContract
            .make(config)
            .runWithDebug(
              scriptContext = ScriptContext(
                txInfo = tx,
                redeemer = NodeAction
                    .Remove(PubKeyHash(emptyKey), updatedCell)
                    .toData,
                scriptInfo = ScriptInfo.MintingScript(policyId)
              )
            )
        if result.isSuccess then
            result.logs.foreach(println)
            println(result)
        else println(result.budget)
        val budget = ExBudget.fromCpuAndMemory(338398996, 1165209)
        assert(
          result.budget.cpu <= budget.cpu && result.budget.memory <= budget.memory,
          "Performance regression"
        )
        assert(result.isFailure, "Linked list removal should fail if key is empty")

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
        val removeRef = Mock.mockTxOutRef(3, 2)
        val updatedCell = head(Some(user2))
        val updatedOutput = TxOut(
          scriptAddress,
          parentValue,
          OutputDatum.OutputDatum(updatedCell.toData)
        )
        val burnValue = Value(cs = policyId, tn = key(user1), v = -1)
        val tx = TxInfo.placeholder.copy(
          inputs = List(
            TxInInfo(removeRef, removeOutput),
            TxInInfo(parentRef, parentOutput)
          ),
          outputs = List.single(updatedOutput),
          mint = burnValue,
          validRange = Interval.entirelyBetween(1000L, 2000L),
          signatories = List.single(PubKeyHash(user1)),
          id = txRef.id
        )
        val result = LinkedListContract
            .make(config)
            .runWithDebug(
              scriptContext = ScriptContext(
                txInfo = tx,
                redeemer = NodeAction
                    .Remove(PubKeyHash(user1), updatedCell)
                    .toData,
                scriptInfo = ScriptInfo.MintingScript(policyId)
              )
            )
        if result.isSuccess then
            result.logs.foreach(println)
            println(result)
        else println(result.budget)
        val budget = ExBudget.fromCpuAndMemory(338398996, 1165209)
        assert(
          result.budget.cpu <= budget.cpu && result.budget.memory <= budget.memory,
          "Performance regression"
        )
        assert(
          result.isFailure,
          "Linked list removal should fail if covering reference isn't empty for the first node"
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
        val tx = TxInfo.placeholder.copy(
          inputs = List(
            TxInInfo(removeRef, removeOutput),
            TxInInfo(parentRef, parentOutput)
          ),
          outputs = List.single(updatedOutput),
          mint = burnValue,
          validRange = Interval.entirelyBetween(1000L, 2000L),
          signatories = List.single(PubKeyHash(user2)),
          id = txRef.id
        )
        val result = LinkedListContract
            .make(config)
            .runWithDebug(
              scriptContext = ScriptContext(
                txInfo = tx,
                redeemer = NodeAction
                    .Remove(PubKeyHash(user2), updatedCell)
                    .toData,
                scriptInfo = ScriptInfo.MintingScript(policyId)
              )
            )
        if result.isSuccess then
            result.logs.foreach(println)
            println(result)
        else println(result.budget)
        val budget = ExBudget.fromCpuAndMemory(338398996, 1165209)
        assert(
          result.budget.cpu <= budget.cpu && result.budget.memory <= budget.memory,
          "Performance regression"
        )
        assert(
          result.isFailure,
          "Linked list removal should fail if covering reference is empty for a non-first node"
        )
