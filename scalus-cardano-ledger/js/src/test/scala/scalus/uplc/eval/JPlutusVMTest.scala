package scalus.uplc.eval

import org.scalatest.funsuite.AnyFunSuite
import scalus.cardano.ledger.{CardanoInfo, Language, MajorProtocolVersion}
import scalus.uplc.*
import scalus.uplc.Constant.given
import scalus.uplc.DefaultFun.*
import scalus.uplc.Term.*
import scalus.uplc.TermDSL.given
import scalus.uplc.builtin.Data
import scalus.utils.Hex

import scalus.utils.scalajs.internal.*
import scala.language.implicitConversions
import scala.scalajs.js

/** `PlutusVM` and `Program` of the npm package give the result of the Scala `PlutusVM` with the
  * same parameters: a machine built once, and a script decoded once.
  */
class JPlutusVMTest extends AnyFunSuite {

    private val mainnetV3 = JEvaluationOptions.mainnet("PlutusV3")

    /** The Scala machine of `EvaluationOptions.mainnet("PlutusV3")`: the reference. */
    private val scalaVM = PlutusVM.makePlutusV3VM(
      MachineParams.fromProtocolParams(CardanoInfo.mainnet.protocolParams, Language.PlutusV3),
      MajorProtocolVersion.vanRossemPV
    )

    /** Adds its two integer arguments, traces "add" and returns unit. Its budget grows with the
      * size of the arguments.
      */
    private val add: Program =
        λ("a", "b")(
          !Trace $ "add" $ (λ("sum")(()) $ (AddInteger $ (UnIData $ vr"a") $ (UnIData $ vr"b")))
        ).plutusV3

    private val small = Seq(Data.I(42), Data.I(1))
    private val large = Seq(Data.I(BigInt(2).pow(800)), Data.I(BigInt(3).pow(500)))

    /** The arguments as the npm API takes them: CBOR, the first as hex, the second as bytes. */
    private def jsArgs(args: Seq[Data]): js.Array[js.Any] =
        js.Array[js.Any](Hex.bytesToHex(args(0).toCbor), args(1).toCbor.toUint8Array)

    private def expected(args: Seq[Data]): Result =
        scalaVM.evaluateScriptDebug(args.foldLeft(add.deBruijnedProgram)(_.applyArg(_)))

    private def assertMatches(actual: JEvaluationResult, expected: Result): Unit = {
        assert(expected.isSuccess, expected) // not vacuous: the script runs to its unit result
        assert(actual.isSuccess, actual.error)
        assert(actual.budget.memory == js.BigInt(expected.budget.memory.toString))
        assert(actual.budget.steps == js.BigInt(expected.budget.steps.toString))
        assert(actual.logs.toSeq == expected.logs)
    }

    private def typeErrorFrom(body: => Any): js.TypeError =
        intercept[js.JavaScriptException](body).exception match
            case e: js.TypeError => e
            case other           => fail(s"expected TypeError, got $other")

    test("a PlutusVM gives the result of the Scala machine, call after call") {
        val vm = new JPlutusVM(mainnetV3)
        for _ <- 1 to 3 do
            assertMatches(vm.evaluateScript(add.doubleCborHex, jsArgs(small)), expected(small))
    }

    test("a Program gives the result of its script, to a PlutusVM and to the evaluator") {
        val vm = new JPlutusVM(mainnetV3)
        val program = JUplc.decodeProgram(add.doubleCborHex)
        assertMatches(vm.evaluateScript(program, jsArgs(small)), expected(small))
        assertMatches(JEvaluator.evaluateScript(program, jsArgs(small), mainnetV3), expected(small))
    }

    test("a Program takes new arguments on every call, and is not changed by them") {
        assert(expected(large).budget != expected(small).budget) // the arguments show in the cost
        val vm = new JPlutusVM(mainnetV3)
        val program = JUplc.decodeProgram(add.doubleCborHex)
        assertMatches(vm.evaluateScript(program, jsArgs(large)), expected(large))
        assertMatches(vm.evaluateScript(program, jsArgs(small)), expected(small))
        assertMatches(vm.evaluateScript(program, jsArgs(large)), expected(large))
    }

    test("decodeProgram takes every script envelope, and toFlat gives the flat bytes back") {
        for script <- Seq[js.Any](
              Hex.bytesToHex(add.flatEncoded),
              Hex.bytesToHex(add.cborEncoded),
              add.doubleCborHex,
              add.flatEncoded.toUint8Array,
              add.doubleCborEncoded.toUint8Array
            )
        do
            val program = JUplc.decodeProgram(script)
            assert(Hex.bytesToHex(program.toFlat().toByteArray) == Hex.bytesToHex(add.flatEncoded))
            assertMatches(
              JEvaluator.evaluateScript(program, jsArgs(small), mainnetV3),
              expected(small)
            )
    }

    test("the maxBudget of the options applies to every call of a PlutusVM") {
        val cost = expected(small).budget
        val below = js.Object
            .assign(
              js.Object(),
              mainnetV3,
              js.Dynamic.literal(
                maxBudget = js.Dynamic
                    .literal(memory = cost.memory.toDouble, steps = (cost.steps - 1).toDouble)
              )
            )
            .asInstanceOf[JEvaluationOptions]
        val vm = new JPlutusVM(below)
        for _ <- 1 to 2 do
            val result = vm.evaluateScript(add.doubleCborHex, jsArgs(small))
            assert(!result.isSuccess)
            assert(result.error.map(_.code).toOption.contains("OUT_OF_BUDGET"))
    }

    test("input that cannot be read throws TypeError") {
        typeErrorFrom(new JPlutusVM(null))
        typeErrorFrom(JUplc.decodeProgram(42))
        typeErrorFrom(JUplc.decodeProgram("zz"))
        val vm = new JPlutusVM(mainnetV3)
        typeErrorFrom(vm.evaluateScript(add.doubleCborHex, js.Array[js.Any]("0x00")))
    }

    test("a script that is neither bytes nor a Program: the TypeError names Program") {
        val vm = new JPlutusVM(mainnetV3)
        for script <- Seq[js.Any](42, js.Dynamic.literal(script = add.doubleCborHex)) do
            for error <- Seq(
                  typeErrorFrom(vm.evaluateScript(script, jsArgs(small))),
                  typeErrorFrom(JEvaluator.evaluateScript(script, jsArgs(small), mainnetV3))
                )
            do assert(error.message.contains("Program"), error.message)
    }
}
