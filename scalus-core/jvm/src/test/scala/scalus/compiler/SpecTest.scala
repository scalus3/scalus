package scalus.compiler

import org.scalatest.funsuite.AnyFunSuite
import scalus.*
import scalus.cardano.onchain.{OnchainError, SpecificationError}
import scalus.cardano.onchain.plutus.prelude.{List as PList, Spec}
import scalus.cardano.onchain.plutus.prelude.Spec.ensuring
import scalus.cardano.onchain.plutus.v3.ScriptContext
import scalus.compiler.sir.{AnnotationsDecl, SIR, SIRType}
import scalus.compiler.sir.transform.EraseSpecifications
import scalus.uplc.{Constant, PlutusV3}
import scalus.uplc.Term.asTerm
import scalus.uplc.eval.PlutusVM
import scalus.uplc.builtin.Data

/** The same function with and without its specification. */
@Compile
object SpecExamples {

    def clamp(x: BigInt, lo: BigInt, hi: BigInt): BigInt =
        if x < lo then lo else if x > hi then hi else x

    def clampSpecified(x: BigInt, lo: BigInt, hi: BigInt): BigInt = {
        Spec.expects(lo <= hi)
        if x < lo then lo else if x > hi then hi else x
    }.ensuring(r => lo <= r && r <= hi)

    def half(x: BigInt): BigInt = {
        val twice = x + x
        twice / BigInt(4)
    }

    def halfSpecified(x: BigInt): BigInt = {
        Spec.expects(x >= BigInt(0))
        Spec.expects(x < BigInt(1000))
        val twice = x + x
        twice / BigInt(4)
    }.ensuring(r => r + r <= x)

    /** A postcondition that does not hold. */
    def wrong(x: BigInt): BigInt = (x + BigInt(1)).ensuring(r => r < x)

    def checked(x: BigInt, limit: BigInt): Unit =
        if x <= limit then () else throw new RuntimeException("above the limit")

    /** Clauses at the head of the body, the postcondition on the last expression, and one that is
      * stated of the parameters, as a handler that returns nothing states it. `within` is written
      * for the specification only.
      */
    def checkedSpecified(x: BigInt, limit: BigInt): Unit = {
        Spec.expects(limit >= BigInt(0))
        Spec.ensures(within(x, limit))
        if x <= limit then () else throw new RuntimeException("above the limit")
    }

    def within(x: BigInt, limit: BigInt): Boolean = x <= limit

    /** Where it returns, as `checkedSpecified` says what holds where it does. */
    def checkedReturning(x: BigInt, limit: BigInt): Unit = {
        Spec.returnsWhen(within(x, limit))
        if x <= limit then () else throw new RuntimeException("above the limit")
    }

    def doubled(x: BigInt): BigInt = x + x

    def squared(x: BigInt): BigInt = x * x

    def combined(x: BigInt): BigInt = doubled(x) - squared(x)

    /** Its clause names the two functions in the other order than its code. A subtraction keeps its
      * operands in the order they are written; `a >= b` is compiled as `b <= a`.
      */
    def combinedSpecified(x: BigInt): BigInt = {
        Spec.ensures(squared(x) - doubled(x) >= BigInt(-1))
        doubled(x) - squared(x)
    }

    def isEven(n: BigInt): Boolean = if n == BigInt(0) then true else isOdd(n - BigInt(1))

    def isOdd(n: BigInt): Boolean = if n == BigInt(0) then false else isEven(n - BigInt(1))

    def halved(n: BigInt): BigInt =
        if isEven(n) then n / BigInt(2) else throw new RuntimeException("an odd number")

    /** Its clause uses one of two functions that call each other, and its code the other. */
    def halvedSpecified(n: BigInt): BigInt = {
        Spec.expects(!isOdd(n))
        if isEven(n) then n / BigInt(2) else throw new RuntimeException("an odd number")
    }

    def clampWithLast(x: BigInt, lo: BigInt, hi: BigInt): BigInt = {
        Spec.expects(lo <= hi)
        (if x < lo then lo else if x > hi then hi else x) .ensuring(r => lo <= r && r <= hi)
    }
}

class SpecTest extends AnyFunSuite {

    private def mentions(sir: SIR, name: String): Boolean = sir.toString.contains(name)

    /** The names of the definitions linking put around a program, outermost first. */
    private def definitions(sir: SIR): List[String] = sir match
        case SIR.Decl(_, term)     => definitions(term)
        case SIR.Apply(f, _, _, _) => definitions(f)
        case SIR.Let(bindings, body, _, _) if bindings.forall(_.name.contains('.')) =>
            bindings.map(_.name) ++ definitions(body)
        case _ => Nil

    test("a specification stays in the function's SIR, for a verifier to read") {
        val specified = compile(SpecExamples.clampSpecified)
        assert(mentions(specified, EraseSpecifications.Expects))
        assert(mentions(specified, EraseSpecifications.Ensuring))
        val erased = EraseSpecifications(specified)
        assert(!mentions(erased, EraseSpecifications.Expects))
        assert(!mentions(erased, EraseSpecifications.Ensuring))
    }

    test("a script has the same bytes with and without a specification") {
        for options <- Seq(Options.release, Options.releaseUntagged, Options.default, Options.debug)
        do
            given Options = options
            assert(
              PlutusV3.compile(SpecExamples.clampSpecified).program.cborEncoded.toSeq ==
                  PlutusV3.compile(SpecExamples.clamp).program.cborEncoded.toSeq,
              s"clamp with $options"
            )
            assert(
              PlutusV3.compile(SpecExamples.halfSpecified).program.cborEncoded.toSeq ==
                  PlutusV3.compile(SpecExamples.half).program.cborEncoded.toSeq,
              s"half with $options"
            )
            assert(
              PlutusV3.compile(SpecExamples.clampWithLast).program.cborEncoded.toSeq ==
                  PlutusV3.compile(SpecExamples.clamp).program.cborEncoded.toSeq,
              s"clamp, with the postcondition on its last expression, with $options"
            )
            // Definitions are nested in the order the code meets them, not the clauses.
            assert(
              PlutusV3.compile(SpecExamples.combinedSpecified).program.cborEncoded.toSeq ==
                  PlutusV3.compile(SpecExamples.combined).program.cborEncoded.toSeq,
              s"combined with $options"
            )
            // A function only the specification uses, `within`, is no part of the script.
            assert(
              PlutusV3.compile(SpecExamples.checkedSpecified).program.cborEncoded.toSeq ==
                  PlutusV3.compile(SpecExamples.checked).program.cborEncoded.toSeq,
              s"checked with $options"
            )
            assert(
              PlutusV3.compile(SpecExamples.checkedReturning).program.cborEncoded.toSeq ==
                  PlutusV3.compile(SpecExamples.checked).program.cborEncoded.toSeq,
              s"checked, stated where it returns, with $options"
            )
    }

    test("definitions are nested in the order of the code, not of its clauses") {
        def order(sir: SIR): List[String] =
            definitions(sir).filter(name => name.endsWith(".doubled") || name.endsWith(".squared"))
        val code =
            List("scalus.compiler.SpecExamples$.doubled", "scalus.compiler.SpecExamples$.squared")
        val specified = compile(SpecExamples.combinedSpecified)
        // The clause is met first, and names them the other way round.
        assert(order(specified) == code.reverse)
        assert(order(compile(SpecExamples.combined)) == code)
        assert(order(EraseSpecifications(specified)) == code)
        // Applied to its parameter, a program has its definitions inside the application.
        val three = SIR.Const(Constant.Integer(3), SIRType.Integer, AnnotationsDecl.empty)
        assert(order(specified $ three) == code.reverse)
        assert(order(EraseSpecifications(specified $ three)) == code)
    }

    test("a program without clauses is nested again as the linker nested it") {
        // The erasure walks a linked program as the linker walked its code, and nests what the
        // walk reaches by the linker's rule. Were that to give another nesting than linking gave,
        // a specified script's bytes would follow its clauses.
        val programs = List(
          "clamp" -> compile(SpecExamples.clamp),
          "combined" -> compile(SpecExamples.combined),
          "halved" -> compile(SpecExamples.halved),
          "checked" -> compile(SpecExamples.checked),
          "lists" -> compile((xs: PList[BigInt]) =>
              xs.map(_ + BigInt(1)).filter(_ > BigInt(0)).foldLeft(BigInt(0))(_ + _)
          ),
          "context" -> compile((data: Data) => data.to[ScriptContext].txInfo.signatories.length)
        )
        for (name, program) <- programs do
            assert(definitions(program).nonEmpty, name)
            assert(EraseSpecifications.reordered(program) == program, name)
    }

    test("a program that starts with a value of a match keeps it, where a clause is linked in") {
        // The plugin names a match's scrutinee `_SpecTest.scala_L_C_match_scrutinee_1`: a name
        // with a dot, as a definition's has. Where a program starts with it, and a function the
        // program links states a clause, the erasure nests the definitions again. It took the
        // value for one of them, which the code refers to as a local value and not as a
        // definition, and dropped it.
        given PlutusVM = PlutusVM.makePlutusV3VM()
        val program = compile {
            (SpecExamples.clampSpecified(BigInt(5), BigInt(0), BigInt(3)), BigInt(1)) match
                case (clamped, one) => clamped + one
        }
        assert(program.toUplc().evaluate == BigInt(4).asTerm)
    }

    test("a script applied to its parameter has the same bytes with and without a specification") {
        for options <- Seq(Options.release, Options.releaseUntagged, Options.default, Options.debug)
        do
            given Options = options
            assert(
              PlutusV3
                  .compile(SpecExamples.combinedSpecified)(BigInt(3))
                  .program
                  .cborEncoded
                  .toSeq ==
                  PlutusV3.compile(SpecExamples.combined)(BigInt(3)).program.cborEncoded.toSeq,
              s"combined with $options"
            )
            assert(
              PlutusV3.compile(SpecExamples.halfSpecified)(BigInt(10)).program.cborEncoded.toSeq ==
                  PlutusV3.compile(SpecExamples.half)(BigInt(10)).program.cborEncoded.toSeq,
              s"half with $options"
            )
    }

    test("a function a clause uses stays where the code's functions call it") {
        // The clause is met first, so `isOdd` is the later of the two in their recursive `let`,
        // and only `isEven`, before it, still uses it.
        def recursive(sir: SIR): List[String] = definitions(sir).filter(_.contains(".is"))
        val isEven = "scalus.compiler.SpecExamples$.isEven"
        val isOdd = "scalus.compiler.SpecExamples$.isOdd"
        val specified = compile(SpecExamples.halvedSpecified)
        assert(recursive(specified) == List(isEven, isOdd))
        // Both are defined, in the order the code gives.
        assert(recursive(EraseSpecifications(specified)) == List(isOdd, isEven))
        assert(recursive(compile(SpecExamples.halved)) == List(isOdd, isEven))
        for options <- Seq(Options.release, Options.releaseUntagged, Options.default, Options.debug)
        do
            given Options = options
            assert(
              PlutusV3.compile(SpecExamples.halvedSpecified).program.cborEncoded.toSeq ==
                  PlutusV3.compile(SpecExamples.halved).program.cborEncoded.toSeq,
              s"halved with $options"
            )
    }

    test("a function written for the specification is kept in the SIR, and erased with it") {
        val specified = compile(SpecExamples.checkedSpecified)
        assert(mentions(specified, EraseSpecifications.Ensures))
        assert(mentions(specified, "SpecExamples$.within"))
        val erased = EraseSpecifications(specified)
        assert(!mentions(erased, EraseSpecifications.Ensures))
        assert(!mentions(erased, "SpecExamples$.within"))
        val returning = compile(SpecExamples.checkedReturning)
        assert(mentions(returning, EraseSpecifications.ReturnsWhen))
        assert(!mentions(EraseSpecifications(returning), EraseSpecifications.ReturnsWhen))
        assert(!mentions(EraseSpecifications(returning), "SpecExamples$.within"))
        // Code without clauses is not touched.
        val plain = compile(SpecExamples.checked)
        assert(EraseSpecifications(plain) == plain)
    }

    test("off-chain, a specification is checked") {
        assert(SpecExamples.clampSpecified(BigInt(7), BigInt(0), BigInt(5)) == BigInt(5))
        assertThrows[SpecificationError](
          SpecExamples.clampSpecified(BigInt(7), BigInt(5), BigInt(0))
        )
        assert(SpecExamples.halfSpecified(BigInt(10)) == BigInt(5))
        assertThrows[SpecificationError](SpecExamples.halfSpecified(BigInt(-2)))
        assertThrows[SpecificationError](SpecExamples.wrong(BigInt(1)))
        // `ensures` at the head of a body is not evaluated: the function's own failure shows.
        assert(
          intercept[RuntimeException](
            SpecExamples.checkedSpecified(BigInt(5), BigInt(1))
          ).getMessage == "above the limit"
        )
        assertThrows[SpecificationError](SpecExamples.checkedSpecified(BigInt(0), BigInt(-1)))
        // Nor is `returnsWhen`: where it says the function returns and the function fails, the
        // failure is the function's own.
        assert(SpecExamples.checkedReturning(BigInt(1), BigInt(5)) == ())
        assert(
          intercept[RuntimeException](
            SpecExamples.checkedReturning(BigInt(5), BigInt(1))
          ).getMessage == "above the limit"
        )
        // A violated specification is no failure of the script, which carries none: a test that
        // expects the script to fail, with an `OnchainError` or any exception, does not pass on it.
        val violated: Throwable =
            intercept[SpecificationError](SpecExamples.halfSpecified(BigInt(-2)))
        assert(!violated.isInstanceOf[OnchainError])
        assert(!violated.isInstanceOf[Exception])
    }
}
