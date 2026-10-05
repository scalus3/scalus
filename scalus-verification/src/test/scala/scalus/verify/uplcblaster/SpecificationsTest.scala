package scalus.verify.uplcblaster

import org.scalatest.funsuite.AnyFunSuite
import scalus.*
import scalus.cardano.onchain.plutus.prelude.{require, Spec}
import scalus.cardano.onchain.plutus.prelude.Spec.ensuring
import scalus.compiler.Compile
import scalus.compiler.sir.SIRType
import scalus.verify.*

/** Functions that state their contracts in their bodies, and callers of one of them. */
@Compile
object SpecifiedExamples {

    def clamp(x: BigInt, lo: BigInt, hi: BigInt): BigInt = {
        Spec.expects(lo <= hi)
        if x < lo then lo else if x > hi then hi else x
    }.ensuring(r => lo <= r && r <= hi)

    def increment(x: BigInt): BigInt = (x + BigInt(1)).ensuring(r => r > x)

    /** A postcondition that does not hold. */
    def wrong(x: BigInt): BigInt = (x + BigInt(1)).ensuring(r => r < x)

    def plain(x: BigInt): BigInt = x + BigInt(1)

    def violates(x: BigInt): BigInt = clamp(x, BigInt(10), BigInt(0))

    def guarded(x: BigInt, lo: BigInt, hi: BigInt): BigInt =
        if lo <= hi then clamp(x, lo, hi) else lo

    /** The distance between two bounds given in order. Where it calls itself, it passes them the
      * other way round, exactly where they are in order.
      */
    def span(lo: BigInt, hi: BigInt): BigInt = {
        Spec.expects(lo <= hi)
        if lo < hi then span(hi, lo) else hi - lo
    }

    /** The same, exchanging them only where they are not in order. */
    def spanOrdered(lo: BigInt, hi: BigInt): BigInt = {
        Spec.expects(lo <= hi)
        if hi < lo then spanOrdered(hi, lo) else hi - lo
    }

    /** A caller that hands the precondition on to its own callers. */
    def passes(x: BigInt, lo: BigInt, hi: BigInt): BigInt = {
        Spec.expects(lo <= hi)
        clamp(x, lo, hi)
    }

    /** A function whose specification, not its code, calls `clamp`. */
    def mentions(x: BigInt): BigInt = {
        Spec.ensures(clamp(x, BigInt(10), BigInt(0)) == BigInt(10))
        x
    }

    /** Every clause at the head of the body, with the postcondition on its last expression. */
    def clampAtHead(x: BigInt, lo: BigInt, hi: BigInt): BigInt = {
        Spec.expects(lo <= hi)
        (if x < lo then lo else if x > hi then hi else x) .ensuring(r => lo <= r && r <= hi)
    }

    /** A check returns nothing: it states what holds of its parameters where it returns. */
    def checked(x: BigInt, limit: BigInt): Unit = {
        Spec.ensures(x <= limit)
        require(x <= limit)
    }

    /** It returns at the limit, where the stated condition does not hold. */
    def checkedWrongly(x: BigInt, limit: BigInt): Unit = {
        Spec.ensures(x < limit)
        require(x <= limit)
    }

    /** A clause in one branch. */
    def branch(x: BigInt): BigInt =
        if x > BigInt(0) then (x + BigInt(1)).ensuring(r => r > BigInt(1)) else x

    def branchWrongly(x: BigInt): BigInt =
        if x > BigInt(0) then (x + BigInt(1)).ensuring(r => r > BigInt(2)) else x

    /** Two clauses: what holds of the parameters where it returns, and what holds of its result. */
    def bounded(x: BigInt, limit: BigInt): BigInt = {
        Spec.ensures(x <= limit)
        require(x <= limit)
        (x + BigInt(1)).ensuring(r => r <= limit + BigInt(1))
    }

    /** The second of its clauses does not hold at the limit. */
    def boundedWrongly(x: BigInt, limit: BigInt): BigInt = {
        Spec.ensures(x <= limit)
        require(x <= limit)
        (x + BigInt(1)).ensuring(r => r <= limit)
    }
}

/** An entry point that calls an `inline` handler, as a validator's `validate` calls `spend`. */
@Compile
trait SpecifiedEntry {
    inline def entry(x: BigInt): Unit = if x > BigInt(100) then handle(x - BigInt(100)) else ()
    inline def handle(x: BigInt): Unit
}

@Compile
object SpecifiedHandler extends SpecifiedEntry {
    inline override def handle(x: BigInt): Unit = {
        Spec.ensures(x >= BigInt(5))
        require(x >= BigInt(5))
    }
}

/** Contracts written in a function's body with `Spec`, read from its SIR by [[Contract.inSource]].
  */
class SpecificationsTest extends AnyFunSuite with LeanProofs {

    private val clamp = FunctionDef(SpecifiedExamples.clamp)
    private val increment = FunctionDef(SpecifiedExamples.increment)
    private val wrong = FunctionDef(SpecifiedExamples.wrong)
    private val plain = FunctionDef(SpecifiedExamples.plain)
    private val violates = FunctionDef(SpecifiedExamples.violates)
    private val guarded = FunctionDef(SpecifiedExamples.guarded)
    private val passes = FunctionDef(SpecifiedExamples.passes)
    private val span = FunctionDef(SpecifiedExamples.span)
    private val spanOrdered = FunctionDef(SpecifiedExamples.spanOrdered)
    private val clampAtHead = FunctionDef(SpecifiedExamples.clampAtHead)
    private val mentions = FunctionDef(SpecifiedExamples.mentions)
    private val checked = FunctionDef(SpecifiedExamples.checked)
    private val checkedWrongly = FunctionDef(SpecifiedExamples.checkedWrongly)
    private val branch = FunctionDef(SpecifiedExamples.branch)
    private val branchWrongly = FunctionDef(SpecifiedExamples.branchWrongly)
    private val bounded = FunctionDef(SpecifiedExamples.bounded)
    private val boundedWrongly = FunctionDef(SpecifiedExamples.boundedWrongly)
    private val entry = FunctionDef.named("entry", (x: BigInt) => SpecifiedHandler.entry(x))

    private def inSource[A, R](function: FunctionDef[A, R]): Contract[A, R] =
        Contract.inSource(function).getOrElse(fail(s"${function.name} states no contract"))

    test("a function's contract is read from its body") {
        val contract = inSource(clamp)
        assert(contract.function == clamp.ref)
        assert(contract.variables.map(_.tp) == List.fill(3)(SIRType.Integer))
        assert(contract.expects.isInstanceOf[Prop.Bool])
        contract.guarantees match
            case List(Prop.Call(function, _, result, false, Prop.Bool(_))) =>
                assert(function == clamp.ref && result.tp == SIRType.Integer)
            case other => fail(s"expected a partial call with a test, got $other")
        // A postcondition alone, a precondition alone, and neither.
        assert(inSource(increment).guarantees.head.isInstanceOf[Prop.Call[?, ?]])
        assert(inSource(passes).guarantees == List(inSource(passes).guarantees.head))
        assert(inSource(passes).guarantees.head.isInstanceOf[Prop.Bool])
        assert(Contract.inSource(plain).isEmpty)
        // The clauses at the head of the body are the same contract.
        val atHead = inSource(clampAtHead)
        assert(atHead.variables.map(_.tp) == contract.variables.map(_.tp))
        assert(atHead.guarantees.size == 1 && atHead.guarantees.head.isInstanceOf[Prop.Call[?, ?]])
        // `Spec.ensures` is a guarantee of the parameters, where the function returns.
        inSource(checked).guarantees match
            case List(Prop.Call(function, _, _, false, Prop.Bool(_))) =>
                assert(function == checked.ref)
            case other => fail(s"expected a partial call with a test, got $other")
        // A clause further inside is no part of the contract.
        assert(Contract.inSource(branch).isEmpty)
    }

    test("a contract read from the body is proved about the compiled function") {
        proven(inSource(clamp).prop, 120, clamp)
        proven(inSource(clamp).returnsWhen((x, lo, hi) => true).prop, 120, clamp)
        proven(inSource(increment).prop, 60, increment)
        // negative control: a postcondition that does not hold
        refuted(inSource(wrong).prop, 60, wrong)
        proven(inSource(clampAtHead).prop, 120, clampAtHead)
        proven(inSource(checked).prop, 80, checked)
        // negative control: it returns at the limit
        refuted(inSource(checkedWrongly).prop, 80, checkedWrongly)
    }

    test("a clause anywhere on a path states what holds where the function returns through it") {
        requireLean()
        def stated(function: FunctionDef[?, ?]): (Verifier, Statement) = {
            val verifier = Verifier.empty
            verifier.addFunction(function)
            verifier.guarantees(function.ref) match
                case StatedGuarantees(List(statement), Nil, Some(together)) =>
                    // one clause is all of them
                    assert(together eq statement)
                    verifier -> statement
                case other => fail(s"expected one guarantee of ${function.name}, got $other")
        }
        def prove(function: FunctionDef[?, ?]): VerificationResult = {
            val (verifier, statement) = stated(function)
            verifier.verify(statement, UplcBlaster(120, leanDirectory))
        }
        // in a branch
        val (_, inBranch) = stated(branch)
        assert(inBranch.name == "branch/ensures#1")
        inBranch.origin match
            case Origin.Guarantee(function, line) => assert(function == branch.ref && line > 0)
            case other => fail(s"expected a guarantee's origin, got $other")
        assert(prove(branch).isInstanceOf[VerificationResult.Proven])
        assert(prove(branchWrongly).isInstanceOf[VerificationResult.Refuted])
        // at the head of a function
        assert(prove(checked).isInstanceOf[VerificationResult.Proven])
        assert(prove(checkedWrongly).isInstanceOf[VerificationResult.Refuted])
        // in the code of an inline handler, inside the entry point that calls it
        assert(prove(entry).isInstanceOf[VerificationResult.Proven])
        // a function without clauses states nothing
        val none = Verifier.empty
        none.addFunction(plain)
        assert(none.guarantees(plain.ref) == StatedGuarantees(Nil, Nil, None))
    }

    test("the clauses of a function are also stated together, about one run of its body") {
        requireLean()
        def stated(function: FunctionDef[?, ?]): (Verifier, StatedGuarantees, Statement) = {
            val verifier = Verifier.empty
            verifier.addFunction(function)
            val guarantees = verifier.guarantees(function.ref)
            assert(guarantees.unsupported.isEmpty, guarantees.unsupported)
            (verifier, guarantees, guarantees.together.getOrElse(fail("no statement of them all")))
        }
        val (verifier, each, together) = stated(bounded)
        assert(each.statements.map(_.name) == List("bounded/ensures#1", "bounded/ensures#2"))
        assert(together.name == "bounded/ensures")
        together.origin match
            case Origin.Guarantees(function, lines) =>
                assert(function == bounded.ref && lines.size == 2 && lines.forall(_ > 0))
            case other => fail(s"expected the origin of several guarantees, got $other")
        // Each statement says that the function returns: a program for its body, and one for the
        // clause. Together they have the body's program once.
        def programs(statement: Statement): Int =
            UplcBlaster.lower(statement.prop, FunctionTable(bounded)) match
                case Right(lowered) => lowered.leaves.size
                case Left(reason)   => fail(reason)
        assert(each.statements.map(programs) == List(2, 2))
        assert(programs(together) == 3)
        assert(
          verifier
              .verify(together, UplcBlaster(120, leanDirectory))
              .isInstanceOf[VerificationResult.Proven]
        )
        // negative control: one clause that does not hold refutes them together
        val (wrong, _, both) = stated(boundedWrongly)
        assert(
          wrong
              .verify(both, UplcBlaster(120, leanDirectory))
              .isInstanceOf[VerificationResult.Refuted]
        )
    }

    test("a clause that relies on the function's precondition holds under its contract") {
        requireLean()
        val verifier = Verifier.empty
        verifier.addFunction(clampAtHead)
        def prove(statement: Statement): VerificationResult =
            verifier.verify(statement, UplcBlaster(120, leanDirectory))
        // The function returns for lo > hi as well, with a result that is not between them: its
        // `Spec.expects` is no check.
        val List(alone) = verifier.guarantees(clampAtHead.ref).statements
        assert(alone.name == "clampAtHead/ensures#1")
        assert(prove(alone).isInstanceOf[VerificationResult.Refuted])
        val contract = verifier.contract("clamp_at_head", inSource(clampAtHead))
        val List(assumed) = verifier.guarantees(contract).statements
        assert(assumed.name == "clamp_at_head/ensures#1")
        assert(assumed.origin == alone.origin)
        assert(prove(assumed).isInstanceOf[VerificationResult.Proven])
        // only a contract has a precondition to assume
        assertThrows[IllegalArgumentException](verifier.guarantees(alone))
    }

    test("callers owe a precondition stated in the callee's body") {
        requireLean()
        def verifierWith(caller: FunctionDef[?, ?]): Verifier = {
            val verifier = Verifier.empty
            verifier.addFunction(clamp)
            verifier.addFunction(caller)
            verifier.contract("clamp_in_range", inSource(clamp))
            verifier
        }
        def prove(verifier: Verifier, owed: Statement): VerificationResult =
            verifier.verify(owed, UplcBlaster(120, leanDirectory))

        // clamp(x, 10, 0)
        val violating = verifierWith(violates)
        val List(violated) = violating.obligations(violates.ref).statements
        assert(prove(violating, violated).isInstanceOf[VerificationResult.Refuted])

        // if lo <= hi then clamp(x, lo, hi) else lo
        val branching = verifierWith(guarded)
        val List(established) = branching.obligations(guarded.ref).statements
        assert(prove(branching, established).isInstanceOf[VerificationResult.Proven])

        // A function that calls itself owes its own precondition there. The contract's variables
        // are then the parameters the call's arguments are written in: span(hi, lo) must be
        // checked with the caller's `hi` and `lo`, not with `lo` as the call has just bound it.
        def owedToItself(function: FunctionDef[?, ?]): VerificationResult = {
            val verifier = Verifier.empty
            verifier.addFunction(function)
            verifier.contract(s"${function.name}_ordered", inSource(function))
            val List(owed) = verifier.obligations(function.ref).statements
            prove(verifier, owed)
        }
        owedToItself(span) match
            case VerificationResult.Refuted(proof) =>
                val values = proof.artifact
                    .asInstanceOf[UplcBlaster.Artifact]
                    .counterexample
                    .map((name, value) => name.takeWhile(_ != '-') -> integer(value))
                    .toMap
                assert(values("lo") < values("hi"), values)
            case other => fail(s"expected a refutation, got $other")
        assert(owedToItself(spanOrdered).isInstanceOf[VerificationResult.Proven])

        // A call in a clause is not a call of the function: it owes nothing.
        val specifying = verifierWith(mentions)
        assert(specifying.obligations(mentions.ref) == CallObligations(Nil, Nil))

        // clamp(x, lo, hi) in a function whose own body expects lo <= hi: not established by the
        // function alone, and established under its contract. Its `Spec.expects` is no check.
        val assuming = verifierWith(passes)
        val List(alone) = assuming.obligations(passes.ref).statements
        assert(prove(assuming, alone).isInstanceOf[VerificationResult.Refuted])
        val assumes = assuming.contract("passes_expects", inSource(passes))
        val List(assumed) = assuming.obligations(assumes).statements
        assert(prove(assuming, assumed).isInstanceOf[VerificationResult.Proven])
    }
}
