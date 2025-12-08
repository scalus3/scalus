package scalus.uplc.eval

import org.scalatest.funsuite.AnyFunSuite
import scalus.cardano.ledger.{ExUnits, Word64}
import scalus.uplc.*
import scalus.uplc.Term.*
import scalus.uplc.TermDSL.given

import scala.language.implicitConversions

class CaseOnBuiltinsTest extends AnyFunSuite:

    val v4vm: PlutusVM = PlutusVM.makePlutusV4VM()
    val v3vm: PlutusVM = PlutusVM.makePlutusV3VM()

    def evalV4(term: Term): Term =
        v4vm.evaluateDeBruijnedTerm(DeBruijn.deBruijnTerm(term))

    def evalV3(term: Term): Term =
        v3vm.evaluateDeBruijnedTerm(DeBruijn.deBruijnTerm(term))

    // Case on Integer tests for V4
    // For integers and booleans, case branches are plain terms (not lambdas)
    // because there are no constructor arguments to bind

    test("V4: Case on integer 0 selects first branch") {
        val term = Case(
          Const(Constant.Integer(0)),
          List(
            Const(Constant.String("zero")),
            Const(Constant.String("one")),
            Const(Constant.String("two"))
          )
        )
        assert(evalV4(term) == Const(Constant.String("zero")))
    }

    test("V4: Case on integer 1 selects second branch") {
        val term = Case(
          Const(Constant.Integer(1)),
          List(
            Const(Constant.String("zero")),
            Const(Constant.String("one")),
            Const(Constant.String("two"))
          )
        )
        assert(evalV4(term) == Const(Constant.String("one")))
    }

    test("V4: Case on integer 2 selects third branch") {
        val term = Case(
          Const(Constant.Integer(2)),
          List(
            Const(Constant.String("zero")),
            Const(Constant.String("one")),
            Const(Constant.String("two"))
          )
        )
        assert(evalV4(term) == Const(Constant.String("two")))
    }

    test("V4: Case on negative integer throws CaseIndexOutOfBounds") {
        val term = Case(
          Const(Constant.Integer(-1)),
          List(Const(Constant.String("zero")), Const(Constant.String("one")))
        )
        assertThrows[CaseIndexOutOfBounds](evalV4(term))
    }

    test("V4: Case on out-of-bounds integer throws CaseIndexOutOfBounds") {
        val term = Case(
          Const(Constant.Integer(5)),
          List(Const(Constant.String("zero")), Const(Constant.String("one")))
        )
        assertThrows[CaseIndexOutOfBounds](evalV4(term))
    }

    // Case on Boolean tests for V4

    test("V4: Case on false selects first branch") {
        val term = Case(
          Const(Constant.Bool(false)),
          List(Const(Constant.String("false")), Const(Constant.String("true")))
        )
        assert(evalV4(term) == Const(Constant.String("false")))
    }

    test("V4: Case on true selects second branch") {
        val term = Case(
          Const(Constant.Bool(true)),
          List(Const(Constant.String("false")), Const(Constant.String("true")))
        )
        assert(evalV4(term) == Const(Constant.String("true")))
    }

    test("V4: Case on false with single branch works") {
        val term = Case(
          Const(Constant.Bool(false)),
          List(Const(Constant.String("false-only")))
        )
        assert(evalV4(term) == Const(Constant.String("false-only")))
    }

    test("V4: Case on true with single branch throws CaseBoolBranchMissing") {
        val term = Case(
          Const(Constant.Bool(true)),
          List(Const(Constant.String("false-only")))
        )
        assertThrows[CaseBoolBranchMissing](evalV4(term))
    }

    // V3 should NOT support case on builtins

    test("V3: Case on integer throws NonConstrScrutinized") {
        val term = Case(
          Const(Constant.Integer(0)),
          List(Const(Constant.String("zero")))
        )
        assertThrows[NonConstrScrutinized](evalV3(term))
    }

    test("V3: Case on boolean throws NonConstrScrutinized") {
        val term = Case(
          Const(Constant.Bool(false)),
          List(Const(Constant.String("false")))
        )
        assertThrows[NonConstrScrutinized](evalV3(term))
    }

    // V4: Case on other constant types should still throw NonConstrScrutinized

    test("V4: Case on string throws NonConstrScrutinized") {
        val term = Case(
          Const(Constant.String("hello")),
          List(Const(Constant.String("branch")))
        )
        assertThrows[NonConstrScrutinized](evalV4(term))
    }

    // Case on Unit tests for V4

    test("V4: Case on unit selects the single branch") {
        val term = Case(Const(Constant.Unit), List(Const(Constant.String("unit-branch"))))
        assert(evalV4(term) == Const(Constant.String("unit-branch")))
    }

    test("V4: Case on unit with no branches throws CaseUnitBranchMissing") {
        val term = Case(Const(Constant.Unit), List())
        assertThrows[CaseUnitBranchMissing](evalV4(term))
    }

    test("V4: Case on unit with two branches throws CaseUnitBranchMissing") {
        val term = Case(
          Const(Constant.Unit),
          List(Const(Constant.String("first")), Const(Constant.String("second")))
        )
        assertThrows[CaseUnitBranchMissing](evalV4(term))
    }

    test("V3: Case on unit throws NonConstrScrutinized") {
        val term = Case(Const(Constant.Unit), List(Const(Constant.String("branch"))))
        assertThrows[NonConstrScrutinized](evalV3(term))
    }

    // V4: Case on Constr should still work as before
    // For Constr, branches ARE lambdas that receive constructor arguments

    test("V4: Case on Constr with no args") {
        val term = Case(
          Constr(Word64(1), Nil),
          List(
            Const(Constant.String("zero")),
            Const(Constant.String("one"))
          )
        )
        assert(evalV4(term) == Const(Constant.String("one")))
    }

    test("V4: Case on Constr with args applies them to lambda branch") {
        val term = Case(
          Constr(Word64(1), List(Const(Constant.Integer(42)))),
          List(
            LamAbs("x", Const(Constant.String("zero"))),
            LamAbs("x", Var(NamedDeBruijn("x", 0)))
          )
        )
        assert(evalV4(term) == Const(Constant.Integer(42)))
    }

    // Large case tables - Fibonacci lookup with 1000+ branches

    test("V4: Case on integer with 1000+ branches - Fibonacci lookup table") {
        // Generate first 1001 Fibonacci numbers using lazy val to ensure memoization
        lazy val fibs: LazyList[BigInt] =
            BigInt(0) #:: BigInt(1) #:: fibs.zip(fibs.tail).map { case (a, b) => a + b }
        val fibTable = fibs.take(1001).toList

        // Create a case expression that acts as a lookup table for fib(n)
        val branches = fibTable.map(fib => Const(Constant.Integer(fib)))

        // Test several values across the range
        val testCases = List(0, 1, 2, 10, 50, 100, 500, 999, 1000)
        for n <- testCases do
            val term = Case(Const(Constant.Integer(n)), branches)
            val result = evalV4(term)
            assert(
              result == Const(Constant.Integer(fibTable(n))),
              s"fib($n) should be ${fibTable(n)}"
            )
    }

    // Commented out: sieve is slow for 1001 primes
    // test("V4: Case on integer with 1000+ branches - Prime table lookup") {
    //     // Generate primes using Sieve of Eratosthenes
    //     def sieve(s: LazyList[Int]): LazyList[Int] =
    //         s.head #:: sieve(s.tail.filter(_ % s.head != 0))
    //     val primes = sieve(LazyList.from(2)).take(1001).toList
    //
    //     // Create a case expression: given index n, return the nth prime
    //     val branches = primes.map(p => Const(Constant.Integer(p)))
    //
    //     // Test several indices across the range
    //     val testCases = List(0, 1, 2, 10, 50, 100, 500, 999, 1000)
    //     for n <- testCases do
    //         val term = Case(Const(Constant.Integer(n)), branches)
    //         val result = evalV4(term)
    //         assert(
    //           result == Const(Constant.Integer(primes(n))),
    //           s"prime($n) should be ${primes(n)}"
    //         )
    // }

    // Budget comparison tests: ifThenElse vs Case on boolean

    def evalWithBudget(vm: PlutusVM, term: Term): (Term, ExUnits) = {
        val spender = CountingBudgetSpender()
        val deTerm = DeBruijn.deBruijnTerm(term)
        val result = vm.evaluateDeBruijnedTerm(deTerm, spender, NoLogger)
        (result, spender.getSpentBudget)
    }

    test("V4: Compare budget of ifThenElse vs Case on boolean (true branch)") {
        val thenBranch = Const(Constant.Integer(1))
        val elseBranch = Const(Constant.Integer(0))

        // ifThenElse version: force (ifThenElse cond (delay then) (delay else))
        val ifThenElseTerm = Force(
          Apply(
            Apply(
              Apply(
                Force(Builtin(DefaultFun.IfThenElse)),
                Const(Constant.Bool(true))
              ),
              Delay(thenBranch)
            ),
            Delay(elseBranch)
          )
        )

        // Case version: Case(cond, [falseBranch, trueBranch])
        val caseTerm = Case(
          Const(Constant.Bool(true)),
          List(elseBranch, thenBranch)
        )

        val (ifThenElseResult, ifThenElseBudget) = evalWithBudget(v4vm, ifThenElseTerm)
        val (caseResult, caseBudget) = evalWithBudget(v4vm, caseTerm)

        // Both should produce the same result
        assert(ifThenElseResult == thenBranch, s"ifThenElse result: $ifThenElseResult")
        assert(caseResult == thenBranch, s"Case result: $caseResult")

        println(s"ifThenElse budget (true): $ifThenElseBudget")
        println(s"Case budget (true): $caseBudget")
        println(
          s"Savings: ${ifThenElseBudget.cpu - caseBudget.cpu} cpu, ${ifThenElseBudget.memory - caseBudget.memory} mem"
        )

        // Case should be more efficient (less budget)
        assert(
          caseBudget.cpu <= ifThenElseBudget.cpu,
          s"Case ($caseBudget) should use less or equal CPU than ifThenElse ($ifThenElseBudget)"
        )
    }

    test("V4: Compare budget of ifThenElse vs Case on boolean (false branch)") {
        val thenBranch = Const(Constant.Integer(1))
        val elseBranch = Const(Constant.Integer(0))

        // ifThenElse version: force (ifThenElse cond (delay then) (delay else))
        val ifThenElseTerm = Force(
          Apply(
            Apply(
              Apply(
                Force(Builtin(DefaultFun.IfThenElse)),
                Const(Constant.Bool(false))
              ),
              Delay(thenBranch)
            ),
            Delay(elseBranch)
          )
        )

        // Case version: Case(cond, [falseBranch, trueBranch])
        val caseTerm = Case(
          Const(Constant.Bool(false)),
          List(elseBranch, thenBranch)
        )

        val (ifThenElseResult, ifThenElseBudget) = evalWithBudget(v4vm, ifThenElseTerm)
        val (caseResult, caseBudget) = evalWithBudget(v4vm, caseTerm)

        // Both should produce the same result
        assert(ifThenElseResult == elseBranch, s"ifThenElse result: $ifThenElseResult")
        assert(caseResult == elseBranch, s"Case result: $caseResult")

        println(s"ifThenElse budget (false): $ifThenElseBudget")
        println(s"Case budget (false): $caseBudget")
        println(
          s"Savings: ${ifThenElseBudget.cpu - caseBudget.cpu} cpu, ${ifThenElseBudget.memory - caseBudget.memory} mem"
        )

        // Case should be more efficient (less budget)
        assert(
          caseBudget.cpu <= ifThenElseBudget.cpu,
          s"Case ($caseBudget) should use less or equal CPU than ifThenElse ($ifThenElseBudget)"
        )
    }

    // Budget comparison tests: equalsInteger chain vs Case on integer

    test("V4: Compare budget of equalsInteger chain vs Case on integer (3 branches)") {
        val branch0 = Const(Constant.String("zero"))
        val branch1 = Const(Constant.String("one"))
        val branch2 = Const(Constant.String("two"))
        val scrutinee = Const(Constant.Integer(1))

        // equalsInteger chain version:
        // force(ifThenElse (equalsInteger n 0) (delay branch0)
        //       (delay (force(ifThenElse (equalsInteger n 1) (delay branch1) (delay branch2)))))
        def mkEqualsChain(n: Term): Term = {
            val eq0 =
                Apply(Apply(Builtin(DefaultFun.EqualsInteger), n), Const(Constant.Integer(0)))
            val eq1 =
                Apply(Apply(Builtin(DefaultFun.EqualsInteger), n), Const(Constant.Integer(1)))
            Force(
              Apply(
                Apply(Apply(Force(Builtin(DefaultFun.IfThenElse)), eq0), Delay(branch0)),
                Delay(
                  Force(
                    Apply(
                      Apply(Apply(Force(Builtin(DefaultFun.IfThenElse)), eq1), Delay(branch1)),
                      Delay(branch2)
                    )
                  )
                )
              )
            )
        }

        val equalsChainTerm = mkEqualsChain(scrutinee)

        // Case version: Case(n, [branch0, branch1, branch2])
        val caseTerm = Case(scrutinee, List(branch0, branch1, branch2))

        val (equalsChainResult, equalsChainBudget) = evalWithBudget(v4vm, equalsChainTerm)
        val (caseResult, caseBudget) = evalWithBudget(v4vm, caseTerm)

        // Both should produce the same result
        assert(equalsChainResult == branch1, s"equalsChain result: $equalsChainResult")
        assert(caseResult == branch1, s"Case result: $caseResult")

        println(s"equalsInteger chain budget (3 branches, select 1): $equalsChainBudget")
        println(s"Case on integer budget (3 branches, select 1): $caseBudget")
        println(
          s"Savings: ${equalsChainBudget.cpu - caseBudget.cpu} cpu, ${equalsChainBudget.memory - caseBudget.memory} mem"
        )

        // Case should be more efficient (less budget)
        assert(
          caseBudget.cpu <= equalsChainBudget.cpu,
          s"Case ($caseBudget) should use less or equal CPU than equalsInteger chain ($equalsChainBudget)"
        )
    }
