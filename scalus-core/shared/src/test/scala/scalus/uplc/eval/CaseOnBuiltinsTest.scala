package scalus.uplc.eval

import org.scalatest.funsuite.AnyFunSuite
import scalus.cardano.ledger.Word64
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
