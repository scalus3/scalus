package scalus.uplc.eval

import org.scalatest.funsuite.AnyFunSuite
import scalus.cardano.ledger.{ExUnits, Word64}
import scalus.uplc.*
import scalus.uplc.Term.*

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
          s"Savings: ${ifThenElseBudget.steps - caseBudget.steps} cpu, ${ifThenElseBudget.memory - caseBudget.memory} mem"
        )

        // Case should be more efficient (less budget)
        assert(
          caseBudget.steps <= ifThenElseBudget.steps,
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
          s"Savings: ${ifThenElseBudget.steps - caseBudget.steps} cpu, ${ifThenElseBudget.memory - caseBudget.memory} mem"
        )

        // Case should be more efficient (less budget)
        assert(
          caseBudget.steps <= ifThenElseBudget.steps,
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
          s"Savings: ${equalsChainBudget.steps - caseBudget.steps} cpu, ${equalsChainBudget.memory - caseBudget.memory} mem"
        )

        // Case should be more efficient (less budget)
        assert(
          caseBudget.steps <= equalsChainBudget.steps,
          s"Case ($caseBudget) should use less or equal CPU than equalsInteger chain ($equalsChainBudget)"
        )
    }

    // Case on List tests for V4
    // List has 2 constructors: Cons=0 (head, tail args), Nil=1 (no args)
    // For Cons, the branch should be a function taking head and tail

    val intListType = DefaultUni.Apply(DefaultUni.ProtoList, DefaultUni.Integer)

    test("V4: Case on empty list selects nil branch") {
        // Branches: [cons_branch, nil_branch]
        val term = Case(
          Const(Constant.List(intListType, Nil)),
          List(
            LamAbs("h", LamAbs("t", Const(Constant.String("cons")))),
            Const(Constant.String("nil"))
          )
        )
        assert(evalV4(term) == Const(Constant.String("nil")))
    }

    test("V4: Case on non-empty list selects cons branch and applies head and tail") {
        // For cons branch, we receive head and tail as arguments
        // The branch should be: \h t -> h (returns the head)
        // Branches: [cons_branch, nil_branch]
        val term = Case(
          Const(Constant.List(intListType, List(Constant.Integer(42), Constant.Integer(43)))),
          List(
            LamAbs("h", LamAbs("t", Var(NamedDeBruijn("h", 0)))),
            Const(Constant.String("nil"))
          )
        )
        assert(evalV4(term) == Const(Constant.Integer(42)))
    }

    test("V4: Case on non-empty list can access tail") {
        // For cons branch: \h t -> t (returns the tail)
        // Branches: [cons_branch, nil_branch]
        val term = Case(
          Const(Constant.List(intListType, List(Constant.Integer(42), Constant.Integer(43)))),
          List(
            LamAbs("h", LamAbs("t", Var(NamedDeBruijn("t", 0)))),
            Const(Constant.String("nil"))
          )
        )
        val expected = Const(Constant.List(intListType, List(Constant.Integer(43))))
        assert(evalV4(term) == expected)
    }

    test("V4: Case on single-element list - tail is empty") {
        // For cons branch: \h t -> t (returns the tail, which should be empty)
        // Branches: [cons_branch, nil_branch]
        val term = Case(
          Const(Constant.List(intListType, List(Constant.Integer(42)))),
          List(
            LamAbs("h", LamAbs("t", Var(NamedDeBruijn("t", 0)))),
            Const(Constant.String("nil"))
          )
        )
        val expected = Const(Constant.List(intListType, Nil))
        assert(evalV4(term) == expected)
    }

    test("V4: Case on non-empty list with single branch works") {
        // Only cons branch provided, no nil branch
        val term = Case(
          Const(Constant.List(intListType, List(Constant.Integer(42)))),
          List(LamAbs("h", LamAbs("t", Var(NamedDeBruijn("h", 0)))))
        )
        assert(evalV4(term) == Const(Constant.Integer(42)))
    }

    test("V4: Case on empty list with single branch throws CaseListBranchError") {
        // Only cons branch provided, but list is empty - needs nil branch
        val term = Case(
          Const(Constant.List(intListType, Nil)),
          List(LamAbs("h", LamAbs("t", Const(Constant.String("cons-only")))))
        )
        assertThrows[CaseListBranchError](evalV4(term))
    }

    test("V4: Case on list with no branches throws CaseListBranchError") {
        val term = Case(
          Const(Constant.List(intListType, Nil)),
          List()
        )
        assertThrows[CaseListBranchError](evalV4(term))
    }

    test("V4: Case on list with three branches throws CaseListBranchError") {
        val term = Case(
          Const(Constant.List(intListType, Nil)),
          List(
            Const(Constant.String("one")),
            Const(Constant.String("two")),
            Const(Constant.String("three"))
          )
        )
        assertThrows[CaseListBranchError](evalV4(term))
    }

    test("V3: Case on list throws NonConstrScrutinized") {
        val term = Case(
          Const(Constant.List(intListType, Nil)),
          List(
            LamAbs("h", LamAbs("t", Const(Constant.String("cons")))),
            Const(Constant.String("nil"))
          )
        )
        assertThrows[NonConstrScrutinized](evalV3(term))
    }

    // Nested case on list - compute list length
    test("V4: Nested case on list to compute length") {
        // length [] = 0
        // length (h:t) = 1 + length t
        // We'll test with a list of 3 elements
        // Branches order: [cons_branch, nil_branch]
        val list3 = Constant.List(
          intListType,
          List(
            Constant.Integer(1),
            Constant.Integer(2),
            Constant.Integer(3)
          )
        )

        // Manually unroll: Case [1,2,3] of cons h t -> 1 + (Case t of ...); nil -> 0
        val term = Case(
          Const(list3),
          List(
            LamAbs(
              "h",
              LamAbs(
                "t",
                Apply(
                  Apply(Builtin(DefaultFun.AddInteger), Const(Constant.Integer(1))),
                  Case(
                    Var(NamedDeBruijn("t", 0)),
                    List(
                      LamAbs(
                        "h2",
                        LamAbs(
                          "t2",
                          Apply(
                            Apply(Builtin(DefaultFun.AddInteger), Const(Constant.Integer(1))),
                            Case(
                              Var(NamedDeBruijn("t2", 0)),
                              List(
                                LamAbs(
                                  "h3",
                                  LamAbs(
                                    "t3",
                                    Apply(
                                      Apply(
                                        Builtin(DefaultFun.AddInteger),
                                        Const(Constant.Integer(1))
                                      ),
                                      Const(Constant.Integer(0)) // we know t3 is empty
                                    )
                                  )
                                ),
                                Const(Constant.Integer(0)) // nil -> 0
                              )
                            )
                          )
                        )
                      ),
                      Const(Constant.Integer(0)) // nil -> 0
                    )
                  )
                )
              )
            ),
            Const(Constant.Integer(0)) // nil -> 0
          )
        )

        assert(evalV4(term) == Const(Constant.Integer(3)))
    }

    // Case on Data tests for V4
    // Data has 5 constructors: Constr=0 (tag, args), Map=1, List=2, I=3, B=4
    // Each branch receives the inner value(s) as arguments

    import scalus.uplc.builtin.{ByteString, Data}
    import scalus.cardano.onchain.plutus.prelude.List as PList

    def mkDataConst(d: Data): Constant = Constant.Data(d)

    test("V4: Case on Data.Constr selects first branch and applies tag and args") {
        // Case on Data.Constr(42, [I(1), I(2)])
        // Constr branch: \tag args -> tag (returns the constructor tag)
        val term = Case(
          Const(mkDataConst(Data.Constr(42, PList(Data.I(1), Data.I(2))))),
          List(
            LamAbs("tag", LamAbs("args", Var(NamedDeBruijn("tag", 0)))),
            LamAbs("entries", Const(Constant.String("map"))),
            LamAbs("elements", Const(Constant.String("list"))),
            LamAbs("i", Const(Constant.String("integer"))),
            LamAbs("b", Const(Constant.String("bytestring")))
          )
        )
        assert(evalV4(term) == Const(Constant.Integer(42)))
    }

    test("V4: Case on Data.Constr can access args") {
        // Case on Data.Constr(0, [I(100)])
        // Constr branch: \tag args -> args (returns the args list)
        val term = Case(
          Const(mkDataConst(Data.Constr(0, PList(Data.I(100))))),
          List(
            LamAbs("tag", LamAbs("args", Var(NamedDeBruijn("args", 0)))),
            LamAbs("entries", Const(Constant.String("map"))),
            LamAbs("elements", Const(Constant.String("list"))),
            LamAbs("i", Const(Constant.String("integer"))),
            LamAbs("b", Const(Constant.String("bytestring")))
          )
        )
        val expected = Const(Constant.List(DefaultUni.Data, List(Constant.Data(Data.I(100)))))
        assert(evalV4(term) == expected)
    }

    test("V4: Case on Data.Map selects second branch and applies entries") {
        // Case on Data.Map([(I(1), I(2))])
        // Map branch: \entries -> "map"
        val term = Case(
          Const(mkDataConst(Data.Map(PList((Data.I(1), Data.I(2)))))),
          List(
            LamAbs("tag", LamAbs("args", Const(Constant.String("constr")))),
            LamAbs("entries", Const(Constant.String("map"))),
            LamAbs("elements", Const(Constant.String("list"))),
            LamAbs("i", Const(Constant.String("integer"))),
            LamAbs("b", Const(Constant.String("bytestring")))
          )
        )
        assert(evalV4(term) == Const(Constant.String("map")))
    }

    test("V4: Case on Data.List selects third branch and applies elements") {
        // Case on Data.List([I(1), I(2), I(3)])
        // List branch: \elements -> "list"
        val term = Case(
          Const(mkDataConst(Data.List(PList(Data.I(1), Data.I(2), Data.I(3))))),
          List(
            LamAbs("tag", LamAbs("args", Const(Constant.String("constr")))),
            LamAbs("entries", Const(Constant.String("map"))),
            LamAbs("elements", Const(Constant.String("list"))),
            LamAbs("i", Const(Constant.String("integer"))),
            LamAbs("b", Const(Constant.String("bytestring")))
          )
        )
        assert(evalV4(term) == Const(Constant.String("list")))
    }

    test("V4: Case on Data.I selects fourth branch and applies integer") {
        // Case on Data.I(42)
        // I branch: \i -> i (returns the integer)
        val term = Case(
          Const(mkDataConst(Data.I(42))),
          List(
            LamAbs("tag", LamAbs("args", Const(Constant.String("constr")))),
            LamAbs("entries", Const(Constant.String("map"))),
            LamAbs("elements", Const(Constant.String("list"))),
            LamAbs("i", Var(NamedDeBruijn("i", 0))),
            LamAbs("b", Const(Constant.String("bytestring")))
          )
        )
        assert(evalV4(term) == Const(Constant.Integer(42)))
    }

    test("V4: Case on Data.B selects fifth branch and applies bytestring") {
        // Case on Data.B(0xDEADBEEF)
        // B branch: \b -> b (returns the bytestring)
        val bs = ByteString.fromHex("DEADBEEF")
        val term = Case(
          Const(mkDataConst(Data.B(bs))),
          List(
            LamAbs("tag", LamAbs("args", Const(Constant.String("constr")))),
            LamAbs("entries", Const(Constant.String("map"))),
            LamAbs("elements", Const(Constant.String("list"))),
            LamAbs("i", Const(Constant.String("integer"))),
            LamAbs("b", Var(NamedDeBruijn("b", 0)))
          )
        )
        assert(evalV4(term) == Const(Constant.ByteString(bs)))
    }

    test("V4: Case on Data with fewer branches throws CaseDataBranchError for missing branch") {
        // Case on Data.I(42) with only 3 branches (needs 4 for I)
        val term = Case(
          Const(mkDataConst(Data.I(42))),
          List(
            LamAbs("tag", LamAbs("args", Const(Constant.String("constr")))),
            LamAbs("entries", Const(Constant.String("map"))),
            LamAbs("elements", Const(Constant.String("list")))
          )
        )
        assertThrows[CaseDataBranchError](evalV4(term))
    }

    test("V4: Case on Data with no branches throws CaseDataBranchError") {
        val term = Case(Const(mkDataConst(Data.I(42))), List())
        assertThrows[CaseDataBranchError](evalV4(term))
    }

    test("V4: Case on Data with more than 5 branches throws CaseDataBranchError") {
        val term = Case(
          Const(mkDataConst(Data.I(42))),
          List(
            Const(Constant.String("1")),
            Const(Constant.String("2")),
            Const(Constant.String("3")),
            Const(Constant.String("4")),
            Const(Constant.String("5")),
            Const(Constant.String("6"))
          )
        )
        assertThrows[CaseDataBranchError](evalV4(term))
    }

    test("V3: Case on Data throws NonConstrScrutinized") {
        val term = Case(
          Const(mkDataConst(Data.I(42))),
          List(
            LamAbs("tag", LamAbs("args", Const(Constant.String("constr")))),
            LamAbs("entries", Const(Constant.String("map"))),
            LamAbs("elements", Const(Constant.String("list"))),
            LamAbs("i", Var(NamedDeBruijn("i", 0))),
            LamAbs("b", Const(Constant.String("bytestring")))
          )
        )
        assertThrows[NonConstrScrutinized](evalV3(term))
    }

    // Test that we can use case on Data to implement pattern matching like chooseData
    test("V4: Case on Data can implement chooseData-like behavior") {
        // A function that returns a string describing the Data type
        def describeData(d: Data): Term = Case(
          Const(mkDataConst(d)),
          List(
            LamAbs("tag", LamAbs("args", Const(Constant.String("constr")))),
            LamAbs("entries", Const(Constant.String("map"))),
            LamAbs("elements", Const(Constant.String("list"))),
            LamAbs("i", Const(Constant.String("integer"))),
            LamAbs("b", Const(Constant.String("bytestring")))
          )
        )

        assert(evalV4(describeData(Data.Constr(0, PList.Nil))) == Const(Constant.String("constr")))
        assert(evalV4(describeData(Data.Map(PList.Nil))) == Const(Constant.String("map")))
        assert(evalV4(describeData(Data.List(PList.Nil))) == Const(Constant.String("list")))
        assert(evalV4(describeData(Data.I(0))) == Const(Constant.String("integer")))
        assert(
          evalV4(describeData(Data.B(ByteString.empty))) == Const(Constant.String("bytestring"))
        )
    }

    // Case on Pair tests for V4
    // Pair has exactly 1 branch that receives both left and right values

    test("V4: Case on Pair extracts left element") {
        // Case on Pair(42, False) -> returns left (42)
        val term = Case(
          Const(Constant.Pair(Constant.Integer(42), Constant.Bool(false))),
          List(
            LamAbs("left", LamAbs("right", Var(NamedDeBruijn("left", 0))))
          )
        )
        assert(evalV4(term) == Const(Constant.Integer(42)))
    }

    test("V4: Case on Pair extracts right element") {
        // Case on Pair(42, "hello") -> returns right ("hello")
        val term = Case(
          Const(Constant.Pair(Constant.Integer(42), Constant.String("hello"))),
          List(
            LamAbs("left", LamAbs("right", Var(NamedDeBruijn("right", 0))))
          )
        )
        assert(evalV4(term) == Const(Constant.String("hello")))
    }

    test("V4: Case on Pair with no branches throws CasePairBranchError") {
        val term = Case(
          Const(Constant.Pair(Constant.Integer(1), Constant.Integer(2))),
          List()
        )
        assertThrows[CasePairBranchError](evalV4(term))
    }

    test("V4: Case on Pair with two branches throws CasePairBranchError") {
        val term = Case(
          Const(Constant.Pair(Constant.Integer(1), Constant.Integer(2))),
          List(
            Const(Constant.String("branch1")),
            Const(Constant.String("branch2"))
          )
        )
        assertThrows[CasePairBranchError](evalV4(term))
    }

    test("V3: Case on Pair throws NonConstrScrutinized") {
        val term = Case(
          Const(Constant.Pair(Constant.Integer(42), Constant.Bool(false))),
          List(
            LamAbs("left", LamAbs("right", Var(NamedDeBruijn("left", 0))))
          )
        )
        assertThrows[NonConstrScrutinized](evalV3(term))
    }

    test("V4: Case on nested Pair") {
        // Case on Pair(Pair(1, 2), 3) -> returns inner pair
        val innerPair = Constant.Pair(Constant.Integer(1), Constant.Integer(2))
        val term = Case(
          Const(Constant.Pair(innerPair, Constant.Integer(3))),
          List(
            LamAbs("left", LamAbs("right", Var(NamedDeBruijn("left", 0))))
          )
        )
        assert(evalV4(term) == Const(innerPair))
    }
