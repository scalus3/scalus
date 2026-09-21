package scalus.uplc.eval

import org.scalatest.funsuite.AnyFunSuite
import scalus.cardano.ledger.{ExUnits, MajorProtocolVersion, Word64}
import scalus.uplc.*
import scalus.uplc.Term.*

import scala.language.implicitConversions

class CaseOnBuiltinsTest extends AnyFunSuite:

    val v4vm: PlutusVM = PlutusVM.makePlutusV3VM(MajorProtocolVersion.vanRossemPV)
    // The "V3: ..." tests document the pre-van-Rossem behavior (case-on-builtins rejected),
    // so this VM pins plominPV — the default V3 VM is PV11 now and accepts them.
    val v3vm: PlutusVM = PlutusVM.makePlutusV3VM(MajorProtocolVersion.plominPV)

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

    test("V4: Case on Constr with an out-of-range tag throws MissingCaseBranch") {
        val term = Case(
          Constr(Word64(2), Nil),
          List(Const(Constant.String("zero")), Const(Constant.String("one")))
        )
        assertThrows[MissingCaseBranch](evalV4(term))
    }

    // A constructor tag is an unsigned 64-bit value stored in a signed Long, so every tag from
    // 2^63 up is a negative Long. A signed range check lets those through and `toInt` then
    // truncates to an arbitrary — usually in-range — branch index, silently running the wrong
    // branch. Upstream answers MissingCaseBranch for any tag > maxBound::Int
    // (`Cek/Internal.hs`). These pin the unsigned comparison.
    test("V4: Case on Constr with a tag >= 2^63 throws MissingCaseBranch, not a wrong branch") {
        def caseOn(tag: Word64): Term = Case(
          Constr(tag, Nil),
          List(Const(Constant.Integer(100)), Const(Constant.Integer(200)))
        )
        // 2^63: truncates to Int 0, which would have selected branch 0
        val twoPow63 = Word64(Long.MinValue)
        assert(twoPow63.toUnsignedString == "9223372036854775808")
        assertThrows[MissingCaseBranch](evalV4(caseOn(twoPow63)))
        // 2^63 + 1: truncates to Int 1, which would have selected branch 1
        val twoPow63Plus1 = Word64(Long.MinValue + 1)
        assert(twoPow63Plus1.toUnsignedString == "9223372036854775809")
        assertThrows[MissingCaseBranch](evalV4(caseOn(twoPow63Plus1)))
        // 2^64 - 1
        assertThrows[MissingCaseBranch](evalV4(caseOn(Word64(-1L))))
    }

    test("V4: Case on Constr with a tag above Int range fails as a MachineError") {
        // 2^32 + 1 truncates to Int 1. This used to fail a `require`, throwing
        // IllegalArgumentException out of the machine instead of a MachineError.
        val term = Case(
          Constr(Word64(4294967297L), Nil),
          List(Const(Constant.Integer(100)), Const(Constant.Integer(200)))
        )
        assertThrows[MissingCaseBranch](evalV4(term))
    }

    test("Constr tags >= 2^63 are legal UPLC: print unsigned, parse back, flat round-trip") {
        // Any Word64 is a legal tag: UntypedPlutusCore/Parser.hs `constrTerm` rejects only
        // values above maxBound :: Word64, and Note [Constr tag type] chose Word64 precisely
        // to rule out negative tags. The printer used to write `tag.value`, a negative
        // number, which no parser accepts.
        val tag = Word64(Long.MinValue + 1) // 2^63 + 1
        val term = Constr(tag, Nil)
        val rendered = term.pretty.render(80)
        assert(rendered == "(constr 9223372036854775809)", rendered)
        UplcParser().parseTerm(rendered) match
            case Right(Constr(parsedTag, Nil, _)) => assert(parsedTag == tag)
            case other => fail(s"did not parse back to the same constr: $other")
        val program = Program((1, 1, 0), term)
        Program.fromFlatEncoded(program.flatEncoded).term match
            case Constr(decodedTag, Nil, _) => assert(decodedTag == tag)
            case other                      => fail(s"flat round-trip lost the tag: $other")
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

    // Case on Data tests
    // Casing on Data arrives with Dijkstra (PV12), one hard fork after the rest of
    // case-on-builtins. Only Data.Constr can be scrutinized: the branch is selected by the
    // constructor tag and receives the list of fields as its single argument.

    import scalus.uplc.builtin.{ByteString, Data}
    import scalus.cardano.onchain.plutus.prelude.List as PList

    val pv12vm: PlutusVM = PlutusVM.makePlutusV3VM(MajorProtocolVersion.dijkstraPV)

    def evalPV12(term: Term): Term =
        pv12vm.evaluateDeBruijnedTerm(DeBruijn.deBruijnTerm(term))

    def mkDataConst(d: Data): Constant = Constant.Data(d)

    def fieldsBranch(result: String): Term = LamAbs("fields", Const(Constant.String(result)))

    test("PV12: Case on Data.Constr selects the branch by the constructor tag") {
        def term(tag: Int) = Case(
          Const(mkDataConst(Data.Constr(tag, PList(Data.I(1), Data.I(2))))),
          List(fieldsBranch("zero"), fieldsBranch("one"), fieldsBranch("two"))
        )
        assert(evalPV12(term(0)) == Const(Constant.String("zero")))
        assert(evalPV12(term(1)) == Const(Constant.String("one")))
        assert(evalPV12(term(2)) == Const(Constant.String("two")))
    }

    test("PV12: Case on Data.Constr applies the fields list to the branch") {
        val term = Case(
          Const(mkDataConst(Data.Constr(1, PList(Data.I(100), Data.B(ByteString.fromHex("FF")))))),
          List(
            fieldsBranch("zero"),
            LamAbs("fields", Var(NamedDeBruijn("fields", 0)))
          )
        )
        val expected = Const(
          Constant.List(
            DefaultUni.Data,
            List(Constant.Data(Data.I(100)), Constant.Data(Data.B(ByteString.fromHex("FF"))))
          )
        )
        assert(evalPV12(term) == expected)
    }

    test("PV12: Case on Data.Constr without fields applies the empty list") {
        val term = Case(
          Const(mkDataConst(Data.Constr(0, PList.Nil))),
          List(LamAbs("fields", Var(NamedDeBruijn("fields", 0))))
        )
        assert(evalPV12(term) == Const(Constant.List(DefaultUni.Data, Nil)))
    }

    test("PV12: Case on Data.Constr with a tag without a branch throws CaseIndexOutOfBounds") {
        val term = Case(
          Const(mkDataConst(Data.Constr(2, PList.Nil))),
          List(fieldsBranch("zero"), fieldsBranch("one"))
        )
        assertThrows[CaseIndexOutOfBounds](evalPV12(term))
    }

    test("PV12: Case on Data.Constr with no branches throws CaseIndexOutOfBounds") {
        val term = Case(Const(mkDataConst(Data.Constr(0, PList.Nil))), List())
        assertThrows[CaseIndexOutOfBounds](evalPV12(term))
    }

    test("PV12: Case on Data.Map/List/I/B throws CaseDataNonConstrError") {
        def term(d: Data) = Case(
          Const(mkDataConst(d)),
          List(
            fieldsBranch("0"),
            fieldsBranch("1"),
            fieldsBranch("2"),
            fieldsBranch("3"),
            fieldsBranch("4")
          )
        )
        assertThrows[CaseDataNonConstrError](
          evalPV12(term(Data.Map(PList((Data.I(1), Data.I(2))))))
        )
        assertThrows[CaseDataNonConstrError](evalPV12(term(Data.List(PList(Data.I(1))))))
        assertThrows[CaseDataNonConstrError](evalPV12(term(Data.I(42))))
        assertThrows[CaseDataNonConstrError](evalPV12(term(Data.B(ByteString.fromHex("DEADBEEF")))))
    }

    test("PV12: the rest of case-on-builtins stays available") {
        val term = Case(
          Const(Constant.Integer(1)),
          List(Const(Constant.String("zero")), Const(Constant.String("one")))
        )
        assert(evalPV12(term) == Const(Constant.String("one")))
    }

    test("V4: Case on Data is not available at van Rossem (PV11)") {
        val onConstr = Case(
          Const(mkDataConst(Data.Constr(0, PList(Data.I(1))))),
          List(fieldsBranch("zero"))
        )
        val onI = Case(Const(mkDataConst(Data.I(42))), List(fieldsBranch("zero")))
        assertThrows[CaseDataNotSupportedError](evalV4(onConstr))
        assertThrows[CaseDataNotSupportedError](evalV4(onI))
    }

    test("V3: Case on Data throws NonConstrScrutinized") {
        val term = Case(
          Const(mkDataConst(Data.Constr(0, PList.Nil))),
          List(fieldsBranch("zero"))
        )
        assertThrows[NonConstrScrutinized](evalV3(term))
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
