package scalus.uplc.jit

import org.scalatest.funsuite.AnyFunSuite
import scalus.cardano.ledger.{ExUnits, MajorProtocolVersion}
import scalus.uplc.{Constant, DeBruijn, DefaultUni, Term}
import scalus.uplc.builtin.{ByteString, Data}
import scalus.uplc.eval.*

/** The JIT returns the result of the CEK machine and spends the same budget, per budget category,
  * for the term shapes that the JIT code generators treat specially.
  *
  * The Plutus conformance suite ([[JitConformanceTest]]) checks the semantics and costing of every
  * builtin, but its terms are small and flat. This test covers the code paths the corpus does not
  * reach: a builtin applied in place, unapplied, forced, or passed around; let-bindings that are
  * inlined into a JVM method and the ones past the inlining limit; a large constant; `constr` and
  * `case`.
  */
class JitShapeTest extends AnyFunSuite {

    // A different cost for each step kind, so that a step charged with the cost of another kind
    // shows up. The real cost models charge all step kinds the same.
    private val params = {
        def step(n: Long) = ExUnits(memory = n, steps = 1000 * n)
        val defaults = PlutusVM.makePlutusV3VM().machineParams
        defaults.copy(machineCosts =
            CekMachineCosts(
              startupCost = step(1),
              varCost = step(2),
              constCost = step(3),
              lamCost = step(4),
              delayCost = step(5),
              forceCost = step(6),
              applyCost = step(7),
              builtinCost = step(8),
              constrCost = step(9),
              caseCost = step(10)
            )
        )
    }

    // Variant E (PlutusV3 at PV11), the scope of the JIT.
    private val vm = PlutusVM.makePlutusV3VM(params, MajorProtocolVersion.vanRossemPV)

    // Dijkstra (PV12): the CEK machine of the same variant, plus case on Data.
    private val dijkstraVM = PlutusVM.makePlutusV3VM(params, MajorProtocolVersion.dijkstraPV)

    /** Compiles `term` for the protocol version of `vm` and runs it with its machine parameters:
      * the JIT generates code for one protocol version.
      */
    private def runJit(term: Term, vm: PlutusVM): JitCekDiff.Run[UplcValue] =
        JitCekDiff.runJit(
          nativestack.JIT
              .jitUplc(term, nativestack.NativeStackContext.DefaultMaxDepth, vm.protocolVersion),
          vm.machineParams
        )

    private def assertSameAsCek(term: Term, vm: PlutusVM = vm): Unit = {
        val cek = JitCekDiff.runCek(vm, term)
        val jitted = runJit(term, vm)
        // also a failed evaluation must stop at the same point and spend the same budget
        assert(
          JitCekDiff.same(cek, jitted),
          s"CEK: ${cek.result} ${cek.total}, JIT: ${jitted.result} ${jitted.total}\n" +
              s"CEK costs ${cek.costs}\nJIT costs ${jitted.costs}"
        )
    }

    private def parse(uplc: String): Term =
        Term.parseUplc(uplc).fold(err => fail(err), identity)

    /** `[(lam x1 ... [(lam xn body) (con integer n)] ...) (con integer 1)]`: `n` nested lets whose
      * body adds them all up. More lets than the nativestack inlining limit (20 terms per method).
      */
    private def nestedLets(n: Int): String = {
        val sum = (2 to n).foldLeft("x1")((acc, i) => s"[(builtin addInteger) $acc x$i]")
        (1 to n).foldRight(sum)((i, body) => s"[(lam x$i $body) (con integer $i)]")
    }

    /** A `Data` constant with a list of `n` integers, as large as an applied script context. */
    private def largeData(n: Int): String =
        (1 to n).map(i => s"I $i").mkString("(con data (List [", ", ", "]))")

    private val terms = List(
      // Fully applied builtins with simple arguments take the inlined path. Each argument term
      // must spend its step once: the generators used to splice an argument twice (cost and
      // result) or, for mkNilData, not at all.
      "2-arg builtin, const args" -> "[(builtin addInteger) (con integer 1) (con integer 2)]",
      "2-arg builtin, var args" -> "[(lam x [(builtin equalsInteger) x x]) (con integer 7)]",
      "1-arg builtin, var arg" -> "[(lam x [(builtin unIData) x]) (con data (I 5))]",
      "1-arg builtin, unused arg" -> "[(builtin mkNilData) (con unit ())]",
      "3-arg builtin, const args" ->
          "[(builtin sliceByteString) (con integer 1) (con integer 2) (con bytestring #010203)]",
      "applied builtin, non-simple arg" ->
          "[(builtin addInteger) [(builtin addInteger) (con integer 1) (con integer 2)] (con integer 3)]",
      "applied builtin, lambda arg" ->
          "[(builtin bData) [(lam x x) (con bytestring #ff)]]",
      "applied builtin fails" -> "[(builtin divideInteger) (con integer 1) (con integer 0)]",
      // Unapplied builtins: the curried form, passed around and applied later.
      "unapplied builtin" -> "(builtin addInteger)",
      "partly applied builtin" -> "[(builtin addInteger) (con integer 1)]",
      "builtin passed to a lambda" ->
          "[(lam f [f (con integer 1) (con integer 2)]) (builtin addInteger)]",
      "partly applied builtin passed to a lambda" ->
          "[(lam f [f (con integer 2)]) [(builtin multiplyInteger) (con integer 3)]]",
      // Forced (polymorphic) builtins.
      "forced builtin" ->
          "[(force (builtin ifThenElse)) (con bool True) (con integer 1) (con integer 2)]",
      "forced builtin, delayed branches" ->
          "(force [(force (builtin ifThenElse)) (con bool False) (delay (error)) (delay (con integer 2))])",
      "headList" -> "[(force (builtin headList)) (con (list integer) [1, 2])]",
      "headList of empty list fails" -> "[(force (builtin headList)) (con (list integer) [])]",
      "tailList" -> "[(force (builtin tailList)) (con (list integer) [1, 2])]",
      "nullList" -> "[(force (builtin nullList)) (con (list integer) [])]",
      "mkCons" -> "[(force (builtin mkCons)) (con integer 0) (con (list integer) [1, 2])]",
      "chooseList, two forces" ->
          "[(force (force (builtin chooseList))) (con (list integer) []) (con integer 1) (con integer 2)]",
      "fstPair" -> "[(force (force (builtin fstPair))) (con (pair integer bool) (1, True))]",
      "sndPair" -> "[(force (force (builtin sndPair))) (con (pair integer bool) (1, True))]",
      "trace" -> "[(force (builtin trace)) (con string \"hi\") (con integer 1)]",
      "chooseData" ->
          "[(force (builtin chooseData)) (con data (I 1)) (con integer 0) (con integer 1) (con integer 2) (con integer 3) (con integer 4)]",
      "forced builtin, unforced" -> "(builtin headList)",
      "forced builtin, partly applied" -> "[(force (builtin ifThenElse)) (con bool True)]",
      // Pairs and maps cross the builtins as one representation.
      "mkPairData, then fstPair" ->
          "[(force (force (builtin fstPair))) [(builtin mkPairData) (con data (I 1)) (con data (B #00))]]",
      "unMapData, then mapData" ->
          "[(builtin mapData) [(builtin unMapData) (con data (Map [(I 1, B #00)]))]]",
      "unConstrData, then sndPair" ->
          "[(force (force (builtin sndPair))) [(builtin unConstrData) (con data (Constr 1 [I 2]))]]",
      // Let-bindings: nativestack inlines `[(lam x b) a]` while a method has < 20 terms.
      "one let" -> "[(lam x [(builtin addInteger) x (con integer 1)]) (con integer 2)]",
      "let with a non-simple argument" ->
          "[(lam x [(builtin addInteger) x x]) [(builtin addInteger) (con integer 1) (con integer 2)]]",
      "let shadows a let" -> "[(lam x [(lam x x) (con integer 2)]) (con integer 1)]",
      "30 nested lets, past the inlining limit" -> nestedLets(30),
      // Constants.
      "large constant" -> s"[(builtin unListData) ${largeData(2000)}]",
      "two constants with equal values" ->
          "(constr 0 (con (list integer) []) (con (list bool) []) (con integer 5))",
      "constant used twice" ->
          "[(lam x [(builtin equalsData) x (con data (I 7))]) (con data (I 7))]",
      // Constr and case.
      "constr and case" -> "(case (constr 1 (con integer 2)) (lam a a) (lam b b))",
      "constr result" -> "(constr 3 (con integer 1) (con bytestring #00))",
      "case with two fields" ->
          "(case (constr 0 (con integer 2) (con integer 3)) (lam a (lam b [(builtin addInteger) a b])))",
      "case on a bool" -> "(case (con bool True) (con integer 0) (con integer 1))",
      "case on an integer" -> "(case (con integer 1) (con integer 0) (con integer 1))",
      "case on a list" ->
          "(case (con (list integer) [5]) (lam h (lam t h)) (con integer 0))",
      "case, missing branch fails" -> "(case (constr 2) (con integer 0))",
      "delay and force" -> "[(force (delay (lam x x))) (con integer 1)]",
      "error" -> "(error)",
      // Failures: the JIT must fail at the same step as the CEK machine and spend the same budget.
      "apply a non-function, after its argument" ->
          "[(con integer 1) [(builtin addInteger) (con integer 1) (con integer 2)]]",
      "force a non-delay" -> "(force (con integer 1))",
      "force a failing term" -> "(force [(builtin divideInteger) (con integer 1) (con integer 0)])",
      "builtin on a wrong-typed argument, costed first" ->
          "[(builtin addInteger) (con integer 1) (con bool True)]",
      "bridged builtin on a function" ->
          "[(builtin appendString) (lam x x) (con string \"a\")]",
      "unapplied bridged builtin on a function" ->
          "[(lam f [f (lam x x) (con string \"a\")]) (builtin appendString)]",
      "mkCons of an element of another type" ->
          "[(force (builtin mkCons)) (con integer 0) (con (list data) [])]",
      "dropList with a non-integer count" ->
          "[(force (builtin dropList)) (con bool True) (con (list integer) [1])]",
      "over-applied builtin" ->
          "[(builtin addInteger) (con integer 1) (con integer 2) (con integer 3)]",
      "forced builtin applied without its force" -> "[(builtin headList) (con (list integer) [1])]",
      "constr with a failing argument" -> "(constr 0 (con integer 1) (error) (con integer 2))",
      "case on an empty list with no nil branch" ->
          "(case (con (list integer) []) (lam h (lam t h)))",
      "case on a pair" -> "(case (con (pair integer bool) (1, True)) (lam a (lam b a)))",
      "case on data" -> "(case (con data (I 1)) (lam x x))",
      "case on a function" -> "(case (lam x x) (lam x x))",
      "case branch applied to too many fields" ->
          "(case (constr 0 (con integer 1) (con integer 2)) (lam a a))",
      // apply and force on the wrong kind of value: each fails with its own CEK error class
      "apply a delay" -> "[(delay (con integer 1)) (con integer 2)]",
      "force a lambda" -> "(force (lam x x))",
      "force a builtin that takes an argument" -> "(force (builtin addInteger))",
      "apply a builtin that takes a force" -> "[(builtin ifThenElse) (con bool True)]",
      // case on builtins: branch counts and indices at the edges
      "case with no branches on a constr" -> "(case (constr 0))",
      "case with no branches on an integer" -> "(case (con integer 0))",
      "case on unit with 2 branches" -> "(case (con unit ()) (con integer 1) (con integer 2))",
      "case on True with 1 branch" -> "(case (con bool True) (con integer 0))",
      "case on a bool with 3 branches" ->
          "(case (con bool False) (con integer 0) (con integer 1) (con integer 2))",
      "case on a negative integer" -> "(case (con integer -1) (con integer 0))",
      "case on constr tag 2^31" -> "(case (constr 2147483648) (con integer 0))",
      "case on constr tag 2^64-1" -> "(case (constr 18446744073709551615) (con integer 0))",
      "case on a list with 3 branches" ->
          "(case (con (list integer) [1]) (lam h (lam t h)) (con integer 0) (con integer 1))",
      "case on a pair with 2 branches" ->
          "(case (con (pair integer bool) (1, True)) (lam a (lam b a)) (con integer 0))",
      "case field applied to a non-function" -> "(case (constr 0 (con integer 1)) (con integer 5))",
      // a free variable fails when it is evaluated, not when the script is compiled
      "free variable" -> "x",
      "free variable in an applied lambda" -> "[(lam y x) (con integer 1)]",
      // the bridge with values that have no constant syntax: a BLS12-381 ML result, a Value
      "BLS12-381 Miller loop and final verify" ->
          "[(builtin bls12_381_finalVerify) [(builtin bls12_381_millerLoop) [(builtin bls12_381_G1_hashToGroup) (con bytestring #01) (con bytestring #02)] [(builtin bls12_381_G2_hashToGroup) (con bytestring #01) (con bytestring #02)]] [(builtin bls12_381_millerLoop) [(builtin bls12_381_G1_hashToGroup) (con bytestring #01) (con bytestring #02)] [(builtin bls12_381_G2_hashToGroup) (con bytestring #01) (con bytestring #02)]]]",
      "valueData of unValueData" -> "[(builtin valueData) [(builtin unValueData) (con data (Map []))]]"
    )

    for (name, uplc) <- terms do
        test(s"$name, as the CEK machine") {
            assertSameAsCek(parse(uplc))
        }

    // Case on Data (Plutus `caseBuiltinData`): a Data.Constr selects the branch of its tag, and
    // the branch gets one argument, the fields as a `(list data)`. Every other Data fails. Each
    // branch returns something different, so a wrong branch or wrong fields change the result.
    private def dataCase(data: String): String =
        s"(case (con data ($data)) (lam fs (con integer 10)) (lam fs fs) (lam fs [(force (builtin headList)) fs]))"

    private val dataCaseTerms = List(
      "Constr 0 selects the first branch" -> dataCase("Constr 0 [I 7]"),
      "Constr 1 passes the fields as one list" -> dataCase("Constr 1 [I 7, B #00]"),
      "Constr 2 with no fields" -> dataCase("Constr 2 []"),
      "Constr 2, the head of the fields" -> dataCase("Constr 2 [I 7, B #00]"),
      "Constr with a tag past the last branch" -> dataCase("Constr 3 [I 7]"),
      "Constr with a huge tag" -> dataCase("Constr 18446744073709551616 []"),
      "Map" -> dataCase("Map [(I 1, I 2)]"),
      "List" -> dataCase("List [I 1]"),
      "I" -> dataCase("I 1"),
      "B" -> dataCase("B #00"),
      "Constr with no branches" -> "(case (con data (Constr 0 [])))",
      "Constr, branch applied to too many fields" ->
          "(case (con data (Constr 0 [I 1])) (lam a (lam b a)))",
      "Constr, branch not a function" -> "(case (con data (Constr 0 [I 1])) (con integer 5))"
    )

    for (name, uplc) <- dataCaseTerms do
        // the same term compiled twice, once per protocol version
        test(s"case on Data at PV12, $name, as the CEK machine") {
            assertSameAsCek(parse(uplc), dijkstraVM)
        }
        test(s"case on Data at PV11, $name, fails as the CEK machine") {
            val term = parse(uplc)
            assertSameAsCek(term)
            // also not vacuous: both machines reject it, with the PV11 error
            runJit(term, vm).result.failed.get match
                case _: CaseDataNotSupportedError =>
                case other => fail(s"expected CaseDataNotSupportedError, got $other")
        }

    test("case on Data at PV12 gives the values of Plutus caseBuiltinData") {
        // what the comparison with the CEK machine above does not show on its own
        def jit(data: String): UplcValue = runJit(parse(dataCase(data)), dijkstraVM).result.get
        assert(jit("Constr 0 [I 7]") == Constant.Integer(10))
        assert(
          jit("Constr 1 [I 7, B #00]") == Constant.List(
            DefaultUni.Data,
            List(Constant.Data(Data.I(7)), Constant.Data(Data.B(ByteString.fromHex("00"))))
          )
        )
        assert(jit("Constr 2 [I 7]") == Constant.Data(Data.I(7)))
    }

    /** Records the category of every spend, in order. */
    private final class OrderSpender extends BudgetSpender {
        val categories = collection.mutable.ArrayBuffer.empty[ExBudgetCategory]
        def spendBudget(cat: ExBudgetCategory, budget: ExUnits, env: CekValEnv): Unit =
            categories += cat
        def getSpentBudget: ExUnits = ExUnits.zero
    }

    // A saturated builtin spends its steps in CEK order (Apply, Force, Builtin, then the
    // arguments, then the builtin cost), so a RestrictingBudgetSpender stops where the CEK does.
    private val appliedTerms = List(
      "2 args" -> "[(lam x [(builtin addInteger) x (con integer 1)]) (con integer 2)]",
      "1 force" -> "[(force (builtin headList)) (con (list integer) [1])]",
      "2 forces" -> "[(force (force (builtin fstPair))) (con (pair integer bool) (1, True))]",
      "bridged" -> "[(builtin appendString) (con string \"a\") (con string \"b\")]",
      "computed argument" ->
          "[(force (builtin ifThenElse)) [(builtin lessThanInteger) (con integer 1) (con integer 2)] (con integer 1) (con integer 2)]"
    )

    for (name, uplc) <- appliedTerms do
        test(s"applied builtin, $name, spends in CEK order") {
            val term = parse(uplc)
            val cek = OrderSpender()
            vm.evaluateDeBruijnedTerm(DeBruijn.deBruijnTerm(term), cek, NoLogger)
            val jitted = OrderSpender()
            nativestack.JIT.jitUplc(term)(NoLogger, jitted, params)
            assert(jitted.categories == cek.categories)
        }

    // The largest corpus script, by term nodes: every lambda and every variable it captures adds
    // entries to the constant pool of the generated class, which holds at most 65 535 ("Class too
    // large").
    test("the largest corpus script compiles and spends the CEK budget") {
        assertSameAsCek(JitCorpus.all.maxBy(s => JitCorpus.nodes(s.term)).term)
    }
}
