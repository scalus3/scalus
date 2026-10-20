package scalus.uplc.jit

import org.scalatest.funsuite.AnyFunSuite
import scalus.cardano.ledger.ExUnits
import scalus.uplc.{DeBruijn, DeBruijnedProgram, Term}
import scalus.uplc.eval.*

import scala.util.Using

/** The JIT spends the same budget as the CEK machine, per budget category. */
class JitBudgetTest extends AnyFunSuite {

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
    private val vm = PlutusVM.makePlutusV3VM(params)

    private def assertSameBudget(jit: JitRunner, term: Term): Unit = {
        val cek = TallyingBudgetSpender(CountingBudgetSpender())
        vm.evaluateDeBruijnedTerm(DeBruijn.deBruijnTerm(term), cek, NoLogger)
        val jitted = TallyingBudgetSpender(CountingBudgetSpender())
        jit.jitUplc(term)(NoLogger, jitted, params)
        assert(jitted.costs.toMap == cek.costs.toMap)
        assert(jitted.getSpentBudget == cek.getSpentBudget)
    }

    private def parse(uplc: String): Term =
        Term.parseUplc(uplc).fold(err => fail(err), identity)

    private val terms = List(
      // Fully-applied builtins with simple arguments take the inlined path. Each argument term
      // must spend its step once: the generators used to splice an argument twice (cost and
      // result) or, for mkNilData, not at all.
      "2-arg builtin, const args" -> "[(builtin addInteger) (con integer 1) (con integer 2)]",
      "2-arg builtin, var args" -> "[(lam x [(builtin equalsInteger) x x]) (con integer 7)]",
      "1-arg builtin, var arg" -> "[(lam x [(builtin unIData) x]) (con data (I 5))]",
      "1-arg builtin, unused arg" -> "[(builtin mkNilData) (con unit ())]",
      "forced builtin" ->
          "[(force (builtin ifThenElse)) (con bool True) (con integer 1) (con integer 2)]",
      "delay and force" -> "[(force (delay (lam x x))) (con integer 1)]",
      "constr and case" -> "(case (constr 1 (con integer 2)) (lam a a) (lam b b))"
    )

    for (jitName, jit) <- List("nativestack" -> nativestack.JIT, "mincont" -> mincont.JIT) do
        for (name, uplc) <- terms do
            test(s"$jitName JIT: $name spends the CEK budget") {
                assertSameBudget(jit, parse(uplc))
            }

    test("nativestack JIT spends the CEK budget on guardrail-sorted-small") {
        val stream = getClass.getClassLoader.getResourceAsStream(
          "data/plutus_use_cases/guardrail-sorted-small.flat"
        )
        assert(stream != null)
        val bytes = Using.resource(stream)(_.readAllBytes())
        val term = DeBruijnedProgram.fromFlatEncoded(bytes).toProgram.term
        assertSameBudget(nativestack.JIT, term)
    }
}
