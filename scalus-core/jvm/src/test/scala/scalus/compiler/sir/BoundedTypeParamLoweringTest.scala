package scalus.compiler.sir

import org.scalatest.funsuite.AnyFunSuite
import scalus.*
import scalus.cardano.onchain.plutus.prelude.List as PList
import scalus.compiler.{compile, Options}
import scalus.compiler.sir.SIR
import scalus.compiler.sir.SIRType
import scalus.compiler.sir.TargetLoweringBackend
import scalus.compiler.sir.lowering.{IntrinsicResolver, SirToUplcV3Lowering}

/** Regression test exposing the bounded-type-parameter lowering chain.
  *
  * Scala code under test (from the prelude `scalus.cardano.onchain.plutus.prelude.List[A]`, used
  * transitively by `flatMap`):
  *
  * {{{
  * extension [A](self: List[A])
  *   // bounded type parameter [B >: A]
  *   def appendedAll[B >: A](other: List[B]): List[B] = other.prependedAll(self)
  *
  *   inline def prependedAll[B >: A](other: List[B]): List[B] = { ... }
  *
  *   inline def ++[B >: A](other: List[B]): List[B] = appendedAll(other)
  *
  *   def foldRight[B](init: B)(combiner: (A, B) => B): B = ...
  *
  *   // uses ++ (= appendedAll[B >: A])
  *   def flatMap[B](mapper: A => List[B]): List[B] =
  *       foldRight(List.empty[B]) { (head, tail) => mapper(head) ++ tail }
  * }}}
  *
  * Caller pattern that triggers the bug (mirrors `KnightsTest.scala:447` `makeStarts`):
  *
  * {{{
  * val it = List.range(1, 3)
  * val l = it.flatMap { x => it.map { y => x + y } }
  * }}}
  *
  * ## Observations
  *
  *   1. The chain `flatMap → ++ → appendedAll[B >: A] → prependedAll[B >: A]` touches two
  *      bounded-type-parameter methods. `SIRType.substitute` produces, during lowering, an
  *      intermediate `rhs.sirType` of `List[A] -> (A -> List[B]) -> List[List[List[List[B]]]]`
  *      (4-deep) for the prelude's `flatMap` binding.
  *   2. At the SIR level, the binding's `b.tp` and `rhs.tp` are both correct (`[A] =>> List[A] ->
  *      [B] =>> (A -> List[B]) -> List[B]`). The deep chain only appears at *lowering time*, when
  *      the `rhs.sirType` of a `VariableLoweredValue` is read back.
  *   3. `Lowering.lowerLet:478` currently uses `b.tp` (declared) instead of `rhs.sirType` in the
  *      `SIR.Var` it constructs. This masks the deep-chain corruption — but leaves a type/repr
  *      consistency hole for let-bindings whose declared type carries a `@UplcRepr` annotation that
  *      the RHS representation doesn't satisfy.
  *
  * ## What this test does
  *
  * Compiles the caller pattern above and captures every `SIR.Let` binding (letrec vs let, declared
  * b.tp, rhs.tp, and RHS SIR). The assertion checks that no node in the compiled SIR carries a
  * 4-deep `List[List[List[List[…]]]]` chain.
  *
  * Currently the test **passes** (the SIR types are correct — the bug is masked during lowering by
  * the `b.tp` path). It will **fail** once either:
  *   - `Lowering.lowerLet` is switched to `rhs.sirType` (consistency fix), or
  *   - `SIRTyper` produces a deep chain at compile time (regression).
  *
  * At that point the dumped SIR output makes the cascade explicit, making the fix path obvious.
  */
class BoundedTypeParamLoweringTest extends AnyFunSuite {

    given Options = Options(
      targetLoweringBackend = TargetLoweringBackend.SirToUplcV3Lowering,
      generateErrorTraces = true,
      optimizeUplc = false,
      debug = false
    )

    test("compile RHS alone (no surrounding val l = ...)") {
        // The code that would normally sit as the RHS of `val l = …` in the caller —
        // compiled directly as the top-level expression with NO let-wrapping in the
        // user's code. If the buggy expression were the source of corruption, this
        // minimal form should still exhibit it.
        //
        // Observation: this test also passes. The SIR produced is a bare
        // `Apply(flatMap, single(1), λx. ...)` with no user-level let wrapping it.
        // All types in the tree (including the flatMap ExternalVar's tp) are correct.
        // The bug does NOT live in "the code under let" — it lives in the interaction
        // between lowering's `rhs.sirType` computation and the `@Compile` module
        // bindings (`List$.flatMap` etc.) brought in by the linker as let-bindings.
        val compiled = compile {
            PList.single(BigInt(1)).flatMap { x => PList.single(x + 1) }
        }

        val deepOffenders = scala.collection.mutable.ArrayBuffer.empty[(String, SIRType)]
        def checkTypes(node: SIR, _lns: Set[String], acc: Int): Int = {
            node match
                case SIR.ExternalVar(_, n, tp, _) if SIRType.hasDeepListChain(tp, 4) =>
                    deepOffenders.append((s"ExternalVar($n)", tp))
                case SIR.Apply(_, _, tp, _) if SIRType.hasDeepListChain(tp, 4) =>
                    deepOffenders.append(("Apply.tp", tp))
                case SIR.LamAbs(p, _, _, _) if SIRType.hasDeepListChain(p.tp, 4) =>
                    deepOffenders.append((s"LamAbs param ${p.name}", p.tp))
                case SIR.Let(bs, _, _, _) =>
                    bs.foreach(b =>
                        if SIRType.hasDeepListChain(b.tp, 4) then
                            deepOffenders.append((s"Let binding ${b.name}", b.tp))
                    )
                case _ => ()
            acc + 1
        }
        SIR.accumulate(compiled, 0, Set.empty, checkTypes)
        assert(
          deepOffenders.isEmpty,
          s"${deepOffenders.size} deep-chain node(s) in minimally-wrapped SIR:\n" +
              deepOffenders.map { case (w, t) => s"  $w: ${t.show}" }.mkString("\n")
        )

        // Lowering still succeeds here — the linker-introduced @Compile let-bindings
        // for flatMap/appendedAll/etc. are still present and get `b.tp` masking in
        // Lowering.lowerLet. That masking is what hides the bug, regardless of whether
        // the user wraps their code in a let.
        val lowering = new SirToUplcV3Lowering(
          compiled,
          generateErrorTraces = true,
          intrinsicModules = IntrinsicResolver.defaultIntrinsicModules,
          supportModules = IntrinsicResolver.defaultSupportModules,
          nativeListElements = false
        )
        lowering.lower()
    }

    test("bounded-type chain via flatMap/++/appendedAll produces no deep List in SIR") {
        // Scala code — exactly the pattern from KnightsTest.makeStarts that triggers
        // the bounded-type-parameter substitution bug during lowering.
        val compiled = compile {
            val it = PList.range(BigInt(1), BigInt(3))
            val l = it.flatMap { x => it.map { y => x + y } }
            l
        }

        // Dump every let binding discovered in the compiled SIR, so readers of a
        // failing test can see exactly which binding went wrong.
        case class Binding(name: String, isRec: Boolean, declTp: SIRType, rhs: SIR)
        val bindings = scala.collection.mutable.ArrayBuffer.empty[Binding]
        def collect(node: SIR, _lns: Set[String], acc: Int): Int = {
            node match
                case SIR.Let(bs, _, flags, _) =>
                    bs.foreach(b => bindings.append(Binding(b.name, flags.isRec, b.tp, b.value)))
                case _ => ()
            acc + 1
        }
        SIR.accumulate(compiled, 0, Set.empty, collect)

        info(s"${bindings.size} let binding(s) in compiled SIR:")
        bindings.foreach { b =>
            info(
              s"  ${if b.isRec then "LETREC" else "LET   "} ${b.name}: " +
                  s"b.tp=${b.declTp.show}, rhs.tp=${b.rhs.tp.show}"
            )
        }

        // Assert no binding's types carry a 4-deep List chain.
        val offenders = bindings.filter(b =>
            SIRType.hasDeepListChain(b.declTp, 4) || SIRType.hasDeepListChain(b.rhs.tp, 4)
        )
        assert(
          offenders.isEmpty,
          s"${offenders.size} binding(s) carry deep List chain:\n" +
              offenders
                  .map(b => s"  ${b.name}: b.tp=${b.declTp.show}, rhs.tp=${b.rhs.tp.show}")
                  .mkString("\n")
        )

        // Lower the compiled SIR — if the bounded-type bug resurfaces (e.g. via a
        // switch to rhs.sirType in Lowering.lowerLet, or a SIRTyper regression), this
        // throws LoweringException("Cannot unify result type of apply").
        val lowering = new SirToUplcV3Lowering(
          compiled,
          generateErrorTraces = true,
          intrinsicModules = IntrinsicResolver.defaultIntrinsicModules,
          supportModules = IntrinsicResolver.defaultSupportModules,
          nativeListElements = false
        )
        lowering.lower()
    }
}
