package scalus.compiler.sir.lowering

import org.scalatest.funsuite.AnyFunSuite
import scalus.*
import scalus.compiler.Options
import scalus.cardano.onchain.plutus.prelude.{AssocMap, AssocMapEq, List}
import scalus.uplc.PlutusV3
import scalus.uplc.Term.asTerm
import scalus.uplc.eval.PlutusVM

/** `AssocMap` has no `Eq`: its equality is order-insensitive (non-structural). Value equality is
  * via `AssocMapEq.equals`; `===` / `==` on `AssocMap` is a compile error. See
  * `docs/local/claude/compiler/v3-eq-eliminating.md`.
  */
class AssocMapEqLoweringTest extends AnyFunSuite {

    private given PlutusVM = PlutusVM.makePlutusV3VM()
    private given Options = Options.release.copy(noWarn = true)

    test("AssocMapEq.equals is order-insensitive on-chain") {
        val compiled = PlutusV3.compile {
            val m1 = AssocMap.unsafeFromList(
              List.Cons((BigInt(1), BigInt(2)), List.Cons((BigInt(0), BigInt(0)), List.Nil))
            )
            val m2 = AssocMap.unsafeFromList(
              List.Cons((BigInt(0), BigInt(0)), List.Cons((BigInt(1), BigInt(2)), List.Nil))
            )
            AssocMapEq.equals(m1, m2)
        }
        assert(compiled.program.term.evaluate == true.asTerm)
    }

    test("AssocMapEq.equals distinguishes different maps on-chain") {
        val compiled = PlutusV3.compile {
            val m1 = AssocMap.unsafeFromList(
              List.Cons((BigInt(1), BigInt(2)), List.Cons((BigInt(0), BigInt(0)), List.Nil))
            )
            val m3 = AssocMap.unsafeFromList(List.Cons((BigInt(1), BigInt(2)), List.Nil))
            AssocMapEq.equals(m1, m3)
        }
        assert(compiled.program.term.evaluate == false.asTerm)
    }
}
