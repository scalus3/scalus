package scalus.compiler

import org.scalatest.funsuite.AnyFunSuite
import scalus.*
import scalus.compiler.compile
import scalus.compiler.sir.{SIR, SIRType}

import scala.language.implicitConversions

/** Audit finding T3: TypeVar ids for binder-scoped type params (PolyType/HKTypeLambda) were derived
  * from `typeSymbol.hashCode`, which resolves through the param's *upper bound* — so every
  * unbounded `[A]` program-wide got the same id (`id(AnyClass)`), and same-named type params of
  * different methods became the *identical* SIR type variable. Ids are now allocated per binder
  * instance, so distinct generic methods get distinct variables.
  */
class TypeVarIdCollapseTest extends AnyFunSuite {

    private def typeVarsNamed(tp: SIRType, name: String): Set[SIRType.TypeVar] = {
        val acc = new SIRType.SetBasedTypeVarGenerationContext(Set.empty, 0L)
        acc.importSetFromType(tp)
        acc.typeVars.filter(_.name == name)
    }

    test("same-named type params of different generic methods get distinct TypeVar ids") {
        import scalus.cardano.onchain.plutus.prelude.*
        val sir = compile { (xs: List[BigInt]) =>
            xs.flatMap(x => List.Cons(x, List.Nil)).map(x => x + BigInt(1))
        }
        val externalVars = SIR.accumulate[Map[String, SIRType]](
          sir,
          Map.empty,
          Set.empty,
          (s, _, acc) =>
              s match {
                  case ev: SIR.ExternalVar => acc + (ev.name -> ev.tp)
                  case _                   => acc
              }
        )
        def tpOf(suffix: String): SIRType =
            externalVars.collectFirst { case (n, tp) if n.endsWith(suffix) => tp }.getOrElse {
                this.fail(
                  s"no ExternalVar ending in '$suffix' among: ${externalVars.keys.mkString(", ")}"
                )
            }
        val mapBs = typeVarsNamed(tpOf(".map"), "B")
        val flatMapBs = typeVarsNamed(tpOf(".flatMap"), "B")
        assert(mapBs.nonEmpty, "map's type should contain a type variable B")
        assert(flatMapBs.nonEmpty, "flatMap's type should contain a type variable B")
        assert(
          mapBs.intersect(flatMapBs).isEmpty,
          s"map and flatMap must not share a B type variable: " +
              s"map=${mapBs.map(_.showDebug)}, flatMap=${flatMapBs.map(_.showDebug)}"
        )
    }
}
