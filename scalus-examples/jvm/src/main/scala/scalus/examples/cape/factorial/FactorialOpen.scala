package scalus.examples.cape.factorial

import scalus.uplc.DefaultFun.{IfThenElse, LessThanEqualsInteger, MultiplyInteger, SubtractInteger}
import scalus.uplc.Term
import scalus.uplc.Term.{asTerm, λ}

import scala.language.implicitConversions

/** CAPE factorial open mode implementation.
  *
  * Hand-crafted UPLC using a fixed-point combinator for optimal script size and execution cost.
  */
object FactorialOpen {
    def term: Term = {
        import scalus.uplc.TermDSL.given
        def pfix(f: Term => Term) = λ { r => r $ r } $ λ { r => f(r $ r) }

        pfix: r =>
            λ: x =>
                !(!IfThenElse $ (LessThanEqualsInteger $ x $ 0) $
                    ~1.asTerm $
                    ~(MultiplyInteger $ x $ (r $ (SubtractInteger $ x $ 1))))
    }
}
