package scalus.examples.cape.fibonacci

import scalus.uplc.DefaultFun.{AddInteger, IfThenElse, LessThanEqualsInteger, SubtractInteger}
import scalus.uplc.Term
import scalus.uplc.Term.λ

import scala.language.implicitConversions

/** CAPE fibonacci open mode implementation.
  *
  * Hand-crafted UPLC using a fixed-point combinator. Uses naive recursion at the UPLC level.
  */
object FibonacciOpen {
    def term: Term = {
        import scalus.uplc.TermDSL.given
        def pfix(f: Term => Term) = λ { r => r $ r } $ λ { r => f(r $ r) }

        pfix: r =>
            λ: n =>
                !(!IfThenElse $ (LessThanEqualsInteger $ n $ 1) $
                    ~n $
                    ~(AddInteger $ (r $ (SubtractInteger $ n $ 1)) $ (r $ (SubtractInteger $ n $ 2))))
    }
}
