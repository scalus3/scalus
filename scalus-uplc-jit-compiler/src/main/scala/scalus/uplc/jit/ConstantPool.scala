package scalus.uplc.jit

import scalus.uplc.{Constant, Term}
import scalus.uplc.internal.TermTraversal.foreach

import scala.collection.mutable
import scala.quoted.*

/** The constants of one script, collected at JIT-compile time.
  *
  * The generated code does not build a constant: it reads it from an `Array[UplcValue]` that the
  * compiled function receives once, after `staging.run` (see [[ConstantPool.compile]]). So a
  * constant never becomes bytecode: a large one (an applied script context) costs no compile time
  * and cannot reach the 64 KB limit of a JVM method. All evaluations share the constants, as the
  * CEK machine shares the constants of the term. A value is the `Constant` of the term itself, the
  * value the CEK machine holds in `VCon`.
  *
  * @param index
  *   the position in [[values]] of each constant, in the order of its first occurrence
  * @param values
  *   the constants
  */
final class ConstantPool private (val index: Map[Constant, Int], val values: Array[UplcValue]) {

    /** Generates the code that reads `const`: `consts(3)`, with `consts` the array parameter of the
      * compiled function. The lookup runs at JIT-compile time.
      */
    def read(const: Constant, consts: Expr[Array[UplcValue]])(using Quotes): Expr[UplcValue] =
        '{ $consts(${ Expr(index(const)) }) }
}

object ConstantPool {

    /** Collects the constants of `term`. */
    def apply(term: Term): ConstantPool = {
        val index = mutable.LinkedHashMap.empty[Constant, Int]
        // Pre-order, so a constant's index is the order of its first occurrence.
        term.foreach {
            case Term.Const(const, _) =>
                if !index.contains(const) then index(const) = index.size
            case _ => ()
        }
        // an `Object[]`, the erasure of `Array[UplcValue]`; filled by an iterator, not
        // `keys.map`: `keys` is a set, which a map could merge
        val values = new Array[AnyRef](index.size).asInstanceOf[Array[UplcValue]]
        index.keysIterator.copyToArray(values)
        new ConstantPool(index.toMap, values)
    }

    /** Compiles the code generated for `term` and applies it to the constants of `pool`.
      *
      * `code` generates a function of the constant array: `(consts: Array[UplcValue]) => (logger,
      * budget, params) => ...`. It is compiled once, then applied once, here, so the compiled
      * script holds the array and every evaluation reads from it.
      */
    def compile[T](term: Term, pool: ConstantPool)(
        code: Quotes ?=> Expr[Array[UplcValue] => T]
    ): T =
        StagingCompilerThread.run(term)(code)(pool.values)
}
