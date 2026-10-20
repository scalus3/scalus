package scalus.uplc.jit

import scalus.uplc.{Constant, Term}

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
        def collect(t: Term): Unit = t match
            case Term.Const(const, _) =>
                if !index.contains(const) then index(const) = index.size
            case Term.LamAbs(_, body, _)  => collect(body)
            case Term.Apply(f, arg, _)    => collect(f); collect(arg)
            case Term.Force(body, _)      => collect(body)
            case Term.Delay(body, _)      => collect(body)
            case Term.Constr(_, args, _)  => args.foreach(collect)
            case Term.Case(arg, cases, _) => collect(arg); cases.foreach(collect)
            case _                        => ()
        collect(term)
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
