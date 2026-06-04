package scalus.compiler

import scalus.compiler.sir.SIR

import scala.quoted.*

/** Home of the `compile` / `compileWithOptions` macro emission — and, deliberately, the source
  * position that every compiled program inherits at its outermost node.
  *
  * Why this lives in its own file, with this name:
  *
  *   - `PlutusVx.compile(code)` expands (via `compileInline*`) into the `compileWithOptions(...)`
  *     call generated below. That call is the *outermost* node of the whole program, so its source
  *     position becomes the program's root position. The Scala→SIR→UPLC pipeline strips source
  *     positions from most of the generated `Apply`/`Case`/`Constr` spine, and the
  *     post-optimization position-fill ([[scalus.uplc.Compiled.toUplc]]) back-fills those nodes
  *     from their nearest positioned ancestor — which, for framework glue with no user code inside
  *     it, is this root. So a large amount of compiler-generated glue used to be attributed to
  *     whatever line this macro happened to sit on (it was `Macros.scala:589`), turning that line
  *     into a fake profiling hot-spot and a call-graph hub.
  *   - Giving the macro its own file with a stable, descriptive name makes that attribution (a)
  *     self-documenting in profiles — it reads as `CompiledProgramRoot:N`, not a random
  *     `Macros:589` — and (b) immune to line drift when unrelated macros are edited.
  *   - Tooling treats positions in this file as *effectively empty*
  *     ([[scalus.utils.ScalusSourcePos.isEffectivelyEmpty]]), so the position-fill and the profiler
  *     attribute generated glue to the nearest *real* user code instead of pooling it here. Keep
  *     this file's basename in sync with [[scalus.utils.ScalusSourcePos.syntheticMarkerFile]].
  */
object CompiledProgramRoot {

    /** Emit the `compile` call for code compiled with the ambient/default options. */
    def generateCompileCall(code: Expr[Any])(using Quotes): Expr[SIR] = '{
        scalus.compiler.compile($code)
    }

    /** Emit the `compileWithOptions` call. This call is the outermost node of the compiled program;
      * see the file header for why its position is the synthetic program root.
      */
    def generateCompileCall(options: Expr[scalus.compiler.Options], code: Expr[Any])(using
        Quotes
    ): Expr[SIR] = '{
        scalus.compiler.compileWithOptions($options, $code)
    }
}
