package scalus.uplc.jit

import scalus.uplc.Constant
import scalus.uplc.eval.{BudgetSpender, Logger, MachineParams}

/** The value of a UPLC term in JIT-compiled code. It mirrors `CekValue` case by case:
  *
  * | `UplcValue`   | `CekValue`                                                 |
  * |:--------------|:-----------------------------------------------------------|
  * | `Constant`    | `VCon`                                                     |
  * | `ConstrValue` | `VConstr`                                                  |
  * | `LamValue`    | `VLamAbs`, or a `VBuiltin` that takes a term argument next |
  * | `DelayValue`  | `VDelay`, or a `VBuiltin` that takes a force next          |
  *
  * A constant is the `scalus.uplc.Constant` the CEK machine holds in `VCon`, so it keeps its type:
  * a list constant knows its element type also when it is empty, and `mkCons` checks it as the CEK
  * machine does. `ConstrValue` exists only because its fields can hold JIT functions, which no
  * `CekValue` can hold. The erasure of the union is `Object`, so generated code passes values
  * without casts.
  */
type UplcValue = Constant | ConstrValue | LamValue | DelayValue

/** The value of `(constr tag a1 .. an)`, as `CekValue.VConstr`. `tag` is the `Word64` of the term,
  * held in a signed `Long`.
  */
final case class ConstrValue(tag: Long, args: List[UplcValue])

/** A function value: a compiled `(lam x body)`, or a partly applied builtin that takes a term
  * argument next. A lambda of generated code compiles to this interface through
  * `LambdaMetafactory`, with no class of its own.
  */
@FunctionalInterface
trait LamValue {
    def apply(arg: UplcValue): UplcValue
}

/** A delayed value: a compiled `(delay body)`, or a partly applied builtin that takes a force next.
  */
@FunctionalInterface
trait DelayValue {
    def apply(): UplcValue
}

/** A partly applied builtin that takes a term argument next: what the CEK machine holds as a
  * `VBuiltin`. A [[LamValue]] for applying it; the marker lets [[JitRuntime.cannotForce]] fail with
  * the CEK machine's error for forcing a builtin that expects a term argument.
  */
@FunctionalInterface
trait BuiltinLamValue extends LamValue

/** A partly applied builtin that takes a force next: what the CEK machine holds as a `VBuiltin`. A
  * [[DelayValue]] for forcing it; the marker lets [[JitRuntime.cannotApply]] fail with the CEK
  * machine's error for applying a builtin that expects a force.
  */
@FunctionalInterface
trait BuiltinDelayValue extends DelayValue

/** A JIT-compiled term: evaluates the term with the given logger, budget spender and machine
  * parameters.
  *
  * The code implements the machine of one protocol version, chosen when it was compiled
  * (`JIT.jitUplc`, `UplcCompiler.compile`): code for van Rossem (PV11) rejects `case` on `Data`,
  * code for Dijkstra (PV12) cases on it. Pass the machine parameters of a `PlutusVM` of that
  * version; `UplcCompiler.Compiled.evaluate` checks the version, a direct call does not.
  *
  * One `CompiledTerm` is safe to call from many threads at once: all state of an evaluation, the
  * call-depth counter included, is local to the call. It throws
  * [[nativestack.StackThresholdException]] when the call depth reaches its limit, and never falls
  * back to the CEK machine itself; `UplcCompiler.Compiled.withFallback` does.
  */
@FunctionalInterface
trait CompiledTerm {
    def apply(logger: Logger, budget: BudgetSpender, params: MachineParams): UplcValue
}
