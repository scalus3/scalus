package scalus.compiler.sir.lowering

import scalus.compiler.sir.SIRPosition

/** A step in the (sourceRepr, targetRepr) conversion graph. Each emitter that declares its outbound
  * conversions via `outboundStep(target)` returns a step describing how to convert from the
  * emitter's owned source repr to the given target.
  *
  * See `docs/local/claude/compiler/sum-prod-dispatch-design.md` §3.3 / Phase 5.
  */
sealed trait ConversionStep

object ConversionStep {

    /** `target == source.representation` — return the input unchanged. */
    case object Identity extends ConversionStep

    /** Direct one-hop conversion. The `emit` runs against the source value to produce the converted
      * `LoweredValue` in the target repr.
      */
    final case class Atomic(emit: AtomicEmit) extends ConversionStep

    /** Two-hop conversion via an intermediate repr: source → intermediate → target. The dispatcher
      * recurses through `LoweredValue.toRepresentation`.
      */
    final case class Via(intermediate: LoweredValueRepresentation) extends ConversionStep

    /** SAM trait carrying the `using LoweringContext` requirement. Lets call sites write
      * `Atomic((input, target, pos) => ...)`-style lambdas while preserving the implicit-context
      * plumbing.
      */
    trait AtomicEmit {
        def emit(
            input: LoweredValue,
            target: LoweredValueRepresentation,
            pos: SIRPosition
        )(using LoweringContext): LoweredValue
    }

    /** Apply a step to obtain the converted value. */
    def apply(
        step: ConversionStep,
        input: LoweredValue,
        target: LoweredValueRepresentation,
        pos: SIRPosition
    )(using LoweringContext): LoweredValue =
        step match {
            case Identity     => input
            case Atomic(emit) => emit.emit(input, target, pos)
            case Via(intermediate) =>
                input.toRepresentation(intermediate, pos).toRepresentation(target, pos)
        }

}
