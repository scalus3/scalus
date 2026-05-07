package scalus.compiler.sir.lowering
package typegens

import scalus.compiler.sir.*

/** Cross-cutting helper for `TypeVarRepresentation` source values (Phase 5,
  * design §3.6). A value carrying `TypeVarRepresentation(kind)` has bytes in
  * *some* concrete repr's form; which form depends on `kind` and the value's
  * underlying SIR type:
  *
  *   - `Transparent` — passthrough; bytes are in whatever form the source
  *     produced. Conversion to a concrete target relabels the value as the
  *     target repr.
  *   - `Unwrapped` — bytes are in `tp.defaultRepresentation` form. Conversion
  *     relabels as that underlying repr first, then delegates back to the
  *     dispatcher to convert to the target.
  *   - `Fixed` — bytes are in `tp.defaultTypeVarReperesentation` form (the
  *     type's natural Data-shaped form). Conversion converts to that
  *     underlying repr first, then routes to the target.
  *
  * `bridgeFromKind` is shared by callers on the Sum side
  * (`SumDispatch.sumCaseImpl`, `SumListEmitterCommon.emitConvert`,
  * `SumUplcConstrEmitter.emitConvert`); each site previously open-coded the
  * same kind dispatch with slightly different hardcoded underlying reprs that
  * already aligned with `lctx.typeGenerator(input.sirType).default*`.
  */
object TypeVarEmitter {

    /** Convert a TypeVar-labeled value to a concrete `target` repr. The
      * underlying-repr lookups go through the input's typegen so the helper
      * works for `DataConstrEmitter`, `SumListEmitterCommon`, etc.
      */
    def bridgeFromKind(
        input: LoweredValue,
        tvr: TypeVarRepresentation,
        target: LoweredValueRepresentation,
        pos: SIRPosition
    )(using lctx: LoweringContext): LoweredValue = {
        import SIRType.TypeVarKind.*
        tvr.kind match
            case Transparent =>
                if input.representation == target then input
                else new RepresentationProxyLoweredValue(input, target, pos)
            case Unwrapped =>
                val typeGen = lctx.typeGenerator(input.sirType)
                val sourceUnderlying = typeGen.defaultRepresentation(input.sirType)
                sourceUnderlying match
                    case _: TypeVarRepresentation =>
                        throw LoweringException(
                          s"Cannot convert unresolved Unwrapped TypeVar to $target " +
                              s"for ${input.sirType.show}",
                          pos
                        )
                    case _ =>
                        val r0 = new RepresentationProxyLoweredValue(input, sourceUnderlying, pos)
                        r0.toRepresentation(target, pos)
            case Fixed =>
                val typeGen = lctx.typeGenerator(input.sirType)
                val fixedUnderlying = typeGen.defaultTypeVarReperesentation(input.sirType)
                fixedUnderlying match
                    case _: TypeVarRepresentation =>
                        throw LoweringException(
                          s"Cannot convert unresolved Fixed TypeVar to $target for ${input.sirType.show}. " +
                              s"TypeVar repr=$tvr, defaultTypeVarRepr=$fixedUnderlying",
                          pos
                        )
                    case _ =>
                        if input.representation == target then input
                        else
                            val r0 = input.toRepresentation(fixedUnderlying, pos)
                            r0.toRepresentation(target, pos)
    }

}
