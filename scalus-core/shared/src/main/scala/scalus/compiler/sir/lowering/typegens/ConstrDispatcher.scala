package scalus.compiler.sir.lowering
package typegens

import scalus.compiler.sir.*

/** Context-driven dispatch for `SIR.Constr` lowering. Encapsulates the cross-generator decisions
  * about whether a Cons/Some/etc. construction should emit native UplcConstr
  * (`SumCaseUplcConstrSirTypeGenerator`) instead of the type's purely-type-driven default
  * representation (chosen by `LoweringContext.typeGenerator`).
  *
  * Used by container generators (Product/Sum/SumListCommon) at the top of their `genConstrLowered`
  * to delegate when context indicates UplcConstr emission. Lowering.scala then doesn't need to
  * embed the dispatch chain — each generator handles its own delegation.
  *
  * Conditions checked, in order, all of which favor `SumCaseUplcConstrSirTypeGenerator`:
  *   1. Parent sum's default representation is `SumUplcConstr`.
  *   2. For List.Cons: tail argument's actual lowered representation is `SumUplcConstr` (handles
  *      inline-expanded `prepended()` chains).
  *   3. The caller's `optTargetType` is annotated `@UplcRepr(UplcConstr)`.
  *   4. `lctx.inUplcConstrListScope == true` AND the constr's parent sum is List/Option.
  *
  * If any condition matches, returns `Some(generator-to-delegate-to)`; otherwise `None` (caller
  * proceeds with its default logic).
  */
object ConstrDispatcher {

    def shouldDelegateToUplcConstr(
        constr: SIR.Constr,
        loweredArgs: scala.List[LoweredValue],
        optTargetType: Option[SIRType]
    )(using lctx: LoweringContext): Option[SirTypeUplcGenerator] = {
        val resolvedType = lctx.resolveTypeVarIfNeeded(constr.tp)

        // 1. Parent's default repr is SumUplcConstr.
        val parentTypeGen = resolvedType match
            case SIRType.CaseClass(_, typeArgs, Some(parent)) =>
                // Apply constructor's typeArgs to parent (which is captured at decl-site
                // as a TypeLambda parameterized by the sum's typeParams).
                val substitutedParent = parent match
                    case SIRType.TypeLambda(params, body) if params.length == typeArgs.length =>
                        SIRType.substitute(body, params.zip(typeArgs).toMap, Map.empty)
                    case _ => parent
                val parentGen = lctx.typeGenerator(substitutedParent)
                parentGen.defaultRepresentation(substitutedParent) match
                    case _: SumCaseClassRepresentation.SumUplcConstr => Some(parentGen)
                    case _                                           => None
            case _ => None
        if parentTypeGen.isDefined then return parentTypeGen

        // 2. List.Cons + tail has SumUplcConstr repr.
        val reprPropGen = resolvedType match
            case SIRType.CaseClass(cd, _, Some(_))
                if cd.name.endsWith("List$.Cons") && loweredArgs.length >= 2 =>
                loweredArgs.last.representation match
                    case _: SumCaseClassRepresentation.SumUplcConstr =>
                        Some(SumCaseUplcConstrSirTypeGenerator)
                    case _ => None
            case _ => None
        if reprPropGen.isDefined then return reprPropGen

        // 3. Caller's target type is @UplcRepr(UplcConstr) annotated.
        val targetUsesUplcConstr = optTargetType.exists {
            case SIRType.Annotated(inner, anns) =>
                anns.data.contains("uplcRepr") &&
                IntrinsicResolver.isUplcConstrListOrOption(inner)
            case _ => false
        }
        if targetUsesUplcConstr then return Some(SumCaseUplcConstrSirTypeGenerator)

        // 4. inUplcConstrListScope flag + this constr's parent sum is List/Option.
        val parentIsListOrOption = resolvedType match
            case SIRType.CaseClass(_, _, Some(parent)) =>
                IntrinsicResolver.isUplcConstrListOrOption(parent)
            case _ =>
                IntrinsicResolver.isUplcConstrListOrOption(resolvedType)
        if lctx.inUplcConstrListScope && parentIsListOrOption then
            return Some(SumCaseUplcConstrSirTypeGenerator)

        None
    }

    /** Pick the type generator for a `Nil` constructor with the lowering context's hints.
      * Equivalent to `lctx.typeGenerator(target-or-resolved-type)` plus the `inUplcConstrListScope`
      * override.
      *
      * Returns `(generator, effective-constr)` — `effective-constr` carries `targetType` as its
      * `tp` when an `optTargetType` is supplied (so the generator sees the right type info for
      * type-correct Nil emission).
      */
    def dispatchNil(
        constr: SIR.Constr,
        resolvedType: SIRType,
        optTargetType: Option[SIRType]
    )(using lctx: LoweringContext): (SirTypeUplcGenerator, SIR.Constr) = {
        optTargetType match
            case Some(targetType) =>
                (lctx.typeGenerator(targetType), constr.copy(tp = targetType))
            case None =>
                val gen =
                    if lctx.inUplcConstrListScope &&
                        IntrinsicResolver.isUplcConstrListOrOption(resolvedType)
                    then SumCaseUplcConstrSirTypeGenerator
                    else lctx.typeGenerator(resolvedType)
                (gen, constr)
    }

}
