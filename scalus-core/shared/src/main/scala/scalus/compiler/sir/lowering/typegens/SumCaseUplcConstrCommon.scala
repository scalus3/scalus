package scalus.compiler.sir.lowering
package typegens

import scalus.compiler.sir.*

/** Shared base for the two `@UplcRepr(UplcConstr)`-driven sum-case generators:
  * [[SumCaseUplcConstrEmitter]] (Data-compatible) and [[SumCaseUplcConstrOnlyEmitter]]
  * (Fun-bearing, no Data side).
  *
  * Both lower to `SumUplcConstr` by default and both promote a `ProdUplcConstr` value into a
  * single-entry `SumUplcConstr` parent in `upcastOne`. Subclasses differ on Data-side surface
  * (`defaultDataRepresentation`, `defaultTypeVarReperesentation`, `canBeConvertedToData`),
  * constructor/select/match emission, and outbound conversion (`emitConvert`).
  */
trait SumCaseUplcConstrCommon extends SirTypeUplcGenerator {

    override def defaultRepresentation(tp: SIRType)(using
        LoweringContext
    ): LoweredValueRepresentation =
        SumUplcConstrOps.buildSumUplcConstr(tp)

    /** `constr` with a CaseClass `tp`, as `ProdUplcConstrOps.genConstr` requires.
      *
      * A nullary enum case (`case Skip`) and a constructor reached through `dispatchNil` carry the
      * *sum* type as `constr.tp` (possibly `Annotated`, with caller-supplied substituted args).
      * Rebuild the CaseClass form for `constr.name` with that sum preserved as its parent, rather
      * than dropping it via the static `decl.constrType(name)` lookup (which would substitute back
      * to the abstract decl typevars).
      */
    protected def withCaseClassTp(constr: SIR.Constr): SIR.Constr = {
        val effectiveTp =
            if SIRType.isProd(constr.tp) then constr.tp
            else if SIRType.isSum(constr.tp) then
                preservedParentCaseClassForm(constr.tp, constr.data, constr.name)
            else constr.data.constrType(constr.name)
        constr.copy(tp = effectiveTp)
    }

    /** Build a CaseClass form for `ctorName` using `parent` as its parent field, preserving any
      * `Annotated`/substituted args on `parent`. The constructor's own shape (typeParams, typeArgs)
      * comes from `decl.constrType(ctorName)`; only the parent reference is swapped.
      */
    private def preservedParentCaseClassForm(
        parent: SIRType,
        decl: scalus.compiler.sir.DataDecl,
        ctorName: String
    ): SIRType = decl.constrType(ctorName) match
        case SIRType.TypeLambda(params, SIRType.CaseClass(c, args, _)) =>
            SIRType.TypeLambda(params, SIRType.CaseClass(c, args, Some(parent)))
        case SIRType.CaseClass(c, args, _) =>
            SIRType.CaseClass(c, args, Some(parent))
        case other => other

    /** Template method: `ProdUplcConstr` is always lifted into a single-entry `SumUplcConstr`
      * parent; everything else is delegated to `upcastOneOther`.
      *
      * Single-entry (no overlay on `buildSumUplcConstr`) is load-bearing: downstream
      * `genMatchUplcConstr.hasTransparentFields` walks `variants.values`, so adding type-derived
      * default variants (whose fields carry Transparent TypeVar reprs from the DataDecl, e.g.
      * `List.Cons.head`) would fire the transparent-branch override even for concrete-shape inputs
      * like `Nil`.
      */
    override def upcastOne(input: LoweredValue, targetType: SIRType, pos: SIRPosition)(using
        lctx: LoweringContext
    ): LoweredValue = input.representation match
        case prod: ProductCaseClassRepresentation.ProdUplcConstr =>
            val sumRepr = SumCaseClassRepresentation.SumUplcConstr(
              scala.collection.immutable.SortedMap(prod.tag -> prod)
            )
            TypeRepresentationProxyLoweredValue(input, targetType, sumRepr, pos)
        case _ => upcastOneOther(input, targetType, pos)

    /** Subclass hook: handle representations other than `ProdUplcConstr` in `upcastOne`. */
    protected def upcastOneOther(input: LoweredValue, targetType: SIRType, pos: SIRPosition)(using
        lctx: LoweringContext
    ): LoweredValue

}
