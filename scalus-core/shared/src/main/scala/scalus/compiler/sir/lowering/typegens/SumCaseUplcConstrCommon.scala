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
