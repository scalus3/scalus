package scalus.compiler.intrinsics

import scalus.compiler.sir.SIRType
import scalus.compiler.sir.lowering.*

/** Repr rules for native UplcConstr Option intrinsics (IntrinsicsUplcConstrOption).
  *
  * Mirrors `UplcConstrListReprRules` — each rule returns the output representation given the call
  * site's concrete output type and the `self` argument's lowered representation.
  */
object UplcConstrOptionReprRules {

    /** Element repr lookup: when `self` has `SumUplcConstr` Option, the `Some` variant's single
      * field is at index 0 of `ProdUplcConstr(0, fieldReprs)`. For `get` / `getOrElse`, we return
      * that field repr; fall back to the output type's default if the shape doesn't match.
      */
    private def someFieldRepr(
        inRepr: LoweredValueRepresentation,
        outTp: SIRType,
        lctx: LoweringContext
    ): LoweredValueRepresentation = {
        given LoweringContext = lctx
        inRepr match
            case sul: SumCaseClassRepresentation.SumUplcConstr =>
                sul.variants.get(0) match
                    case Some(puc: ProductCaseClassRepresentation.ProdUplcConstr)
                        if puc.fieldReprs.nonEmpty =>
                        puc.fieldReprs.head
                    case _ => lctx.typeGenerator(outTp).defaultRepresentation(outTp)
            case _ => lctx.typeGenerator(outTp).defaultRepresentation(outTp)
    }

    /** For operations returning a scalar (Boolean, etc.): use the output type's default repr. */
    val scalarRule: ReprRule = (outTp, _, lctx) =>
        given LoweringContext = lctx
        lctx.typeGenerator(outTp).defaultRepresentation(outTp)

    /** For `get`: Option[A] -> A. Returns the Some's stored field repr. */
    val getRule: ReprRule = (outTp, inRepr, lctx) => someFieldRepr(inRepr, outTp, lctx)

    /** For `getOrElse`: Option[A] -> A -> A. Produces LambdaRepresentation with element repr. */
    val getOrElseRule: ReprRule = (outTp, inRepr, lctx) => {
        given LoweringContext = lctx
        outTp match
            case SIRType.TypeLambda(_, body) => getOrElseRule(body, inRepr, lctx)
            case SIRType.Fun(argTp, retTp) =>
                val elemRepr = someFieldRepr(inRepr, retTp, lctx)
                LambdaRepresentation(
                  outTp,
                  InOutRepresentationPair(elemRepr, elemRepr)
                )
            case _ => someFieldRepr(inRepr, outTp, lctx)
    }

    val rules: Map[String, ReprRule] = Map(
      "isDefined" -> scalarRule,
      "isEmpty" -> scalarRule,
      "nonEmpty" -> scalarRule,
      "get" -> getRule,
      "getOrElse" -> getOrElseRule
      // map, flatMap, filter, exists, forall: no repr rule — body's Transparent TypeVar repr
      // flows through; the resolver's reprFun resolves TypeVars at the call site.
    )
}
