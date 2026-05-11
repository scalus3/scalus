package scalus.compiler.sir.lowering
package typegens

import org.typelevel.paiges.Doc
import scalus.compiler.sir.lowering.ProductCaseClassRepresentation.*
import scalus.compiler.sir.*
import scalus.uplc.{Term, UplcAnnotation}

/** Emitter for `ProductCaseClassRepresentation.ProdUplcConstr` — native UPLC
  * `Constr(tag, [t1, t2, ...])` emission for product values.
  *
  * Owns:
  *   - `genConstr`: adopts each argument to its target field representation (canonical
  *     LambdaRepresentation for function fields; the `@UplcRepr`-annotated repr for annotated
  *     fields; the field type's natural repr when the type itself carries a class-level
  *     `@UplcRepr`), then assembles a `Term.Constr`-shaped `LoweredValue` tagged with the
  *     constructor's index.
  *   - `hasClassLevelUplcRepr` (private helper): unwraps `Annotated` / `TypeLambda` / `TypeProxy`
  *     and returns true when the underlying `(CaseClass | SumCaseClass).decl.annotations` carry the
  *     `uplcRepr` key.
  *
  * Called from:
  *   - `ProductCaseEmitter.genConstrLowered` (Data-vs-UplcConstr dispatcher);
  *   - `ProductCaseUplcConstrOnlyEmitter`, `ProductCaseUplcConstrEmitter`,
  *     `SumCaseUplcConstrEmitter`, `SumCaseUplcConstrOnlyEmitter` — all with the same
  *     `(constr, loweredArgs)` signature.
  *
  * Constructor-handling helpers (`retrieveConstrIndex`, `retrieveConstrDecl`) still live on
  * `ProductCaseEmitter`; called cross-class from here.
  */
object ProdUplcConstrOps {

    def genConstr(
        constr: SIR.Constr,
        loweredArgs: scala.List[LoweredValue]
    )(using
        lctx: LoweringContext
    ): LoweredValue = {
        val constrIndex =
            ProductCaseEmitter.retrieveConstrIndex(constr.tp, constr.anns.pos)
        val constrDecl =
            ProductCaseEmitter.retrieveConstrDecl(constr.tp, constr.anns.pos)

        // Adopt fields: convert each argument to the target field representation.
        // For function fields: canonical LambdaRepresentation.
        // For fields with @UplcRepr annotation: the annotated representation.
        // For other fields: when the field's static type has a class-level
        //   @UplcRepr annotation that pins it (e.g., `board: ChessSet` where
        //   `ChessSet` is `@UplcRepr(UplcConstr)`), force conversion to that
        //   pinned repr. Without this, an arg in Data form would be silently
        //   embedded as the field value with the arg's Data static repr —
        //   downstream projections would then produce Data bytes where UC was
        //   expected, surfacing as native-UC selectors applied to Data.Constr
        //   scrutinees at runtime. For TypeVar fields, keep arg as-is (we
        //   cannot compute a concrete target repr without the substitution).
        val adoptedArgs = loweredArgs.zip(constrDecl.params).map { (arg, param) =>
            if SIRType.isPolyFunOrFun(param.tp) then
                val canonicalRepr = FunSirTypeGenerator.defaultRepresentation(param.tp)
                arg.toRepresentation(canonicalRepr, constr.anns.pos)
            else
                val paramType = lctx.resolveTypeVarIfNeeded(param.tp)
                SirTypeUplcGenerator.resolveFieldRepr(param, paramType) match
                    case Some(targetRepr) =>
                        arg.maybeUpcast(paramType, constr.anns.pos)
                            .toRepresentation(targetRepr, constr.anns.pos)
                    case None =>
                        // Pin to the field type's natural repr only when the
                        // type itself has a class-level @UplcRepr annotation
                        // (so the type carries a binding intent). For
                        // unannotated types, keep the arg's actual repr — that
                        // preserves existing behavior for plain product fields.
                        if hasClassLevelUplcRepr(paramType) then
                            val targetRepr =
                                SirTypeUplcGenerator.defaultRepresentation(paramType)
                            arg.maybeUpcast(paramType, constr.anns.pos)
                                .toRepresentation(targetRepr, constr.anns.pos)
                        else arg
        }
        val fieldReprs = adoptedArgs.map(_.representation).toList
        val repr = ProdUplcConstr(constrIndex, fieldReprs)

        // Build Term.Constr(tag, [t1, t2, ...])
        val loweredValue = new ComplexLoweredValue(Set.empty, adoptedArgs*) {
            override def sirType: SIRType = constr.tp
            override def representation: LoweredValueRepresentation = repr
            override def pos: SIRPosition = constr.anns.pos

            override def termInternal(gctx: TermGenerationContext): Term = {
                Term.Constr(
                  scalus.cardano.ledger.Word64(constrIndex.toLong),
                  adoptedArgs.map(_.termWithNeededVars(gctx)).toList,
                  UplcAnnotation(constr.anns.pos)
                )
            }

            override def docDef(ctx: LoweredValue.PrettyPrintingContext): Doc = {
                val left = Doc.text(s"UplcConstr($constrIndex, ")
                val right = Doc.text(")")
                val args = adoptedArgs.map(_.docRef(ctx))
                Doc.intercalate(Doc.comma + Doc.space, args).bracketBy(left, right)
            }

            override def docRef(ctx: LoweredValue.PrettyPrintingContext): Doc = docDef(ctx)
        }

        loweredValue
    }

    /** Returns true if `tp` (or any unwrapped form) carries a class-level `@UplcRepr` annotation in
      * its constrDecl. Used by `genConstr` to decide whether the field's static type pins a
      * specific repr that arg.repr must conform to.
      */
    private def hasClassLevelUplcRepr(tp: SIRType): Boolean = tp match
        case SIRType.CaseClass(decl, _, _)         => decl.annotations.data.contains("uplcRepr")
        case SIRType.SumCaseClass(decl, _)         => decl.annotations.data.contains("uplcRepr")
        case SIRType.Annotated(inner, _)           => hasClassLevelUplcRepr(inner)
        case SIRType.TypeLambda(_, body)           => hasClassLevelUplcRepr(body)
        case SIRType.TypeProxy(ref) if ref != null => hasClassLevelUplcRepr(ref)
        case _                                     => false

}
