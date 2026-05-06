package scalus.compiler.sir.lowering

import org.typelevel.paiges.Doc
import scalus.compiler.sir.*
import scalus.uplc.*
import scala.util.control.NonFatal

abstract class BaseRepresentationProxyLoweredValue(
    input: LoweredValue,
    override val representation: LoweredValueRepresentation,
    override val pos: SIRPosition
) extends ProxyLoweredValue(input) {

    override def sirType: SIRType = input.sirType

    override def docRef(ctx: LoweredValue.PrettyPrintingContext): Doc = {
        docDef(ctx)
    }
}

/** A proxy which change only the representation of the input value (without changing the underlying
  * generated code)
  */
final class RepresentationProxyLoweredValue(
    val input: LoweredValue,
    override val representation: LoweredValueRepresentation,
    override val pos: SIRPosition
) extends BaseRepresentationProxyLoweredValue(input, representation, pos) {

    if input.representation == PrimitiveRepresentation.Constant && representation == PrimitiveRepresentation.PackedData
        && !SIRType.Data.unapply(input.sirType)
    then
        throw LoweringException(
          s"invalid conversion of constant value ${input.show} of type ${input.sirType.show} to packed data representation",
          pos
        )

    override def termInternal(gctx: TermGenerationContext): Term =
        input.termInternal(gctx)

    override def docDef(ctx: LoweredValue.PrettyPrintingContext): Doc = {
        val left = Doc.text("repr.proxy") + Doc.text("(")
        val right = PrettyPrinter.inBrackets(
          representation.doc
        ) + Doc.text(")")
        input.docRef(ctx).bracketBy(left, right)
    }

    override def toRepresentation(representation: LoweredValueRepresentation, pos: SIRPosition)(
        using LoweringContext
    ): LoweredValue = {
        if input.representation == representation then input
        else super.toRepresentation(representation, pos)
    }

}

object RepresentationProxyLoweredValue {

    def apply(
        input: LoweredValue,
        representation: LoweredValueRepresentation,
        pos: SIRPosition
    ): RepresentationProxyLoweredValue = {
        input match
            case proxy: RepresentationProxyLoweredValue =>
                if proxy.representation == representation then
                    proxy // no need to create a new proxy if the representation is the same
                else new RepresentationProxyLoweredValue(input, representation, pos)
            case _ =>
                new RepresentationProxyLoweredValue(input, representation, pos)
    }

}

/** Proxy whose `sirType` is `origin.sirType` with an `@UplcRepr` annotation derived from the
  * value's actual `representation`, attached at the return / scalar position. `representation` is
  * inherited from `origin`, so the annotation can never drift from the emitted bytes.
  */
final class RepresentationAnnotatedTypeProxyLoweredValue private (
    input: LoweredValue,
    annotated: SIRType,
    inPos: SIRPosition
) extends ProxyLoweredValue(input) {

    override def sirType: SIRType = annotated
    override def pos: SIRPosition = inPos
    override def termInternal(gctx: TermGenerationContext): Term = input.termInternal(gctx)

    override def docDef(ctx: LoweredValue.PrettyPrintingContext): Doc = {
        val left = Doc.text("repr.annotated.type.proxy") + Doc.text("(")
        val right = Doc.text(":") + Doc.text(sirType.show) + Doc.text(")")
        input.docRef(ctx).bracketBy(left, right)
    }
    override def docRef(ctx: LoweredValue.PrettyPrintingContext): Doc = docDef(ctx)
}

object RepresentationAnnotatedTypeProxyLoweredValue {

    /** Wrap when the representation has a non-default surface `@UplcRepr` form; otherwise return
      * `input` verbatim (proxy would be a hot-path no-op).
      */
    def wrapIfAnnotated(input: LoweredValue, inPos: SIRPosition)(using
        LoweringContext
    ): LoweredValue = {
        val annotated = annotateAtReturn(input.sirType, input.representation, inPos)
        if annotated eq input.sirType then input
        else new RepresentationAnnotatedTypeProxyLoweredValue(input, annotated, inPos)
    }

    /** Attach an `@UplcRepr` annotation derived from `repr` at `tp`'s return / scalar position.
      * Returns `tp` unchanged when `repr` is the type's natural default, has no surface form, or
      * computing the type's default fails (e.g. unresolved typeGenerator).
      */
    def annotateAtReturn(
        tp: SIRType,
        repr: LoweredValueRepresentation,
        pos: SIRPosition
    )(using LoweringContext): SIRType = annotationForRepresentation(tp, repr, pos) match
        case Some(anns) => IntrinsicResolver.propagateReturnAnnotation(tp, anns)
        case None       => tp

    private def annotationForRepresentation(
        tp: SIRType,
        repr: LoweredValueRepresentation,
        pos: SIRPosition
    )(using lctx: LoweringContext): Option[scalus.compiler.sir.AnnotationsDecl] = {
        // Skip the type-default fast path for reprs that obviously have no surface form.
        // Avoids running `defaultRepresentation` (the expensive part) on the common cases
        // where the encoder would return None anyway.
        encodeReprAsAnnotationValue(tp, repr, pos).flatMap { sir =>
            // No override needed when repr already matches the type's natural default.
            val typeDefault =
                try Some(lctx.typeGenerator(tp).defaultRepresentation(tp)(using lctx))
                catch case NonFatal(_) => None
            if typeDefault.contains(repr) then None
            else
                Some(
                  scalus.compiler.sir.AnnotationsDecl(pos, data = Map("uplcRepr" -> sir))
                )
        }
    }

    /** Encode `repr` as the SIR value placed under the `"uplcRepr"` annotation key, mirroring
      * `SIRTyper.encodeReprTag`'s output so `SirTypeUplcGenerator.resolveReprAnnotation` reads it
      * back to the same repr. Returns `None` for reprs without a surface form — their full detail
      * lives on the lowered value's `representation` field, where downstream consumers read it
      * directly.
      */
    private def encodeReprAsAnnotationValue(
        tp: SIRType,
        repr: LoweredValueRepresentation,
        pos: SIRPosition
    )(using LoweringContext): Option[SIR] = {
        def stringSentinel(name: String): SIR =
            SIR.Const(
              scalus.uplc.Constant.String(name),
              SIRType.String,
              scalus.compiler.sir.AnnotationsDecl(pos)
            )

        def constrSir(shortName: String, args: scala.List[SIR]): SIR = {
            val constrDecl = scalus.compiler.sir.ConstrDecl(
              shortName,
              args.map(_ => scalus.compiler.sir.TypeBinding("arg", SIRType.FreeUnificator)),
              Nil,
              Nil,
              scalus.compiler.sir.AnnotationsDecl.emptyModule
            )
            val dataDecl = scalus.compiler.sir.DataDecl(
              shortName,
              scala.List(constrDecl),
              Nil,
              scalus.compiler.sir.AnnotationsDecl.emptyModule
            )
            SIR.Constr(
              shortName,
              dataDecl,
              args,
              SIRType.SumCaseClass(dataDecl, Nil),
              scalus.compiler.sir.AnnotationsDecl(pos)
            )
        }

        repr match
            case _: SumCaseClassRepresentation.SumUplcConstr =>
                Some(stringSentinel("UplcConstr"))
            case SumCaseClassRepresentation.DataConstr =>
                Some(stringSentinel("DataConstr"))
            case SumCaseClassRepresentation.DataData =>
                Some(stringSentinel("DataData"))
            case SumCaseClassRepresentation.PackedSumDataList =>
                Some(stringSentinel("PackedSumDataList"))
            case PrimitiveRepresentation.Constant =>
                Some(stringSentinel("Constant"))
            case PrimitiveRepresentation.PackedData =>
                Some(stringSentinel("PackedData"))
            case SumCaseClassRepresentation.SumBuiltinList(elemRepr) =>
                val elemTp = SumCaseClassRepresentation.SumBuiltinList
                    .retrieveListElementType(tp)
                    .getOrElse(SIRType.Data.tp)
                encodeReprAsAnnotationValue(elemTp, elemRepr, pos).map { argSir =>
                    constrSir("SumBuiltinList", scala.List(argSir))
                }
            case ProductCaseClassRepresentation.ProdBuiltinPair(fstRepr, sndRepr) =>
                val (fstTp, sndTp) =
                    ProductCaseClassRepresentation.ProdBuiltinPair
                        .extractPairComponentTypes(tp)
                for {
                    fstSir <- encodeReprAsAnnotationValue(fstTp, fstRepr, pos)
                    sndSir <- encodeReprAsAnnotationValue(sndTp, sndRepr, pos)
                } yield constrSir("ProdBuiltinPair", scala.List(fstSir, sndSir))
            // No surface annotation form for the rest — full detail lives on the
            // `representation` field; absence here is correct, not lossy.
            case _ => None
    }
}

/** Proxy wrapping the value's `sirType` with an outer `TypeLambda` binding free TypeVars not bound
  * by any inner `TypeLambda`. When the underlying value is a Lambda, its
  * `LambdaRepresentation.funTp` is rewrapped consistently so `representation.funTp == sirType`.
  * Used when an inlined intrinsic body has dropped its outer `TypeLambda` and downstream
  * `lvApply.reprFun` needs the bound TypeVars to resolve element reprs.
  */
final class OuterTypeLambdaWrappedLoweredValue private (
    input: LoweredValue,
    freeVars: scala.List[SIRType.TypeVar],
    inPos: SIRPosition
) extends ProxyLoweredValue(input) {

    override val sirType: SIRType = SIRType.TypeLambda(freeVars, input.sirType)

    override val representation: LoweredValueRepresentation =
        input.representation match
            case lr: LambdaRepresentation =>
                LambdaRepresentation(sirType, lr.canonicalRepresentationPair)
            case other => other

    override def pos: SIRPosition = inPos
    override def termInternal(gctx: TermGenerationContext): Term = input.termInternal(gctx)

    override def docDef(ctx: LoweredValue.PrettyPrintingContext): Doc = {
        val left = Doc.text("outer.tlam.proxy") + Doc.text("(")
        val right = Doc.text(":") + Doc.text(sirType.show) + Doc.text(")")
        input.docRef(ctx).bracketBy(left, right)
    }
    override def docRef(ctx: LoweredValue.PrettyPrintingContext): Doc = docDef(ctx)
}

object OuterTypeLambdaWrappedLoweredValue {

    /** Wrap when `input.sirType` has free TypeVars; otherwise return `input` verbatim. */
    def wrapIfNeeded(input: LoweredValue, inPos: SIRPosition): LoweredValue = {
        val frees = collectFreeTypeVars(input.sirType)
        if frees.isEmpty then input
        else new OuterTypeLambdaWrappedLoweredValue(input, frees, inPos)
    }

    /** Collect TypeVars referenced in `tp` that are NOT bound by any inner `TypeLambda`. */
    def collectFreeTypeVars(tp: SIRType): scala.List[SIRType.TypeVar] = {
        val seen = new java.util.IdentityHashMap[SIRType.TypeProxy, Boolean]()
        val acc = scala.collection.mutable.LinkedHashSet[SIRType.TypeVar]()
        def go(t: SIRType, bound: Set[SIRType.TypeVar]): Unit = t match
            case tv: SIRType.TypeVar =>
                if !bound.contains(tv) then acc += tv
            case SIRType.TypeLambda(ps, body) =>
                go(body, bound ++ ps)
            case SIRType.Fun(in, out) =>
                go(in, bound); go(out, bound)
            case SIRType.CaseClass(_, args, parent) =>
                args.foreach(go(_, bound))
                parent.foreach(go(_, bound))
            case SIRType.SumCaseClass(_, args) =>
                args.foreach(go(_, bound))
            case SIRType.Annotated(t1, _) => go(t1, bound)
            case SIRType.TypeProxy(ref) =>
                if ref != null && !seen.containsKey(t) then {
                    seen.put(t.asInstanceOf[SIRType.TypeProxy], true)
                    go(ref, bound)
                }
            case _ => // primitives, FreeUnificator, TypeNothing
        go(tp, Set.empty)
        acc.toList
    }
}

/** A proxy which changes the input value to be specific type and representation.
  */
final class TypeRepresentationProxyLoweredValue(
    input: LoweredValue,
    inSirType: SIRType,
    override val representation: LoweredValueRepresentation,
    inPos: SIRPosition
) extends ProxyLoweredValue(input) {

    override def sirType: SIRType = inSirType

    override def pos: SIRPosition = inPos

    override def termInternal(gctx: TermGenerationContext): Term =
        input.termInternal(gctx)

    override def docDef(ctx: LoweredValue.PrettyPrintingContext): Doc = {
        val left = Doc.text("cast.repr.proxy") + Doc.text("(")
        val right = Doc.text(":") + Doc.text(sirType.show) + PrettyPrinter.inBrackets(
          representation.doc
        ) + Doc.text(")")
        input.docRef(ctx).bracketBy(left, right)
    }

    override def docRef(ctx: LoweredValue.PrettyPrintingContext): Doc = {
        docDef(ctx)
    }

}
