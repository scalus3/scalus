package scalus.compiler.sir.lowering

import org.typelevel.paiges.Doc
import scalus.compiler.sir.*
import scalus.uplc.*

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

/** A proxy which changes the input value to be specific type and representation.
  */
final class TypeRepresentationProxyLoweredValue(
    input: LoweredValue,
    inSirType: SIRType,
    override val representation: LoweredValueRepresentation,
    inPos: SIRPosition
) extends ProxyLoweredValue(input) {

    // DEBUG: catch type/repr mismatch — DataData is only valid for Data-typed elements
    representation match
        case SumCaseClassRepresentation.SumBuiltinList(SumCaseClassRepresentation.DataData) =>
            SumCaseClassRepresentation.SumBuiltinList.retrieveListElementType(inSirType) match
                case Some(elemType)
                    if elemType != SIRType.Data.tp
                        && !elemType.isInstanceOf[SIRType.TypeVar]
                        && elemType != SIRType.FreeUnificator
                        && elemType != SIRType.TypeNothing
                        && (elemType == SIRType.Integer
                            || elemType == SIRType.ByteString
                            || elemType == SIRType.String
                            || elemType == SIRType.Boolean) =>
                    throw new RuntimeException(
                      s"TypeRepresentationProxy: primitive element type ${elemType.show} with DataData repr. Should be PackedData. type=${inSirType.show}, inputRepr=${input.representation}"
                    )
                case _ => ()
        case _ => ()

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
