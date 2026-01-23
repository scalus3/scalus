package scalus.compiler.sir.lowering
package typegens

import org.typelevel.paiges.Doc
import scalus.compiler.sir.lowering.LoweredValue.Builder.*
import scalus.compiler.sir.*
import scalus.uplc.Term

/** Representation is data in pack. Used like Product with have one element wibt name 'toList'
  */
object MapSirTypeGenerator extends SirTypeUplcGenerator {

    override def defaultRepresentation(tp: SIRType)(using
        LoweringContext
    ): LoweredValueRepresentation = {
        ProductCaseClassRepresentation.PackedDataMap
    }

    override def defaultDataRepresentation(tp: SIRType)(using
        LoweringContext
    ): LoweredValueRepresentation = {
        ProductCaseClassRepresentation.PackedDataMap
    }

    override def defaultTypeVarReperesentation(tp: SIRType)(using
        LoweringContext
    ): LoweredValueRepresentation = {
        ProductCaseClassRepresentation.PackedDataMap
    }

    override def isDataSupported(tp: SIRType)(using
        lctx: LoweringContext
    ): Boolean = {
        true
    }

    override def toRepresentation(
        input: LoweredValue,
        representation: LoweredValueRepresentation,
        pos: SIRPosition
    )(using
        lctx: LoweringContext
    ): LoweredValue = {
        // we have only one representation, so
        (input.representation, representation) match
            case (
                  ProductCaseClassRepresentation.PackedDataMap,
                  ProductCaseClassRepresentation.PackedDataMap
                ) =>
                input
            case (ProductCaseClassRepresentation.PackedDataMap, TypeVarRepresentation(_)) =>
                input
            case (TypeVarRepresentation(_), ProductCaseClassRepresentation.PackedDataMap) =>
                RepresentationProxyLoweredValue(
                  input,
                  ProductCaseClassRepresentation.PackedDataMap,
                  pos
                )
            case _ =>
                throw LoweringException(
                  s"invalid conversion of ${input.representation} to $representation",
                  pos
                )
    }

    override def upcastOne(
        input: LoweredValue,
        targetType: SIRType,
        pos: SIRPosition
    )(using
        LoweringContext
    ): LoweredValue = {
        throw LoweringException(
          s"invalid upcast of ${input.sirType} to $targetType",
          pos
        )
    }

    override def genConstr(constr: SIR.Constr)(using
        lctx: LoweringContext
    ): LoweredValue = {
        if constr.name == "scalus.cardano.onchain.plutus.prelude.AssocMap"
            ||
            constr.name == "scalus.cardano.onchain.plutus.prelude.SortedMap"
        then
            // TODO: add 'target type' to lower
            val loweredArg = lctx.lower(constr.args.head)
            val loweredArgR = loweredArg.toRepresentation(
              SumCaseClassRepresentation.SumDataPairList,
              constr.anns.pos
            )
            lvBuiltinApply(
              SIRBuiltins.mapData,
              loweredArgR,
              constr.tp,
              ProductCaseClassRepresentation.PackedDataMap,
              constr.anns.pos
            )
        else
            throw LoweringException(
              s"MapSirTypeGenerator.genConstr: invalid constructor ${constr.name}",
              constr.anns.pos
            )
    }

    override def genSelect(sel: SIR.Select, loweredScrutinee: LoweredValue)(using
        lctx: LoweringContext
    ): LoweredValue = {
        if sel.field == "toList" then
            lvBuiltinApply(
              SIRBuiltins.unMapData,
              loweredScrutinee,
              sel.tp,
              SumCaseClassRepresentation.SumDataPairList,
              sel.anns.pos
            )
        else
            throw LoweringException(
              s"MapSirTypeGenerator.genSelect: invalid select ${sel.field} (expected 'toList')",
              sel.anns.pos
            )
    }

    class MapMatchLoweredValue(
        bindedVar: IdentifiableLoweredValue,
        scrutinee: LoweredValue,
        body: LoweredValue,
        inPos: SIRPosition
    ) extends ComplexLoweredValue(Set(bindedVar), scrutinee, body) {

        override def sirType: SIRType = body.sirType

        override def pos: SIRPosition = inPos

        override def representation: LoweredValueRepresentation = body.representation

        override def isEffortLess: Boolean = false

        override def termInternal(gctx: TermGenerationContext): Term = {
            // this will add bidndendVar and scrutinee as rhs if needed
            body.termWithNeededVars(gctx)
        }

        override def docDef(ctx: LoweredValue.PrettyPrintingContext): Doc =
            val left =
                (Doc.text("AssocMapMatch(") & scrutinee.docRef(ctx) & Doc.lineOrSpace).grouped
            val right = Doc.text(")")
            body.docRef(ctx).bracketBy(left, right)
    }

    override def genMatch(
        matchData: SIR.Match,
        loweredScrutinee: LoweredValue,
        optTargetType: Option[SIRType]
    )(using
        lctx: LoweringContext
    ): LoweredValue = {
        if matchData.cases.length != 1 then
            throw LoweringException(
              s"MapSirTypeGenerator.genMatch: invalid match on Map, expected one case, got ${matchData.cases.length}",
              matchData.anns.pos
            )
        val loweredScrutineeR = loweredScrutinee.toRepresentation(
          ProductCaseClassRepresentation.PackedDataMap,
          matchData.anns.pos
        )
        val caseData = matchData.cases.head
        val argName = caseData.pattern match
            case SIR.Pattern.Constr(constr, bindings, typeParamsBindings) =>
                bindings.head
            case _ => "unused"
        val listType = retrieveListType(loweredScrutineeR.sirType)
        val rhs = lvBuiltinApply(
          SIRBuiltins.unMapData,
          loweredScrutineeR,
          listType,
          SumCaseClassRepresentation.SumDataPairList,
          matchData.anns.pos
        )
        val prevScope = lctx.scope
        val matchedVar = lvNewLazyNamedVar(
          argName,
          retrieveListType(loweredScrutinee.sirType),
          SumCaseClassRepresentation.SumDataPairList,
          rhs,
          caseData.anns.pos
        )
        val loweredBody = lctx.lower(caseData.body, optTargetType)
        lctx.scope = prevScope
        MapMatchLoweredValue(
          matchedVar,
          loweredScrutineeR,
          loweredBody,
          matchData.anns.pos
        )
    }

    private def retrieveListType(tp: SIRType): SIRType = {
        tp match
            case SIRType.CaseClass(decl, typeArgs, optParent) =>
                decl.name match
                    case "scalus.cardano.onchain.plutus.prelude.AssocMap" =>
                        val (ta, tb) = typeArgs match
                            case List(ta, tb) => (ta, tb)
                            case _ =>
                                throw LoweringException(
                                  s"Expected that AssocMap have two arguments, we have ${tp.show}",
                                  SIRPosition.empty
                                )
                        val pairType = SIRType.BuiltinPair(ta, tb)
                        SIRType.List(pairType)
                    case _ =>
                        throw LoweringException(
                          s"MapSirTypeGenerator.retrieveListType: expected List, got $tp",
                          SIRPosition.empty
                        )
            case SIRType.TypeLambda(params, body) =>
                SIRType.TypeLambda(params, retrieveListType(body))
            case SIRType.TypeProxy(ref) =>
                retrieveListType(ref)
            case _ =>
                throw LoweringException(
                  s"MapSirTypeGenerator.retrieveListType: expected List, got $tp",
                  SIRPosition.empty
                )
    }

}
