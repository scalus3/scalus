package scalus.compiler.sir.lowering
package typegens

import org.typelevel.paiges.Doc
import scalus.compiler.sir.lowering.LoweredValue.Builder.*
import scalus.compiler.sir.*
import scalus.uplc.Term

case class OneElementWrapperEmitter(
    argGenerator: SirTypeUplcGenerator,
) extends SirTypeUplcGenerator {

    override def defaultRepresentation(
        tp: SIRType
    )(using LoweringContext): LoweredValueRepresentation =
        ProductCaseClassRepresentation.OneElementWrapper(
          argGenerator.defaultRepresentation(tp)
        )

    override def defaultDataRepresentation(
        tp: SIRType
    )(using LoweringContext): LoweredValueRepresentation =
        ProductCaseClassRepresentation.OneElementWrapper(
          argGenerator.defaultDataRepresentation(tp)
        )

    override def defaultTypeVarReperesentation(
        tp: SIRType
    )(using lctx: LoweringContext): LoweredValueRepresentation =
        ProductCaseClassRepresentation.OneElementWrapper(
          argGenerator.defaultTypeVarReperesentation(tp)
        )

    override def canBeConvertedToData(tp: SIRType)(using lctx: LoweringContext): Boolean =
        argGenerator.canBeConvertedToData(tp)

    override def upcastOne(input: LoweredValue, targetType: SIRType, pos: SIRPosition)(using
        lctx: LoweringContext
    ): LoweredValue = {

        val dataDecl = SIRType
            .retrieveDataDecl(targetType)
            .fold(
              msg =>
                  throw LoweringException(
                    s"Can't retrieve decl from ${targetType.show}: $msg",
                    pos
                  ),
              identity
            )
        val constrDecl = SIRType
            .retrieveConstrDecl(input.sirType)
            .fold(
              msg =>
                  throw LoweringException(
                    s"Can't retrieve constr decl from ${input.sirType.show}: $msg",
                    pos
                  ),
              identity
            )

        val constrIndex = dataDecl.constructors.indexWhere(_.name == constrDecl.name)
        if constrIndex < 0 then {
            throw LoweringException(
              s"Expected case class ${dataDecl.name} with constr ${constrDecl.name}, but it is not found in data declaration",
              pos
            )
        }

        val constrIndexConstant = lvIntConstant(constrIndex, pos)
        val argValueIn = argLoweredValue(input)
        val argValue = argValueIn.toRepresentation(
          argGenerator.defaultDataRepresentation(argValueIn.sirType),
          pos
        )
        val prodArgs = lvBuiltinApply2(
          SIRBuiltins.mkCons,
          argValue,
          lvDataNil(
            pos,
            SIRType.List(SIRType.Data.tp),
            SumCaseClassRepresentation.SumBuiltinList(SumCaseClassRepresentation.DataData)
          ),
          SIRType.List(SIRType.Data.tp),
          SumCaseClassRepresentation.SumBuiltinList(SumCaseClassRepresentation.DataData),
          pos
        )

        lvBuiltinApply2(
          SIRBuiltins.constrData,
          constrIndexConstant,
          prodArgs,
          targetType,
          SumCaseClassRepresentation.DataConstr,
          pos
        )

    }

    override def genConstrLowered(
        constr: SIR.Constr,
        loweredArgs: scala.List[LoweredValue],
        optTargetType: Option[SIRType]
    )(using lctx: LoweringContext): LoweredValue = {
        import OneElementWrapperEmitter.*
        if loweredArgs.size != 1 then
            throw LoweringException(
              s"Expected one argument for product case class, got ${loweredArgs.size}",
              constr.anns.pos
            )
        WrappedArg(constr, loweredArgs.head)
    }

    override def genSelect(sel: SIR.Select, loweredScrutinee: LoweredValue)(using
        lctx: LoweringContext
    ): LoweredValue = {
        import OneElementWrapperEmitter.*
        val sirCaseClass = retrieveCaseClassSirType(loweredScrutinee.sirType, loweredScrutinee.pos)
        val name = sirCaseClass.constrDecl.params.head.name
        if sel.field != name then
            throw LoweringException(
              s"Expected select on ${name}, got ${sel.field}",
              sel.anns.pos
            )
        else argLoweredValue(loweredScrutinee)
    }

    override def genMatch(
        matchData: SIR.Match,
        loweredScrutinee: LoweredValue,
        optTargetType: Option[SIRType]
    )(using
        lctx: LoweringContext
    ): LoweredValue = {
        import OneElementWrapperEmitter.*
        val sirCaseClass = retrieveCaseClassSirType(loweredScrutinee.sirType, loweredScrutinee.pos)
        val name = sirCaseClass.constrDecl.params.head.name
        matchData.cases match {
            case Nil =>
                throw LoweringException("Empty match cases", matchData.anns.pos)
            case SIR.Case(pattern, body, anns) :: Nil =>
                pattern match
                    case SIR.Pattern.Constr(constr, bindings, typeParams) =>
                        if constr.name == sirCaseClass.constrDecl.name then
                            val argType = SIRType.substitute(
                              constr.params.head.tp,
                              sirCaseClass.constrDecl.typeParams.zip(typeParams).toMap,
                              Map.empty
                            )
                            val prevScope = lctx.scope
                            val arg = argLoweredValue(loweredScrutinee)
                            val bindingVar = lvNewLazyNamedVar(
                              bindings.head,
                              argType,
                              arg.representation,
                              arg,
                              anns.pos
                            )
                            val loweredBody = lctx.lower(body, optTargetType)
                            lctx.scope = prevScope
                            ScopeBracketsLoweredValue(Set(bindingVar), loweredBody)
                        else
                            throw LoweringException(
                              s"Expected case class ${sirCaseClass.constrDecl.name}, got ${constr.name}",
                              anns.pos
                            )
                    case SIR.Pattern.Const(_) =>
                        throw LoweringException(
                          s"Constant pattern not supported for product case class ${sirCaseClass.constrDecl.name}",
                          anns.pos
                        )
                    case SIR.Pattern.Wildcard =>
                        lctx.lower(body, optTargetType)
            case _ =>
                throw LoweringException(
                  s"Expected single case with select on ${name}, got ${matchData.cases}",
                  matchData.anns.pos
                )
        }
    }

    private[lowering] def argLoweredValue(
        input: LoweredValue
    )(using LoweringContext): LoweredValue = {
        import OneElementWrapperEmitter.*
        input match {
            case WrappedArg(constr, arg) => arg
            case other =>
                val argType = retrieveArgType(other.sirType, other.pos)
                ArgProxyLoweredValue(
                  other,
                  argType,
                  input.representation match {
                      case ProductCaseClassRepresentation.OneElementWrapper(argRepr) =>
                          argRepr
                      case tvr: TypeVarRepresentation =>
                          import SIRType.TypeVarKind.*
                          tvr.kind match
                              case Transparent | Unwrapped =>
                                  argGenerator.defaultRepresentation(argType)
                              case Fixed =>
                                  argGenerator.defaultTypeVarReperesentation(argType)
                      case _ =>
                          throw LoweringException(
                            s"Expected OneElementWrapper representation, got ${input.representation}",
                            other.pos
                          )
                  },
                  other.pos
                )
        }
    }

    /** Convert a `OneElementWrapper(_)` source to `target`. The handled targets cover the wrapper's
      * direct outbound moves (rewrap with a different argRepr, materialize as `ProdDataList`, route
      * via `ProdDataList` for `ProdDataConstr`, relabel-as-target for TypeVar). Other source-target
      * pairs fall through to `ProductCaseEmitter.emitConvert`; source TypeVar bytes route through
      * `bridgeFromKind`.
      */
    def emitConvert(
        input: LoweredValue,
        representation: LoweredValueRepresentation,
        pos: SIRPosition
    )(using lctx: LoweringContext): LoweredValue = {
        import ProductCaseClassRepresentation.{OneElementWrapper, ProdDataList, ProdDataConstr}
        (input.representation, representation) match
            case (OneElementWrapper(argRepr), OneElementWrapper(newArgRepr)) =>
                if argRepr == newArgRepr then input
                else
                    val newArg = argLoweredValue(input).toRepresentation(newArgRepr, pos)
                    new TypeRepresentationProxyLoweredValue(
                      newArg,
                      input.sirType,
                      OneElementWrapper(newArgRepr),
                      input.pos
                    )
            case (_: OneElementWrapper, ProdDataList) =>
                val argInData = argLoweredValue(input).toRepresentation(
                  argGenerator.defaultDataRepresentation(input.sirType),
                  pos
                )
                lvBuiltinApply2(
                  SIRBuiltins.mkCons,
                  argInData,
                  lvDataNil(
                    pos,
                    SIRType.List(SIRType.Data.tp),
                    SumCaseClassRepresentation.SumBuiltinList(SumCaseClassRepresentation.DataData)
                  ),
                  input.sirType,
                  ProdDataList,
                  pos
                )
            case (_: OneElementWrapper, ProdDataConstr) =>
                input.toRepresentation(ProdDataList, pos).toRepresentation(ProdDataConstr, pos)
            case (_: OneElementWrapper, tvr: TypeVarRepresentation) =>
                import SIRType.TypeVarKind.*
                tvr.kind match
                    case Transparent => input
                    case Unwrapped | Fixed =>
                        val targetUnderlying =
                            if tvr.kind == Unwrapped then
                                argGenerator.defaultRepresentation(input.sirType)
                            else argGenerator.defaultTypeVarReperesentation(input.sirType)
                        val argValue = argLoweredValue(input)
                        val convertedArg = argValue.toRepresentation(targetUnderlying, pos)
                        new TypeRepresentationProxyLoweredValue(
                          convertedArg,
                          input.sirType,
                          tvr,
                          pos
                        )
            case (tvr: TypeVarRepresentation, _: OneElementWrapper) =>
                // Source TypeVar dispatch via shared helper. Transparent relabels; Unwrapped/Fixed
                // relabel as the input type's defaultRepresentation (= OneElementWrapper(arg's
                // default)) and recurse — landing in the (OneElementWrapper, OneElementWrapper)
                // arm above. Same final shape as if we'd extracted the arg + converted + wrapped
                // open-coded, via a slightly longer proxy chain.
                TypeVarOps.bridgeFromKind(input, tvr, representation, pos)
            case _ =>
                ProductCaseEmitter.emitConvert(input, representation, pos)
    }

}

object OneElementWrapperEmitter {

    case class WrappedArg(constr: SIR.Constr, arg: LoweredValue) extends ProxyLoweredValue(arg) {

        override def sirType: SIRType = constr.tp

        override def pos: SIRPosition = constr.anns.pos

        override def representation: LoweredValueRepresentation =
            ProductCaseClassRepresentation.OneElementWrapper(arg.representation)

        override def termInternal(gctx: TermGenerationContext): Term =
            arg.termInternal(gctx)

        override def docDef(ctx: LoweredValue.PrettyPrintingContext): Doc = {
            val left = Doc.text("ProduceCaseOneElement.Wrapped") + Doc.text("(")
            val arg1 = Doc.text(constr.name)
            val arg2 = arg.docRef(ctx)
            val args = Doc.intercalate(Doc.text(", "), List(arg1, arg2)).grouped
            val right = Doc.text(")")
            args.bracketBy(left, right)
        }

    }

    case class ArgProxyLoweredValue(
        wrapper: LoweredValue,
        tp: SIRType,
        repr: LoweredValueRepresentation,
        inPos: SIRPosition,
    ) extends ProxyLoweredValue(wrapper) {

        override def sirType: SIRType = tp

        override def pos: SIRPosition = inPos

        override def representation: LoweredValueRepresentation = repr

        override def termInternal(gctx: TermGenerationContext): Term =
            wrapper.termInternal(gctx)

        override def docDef(ctx: LoweredValue.PrettyPrintingContext): Doc = {
            val left = Doc.text("ProduceCaseOneElement.ArgProxy") + Doc.text("(")
            val arg = wrapper.docRef(ctx)
            val right =
                Doc.text(")") + Doc.text(":") + Doc.text(sirType.show) + PrettyPrinter.inBrackets(
                  repr.doc
                )
            arg.bracketBy(left, right)
        }

    }

    private def retrieveCaseClassSirType(
        rType: SIRType,
        pos: SIRPosition
    ): SIRType.CaseClass = {
        rType match
            case r @ SIRType.CaseClass(constrDecl, typeArgs, optParent) =>
                if r.constrDecl.params.size != 1 then
                    throw LoweringException(
                      s"Expected one parameter for product case class, got ${r.constrDecl.params.size}",
                      pos
                    )
                r
            case SIRType.TypeProxy(ref) =>
                retrieveCaseClassSirType(ref, pos)
            case SIRType.TypeLambda(params, body) =>
                retrieveCaseClassSirType(body, pos)
            case _ =>
                throw LoweringException(
                  s"Expected case class type, got ${rType.show}",
                  pos
                )
    }

    def retrieveArgType(rType: SIRType, pos: SIRPosition): SIRType = {
        rType match {
            case SIRType.CaseClass(constrDecl, typeArgs, optParent) =>
                val param = constrDecl.params.head
                SIRType.substitute(param.tp, constrDecl.typeParams.zip(typeArgs).toMap, Map.empty)
            case SIRType.TypeProxy(ref) =>
                retrieveArgType(ref, pos)
            case SIRType.TypeLambda(params, body) =>
                retrieveArgType(body, pos)
            case _ =>
                throw LoweringException(
                  s"Expected case class type, got ${rType.show}",
                  pos
                )
        }
    }

}
