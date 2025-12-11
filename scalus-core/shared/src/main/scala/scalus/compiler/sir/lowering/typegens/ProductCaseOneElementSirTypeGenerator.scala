package scalus.compiler.sir.lowering
package typegens

import org.typelevel.paiges.Doc
import scalus.compiler.sir.lowering.LoweredValue.Builder.*
import scalus.compiler.sir.lowering.ProductCaseClassRepresentation.{ProdDataConstr, ProdDataList}
import scalus.compiler.sir.*
import scalus.uplc.Term

case class ProductCaseOneElementSirTypeGenerator(
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

    override def isDataSupported(tp: SIRType)(using lctx: LoweringContext): Boolean =
        argGenerator.isDataSupported(tp)

    override def toRepresentation(
        input: LoweredValue,
        representation: LoweredValueRepresentation,
        pos: SIRPosition
    )(using lctx: LoweringContext): LoweredValue = {
        (input.representation, representation) match
            case (
                  ProductCaseClassRepresentation.OneElementWrapper(argRepr),
                  ProductCaseClassRepresentation.OneElementWrapper(newArgRepr)
                ) =>
                if argRepr == newArgRepr then input
                else
                    val newArg = argLoweredValue(input).toRepresentation(newArgRepr, pos)
                    new TypeRepresentationProxyLoweredValue(
                      newArg,
                      input.sirType,
                      ProductCaseClassRepresentation.OneElementWrapper(newArgRepr),
                      input.pos
                    )
            case (ProductCaseClassRepresentation.OneElementWrapper(argRepr), ProdDataList) =>
                val argInData = argLoweredValue(input).toRepresentation(
                  argGenerator.defaultDataRepresentation(input.sirType),
                  pos
                )
                lvBuiltinApply2(
                  SIRBuiltins.mkCons,
                  argInData,
                  lvBuiltinApply0(
                    SIRBuiltins.mkNilData,
                    SIRType.List(SIRType.Data.tp),
                    SumCaseClassRepresentation.SumDataList,
                    pos
                  ),
                  input.sirType,
                  ProdDataList,
                  pos
                )
            case (ProductCaseClassRepresentation.OneElementWrapper(argRepr), ProdDataConstr) =>
                input.toRepresentation(ProdDataList, pos).toRepresentation(ProdDataConstr, pos)
            case (
                  ProductCaseClassRepresentation.OneElementWrapper(argRepr),
                  outRepr @ TypeVarRepresentation(isBuiltin)
                ) =>
                if isBuiltin then input
                else
                    val argValue = argLoweredValue(input)
                    if argRepr.isCompatibleOn(argValue.sirType, outRepr, pos) then
                        new RepresentationProxyLoweredValue(input, outRepr, pos)
                    else
                        val newArg = argGenerator.toRepresentation(argValue, representation, pos)
                        val inPos = pos
                        new TypeRepresentationProxyLoweredValue(
                          newArg,
                          input.sirType,
                          outRepr,
                          inPos
                        )
            case (
                  inRepr @ TypeVarRepresentation(isBuiltin),
                  outRepr @ ProductCaseClassRepresentation.OneElementWrapper(argRepr)
                ) =>
                if isBuiltin then new RepresentationProxyLoweredValue(input, representation, pos)
                else
                    val argValue = argLoweredValue(input)
                    if argRepr.isCompatibleOn(argValue.sirType, argValue.representation, pos) then
                        new RepresentationProxyLoweredValue(input, representation, pos)
                    else
                        // we need to convert the argument to the new representation
                        val newArg = argGenerator.toRepresentation(argValue, argRepr, pos)
                        val inPos = pos
                        val retval = new TypeRepresentationProxyLoweredValue(
                          newArg,
                          input.sirType,
                          outRepr,
                          inPos
                        )
                        retval
            case (_, _) =>
                ProductCaseSirTypeGenerator.toRepresentation(input, representation, pos)

    }

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

        val constrIndexConstant = lvIntConstant(3, pos)
        val argValueIn = argLoweredValue(input)
        val argValue = argValueIn.toRepresentation(
          argGenerator.defaultDataRepresentation(argValueIn.sirType),
          pos
        )
        val prodArgs = lvBuiltinApply2(
          SIRBuiltins.mkCons,
          argValue,
          lvBuiltinApply0(
            SIRBuiltins.mkNilData,
            SIRType.List(SIRType.Data.tp),
            SumCaseClassRepresentation.SumDataList,
            pos
          ),
          SIRType.List(SIRType.Data.tp),
          SumCaseClassRepresentation.SumDataList,
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

    override def genConstr(constr: SIR.Constr)(using
        lctx: LoweringContext
    ): LoweredValue = {
        import ProductCaseOneElementSirTypeGenerator.*
        if constr.args.size != 1 then
            throw LoweringException(
              s"Expected one argument for product case class, got ${constr.args.size}",
              constr.anns.pos
            )
        val loweredArg = lctx.lower(constr.args.head)
        WrappedArg(constr, loweredArg)
    }

    override def genSelect(sel: SIR.Select, loweredScrutinee: LoweredValue)(using
        lctx: LoweringContext
    ): LoweredValue = {
        import ProductCaseOneElementSirTypeGenerator.*
        val sirCaseClass = retrieveCaseClassSirType(loweredScrutinee.sirType, loweredScrutinee.pos)
        val name = sirCaseClass.constrDecl.params.head.name
        if sel.field != name then
            throw LoweringException(
              s"Expected select on ${name}, got ${sel.field}",
              sel.anns.pos
            )
        else {
            val retval = argLoweredValue(loweredScrutinee)
            argLoweredValue(loweredScrutinee)
        }
    }

    override def genMatch(
        matchData: SIR.Match,
        loweredScrutinee: LoweredValue,
        optTargetType: Option[SIRType]
    )(using
        lctx: LoweringContext
    ): LoweredValue = {
        import ProductCaseOneElementSirTypeGenerator.*
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

    private def argLoweredValue(input: LoweredValue)(using LoweringContext): LoweredValue = {
        import ProductCaseOneElementSirTypeGenerator.*
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
                      case TypeVarRepresentation(isBuiltin) =>
                          if isBuiltin then argGenerator.defaultRepresentation(argType)
                          else argGenerator.defaultTypeVarReperesentation(argType)
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

}

object ProductCaseOneElementSirTypeGenerator {

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
