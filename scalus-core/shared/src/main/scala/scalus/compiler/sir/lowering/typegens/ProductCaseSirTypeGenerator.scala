package scalus.compiler.sir.lowering
package typegens

import scalus.compiler.sir.lowering.ProductCaseClassRepresentation.*
import scalus.compiler.sir.*

/** Product with one element without parent, represented as an element.
  */
object ProductCaseSirTypeGenerator extends SirTypeUplcGenerator {

    override def defaultRepresentation(tp: SIRType)(using
        loweringContext: LoweringContext
    ): LoweredValueRepresentation = {
        SIRType.collectProd(tp) match {
            case Some(_, constrDecl, _) =>
                if constrDecl.name == SIRType.BuiltinPair.name
                then {
                    // ProdBuiltinPair now carries explicit fstRepr/sndRepr, so use the
                    // components' natural `defaultRepresentation` (Transparent stays Transparent
                    // for passthrough; concrete types get their own default). Previously forced
                    // Data on both components, which silently coerced Transparent TypeVars into
                    // the Data-centric Fixed fallback and leaked `TypeVarRepresentation(Fixed)`
                    // into enclosing structures.
                    val (fstType, sndType) =
                        ProductCaseClassRepresentation.ProdBuiltinPair
                            .extractPairComponentTypes(tp)
                    val fstRepr =
                        loweringContext.typeGenerator(fstType).defaultRepresentation(fstType)
                    val sndRepr =
                        loweringContext.typeGenerator(sndType).defaultRepresentation(sndType)
                    ProductCaseClassRepresentation.ProdBuiltinPair(fstRepr, sndRepr)
                } else if constrDecl.name == SIRType.Tuple2.name
                then { // here we can change and see tests.
                    // ProductCaseClassRepresentation.PairData
                    // better for benchmarks for both mem and cpu
                    ProductCaseClassRepresentation.ProdDataList
                } else ProductCaseClassRepresentation.ProdDataList
            case _ =>
                // impossinle
                ProductCaseClassRepresentation.ProdDataList
        }
    }

    override def defaultDataRepresentation(tp: SIRType)(using
        loweringContext: LoweringContext
    ): LoweredValueRepresentation =
        ProductCaseClassRepresentation.ProdDataConstr

    override def defaultTypeVarReperesentation(tp: SIRType)(using
        loweringContext: LoweringContext
    ): LoweredValueRepresentation =
        ProductCaseClassRepresentation.ProdDataConstr

    override def canBeConvertedToData(
        tp: SIRType
    )(using loweringContext: LoweringContext): Boolean = {
        true
    }

    override def toRepresentation(
        input: LoweredValue,
        representation: LoweredValueRepresentation,
        pos: SIRPosition
    )(using lctx: LoweringContext): LoweredValue =
        ProdDispatch.dispatcherBypass("ProductCaseSirTypeGenerator")

    override def upcastOne(input: LoweredValue, targetType: SIRType, pos: SIRPosition)(using
        lctx: LoweringContext
    ): LoweredValue = {
        // INPUT-repr-first dispatch: when input is `ProdUplcConstr`, its UPLC bytes are
        // already `Constr(tag, fields)`. `SumDispatch.chooseUpcastOutputRepr` overlays
        // the input's actual puc onto `buildSumUplcConstr(targetType)` so concrete
        // field-repr refinements survive without a Data round-trip that would fail for
        // abstract TypeVar fields in isolation.
        input.representation match
            case _: ProductCaseClassRepresentation.ProdUplcConstr =>
                val outRepr = SumDispatch.chooseUpcastOutputRepr(input, targetType, pos)
                return new TypeRepresentationProxyLoweredValue(input, targetType, outRepr, pos)
            case _ => // fall through to target-default-based dispatch
        val targetTypeGenerator = lctx.typeGenerator(targetType)
        targetTypeGenerator.defaultRepresentation(targetType) match {
            case targetListRepr @ SumCaseClassRepresentation.SumBuiltinList(elemRepr) =>
                if !elemRepr.isDataCentric then
                    throw LoweringException(
                      s"upcastOne to SumBuiltinList with non-data-centric element repr ${elemRepr} is not yet supported",
                      pos
                    )
                val constrDecl = SIRType
                    .retrieveConstrDecl(input.sirType)
                    .getOrElse(
                      throw LoweringException(
                        s"can't retrieve constrDecl from value with Prod representation: ${input.sirType}, input=${input}",
                        pos
                      )
                    )
                if constrDecl.name == SIRType.List.Cons.name
                    || constrDecl.name == SIRType.List.NilConstr.name
                then
                    val inputR = input.toRepresentation(ProdDataList, pos)
                    new TypeRepresentationProxyLoweredValue(
                      inputR,
                      targetType,
                      targetListRepr,
                      pos
                    )
                else
                    throw LoweringException(
                      s"Unkonow constructor name for data-list: ${constrDecl.name}",
                      pos
                    )
            case targetUplcConstr: SumCaseClassRepresentation.SumUplcConstr =>
                // Target is SumUplcConstr — keep native Constr representation
                new TypeRepresentationProxyLoweredValue(
                  input,
                  targetType,
                  targetUplcConstr,
                  pos
                )
            case other =>
                // All other representations are Data-compatible; convert to ProdDataConstr.
                val asDataConstr = input.toRepresentation(
                  ProductCaseClassRepresentation.ProdDataConstr,
                  pos
                )
                new TypeRepresentationProxyLoweredValue(
                  asDataConstr,
                  targetType,
                  SumCaseClassRepresentation.DataConstr,
                  pos
                )
        }
    }

    override def genConstrLowered(
        constr: SIR.Constr,
        loweredArgs: scala.List[LoweredValue],
        optTargetType: Option[SIRType]
    )(using lctx: LoweringContext): LoweredValue = {
        // Repr-routing decisions live in `SumDispatch.chooseConstrOutputRepr`. The
        // remaining decision here is structural: a Transparent TypeVar arg or a
        // non-Data-convertible payload forces native UplcConstr emission.
        if hasTransparentTypeVarArgs(loweredArgs) then
            ProdUplcConstrEmitter.genConstr(constr, loweredArgs)
        else
            val argTypeGens = loweredArgs.map(_.sirType).map(lctx.typeGenerator)
            val canBeConvertedToData = loweredArgs.zip(argTypeGens).forall {
                case (arg, typeGen) => typeGen.canBeConvertedToData(arg.sirType)
            }
            if !canBeConvertedToData then ProdUplcConstrEmitter.genConstr(constr, loweredArgs)
            else ProdDataListEmitter.genConstr(constr, loweredArgs, argTypeGens)
    }

    override def genSelect(sel: SIR.Select, loweredScrutinee: LoweredValue)(using
        lctx: LoweringContext
    ): LoweredValue = {
        // use data for now
        // TODO: optimize
        ProdDataListEmitter.genSelect(sel, loweredScrutinee)
    }

    override def genMatch(
        matchData: SIR.Match,
        loweredScrutinee: LoweredValue,
        optTargetType: Option[SIRType]
    )(using
        LoweringContext
    ): LoweredValue = {
        // Per-repr dispatch lives in `ProdDispatch.genMatch` (Phase 4c step 2). The
        // recognized reprs route there directly; unrecognized reprs would loop through
        // `ProdDispatch`'s typegen-fallback back into this method, so we throw
        // explicitly to keep the failure mode visible. Mirror of
        // `DataConstrEmitter.genMatch`.
        loweredScrutinee.representation match
            case ProdDataList | PackedDataList | ProdDataConstr | PairIntDataList |
                _: ProductCaseClassRepresentation.ProdBuiltinPair |
                _: ProductCaseClassRepresentation.ProdUplcConstr |
                _: SumCaseClassRepresentation.SumUplcConstr | _: TypeVarRepresentation =>
                ProdDispatch.genMatch(matchData, loweredScrutinee, optTargetType)
            case _ =>
                throw LoweringException(
                  s"Unsupported representation ${loweredScrutinee.representation} for match expression",
                  matchData.anns.pos
                )
    }

    def selectMatchCase(
        matchData: SIR.Match,
        loweredScroutine: LoweredValue,
        constrDecl: ConstrDecl
    )(using lctx: LoweringContext): SIR.Case = {
        if matchData.cases.length > 1 then
            lctx.info(
              s"More than one case for product ${loweredScroutine.sirType.show} in match, will shrink to one case",
              matchData.anns.pos
            )
        val myCase = {
            val constrCases = matchData.cases.filter { c =>
                c.pattern match
                    case SIR.Pattern.Constr(constrDecl1, args, typeArgs) =>
                        constrDecl1.name == constrDecl.name
                    case _ => false
            }
            if constrCases.length > 1 then
                throw LoweringException(
                  s"More than one case for ${constrDecl.name} found: ${constrCases.map(_.anns.pos).mkString(", ")}",
                  matchData.anns.pos
                )
            constrCases.headOption.orElse(
              matchData.cases.find { _.pattern == SIR.Pattern.Wildcard }
            )
        }
        myCase.getOrElse(
          throw LoweringException(
            s"No applicable case found for ${constrDecl.name} match",
            matchData.anns.pos
          )
        )
    }

    def retrieveConstrIndex(tp: SIRType, pos: SIRPosition): Int = {
        tp match {
            case SIRType.CaseClass(constrDecl, targs, optParent) =>
                optParent match
                    case None => 0
                    case Some(parent) =>
                        val parentDecl = SIRType
                            .retrieveDataDecl(parent)
                            .fold(
                              msg =>
                                  throw LoweringException(
                                    s"Can't retrieve parent decl from ${parent.show}: $msg",
                                    pos
                                  ),
                              identity
                            )
                        val retval = parentDecl.constructors.indexWhere(_.name == constrDecl.name)
                        if retval < 0 then {
                            throw LoweringException(
                              s"Expected case class ${constrDecl.name} with constr ${constrDecl.name}, but it is not found in data declaration",
                              pos
                            )
                        }
                        retval
            case SIRType.Annotated(underlying, _) =>
                retrieveConstrIndex(underlying, pos)
            case SIRType.TypeLambda(params, body) =>
                retrieveConstrIndex(body, pos)
            case SIRType.TypeProxy(ref) =>
                retrieveConstrIndex(ref, pos)
            case _ =>
                throw LoweringException(
                  s"Expected case class type, got ${tp.show}",
                  pos
                )
        }
    }

    def retrieveConstrDecl(tp: SIRType, pos: SIRPosition): ConstrDecl = {
        SIRType.retrieveConstrDecl(tp) match
            case Right(decl) => decl
            case Left(msg) =>
                throw LoweringException(
                  s"Can't retrieve constr decl from ${tp.show}: $msg",
                  pos
                )
    }

}
