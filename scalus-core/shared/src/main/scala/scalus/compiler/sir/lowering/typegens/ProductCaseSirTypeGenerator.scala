package scalus.compiler.sir.lowering
package typegens

import org.typelevel.paiges.Doc
import scalus.cardano.ledger.MajorProtocolVersion
import scalus.compiler.sir.lowering.LoweredValue.Builder.*
import scalus.compiler.sir.lowering.ProductCaseClassRepresentation.*
import scalus.compiler.sir.*
import scalus.uplc.*

import scala.util.control.NonFatal

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
            genConstrUplcConstr(constr, loweredArgs)
        else
            val argTypeGens = loweredArgs.map(_.sirType).map(lctx.typeGenerator)
            val canBeConvertedToData = loweredArgs.zip(argTypeGens).forall {
                case (arg, typeGen) => typeGen.canBeConvertedToData(arg.sirType)
            }
            if !canBeConvertedToData then genConstrUplcConstr(constr, loweredArgs)
            else genConstrDataConstr(constr, loweredArgs, argTypeGens)
    }

    override def genSelect(sel: SIR.Select, loweredScrutinee: LoweredValue)(using
        lctx: LoweringContext
    ): LoweredValue = {
        // use data for now
        // TODO: optimize
        genSelectDataList(sel, loweredScrutinee)
    }

    def genSelectDataList(sel: SIR.Select, loweredScrutinee: LoweredValue)(using
        lctx: LoweringContext
    ): LoweredValue = {
        val dataListScrutinee = loweredScrutinee.toRepresentation(ProdDataList, sel.anns.pos)
        // val prevScope = lctx.scope
        val (list0, addList0ToScope) =
            if dataListScrutinee.isInstanceOf[IdentifiableLoweredValue] then
                (dataListScrutinee.asInstanceOf[IdentifiableLoweredValue], false)
            else
                (
                  lvNewLazyIdVar(
                    lctx.uniqueVarName("list_sel"),
                    dataListScrutinee.sirType,
                    SumCaseClassRepresentation.SumBuiltinList(SumCaseClassRepresentation.DataData),
                    dataListScrutinee,
                    sel.anns.pos
                  ),
                  true
                )
        val list0id = list0.id
        val constrDecl = retrieveConstrDecl(loweredScrutinee.sirType, sel.anns.pos)
        val fieldIndex = constrDecl.params.indexWhere(_.name == sel.field)
        if fieldIndex < 0 then
            throw LoweringException(
              s"Unknown field ${sel.field} for ${constrDecl.name}",
              sel.anns.pos
            )
        val scopeVars0: Set[IdentifiableLoweredValue] =
            if addList0ToScope then Set(list0) else Set.empty
        val (selHeadList, scopeVars) =
            if lctx.targetProtocolVersion >= MajorProtocolVersion.vanRossemPV && fieldIndex >= 2
            then
                // dropList(fieldIndex, list) is more efficient for n >= 2
                val droppedId = list0id + s"_drop_$fieldIndex"
                val droppedList = lctx.scope.getById(droppedId) match
                    case Some(v) => v
                    case None =>
                        lvNewLazyIdVar(
                          droppedId,
                          SIRType.List(SIRType.Data.tp),
                          SumCaseClassRepresentation.SumBuiltinList(
                            SumCaseClassRepresentation.DataData
                          ),
                          lvBuiltinApply2(
                            SIRBuiltins.dropList,
                            lvIntConstant(fieldIndex, sel.anns.pos),
                            list0,
                            SIRType.List(SIRType.Data.tp),
                            SumCaseClassRepresentation.SumBuiltinList(
                              SumCaseClassRepresentation.DataData
                            ),
                            sel.anns.pos
                          ),
                          sel.anns.pos
                        )
                (droppedList, scopeVars0 + droppedList)
            else
                // Original tail-chaining for V1-V3 or small indices (0, 1)
                (0 until fieldIndex).foldLeft((list0, scopeVars0)) { (acc, idx) =>
                    val tailId = list0id + s"_tail_${idx + 1}"
                    val tailLazyVar = lctx.scope.getById(tailId) match
                        case Some(v) => v
                        case None =>
                            lvNewLazyIdVar(
                              tailId,
                              SIRType.List(SIRType.Data.tp),
                              SumCaseClassRepresentation.SumBuiltinList(
                                SumCaseClassRepresentation.DataData
                              ),
                              lvBuiltinApply(
                                SIRBuiltins.tailList,
                                acc._1,
                                SIRType.List(SIRType.Data.tp),
                                SumCaseClassRepresentation.SumBuiltinList(
                                  SumCaseClassRepresentation.DataData
                                ),
                                sel.anns.pos
                              ),
                              sel.anns.pos
                            )
                    (tailLazyVar, acc._2 + tailLazyVar)
                }
        val body = lvBuiltinApply(
          SIRBuiltins.headList,
          selHeadList,
          sel.tp,
          lctx.typeGenerator(sel.tp).defaultDataRepresentation(sel.tp),
          sel.anns.pos
        )
        ScopeBracketsLoweredValue(scopeVars, body)
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

    def genMatchDataList(
        matchData: SIR.Match,
        loweredScrutinee: LoweredValue,
        optTargetType: Option[SIRType]
    )(using
        lctx: LoweringContext
    ): LoweredValue = {
        val constrDecl = retrieveConstrDecl(loweredScrutinee.sirType, matchData.anns.pos)
        matchData.cases match {
            case oneCase :: Nil =>
                val matchCase = oneCase.pattern match
                    case cs @ SIR.Pattern.Constr(constrDecl1, args, _) =>
                        if constrDecl1.name != constrDecl.name then
                            throw LoweringException(
                              s"Expected constructor ${constrDecl.name}, got ${constrDecl1.name}",
                              matchData.anns.pos
                            )
                        oneCase
                    case SIR.Pattern.Const(_) =>
                        throw LoweringException(
                          s"Constant pattern not supported for product case class ${constrDecl.name}",
                          matchData.anns.pos
                        )
                    case SIR.Pattern.Wildcard =>
                        val argsNames = constrDecl.params.map(_.name)
                        val argsTypes = constrDecl.params.map(_.tp)
                        // TODO: add typeArgs to env ?
                        oneCase.copy(pattern = SIR.Pattern.Constr(constrDecl, argsNames, argsTypes))
                val (dataList, addToScope) = loweredScrutinee.toRepresentation(
                  ProductCaseClassRepresentation.ProdDataList,
                  loweredScrutinee.pos
                ) match
                    case idv: IdentifiableLoweredValue => (idv, false)
                    case other =>
                        val v = lvNewLazyIdVar(
                          lctx.uniqueVarName("_match_data_list"),
                          SIRType.List(SIRType.Data.tp),
                          SumCaseClassRepresentation.SumBuiltinList(
                            SumCaseClassRepresentation.DataData
                          ),
                          other,
                          matchData.anns.pos
                        )
                        (v, true)
                DataConstrEmitter.genMatchDataConstrCase(
                  matchCase,
                  dataList,
                  optTargetType,
                  addToScope
                )
            case _ =>
                val myCase = selectMatchCase(
                  matchData,
                  loweredScrutinee,
                  constrDecl
                )
                lctx.info(
                  s"Product case class match should have only one case, but ${matchData.cases.length} found. Non-matched cases will be statically optimized out",
                  matchData.anns.pos
                )
                genMatchDataList(
                  matchData.copy(cases = List(myCase)),
                  loweredScrutinee,
                  optTargetType
                )
        }
    }

    def genConstrDataConstr(
        constr: SIR.Constr,
        loweredArgs: Seq[LoweredValue],
        argTypeGens: Seq[SirTypeUplcGenerator],
    )(using lctx: LoweringContext): LoweredValue = {
        val dataRepresentations = loweredArgs.zip(argTypeGens).map { case (arg, typeGen) =>
            try
                arg.toRepresentation(
                  typeGen.defaultDataRepresentation(arg.sirType),
                  constr.anns.pos
                )
            catch
                case NonFatal(e) =>
                    println(
                      s"error while converting to data representation: arg=${arg}, argTypeGen=${typeGen} "
                    )
                    println(
                      s"arg.sirType = ${arg.sirType.show}, representation = ${arg.representation}, constr.anns.pos = ${constr.anns.pos}"
                    )
                    println(
                      s"defaultDataRepresentation(${arg.sirType.show}) = ${typeGen.defaultDataRepresentation(arg.sirType)}"
                    )
                    println(s"typeGen = ${typeGen}")
                    println(
                      s"defaultTypeGen(${arg.sirType.show}) = ${lctx.typeGenerator(arg.sirType)}"
                    )
                    println(
                      s"arg created from: ${constr.anns.pos.file}:${constr.anns.pos.startLine + 1}"
                    )
                    throw e
        }
        // TODO: consider using ProdUplcConstr for more efficient encoding
        val s0 = lvDataDataListNil(constr.anns.pos)
        val dataList = dataRepresentations.foldRight(s0) { (arg, acc) =>
            lvBuiltinApply2(
              SIRBuiltins.mkCons,
              arg,
              acc,
              SIRType.List(SIRType.Data.tp),
              SumCaseClassRepresentation.SumBuiltinList(SumCaseClassRepresentation.DataData),
              constr.anns.pos
            )
        }

        // TODO: check correctness of constr.tp
        // val constrType =
        //    if SIRtype.isSumCaseClass(constr.tp) then
        //        ???
        //    else {
        //        constr.tp
        //    }

        val retval = new ProxyLoweredValue(dataList) {
            override def sirType: SIRType = constr.tp

            override def representation: LoweredValueRepresentation = ProdDataList

            override def termInternal(gctx: TermGenerationContext): Term = {
                dataList.termWithNeededVars(gctx)
            }

            override def docDef(ctx: LoweredValue.PrettyPrintingContext): Doc = {
                val left = Doc.text("Constr(") + Doc.text(constr.tp.show) + Doc.comma
                val right = Doc.text(")")
                dataList.docRef(ctx).bracketBy(left, right)
            }

            override def docRef(ctx: LoweredValue.PrettyPrintingContext): Doc = docDef(ctx)

        }
        retval
    }

    def genConstrUplcConstr(
        constr: SIR.Constr,
        loweredArgs: scala.List[LoweredValue]
    )(using
        lctx: LoweringContext
    ): LoweredValue = {
        val constrIndex = retrieveConstrIndex(constr.tp, constr.anns.pos)
        val constrDecl = retrieveConstrDecl(constr.tp, constr.anns.pos)

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
                            val targetRepr = lctx
                                .typeGenerator(paramType)
                                .defaultRepresentation(paramType)
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
      * its constrDecl. Used by `genConstrUplcConstr` to decide whether the field's static type pins
      * a specific repr that arg.repr must conform to.
      */
    private def hasClassLevelUplcRepr(tp: SIRType): Boolean = tp match
        case SIRType.CaseClass(decl, _, _)         => decl.annotations.data.contains("uplcRepr")
        case SIRType.SumCaseClass(decl, _)         => decl.annotations.data.contains("uplcRepr")
        case SIRType.Annotated(inner, _)           => hasClassLevelUplcRepr(inner)
        case SIRType.TypeLambda(_, body)           => hasClassLevelUplcRepr(body)
        case SIRType.TypeProxy(ref) if ref != null => hasClassLevelUplcRepr(ref)
        case _                                     => false

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
