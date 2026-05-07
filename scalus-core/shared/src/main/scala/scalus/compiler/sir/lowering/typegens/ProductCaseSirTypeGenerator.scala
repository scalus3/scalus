package scalus.compiler.sir.lowering
package typegens

import org.typelevel.paiges.Doc
import scalus.cardano.ledger.MajorProtocolVersion
import scalus.compiler.sir.lowering.LoweredValue.Builder.*
import scalus.compiler.sir.lowering.ProductCaseClassRepresentation.*
import scalus.compiler.sir.*
import scalus.uplc.{Term, UplcAnnotation}

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
                        SirTypeUplcGenerator.defaultRepresentation(fstType)
                    val sndRepr =
                        SirTypeUplcGenerator.defaultRepresentation(sndType)
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
            val canBeConvertedToData = loweredArgs.zip(argTypeGens).forall { case (arg, typeGen) =>
                typeGen.canBeConvertedToData(arg.sirType)
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

    /** Outbound conversions for plain product values. Handles `ProdDataList` / `ProdDataConstr` /
      * `PackedDataList` / `ProdUplcConstr` / `ProdBuiltinPair` / `OneElementWrapper` sources via
      * direct emission or two-hop chains.
      */
    def emitConvert(
        input: LoweredValue,
        representation: LoweredValueRepresentation,
        pos: SIRPosition
    )(using lctx: LoweringContext): LoweredValue = {
        (input.representation, representation) match {
            case (ProdDataList, ProdDataList) => input
            case (ProdDataList, PackedDataList) =>
                lvBuiltinApply(SIRBuiltins.listData, input, input.sirType, PackedDataList, pos)
            case (ProdDataList, ProdDataConstr) =>
                val constrIndex = retrieveConstrIndex(input.sirType, pos)
                lvBuiltinApply2(
                  SIRBuiltins.constrData,
                  lvIntConstant(constrIndex, pos),
                  input,
                  input.sirType,
                  ProdDataConstr,
                  pos
                )
            case (ProdDataList, PairIntDataList) =>
                emitConvert(input, ProdDataConstr, pos).toRepresentation(PairIntDataList, pos)
            case (ProdDataList, puc: ProdUplcConstr) =>
                // ProdDataList → ProdUplcConstr: extract each field via headList/tailList,
                // convert from Data to native repr, build Term.Constr(tag, [fields])
                val constrDecl = retrieveConstrDecl(input.sirType, pos)
                val dataListRepr =
                    SumCaseClassRepresentation.SumBuiltinList(SumCaseClassRepresentation.DataData)
                val (inputIdv, inputIdvAdded) =
                    lvAsIdentifiable(input, "dl_to_uc", input.sirType, ProdDataList, pos)
                var currentList: LoweredValue = inputIdv
                val fields = constrDecl.params.zip(puc.fieldReprs).map { (param, fieldRepr) =>
                    val tp = lctx.resolveTypeVarIfNeeded(param.tp)
                    val dataRepr = SirTypeUplcGenerator.defaultDataRepresentation(tp)
                    val head = lvBuiltinApply(SIRBuiltins.headList, currentList, tp, dataRepr, pos)
                    currentList = lvBuiltinApply(
                      SIRBuiltins.tailList,
                      currentList,
                      SIRType.List(SIRType.Data.tp),
                      dataListRepr,
                      pos
                    )
                    head.toRepresentation(fieldRepr, pos)
                }
                val inPos = pos
                val result = new ComplexLoweredValue(Set.empty, fields*) {
                    override def sirType = input.sirType
                    override def representation = puc
                    override def pos = inPos
                    override def termInternal(gctx: TermGenerationContext) =
                        Term.Constr(
                          scalus.cardano.ledger.Word64(puc.tag.toLong),
                          fields.map(_.termWithNeededVars(gctx)).toList,
                          UplcAnnotation(inPos)
                        )
                    override def docDef(ctx: LoweredValue.PrettyPrintingContext) =
                        Doc.text("DataList→UplcConstr")
                    override def docRef(ctx: LoweredValue.PrettyPrintingContext) = docDef(ctx)
                }
                if inputIdvAdded then ScopeBracketsLoweredValue(Set(inputIdv), result)
                else result
            case (ProdDataList, outRep @ OneElementWrapper(_)) =>
                lvBuiltinApply(SIRBuiltins.headList, input, input.sirType, outRep, pos)
            case (ProdDataList, outPair @ ProdBuiltinPair(fstRepr, sndRepr)) =>
                val (inputIdv, addToScoped) =
                    lvAsIdentifiable(
                      input,
                      "dl_to_pair_input",
                      input.sirType,
                      input.representation,
                      pos
                    )
                val head =
                    lvBuiltinApply(SIRBuiltins.headList, inputIdv, input.sirType, fstRepr, pos)
                val headTail = lvBuiltinApply(
                  SIRBuiltins.tailList,
                  inputIdv,
                  SIRType.List(SIRType.Data.tp),
                  ProdDataList,
                  pos
                )
                val headTailHead =
                    lvBuiltinApply(SIRBuiltins.headList, headTail, SIRType.Data.tp, sndRepr, pos)
                val body = lvBuiltinApply2(
                  SIRBuiltins.mkPairData,
                  head,
                  headTailHead,
                  input.sirType,
                  outPair,
                  pos
                )
                if addToScoped then ScopeBracketsLoweredValue(Set(inputIdv), body)
                else body
            case (PackedDataList, ProdDataList) =>
                lvBuiltinApply(SIRBuiltins.unListData, input, input.sirType, ProdDataList, pos)
            case (PackedDataList, PackedDataList) =>
                input
            case (PackedDataList, puc: ProdUplcConstr) =>
                input.toRepresentation(ProdDataList, pos).toRepresentation(puc, pos)
            case (PackedDataList, outputRep @ OneElementWrapper(_)) =>
                input.toRepresentation(ProdDataList, pos).toRepresentation(outputRep, pos)
            case (PackedDataList, outPair: ProdBuiltinPair) =>
                input.toRepresentation(ProdDataList, pos).toRepresentation(outPair, pos)
            case (ProdDataConstr, ProdDataList) =>
                val pairIntDataList = lvBuiltinApply(
                  SIRBuiltins.unConstrData,
                  input,
                  SIRType.BuiltinPair(SIRType.Integer, SIRType.Data.tp),
                  PairIntDataList,
                  pos
                )
                lvBuiltinApply(
                  SIRBuiltins.sndPair,
                  pairIntDataList,
                  input.sirType,
                  ProdDataList,
                  pos
                )
            case (ProdDataConstr, PackedDataList) =>
                input.toRepresentation(ProdDataList, pos).toRepresentation(PackedDataList, pos)
            case (ProdDataConstr, ProdDataConstr) =>
                input
            case (ProdDataConstr, PairIntDataList) =>
                lvBuiltinApply(
                  SIRBuiltins.unConstrData,
                  input,
                  SIRType.BuiltinPair(SIRType.Integer, SIRType.Data.tp),
                  PairIntDataList,
                  pos
                )
            case (ProdDataConstr, puc: ProdUplcConstr) =>
                input.toRepresentation(ProdDataList, pos).toRepresentation(puc, pos)
            case (ProdDataConstr, outPair: ProdBuiltinPair) =>
                input.toRepresentation(ProdDataList, pos).toRepresentation(outPair, pos)
            case (puc: ProdUplcConstr, ProdDataList) =>
                val constrDecl = retrieveConstrDecl(input.sirType, pos)
                val fieldNames =
                    constrDecl.params.indices.map(i => lctx.uniqueVarName(s"_uc_f$i"))
                val fieldTypes = constrDecl.params.map(p => lctx.resolveTypeVarIfNeeded(p.tp))
                val fieldVars =
                    fieldNames.zip(fieldTypes).zip(puc.fieldReprs).map { case ((name, tp), repr) =>
                        new VariableLoweredValue(
                          id = name,
                          name = name,
                          sir = SIR.Var(name, tp, AnnotationsDecl(pos)),
                          representation = repr
                        )
                    }
                val dataListNil = lvDataDataListNil(pos)
                val dataList = fieldVars.zip(fieldTypes).foldRight(dataListNil: LoweredValue) {
                    case ((fv, tp), acc) =>
                        val dataRepr = SirTypeUplcGenerator.defaultDataRepresentation(tp)
                        lvBuiltinApply2(
                          SIRBuiltins.mkCons,
                          fv.toRepresentation(dataRepr, pos),
                          acc,
                          SIRType.List(SIRType.Data.tp),
                          SumCaseClassRepresentation.SumBuiltinList(dataRepr),
                          pos
                        )
                }
                new ComplexLoweredValue(fieldVars.toSet, input, dataList) {
                    override def sirType = input.sirType
                    override def representation = ProdDataList
                    override def pos = input.pos
                    override def termInternal(gctx: TermGenerationContext) = {
                        val innerCtx =
                            gctx.copy(generatedVars = gctx.generatedVars ++ fieldVars.map(_.id))
                        val body = dataList.termWithNeededVars(innerCtx)
                        val branch = fieldVars.foldRight(body) { (fv, inner) =>
                            Term.LamAbs(fv.id, inner, UplcAnnotation(pos))
                        }
                        // Pad with Error branches for tags < this constructor's tag
                        val errorBranches =
                            scala.List.fill(puc.tag)(Term.Error(UplcAnnotation(pos)))
                        Term.Case(
                          input.termWithNeededVars(gctx),
                          errorBranches :+ branch,
                          UplcAnnotation(pos)
                        )
                    }
                    override def docDef(ctx: LoweredValue.PrettyPrintingContext) =
                        Doc.text("UplcConstr→DataList(") + input.docRef(ctx) + Doc.text(")")
                    override def docRef(ctx: LoweredValue.PrettyPrintingContext) = docDef(ctx)
                }
            case (_: ProdUplcConstr, PackedDataList) =>
                input.toRepresentation(ProdDataList, pos).toRepresentation(PackedDataList, pos)
            case (_: ProdUplcConstr, ProdDataConstr) =>
                input.toRepresentation(ProdDataList, pos).toRepresentation(ProdDataConstr, pos)
            case (_: ProdUplcConstr, PairIntDataList) =>
                input.toRepresentation(ProdDataList, pos).toRepresentation(PairIntDataList, pos)
            case (inPuc: ProdUplcConstr, outPuc: ProdUplcConstr) =>
                if inPuc == outPuc then input
                else {
                    // Different field reprs — destructure via Case, convert each field
                    // individually, and rebuild Constr. Without this, ProdUplcConstr→ProdUplcConstr
                    // was a silent no-op even when fields needed UnIData/IData conversions
                    // (e.g. mapping `[Fixed, …]` to `[Constant, …]` for default-repr consumers).
                    val constrDecl = retrieveConstrDecl(input.sirType, pos)
                    val fieldNames =
                        constrDecl.params.indices.map(i => lctx.uniqueVarName(s"_uc_conv_f$i"))
                    val fieldTypes =
                        constrDecl.params.map(p => lctx.resolveTypeVarIfNeeded(p.tp))
                    val fieldVars =
                        fieldNames.zip(fieldTypes).zip(inPuc.fieldReprs).map {
                            case ((name, tp), repr) =>
                                new VariableLoweredValue(
                                  id = name,
                                  name = name,
                                  sir = SIR.Var(name, tp, AnnotationsDecl(pos)),
                                  representation = repr
                                )
                        }
                    val convertedFields =
                        fieldVars.zip(outPuc.fieldReprs).zip(fieldTypes).map {
                            case ((fv, targetRepr), fieldType) =>
                                fv.maybeUpcast(fieldType, pos).toRepresentation(targetRepr, pos)
                        }
                    val inPos = pos
                    new ComplexLoweredValue(
                      fieldVars.toSet,
                      (input +: convertedFields)*
                    ) {
                        override def sirType = input.sirType
                        override def representation: LoweredValueRepresentation = outPuc
                        override def pos = inPos
                        override def termInternal(gctx: TermGenerationContext) = {
                            val innerCtx =
                                gctx.copy(generatedVars = gctx.generatedVars ++ fieldVars.map(_.id))
                            val body = Term.Constr(
                              scalus.cardano.ledger.Word64(outPuc.tag.toLong),
                              convertedFields.map(_.termWithNeededVars(innerCtx)).toList,
                              UplcAnnotation(inPos)
                            )
                            val branch = fieldVars.foldRight(body: Term) { (fv, inner) =>
                                Term.LamAbs(fv.id, inner, UplcAnnotation(inPos))
                            }
                            val errorBranches =
                                scala.List.fill(inPuc.tag)(Term.Error(UplcAnnotation(inPos)))
                            Term.Case(
                              input.termWithNeededVars(gctx),
                              errorBranches :+ branch,
                              UplcAnnotation(inPos)
                            )
                        }
                        override def docDef(ctx: LoweredValue.PrettyPrintingContext) =
                            Doc.text("UplcConstr→UplcConstr(") + input.docRef(ctx) + Doc.text(")")
                        override def docRef(ctx: LoweredValue.PrettyPrintingContext) = docDef(ctx)
                    }
                }
            case (OneElementWrapper(_), _) =>
                // in theory never bin here, but let's delegate
                val generator = lctx.typeGenerator(input.sirType)
                generator match
                    case oneElement: OneElementWrapperEmitter =>
                        oneElement.emitConvert(input, representation, pos)
                    case _ =>
                        throw LoweringException(
                          s"Can't use one-element=generator for type ${input.sirType.show}",
                          pos
                        )
            case (ProdBuiltinPair(inFst, inSnd), outPair @ ProdBuiltinPair(outFst, outSnd)) =>
                if inFst == outFst && inSnd == outSnd then input
                else
                    // Convert component-wise: extract fst/snd in input reprs, convert to output reprs, rebuild
                    val (fstType, sndType) =
                        ProdBuiltinPair.extractPairComponentTypes(input.sirType)
                    val (inputIdv, inputIdvAdded) =
                        lvAsIdentifiable(
                          input,
                          "pair_conv_input",
                          input.sirType,
                          input.representation,
                          pos
                        )
                    val fst = lvBuiltinApply(SIRBuiltins.fstPair, inputIdv, fstType, inFst, pos)
                    val snd = lvBuiltinApply(SIRBuiltins.sndPair, inputIdv, sndType, inSnd, pos)
                    val convertedFst = fst.toRepresentation(outFst, pos)
                    val convertedSnd = snd.toRepresentation(outSnd, pos)
                    val body = lvBuiltinApply2(
                      SIRBuiltins.mkPairData,
                      convertedFst,
                      convertedSnd,
                      input.sirType,
                      outPair,
                      pos
                    )
                    if inputIdvAdded then ScopeBracketsLoweredValue(Set(inputIdv), body)
                    else body
            case (ProdBuiltinPair(inFst, inSnd), ProdDataList) =>
                val (fstType, sndType) =
                    ProdBuiltinPair.extractPairComponentTypes(input.sirType)
                val (inputIdv, _) =
                    lvAsIdentifiable(
                      input,
                      "pair_to_dl_input",
                      input.sirType,
                      input.representation,
                      pos
                    )
                if lctx.targetProtocolVersion >= MajorProtocolVersion.vanRossemPV then {
                    // For PlutusV4: use Case on Pair
                    val frsVarId = lctx.uniqueVarName("pair_frs")
                    val frsVar = new VariableLoweredValue(
                      id = frsVarId,
                      name = frsVarId,
                      sir = SIR.Var(frsVarId, fstType, AnnotationsDecl.empty),
                      representation = inFst,
                      optRhs = None
                    )
                    val sndVarId = lctx.uniqueVarName("pair_snd")
                    val sndVar = new VariableLoweredValue(
                      id = sndVarId,
                      name = sndVarId,
                      sir = SIR.Var(sndVarId, sndType, AnnotationsDecl.empty),
                      representation = inSnd,
                      optRhs = None
                    )
                    val frsDataRepr = SirTypeUplcGenerator.defaultDataRepresentation(fstType)
                    val sndDataRepr = SirTypeUplcGenerator.defaultDataRepresentation(sndType)
                    val frsData = frsVar.toRepresentation(frsDataRepr, pos)
                    val sndData = sndVar.toRepresentation(sndDataRepr, pos)
                    val consSndNil = lvBuiltinApply2(
                      SIRBuiltins.mkCons,
                      sndData,
                      lvProdDataListNil(pos),
                      SIRType.List(SIRType.Data.tp),
                      ProdDataList,
                      pos
                    )
                    val body = lvBuiltinApply2(
                      SIRBuiltins.mkCons,
                      frsData,
                      consSndNil,
                      input.sirType,
                      ProdDataList,
                      pos
                    )
                    lvCasePair(inputIdv, frsVar, sndVar, body, pos)
                } else {
                    // For V1-V3: use fstPair/sndPair builtins
                    val frs = lvBuiltinApply(SIRBuiltins.fstPair, inputIdv, fstType, inFst, pos)
                    val snd = lvBuiltinApply(SIRBuiltins.sndPair, inputIdv, sndType, inSnd, pos)
                    val frsDataRepr = SirTypeUplcGenerator.defaultDataRepresentation(fstType)
                    val sndDataRepr = SirTypeUplcGenerator.defaultDataRepresentation(sndType)
                    val frsData = frs.toRepresentation(frsDataRepr, pos)
                    val sndData = snd.toRepresentation(sndDataRepr, pos)
                    val consSndNil = lvBuiltinApply2(
                      SIRBuiltins.mkCons,
                      sndData,
                      lvProdDataListNil(pos),
                      SIRType.List(SIRType.Data.tp),
                      ProdDataList,
                      pos
                    )
                    val retval = lvBuiltinApply2(
                      SIRBuiltins.mkCons,
                      frsData,
                      consSndNil,
                      input.sirType,
                      ProdDataList,
                      pos
                    )
                    retval
                }
            case (_: ProdBuiltinPair, tvr: TypeVarRepresentation) =>
                import SIRType.TypeVarKind.*
                tvr.kind match
                    case Transparent =>
                        RepresentationProxyLoweredValue(input, representation, pos)
                    case Unwrapped =>
                        val targetUnderlying = defaultRepresentation(input.sirType)
                        input
                            .toRepresentation(targetUnderlying, pos)
                            .toRepresentation(representation, pos)
                    case Fixed =>
                        val typeVarRepr = lctx
                            .typeGenerator(input.sirType)
                            .defaultTypeVarReperesentation(input.sirType)
                        input
                            .toRepresentation(typeVarRepr, pos)
                            .toRepresentation(representation, pos)
            case (_: ProdBuiltinPair, _) =>
                input
                    .toRepresentation(ProdDataList, pos)
                    .toRepresentation(representation, pos)
            case (_: TypeVarRepresentation, ProdDataConstr) =>
                RepresentationProxyLoweredValue(input, representation, pos)
            case (tvr: TypeVarRepresentation, _) =>
                import SIRType.TypeVarKind.*
                tvr.kind match
                    case Transparent =>
                        // Transparent passthrough: bytes came in under a TypeVar whose concrete
                        // shape is determined by the caller's substitution. We deliberately do
                        // NOT relabel as `representation` — relabeling Data-encoded bytes as
                        // native UplcConstr (or vice versa) produces silent miscompilations
                        // downstream (see the KnightsTest:475 heisenbug). Callers that ask for a
                        // concrete product repr against a Transparent TypeVar input must arrange
                        // the substitution at the dispatcher boundary (typeVarReprEnv) before
                        // this point. Returning `input` unchanged is safe for the only cases that
                        // reach this branch (HO-lambda body lowering inside intrinsics).
                        input
                    case Unwrapped =>
                        val sourceUnderlying = defaultRepresentation(input.sirType)
                        new RepresentationProxyLoweredValue(input, sourceUnderlying, pos)
                            .toRepresentation(representation, pos)
                    case Fixed =>
                        val inputDataConstr = input.toRepresentation(ProdDataConstr, pos)
                        inputDataConstr.toRepresentation(representation, pos)
            case (_, tvr: TypeVarRepresentation) =>
                import SIRType.TypeVarKind.*
                tvr.kind match
                    case Transparent => input
                    case Unwrapped   =>
                        // Need bytes in the type's actual defaultRepresentation form before
                        // relabeling. Dispatch via the type's own generator (e.g. for a
                        // @UplcRepr(UplcConstr) Point, the default is ProdUplcConstr, NOT
                        // this generator's Data-backed ProdDataList).
                        val targetUnderlying = lctx
                            .typeGenerator(input.sirType)
                            .defaultRepresentation(input.sirType)
                        val converted = input.toRepresentation(targetUnderlying, pos)
                        new RepresentationProxyLoweredValue(converted, tvr, pos)
                    case Fixed => emitConvert(input, ProdDataConstr, pos)
            case _ =>
                throw LoweringException(
                  s"Unsupported conversion for ${input.sirType.show} from ${input.representation} to $representation",
                  pos
                )
        }
    }

}
