package scalus.compiler.sir.lowering

import org.typelevel.paiges.Doc
import scalus.cardano.ledger.MajorProtocolVersion
import scalus.compiler.sir.*
import scalus.compiler.sir.lowering.LoweredValue.Builder.*
import scalus.compiler.sir.lowering.typegens.*
import scalus.uplc.{Term, UplcAnnotation}

/** Dispatch layer for product-typed operations. Single entry point for Prod-side
  * `toRepresentation`; per-typegen overrides throw `dispatcherBypass` to
  * surface any caller that bypassed this object. See
  * `docs/local/claude/compiler/sum-prod-dispatch-design.md`.
  */
object ProdDispatch {

    import ProductCaseClassRepresentation.*

    /** Throw used by Prod typegens' `toRepresentation` overrides to assert that
      * dispatch goes through this object.
      */
    def dispatcherBypass(genName: String): Nothing =
        throw new IllegalStateException(
          s"$genName.toRepresentation called directly — dispatch via ProdDispatch.toRepresentation"
        )

    def toRepresentation(
        input: LoweredValue,
        target: LoweredValueRepresentation,
        pos: SIRPosition
    )(using lctx: LoweringContext): LoweredValue = {
        val gen = lctx.typeGenerator(input.sirType)
        gen match
            case ProductCaseSirTypeGenerator =>
                prodCaseImpl(input, target, pos)
            case ProductCaseUplcConstrSirTypeGenerator =>
                prodCaseUplcConstrImpl(input, target, pos)
            case ProductCaseUplcOnlySirTypeGenerator =>
                // Original ProductCaseUplcOnly body: identity → input, otherwise delegate to ProdCase.
                if input.representation == target then input
                else prodCaseImpl(input, target, pos)
            case oneElement: ProductCaseOneElementSirTypeGenerator =>
                prodCaseOneElementImpl(oneElement, input, target, pos)
            case _ =>
                gen.toRepresentation(input, target, pos)
    }

    /** Plain product-class source path: handles ProdDataList / ProdDataConstr /
      * PackedDataList / ProdUplcConstr / ProdBuiltinPair / OneElementWrapper sources
      * and delegates between them via two-hop chains where needed.
      */
    private def prodCaseImpl(
        input: LoweredValue,
        representation: LoweredValueRepresentation,
        pos: SIRPosition
    )(using lctx: LoweringContext): LoweredValue = {
        (input.representation, representation) match {
            case (ProdDataList, ProdDataList) => input
            case (ProdDataList, PackedDataList) =>
                lvBuiltinApply(SIRBuiltins.listData, input, input.sirType, PackedDataList, pos)
            case (ProdDataList, ProdDataConstr) =>
                val constrIndex = ProductCaseSirTypeGenerator.retrieveConstrIndex(input.sirType, pos)
                lvBuiltinApply2(
                  SIRBuiltins.constrData,
                  lvIntConstant(constrIndex, pos),
                  input,
                  input.sirType,
                  ProdDataConstr,
                  pos
                )
            case (ProdDataList, PairIntDataList) =>
                prodCaseImpl(input, ProdDataConstr, pos).toRepresentation(PairIntDataList, pos)
            case (ProdDataList, puc: ProdUplcConstr) =>
                // ProdDataList → ProdUplcConstr: extract each field via headList/tailList,
                // convert from Data to native repr, build Term.Constr(tag, [fields])
                val constrDecl = ProductCaseSirTypeGenerator.retrieveConstrDecl(input.sirType, pos)
                val dataListRepr =
                    SumCaseClassRepresentation.SumBuiltinList(SumCaseClassRepresentation.DataData)
                val inputIdv = input match
                    case idv: IdentifiableLoweredValue => idv
                    case other =>
                        lvNewLazyIdVar(
                          lctx.uniqueVarName("dl_to_uc"),
                          input.sirType,
                          ProdDataList,
                          other,
                          pos
                        )
                var currentList: LoweredValue = inputIdv
                val fields = constrDecl.params.zip(puc.fieldReprs).map { (param, fieldRepr) =>
                    val tp = lctx.resolveTypeVarIfNeeded(param.tp)
                    val dataRepr = lctx.typeGenerator(tp).defaultDataRepresentation(tp)
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
                if inputIdv ne input then ScopeBracketsLoweredValue(Set(inputIdv), result)
                else result
            case (ProdDataList, outRep @ OneElementWrapper(_)) =>
                lvBuiltinApply(SIRBuiltins.headList, input, input.sirType, outRep, pos)
            case (ProdDataList, outPair @ ProdBuiltinPair(fstRepr, sndRepr)) =>
                val (inputIdv, addToScoped) = input match
                    case idv: IdentifiableLoweredValue => (idv, false)
                    case other =>
                        val id = lctx.uniqueVarName("dl_to_pair_input")
                        val v = lvNewLazyIdVar(
                          id,
                          input.sirType,
                          input.representation,
                          other,
                          pos
                        )
                        (v, true)
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
                val constrDecl = ProductCaseSirTypeGenerator.retrieveConstrDecl(input.sirType, pos)
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
                        val dataRepr = lctx.typeGenerator(tp).defaultDataRepresentation(tp)
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
                    val constrDecl =
                        ProductCaseSirTypeGenerator.retrieveConstrDecl(input.sirType, pos)
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
            case (OneElementWrapper(internalInputRep), _) =>
                // in theory never bin here, but let's delegate
                val generator = lctx.typeGenerator(input.sirType)
                generator match
                    case oneElement: ProductCaseOneElementSirTypeGenerator =>
                        prodCaseOneElementImpl(oneElement, input, representation, pos)
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
                    val inputIdv = input match
                        case idv: IdentifiableLoweredValue => idv
                        case other =>
                            lvNewLazyIdVar(
                              lctx.uniqueVarName("pair_conv_input"),
                              input.sirType,
                              input.representation,
                              other,
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
                    if inputIdv ne input then ScopeBracketsLoweredValue(Set(inputIdv), body)
                    else body
            case (ProdBuiltinPair(inFst, inSnd), ProdDataList) =>
                val (fstType, sndType) =
                    ProdBuiltinPair.extractPairComponentTypes(input.sirType)
                val inputIdv = input match
                    case idv: IdentifiableLoweredValue => idv
                    case other =>
                        lvNewLazyIdVar(
                          lctx.uniqueVarName("pair_to_dl_input"),
                          input.sirType,
                          input.representation,
                          other,
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
                    val frsDataRepr = lctx.typeGenerator(fstType).defaultDataRepresentation(fstType)
                    val sndDataRepr = lctx.typeGenerator(sndType).defaultDataRepresentation(sndType)
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
                    val frsDataRepr = lctx.typeGenerator(fstType).defaultDataRepresentation(fstType)
                    val sndDataRepr = lctx.typeGenerator(sndType).defaultDataRepresentation(sndType)
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
                        val targetUnderlying =
                            ProductCaseSirTypeGenerator.defaultRepresentation(input.sirType)
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
                        val sourceUnderlying =
                            ProductCaseSirTypeGenerator.defaultRepresentation(input.sirType)
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
                    case Fixed => prodCaseImpl(input, ProdDataConstr, pos)
            case _ =>
                throw LoweringException(
                  s"Unsupported conversion for ${input.sirType.show} from ${input.representation} to $representation",
                  pos
                )
        }
    }

    /** `@UplcRepr(UplcConstr)` product source path. Resolves TypeVar inputs to the
      * appropriate concrete repr, then delegates to `prodCaseImpl`.
      */
    private def prodCaseUplcConstrImpl(
        input: LoweredValue,
        outputRepresentation: LoweredValueRepresentation,
        pos: SIRPosition
    )(using lctx: LoweringContext): LoweredValue = {
        if input.representation.isCompatibleOn(input.sirType, outputRepresentation, pos) then
            if input.representation == outputRepresentation then input
            else RepresentationProxyLoweredValue(input, outputRepresentation, pos)
        else
            val resolved = input.representation match
                case tvr: TypeVarRepresentation =>
                    import SIRType.TypeVarKind.*
                    tvr.kind match
                        case Transparent =>
                            val pucRepr =
                                ProductCaseUplcConstrSirTypeGenerator
                                    .defaultRepresentation(input.sirType)
                            RepresentationProxyLoweredValue(input, pucRepr, pos)
                        case Unwrapped =>
                            input.toRepresentation(
                              ProductCaseUplcConstrSirTypeGenerator
                                  .defaultRepresentation(input.sirType),
                              pos
                            )
                        case Fixed =>
                            input.toRepresentation(
                              ProductCaseUplcConstrSirTypeGenerator
                                  .defaultTypeVarReperesentation(input.sirType),
                              pos
                            )
                case _ => input
            prodCaseImpl(resolved, outputRepresentation, pos)
    }

    /** OneElementWrapper source path. Inner value extracted via the gen's
      * `argLoweredValue`; `gen.argGenerator` carries the inner type's default-repr
      * lookups.
      */
    private def prodCaseOneElementImpl(
        gen: ProductCaseOneElementSirTypeGenerator,
        input: LoweredValue,
        representation: LoweredValueRepresentation,
        pos: SIRPosition
    )(using lctx: LoweringContext): LoweredValue = {
        (input.representation, representation) match
            case (OneElementWrapper(argRepr), OneElementWrapper(newArgRepr)) =>
                if argRepr == newArgRepr then input
                else
                    val newArg = gen.argLoweredValue(input).toRepresentation(newArgRepr, pos)
                    new TypeRepresentationProxyLoweredValue(
                      newArg,
                      input.sirType,
                      OneElementWrapper(newArgRepr),
                      input.pos
                    )
            case (OneElementWrapper(argRepr), ProdDataList) =>
                val argInData = gen.argLoweredValue(input).toRepresentation(
                  gen.argGenerator.defaultDataRepresentation(input.sirType),
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
            case (OneElementWrapper(argRepr), ProdDataConstr) =>
                input.toRepresentation(ProdDataList, pos).toRepresentation(ProdDataConstr, pos)
            case (OneElementWrapper(argRepr), tvr: TypeVarRepresentation) =>
                import SIRType.TypeVarKind.*
                tvr.kind match
                    case Transparent =>
                        input
                    case Unwrapped | Fixed =>
                        val targetUnderlying = tvr.kind match
                            case Unwrapped =>
                                gen.argGenerator.defaultRepresentation(input.sirType)
                            case Fixed =>
                                gen.argGenerator.defaultTypeVarReperesentation(input.sirType)
                            case Transparent => argRepr // unreachable
                        val argValue = gen.argLoweredValue(input)
                        val convertedArg = argValue.toRepresentation(targetUnderlying, pos)
                        new TypeRepresentationProxyLoweredValue(
                          convertedArg,
                          input.sirType,
                          tvr,
                          pos
                        )
            case (tvr: TypeVarRepresentation, outRepr @ OneElementWrapper(argRepr)) =>
                import SIRType.TypeVarKind.*
                tvr.kind match
                    case Transparent =>
                        new RepresentationProxyLoweredValue(input, representation, pos)
                    case Unwrapped | Fixed =>
                        val argValue = gen.argLoweredValue(input)
                        val convertedArg = argValue.toRepresentation(argRepr, pos)
                        new TypeRepresentationProxyLoweredValue(
                          convertedArg,
                          input.sirType,
                          outRepr,
                          pos
                        )
            case (_, _) =>
                prodCaseImpl(input, representation, pos)
    }

    def genConstr(
        constr: SIR.Constr,
        loweredArgs: scala.List[LoweredValue],
        optTargetType: Option[SIRType]
    )(using lctx: LoweringContext): LoweredValue = {
        val gen = SumDispatch.chooseConstrOutputRepr(constr, loweredArgs, optTargetType)
        gen.genConstrLowered(constr, loweredArgs, optTargetType)
    }

    def genSelect(
        sel: SIR.Select,
        loweredScrutinee: LoweredValue
    )(using lctx: LoweringContext): LoweredValue =
        lctx.typeGenerator(loweredScrutinee.sirType).genSelect(sel, loweredScrutinee)

    /** Representation-aware dispatch for product-typed `genMatch`. Mirror of
      * `SumDispatch.genMatch`:
      *
      *   - `(Prod|Sum)UplcConstr` → tag-ordered Case via `genMatchUplcConstr`.
      *   - `ProdDataList` / `ProdDataConstr` / `PackedDataList` / `PairIntDataList`
      *     → `ProductCaseSirTypeGenerator.genMatchDataList` (Data-shape extraction
      *     via `unConstrData` + `headList`/`tailList`, or just field projection).
      *   - `ProdBuiltinPair(_, _)` → `ProdBuiltinPairEmitter.genMatch`
      *     (`Case` on Pair for V4+, `fstPair`/`sndPair` for V1-V3).
      *   - `OneElementWrapper(_)` → fall through to the per-type
      *     `ProductCaseOneElementSirTypeGenerator` instance — its `genMatch`
      *     captures argType-specific binding extraction.
      *   - `TypeVarRepresentation(_)` → relabel to the type's
      *     `defaultTypeVarRepresentation` and recurse.
      *   - everything else → fall back to the type-keyed typegen's `genMatch`.
      *
      * Pre-Phase-4c-step-2 this dispatch was inlined in
      * `ProductCaseUplcConstrSirTypeGenerator.genMatch` and
      * `ProductCaseSirTypeGenerator.genMatch`; consolidating it here mirrors
      * Phase 4a on the Sum side.
      */
    def genMatch(
        matchData: SIR.Match,
        loweredScrutinee: LoweredValue,
        optTargetType: Option[SIRType]
    )(using lctx: LoweringContext): LoweredValue = {
        loweredScrutinee.representation match
            case _: ProdUplcConstr | _: SumCaseClassRepresentation.SumUplcConstr =>
                typegens.SumUplcConstrSirTypeGenerator
                    .genMatchUplcConstr(matchData, loweredScrutinee, optTargetType)
            case ProdDataList | ProdDataConstr | PackedDataList | PairIntDataList =>
                typegens.ProductCaseSirTypeGenerator
                    .genMatchDataList(matchData, loweredScrutinee, optTargetType)
            case _: ProdBuiltinPair =>
                typegens.ProdBuiltinPairEmitter
                    .genMatch(matchData, loweredScrutinee, optTargetType)
            case _: OneElementWrapper =>
                lctx.typeGenerator(loweredScrutinee.sirType)
                    .genMatch(matchData, loweredScrutinee, optTargetType)
            case TypeVarRepresentation(_) =>
                val gen = lctx.typeGenerator(loweredScrutinee.sirType)
                val properRepresentation =
                    gen.defaultTypeVarReperesentation(loweredScrutinee.sirType)
                val scrutineeWithProperRepr = TypeRepresentationProxyLoweredValue(
                  loweredScrutinee,
                  loweredScrutinee.sirType,
                  properRepresentation,
                  matchData.anns.pos
                )
                genMatch(matchData, scrutineeWithProperRepr, optTargetType)
            case _ =>
                lctx.typeGenerator(loweredScrutinee.sirType)
                    .genMatch(matchData, loweredScrutinee, optTargetType)
    }

    def upcastOne(
        input: LoweredValue,
        targetType: SIRType,
        pos: SIRPosition
    )(using lctx: LoweringContext): LoweredValue =
        lctx.typeGenerator(input.sirType).upcastOne(input, targetType, pos)

}
