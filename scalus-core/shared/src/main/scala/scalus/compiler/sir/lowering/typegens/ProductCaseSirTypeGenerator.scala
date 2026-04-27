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
    )(using lctx: LoweringContext): LoweredValue = {
        (input.representation, representation) match {
            case (ProdDataList, ProdDataList) => input
            case (ProdDataList, PackedDataList) =>
                lvBuiltinApply(
                  SIRBuiltins.listData,
                  input,
                  input.sirType,
                  ProductCaseClassRepresentation.PackedDataList,
                  pos
                )
            case (ProdDataList, ProdDataConstr) =>
                val constrIndex = retrieveConstrIndex(input.sirType, pos)
                lvBuiltinApply2(
                  SIRBuiltins.constrData,
                  lvIntConstant(constrIndex, pos),
                  input,
                  input.sirType,
                  ProductCaseClassRepresentation.ProdDataConstr,
                  pos
                )
            case (ProdDataList, PairIntDataList) =>
                toRepresentation(input, ProdDataConstr, pos).toRepresentation(PairIntDataList, pos)
            case (ProdDataList, puc: ProdUplcConstr) =>
                // ProdDataList → ProdUplcConstr: extract each field via headList/tailList,
                // convert from Data to native repr, build Term.Constr(tag, [fields])
                val constrDecl = retrieveConstrDecl(input.sirType, pos)
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
            case (ProdDataList, outRep @ ProductCaseClassRepresentation.OneElementWrapper(_)) =>
                lvBuiltinApply(SIRBuiltins.headList, input, input.sirType, outRep, pos)
            case (
                  ProdDataList,
                  outPair @ ProductCaseClassRepresentation.ProdBuiltinPair(fstRepr, sndRepr)
                ) =>
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
                val head = lvBuiltinApply(
                  SIRBuiltins.headList,
                  inputIdv,
                  input.sirType,
                  fstRepr,
                  pos
                )
                val headTail = lvBuiltinApply(
                  SIRBuiltins.tailList,
                  inputIdv,
                  SIRType.List(SIRType.Data.tp),
                  ProductCaseClassRepresentation.ProdDataList,
                  pos
                )
                val headTailHead = lvBuiltinApply(
                  SIRBuiltins.headList,
                  headTail,
                  SIRType.Data.tp,
                  sndRepr,
                  pos
                )
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
                lvBuiltinApply(
                  SIRBuiltins.unListData,
                  input,
                  input.sirType,
                  ProductCaseClassRepresentation.ProdDataList,
                  pos
                )
            case (PackedDataList, PackedDataList) =>
                input
            case (PackedDataList, puc: ProdUplcConstr) =>
                input
                    .toRepresentation(ProdDataList, pos)
                    .toRepresentation(puc, pos)
            case (
                  PackedDataList,
                  outputRep @ ProductCaseClassRepresentation.OneElementWrapper(_)
                ) =>
                input
                    .toRepresentation(ProdDataList, pos)
                    .toRepresentation(outputRep, pos)
            case (PackedDataList, outPair: ProductCaseClassRepresentation.ProdBuiltinPair) =>
                input
                    .toRepresentation(ProdDataList, pos)
                    .toRepresentation(outPair, pos)
            case (ProdDataConstr, ProdDataList) =>
                val pairIntDataList = lvBuiltinApply(
                  SIRBuiltins.unConstrData,
                  input,
                  SIRType.BuiltinPair(SIRType.Integer, SIRType.Data.tp),
                  ProductCaseClassRepresentation.PairIntDataList,
                  pos
                )
                lvBuiltinApply(
                  SIRBuiltins.sndPair,
                  pairIntDataList,
                  input.sirType,
                  ProductCaseClassRepresentation.ProdDataList,
                  pos
                )
            case (ProdDataConstr, PackedDataList) =>
                input
                    .toRepresentation(ProdDataList, pos)
                    .toRepresentation(PackedDataList, pos)
            case (ProdDataConstr, ProdDataConstr) =>
                input
            case (ProdDataConstr, PairIntDataList) =>
                lvBuiltinApply(
                  SIRBuiltins.unConstrData,
                  input,
                  SIRType.BuiltinPair(SIRType.Integer, SIRType.Data.tp),
                  ProductCaseClassRepresentation.PairIntDataList,
                  pos
                )
            case (ProdDataConstr, puc: ProdUplcConstr) =>
                input
                    .toRepresentation(ProdDataList, pos)
                    .toRepresentation(puc, pos)
            case (ProdDataConstr, outPair: ProductCaseClassRepresentation.ProdBuiltinPair) =>
                input
                    .toRepresentation(ProdDataList, pos)
                    .toRepresentation(outPair, pos)
            case (puc: ProdUplcConstr, ProdDataList) =>
                // ProdUplcConstr → ProdDataList: Case(input, [λf0.λf1...λfN. mkCons(toData(f0), ...)])
                val constrDecl = retrieveConstrDecl(input.sirType, pos)
                val fieldNames = constrDecl.params.indices.map(i => lctx.uniqueVarName(s"_uc_f$i"))
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
                          SumCaseClassRepresentation
                              .SumBuiltinList(SumCaseClassRepresentation.DataData),
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
                input
                    .toRepresentation(ProdDataList, pos)
                    .toRepresentation(PackedDataList, pos)
            case (_: ProdUplcConstr, ProdDataConstr) =>
                input
                    .toRepresentation(ProdDataList, pos)
                    .toRepresentation(ProdDataConstr, pos)
            case (_: ProdUplcConstr, PairIntDataList) =>
                // ProdUplcConstr → PairIntDataList: go through ProdDataList first
                input
                    .toRepresentation(ProdDataList, pos)
                    .toRepresentation(PairIntDataList, pos)
            case (inPuc: ProdUplcConstr, outPuc: ProdUplcConstr) =>
                // TODO: stucturall isComatible.  In general we have ProdUplcConstrSirTypeGeneratoe, check code dulication
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
                            Doc.text("UplcConstr→UplcConstr(") + input.docRef(ctx) +
                                Doc.text(")")
                        override def docRef(ctx: LoweredValue.PrettyPrintingContext) =
                            docDef(ctx)
                    }
                }
            case (
                  ProductCaseClassRepresentation.OneElementWrapper(internalInputRep),
                  _
                ) =>
                // in theory never bin here, but let's delegate
                val generator = lctx.typeGenerator(input.sirType)
                if generator.isInstanceOf[ProductCaseOneElementSirTypeGenerator] then
                    // delegate this to the generator
                    generator.toRepresentation(input, representation, pos)
                else
                    throw LoweringException(
                      s"Can't use one-element=generator for type ${input.sirType.show}",
                      pos
                    )
            case (
                  ProductCaseClassRepresentation.ProdBuiltinPair(inFst, inSnd),
                  outPair @ ProductCaseClassRepresentation.ProdBuiltinPair(outFst, outSnd)
                ) =>
                if inFst == outFst && inSnd == outSnd then input
                else
                    // Convert component-wise: extract fst/snd in input reprs, convert to output reprs, rebuild
                    val (fstType, sndType) =
                        ProductCaseClassRepresentation.ProdBuiltinPair
                            .extractPairComponentTypes(input.sirType)
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
            case (ProductCaseClassRepresentation.ProdBuiltinPair(inFst, inSnd), ProdDataList) =>
                val (fstType, sndType) =
                    ProductCaseClassRepresentation.ProdBuiltinPair.extractPairComponentTypes(
                      input.sirType
                    )
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
                      ProductCaseClassRepresentation.ProdDataList,
                      pos
                    )
                    val body = lvBuiltinApply2(
                      SIRBuiltins.mkCons,
                      frsData,
                      consSndNil,
                      input.sirType,
                      ProductCaseClassRepresentation.ProdDataList,
                      pos
                    )
                    lvCasePair(inputIdv, frsVar, sndVar, body, pos)
                } else {
                    // For V1-V3: use fstPair/sndPair builtins
                    val frs = lvBuiltinApply(
                      SIRBuiltins.fstPair,
                      inputIdv,
                      fstType,
                      inFst,
                      pos
                    )
                    val snd = lvBuiltinApply(
                      SIRBuiltins.sndPair,
                      inputIdv,
                      sndType,
                      inSnd,
                      pos
                    )
                    val frsDataRepr = lctx.typeGenerator(fstType).defaultDataRepresentation(fstType)
                    val sndDataRepr = lctx.typeGenerator(sndType).defaultDataRepresentation(sndType)
                    val frsData = frs.toRepresentation(frsDataRepr, pos)
                    val sndData = snd.toRepresentation(sndDataRepr, pos)
                    val consSndNil =
                        lvBuiltinApply2(
                          SIRBuiltins.mkCons,
                          sndData,
                          lvProdDataListNil(pos),
                          SIRType.List(SIRType.Data.tp),
                          ProductCaseClassRepresentation.ProdDataList,
                          pos
                        )
                    val retval = lvBuiltinApply2(
                      SIRBuiltins.mkCons,
                      frsData,
                      consSndNil,
                      input.sirType,
                      ProductCaseClassRepresentation.ProdDataList,
                      pos
                    )
                    retval
                }
            case (
                  _: ProductCaseClassRepresentation.ProdBuiltinPair,
                  tvr: TypeVarRepresentation
                ) =>
                import SIRType.TypeVarKind.*
                tvr.kind match
                    case Transparent =>
                        RepresentationProxyLoweredValue(input, representation, pos)
                    case Unwrapped =>
                        // Underlying form for Unwrapped target = defaultRepresentation
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
            case (_: ProductCaseClassRepresentation.ProdBuiltinPair, _) =>
                input
                    .toRepresentation(ProdDataList, pos)
                    .toRepresentation(representation, pos)
            case (
                  _: TypeVarRepresentation,
                  ProductCaseClassRepresentation.ProdDataConstr
                ) =>
                RepresentationProxyLoweredValue(input, representation, pos)
            case (tvr: TypeVarRepresentation, _) =>
                import SIRType.TypeVarKind.*
                tvr.kind match
                    case Transparent =>
                        RepresentationProxyLoweredValue(input, representation, pos)
                    case Unwrapped =>
                        // Source bytes are in defaultRepresentation form for input.sirType.
                        val sourceUnderlying = defaultRepresentation(input.sirType)
                        new RepresentationProxyLoweredValue(input, sourceUnderlying, pos)
                            .toRepresentation(representation, pos)
                    case Fixed =>
                        val inputDataConstr =
                            input.toRepresentation(
                              ProductCaseClassRepresentation.ProdDataConstr,
                              pos
                            )
                        val output = inputDataConstr.toRepresentation(representation, pos)
                        output
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
                    case Fixed => toRepresentation(input, ProdDataConstr, pos)
            case _ =>
                throw LoweringException(
                  s"Unsupported conversion for ${input.sirType.show} from ${input.representation} to $representation",
                  pos
                )
        }
    }

    override def upcastOne(input: LoweredValue, targetType: SIRType, pos: SIRPosition)(using
        lctx: LoweringContext
    ): LoweredValue = {
        // INPUT-repr-first dispatch: if input is already `ProdUplcConstr`, its UPLC bytes
        // are already `Constr(tag, fields)` — exactly the shape of a `SumUplcConstr` value.
        // Upcasting to the parent sum is a zero-cost relabel: build target's SumUplcConstr
        // using the TYPE's defaults for the OTHER variants, but keep the input's actual
        // ProdUplcConstr entry at its own tag. This preserves field-repr refinements the
        // input carries (e.g. concrete Unwrapped reprs that may differ from the type's
        // abstract defaults) and avoids Data round-trips that would fail for abstract
        // TypeVar fields in isolation.
        input.representation match
            case puc: ProductCaseClassRepresentation.ProdUplcConstr =>
                val baseSum =
                    typegens.SumUplcConstrSirTypeGenerator.buildSumUplcConstr(targetType)
                val targetSum = baseSum match
                    case SumCaseClassRepresentation.SumUplcConstr(variants) =>
                        SumCaseClassRepresentation.SumUplcConstr(variants.updated(puc.tag, puc))
                    case other => other
                return new TypeRepresentationProxyLoweredValue(
                  input,
                  targetType,
                  targetSum,
                  pos
                )
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
                if constrDecl.name == "scalus.cardano.onchain.plutus.prelude.List$.Cons" || constrDecl.name == "scalus.cardano.onchain.plutus.prelude.List$.Nil"
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
        // Context-driven delegation: parent's default repr is SumUplcConstr, target type
        // is annotated @UplcConstr, tail repr is SumUplcConstr (Cons), or
        // inUplcConstrListScope. See ConstrDispatcher.shouldDelegateToUplcConstr for the
        // full set of conditions.
        ConstrDispatcher.shouldDelegateToUplcConstr(constr, loweredArgs, optTargetType) match
            case Some(other) if other ne this =>
                other.genConstrLowered(constr, loweredArgs, optTargetType)
            case _ =>
                // Cascade to UplcConstr when any argument has Transparent TypeVar repr
                // (can't be serialized to Data)
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
        loweredScrutinee.representation match
            case ProdDataList | PackedDataList | ProdDataConstr | PairIntDataList =>
                genMatchDataList(matchData, loweredScrutinee, optTargetType)
            case _: ProductCaseClassRepresentation.ProdBuiltinPair =>
                genMatchPairData(matchData, loweredScrutinee, optTargetType)
            case _ =>
                genMatchDataList(matchData, loweredScrutinee, optTargetType)
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
                SumCaseSirTypeGenerator.genMatchDataConstrCase(
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

    class MatchPairDataLoweredValue(
        frs: IdentifiableLoweredValue,
        snd: IdentifiableLoweredValue,
        scrutinee: IdentifiableLoweredValue,
        addScrutineeToScope: Boolean,
        body: LoweredValue,
        inPos: SIRPosition
    ) extends ComplexLoweredValue(
          Set(frs, snd) ++ (if addScrutineeToScope then Set(scrutinee) else Set.empty),
          scrutinee,
          body
        ) {

        override def sirType: SIRType = body.sirType

        override def representation: LoweredValueRepresentation = body.representation

        override def pos: SIRPosition = inPos

        override def termInternal(gctx: TermGenerationContext): Term = {
            body.termWithNeededVars(gctx)
        }

        override def docDef(ctx: LoweredValue.PrettyPrintingContext): Doc = {
            val left = Doc.text("MatchPairData(")
            val right = Doc.text(")")
            body.docRef(ctx).bracketBy(left, right)
        }

    }

    def genMatchPairData(
        matchData: SIR.Match,
        loweredScrutinee: LoweredValue,
        optTargetType: Option[SIRType]
    )(using
        lctx: LoweringContext
    ): LoweredValue = {
        val (typeParams, constrDecl, typeArgs) = SIRType
            .collectProd(loweredScrutinee.sirType)
            .getOrElse(
              throw LoweringException(
                s"Expected product type, got ${loweredScrutinee.sirType.show}",
                matchData.anns.pos
              )
            )
        // val constrDecl = retrieveConstrDecl(loweredScrutinee.sirType, matchData.anns.pos)
        val myCase = selectMatchCase(matchData, loweredScrutinee, constrDecl)
        val prevScope = lctx.scope
        val (matchVal, addMatchValToScope) = loweredScrutinee match
            case idv: IdentifiableLoweredValue =>
                (idv, false)
            case other =>
                (
                  lvNewLazyIdVar(
                    lctx.uniqueVarName("match_pair_data"),
                    SIRType.List(SIRType.Data.tp),
                    SumCaseClassRepresentation.SumBuiltinList(SumCaseClassRepresentation.DataData),
                    other,
                    matchData.anns.pos
                  ),
                  true
                )
        val (frsName, sndName) = myCase.pattern match {
            case SIR.Pattern.Constr(constr, bindings, typeParamsBindinsg) =>
                (bindings.head, bindings.tail.head)
            case SIR.Pattern.Const(_) =>
                throw LoweringException(
                  s"Constant pattern not supported for product case class ${constrDecl.name}",
                  matchData.anns.pos
                )
            case SIR.Pattern.Wildcard =>
                ("_unused1", "_unused2")
        }
        val argsMapping = constrDecl.typeParams.zip(typeArgs).toMap
        val frsTp = SIRType.substitute(constrDecl.params.head.tp, argsMapping, Map.empty)
        val sndTp = SIRType.substitute(constrDecl.params.tail.head.tp, argsMapping, Map.empty)
        // val SIRType.partitionGround()
        val frsRepr = lctx.typeGenerator(frsTp).defaultDataRepresentation(frsTp)
        val sndRepr = lctx.typeGenerator(sndTp).defaultDataRepresentation(sndTp)

        if lctx.targetProtocolVersion >= MajorProtocolVersion.vanRossemPV then {
            // For PlutusV4: use Case on Pair - frs and snd are lambda parameters
            val frsVarId = lctx.uniqueVarName(frsName)
            val frsVar = new VariableLoweredValue(
              id = frsVarId,
              name = frsName,
              sir = SIR.Var(frsVarId, frsTp, AnnotationsDecl.empty),
              representation = frsRepr,
              optRhs = None // lambda parameter, not derived from builtin
            )
            val sndVarId = lctx.uniqueVarName(sndName)
            val sndVar = new VariableLoweredValue(
              id = sndVarId,
              name = sndName,
              sir = SIR.Var(sndVarId, sndTp, AnnotationsDecl.empty),
              representation = sndRepr,
              optRhs = None // lambda parameter, not derived from builtin
            )
            // Add pair element vars to scope so the body can reference them
            lctx.scope = lctx.scope.addAll(scala.collection.immutable.List(frsVar, sndVar))
            val lwBody = lctx.lower(myCase.body, optTargetType)
            lvCasePair(matchVal, frsVar, sndVar, lwBody, matchData.anns.pos)
        } else {
            // For V1-V3: use fstPair/sndPair builtins
            val frs = lvNewLazyNamedVar(
              frsName,
              frsTp,
              frsRepr,
              lvBuiltinApply(SIRBuiltins.fstPair, matchVal, frsTp, frsRepr, myCase.anns.pos),
              myCase.anns.pos
            )
            val snd = lvNewLazyNamedVar(
              sndName,
              sndTp,
              sndRepr,
              lvBuiltinApply(SIRBuiltins.sndPair, matchVal, sndTp, sndRepr, myCase.anns.pos),
              myCase.anns.pos
            )
            val lwBody = lctx.lower(myCase.body, optTargetType)
            MatchPairDataLoweredValue(
              frs,
              snd,
              matchVal,
              addMatchValToScope,
              lwBody,
              matchData.anns.pos
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

    /** Returns true if `tp` (or any unwrapped form) carries a class-level
      * `@UplcRepr` annotation in its constrDecl. Used by `genConstrUplcConstr`
      * to decide whether the field's static type pins a specific repr that
      * arg.repr must conform to.
      */
    private def hasClassLevelUplcRepr(tp: SIRType): Boolean = tp match
        case SIRType.CaseClass(decl, _, _)    => decl.annotations.data.contains("uplcRepr")
        case SIRType.SumCaseClass(decl, _)    => decl.annotations.data.contains("uplcRepr")
        case SIRType.Annotated(inner, _)      => hasClassLevelUplcRepr(inner)
        case SIRType.TypeLambda(_, body)      => hasClassLevelUplcRepr(body)
        case SIRType.TypeProxy(ref) if ref != null => hasClassLevelUplcRepr(ref)
        case _                                => false

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
