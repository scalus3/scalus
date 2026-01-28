package scalus.compiler.sir.lowering
package typegens

import org.typelevel.paiges.Doc
import scalus.cardano.ledger.Language
import scalus.compiler.sir.lowering.LoweredValue.Builder.*
import scalus.compiler.sir.lowering.ProductCaseClassRepresentation.*
import scalus.compiler.sir.lowering.SumCaseClassRepresentation.SumDataList
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
                    // don't change: builtin functions rely on this
                    ProductCaseClassRepresentation.PairData
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

    override def isDataSupported(tp: SIRType)(using loweringContext: LoweringContext): Boolean = {
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
            case (ProdDataList, UplcConstr) =>
                ???
            case (ProdDataList, outRep @ ProductCaseClassRepresentation.OneElementWrapper(_)) =>
                lvBuiltinApply(SIRBuiltins.headList, input, input.sirType, outRep, pos)
            case (ProdDataList, PairData) =>
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
                  ProductCaseClassRepresentation.PairData,
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
                  ProductCaseClassRepresentation.ProdDataList,
                  pos
                )
                val body = lvBuiltinApply2(
                  SIRBuiltins.mkPairData,
                  head,
                  headTailHead,
                  input.sirType,
                  ProductCaseClassRepresentation.PairData,
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
            case (PackedDataList, UplcConstr) =>
                input
                    .toRepresentation(ProdDataList, pos)
                    .toRepresentation(UplcConstr, pos)
            case (
                  PackedDataList,
                  outputRep @ ProductCaseClassRepresentation.OneElementWrapper(_)
                ) =>
                input
                    .toRepresentation(ProdDataList, pos)
                    .toRepresentation(outputRep, pos)
            case (PackedDataList, PairData) =>
                input
                    .toRepresentation(ProdDataList, pos)
                    .toRepresentation(
                      PairData,
                      pos
                    )
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
            case (ProdDataConstr, UplcConstr) =>
                input
                    .toRepresentation(ProdDataList, pos)
                    .toRepresentation(UplcConstr, pos)
            case (ProdDataConstr, PairData) =>
                input
                    .toRepresentation(ProdDataList, pos)
                    .toRepresentation(PairData, pos)
            case (UplcConstr, ProdDataList) => ???
            case (UplcConstr, PackedDataList) =>
                input
                    .toRepresentation(ProdDataList, pos)
                    .toRepresentation(PackedDataList, pos)
            case (UplcConstr, ProdDataConstr) =>
                input
                    .toRepresentation(ProdDataList, pos)
                    .toRepresentation(ProdDataConstr, pos)
            case (UplcConstr, PairIntDataList) =>
                ???
            case (UplcConstr, UplcConstr) =>
                input
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
            case (PairData, PairData) => input
            case (PairData, ProdDataList) =>
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
                if lctx.targetLanguage == Language.PlutusV4 then {
                    // For PlutusV4: use Case on Pair
                    val frsVarId = lctx.uniqueVarName("pair_frs")
                    val frsVar = new VariableLoweredValue(
                      id = frsVarId,
                      name = frsVarId,
                      sir = SIR.Var(frsVarId, SIRType.Data.tp, AnnotationsDecl.empty),
                      representation = PrimitiveRepresentation.PackedData,
                      optRhs = None
                    )
                    val sndVarId = lctx.uniqueVarName("pair_snd")
                    val sndVar = new VariableLoweredValue(
                      id = sndVarId,
                      name = sndVarId,
                      sir = SIR.Var(sndVarId, SIRType.Data.tp, AnnotationsDecl.empty),
                      representation = PrimitiveRepresentation.PackedData,
                      optRhs = None
                    )
                    val consSndNil = lvBuiltinApply2(
                      SIRBuiltins.mkCons,
                      sndVar,
                      lvDataNil(pos),
                      SIRType.List(SIRType.Data.tp),
                      ProductCaseClassRepresentation.ProdDataList,
                      pos
                    )
                    val body = lvBuiltinApply2(
                      SIRBuiltins.mkCons,
                      frsVar,
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
                      SIRType.Data.tp,
                      PrimitiveRepresentation.PackedData,
                      pos
                    )
                    val snd = lvBuiltinApply(
                      SIRBuiltins.sndPair,
                      inputIdv,
                      SIRType.Data.tp,
                      PrimitiveRepresentation.PackedData,
                      pos
                    )
                    val consSndNil =
                        lvBuiltinApply2(
                          SIRBuiltins.mkCons,
                          snd,
                          lvDataNil(pos),
                          SIRType.List(SIRType.Data.tp),
                          ProductCaseClassRepresentation.ProdDataList,
                          pos
                        )
                    val retval = lvBuiltinApply2(
                      SIRBuiltins.mkCons,
                      frs,
                      consSndNil,
                      input.sirType,
                      ProductCaseClassRepresentation.ProdDataList,
                      pos
                    )
                    retval
                }
            case (PairData, TypeVarRepresentation(isBuiltin)) =>
                if isBuiltin then RepresentationProxyLoweredValue(input, representation, pos)
                else
                    input
                        .toRepresentation(ProductCaseClassRepresentation.ProdDataConstr, pos)
                        .toRepresentation(representation, pos)
            case (PairData, _) =>
                input
                    .toRepresentation(ProdDataList, pos)
                    .toRepresentation(representation, pos)
            case (
                  TypeVarRepresentation(isBuiltin),
                  ProductCaseClassRepresentation.ProdDataConstr
                ) =>
                RepresentationProxyLoweredValue(input, representation, pos)
            case (TypeVarRepresentation(isBuiltin), _) =>
                if isBuiltin then RepresentationProxyLoweredValue(input, representation, pos)
                else {
                    val inputDataConstr =
                        input.toRepresentation(ProductCaseClassRepresentation.ProdDataConstr, pos)
                    val output = inputDataConstr.toRepresentation(representation, pos)
                    output
                }
            case (_, TypeVarRepresentation(isBuiltin)) =>
                if isBuiltin then input else toRepresentation(input, ProdDataConstr, pos)
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
        val targetTypeGenerator = lctx.typeGenerator(targetType)
        targetTypeGenerator.defaultRepresentation(targetType) match {
            case SumCaseClassRepresentation.SumDataList =>
                // we are constr or nill
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
                      SumCaseClassRepresentation.SumDataList,
                      pos
                    )
                else
                    throw LoweringException(
                      s"Unkonow constructor name for data-list: ${constrDecl.name}",
                      pos
                    )
            case other =>
                // all other types should be convertible to data-constr
                val asDataConstr = input.toRepresentation(
                  ProductCaseClassRepresentation.ProdDataConstr,
                  pos
                )
                val retval = new TypeRepresentationProxyLoweredValue(
                  asDataConstr,
                  targetType,
                  SumCaseClassRepresentation.DataConstr,
                  pos
                )
                retval
        }
    }

    override def genConstr(constr: SIR.Constr)(using
        lctx: LoweringContext
    ): LoweredValue = {
        val loweredArgs = constr.args.map(arg => lctx.lower(arg))
        val argTypeGens = loweredArgs.map(_.sirType).map(lctx.typeGenerator)
        val isDataSupported = loweredArgs.zip(argTypeGens).forall { case (arg, typeGen) =>
            typeGen.isDataSupported(arg.sirType)
        }
        if !isDataSupported then {
            val notSupportedData = loweredArgs.zip(argTypeGens).filterNot { case (arg, typeGen) =>
                typeGen.isDataSupported(arg.sirType)
            }
            val firstNotSupported = notSupportedData.head._1
            throw LoweringException(
              s"Sorry, data representation is not supported for ${firstNotSupported.sirType.show}",
              firstNotSupported.pos
            )
            genConstrUplcConstr(constr)
        } else
            // check majority
            val nDataCentric = loweredArgs.count(_.representation.isDataCentric)
            if nDataCentric >= loweredArgs.size / 2 then
                genConstrDataConstr(constr, loweredArgs, argTypeGens)
            else
                // also check data becoud genContrUplcConstr is not implemented yet
                genConstrDataConstr(constr, loweredArgs, argTypeGens)
                // genConstrUplcConstr(constr, loweredArgs, argTypeGens)
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
                    SumDataList,
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
            if lctx.targetLanguage == Language.PlutusV4 && fieldIndex >= 2 then
                // dropList(fieldIndex, list) is more efficient for n >= 2
                val droppedId = list0id + s"_drop_$fieldIndex"
                val droppedList = lctx.scope.getById(droppedId) match
                    case Some(v) => v
                    case None =>
                        lvNewLazyIdVar(
                          droppedId,
                          SIRType.List(SIRType.Data.tp),
                          SumDataList,
                          lvBuiltinApply2(
                            SIRBuiltins.dropList,
                            lvIntConstant(fieldIndex, sel.anns.pos),
                            list0,
                            SIRType.List(SIRType.Data.tp),
                            SumDataList,
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
                              SumDataList,
                              lvBuiltinApply(
                                SIRBuiltins.tailList,
                                acc._1,
                                SIRType.List(SIRType.Data.tp),
                                SumDataList,
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
            case PairData =>
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
                          SumCaseClassRepresentation.SumDataList,
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
                    SumCaseClassRepresentation.SumDataList,
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

        if lctx.targetLanguage == Language.PlutusV4 then {
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
        // TODO: check UplcConstrOnData, it can be more efficient
        val s0 = lvBuiltinApply0(
          SIRBuiltins.mkNilData,
          SIRType.List(SIRType.Data.tp),
          SumDataList,
          constr.anns.pos
        )
        val dataList = dataRepresentations.foldRight(s0) { (arg, acc) =>
            lvBuiltinApply2(
              SIRBuiltins.mkCons,
              arg,
              acc,
              SIRType.List(SIRType.Data.tp),
              SumDataList,
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

    def genConstrUplcConstr(constr: SIR.Constr)(using
        lctx: LoweringContext
    ): LoweredValue = {
        ???
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
