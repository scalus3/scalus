package scalus.compiler.sir.lowering
package typegens

import scalus.cardano.ledger.MajorProtocolVersion
import scalus.compiler.sir.lowering.LoweredValue.Builder.*
import scalus.compiler.sir.*

import scala.util.control.NonFatal

/** handle next cases: scalus.cardano.onchain.plutus.prelude.List[A]
  * scalus.uplc.builtin.BuiltinList[A] scalus.cardano.onchain.plutus.prelude.PairList[A, B]
  */
trait SumListEmitterCommon extends SirTypeUplcGenerator {

    def defaultListRepresentation(tp: SIRType, pos: SIRPosition)(using
        LoweringContext
    ): LoweredValueRepresentation

    def defaultElementRepresentation(tp: SIRType, pos: SIRPosition)(using
        LoweringContext
    ): LoweredValueRepresentation

    /** mkNilData and mkNilPairData
      * @param pos
      * @return
      */
    def genNil(resType: SIRType, pos: SIRPosition)(using LoweringContext): LoweredValue

    override def canBeConvertedToData(tp: SIRType)(using lctx: LoweringContext): Boolean = true

    override def toRepresentation(
        input: LoweredValue,
        outputRepresentation: LoweredValueRepresentation,
        pos: SIRPosition
    )(using lctx: LoweringContext): LoweredValue =
        SumDispatch.dispatcherBypass("SumListEmitterCommon")

    override def upcastOne(
        input: LoweredValue,
        targetType: SIRType,
        pos: SIRPosition
    )(using lctx: LoweringContext): LoweredValue = {
        val targetElementType = retrieveElementType(
          targetType,
          pos
        )
        // Preserve the input's element type when the target uses FreeUnificator.
        // subtypeSeq replaces type params with FreeUnificator (no variance tracking),
        // but losing the concrete element type breaks downstream repr conversions.
        val effectiveTargetType =
            if targetElementType == SIRType.FreeUnificator then input.sirType
            else targetType
        input.representation match {
            case SumCaseClassRepresentation.SumBuiltinList(_) |
                SumCaseClassRepresentation.SumPairBuiltinList(_, _) |
                SumCaseClassRepresentation.PackedSumDataList =>
                TypeRepresentationProxyLoweredValue(
                  input,
                  effectiveTargetType,
                  input.representation,
                  pos
                )
            case SumCaseClassRepresentation.SumDataAssocMap =>
                if SirTypeUplcGenerator.isPairOrTuple2(targetElementType) then
                    TypeRepresentationProxyLoweredValue(
                      input,
                      effectiveTargetType,
                      input.representation,
                      pos
                    )
                else
                    TypeRepresentationProxyLoweredValue(
                      input,
                      effectiveTargetType,
                      input.representation,
                      pos
                    )
            case tvr: TypeVarRepresentation =>
                val targetRepresentation = {
                    if tvr.isBuiltin then defaultRepresentation(input.sirType)
                    else this.defaultTypeVarReperesentation(input.sirType)
                }
                val alignedInput = input.toRepresentation(
                  targetRepresentation,
                  pos
                )
                upcastOne(alignedInput, effectiveTargetType, pos)
            case _: ProductCaseClassRepresentation.ProdUplcConstr =>
                // UplcConstr product → delegate to SumCaseUplcConstr upcast
                SumCaseUplcConstrSirTypeGenerator.upcastOne(input, targetType, pos)
            case _ =>
                throw LoweringException(
                  s"Unexpected representation ${input.representation.show} for List upcast from ${input.sirType.show} to ${targetType.show}",
                  pos
                )
        }
    }

    override def genConstrLowered(
        constr: SIR.Constr,
        loweredArgs: scala.List[LoweredValue],
        optTargetType: Option[SIRType]
    )(using lctx: LoweringContext): LoweredValue = {
        import SumListEmitterCommon.*
        // Routing decisions live in `SumDispatch.chooseConstrOutputRepr`.
        constr.name match
            case SIRType.List.NilConstr.name | SIRType.BuiltinList.Nil.name | PairNilName =>
                genNil(constr.tp, constr.anns.pos)
            case SIRType.List.Cons.name | SIRType.BuiltinList.Cons.name | PairConsName =>
                if loweredArgs.size != 2 then
                    throw LoweringException(
                      s"Constr construnctor with ${loweredArgs.size} args, should be 2",
                      constr.anns.pos
                    )
                val head = loweredArgs.head
                val tail = loweredArgs.tail.head
                val elementType = retrieveElementType(constr.tp, constr.anns.pos)
                val headElementUpcasted = head
                    .maybeUpcast(elementType, constr.anns.pos)
                // Propagate element repr from tail if it has a specific list repr,
                // otherwise use the default. This ensures Cons(elem, nativeList)
                // produces a native list instead of falling back to Data.
                val (effectiveElemRepr, effectiveListRepr) = tail.representation match
                    case SumCaseClassRepresentation.SumBuiltinList(tailElemRepr) =>
                        (tailElemRepr, tail.representation)
                    case _ =>
                        (
                          defaultElementRepresentation(elementType, constr.anns.pos),
                          defaultListRepresentation(constr.tp, constr.anns.pos)
                        )
                val headElementRepr =
                    try
                        headElementUpcasted.toRepresentation(
                          effectiveElemRepr,
                          constr.anns.pos
                        )
                    catch
                        case NonFatal(ex) =>
                            println(
                              s"error in genConstr for List, head.sirType: ${head.sirType.show}, tail.sirType: ${tail.sirType.show}, constr.tp=${constr.tp.show}"
                            )
                            println(s"elementType: ${elementType.show}")
                            println(
                              s"effectiveElemRepr: ${effectiveElemRepr.show}"
                            )
                            throw ex
                // special case when tail is Nil, than have type List[Nothing]
                val listRepr = effectiveListRepr
                val fixedTail =
                    if isNilType(tail.sirType) then fixNilInConstr(tail, constr.tp, listRepr)
                    else tail

                val tailElementRepr = {
                    try fixedTail.toRepresentation(listRepr, constr.anns.pos)
                    catch
                        case NonFatal(ex) =>
                            println(
                              s"error in genConstr for List, head.sirType: ${head.sirType.show}, tail.sirType: ${tail.sirType.show}, constr.tp=${constr.tp.show}"
                            )
                            println(s"relementType: ${elementType.show}")
                            println(s"defaultListRepresentation: ${listRepr.show}")
                            println(s"tail.sirType: ${tail.sirType.show}")
                            throw ex
                }
                lvBuiltinApply2(
                  SIRBuiltins.mkCons,
                  headElementRepr,
                  tailElementRepr,
                  constr.tp,
                  listRepr,
                  constr.anns.pos
                )
            case _ =>
                throw LoweringException(
                  s"Unknown constructor ${constr.name} for List",
                  constr.anns.pos
                )
    }

    def isNilType(tp: SIRType): Boolean = {
        import SumListEmitterCommon.*
        SIRType.retrieveConstrDecl(tp) match {
            case Left(r) => false
            case Right(constrDecl) =>
                constrDecl.name == SIRType.List.NilConstr.name
                || constrDecl.name == SIRType.BuiltinList.Nil.name
                || constrDecl.name == PairNilName
        }
    }

    def fixNilInConstr(
        input: LoweredValue,
        targetListType: SIRType,
        targetRepr: LoweredValueRepresentation
    )(using lctx: LoweringContext): LoweredValue = {
        if input.representation == targetRepr then input
        else if input.isConstant then genNil(targetListType, input.pos)
        else
            throw LoweringException(
              s"Implementation restriction: can't use non-standard expression of type Nil",
              input.pos
            )
    }

    override def genSelect(sel: SIR.Select, loweredScrutinee: LoweredValue)(using
        lctx: LoweringContext
    ): LoweredValue = {
        // If scrutinee has UplcConstr repr, delegate to UplcConstr select handler
        loweredScrutinee.representation match
            case _: SumCaseClassRepresentation.SumUplcConstr |
                _: SumCaseClassRepresentation.SumReprProxy =>
                return SumUplcConstrEmitter.genSelectUplcConstr(sel, loweredScrutinee)
            case _ =>
        val (scrutineeReady, listRepr, elemRepr) = loweredScrutinee.representation match
            case sbl @ SumCaseClassRepresentation.SumBuiltinList(er) =>
                (loweredScrutinee, sbl, er)
            case _ =>
                val elemType = retrieveElementType(loweredScrutinee.sirType, sel.anns.pos)
                val er = SirTypeUplcGenerator.defaultDataRepresentation(elemType)
                val lr = SumCaseClassRepresentation.SumBuiltinList(er)
                (loweredScrutinee.toRepresentation(lr, sel.anns.pos), lr, er)
        sel.field match {
            case "head" =>
                lvBuiltinApply(
                  SIRBuiltins.headList,
                  scrutineeReady,
                  sel.tp,
                  elemRepr,
                  sel.anns.pos
                )
            case "tail" =>
                lvBuiltinApply(
                  SIRBuiltins.tailList,
                  scrutineeReady,
                  sel.tp,
                  listRepr,
                  sel.anns.pos
                )
            case "isNull" =>
                // isNull is not a field, but a method, that returns true if list is empty
                lvBuiltinApply(
                  SIRBuiltins.nullList,
                  scrutineeReady,
                  SIRType.Boolean,
                  PrimitiveRepresentation.Constant,
                  sel.anns.pos
                )
            case _ =>
                throw LoweringException(
                  s"Unknown field ${sel.field} for List, which have 'head' and 'tail' fields and isNull method",
                  sel.anns.pos
                )
        }

    }

    def retrieveElementType(tp: SIRType, pos: SIRPosition)(using lctx: LoweringContext): SIRType = {
        import SumListEmitterCommon.*
        tp match {
            case SIRType.SumCaseClass(decl, typeArgs) =>
                if decl.name == PairListDataDeclName then
                    // PairList[A, B] has element type (A, B) — get it from PairCons head param
                    val pairCons = decl.constructors
                        .find(_.name == PairConsName)
                        .getOrElse(
                          throw LoweringException(
                            s"PairCons constructor not found in ${decl.name}",
                            pos
                          )
                        )
                    // Use constructor's own typeParams for substitution — they share
                    // TypeVar identity with the constructor's param types.
                    val substEnv = pairCons.typeParams.zip(typeArgs).toMap
                    SIRType.substitute(
                      pairCons.params.head.tp,
                      substEnv,
                      Map.empty
                    )
                else typeArgs.head
            case SIRType.CaseClass(constrDecl, typeArgs, optParent) =>
                if constrDecl.name == SIRType.List.NilConstr.name
                    || constrDecl.name == SIRType.BuiltinList.Nil.name
                    || constrDecl.name == PairNilName
                then SIRType.FreeUnificator
                else if constrDecl.name == SIRType.List.Cons.name
                    || constrDecl.name == SIRType.BuiltinList.Cons.name
                then typeArgs.head
                else if constrDecl.name == PairConsName then
                    // PairCons[A, B] has head: (A, B) — substitute type args
                    SIRType.substitute(
                      constrDecl.params.head.tp,
                      constrDecl.typeParams.zip(typeArgs).toMap,
                      Map.empty
                    )
                else
                    throw LoweringException(
                      s"Unknown case class ${constrDecl.name} for List",
                      pos
                    )
            case SIRType.TypeLambda(params, body) =>
                retrieveElementType(body, pos)
            case SIRType.TypeProxy(ref) =>
                retrieveElementType(ref, pos)
            case SIRType.Annotated(inner, _) =>
                // `@UplcRepr` (and other) annotations are metadata on top of the real
                // structural type; peel them before structural checks.
                retrieveElementType(inner, pos)
            case _ =>
                throw LoweringException(
                  s"Cannot retrieve element type from ${tp.show}, expected List type",
                  pos
                )
        }
    }

    def isBuiltinList(tp: SIRType): Boolean = {
        SIRType.retrieveDataDecl(tp) match {
            case Right(dataDecl) => dataDecl.name == SIRType.BuiltinList.name
            case Left(_) =>
                SIRType.retrieveConstrDecl(tp) match
                    case Right(constrDecl) =>
                        constrDecl.name == SIRType.BuiltinList.Cons.name ||
                        constrDecl.name == SIRType.BuiltinList.Nil.name
                    case Left(_) => false
        }
    }

    override def genMatch(
        matchData: SIR.Match,
        loweredScrutinee: LoweredValue,
        optTargetType: Option[SIRType]
    )(using
        lctx: LoweringContext
    ): LoweredValue = {
        // If scrutinee has UplcConstr repr, delegate to UplcConstr match handler
        loweredScrutinee.representation match
            case _: SumCaseClassRepresentation.SumUplcConstr |
                _: SumCaseClassRepresentation.SumReprProxy =>
                return SumUplcConstrEmitter.genMatchUplcConstr(
                  matchData,
                  loweredScrutinee,
                  optTargetType
                )
            case _ =>
        // Nil, Cons
        import SumListEmitterCommon.*
        var optNilCase: Option[SIR.Case] = None
        var optConsCase: Option[SIR.Case] = None
        var optWildcardCase: Option[SIR.Case] = None
        var noBindingInConsCase = false
        matchData.cases.foreach { cs =>
            cs.pattern match
                case SIR.Pattern.Constr(constrDecl, _, _)
                    if constrDecl.name == SIRType.List.NilConstr.name
                        || constrDecl.name == SIRType.BuiltinList.Nil.name
                        || constrDecl.name == PairNilName =>
                    optNilCase = Some(cs)
                case SIR.Pattern.Constr(constrDecl, _, _)
                    if constrDecl.name == SIRType.List.Cons.name
                        || constrDecl.name == SIRType.BuiltinList.Cons.name
                        || constrDecl.name == PairConsName =>
                    optConsCase = Some(cs)
                case SIR.Pattern.Wildcard =>
                    optWildcardCase = Some(cs)
                case _ =>
                    throw LoweringException(s"Unknown pattern ${cs.pattern}", cs.anns.pos)
        }
        val isUnchecked = matchData.anns.data.contains("unchecked")
        val optEffectiveNilCase = optNilCase
            .orElse(optWildcardCase)
            .orElse(
              if !isUnchecked then {
                  println("debug: no Nil case in List match")
                  println("annotation keys: " + matchData.anns.data.keys.mkString(", "))
                  throw LoweringException("No Nil case in match", matchData.anns.pos)
              } else None
            )
        val optEffectiveConsCase = optConsCase
            .orElse(optWildcardCase)
            .orElse(
              if !isUnchecked then
                  throw LoweringException("No Cons case in match", matchData.anns.pos)
              else None
            )
        // At least one case must be present
        if optEffectiveNilCase.isEmpty && optEffectiveConsCase.isEmpty then
            throw LoweringException("Match must have at least one case", matchData.anns.pos)
        val nilCase = optEffectiveNilCase
        val consCase = optEffectiveConsCase

        val (consHeadName, consTailName) = consCase match
            case Some(cs) =>
                cs.pattern match
                    case SIR.Pattern.Constr(_, List(h, t), _) => (h, t)
                    case SIR.Pattern.Constr(_, _, _) =>
                        throw LoweringException(
                          s"Cons case should have two bindings, but found ${cs.pattern}",
                          cs.anns.pos
                        )
                    case SIR.Pattern.Const(_) =>
                        throw LoweringException(
                          s"Constant pattern not supported for list matching",
                          cs.anns.pos
                        )
                    case SIR.Pattern.Wildcard => ("_head", "_tail")
            case None => ("_head", "_tail")

        val listInputId = lctx.uniqueVarName("listInput")
        val listType = matchData.scrutinee.tp
        val listRepr = defaultListRepresentation(listType, matchData.anns.pos)
        val listInput = new VariableLoweredValue(
          id = listInputId,
          name = listInputId,
          sir = SIR.Var(
            listInputId,
            matchData.scrutinee.tp,
            matchData.anns
          ),
          representation = listRepr,
          optRhs = Some(
            loweredScrutinee.toRepresentation(
              listRepr,
              matchData.anns.pos
            )
          )
        )

        val elementType = retrieveElementType(matchData.scrutinee.tp, matchData.anns.pos)

        val consHeadRepresentation = defaultElementRepresentation(elementType, matchData.anns.pos)

        val useCaseOnList = lctx.targetProtocolVersion >= MajorProtocolVersion.vanRossemPV

        // For PlutusV4 Case on list, head/tail are lambda parameters (no optRhs)
        // For ChooseList, they're derived from headList/tailList builtins
        val consHead = new VariableLoweredValue(
          id = lctx.uniqueVarName("consHead"),
          name = consHeadName,
          sir = SIR.Var(
            consHeadName,
            elementType,
            matchData.anns
          ),
          representation = consHeadRepresentation,
          optRhs =
              if useCaseOnList then None
              else
                  Some(
                    lvBuiltinApply(
                      SIRBuiltins.headList,
                      listInput,
                      elementType,
                      consHeadRepresentation,
                      matchData.anns.pos
                    )
                  )
        )

        val consTail = new VariableLoweredValue(
          id = lctx.uniqueVarName("consTail"),
          name = consTailName,
          sir = SIR.Var(
            consTailName,
            listType,
            matchData.anns
          ),
          representation = listRepr,
          optRhs =
              if useCaseOnList then None
              else
                  Some(
                    lvBuiltinApply(
                      SIRBuiltins.tailList,
                      listInput,
                      listType,
                      listRepr,
                      matchData.anns.pos
                    )
                  )
        )

        val prevScope = lctx.scope
        lctx.scope = lctx.scope.addAll(
          List(listInput, consHead, consTail)
        )

        val resType = optTargetType.getOrElse(matchData.tp)

        val optLoweredConsBody = consCase.map { cs =>
            lctx.lower(cs.body, Some(resType)).maybeUpcast(resType, cs.anns.pos)
        }

        lctx.scope = prevScope

        val optLoweredNilBody = nilCase.map { cs =>
            lctx.lower(cs.body, Some(resType)).maybeUpcast(resType, cs.anns.pos)
        }

        if SIRType.isProd(loweredScrutinee.sirType) then
            val constrDecl = SIRType
                .retrieveConstrDecl(loweredScrutinee.sirType)
                .getOrElse(
                  throw LoweringException(
                    s"Cannot retrieve constrDecl from ${loweredScrutinee.sirType.show}",
                    matchData.anns.pos
                  )
                )
            if constrDecl.name == SIRType.List.NilConstr.name
                || constrDecl.name == SIRType.BuiltinList.Nil.name
                || constrDecl.name == PairNilName
            then {
                println("info: unused case Cons in List match will be removed")
                optLoweredNilBody.getOrElse(
                  throw LoweringException(
                    "Nil case required for Nil-typed scrutinee",
                    matchData.anns.pos
                  )
                )
            } else if constrDecl.name == SIRType.List.Cons.name
                || constrDecl.name == SIRType.BuiltinList.Cons.name
                || constrDecl.name == PairConsName
            then {
                println("info: unused case Nil in List match will be removed")
                optLoweredConsBody.getOrElse(
                  throw LoweringException(
                    "Cons case required for Cons-typed scrutinee",
                    matchData.anns.pos
                  )
                )
            } else
                throw LoweringException(
                  s"Unknown list constructior ${constrDecl.name}",
                  matchData.anns.pos
                )
        else
            // Need at least the Cons branch for Case on List / ChooseList
            val loweredConsBody = optLoweredConsBody.getOrElse(
              throw LoweringException("Cons case is required for list match", matchData.anns.pos)
            )
            val nilBranchPresent = optLoweredNilBody.isDefined
            val branches = Seq(loweredConsBody) ++ optLoweredNilBody.toSeq
            val scaffolding = builtinListScaffolding(
              listInput,
              consHead,
              consTail,
              nilBranchPresent,
              useCaseOnList,
              matchData.anns.pos
            )
            SumDispatch.assembleMatch(
              scaffolding,
              branches,
              optTargetType,
              matchData.tp,
              matchData.anns.pos
            )
    }

    /** `Case`-on-list (V4) / `ChooseList` (V3) scaffolding. The cons-head/tail bindings and the
      * lifted scrutinee var are captured at condition time; `assemble` receives aligned branches
      * plus the chosen target repr.
      */
    private def builtinListScaffolding(
        listInput: IdentifiableLoweredValue,
        consHead: IdentifiableLoweredValue,
        consTail: IdentifiableLoweredValue,
        nilBranchPresent: Boolean,
        useCaseOnList: Boolean,
        matchPos: SIRPosition
    ): SumDispatch.MatchScaffolding = new SumDispatch.MatchScaffolding {
        override def assemble(
            branches: Seq[LoweredValue],
            targetRepr: LoweredValueRepresentation,
            resultType: SIRType,
            pos: SIRPosition
        )(using LoweringContext): LoweredValue = {
            val consBody = branches(0)
            val optNilBody = if nilBranchPresent then Some(branches(1)) else None
            if useCaseOnList then
                CaseListLoweredValue(
                  listInput,
                  consHead,
                  consTail,
                  consBody,
                  optNilBody,
                  resultType,
                  targetRepr,
                  matchPos
                )
            else
                ChooseListLoweredValue(
                  listInput,
                  consHead,
                  consTail,
                  consBody,
                  optNilBody.getOrElse(
                    throw LoweringException(
                      "Nil case is required for ChooseList (V3). Use @unchecked only with targetProtocolVersion >= vanRossemPV",
                      matchPos
                    )
                  ),
                  resultType,
                  targetRepr,
                  matchPos
                )
        }
    }

    /** Outbound conversions from a list-shaped value (Phase 5). The receiver `this` is the source
      * emitter — `SumBuiltinListEmitter(elemRepr)` for cons-list sources,
      * `SumPairBuiltinListEmitter` for pair-list sources.
      *
      * Body lifted from `SumDispatch.sumListCommonImpl`: the previous `gen` parameter is now
      * `this`. Targets covered: SumBuiltinList variants, PackedSumDataList, SumPairBuiltinList
      * variants, SumDataAssocMap, plus cross-class delegations for `SumUplcConstr` /
      * `ProdUplcConstr` / `SumReprProxy` and the `PairIntDataList` rebuild path.
      */
    def emitConvert(
        input: LoweredValue,
        outputRepresentation: LoweredValueRepresentation,
        pos: SIRPosition
    )(using lctx: LoweringContext): LoweredValue = {
        import SumCaseClassRepresentation.*
        import ProductCaseClassRepresentation.ProdBuiltinPair
        if input.representation == outputRepresentation then return input
        if input.representation.isCompatibleOn(input.sirType, outputRepresentation, pos) then
            return RepresentationProxyLoweredValue(input, outputRepresentation, pos)
        (input.representation, outputRepresentation) match
            // === SumBuiltinList identity ===
            case (SumBuiltinList(inElemRepr), SumBuiltinList(outElemRepr))
                if inElemRepr == outElemRepr =>
                input
            // === SumBuiltinList cross-element-repr conversions ===
            case (SumBuiltinList(inElemRepr), SumBuiltinList(outElemRepr))
                if hasConstantOrTypeVar(inElemRepr, outElemRepr) =>
                val elemType = retrieveElementType(input.sirType, pos)
                def resolveElementRepr(
                    repr: LoweredValueRepresentation
                ): LoweredValueRepresentation =
                    repr match
                        case tvr: TypeVarRepresentation =>
                            val elemGen = lctx.typeGenerator(elemType)
                            if !tvr.isPackedData then elemGen.defaultRepresentation(elemType)
                            else elemGen.defaultTypeVarReperesentation(elemType)
                        case other => other
                val resolvedIn = resolveElementRepr(inElemRepr)
                val resolvedOut = resolveElementRepr(outElemRepr)
                val hasNativeTv = (inElemRepr, outElemRepr) match
                    case (tvr: TypeVarRepresentation, _) if isNativeTypeVar(tvr) => true
                    case (_, tvr: TypeVarRepresentation) if isNativeTypeVar(tvr) => true
                    case _                                                       => false
                if resolvedIn == resolvedOut
                    || elemType == SIRType.FreeUnificator
                    || elemType == SIRType.TypeNothing
                    || hasNativeTv
                then RepresentationProxyLoweredValue(input, outputRepresentation, pos)
                else
                    convertBuiltinList(
                      input,
                      elemType,
                      resolvedIn,
                      resolvedOut,
                      outputRepresentation,
                      pos
                    )
            // === SumBuiltinList(Constant) special cases ===
            case (SumBuiltinList(PrimitiveRepresentation.Constant), PackedSumDataList) =>
                val elemType = retrieveElementType(input.sirType, pos)
                val elemRepr = SirTypeUplcGenerator.defaultDataRepresentation(elemType)
                input
                    .toRepresentation(SumBuiltinList(elemRepr), pos)
                    .toRepresentation(PackedSumDataList, pos)
            case (PackedSumDataList, SumBuiltinList(PrimitiveRepresentation.Constant)) =>
                val elemType = retrieveElementType(input.sirType, pos)
                val elemRepr = SirTypeUplcGenerator.defaultDataRepresentation(elemType)
                input
                    .toRepresentation(SumBuiltinList(elemRepr), pos)
                    .toRepresentation(outputRepresentation, pos)
            case (
                  SumBuiltinList(PrimitiveRepresentation.Constant),
                  tv @ TypeVarRepresentation(kind)
                ) =>
                import SIRType.TypeVarKind.*
                kind match
                    case Transparent =>
                        new RepresentationProxyLoweredValue(input, tv, pos)
                    case Unwrapped =>
                        new RepresentationProxyLoweredValue(input, tv, pos)
                    case Fixed =>
                        input
                            .toRepresentation(PackedSumDataList, pos)
                            .toRepresentation(tv, pos)
            case (
                  tv @ TypeVarRepresentation(kind),
                  SumBuiltinList(PrimitiveRepresentation.Constant)
                ) =>
                import SIRType.TypeVarKind.*
                kind match
                    case Transparent =>
                        new RepresentationProxyLoweredValue(input, outputRepresentation, pos)
                    case Unwrapped =>
                        new RepresentationProxyLoweredValue(input, outputRepresentation, pos)
                    case Fixed =>
                        input
                            .toRepresentation(PackedSumDataList, pos)
                            .toRepresentation(outputRepresentation, pos)
            // === SumBuiltinList → PackedSumDataList (via listData) ===
            case (SumBuiltinList(_), PackedSumDataList) =>
                val elemType = retrieveElementType(input.sirType, pos)
                val asDataList =
                    if elemType == SIRType.TypeNothing || elemType == SIRType.FreeUnificator then
                        input
                    else
                        val elemRepr =
                            SirTypeUplcGenerator.defaultDataRepresentation(elemType)
                        input.toRepresentation(SumBuiltinList(elemRepr), pos)
                lvBuiltinApply(
                  SIRBuiltins.listData,
                  asDataList,
                  input.sirType,
                  PackedSumDataList,
                  pos
                )
            // === SumBuiltinList catch-all (compatible element reprs) ===
            case (SumBuiltinList(inElemRepr), out @ SumBuiltinList(outElemRepr)) =>
                if inElemRepr == outElemRepr then input
                else if (inElemRepr == PrimitiveRepresentation.PackedData && outElemRepr == DataData)
                    || (inElemRepr == DataData && outElemRepr == PrimitiveRepresentation.PackedData)
                then
                    throw LoweringException(
                      s"PackedData/DataData mismatch in SumBuiltinList: $inElemRepr -> $outElemRepr " +
                          s"type=${input.sirType.show} createdEx=${input.createdEx}",
                      pos
                    )
                else
                    val elemType = retrieveElementType(input.sirType, pos)
                    if inElemRepr.isCompatibleOn(elemType, outElemRepr, pos) then
                        RepresentationProxyLoweredValue(input, out, pos)
                    else convertBuiltinList(input, elemType, inElemRepr, outElemRepr, out, pos)
            // === SumBuiltinList → SumDataAssocMap (go through SumPairBuiltinList) ===
            case (SumBuiltinList(_), SumDataAssocMap) =>
                val elemType = retrieveElementType(input.sirType, pos)
                val pairRepr = SumPairBuiltinList.fromElementType(elemType, pos)
                input
                    .toRepresentation(pairRepr, pos)
                    .toRepresentation(SumDataAssocMap, pos)
            // === PackedSumDataList conversions ===
            case (PackedSumDataList, out @ SumBuiltinList(_)) =>
                val elemType = retrieveElementType(input.sirType, pos)
                val elemRepr = SirTypeUplcGenerator.defaultDataRepresentation(elemType)
                val dataListRepr = SumBuiltinList(elemRepr)
                val asDataList =
                    lvBuiltinApply(SIRBuiltins.unListData, input, input.sirType, dataListRepr, pos)
                if out == dataListRepr then asDataList
                else asDataList.toRepresentation(out, pos)
            case (PackedSumDataList, PackedSumDataList) =>
                input
            case (PackedSumDataList, SumDataAssocMap) =>
                val elemType = retrieveElementType(input.sirType, pos)
                val elemRepr = SirTypeUplcGenerator.defaultDataRepresentation(elemType)
                val pairRepr = SumPairBuiltinList.fromElementType(elemType, pos)
                input
                    .toRepresentation(SumBuiltinList(elemRepr), pos)
                    .toRepresentation(pairRepr, pos)
                    .toRepresentation(SumDataAssocMap, pos)
            case (PackedSumDataList, out @ SumPairBuiltinList(_, _)) =>
                val elemType = retrieveElementType(input.sirType, pos)
                val elemRepr = SirTypeUplcGenerator.defaultDataRepresentation(elemType)
                input
                    .toRepresentation(SumBuiltinList(elemRepr), pos)
                    .toRepresentation(out, pos)
            // === SumPairBuiltinList conversions ===
            case (SumPairBuiltinList(_, _), SumPairBuiltinList(_, _)) =>
                input
            case (SumPairBuiltinList(_, _), SumDataAssocMap) =>
                lvBuiltinApply(SIRBuiltins.mapData, input, input.sirType, SumDataAssocMap, pos)
            case (SumPairBuiltinList(_, _), PackedSumDataList) =>
                val elemType = retrieveElementType(input.sirType, pos)
                val dataElemRepr =
                    SirTypeUplcGenerator.defaultDataRepresentation(elemType)
                input
                    .toRepresentation(SumBuiltinList(dataElemRepr), pos)
                    .toRepresentation(PackedSumDataList, pos)
            case (
                  SumPairBuiltinList(inKeyRepr, inValueRepr),
                  out @ SumBuiltinList(outElemRepr)
                ) =>
                val elemType = retrieveElementType(input.sirType, pos)
                val inElemRepr = ProdBuiltinPair(inKeyRepr, inValueRepr)
                convertBuiltinList(input, elemType, inElemRepr, outElemRepr, out, pos)
            case (SumBuiltinList(inElemRepr), out @ SumPairBuiltinList(outKeyRepr, outValueRepr)) =>
                if isNilType(input.sirType) then lvPairDataNil(pos, input.sirType, out)
                else
                    val elemType = retrieveElementType(input.sirType, pos)
                    val outElemRepr = ProdBuiltinPair(outKeyRepr, outValueRepr)
                    convertBuiltinList(input, elemType, inElemRepr, outElemRepr, out, pos)
            // === SumDataAssocMap conversions ===
            case (SumDataAssocMap, SumPairBuiltinList(_, _)) =>
                val elemType = retrieveElementType(input.sirType, pos)
                val pairRepr = SumPairBuiltinList.fromElementType(elemType, pos)
                lvBuiltinApply(SIRBuiltins.unMapData, input, input.sirType, pairRepr, pos)
            case (SumDataAssocMap, _) =>
                val elemType = retrieveElementType(input.sirType, pos)
                val pairRepr = SumPairBuiltinList.fromElementType(elemType, pos)
                input
                    .toRepresentation(pairRepr, pos)
                    .toRepresentation(outputRepresentation, pos)
            // === PairIntDataList for list-typed values (suspicious — DataConstr should
            //     not exist for List types per design, but PairIntDataList is its
            //     unpacked form. Probe with logging if hit during refactor).
            case (PairIntDataList, SumBuiltinList(elementRepr)) =>
                throw LoweringException(
                  s"PairIntDataList → SumBuiltinList($elementRepr) for ${input.sirType.show}: " +
                      "DataConstr should not exist for List types.",
                  pos
                )
            case (PairIntDataList, _) =>
                // Reconstruct DataConstr from (tag, fieldList), then route via PackedSumDataList.
                val asDataConstr = lvBuiltinApply2(
                  SIRBuiltins.constrData,
                  lvBuiltinApply(
                    SIRBuiltins.fstPair,
                    input,
                    SIRType.Integer,
                    PrimitiveRepresentation.Constant,
                    pos
                  ),
                  lvBuiltinApply(
                    SIRBuiltins.sndPair,
                    input,
                    SIRType.List(SIRType.Data.tp),
                    SumBuiltinList(DataData),
                    pos
                  ),
                  input.sirType,
                  DataConstr,
                  pos
                )
                asDataConstr
                    .toRepresentation(PackedSumDataList, pos)
                    .toRepresentation(outputRepresentation, pos)
            // === TypeVarRepresentation ===
            case (_, tv: TypeVarRepresentation) =>
                import SIRType.TypeVarKind.*
                tv.kind match
                    case Transparent =>
                        new RepresentationProxyLoweredValue(input, tv, pos)
                    case Unwrapped =>
                        val targetUnderlying = defaultRepresentation(input.sirType)
                        val converted = input.toRepresentation(targetUnderlying, pos)
                        new RepresentationProxyLoweredValue(converted, tv, pos)
                    case Fixed =>
                        val inputAsData = input.toRepresentation(PackedSumDataList, pos)
                        new RepresentationProxyLoweredValue(inputAsData, tv, pos)
            case (tv: TypeVarRepresentation, _) =>
                import SIRType.TypeVarKind.*
                tv.kind match
                    case Transparent =>
                        new RepresentationProxyLoweredValue(input, outputRepresentation, pos)
                    case Unwrapped =>
                        val sourceUnderlying = defaultRepresentation(input.sirType)
                        val r0 = new RepresentationProxyLoweredValue(input, sourceUnderlying, pos)
                        r0.toRepresentation(outputRepresentation, pos)
                    case Fixed =>
                        if input.representation == outputRepresentation then input
                        else
                            val r0 = new RepresentationProxyLoweredValue(
                              input,
                              PackedSumDataList,
                              pos
                            )
                            r0.toRepresentation(outputRepresentation, pos)
            // SumReprProxy: unwrap and delegate
            case (_: SumReprProxy, _) =>
                SumUplcConstrEmitter.emitConvert(input, outputRepresentation, pos)
            case (_, _: SumReprProxy) =>
                SumUplcConstrEmitter.emitConvert(input, outputRepresentation, pos)
            // SumUplcConstr → anything: delegate
            case (_: SumUplcConstr, _) =>
                SumUplcConstrEmitter.emitConvert(input, outputRepresentation, pos)
            // anything → SumUplcConstr: delegate
            case (_, _: SumUplcConstr) =>
                SumUplcConstrEmitter.emitConvert(input, outputRepresentation, pos)
            // ProdUplcConstr value at a sum-list-type site: wrap as singleton SumUplcConstr
            case (puc: ProductCaseClassRepresentation.ProdUplcConstr, _) =>
                val wrappedRepr =
                    SumUplcConstr(scala.collection.immutable.SortedMap(puc.tag -> puc))
                val wrapped = new RepresentationProxyLoweredValue(input, wrappedRepr, pos)
                SumUplcConstrEmitter.emitConvert(
                  wrapped,
                  outputRepresentation,
                  pos
                )
            case _ =>
                throw LoweringException(
                  s"Unexpected representation conversion for ${input.sirType.show} from ${input.representation} to $outputRepresentation",
                  pos
                )
    }

    /** Cross-element-repr conversion for BuiltinList, applied via ScalusRuntime.mapList. */
    protected def convertBuiltinList(
        input: LoweredValue,
        elemType: SIRType,
        resolvedIn: LoweredValueRepresentation,
        resolvedOut: LoweredValueRepresentation,
        outputRepresentation: LoweredValueRepresentation,
        pos: SIRPosition
    )(using lctx: LoweringContext): LoweredValue = {
        import SumCaseClassRepresentation.*
        val inElemUplcType = resolvedIn.uplcType(elemType)
        val outElemUplcType = resolvedOut.uplcType(elemType)
        val inListType = SIRType.BuiltinList(inElemUplcType)
        val outListType = SIRType.BuiltinList(outElemUplcType)
        val isPairIn = resolvedIn match
            case _: ProductCaseClassRepresentation.ProdBuiltinPair => true
            case _                                                 => false
        val isPairOut = resolvedOut match
            case _: ProductCaseClassRepresentation.ProdBuiltinPair => true
            case _                                                 => false
        val inListRepr: LoweredValueRepresentation =
            if isPairIn then SumPairBuiltinList.fromElementType(elemType, pos)
            else SumBuiltinList(resolvedIn)
        val outListRepr: LoweredValueRepresentation =
            if isPairOut then SumPairBuiltinList.fromElementType(elemType, pos)
            else SumBuiltinList(resolvedOut)
        val outNil =
            if isPairOut then SumPairBuiltinListEmitter.genNil(outListType, pos)
            else new SumBuiltinListEmitter(resolvedOut).genNil(outListType, pos)
        val convFn = lvLamAbs(
          "elem",
          elemType,
          resolvedIn,
          elem => elem.toRepresentation(resolvedOut, pos),
          pos
        )
        val afterFnType = outListType ->: inListType ->: outListType
        val afterFnRepr = LambdaRepresentation(
          afterFnType,
          InOutRepresentationPair(
            outListRepr,
            LambdaRepresentation(
              inListType ->: outListType,
              InOutRepresentationPair(inListRepr, outListRepr)
            )
          )
        )
        val afterNilType = inListType ->: outListType
        val afterNilRepr =
            LambdaRepresentation(afterNilType, InOutRepresentationPair(inListRepr, outListRepr))

        val result = lvApplyDirect(
          lvApplyDirect(
            lvApplyDirect(ScalusRuntime.mapList, convFn, afterFnType, afterFnRepr, pos),
            outNil,
            afterNilType,
            afterNilRepr,
            pos
          ),
          input,
          input.sirType,
          outListRepr,
          pos
        )
        if outputRepresentation == outListRepr then result
        else RepresentationProxyLoweredValue(result, outputRepresentation, pos)
    }

    private def hasConstantOrTypeVar(
        a: LoweredValueRepresentation,
        b: LoweredValueRepresentation
    ): Boolean =
        a == PrimitiveRepresentation.Constant || b == PrimitiveRepresentation.Constant ||
            a.isInstanceOf[TypeVarRepresentation] || b.isInstanceOf[TypeVarRepresentation]

    private def isNativeTypeVar(tvr: TypeVarRepresentation)(using
        lctx: LoweringContext
    ): Boolean =
        import SIRType.TypeVarKind.*
        tvr.kind match
            case Transparent | Unwrapped => true
            case Fixed                   => false

}

object SumListEmitterCommon {
    val PairListDataDeclName = SIRType.PairList.DataDeclName
    val PairNilName = SIRType.PairList.PairNilName
    val PairConsName = SIRType.PairList.PairConsName
}
