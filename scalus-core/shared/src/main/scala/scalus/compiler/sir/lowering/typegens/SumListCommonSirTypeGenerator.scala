package scalus.compiler.sir.lowering
package typegens

import scalus.cardano.ledger.MajorProtocolVersion
import scalus.compiler.sir.lowering.LoweredValue.Builder.*
import scalus.compiler.sir.*

import scala.util.control.NonFatal

/** handle next cases: scalus.cardano.onchain.plutus.prelude.List[A]
  * scalus.uplc.builtin.BuiltinList[A]
  */
trait SumListCommonSirTypeGenerator extends SirTypeUplcGenerator {

    def defaultListRepresentation(using LoweringContext): LoweredValueRepresentation

    def defaultElementRepresentation(tp: SIRType, pos: SIRPosition)(using
        LoweringContext
    ): LoweredValueRepresentation

    /** mkNilData and mkNilPairData
      * @param pos
      * @return
      */
    def genNil(resType: SIRType, pos: SIRPosition)(using LoweringContext): LoweredValue

    override def isDataSupported(tp: SIRType)(using lctx: LoweringContext): Boolean = true

    override def toRepresentation(
        input: LoweredValue,
        outputRepresentation: LoweredValueRepresentation,
        pos: SIRPosition
    )(using lctx: LoweringContext): LoweredValue = {
        (input.representation, outputRepresentation) match
            case (
                  SumCaseClassRepresentation.SumDataList,
                  SumCaseClassRepresentation.PackedSumDataList
                ) =>
                lvBuiltinApply(
                  SIRBuiltins.listData,
                  input,
                  input.sirType,
                  SumCaseClassRepresentation.PackedSumDataList,
                  pos
                )
            case (SumCaseClassRepresentation.SumDataList, SumCaseClassRepresentation.SumDataList) =>
                input
            case (
                  SumCaseClassRepresentation.SumDataList,
                  SumCaseClassRepresentation.SumDataPairList
                ) =>
                if isBuiltinList(input.sirType) then
                    throw LoweringException(
                      "Convering representation of builtin List (SumDataList => SumDataPairList) is not  allowed",
                      pos
                    )
                // Special case: Nil list can be converted directly to empty pair list
                if isNilType(input.sirType) then lvPairDataNil(pos, input.sirType)
                else
                    val elementType = retrieveElementType(
                      input.sirType,
                      pos
                    )
                    val (elemTypeParams, elemConstrDecl, elemTypeArgs) = SIRType
                        .collectProd(elementType)
                        .getOrElse(
                          throw new LoweringException(
                            s"Element type of pair-list should be a pair or tuple, but we have ${elementType.show} from ${input.sirType.show}",
                            pos
                          )
                        )
                    val fun =
                        if elemConstrDecl.name == SIRType.BuiltinPair.name then {
                            val retval = ScalusRuntime.dataListToPairsList
                            ScalusRuntime.dataListToPairsList
                        } else if elemConstrDecl.name == "scala.Tuple2" then
                            ScalusRuntime.dataListToTuplesList
                        else
                            throw new LoweringException(
                              s"Element type of pair-list should be a pair or tuple, but we have ${elementType.show}",
                              pos
                            )
                    lvApply(fun, input, pos, None, None)
            case (
                  SumCaseClassRepresentation.SumDataList,
                  SumCaseClassRepresentation.SumDataAssocMap
                ) =>
                input
                    .toRepresentation(SumCaseClassRepresentation.SumDataPairList, pos)
                    .toRepresentation(SumCaseClassRepresentation.SumDataAssocMap, pos)
            case (
                  SumCaseClassRepresentation.PackedSumDataList,
                  SumCaseClassRepresentation.SumDataPairList
                ) =>
                input
                    .toRepresentation(SumCaseClassRepresentation.SumDataList, pos)
                    .toRepresentation(SumCaseClassRepresentation.SumDataPairList, pos)
            case (
                  SumCaseClassRepresentation.PackedSumDataList,
                  SumCaseClassRepresentation.PackedSumDataList
                ) =>
                input
            case (
                  SumCaseClassRepresentation.PackedSumDataList,
                  SumCaseClassRepresentation.SumDataList
                ) =>
                lvBuiltinApply(
                  SIRBuiltins.unListData,
                  input,
                  input.sirType,
                  SumCaseClassRepresentation.SumDataList,
                  pos
                )
            case (
                  SumCaseClassRepresentation.PackedSumDataList,
                  SumCaseClassRepresentation.SumDataAssocMap
                ) =>
                input
                    .toRepresentation(SumCaseClassRepresentation.SumDataList, pos)
                    .toRepresentation(SumCaseClassRepresentation.SumDataPairList, pos)
                    .toRepresentation(SumCaseClassRepresentation.SumDataAssocMap, pos)
            case (
                  SumCaseClassRepresentation.SumDataPairList,
                  SumCaseClassRepresentation.SumDataPairList
                ) =>
                input
            case (
                  SumCaseClassRepresentation.SumDataPairList,
                  SumCaseClassRepresentation.SumDataAssocMap
                ) =>
                lvBuiltinApply(
                  SIRBuiltins.mapData,
                  input,
                  input.sirType,
                  SumCaseClassRepresentation.SumDataAssocMap,
                  pos
                )
            case (
                  SumCaseClassRepresentation.SumDataPairList,
                  SumCaseClassRepresentation.PackedSumDataList
                ) =>
                input
                    .toRepresentation(SumCaseClassRepresentation.SumDataList, pos)
                    .toRepresentation(SumCaseClassRepresentation.PackedSumDataList, pos)
            case (
                  SumCaseClassRepresentation.SumDataPairList,
                  SumCaseClassRepresentation.SumDataList
                ) =>
                if isBuiltinList(input.sirType) then
                    throw LoweringException(
                      "Convering representation of builtin List (SumDataPairList => SumDataList) is not  allowed",
                      pos
                    )
                //   when it potenitallu can be used -- when one part of the program know, that element
                //     is a list of pairs, but another part - does not know.
                //     (pass to foldLeft, foldLeft expect List as dat)
                // we should change stdlib, do not run such conversions
                val elementType = retrieveElementType(
                  input.sirType,
                  pos
                )
                val (typeParams, constrDecl, typeArgs) = SIRType
                    .collectProd(elementType)
                    .getOrElse(
                      throw new LoweringException(
                        s"Element type of pair-list should be a pair or tuple, but we have ${elementType.show}",
                        pos
                      )
                    )
                val fun =
                    if constrDecl.name == SIRType.BuiltinPair.name then {
                        ScalusRuntime.pairsListToDataList
                    } else if constrDecl.name == "scala.Tuple2" then {
                        ScalusRuntime.tuplesListToDataList
                    } else
                        throw new LoweringException(
                          s"Element type of pair-list should be a pair or tuple, but we have ${elementType.show}",
                          pos
                        )
                // we know that function will have appropriate type and representation by definition
                lvApply(fun, input, pos, None, None)
            case (
                  SumCaseClassRepresentation.SumDataAssocMap,
                  SumCaseClassRepresentation.SumDataPairList
                ) =>
                lvBuiltinApply(
                  SIRBuiltins.unMapData,
                  input,
                  input.sirType,
                  SumCaseClassRepresentation.SumDataPairList,
                  pos
                )
            case (SumCaseClassRepresentation.SumDataAssocMap, _) =>
                input
                    .toRepresentation(SumCaseClassRepresentation.SumDataPairList, pos)
                    .toRepresentation(outputRepresentation, pos)
            case (_, tv @ TypeVarRepresentation(isBuiltin)) =>
                if isBuiltin then input
                else {
                    val inputAsData =
                        input.toRepresentation(SumCaseClassRepresentation.PackedSumDataList, pos)
                    new RepresentationProxyLoweredValue(inputAsData, tv, pos)
                }
            case (TypeVarRepresentation(isBuiltin), _) =>
                if isBuiltin then RepresentationProxyLoweredValue(input, outputRepresentation, pos)
                else if input.representation == outputRepresentation then input
                else
                    val r0 = RepresentationProxyLoweredValue(
                      input,
                      SumCaseClassRepresentation.PackedSumDataList,
                      pos
                    )
                    r0.toRepresentation(outputRepresentation, pos)
            case _ =>
                throw LoweringException(
                  s"Unexpected representation conversion  for ${input.sirType.show} from ${input.representation} to ${outputRepresentation}",
                  pos
                )
    }

    override def upcastOne(
        input: LoweredValue,
        targetType: SIRType,
        pos: SIRPosition
    )(using lctx: LoweringContext): LoweredValue = {
        val targetElementType = retrieveElementType(
          targetType,
          pos
        )
        input.representation match {
            case SumCaseClassRepresentation.SumDataList |
                SumCaseClassRepresentation.PackedSumDataList =>
                // no changes
                TypeRepresentationProxyLoweredValue(input, targetType, input.representation, pos)
            case SumCaseClassRepresentation.SumDataPairList |
                SumCaseClassRepresentation.SumDataAssocMap =>
                if SirTypeUplcGenerator.isPairOrTuple2(targetElementType) then
                    TypeRepresentationProxyLoweredValue(
                      input,
                      targetType,
                      input.representation,
                      pos
                    )
                else
                    // and here is error:  we should not change representation during upcast
                    //  (because changing representation calls apply, apply call upcast...
                    //
                    // val alignedInput =
                    //    try
                    //        input.toRepresentation(
                    //          SumCaseClassRepresentation.SumDataList,
                    //          pos
                    //        )
                    //    catch
                    //        case ex: StackOverflowError =>
                    //            println("error in upcastOne for List: StackOverflowError")
                    //            println(s"targetType: ${targetType.show}")
                    //            println(s"inputType: ${input.sirType.show}")
                    //            println(s"input: ${input.show}")
                    //            // ex.printStackTrace()
                    //            throw ex;
                    TypeRepresentationProxyLoweredValue(
                      input,
                      targetType,
                      input.representation,
                      pos
                    )
            case TypeVarRepresentation(isBuiltin) =>
                val targetRepresentation = {
                    if isBuiltin then defaultRepresentation(input.sirType)
                    else this.defaultTypeVarReperesentation(input.sirType)
                }
                val alignedInput = input.toRepresentation(
                  targetRepresentation,
                  pos
                )
                upcastOne(alignedInput, targetType, pos)
            case _ =>
                throw LoweringException(
                  s"Unexpected representation ${input.representation.show} for List upcast from ${input.sirType.show} to ${targetType.show}",
                  pos
                )
        }
    }

    override def genConstr(constr: SIR.Constr)(using lctx: LoweringContext): LoweredValue = {
        constr.name match
            case SIRType.List.NilConstr.name | SIRType.BuiltinList.Nil.name =>
                genNil(constr.tp, constr.anns.pos)
            case SIRType.List.Cons.name | SIRType.BuiltinList.Cons.name =>
                if constr.args.size != 2 then
                    throw LoweringException(
                      s"Constr construnctor with ${constr.args.size} args, should be 2",
                      constr.anns.pos
                    )
                val head = lctx.lower(constr.args.head)
                val tail = lctx.lower(constr.args.tail.head)
                val elementType = retrieveElementType(constr.tp, constr.anns.pos)
                val headElementUpcasted = head
                    .maybeUpcast(elementType, constr.anns.pos)
                val headElementRepr =
                    try
                        headElementUpcasted.toRepresentation(
                          defaultElementRepresentation(elementType, constr.anns.pos),
                          constr.anns.pos
                        )
                    catch
                        case NonFatal(ex) =>
                            println(
                              s"error in genConstr for List, head.sirType: ${head.sirType.show}, tail.sirType: ${tail.sirType.show}, constr.tp=${constr.tp.show}"
                            )
                            println(s"elementType: ${elementType.show}")
                            println(
                              s"defaultElementRepresentation: ${defaultElementRepresentation(elementType, constr.anns.pos).show}"
                            )
                            throw ex
                // special case when tail is Nil, than have tyoe List[Nothing]
                val fixedTail =
                    if isNilType(tail.sirType) then
                        fixNilInConstr(tail, constr.tp, defaultListRepresentation)
                    else tail

                val tailElementRepr = {
                    try fixedTail.toRepresentation(defaultListRepresentation, constr.anns.pos)
                    catch
                        case NonFatal(ex) =>
                            println(
                              s"error in genConstr for List, head.sirType: ${head.sirType.show}, tail.sirType: ${tail.sirType.show}, constr.tp=${constr.tp.show}"
                            )
                            println(s"relementType: ${elementType.show}")
                            println(s"defaultListRepresentation: ${defaultListRepresentation.show}")
                            println(s"tail.sirType: ${tail.sirType.show}")
                            throw ex
                }
                lvBuiltinApply2(
                  SIRBuiltins.mkCons,
                  headElementRepr,
                  tailElementRepr,
                  constr.tp,
                  defaultListRepresentation,
                  constr.anns.pos
                )
            case _ =>
                throw LoweringException(
                  s"Unknown constructor ${constr.name} for List",
                  constr.anns.pos
                )
    }

    def isNilType(tp: SIRType): Boolean = {
        SIRType.retrieveConstrDecl(tp) match {
            case Left(r) => false
            case Right(constrDecl) =>
                constrDecl.name == SIRType.List.NilConstr.name
                || constrDecl.name == SIRType.BuiltinList.Nil.name
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
        val scrutineeDataRepr = loweredScrutinee.toRepresentation(
          SumCaseClassRepresentation.SumDataList,
          sel.anns.pos
        )
        sel.field match {
            case "head" =>
                lvBuiltinApply(
                  SIRBuiltins.headList,
                  scrutineeDataRepr,
                  sel.tp,
                  lctx.typeGenerator(sel.tp).defaultDataRepresentation(sel.tp),
                  sel.anns.pos
                )
            case "tail" =>
                lvBuiltinApply(
                  SIRBuiltins.tailList,
                  scrutineeDataRepr,
                  sel.tp,
                  SumCaseClassRepresentation.SumDataList,
                  sel.anns.pos
                )
            case "isNull" =>
                // isNull is not a field, but a method, that returns true if list is empty
                lvBuiltinApply(
                  SIRBuiltins.nullList,
                  scrutineeDataRepr,
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
        tp match {
            case SIRType.SumCaseClass(decl, typeArgs) =>
                typeArgs.head
            case SIRType.CaseClass(constrDecl, typeArgs, optParent) =>
                if constrDecl.name == SIRType.List.NilConstr.name
                    || constrDecl.name == SIRType.BuiltinList.Nil.name
                then SIRType.FreeUnificator
                else if constrDecl.name == SIRType.List.Cons.name
                    || constrDecl.name == SIRType.BuiltinList.Cons.name
                then typeArgs.head
                else
                    throw LoweringException(
                      s"Unknown case class ${constrDecl.name} for List",
                      pos
                    )
            case SIRType.TypeLambda(params, body) =>
                retrieveElementType(body, pos)
            case SIRType.TypeProxy(ref) =>
                retrieveElementType(ref, pos)
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
        // Nil, Cons
        var optNilCase: Option[SIR.Case] = None
        var optConsCase: Option[SIR.Case] = None
        var optWildcardCase: Option[SIR.Case] = None
        var noBindingInConsCase = false
        matchData.cases.foreach { cs =>
            cs.pattern match
                case SIR.Pattern.Constr(constrDecl, _, _)
                    if constrDecl.name == SIRType.List.NilConstr.name
                        || constrDecl.name == SIRType.BuiltinList.Nil.name =>
                    optNilCase = Some(cs)
                case SIR.Pattern.Constr(constrDecl, _, _)
                    if constrDecl.name == SIRType.List.Cons.name
                        || constrDecl.name == SIRType.BuiltinList.Cons.name =>
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
        val listInput = new VariableLoweredValue(
          id = listInputId,
          name = listInputId,
          sir = SIR.Var(
            listInputId,
            matchData.scrutinee.tp,
            matchData.anns
          ),
          representation = defaultListRepresentation,
          optRhs = Some(
            loweredScrutinee.toRepresentation(
              defaultListRepresentation,
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
          representation = defaultListRepresentation,
          optRhs =
              if useCaseOnList then None
              else
                  Some(
                    lvBuiltinApply(
                      SIRBuiltins.tailList,
                      listInput,
                      listType,
                      defaultListRepresentation,
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

            val allBranches = Seq(loweredConsBody) ++ optLoweredNilBody.toSeq
            val resRepr = LoweredValue.chooseCommonRepresentation(
              allBranches,
              resType,
              matchData.anns.pos
            )
            val loweredConsBodyR =
                loweredConsBody.toRepresentation(resRepr, consCase.get.anns.pos)
            val optLoweredNilBodyR =
                optLoweredNilBody.map(nb => nb.toRepresentation(resRepr, nilCase.get.anns.pos))

            // For PlutusV4, use Case on list; otherwise use ChooseList builtin
            val retval =
                if useCaseOnList then
                    CaseListLoweredValue(
                      listInput,
                      consHead,
                      consTail,
                      loweredConsBodyR,
                      optLoweredNilBodyR,
                      resType,
                      resRepr,
                      matchData.anns.pos
                    )
                else
                    ChooseListLoweredValue(
                      listInput,
                      consHead,
                      consTail,
                      loweredConsBodyR,
                      optLoweredNilBodyR.getOrElse(
                        throw LoweringException(
                          "Nil case is required for ChooseList (V3). Use @unchecked only with targetProtocolVersion >= vanRossemPV",
                          matchData.anns.pos
                        )
                      ),
                      resType,
                      resRepr,
                      matchData.anns.pos
                    )

            retval
    }

}
