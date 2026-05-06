package scalus.compiler.sir.lowering
package typegens

import scalus.cardano.ledger.MajorProtocolVersion
import scalus.compiler.sir.lowering.LoweredValue.Builder.*
import scalus.compiler.sir.*

import scala.util.control.NonFatal

/** handle next cases: scalus.cardano.onchain.plutus.prelude.List[A]
  * scalus.uplc.builtin.BuiltinList[A] scalus.cardano.onchain.plutus.prelude.PairList[A, B]
  */
trait SumListCommonSirTypeGenerator extends SirTypeUplcGenerator {

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
        SumDispatch.dispatcherBypass("SumListCommonSirTypeGenerator")

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
        import SumListCommonSirTypeGenerator.*
        // Routing decisions live in SumDispatch.chooseConstrOutputRepr now (see
        // ConstrDispatcher.shouldDelegateToUplcConstr for the original rules).
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
        import SumListCommonSirTypeGenerator.*
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
                return SumUplcConstrSirTypeGenerator.genSelectUplcConstr(sel, loweredScrutinee)
            case _ =>
        val (scrutineeReady, listRepr, elemRepr) = loweredScrutinee.representation match
            case sbl @ SumCaseClassRepresentation.SumBuiltinList(er) =>
                (loweredScrutinee, sbl, er)
            case _ =>
                val elemType = retrieveElementType(loweredScrutinee.sirType, sel.anns.pos)
                val er = lctx.typeGenerator(elemType).defaultDataRepresentation(elemType)
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
        import SumListCommonSirTypeGenerator.*
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
                return SumUplcConstrSirTypeGenerator.genMatchUplcConstr(
                  matchData,
                  loweredScrutinee,
                  optTargetType
                )
            case _ =>
        // Nil, Cons
        import SumListCommonSirTypeGenerator.*
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

            // Phase 3b: branch convergence routes through SumDispatch. The
            // Transparent-UplcConstr override fires when a branch carries
            // ProdUplcConstr/SumUplcConstr with Transparent TypeVar fields and
            // synthesizes a SumUplcConstr aligned structurally; otherwise we
            // fall back to chooseCommonRepresentation + uniform conversion.
            val allBranches = Seq(loweredConsBody) ++ optLoweredNilBody.toSeq
            val (resRepr, alignedBranches) =
                SumDispatch
                    .transparentSumUplcConstrAlignment(allBranches, resType, matchData.anns.pos)
                    .getOrElse {
                        val repr = LoweredValue.chooseCommonRepresentation(
                          allBranches,
                          resType,
                          matchData.anns.pos
                        )
                        val aligned = Seq(
                          loweredConsBody.toRepresentation(repr, consCase.get.anns.pos)
                        ) ++ optLoweredNilBody.map(nb =>
                            nb.toRepresentation(repr, nilCase.get.anns.pos)
                        )
                        (repr, aligned)
                    }
            val loweredConsBodyR = alignedBranches(0)
            val optLoweredNilBodyR =
                if optLoweredNilBody.isDefined then Some(alignedBranches(1)) else None

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

object SumListCommonSirTypeGenerator {
    val PairListDataDeclName = SIRType.PairList.DataDeclName
    val PairNilName = SIRType.PairList.PairNilName
    val PairConsName = SIRType.PairList.PairConsName
}
