package scalus.compiler.sir.lowering
package typegens

import scalus.compiler.sir.lowering.LoweredValue.Builder.*
import scalus.compiler.sir.*
import scalus.compiler.sir.SIR.Pattern

import scala.collection.mutable

object SumCaseSirTypeGenerator extends SirTypeUplcGenerator {

    import SumCaseClassRepresentation.*

    override def defaultRepresentation(tp: SIRType)(using
        LoweringContext
    ): LoweredValueRepresentation = {
        SumCaseClassRepresentation.DataConstr
    }

    override def defaultDataRepresentation(tp: SIRType)(using
        LoweringContext
    ): LoweredValueRepresentation = {
        SumCaseClassRepresentation.DataConstr
    }

    override def defaultTypeVarReperesentation(tp: SIRType)(using
        lctx: LoweringContext
    ): LoweredValueRepresentation =
        SumCaseClassRepresentation.DataConstr

    override def isDataSupported(tp: SIRType)(using lctx: LoweringContext): Boolean = true

    override def toRepresentation(
        input: LoweredValue,
        representation: LoweredValueRepresentation,
        pos: SIRPosition
    )(using lctx: LoweringContext): LoweredValue = {
        (input.representation, representation) match {
            case (DataConstr, DataConstr) =>
                input
            case (DataConstr, PairIntDataList) =>
                lvBuiltinApply(SIRBuiltins.unConstrData, input, input.sirType, PairIntDataList, pos)
            case (DataConstr, SumDataList) =>
                ???
            case (DataConstr, PackedSumDataList) =>
                val asDataList = toRepresentation(input, SumDataList, pos)
                lvBuiltinApply(
                  SIRBuiltins.listData,
                  asDataList,
                  input.sirType,
                  PackedSumDataList,
                  pos
                )
            case (DataConstr, UplcConstr) =>
                ???
            case (DataConstr, UplcConstrOnData) =>
                ???
            case (SumDataList, DataConstr) =>
                ???
            case (SumDataList, SumDataList) =>
                input
            case (SumDataList, PackedSumDataList) =>
                lvBuiltinApply(SIRBuiltins.listData, input, input.sirType, PackedSumDataList, pos)
            case (SumDataList, UplcConstr) =>
                ???
            case (SumDataList, UplcConstrOnData) =>
                ???
            case (PackedSumDataList, SumDataList) =>
                lvBuiltinApply(SIRBuiltins.unListData, input, input.sirType, SumDataList, pos)
            case (PackedSumDataList, PackedSumDataList) =>
                input
            case (PackedSumDataList, DataConstr) =>
                val asDataList = toRepresentation(input, SumDataList, pos)
                asDataList.toRepresentation(DataConstr, pos)
            case (PackedSumDataList, UplcConstr) =>
                ???
            case (PackedSumDataList, UplcConstrOnData) =>
                ???
            case (UplcConstr, DataConstr) =>
                ???
            case (UplcConstr, SumDataList) =>
                ???
            case (UplcConstr, PackedSumDataList) =>
                val asDataList = toRepresentation(input, SumDataList, pos)
                asDataList.toRepresentation(PackedSumDataList, pos)
            case (TypeVarRepresentation(inBuiltin), outRepr) =>
                if inBuiltin then RepresentationProxyLoweredValue(input, representation, pos)
                else
                    val r0 = RepresentationProxyLoweredValue(input, DataConstr, pos)
                    toRepresentation(r0, representation, pos)
            case (inRepr, TypeVarRepresentation(outBuiltin)) =>
                if outBuiltin then input
                else toRepresentation(input, DataConstr, pos)
            case (_, _) =>
                throw LoweringException(
                  s"Unsupported conversion for ${input.sirType.show} from ${input.representation} to $representation",
                  pos
                )
        }
    }

    override def upcastOne(
        input: LoweredValue,
        targetType: SIRType,
        pos: SIRPosition
    )(using lctx: LoweringContext): LoweredValue = {
        val targetDataDecl = retrieveDataDecl(targetType, pos)
        val dataConstructors = targetDataDecl.constructors
        val inputDataDecl = retrieveDataDecl(input.sirType, pos)
        val constrName = SIRType.syntheticNarrowConstrDeclName(inputDataDecl.name)
        val constrIndex = dataConstructors.indexWhere(_.name == constrName)
        if constrIndex < 0 then {
            throw LoweringException(
              s"Expected case class ${inputDataDecl.name} with constr ${constrName}, but it is not found in data declaration",
              pos
            )
        }
        val inputDataRepr = input.toRepresentation(SumCaseClassRepresentation.DataConstr, pos)

        val inPos = pos

        val constrProduct = new TypeRepresentationProxyLoweredValue(
          inputDataRepr,
          targetDataDecl.constrType(constrName),
          ProductCaseClassRepresentation.OneElementWrapper(inputDataRepr.representation),
          inPos
        )

        val constrProductDataConstr =
            constrProduct.toRepresentation(ProductCaseClassRepresentation.ProdDataConstr, pos)

        val retval = new TypeRepresentationProxyLoweredValue(
          constrProductDataConstr,
          targetType,
          SumCaseClassRepresentation.DataConstr,
          inPos
        )

        retval
    }

    override def genSelect(sel: SIR.Select, loweredScrutinee: LoweredValue)(using
        lctx: LoweringContext
    ): LoweredValue = {
        throw LoweringException(
          s"Cannot generate select for ${sel.tp} as it is a sum type",
          sel.anns.pos
        )
    }

    override def genConstr(constr: SIR.Constr)(using
        lctx: LoweringContext
    ): LoweredValue = {
        val caseClassType = constr.data.constrType(constr.name)
        lctx.typeGenerator(caseClassType).genConstr(constr.copy(tp = caseClassType))

    }

    override def genMatch(
        matchData: SIR.Match,
        loweredScrutinee: LoweredValue,
        optTargetType: Option[SIRType]
    )(using
        lctx: LoweringContext
    ): LoweredValue = {
        loweredScrutinee.representation match {
            case DataConstr =>
                genMatchDataConstr(
                  matchData,
                  loweredScrutinee,
                  optTargetType
                )
            case SumDataList =>
                SumDataListSirTypeGenerator.genMatch(matchData, loweredScrutinee, optTargetType)
            case PackedSumDataList =>
                SumDataListSirTypeGenerator.genMatch(matchData, loweredScrutinee, optTargetType)
            case UplcConstr =>
                genMatchUplcConstr(
                  matchData,
                  loweredScrutinee,
                  optTargetType
                )
            case UplcConstrOnData =>
                ???
            case TypeVarRepresentation(_) =>
                // When we have TypeVarRepresentation, convert to the proper representation
                // for the scrutinee's actual type and recurse
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
                throw LoweringException(
                  s"Unsupported representation ${loweredScrutinee.representation} for match expression",
                  matchData.anns.pos
                )
        }

    }

    /** Prepares cases withour wildcards, ordered the same as in enum definition, for match
      * expression,
      *
      * @param matchData
      * @param loweredScrutinee
      * @param lctx
      * @return
      */
    private def prepareOrderedCased(
        matchData: SIR.Match,
        loweredScrutinee: LoweredValue
    )(using LoweringContext): List[SIR.Case] = {

        val cases = matchData.cases
        val anns = matchData.anns

        val constructors = findConstructors(loweredScrutinee.sirType, anns.pos)

        // 1. If we have a wildcard case, it must be the last one
        // 2. Validate we don't have any errors
        // 3. Convert Wildcard to the rest of the cases/constructors
        // 4. Sort the cases by constructor name

        var idx = 0

        val allConstructors = constructors.toSet
        val matchedConstructors = mutable.HashSet.empty[String]
        val expandedCases = mutable.ArrayBuffer.empty[SIR.Case]

        // when we have a deconstruction like this:
        // val Some(x) = expr
        // Scala compiler generates an @unchecked annotation
        // In Scala 3, it may or may not generate a wildcard case with ERROR.
        // We need to add a wildcard case only if one doesn't already exist.
        val isUnchecked = anns.data.contains("unchecked")
        val hasWildcard = cases.lastOption.exists(_.pattern == SIR.Pattern.Wildcard)
        val enhancedCases =
            if isUnchecked && cases.length < allConstructors.size && !hasWildcard then
                cases :+ SIR.Case(
                  SIR.Pattern.Wildcard,
                  SIR.Error(
                    s"Unexpected case at ${anns.pos.file}:${anns.pos.startLine + 1}",
                    anns
                  ),
                  anns
                )
            else cases

        val casesIter = enhancedCases.iterator

        while casesIter.hasNext do
            casesIter.next() match
                case c @ SIR.Case(SIR.Pattern.Constr(constrDecl, _, _), _, anns) =>
                    constructors.find(_.name == constrDecl.name) match
                        case None =>
                            throw LoweringException(
                              s"Constructor ${constrDecl.name} not found in type ${loweredScrutinee.sirType.show} at ${anns.pos}",
                              anns.pos
                            )
                        case Some(_) =>
                            matchedConstructors += constrDecl.name // collect all matched constructors
                            expandedCases += c
                case SIR.Case(SIR.Pattern.Const(_), _, anns) =>
                    throw LoweringException(
                      s"Constant pattern not supported for sum type ${loweredScrutinee.sirType.show}",
                      anns.pos
                    )
                case SIR.Case(SIR.Pattern.Wildcard, rhs, anns) =>
                    // If we have a wildcard case, it must be the last one
                    if idx != enhancedCases.length - 1 then
                        throw new IllegalArgumentException(
                          s"Wildcard case must be the last and only one in match expression"
                        )
                    else
                        // Convert Wildcard to the rest of the cases/constructors
                        val missingConstructors =
                            allConstructors.filter(c => !matchedConstructors.contains(c.name))
                        missingConstructors.foreach { constrDecl =>
                            val bindings =
                                constrDecl.params.map(p => s"__scalus_unused_binding_${p.name}")
                            // TODO: extract rhs to a let binding before the match
                            // so we don't have to repeat it for each case
                            // also we have no way to know type-arguments, so use abstract type-vars (will use FreeUnificator)
                            val typeArgs =
                                constrDecl.typeParams.map(_ => SIRType.FreeUnificator)
                            expandedCases += SIR.Case(
                              Pattern.Constr(constrDecl, bindings, typeArgs),
                              rhs,
                              anns
                            )
                            matchedConstructors += constrDecl.name // collect all matched constructors
                        }
            idx += 1
        end while

        // Sort the cases by the same order as the constructors

        val orderedCases = constructors.map { constr =>
            val optExpandedCase = expandedCases.find(_.pattern match {
                case Pattern.Constr(constrDecl, _, _) => constrDecl.name == constr.name
                case _                                => false
            })
            optExpandedCase.getOrElse(
              throw LoweringException(
                s"Missing case for constructor ${constr.name}",
                anns.pos
              )
            )
        }.toList

        orderedCases
    }

    def genMatchDataConstr(
        matchData: SIR.Match,
        loweredScrutinee: LoweredValue,
        optTargetType: Option[SIRType]
    )(using lctx: LoweringContext): LoweredValue = {

        val orderedCases = prepareOrderedCased(matchData, loweredScrutinee)

        val prevScope = lctx.scope

        val scrutineeVarId = lctx.uniqueVarName("_match_scrutinee")
        val scrutineeVar = lvNewLazyIdVar(
          scrutineeVarId,
          SIRType.Integer,
          PrimitiveRepresentation.Constant,
          loweredScrutinee.toRepresentation(PairIntDataList, matchData.scrutinee.anns.pos),
          matchData.scrutinee.anns.pos
        )

        val constrIdxVarId = lctx.uniqueVarName("_match_constr_idx")
        val constrIdxVar = lvNewLazyIdVar(
          constrIdxVarId,
          SIRType.Integer,
          PrimitiveRepresentation.Constant,
          lvBuiltinApply(
            SIRBuiltins.fstPair,
            scrutineeVar,
            SIRType.Integer,
            PrimitiveRepresentation.Constant,
            matchData.scrutinee.anns.pos
          ),
          matchData.scrutinee.anns.pos
        )

        val dataListVarId = lctx.uniqueVarName("_match_datalist")
        val dataListVar = lvNewLazyIdVar(
          dataListVarId,
          SIRType.List(SIRType.Data),
          SumCaseClassRepresentation.SumDataList,
          lvBuiltinApply(
            SIRBuiltins.sndPair,
            scrutineeVar,
            SIRType.List(SIRType.Data),
            SumCaseClassRepresentation.SumDataList,
            matchData.scrutinee.anns.pos
          ),
          matchData.scrutinee.anns.pos
        )

        val lastTerm = lctx.lower(
          SIR.Error(
            s"Incorrect constructor index for type ${loweredScrutinee.sirType.show}",
            matchData.anns
          )
        )

        val body = orderedCases.zipWithIndex.foldRight(lastTerm) {
            case ((sirCase, caseIndex), state) =>
                val body = genMatchDataConstrCase(sirCase, dataListVar, optTargetType, false)
                lvIfThenElse(
                  lvEqualsInteger(
                    constrIdxVar,
                    lvIntConstant(caseIndex, sirCase.anns.pos),
                    body.pos
                  ),
                  body,
                  state,
                  sirCase.anns.pos
                )
        }

        lctx.scope = prevScope

        ScopeBracketsLoweredValue(
          Set(scrutineeVar, constrIdxVar, dataListVar),
          body
        )
    }

    def genMatchDataConstrCase(
        sirCase: SIR.Case,
        dataListVar: IdentifiableLoweredValue,
        optTargetType: Option[SIRType],
        addDataListToScope: Boolean
    )(using lctx: LoweringContext): LoweredValue = {
        val prevScope = lctx.scope

        val constrPattern = sirCase.pattern match
            case p: Pattern.Constr => p
            case _ =>
                throw new LoweringException(
                  s"Expected constructor pattern, got ${sirCase.pattern}",
                  sirCase.anns.pos
                )

        val dataListId = dataListVar.id

        val listDataType = SIRType.List(SIRType.Data)

        val constrDecl = constrPattern.constr
        if constrDecl.params.length != constrPattern.bindings.length then
            throw new LoweringException(
              s"Expected ${constrDecl.params.length} bindings, got ${constrPattern.bindings.length} for constructor ${constrDecl.name}",
              sirCase.anns.pos
            )

        val scopedVars0 =
            if addDataListToScope then Set(dataListVar) else Set.empty[IdentifiableLoweredValue]

        val (lastTail, scopedVars, n) =
            constrPattern.bindings.zip(constrDecl.params).foldLeft((dataListVar, scopedVars0, 0)) {
                case ((currentTail, currentSet, idx), (name, typeBinding)) =>
                    val tp0 = typeBinding.tp
                    val prevId = currentTail.id
                    val tp = lctx.resolveTypeVarIfNeeded(tp0)
                    val tpDataRepresentation =
                        lctx.typeGenerator(tp).defaultDataRepresentation(tp)
                    val bindedVar = lvNewLazyNamedVar(
                      name,
                      tp,
                      tpDataRepresentation,
                      lvBuiltinApply(
                        SIRBuiltins.headList,
                        currentTail,
                        tp,
                        tpDataRepresentation,
                        sirCase.anns.pos
                      ),
                      sirCase.anns.pos
                    )
                    val nextTailId = s"${currentTail.id}_t"
                    // mb we already have this id in the scope
                    val nextTailVar =
                        lctx.scope.get(nextTailId, SumCaseClassRepresentation.SumDataList) match
                            case Some(v) => v
                            case None =>
                                lvNewLazyIdVar(
                                  nextTailId,
                                  listDataType,
                                  SumCaseClassRepresentation.SumDataList,
                                  lvBuiltinApply(
                                    SIRBuiltins.tailList,
                                    currentTail,
                                    listDataType,
                                    SumCaseClassRepresentation.SumDataList,
                                    sirCase.anns.pos
                                  ),
                                  sirCase.anns.pos
                                )
                    (nextTailVar, currentSet + nextTailVar, idx + 1)
            }

        // now with the all named variable in the scope we can generate the body
        val body = lctx.lower(sirCase.body, optTargetType)

        lctx.scope = prevScope

        ScopeBracketsLoweredValue(scopedVars, body)
    }

    def genMatchUplcConstr(
        mathData: SIR.Match,
        loweredScrutinee: LoweredValue,
        optTargetType: Option[SIRType]
    )(using lctx: LoweringContext): LoweredValue = {
        ???
    }

    def findConstructors(sirType: SIRType, pos: SIRPosition): Seq[ConstrDecl] = {
        sirType match
            case SIRType.CaseClass(constrDecl, typeArgs, optParent) =>
                optParent match
                    case None         => Seq(constrDecl)
                    case Some(parent) => findConstructors(parent, pos)
            case SIRType.SumCaseClass(decl, _) =>
                decl.constructors
            case SIRType.TypeLambda(_, t) => findConstructors(t, pos)
            case SIRType.TypeProxy(ref) =>
                findConstructors(ref, pos)
            case _ =>
                throw new IllegalArgumentException(
                  s"Expected case class type, got ${sirType} at match at ${pos}"
                )
    }

    def retrieveDataDecl(tp: SIRType, pos: SIRPosition): DataDecl = {
        SIRType.retrieveDataDecl(tp) match
            case Right(decl) => decl
            case Left(msg) =>
                throw LoweringException(
                  s"Can't retrieve data declaration from ${tp.show}: $msg",
                  pos
                )
    }

}
