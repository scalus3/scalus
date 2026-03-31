package scalus.compiler.sir.lowering
package typegens

import scalus.cardano.ledger.MajorProtocolVersion
import scalus.compiler.sir.lowering.LoweredValue.Builder.*
import scalus.compiler.sir.*
import scalus.compiler.sir.SIR.Pattern
import scalus.uplc.{Term, UplcAnnotation}
import org.typelevel.paiges.Doc

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

    override def canBeConvertedToData(tp: SIRType)(using lctx: LoweringContext): Boolean = true

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
            case (DataConstr, SumBuiltinList(elementRepr)) =>
                // Nil or Cons(head, tail).  In theory should go to SumCommonList
                toRepresentation(input, PairIntDataList, pos).toRepresentation(
                  SumBuiltinList(elementRepr),
                  pos
                )
            case (DataConstr, PackedSumDataList) =>
                val elemType =
                    SumBuiltinList.retrieveListElementType(input.sirType).getOrElse(SIRType.Data.tp)
                val elemRepr = lctx.typeGenerator(elemType).defaultDataRepresentation(elemType)
                val asDataList = toRepresentation(input, SumBuiltinList(elemRepr), pos)
                lvBuiltinApply(
                  SIRBuiltins.listData,
                  asDataList,
                  input.sirType,
                  PackedSumDataList,
                  pos
                )
            case (DataConstr, outSum: SumUplcConstr) =>
                // DataConstr → SumUplcConstr: unConstrData → (tag, dataFields) →
                // dispatch on tag → rebuild Constr(tag, [converted fields])
                val asConstr = input.toRepresentation(PairIntDataList, pos)
                val tagVar = lvNewLazyIdVar(
                  lctx.uniqueVarName("_dc_tag"),
                  SIRType.Integer,
                  PrimitiveRepresentation.Constant,
                  lvBuiltinApply(
                    SIRBuiltins.fstPair,
                    asConstr,
                    SIRType.Integer,
                    PrimitiveRepresentation.Constant,
                    pos
                  ),
                  pos
                )
                val dataListRepr = SumCaseClassRepresentation.SumBuiltinList(DataData)
                val dataListVar = lvNewLazyIdVar(
                  lctx.uniqueVarName("_dc_fields"),
                  SIRType.List(SIRType.Data.tp),
                  dataListRepr,
                  lvBuiltinApply(
                    SIRBuiltins.sndPair,
                    asConstr,
                    SIRType.List(SIRType.Data.tp),
                    dataListRepr,
                    pos
                  ),
                  pos
                )
                // For each variant, build a branch that extracts fields and rebuilds as Constr
                val constructors = SumCaseSirTypeGenerator.findConstructors(input.sirType, pos)
                val branches = constructors.zipWithIndex.map { (constrDecl, idx) =>
                    val variantRepr = outSum.variants.getOrElse(
                      idx,
                      ProductCaseClassRepresentation.ProdUplcConstr(
                        idx,
                        constrDecl.params.map { p =>
                            val tp = lctx.resolveTypeVarIfNeeded(p.tp)
                            lctx.typeGenerator(tp).defaultDataRepresentation(tp)
                        }
                      )
                    )
                    // Extract fields from dataListVar via headList/tailList
                    var currentList: LoweredValue = dataListVar
                    val fields =
                        constrDecl.params.zip(variantRepr.fieldReprs).map { (param, fieldRepr) =>
                            val tp = lctx.resolveTypeVarIfNeeded(param.tp)
                            val dataRepr = lctx.typeGenerator(tp).defaultDataRepresentation(tp)
                            val head =
                                lvBuiltinApply(SIRBuiltins.headList, currentList, tp, dataRepr, pos)
                            val tail = lvBuiltinApply(
                              SIRBuiltins.tailList,
                              currentList,
                              SIRType.List(SIRType.Data.tp),
                              dataListRepr,
                              pos
                            )
                            currentList = tail
                            head.toRepresentation(fieldRepr, pos)
                        }
                    // Build Constr(idx, [fields])
                    val inPos = pos
                    new ComplexLoweredValue(Set.empty, fields*) {
                        override def sirType = input.sirType
                        override def representation = variantRepr
                        override def pos = inPos
                        override def termInternal(gctx: TermGenerationContext) =
                            Term.Constr(
                              scalus.cardano.ledger.Word64(idx.toLong),
                              fields.map(_.termWithNeededVars(gctx)).toList,
                              UplcAnnotation(inPos)
                            )
                        override def docDef(ctx: LoweredValue.PrettyPrintingContext) =
                            Doc.text(s"DataConstr→UplcConstr($idx)")
                        override def docRef(ctx: LoweredValue.PrettyPrintingContext) = docDef(ctx)
                    }: LoweredValue
                }
                // Dispatch on tag using CaseInteger (PlutusV4) or if-then-else chain
                val result =
                    if lctx.targetProtocolVersion >= MajorProtocolVersion.vanRossemPV then
                        lvCaseInteger(tagVar, branches.toList, pos, None)
                    else
                        branches.zipWithIndex.foldRight(branches.last: LoweredValue) {
                            case ((branch, idx), acc) =>
                                if idx == branches.size - 1 then acc
                                else
                                    lvIfThenElse(
                                      lvEqualsInteger(tagVar, lvIntConstant(idx, pos), pos),
                                      branch,
                                      acc,
                                      pos
                                    )
                        }
                ScopeBracketsLoweredValue(Set(tagVar, dataListVar), result)
            case (SumBuiltinList(_), DataConstr) =>
                ???
            case (SumBuiltinList(inElemRepr), out @ SumBuiltinList(outElemRepr)) =>
                if inElemRepr == outElemRepr then input
                else
                    new SumBuiltinListSirTypeGenerator(inElemRepr)
                        .toRepresentation(input, out, pos)
            case (SumBuiltinList(inElementRepr), PackedSumDataList) =>
                val elemRepr =
                    lctx.typeGenerator(SIRType.Data.tp).defaultDataRepresentation(SIRType.Data.tp)
                val asDataList = input.toRepresentation(SumBuiltinList(elemRepr), pos)
                lvBuiltinApply(
                  SIRBuiltins.listData,
                  asDataList,
                  input.sirType,
                  PackedSumDataList,
                  pos
                )
            case (SumBuiltinList(_), _: SumUplcConstr) =>
                ???
            case (PackedSumDataList, SumBuiltinList(outElemRepr)) =>
                val elemType =
                    SumBuiltinList.retrieveListElementType(input.sirType).getOrElse(SIRType.Data.tp)
                val elemRepr = lctx.typeGenerator(elemType).defaultDataRepresentation(elemType)
                val asDataList = lvBuiltinApply(
                  SIRBuiltins.unListData,
                  input,
                  input.sirType,
                  SumBuiltinList(elemRepr),
                  pos
                )
                if outElemRepr == elemRepr then asDataList
                else asDataList.toRepresentation(SumBuiltinList(outElemRepr), pos)
            case (PackedSumDataList, PackedSumDataList) =>
                input
            case (PackedSumDataList, DataConstr) =>
                val elemType =
                    SumBuiltinList.retrieveListElementType(input.sirType).getOrElse(SIRType.Data.tp)
                val elemRepr = lctx.typeGenerator(elemType).defaultDataRepresentation(elemType)
                val asDataList = toRepresentation(input, SumBuiltinList(elemRepr), pos)
                asDataList.toRepresentation(DataConstr, pos)
            case (PackedSumDataList, _: SumUplcConstr) =>
                ???
            case (_: SumUplcConstr, DataConstr) =>
                ???
            case (_: SumUplcConstr, SumBuiltinList(_)) =>
                ???
            case (_: SumUplcConstr, PackedSumDataList) =>
                val elemType =
                    SumBuiltinList.retrieveListElementType(input.sirType).getOrElse(SIRType.Data.tp)
                val elemRepr = lctx.typeGenerator(elemType).defaultDataRepresentation(elemType)
                val asDataList = toRepresentation(input, SumBuiltinList(elemRepr), pos)
                asDataList.toRepresentation(PackedSumDataList, pos)
            case (inTvr: TypeVarRepresentation, outRepr) =>
                if inTvr.isBuiltin then RepresentationProxyLoweredValue(input, representation, pos)
                else
                    val r0 = RepresentationProxyLoweredValue(input, DataConstr, pos)
                    toRepresentation(r0, representation, pos)
            // ProdUplcConstr → SumUplcConstr: check field reprs compatibility per variant
            case (inProd: ProductCaseClassRepresentation.ProdUplcConstr, outSum: SumUplcConstr) =>
                outSum.variants.get(inProd.tag) match
                    case Some(expectedProd)
                        if inProd.fieldReprs.size == expectedProd.fieldReprs.size
                            && inProd.fieldReprs.zip(expectedProd.fieldReprs).forall {
                                (inR, outR) =>
                                    inR.isCompatibleOn(SIRType.FreeUnificator, outR, pos)
                            } =>
                        // Field reprs compatible — same UPLC structure, proxy is safe
                        new RepresentationProxyLoweredValue(input, representation, pos)
                    case Some(expectedProd) =>
                        // Field reprs incompatible — convert each field
                        // TODO: unpack Constr fields, convert each, rebuild
                        throw LoweringException(
                          s"ProdUplcConstr → SumUplcConstr field repr mismatch for tag ${inProd.tag}: " +
                              s"got ${inProd.fieldReprs} but expected ${expectedProd.fieldReprs}",
                          pos
                        )
                    case None =>
                        // Tag not in target variants — proxy to requested representation
                        new RepresentationProxyLoweredValue(input, representation, pos)
            // SumUplcConstr → SumUplcConstr: check per-variant compatibility
            case (inSum: SumUplcConstr, outSum: SumUplcConstr) =>
                val allCompatible = inSum.variants.forall { (tag, inProd) =>
                    outSum.variants.get(tag) match
                        case None => true // unknown variant, allow
                        case Some(outProd) =>
                            inProd.fieldReprs.size == outProd.fieldReprs.size &&
                            inProd.fieldReprs.zip(outProd.fieldReprs).forall { (inR, outR) =>
                                inR.isCompatibleOn(SIRType.FreeUnificator, outR, pos)
                            }
                }
                if allCompatible then
                    new RepresentationProxyLoweredValue(input, representation, pos)
                else
                    throw LoweringException(
                      s"SumUplcConstr → SumUplcConstr variant repr mismatch",
                      pos
                    )
            case (inRepr, outTvr: TypeVarRepresentation) =>
                if outTvr.isBuiltin then input
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
            case SumBuiltinList(elemRepr) =>
                new SumBuiltinListSirTypeGenerator(elemRepr)
                    .genMatch(matchData, loweredScrutinee, optTargetType)
            case PackedSumDataList =>
                val elemType = SumBuiltinList
                    .retrieveListElementType(loweredScrutinee.sirType)
                    .getOrElse(SIRType.Data.tp)
                val elemRepr = lctx.typeGenerator(elemType).defaultDataRepresentation(elemType)
                val listRepr = SumBuiltinList(elemRepr)
                val unpacked = loweredScrutinee.toRepresentation(listRepr, matchData.anns.pos)
                val unpackedVar = lvNewLazyIdVar(
                  lctx.uniqueVarName("_unpacked"),
                  loweredScrutinee.sirType,
                  listRepr,
                  unpacked,
                  matchData.anns.pos
                )
                new SumBuiltinListSirTypeGenerator(elemRepr)
                    .genMatch(matchData, unpackedVar, optTargetType)
            case _: SumUplcConstr =>
                genMatchUplcConstr(
                  matchData,
                  loweredScrutinee,
                  optTargetType
                )
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

    case class PreparedCase(sirCase: SIR.Case, constrIndex: Option[Int], originIndex: Int)

    /** Prepares cases withour wildcards, ordered the same as in enum definition, for match
      * expression,
      *
      * @param matchData
      * @param loweredScrutinee
      * @param lctx
      * @return
      */
    private def prepareCases(
        matchData: SIR.Match,
        loweredScrutinee: LoweredValue,
    )(using LoweringContext): List[PreparedCase] = {

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
        val expandedCases = mutable.ArrayBuffer.empty[PreparedCase]

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
                            expandedCases += PreparedCase(c, Some(-1), idx)
                case SIR.Case(SIR.Pattern.Const(_), _, anns) =>
                    throw LoweringException(
                      s"Constant pattern not supported for sum type ${loweredScrutinee.sirType.show}",
                      anns.pos
                    )
                case SIR.Case(SIR.Pattern.Wildcard, rhs, anns) =>
                    // If we have a wildcard case, it must be the last one
                    if idx != enhancedCases.length - 1 then {
                        throw new IllegalArgumentException(
                          s"Wildcard case must be the last and only one in match expression"
                        )
                    } else
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
                            val newCase = SIR.Case(
                              Pattern.Constr(constrDecl, bindings, typeArgs),
                              rhs,
                              anns
                            )
                            expandedCases += PreparedCase(newCase, None, idx)
                            matchedConstructors += constrDecl.name // collect all matched constructors
                        }
            idx += 1
        end while

        // Sort the cases by the same order as the constructors

        val orderedCases = constructors.zipWithIndex.map { case (constr, constrIndex) =>
            val optExpandedCase = expandedCases
                .find(_.sirCase.pattern match {
                    case Pattern.Constr(constrDecl, _, _) => constrDecl.name == constr.name
                    case _                                => false
                })
                .map { pc =>
                    pc.constrIndex match
                        case Some(_) => pc.copy(constrIndex = Some(constrIndex))
                        case None    => pc // keep None for wildcard-expanded cases
                }
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

        val dataListElemRepr =
            lctx.typeGenerator(SIRType.Data.tp).defaultDataRepresentation(SIRType.Data.tp)
        val dataListRepr = SumCaseClassRepresentation.SumBuiltinList(dataListElemRepr)
        val dataListVarId = lctx.uniqueVarName("_match_datalist")
        val dataListVar = lvNewLazyIdVar(
          dataListVarId,
          SIRType.List(SIRType.Data.tp),
          dataListRepr,
          lvBuiltinApply(
            SIRBuiltins.sndPair,
            scrutineeVar,
            SIRType.List(SIRType.Data.tp),
            dataListRepr,
            matchData.scrutinee.anns.pos
          ),
          matchData.scrutinee.anns.pos
        )

        val orderedCases = prepareCases(matchData, loweredScrutinee)

        // For PlutusV4, use Case on integer directly since orderedCases are already 0..n-1
        val body = if lctx.targetProtocolVersion >= MajorProtocolVersion.vanRossemPV then {
            val branches = orderedCases.map { preparedCase =>
                genMatchDataConstrCase(preparedCase.sirCase, dataListVar, optTargetType, false)
            }
            lvCaseInteger(constrIdxVar, branches, matchData.anns.pos, optTargetType)
        } else {

            // Find wildcard-expanded case (if any) - these have constrIndex = None
            val wildcardOriginIndex = orderedCases.find(_.constrIndex.isEmpty).map(_.originIndex)

            // For the else branch: use wildcard body if present, otherwise use the last case
            // (for complete matches, the last case becomes the else branch - more efficient than Error)
            val (lastTerm, defaultOriginIndex) = wildcardOriginIndex match {
                case Some(origIdx) =>
                    // Find the wildcard case and use its body
                    val wc = orderedCases.find(_.originIndex == origIdx).get
                    (genMatchDataConstrCase(wc.sirCase, dataListVar, optTargetType, false), origIdx)
                case None =>
                    // Complete match: use the last case (highest constructor index) as default
                    val lastCase = orderedCases.last
                    (
                      genMatchDataConstrCase(lastCase.sirCase, dataListVar, optTargetType, false),
                      lastCase.originIndex
                    )
            }

            // Build if-then-else chain, skipping cases that share originIndex with the default
            orderedCases.foldRight(lastTerm) { (preparedCase, state) =>
                if preparedCase.originIndex == defaultOriginIndex then
                    state // this case is handled in lastTerm
                else
                    val caseBody =
                        genMatchDataConstrCase(
                          preparedCase.sirCase,
                          dataListVar,
                          optTargetType,
                          false
                        )
                    val constrIndex = preparedCase.constrIndex.get
                    lvIfThenElse(
                      lvEqualsInteger(
                        constrIdxVar,
                        lvIntConstant(constrIndex, preparedCase.sirCase.anns.pos),
                        caseBody.pos
                      ),
                      caseBody,
                      state,
                      preparedCase.sirCase.anns.pos
                    )
            }

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

        val listDataType = SIRType.List(SIRType.Data.tp)

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
                        lctx.scope.get(nextTailId, dataListVar.representation) match
                            case Some(v) => v
                            case None =>
                                lvNewLazyIdVar(
                                  nextTailId,
                                  listDataType,
                                  dataListVar.representation,
                                  lvBuiltinApply(
                                    SIRBuiltins.tailList,
                                    currentTail,
                                    listDataType,
                                    dataListVar.representation,
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

    /** Match on UplcConstr representation using the Case builtin.
      *
      * Case(scrutinee, [branch0, branch1, ...]) where each branch is a nested lambda that binds the
      * constructor's fields: λf0.λf1.…λfN.body
      *
      * Fields are bound with their native representations (from ProdUplcConstr.fieldReprs or
      * derived from the constructor's parameter types).
      */
    def genMatchUplcConstr(
        matchData: SIR.Match,
        loweredScrutinee: LoweredValue,
        optTargetType: Option[SIRType]
    )(using lctx: LoweringContext): LoweredValue = {

        val prevScope = lctx.scope
        val orderedCases = prepareCases(matchData, loweredScrutinee)
        val pos = matchData.anns.pos

        // For each ordered case, generate a branch lambda that binds the constructor's fields
        val branches = orderedCases.map { preparedCase =>
            genMatchUplcConstrCase(preparedCase.sirCase, optTargetType)
        }

        lctx.scope = prevScope

        // Determine result type and repr from the first branch
        val resultRepr = branches.head.representation
        val resultType = optTargetType.getOrElse(matchData.tp)

        // Generate Case(scrutinee, [branch0, branch1, ...])
        new ComplexLoweredValue(Set.empty, (loweredScrutinee :: branches.toList)*) {
            override def sirType: SIRType = resultType
            override def representation: LoweredValueRepresentation = resultRepr
            override def pos: SIRPosition = matchData.anns.pos

            override def termInternal(gctx: TermGenerationContext): Term = {
                Term.Case(
                  loweredScrutinee.termWithNeededVars(gctx),
                  branches.map(_.termWithNeededVars(gctx)).toList,
                  UplcAnnotation(matchData.anns.pos)
                )
            }

            override def docDef(ctx: LoweredValue.PrettyPrintingContext): Doc = {
                import Doc.*
                val branchDocs = branches.zipWithIndex.map { (b, i) =>
                    line + text(s"$i ->") + (lineOrSpace + b.docRef(ctx)).nested(2)
                }
                ((text("case") + space + loweredScrutinee.docRef(ctx) + space + text("of"))
                    + branchDocs.foldLeft(empty)(_ + _.grouped)).aligned
            }

            override def docRef(ctx: LoweredValue.PrettyPrintingContext): Doc = docDef(ctx)
        }
    }

    /** Generate a single branch for UplcConstr match. For Constr patterns: builds nested lambda
      * λf0.λf1.…λfN.body For Wildcard: just the body (no lambdas)
      */
    private def genMatchUplcConstrCase(
        sirCase: SIR.Case,
        optTargetType: Option[SIRType]
    )(using lctx: LoweringContext): LoweredValue = {
        val caseScope = lctx.scope
        val pos = sirCase.anns.pos

        sirCase.pattern match
            case constrPattern: Pattern.Constr =>
                val constrDecl = constrPattern.constr

                // Create variables for each field — fields are in native representation
                val fieldVars =
                    constrPattern.bindings.zip(constrDecl.params).map { (name, typeBinding) =>
                        val tp = lctx.resolveTypeVarIfNeeded(typeBinding.tp)
                        val fieldRepr = lctx.typeGenerator(tp).defaultRepresentation(tp)
                        val fieldVar = new VariableLoweredValue(
                          id = lctx.uniqueVarName(name),
                          name = name,
                          sir = SIR.Var(name, tp, AnnotationsDecl(pos)),
                          representation = fieldRepr
                        )
                        lctx.scope = lctx.scope.add(fieldVar)
                        fieldVar
                    }

                // Lower the case body with field variables in scope
                val body = lctx.lower(sirCase.body, optTargetType)
                lctx.scope = caseScope

                // Build nested lambda: λf0.λf1.…λfN.body
                fieldVars.foldRight(body) { (fieldVar, inner) =>
                    new ComplexLoweredValue(Set(fieldVar), inner) {
                        override def sirType: SIRType = inner.sirType
                        override def representation: LoweredValueRepresentation =
                            inner.representation
                        override def pos: SIRPosition = sirCase.anns.pos

                        override def termInternal(gctx: TermGenerationContext): Term = {
                            val innerCtx =
                                gctx.copy(generatedVars = gctx.generatedVars + fieldVar.id)
                            Term.LamAbs(
                              fieldVar.id,
                              inner.termWithNeededVars(innerCtx),
                              UplcAnnotation(sirCase.anns.pos)
                            )
                        }

                        override def docDef(ctx: LoweredValue.PrettyPrintingContext): Doc =
                            Doc.text(s"λ${fieldVar.name}.") + inner.docRef(ctx)

                        override def docRef(ctx: LoweredValue.PrettyPrintingContext): Doc =
                            docDef(ctx)
                    }
                }

            case Pattern.Wildcard =>
                val body = lctx.lower(sirCase.body, optTargetType)
                lctx.scope = caseScope
                body

            case other =>
                throw LoweringException(
                  s"Unexpected pattern in UplcConstr match: $other",
                  pos
                )
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
