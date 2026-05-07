package scalus.compiler.sir.lowering
package typegens

import scalus.cardano.ledger.MajorProtocolVersion
import scalus.compiler.sir.lowering.LoweredValue.Builder.*
import scalus.compiler.sir.*
import scalus.compiler.sir.SIR.Pattern
import scala.collection.mutable

/** DataConstr representation emitter for sum case-class types.
  *
  * Owns the DataConstr-specific emission shapes for a Scala 3 sum (sealed-trait + case-class
  * hierarchy lowered to Cardano `Constr(tag, [field-data, ...])`):
  *
  *   - `genConstrLowered`: forwards to the variant case-class's own typegen (variants live as Prod
  *     types that know how to assemble their own bytes).
  *   - `genMatchDataConstr` / `genMatchDataConstrCase`: tag/field extraction via `unConstrData`,
  *     with `lvCaseInteger` (V4+) or if-then-else chain (V3) for branch dispatch.
  *   - `upcastOne`: synthesizes a parent-sum Constr by reusing the child's bytes under the parent's
  *     variant tag.
  *
  * Pre-Phase-4c-step-3 this was named `SumCaseSirTypeGenerator`; the rename aligns terminology with
  * the design doc (per-repr emitters at §3.2). `prepareCases`, `findConstructors`, and
  * `retrieveDataDecl` stay here as shared helpers because `SumUplcConstrEmitter` and
  * `ProductCaseSirTypeGenerator` both reuse them.
  */
object DataConstrEmitter extends SirTypeUplcGenerator {

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
    )(using lctx: LoweringContext): LoweredValue =
        SumDispatch.dispatcherBypass("DataConstrEmitter")

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

    override def genConstrLowered(
        constr: SIR.Constr,
        loweredArgs: scala.List[LoweredValue],
        optTargetType: Option[SIRType]
    )(using lctx: LoweringContext): LoweredValue = {
        // Routing decisions (parent-default check, tail-repr propagation,
        // `@UplcRepr(UplcConstr)` annotation, `inUplcConstrListScope`) live in
        // `SumDispatch.chooseConstrOutputRepr`. By the time we land here, the
        // dispatcher already routed the constr to this generator.
        val caseClassType = constr.data.constrType(constr.name)
        lctx.typeGenerator(caseClassType)
            .genConstrLowered(
              constr.copy(tp = caseClassType),
              loweredArgs,
              optTargetType
            )
    }

    override def genMatch(
        matchData: SIR.Match,
        loweredScrutinee: LoweredValue,
        optTargetType: Option[SIRType]
    )(using
        lctx: LoweringContext
    ): LoweredValue = {
        // Per-repr dispatch lives in `SumDispatch.genMatch` (Phase 4a). The recognized
        // reprs route there directly; unrecognized reprs would loop through
        // `SumDispatch`'s typegen-fallback back into this method, so we throw
        // explicitly to keep the failure mode visible.
        loweredScrutinee.representation match
            case DataConstr | _: SumBuiltinList | PackedSumDataList | _: SumUplcConstr |
                _: TypeVarRepresentation =>
                SumDispatch.genMatch(matchData, loweredScrutinee, optTargetType)
            case _ =>
                throw LoweringException(
                  s"Unsupported representation ${loweredScrutinee.representation} for match expression",
                  matchData.anns.pos
                )

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
    private[typegens] def prepareCases(
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
            SirTypeUplcGenerator.defaultDataRepresentation(SIRType.Data.tp)
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
                        SirTypeUplcGenerator.defaultDataRepresentation(tp)
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

    // genMatchUplcConstr and genMatchUplcConstrCase moved to SumUplcConstrEmitter

    def findConstructors(sirType: SIRType, pos: SIRPosition): Seq[ConstrDecl] = {
        sirType match
            case SIRType.CaseClass(constrDecl, typeArgs, optParent) =>
                optParent match
                    case None         => Seq(constrDecl)
                    case Some(parent) => findConstructors(parent, pos)
            case SIRType.SumCaseClass(decl, _) =>
                decl.constructors
            case SIRType.Annotated(tp, _) => findConstructors(tp, pos)
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

    /** Outbound conversion graph from a `DataConstr`-shaped value. Each row pairs the expected
      * source representation with the conversion step. The source repr is `DataConstr` for every
      * row here; the pair shape is what `ConversionStep`-using emitters declare per design §3.3, so
      * a future graph walker can read the table without asking the emitter who owns it.
      */
    def outboundStep(
        target: LoweredValueRepresentation
    ): Option[(LoweredValueRepresentation, ConversionStep)] =
        target match {
            case DataConstr =>
                Some((DataConstr, ConversionStep.Identity))
            case PairIntDataList =>
                Some((DataConstr, ConversionStep.Atomic(unConstrDataAtomic)))
            case _: SumBuiltinList | PackedSumDataList | _: SumUplcConstr =>
                Some((DataConstr, ConversionStep.Via(PairIntDataList)))
            case _ =>
                None
        }

    private val unConstrDataAtomic: ConversionStep.AtomicEmit =
        new ConversionStep.AtomicEmit {
            def emit(
                input: LoweredValue,
                target: LoweredValueRepresentation,
                pos: SIRPosition
            )(using LoweringContext): LoweredValue =
                lvBuiltinApply(SIRBuiltins.unConstrData, input, input.sirType, target, pos)
        }

    /** Convert from a `DataConstr` source to `target`. TypeVar-source/target handling lives in
      * `SumDispatch.sumCaseImpl`; only the targets listed in `outboundStep` reach this method. The
      * source-repr check enforces the declared row's invariant — `input.representation` must equal
      * the table's source.
      */
    def emitConvert(
        input: LoweredValue,
        target: LoweredValueRepresentation,
        pos: SIRPosition
    )(using lctx: LoweringContext): LoweredValue =
        outboundStep(target) match
            case Some((expectedSrc, step)) =>
                require(
                  input.representation == expectedSrc,
                  s"DataConstrEmitter.emitConvert: expected source $expectedSrc, got ${input.representation} for ${input.sirType.show}"
                )
                ConversionStep(step, input, target, pos)
            case None =>
                throw LoweringException(
                  s"DataConstrEmitter.emitConvert: unsupported target $target for ${input.sirType.show}",
                  pos
                )

}
