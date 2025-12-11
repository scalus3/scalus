package scalus.compiler.sir.lowering.typegens

import scalus.compiler.sir.lowering.*
import scalus.compiler.sir.{AnnotationsDecl, SIR, SIRBuiltins, SIRPosition, SIRType}
import scalus.compiler.sir.SIR.Pattern

/** Type generator for the builtin Data type.
  *
  * Data is a special Plutus type that can represent arbitrary structured data. It has 5
  * constructors:
  *   - Constr(tag: Integer, args: List[Data])
  *   - Map(pairs: List[(Data, Data)])
  *   - List(elements: List[Data])
  *   - I(value: Integer)
  *   - B(value: ByteString)
  *
  * Pattern matching on Data requires PlutusV4's Case on Data instruction.
  */
object SIRTypeUplcDataGenerator extends SirTypeUplcGenerator {
    import LoweredValue.Builder.*

    override def defaultRepresentation(tp: SIRType)(using
        LoweringContext
    ): LoweredValueRepresentation =
        SumCaseClassRepresentation.DataData

    override def defaultDataRepresentation(tp: SIRType)(using
        LoweringContext
    ): LoweredValueRepresentation =
        SumCaseClassRepresentation.DataData

    override def defaultTypeVarReperesentation(tp: SIRType)(using
        LoweringContext
    ): LoweredValueRepresentation =
        SumCaseClassRepresentation.DataData

    override def isDataSupported(tp: SIRType)(using LoweringContext): Boolean =
        true

    override def toRepresentation(
        input: LoweredValue,
        outputRepresentation: LoweredValueRepresentation,
        pos: SIRPosition
    )(using lctx: LoweringContext): LoweredValue = {
        // Data is default representation, so we should not cases, except TypeVae
        outputRepresentation match {
            case SumCaseClassRepresentation.DataData => input
            case TypeVarRepresentation(isBuiltin)    => input
            case _ =>
                throw LoweringException(
                  s"Conversion Data to representation $outputRepresentation should not happen",
                  pos
                )
        }
    }

    override def upcastOne(input: LoweredValue, targetType: SIRType, pos: SIRPosition)(using
        LoweringContext
    ): LoweredValue = {
        SIRType.collectProd(input.sirType) match
            case None =>
                // it's not a product, so upcase
                SIRType.collectSum(input.sirType) match
                    case None =>
                        throw LoweringException(
                          s"Cannot upcast non-sum, non-product type ${input.sirType} to ${targetType}",
                          pos
                        )
                    case Some((_, dataDecl, _)) =>
                        if dataDecl.name == SIRType.Data.name then {
                            // it's already Data
                            input
                        } else {
                            throw LoweringException(
                              s"Upcasting from Data to other sum types is not supported, got ${dataDecl.name}",
                              pos
                            )
                        }
            case Some((_, constrDecl, _)) =>
                val name = constrDecl.name
                if name == SIRType.Data.B.name
                    || name == SIRType.Data.I.name
                    || name == SIRType.Data.List.name
                    || name == SIRType.Data.Map.name
                    || name == SIRType.Data.Constr.name
                then
                    // it's already Data variant, upcast to Data
                    TypeRepresentationProxyLoweredValue(
                      input,
                      targetType,
                      SumCaseClassRepresentation.DataData,
                      pos
                    )
                else
                    throw LoweringException(
                      s"Upcasting from product type ${constrDecl.name} should not happen to ${targetType}",
                      pos
                    )
    }

    override def genConstr(constr: SIR.Constr)(using lctx: LoweringContext): LoweredValue = {
        val pos = constr.anns.pos
        constr.name match {
            case SIRType.Data.I.name =>
                // Data.I(value: Integer) => iData(value)
                val argLowered = lctx.lower(constr.args.head)
                // If arg is already packed as Data, just retype it to Data.I
                if argLowered.representation.isPackedData then
                    TypeRepresentationProxyLoweredValue(
                      argLowered,
                      SIRType.Data.tp,
                      SumCaseClassRepresentation.DataData,
                      pos
                    )
                else
                    // Convert to Constant representation and apply iData
                    val argAsConstant =
                        argLowered.toRepresentation(PrimitiveRepresentation.Constant, pos)
                    lvBuiltinApply(
                      SIRBuiltins.iData,
                      argAsConstant,
                      SIRType.Data.tp,
                      SumCaseClassRepresentation.DataData,
                      pos
                    )

            case SIRType.Data.B.name =>
                // Data.B(value: ByteString) => bData(value)
                val argLowered = lctx.lower(constr.args.head)
                // If arg is already packed as Data, just retype it to Data.B
                if argLowered.representation.isPackedData then
                    TypeRepresentationProxyLoweredValue(
                      argLowered,
                      SIRType.Data.tp,
                      SumCaseClassRepresentation.DataData,
                      pos
                    )
                else
                    // Convert to Constant representation and apply bData
                    val argAsConstant =
                        argLowered.toRepresentation(PrimitiveRepresentation.Constant, pos)
                    lvBuiltinApply(
                      SIRBuiltins.bData,
                      argAsConstant,
                      SIRType.Data.tp,
                      SumCaseClassRepresentation.DataData,
                      pos
                    )

            case SIRType.Data.List.name =>
                // Data.List(values: List[Data]) => listData(values)
                // The argument is List[Data] which lowers to SumDataList (BuiltinList[Data])
                val argLowered = lctx.lower(constr.args.head)
                val argAsSumDataList =
                    argLowered.toRepresentation(SumCaseClassRepresentation.SumDataList, pos)
                lvBuiltinApply(
                  SIRBuiltins.listData,
                  argAsSumDataList,
                  SIRType.Data.tp,
                  SumCaseClassRepresentation.DataData,
                  pos
                )

            case SIRType.Data.Map.name =>
                // Data.Map(values: List[(Data, Data)]) => mapData(values)
                // The argument is List[(Data, Data)] which lowers to SumDataPairList
                val argLowered = lctx.lower(constr.args.head)
                val argAsSumDataPairList =
                    argLowered.toRepresentation(SumCaseClassRepresentation.SumDataPairList, pos)
                lvBuiltinApply(
                  SIRBuiltins.mapData,
                  argAsSumDataPairList,
                  SIRType.Data.tp,
                  SumCaseClassRepresentation.DataData,
                  pos
                )

            case SIRType.Data.Constr.name =>
                // Data.Constr(tag: Integer, args: List[Data]) => constrData(tag, args)
                val tagLowered = lctx.lower(constr.args(0))
                val tagAsConstant =
                    tagLowered.toRepresentation(PrimitiveRepresentation.Constant, pos)
                val argsLowered = lctx.lower(constr.args(1))
                val argsAsSumDataList =
                    argsLowered.toRepresentation(SumCaseClassRepresentation.SumDataList, pos)
                lvBuiltinApply2(
                  SIRBuiltins.constrData,
                  tagAsConstant,
                  argsAsSumDataList,
                  SIRType.Data.tp,
                  SumCaseClassRepresentation.DataData,
                  pos
                )

            case other =>
                throw LoweringException(
                  s"Unknown Data constructor: $other",
                  pos
                )
        }
    }

    override def genSelect(sel: SIR.Select, loweredScrutinee: LoweredValue)(using
        LoweringContext
    ): LoweredValue =
        ???

    override def genMatch(
        matchData: SIR.Match,
        loweredScrutinee: LoweredValue,
        optTargetType: Option[SIRType]
    )(using lctx: LoweringContext): LoweredValue = {
        val cases = matchData.cases
        val anns = matchData.anns

        // Data has exactly 5 constructors in fixed order:
        // 0: Constr (tag: Integer, args: List[Data])
        // 1: Map (entries: List[(Data, Data)])
        // 2: List (elements: List[Data])
        // 3: I (value: Integer)
        // 4: B (value: ByteString)

        // Find or create branch for each Data variant
        // If a wildcard is present, use it for unmatched cases
        val wildcardCase = cases.find(_.pattern == Pattern.Wildcard)

        def findCaseForVariant(variantName: String): Option[SIR.Case] =
            cases.find {
                case SIR.Case(Pattern.Constr(constrDecl, _, _), _, _) =>
                    constrDecl.name == variantName
                case _ => false
            }

        def errorBranch(msg: String): SIR =
            SIR.Error(msg, anns)

        // Get the case for each variant, or use wildcard, or generate error
        val constrCase = findCaseForVariant(SIRType.Data.Constr.name)
            .orElse(wildcardCase)
            .getOrElse(SIR.Case(Pattern.Wildcard, errorBranch("Unmatched Data.Constr"), anns))

        val mapCase = findCaseForVariant(SIRType.Data.Map.name)
            .orElse(wildcardCase)
            .getOrElse(SIR.Case(Pattern.Wildcard, errorBranch("Unmatched Data.Map"), anns))

        val listCase = findCaseForVariant(SIRType.Data.List.name)
            .orElse(wildcardCase)
            .getOrElse(SIR.Case(Pattern.Wildcard, errorBranch("Unmatched Data.List"), anns))

        val iCase = findCaseForVariant(SIRType.Data.I.name)
            .orElse(wildcardCase)
            .getOrElse(SIR.Case(Pattern.Wildcard, errorBranch("Unmatched Data.I"), anns))

        val bCase = findCaseForVariant(SIRType.Data.B.name)
            .orElse(wildcardCase)
            .getOrElse(SIR.Case(Pattern.Wildcard, errorBranch("Unmatched Data.B"), anns))

        val prevScope = lctx.scope

        // Generate bound variables and branches for each case
        // Constr: bindings are [tag, args] or [_, _] if wildcard
        val (constrTagVar, constrArgsVar, constrBranch) = genConstrBranch(constrCase, anns.pos)

        // Map: bindings are [entries] or [_] if wildcard
        val (mapEntriesVar, mapBranchLv) = genSingleArgBranch(
          mapCase,
          "_map_entries",
          SIRType.List(SIRType.Tuple2(SIRType.Data.tp, SIRType.Data.tp)),
          SumCaseClassRepresentation.SumDataPairList,
          anns.pos,
          optTargetType
        )

        // List: bindings are [elements] or [_] if wildcard
        val (listElementsVar, listBranchLv) = genSingleArgBranch(
          listCase,
          "_list_elements",
          SIRType.List(SIRType.Data.tp),
          SumCaseClassRepresentation.SumDataList,
          anns.pos,
          optTargetType
        )

        // I: bindings are [value] or [_] if wildcard
        val (iValueVar, iBranchLv) = genSingleArgBranch(
          iCase,
          "_i_value",
          SIRType.Integer,
          PrimitiveRepresentation.Constant,
          anns.pos,
          optTargetType
        )

        // B: bindings are [value] or [_] if wildcard
        val (bValueVar, bBranchLv) = genSingleArgBranch(
          bCase,
          "_b_value",
          SIRType.ByteString,
          PrimitiveRepresentation.Constant,
          anns.pos,
          optTargetType
        )

        lctx.scope = prevScope

        lvCaseData(
          loweredScrutinee,
          constrTagVar,
          constrArgsVar,
          constrBranch,
          mapEntriesVar,
          mapBranchLv,
          listElementsVar,
          listBranchLv,
          iValueVar,
          iBranchLv,
          bValueVar,
          bBranchLv,
          anns.pos,
          optTargetType
        )
    }

    /** Generate the Constr branch with two bound variables: tag and args. Lambda-bound variables
      * for Case don't have an rhs - they receive their value from the Case instruction.
      */
    private def genConstrBranch(
        sirCase: SIR.Case,
        pos: SIRPosition
    )(using
        lctx: LoweringContext
    ): (IdentifiableLoweredValue, IdentifiableLoweredValue, LoweredValue) = {
        val prevScope = lctx.scope

        val (tagName, argsName) = sirCase.pattern match {
            case Pattern.Constr(_, bindings, _) if bindings.length >= 2 =>
                (bindings(0), bindings(1))
            case Pattern.Constr(_, bindings, _) if bindings.length == 1 =>
                (bindings(0), lctx.uniqueVarName("_constr_args"))
            case _ =>
                (lctx.uniqueVarName("_constr_tag"), lctx.uniqueVarName("_constr_args"))
        }

        val tagVarId = lctx.uniqueVarName(tagName)
        val tagVar = new VariableLoweredValue(
          id = tagVarId,
          name = tagName,
          sir = SIR.Var(tagName, SIRType.Integer, AnnotationsDecl(pos)),
          representation = PrimitiveRepresentation.Constant,
          optRhs = None
        )
        lctx.scope = lctx.scope.add(tagVar)

        val argsVarId = lctx.uniqueVarName(argsName)
        val argsVar = new VariableLoweredValue(
          id = argsVarId,
          name = argsName,
          sir = SIR.Var(argsName, SIRType.List(SIRType.Data.tp), AnnotationsDecl(pos)),
          representation = SumCaseClassRepresentation.SumDataList,
          optRhs = None
        )
        lctx.scope = lctx.scope.add(argsVar)

        val body = lctx.lower(sirCase.body, None)

        lctx.scope = prevScope

        (tagVar, argsVar, body)
    }

    /** Generate a branch with a single bound variable. Lambda-bound variables for Case don't have
      * an rhs - they receive their value from the Case instruction.
      */
    private def genSingleArgBranch(
        sirCase: SIR.Case,
        defaultVarName: String,
        varType: SIRType,
        varRepr: LoweredValueRepresentation,
        pos: SIRPosition,
        optTargetType: Option[SIRType]
    )(using lctx: LoweringContext): (IdentifiableLoweredValue, LoweredValue) = {
        val prevScope = lctx.scope

        val varName = sirCase.pattern match {
            case Pattern.Constr(_, bindings, _) if bindings.nonEmpty =>
                bindings.head
            case _ =>
                lctx.uniqueVarName(defaultVarName)
        }

        val varId = lctx.uniqueVarName(varName)
        val boundVar = new VariableLoweredValue(
          id = varId,
          name = varName,
          sir = SIR.Var(varName, varType, AnnotationsDecl(pos)),
          representation = varRepr,
          optRhs = None
        )
        lctx.scope = lctx.scope.add(boundVar)

        val body = lctx.lower(sirCase.body, optTargetType)

        lctx.scope = prevScope

        (boundVar, body)
    }

}
