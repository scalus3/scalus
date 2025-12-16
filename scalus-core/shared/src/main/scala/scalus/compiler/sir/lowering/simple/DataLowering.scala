package scalus.compiler.sir.lowering.simple

import scalus.cardano.ledger.Language
import scalus.compiler.sir.SIR.Pattern
import scalus.compiler.sir.*
import scalus.uplc.*

/** Trait providing Data type lowering support for simple lowering backends.
  *
  * Data is a builtin Plutus type with 5 constructors:
  *   - Constr(tag: Integer, args: List[Data])
  *   - Map(entries: List[(Data, Data)])
  *   - List(elements: List[Data])
  *   - I(value: Integer)
  *   - B(value: ByteString)
  *
  * This trait provides methods for:
  *   - Lowering Data constructors to UPLC builtins (iData, bData, listData, mapData, constrData)
  *   - Lowering pattern matches on Data (chooseData for V3, Case on Data for V4)
  *   - Lowering field selections on Data variants (unIData, unBData, etc.)
  */
trait DataLowering { self: BaseSimpleLowering =>

    /** Check if a SIR type is the builtin Data type. */
    protected def isDataType(sirType: SIRType): Boolean = SIRType.Data.unapply(sirType)

    /** Check if a constructor name is a Data type constructor. */
    protected def isDataConstructor(name: String): Boolean = name match {
        case SIRType.Data.I.name      => true
        case SIRType.Data.B.name      => true
        case SIRType.Data.List.name   => true
        case SIRType.Data.Map.name    => true
        case SIRType.Data.Constr.name => true
        case _                        => false
    }

    /** Check if a scrutinee type is a known Data variant (I, B, List, Map, Constr). */
    protected def isDataVariantType(sirType: SIRType): Boolean = sirType match {
        case SIRType.CaseClass(constrDecl, _, _) => isDataConstructor(constrDecl.name)
        case SIRType.SumCaseClass(decl, _) =>
            decl.name == SIRType.Data.name || (decl.constructors.length == 1 && isDataConstructor(
              decl.constructors.head.name
            ))
        case _ => false
    }

    /** Lower a Data constructor expression to UPLC.
      *
      * Data constructors are lowered to their corresponding builtin functions:
      *   - Data.I(value) → iData(value)
      *   - Data.B(value) → bData(value)
      *   - Data.List(values) → listData(values)
      *   - Data.Map(values) → mapData(values)
      *   - Data.Constr(tag, args) → constrData(tag, args)
      */
    protected def lowerDataConstr(
        name: String,
        args: List[SIR],
        anns: AnnotationsDecl
    ): Term = {
        name match {
            case SIRType.Data.I.name =>
                // Data.I(value: Integer) → iData(value)
                // Note: iData is not polymorphic, so builtinTerms already has the unforced term
                val valueTerm = self.lowerInner(args.head)
                self.builtinTerms(DefaultFun.IData) $ valueTerm

            case SIRType.Data.B.name =>
                // Data.B(value: ByteString) → bData(value)
                val valueTerm = self.lowerInner(args.head)
                self.builtinTerms(DefaultFun.BData) $ valueTerm

            case SIRType.Data.List.name =>
                // Data.List(values: List[Data]) → listData(values)
                val valuesTerm = self.lowerInner(args.head)
                self.builtinTerms(DefaultFun.ListData) $ valuesTerm

            case SIRType.Data.Map.name =>
                // Data.Map(values: List[(Data, Data)]) → mapData(values)
                val valuesTerm = self.lowerInner(args.head)
                self.builtinTerms(DefaultFun.MapData) $ valuesTerm

            case SIRType.Data.Constr.name =>
                // Data.Constr(tag: Integer, args: List[Data]) → constrData(tag, args)
                val tagTerm = self.lowerInner(args(0))
                val argsTerm = self.lowerInner(args(1))
                self.builtinTerms(DefaultFun.ConstrData) $ tagTerm $ argsTerm

            case _ =>
                val pos = anns.pos
                throw new IllegalArgumentException(
                  s"Unknown Data constructor: $name at ${pos.file}:${pos.startLine}, ${pos.startColumn}"
                )
        }
    }

    /** Lower a match expression on Data type.
      *
      * Data has 5 constructors in a fixed order:
      *   - 0: Constr (tag: Integer, args: List[Data])
      *   - 1: Map (entries: List[(Data, Data)])
      *   - 2: List (elements: List[Data])
      *   - 3: I (value: Integer)
      *   - 4: B (value: ByteString)
      *
      * For PlutusV4, use Case on Data instruction. For PlutusV3 and earlier, use chooseData
      * builtin.
      *
      * For V4 Case on Data:
      *   - Branches are lambdas that receive the inner values
      *   - Constr branch: \tag args -> body
      *   - Other branches: \value -> body
      *
      * For V3 chooseData:
      *   - chooseData just selects a delayed branch, it doesn't pass arguments
      *   - We bind the scrutinee first, then extract values within each branch
      */
    protected def lowerDataMatch(matchExpr: SIR.Match): Term = {
        val scrutinee = matchExpr.scrutinee
        val cases = matchExpr.cases
        val anns = matchExpr.anns
        val scrutineeTerm = self.lowerInner(scrutinee)
        val isUnchecked = anns.data.contains("unchecked")

        // Find case for each Data variant
        def findCaseForVariant(variantName: String): Option[SIR.Case] =
            cases.find {
                case SIR.Case(Pattern.Constr(constrDecl, _, _), _, _) =>
                    constrDecl.name == variantName
                case _ => false
            }

        val wildcardCase = cases.find(_.pattern == Pattern.Wildcard)

        def errorBranch(msg: String): SIR =
            SIR.Error(msg, anns)

        // Get case for each variant in order: Constr, Map, List, I, B
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

        if self.targetLanguage == Language.PlutusV4 then
            // For PlutusV4: Case on Data with 5 branches in order: Constr, Map, List, I, B
            // Branches are lambdas that receive the inner values
            val constrBranch = lowerDataCaseBranchV4(constrCase, SIRType.Data.Constr.name)
            val mapBranch = lowerDataCaseBranchV4(mapCase, SIRType.Data.Map.name)
            val listBranch = lowerDataCaseBranchV4(listCase, SIRType.Data.List.name)
            val iBranch = lowerDataCaseBranchV4(iCase, SIRType.Data.I.name)
            val bBranch = lowerDataCaseBranchV4(bCase, SIRType.Data.B.name)
            Term.Case(scrutineeTerm, List(constrBranch, mapBranch, listBranch, iBranch, bBranch))
        else
            // For PlutusV3 and earlier: use chooseData builtin
            // Bind scrutinee first, then use it in branches
            val scrutineeVar = "__data_scrutinee__"
            val scrutineeRef = Term.Var(NamedDeBruijn(scrutineeVar))

            val constrBranch =
                lowerDataCaseBranchV3(constrCase, SIRType.Data.Constr.name, scrutineeRef)
            val mapBranch = lowerDataCaseBranchV3(mapCase, SIRType.Data.Map.name, scrutineeRef)
            val listBranch = lowerDataCaseBranchV3(listCase, SIRType.Data.List.name, scrutineeRef)
            val iBranch = lowerDataCaseBranchV3(iCase, SIRType.Data.I.name, scrutineeRef)
            val bBranch = lowerDataCaseBranchV3(bCase, SIRType.Data.B.name, scrutineeRef)

            // (\scrutinee -> force(chooseData scrutinee (delay branch) ...)) data
            // chooseData returns the selected delayed branch, so we need to force it
            val chooseDataExpr = !(self.builtinTerms(DefaultFun.ChooseData) $
                scrutineeRef $
                ~constrBranch $
                ~mapBranch $
                ~listBranch $
                ~iBranch $
                ~bBranch)
            Term.Apply(Term.LamAbs(scrutineeVar, chooseDataExpr), scrutineeTerm)
    }

    /** Lower a single case branch for Data pattern matching (PlutusV4).
      *
      * For V4 Case on Data, branches are lambdas that receive the inner values directly:
      *   - Constr: \tag args -> body
      *   - Map: \entries -> body
      *   - List: \elements -> body
      *   - I: \value -> body
      *   - B: \value -> body
      *
      * Even for wildcards/no-bindings, we need lambdas that ignore the inner values.
      */
    protected def lowerDataCaseBranchV4(sirCase: SIR.Case, variantName: String): Term = {
        val bodyTerm = self.lowerInner(sirCase.body)

        // Get bindings from the pattern, or generate dummy ones for wildcards
        val bindings = sirCase.pattern match {
            case Pattern.Constr(_, bs, _) => bs
            case _                        => Nil
        }

        variantName match {
            case SIRType.Data.Constr.name =>
                // Constr branch receives tag and args (2 arguments)
                val tagBinding = if bindings.length >= 1 then bindings(0) else "_tag"
                val argsBinding = if bindings.length >= 2 then bindings(1) else "_args"
                Term.LamAbs(tagBinding, Term.LamAbs(argsBinding, bodyTerm))

            case SIRType.Data.Map.name =>
                // Map branch receives entries (1 argument)
                val valuesBinding = if bindings.nonEmpty then bindings(0) else "_entries"
                Term.LamAbs(valuesBinding, bodyTerm)

            case SIRType.Data.List.name =>
                // List branch receives elements (1 argument)
                val elementsBinding = if bindings.nonEmpty then bindings(0) else "_elements"
                Term.LamAbs(elementsBinding, bodyTerm)

            case SIRType.Data.I.name =>
                // I branch receives value (1 argument)
                val valueBinding = if bindings.nonEmpty then bindings(0) else "_value"
                Term.LamAbs(valueBinding, bodyTerm)

            case SIRType.Data.B.name =>
                // B branch receives value (1 argument)
                val valueBinding = if bindings.nonEmpty then bindings(0) else "_value"
                Term.LamAbs(valueBinding, bodyTerm)

            case _ =>
                throw new IllegalArgumentException(
                  s"Unknown Data variant: $variantName"
                )
        }
    }

    /** Lower a single case branch for Data pattern matching (PlutusV3).
      *
      * For V3 chooseData, branches are thunks that reference a pre-bound scrutinee. We extract
      * values from the scrutinee within each branch.
      */
    protected def lowerDataCaseBranchV3(
        sirCase: SIR.Case,
        variantName: String,
        scrutineeRef: Term
    ): Term = {
        sirCase.pattern match {
            case Pattern.Constr(_, bindings, _) if bindings.nonEmpty =>
                val bodyTerm = self.lowerInner(sirCase.body)
                variantName match {
                    case SIRType.Data.Constr.name if bindings.length >= 2 =>
                        // Extract pair using unConstrData, then fstPair/sndPair
                        val pairVar = "__constr_pair__"
                        val tagBinding = bindings(0)
                        val argsBinding = bindings(1)
                        val pairRef = Term.Var(NamedDeBruijn(pairVar))
                        val pairTerm = self.builtinTerms(DefaultFun.UnConstrData) $ scrutineeRef
                        val tagTerm = self.builtinTerms(DefaultFun.FstPair) $ pairRef
                        val argsTerm = self.builtinTerms(DefaultFun.SndPair) $ pairRef
                        Term.Apply(
                          Term.LamAbs(
                            pairVar,
                            Term.Apply(
                              Term.LamAbs(
                                tagBinding,
                                Term.Apply(Term.LamAbs(argsBinding, bodyTerm), argsTerm)
                              ),
                              tagTerm
                            )
                          ),
                          pairTerm
                        )

                    case SIRType.Data.Map.name if bindings.length >= 1 =>
                        val valuesBinding = bindings(0)
                        val valuesTerm = self.builtinTerms(DefaultFun.UnMapData) $ scrutineeRef
                        Term.Apply(Term.LamAbs(valuesBinding, bodyTerm), valuesTerm)

                    case SIRType.Data.List.name if bindings.length >= 1 =>
                        val elementsBinding = bindings(0)
                        val elementsTerm = self.builtinTerms(DefaultFun.UnListData) $ scrutineeRef
                        Term.Apply(Term.LamAbs(elementsBinding, bodyTerm), elementsTerm)

                    case SIRType.Data.I.name if bindings.length >= 1 =>
                        val valueBinding = bindings(0)
                        val valueTerm = self.builtinTerms(DefaultFun.UnIData) $ scrutineeRef
                        Term.Apply(Term.LamAbs(valueBinding, bodyTerm), valueTerm)

                    case SIRType.Data.B.name if bindings.length >= 1 =>
                        val valueBinding = bindings(0)
                        val valueTerm = self.builtinTerms(DefaultFun.UnBData) $ scrutineeRef
                        Term.Apply(Term.LamAbs(valueBinding, bodyTerm), valueTerm)

                    case _ =>
                        self.lowerInner(sirCase.body)
                }

            case Pattern.Constr(_, _, _) | Pattern.Wildcard =>
                self.lowerInner(sirCase.body)

            case Pattern.Const(_) =>
                val pos = sirCase.anns.pos
                throw new IllegalArgumentException(
                  s"Constant pattern not supported for Data at ${pos.file}:${pos.startLine}, ${pos.startColumn}"
                )
        }
    }

    /** Lower a field selection on Data type.
      *
      * Data field selections are lowered to their corresponding builtin unpack functions:
      *   - Data.I.value → unIData(scrutinee)
      *   - Data.B.value → unBData(scrutinee)
      *   - Data.List.values → unListData(scrutinee)
      *   - Data.Map.values → unMapData(scrutinee)
      *   - Data.Constr.constr → fstPair(unConstrData(scrutinee))
      *   - Data.Constr.args → sndPair(unConstrData(scrutinee))
      */
    protected def lowerDataSelect(
        scrutinee: SIR,
        field: String,
        tp: SIRType,
        anns: AnnotationsDecl
    ): Term = {
        val scrutineeTerm = self.lowerInner(scrutinee)

        // Determine the Data variant from the scrutinee type
        val constrName = scrutinee.tp match {
            case SIRType.CaseClass(constrDecl, _, _) => constrDecl.name
            case SIRType.SumCaseClass(decl, _) if decl.constructors.length == 1 =>
                decl.constructors.head.name
            case _ =>
                // Check if it's the Data sum type itself - need to look at field to determine variant
                field match {
                    case "value" =>
                        // Could be I or B, but unIData/unBData will fail at runtime if wrong
                        // This shouldn't happen in well-typed code
                        val pos = anns.pos
                        throw new IllegalArgumentException(
                          s"Cannot determine Data variant for field '$field' at ${pos.file}:${pos.startLine}, ${pos.startColumn}"
                        )
                    case "values" =>
                        // Could be List or Map
                        val pos = anns.pos
                        throw new IllegalArgumentException(
                          s"Cannot determine Data variant for field '$field' at ${pos.file}:${pos.startLine}, ${pos.startColumn}"
                        )
                    case "constr" | "args" =>
                        SIRType.Data.Constr.name
                    case _ =>
                        val pos = anns.pos
                        throw new IllegalArgumentException(
                          s"Unknown Data field '$field' at ${pos.file}:${pos.startLine}, ${pos.startColumn}"
                        )
                }
        }

        constrName match {
            case SIRType.Data.I.name =>
                // Data.I.value → unIData(scrutinee)
                // Note: unIData is not polymorphic
                self.builtinTerms(DefaultFun.UnIData) $ scrutineeTerm

            case SIRType.Data.B.name =>
                // Data.B.value → unBData(scrutinee)
                self.builtinTerms(DefaultFun.UnBData) $ scrutineeTerm

            case SIRType.Data.List.name =>
                // Data.List.values → unListData(scrutinee)
                self.builtinTerms(DefaultFun.UnListData) $ scrutineeTerm

            case SIRType.Data.Map.name =>
                // Data.Map.values → unMapData(scrutinee)
                self.builtinTerms(DefaultFun.UnMapData) $ scrutineeTerm

            case SIRType.Data.Constr.name =>
                field match {
                    case "constr" =>
                        // Data.Constr.constr → fstPair(unConstrData(scrutinee))
                        // unConstrData is not polymorphic, but fstPair IS polymorphic (builtinTerms handles this)
                        val pair = self.builtinTerms(DefaultFun.UnConstrData) $ scrutineeTerm
                        self.builtinTerms(DefaultFun.FstPair) $ pair
                    case "dataArgs" =>
                        // Data.Constr.args → sndPair(unConstrData(scrutinee))
                        val pair = self.builtinTerms(DefaultFun.UnConstrData) $ scrutineeTerm
                        self.builtinTerms(DefaultFun.SndPair) $ pair
                    case _ =>
                        val pos = anns.pos
                        throw new IllegalArgumentException(
                          s"Unknown field '$field' for Data.Constr at ${pos.file}:${pos.startLine}, ${pos.startColumn}"
                        )
                }

            case _ =>
                val pos = anns.pos
                throw new IllegalArgumentException(
                  s"Cannot select field from non-Data type: $constrName at ${pos.file}:${pos.startLine}, ${pos.startColumn}"
                )
        }
    }
}
