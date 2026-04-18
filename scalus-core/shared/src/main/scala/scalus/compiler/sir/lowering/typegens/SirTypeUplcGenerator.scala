package scalus.compiler.sir.lowering
package typegens

import scalus.compiler.sir.*

import java.util.IdentityHashMap

trait SirTypeUplcGenerator {

    /** Get default representation. Assumed that input parameters of functions and vars and are in
      * this representation.
      */
    def defaultRepresentation(tp: SIRType)(using LoweringContext): LoweredValueRepresentation

    /** Get default representation for data representation of this type. This representation is used
      * when converting to data instrucutres, which holds value as data.
      */
    def defaultDataRepresentation(tp: SIRType)(using LoweringContext): LoweredValueRepresentation

    /** Get default representation for type variable. This representation is used when converting to
      * parameters of scala functions with type parameters. (Usually the same as
      * defaultDataRepresentation, except Lambdas).
      */
    def defaultTypeVarReperesentation(tp: SIRType)(using
        LoweringContext
    ): LoweredValueRepresentation

    def canBeConvertedToData(tp: SIRType)(using
        lctx: LoweringContext
    ): Boolean

    def toRepresentation(
        input: LoweredValue,
        representation: LoweredValueRepresentation,
        pos: SIRPosition
    )(using
        lctx: LoweringContext
    ): LoweredValue

    def upcastOne(input: LoweredValue, targetType: SIRType, pos: SIRPosition)(using
        LoweringContext
    ): LoweredValue

    /** Generate constructor for this type. Always called on DataDecl.tp
      *
      * @param constr - constructor, which we should generate
      */
    def genConstr(constr: SIR.Constr)(using
        LoweringContext
    ): LoweredValue

    /** Generate constructor with pre-lowered arguments.
      *
      * Used when the caller has already lowered arguments (e.g., to inspect their repr
      * for Transparent TypeVar cascade). Default implementation ignores loweredArgs and
      * falls back to genConstr (which re-lowers args). Generators that support pre-lowered
      * args should override this.
      *
      * @param constr - constructor SIR node
      * @param loweredArgs - already-lowered constructor arguments
      * @param optTargetType - optional target type (e.g., for Nil typing)
      */
    def genConstrFromLowered(
        constr: SIR.Constr,
        loweredArgs: scala.List[LoweredValue],
        optTargetType: Option[SIRType] = None
    )(using LoweringContext): LoweredValue = genConstr(constr)

    def genSelect(sel: SIR.Select, loweredScrutinee: LoweredValue)(using
        LoweringContext
    ): LoweredValue

    def genMatch(
        matchData: SIR.Match,
        loweredScrutinee: LoweredValue,
        optTargetType: Option[SIRType]
    )(using
        LoweringContext
    ): LoweredValue

    /** Check if any lowered argument has a passthrough TypeVar representation (Transparent or
      * Unwrapped). When true, the constructor should cascade to UplcConstr representation because
      * such values can't be serialized to Data.
      */
    protected def hasTransparentTypeVarArgs(loweredArgs: scala.List[LoweredValue]): Boolean =
        loweredArgs.exists { arg =>
            arg.representation match
                case tvr: TypeVarRepresentation => tvr.isBuiltin
                case _                          => false
        }

}

object SirTypeUplcGenerator {

    /** Extracts the uplcRepr annotation value from AnnotationsDecl. */
    private def getUplcReprAnnotation(anns: AnnotationsDecl): Option[String] =
        anns.data.get("uplcRepr").collect {
            case SIR.Const(scalus.uplc.Constant.String(spec), _, _) => spec
        }

    /** Resolve a field's representation from its @UplcRepr annotation.
      *
      * Returns None if no annotation, otherwise resolves the annotation to a concrete
      * LoweredValueRepresentation based on the field's type.
      */
    def resolveFieldRepr(param: TypeBinding, paramType: SIRType)(using
        lctx: LoweringContext
    ): Option[LoweredValueRepresentation] = {
        param.annotations.data.get("uplcRepr").map { reprSir =>
            resolveReprAnnotation(reprSir, paramType)
        }
    }

    /** Resolve a @UplcRepr annotation SIR value to a concrete LoweredValueRepresentation.
      *
      * Handles:
      *   - String constants (e.g., "UplcConstr", "Data") — type-level repr tags
      *   - SIR.Constr with args (e.g., SumBuiltinList(UplcConstr)) — parameterized reprs
      */
    def resolveReprAnnotation(reprSir: SIR, fieldType: SIRType)(using
        lctx: LoweringContext
    ): LoweredValueRepresentation = {
        reprSir match
            case SIR.Const(scalus.uplc.Constant.String(name), _, _) =>
                resolveReprName(name, fieldType)
            case SIR.Constr(name, _, args, _, _) =>
                val shortName = name.split("\\$?\\.").last
                shortName match
                    case "SumBuiltinList" if args.size == 1 =>
                        val elemType = SumCaseClassRepresentation.SumBuiltinList
                            .retrieveListElementType(fieldType)
                            .getOrElse(SIRType.Data.tp)
                        val elemRepr = resolveReprAnnotation(args.head, elemType)
                        SumCaseClassRepresentation.SumBuiltinList(elemRepr)
                    case "ProdBuiltinPair" if args.size == 2 =>
                        val (fstType, sndType) =
                            ProductCaseClassRepresentation.ProdBuiltinPair
                                .extractPairComponentTypes(fieldType)
                        val fstRepr = resolveReprAnnotation(args(0), fstType)
                        val sndRepr = resolveReprAnnotation(args(1), sndType)
                        ProductCaseClassRepresentation.ProdBuiltinPair(fstRepr, sndRepr)
                    case _ =>
                        throw LoweringException(
                          s"Unsupported parameterized @UplcRepr annotation: $name",
                          SIRPosition.empty
                        )
            case _ =>
                throw LoweringException(
                  s"Cannot interpret @UplcRepr annotation: $reprSir",
                  SIRPosition.empty
                )
    }

    /** Resolve a @UplcRepr annotation SIR value to the appropriate SirTypeUplcGenerator. */
    private def resolveGeneratorFromAnnotation(reprSir: SIR, tp: SIRType, debug: Boolean)(using
        lctx: LoweringContext
    ): SirTypeUplcGenerator = {
        reprSir match
            case SIR.Const(scalus.uplc.Constant.String(name), _, _) =>
                name match
                    case "UplcConstr" =>
                        if SIRType.isSum(tp) then SumCaseUplcConstrSirTypeGenerator
                        else ProductCaseUplcConstrSirTypeGenerator
                    case "Data" => SIRTypeUplcDataGenerator
                    case _      => lctx.typeGenerator(tp)
            case _ =>
                // Parameterized annotations (SumBuiltinList etc.) — fall back to inner type
                lctx.typeGenerator(tp)
    }

    /** Resolve a type-level repr name to a concrete LoweredValueRepresentation for a given type. */
    private def resolveReprName(name: String, fieldType: SIRType)(using
        lctx: LoweringContext
    ): LoweredValueRepresentation = {
        name match
            case "UplcConstr" =>
                // Use UplcConstr generator's defaultRepresentation for this type
                if SIRType.isSum(fieldType) then
                    SumCaseUplcConstrSirTypeGenerator.defaultRepresentation(fieldType)
                else ProductCaseUplcConstrSirTypeGenerator.defaultRepresentation(fieldType)
            case "Data" =>
                lctx.typeGenerator(fieldType).defaultDataRepresentation(fieldType)
            case "DataData" =>
                SumCaseClassRepresentation.DataData
            case "DataConstr" =>
                SumCaseClassRepresentation.DataConstr
            case "Constant" =>
                PrimitiveRepresentation.Constant
            case _ =>
                // Fallback: use the type's own generator
                lctx.typeGenerator(fieldType).defaultRepresentation(fieldType)
    }

    /** Resolves the encoded UplcRepresentation string to the appropriate generator.
      * @param isSum
      *   true when resolving for a SumCaseClass type, false for a CaseClass (product) type
      */
    private def resolveUplcRepresentation(
        encoded: String,
        constrDecl: ConstrDecl,
        typeArgs: List[SIRType],
        debug: Boolean,
        isSum: Boolean = false
    )(using lctx: LoweringContext): SirTypeUplcGenerator =
        encoded match {
            case "ProductCase" => ProductCaseSirTypeGenerator
            case "SumCase"     => SumCaseSirTypeGenerator
            case "SumDataList" =>
                new SumBuiltinListSirTypeGenerator(PrimitiveRepresentation.PackedData)
            case "SumPairDataList"       => SumPairBuiltinListSirTypeGenerator
            case "PackedDataMap"         => MapSirTypeGenerator
            case "Data"                  => SIRTypeUplcDataGenerator
            case "BuiltinArray"          => BuiltinArraySirTypeGenerator
            case "ProductCaseOneElement" =>
                // Derive inner type from first constructor parameter
                val paramType = SIRType.substitute(
                  constrDecl.params.head.tp,
                  constrDecl.typeParams.zip(typeArgs).toMap,
                  Map.empty
                )
                val innerGenerator = SirTypeUplcGenerator(paramType, debug)
                ProductCaseOneElementSirTypeGenerator(innerGenerator)
            case "UplcConstr" =>
                if isSum then SumCaseUplcConstrSirTypeGenerator
                else ProductCaseUplcConstrSirTypeGenerator
            case other =>
                throw IllegalArgumentException(s"Unknown UplcRepresentation: $other")
        }

    def apply(tp: SIRType, debug: Boolean = false)(using
        lctx: LoweringContext
    ): SirTypeUplcGenerator = {
        val retval = tp match
            case SIRType.Boolean =>
                SIRTypeUplcBooleanGenerator
            case SIRType.Integer =>
                SIRTypeUplcIntegerGenerator
            case SIRType.ByteString =>
                SIRTypeUplcByteStringGenerator
            case SIRType.String =>
                SIRTypeUplcStringGenerator
            case SIRType.Unit =>
                UnitSirTypeGenerator
            case SIRType.SumCaseClass(decl, typeArgs) =>
                // Check for @UplcRepr annotation on the sum type
                getUplcReprAnnotation(decl.annotations)
                    .map { encoded =>
                        // For sum types, we need to pick a constructor to resolve the representation
                        // Use the first constructor if available
                        decl.constructors.headOption match {
                            case Some(constr) =>
                                resolveUplcRepresentation(
                                  encoded,
                                  constr,
                                  typeArgs,
                                  debug,
                                  isSum = true
                                )
                            case None =>
                                throw IllegalArgumentException(
                                  s"Sum type ${decl.name} has no constructors but has @UplcRepr annotation"
                                )
                        }
                    }
                    .getOrElse {
                        // Existing structural logic
                        val trace = new IdentityHashMap[SIRType, SIRType]()
                        if decl.name == SIRType.Data.name then SIRTypeUplcDataGenerator
                        else if decl.name == SumListCommonSirTypeGenerator.PairListDataDeclName then
                            if !containsFun(tp, trace) then SumPairBuiltinListSirTypeGenerator
                            else SumCaseUplcOnlySirTypeGenerator
                        else if decl.name == "scalus.cardano.onchain.plutus.prelude.List" then
                            if !containsFun(tp, trace) then {
                                val elemRepr = elementReprFor(typeArgs.head)
                                elemRepr match
                                    case _: ProductCaseClassRepresentation.ProdUplcConstr |
                                        _: SumCaseClassRepresentation.SumUplcConstr =>
                                        SumCaseUplcConstrSirTypeGenerator
                                    case _ if isPair(typeArgs.head) =>
                                        SumPairBuiltinListSirTypeGenerator
                                    case _ => new SumBuiltinListSirTypeGenerator(elemRepr)
                            } else SumCaseUplcOnlySirTypeGenerator
                        else if decl.name == SIRType.BuiltinList.name then
                            if isPair(typeArgs.head) then SumPairBuiltinListSirTypeGenerator
                            else new SumBuiltinListSirTypeGenerator(elementReprFor(typeArgs.head))
                        else if !containsFun(tp, trace) then SumCaseSirTypeGenerator
                        else SumCaseUplcOnlySirTypeGenerator
                    }
            case SIRType.CaseClass(constrDecl, typeArgs, optParent) =>
                // Data constructors are handled by SIRTypeUplcDataGenerator
                if constrDecl.name == SIRType.Data.Constr.name
                    || constrDecl.name == SIRType.Data.Map.name
                    || constrDecl.name == SIRType.Data.List.name
                    || constrDecl.name == SIRType.Data.I.name
                    || constrDecl.name == SIRType.Data.B.name
                then SIRTypeUplcDataGenerator
                // BuiltinArray has its own generator for proper Data conversion
                else if constrDecl.name == SIRType.BuiltinArray.name
                then BuiltinArraySirTypeGenerator
                else
                    // 1. Non-trivial structural constraints (can't be expressed by annotations)
                    resolveWithConstraints(constrDecl, typeArgs)
                        // 2. @UplcRepr annotation
                        .orElse(
                          getUplcReprAnnotation(constrDecl.annotations)
                              .map(encoded =>
                                  resolveUplcRepresentation(encoded, constrDecl, typeArgs, debug)
                              )
                        )
                        // 3. Basic structural inference with diagnostic check
                        .getOrElse {
                            warnIfExpectedAnnotation(constrDecl)
                            resolveBasicStructural(constrDecl)
                        }
            case SIRType.TypeLambda(_, body) =>
                SirTypeUplcGenerator(body, debug)
            case SIRType.TypeProxy(ref) =>
                SirTypeUplcGenerator(ref)
            case SIRType.Fun(input, output) =>
                FunSirTypeGenerator
            case SIRType.TypeVar(_, _, _) =>
                TypeVarSirTypeGenerator
            case SIRType.FreeUnificator =>
                TypeVarSirTypeGenerator
            case SIRType.TypeNothing =>
                TypeNothingSirTypeGenerator
            case SIRType.BLS12_381_G1_Element =>
                BLS12_381_G1_SirTypeGenerator
            case SIRType.BLS12_381_G2_Element =>
                BLS12_381_G2_SirTypeGenerator
            case SIRType.BLS12_381_MlResult =>
                BLS12_381_MLResultSirTypeGenerator
            case SIRType.BuiltinValue =>
                BuiltinValueSirTypeGenerator
            case SIRType.Annotated(tp, anns) =>
                anns.data.get("uplcRepr") match
                    case Some(reprSir) =>
                        resolveGeneratorFromAnnotation(reprSir, tp, debug)
                    case None =>
                        SirTypeUplcGenerator(tp, debug)
            case _ =>
                // TODO: implement
                ???
        if debug then println(s"SirTypeUplcGenerator: ${tp} ->${retval.getClass.getSimpleName}")
        retval
    }

    /** Types that should have @UplcRepr annotation. If we reach here, the annotation wasn't
      * propagated.
      */
    private val expectedAnnotatedTypes = Set(
      "scalus.cardano.onchain.plutus.v1.PubKeyHash",
      "scalus.cardano.onchain.plutus.v3.TxId",
      "scalus.cardano.onchain.plutus.prelude.AssocMap",
      "scalus.cardano.onchain.plutus.prelude.SortedMap",
      "scalus.cardano.onchain.plutus.prelude.Varargs",
      "scalus.cardano.onchain.plutus.v1.Value"
    )

    private def warnIfExpectedAnnotation(constrDecl: ConstrDecl): Unit =
        if expectedAnnotatedTypes.contains(constrDecl.name) then
            System.err.println(
              s"WARNING: ${constrDecl.name} expected @UplcRepr annotation but none found. " +
                  "Falling back to structural inference, which may produce incorrect representation."
            )

    /** Non-trivial structural constraints that can't be expressed by annotations
      * (List/BuiltinList/PairList handling with containsFun/isPairOrTuple2 checks).
      */
    private def resolveWithConstraints(
        constrDecl: ConstrDecl,
        typeArgs: List[SIRType]
    )(using lctx: LoweringContext): Option[SirTypeUplcGenerator] = {
        if constrDecl.name == SIRType.List.NilConstr.name || constrDecl.name == SIRType.List.Cons.name
            || constrDecl.name == SIRType.BuiltinList.Nil.name || constrDecl.name == SIRType.BuiltinList.Cons.name
            || constrDecl.name == SumListCommonSirTypeGenerator.PairNilName || constrDecl.name == SumListCommonSirTypeGenerator.PairConsName
        then
            val hasFun = containsFun(constrDecl, new IdentityHashMap[SIRType, SIRType]())
            if hasFun then Some(SumCaseUplcOnlySirTypeGenerator)
            else if constrDecl.name == SumListCommonSirTypeGenerator.PairNilName
                || constrDecl.name == SumListCommonSirTypeGenerator.PairConsName
            then Some(SumPairBuiltinListSirTypeGenerator)
            else if (constrDecl.name == SIRType.List.Cons.name || constrDecl.name == SIRType.BuiltinList.Cons.name) && isPair(
                  typeArgs.head
                )
            then Some(SumPairBuiltinListSirTypeGenerator)
            else if typeArgs.nonEmpty && typeArgs.head != SIRType.TypeNothing then
                Some(new SumBuiltinListSirTypeGenerator(elementReprFor(typeArgs.head)))
            else if constrDecl.name == SIRType.List.NilConstr.name
                || constrDecl.name == SIRType.BuiltinList.Nil.name
                || constrDecl.name == SumListCommonSirTypeGenerator.PairNilName
            then
                Some(
                  new SumBuiltinListSirTypeGenerator(
                    TypeVarRepresentation(SIRType.TypeVarKind.Fixed)
                  )
                )
            else
                throw LoweringException(
                  s"Cannot determine element representation for list constructor ${constrDecl.name} with typeArgs=${typeArgs.map(_.show)}",
                  SIRPosition.empty
                )
        else None
    }

    /** Basic structural inference: ProductCase or ProductCaseUplcOnly based on containsFun. */
    private def resolveBasicStructural(
        constrDecl: ConstrDecl
    ): SirTypeUplcGenerator =
        val hasFun = containsFun(constrDecl, new IdentityHashMap[SIRType, SIRType]())
        if hasFun then ProductCaseUplcOnlySirTypeGenerator
        else ProductCaseSirTypeGenerator

    def isPairOrTuple2(tp: SIRType): Boolean =
        ProductCaseClassRepresentation.ProdBuiltinPair.isPairOrTuple2(tp)

    def isPrimitiveElementType(tp: SIRType): Boolean = tp match
        case SIRType.Integer | SIRType.ByteString | SIRType.String | SIRType.Boolean => true
        case _                                                                       => false

    /** Compute the element representation for a list element type. Note: only BuiltinPair gets
      * ProdBuiltinPair. Tuple2 gets ProdDataConstr via defaultDataRepresentation because Tuple2 is
      * constr-encoded (Data.Constr(0, [fst, snd])), not pair-encoded.
      */
    def elementReprFor(elemType: SIRType)(using lctx: LoweringContext): LoweredValueRepresentation =
        if isPair(elemType) then
            val (fstType, sndType) =
                ProductCaseClassRepresentation.ProdBuiltinPair.extractPairComponentTypes(elemType)
            ProductCaseClassRepresentation.ProdBuiltinPair(
              elementReprFor(fstType),
              elementReprFor(sndType)
            )
        else if elemType == SIRType.TypeNothing || elemType == SIRType.Unit then
            TypeVarRepresentation(SIRType.TypeVarKind.Fixed)
        else
            elemType match
                case tv: SIRType.TypeVar                   => TypeVarRepresentation(tv.kind)
                case SIRType.TypeLambda(_, body)           => elementReprFor(body)
                case SIRType.TypeProxy(ref) if ref != null => elementReprFor(ref)
                case _ =>
                    val gen = SirTypeUplcGenerator(elemType)
                    if gen.canBeConvertedToData(elemType) then
                        gen.defaultDataRepresentation(elemType)
                    else gen.defaultRepresentation(elemType)

    def isPair(tp: SIRType): Boolean =
        SIRType.retrieveConstrDecl(tp) match {
            case Right(constrDecl) => constrDecl.name == SIRType.BuiltinPair.name
            case Left(_)           => false
        }

    private def containsFun(tp: SIRType, trace: IdentityHashMap[SIRType, SIRType]): Boolean = {
        if !(trace.get(tp) eq null) then false
        else
            tp match
                case SIRType.Fun(_, _) => true
                case SIRType.TypeLambda(_, body) =>
                    trace.put(tp, tp)
                    containsFun(body, trace)
                case SIRType.SumCaseClass(decl, typeArgs) =>
                    trace.put(tp, tp)
                    decl.constructors.exists(constr => containsFun(constr, trace)) || typeArgs
                        .exists(ta => containsFun(ta, trace))
                case SIRType.CaseClass(constrDecl, typeArgs, _) =>
                    trace.put(tp, tp)
                    typeArgs.exists(ta => containsFun(ta, trace)) || containsFun(constrDecl, trace)
                case SIRType.TypeProxy(ref) =>
                    containsFun(ref, trace)
                case SIRType.Annotated(tp1, _) =>
                    containsFun(tp1, trace)
                case _ => false
    }

    private def containsFun(
        constrDecl: ConstrDecl,
        trace: IdentityHashMap[SIRType, SIRType]
    ): Boolean = {
        constrDecl.params.exists(tpb => containsFun(tpb.tp, trace)) ||
        constrDecl.typeParams.exists(tp => containsFun(tp, trace))
    }

}
