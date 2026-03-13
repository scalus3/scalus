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

    def isDataSupported(tp: SIRType)(using
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
      * @param targetType - type of the generated value, which should be a subtype of constr.tp
      */
    def genConstr(constr: SIR.Constr)(using
        LoweringContext
    ): LoweredValue

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

}

object SirTypeUplcGenerator {

    /** Extracts the uplcRepr annotation value from AnnotationsDecl. */
    private def getUplcReprAnnotation(anns: AnnotationsDecl): Option[String] =
        anns.data.get("uplcRepr").collect {
            case SIR.Const(scalus.uplc.Constant.String(spec), _, _) => spec
        }

    /** Resolves the encoded UplcRepresentation string to the appropriate generator. */
    private def resolveUplcRepresentation(
        encoded: String,
        constrDecl: ConstrDecl,
        typeArgs: List[SIRType],
        debug: Boolean
    ): SirTypeUplcGenerator =
        encoded match {
            case "ProductCase"           => ProductCaseSirTypeGenerator
            case "SumCase"               => SumCaseSirTypeGenerator
            case "SumDataList"           => SumDataListSirTypeGenerator
            case "SumPairDataList"       => SumPairDataListSirTypeGenerator
            case "Map"                   => MapSirTypeGenerator
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
            case other =>
                throw IllegalArgumentException(s"Unknown UplcRepresentation: $other")
        }

    def apply(tp: SIRType, debug: Boolean = false): SirTypeUplcGenerator = {
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
                                resolveUplcRepresentation(encoded, constr, typeArgs, debug)
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
                            if !containsFun(tp, trace) then SumPairDataListSirTypeGenerator
                            else SumCaseUplcOnlySirTypeGenerator
                        else if decl.name == "scalus.cardano.onchain.plutus.prelude.List" then
                            if !containsFun(tp, trace) then {
                                if isPair(typeArgs.head) // isPairOrTuple2(typeArgs.head)
                                then SumPairDataListSirTypeGenerator
                                else SumDataListSirTypeGenerator
                            } else SumCaseUplcOnlySirTypeGenerator
                        else if decl.name == SIRType.BuiltinList.name then
                            if isPairOrTuple2(typeArgs.head) then SumPairDataListSirTypeGenerator
                            else SumDataListSirTypeGenerator
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
                    // Check for @UplcRepr annotation on the case class
                    getUplcReprAnnotation(constrDecl.annotations) match {
                        case Some(encoded) =>
                            resolveUplcRepresentation(encoded, constrDecl, typeArgs, debug)
                        case None =>
                            // Existing structural logic
                            val hasFun =
                                containsFun(constrDecl, new IdentityHashMap[SIRType, SIRType]())
                            if constrDecl.name == SIRType.List.NilConstr.name || constrDecl.name == SIRType.List.Cons.name
                                || constrDecl.name == SIRType.BuiltinList.Nil.name || constrDecl.name == SIRType.BuiltinList.Cons.name
                                || constrDecl.name == SumListCommonSirTypeGenerator.PairNilName || constrDecl.name == SumListCommonSirTypeGenerator.PairConsName
                            then {
                                if hasFun then SumCaseUplcOnlySirTypeGenerator
                                // PairList constructors always use SumPairDataList
                                else if constrDecl.name == SumListCommonSirTypeGenerator.PairNilName
                                    || constrDecl.name == SumListCommonSirTypeGenerator.PairConsName
                                then SumPairDataListSirTypeGenerator
                                else if (constrDecl.name == SIRType.List.Cons.name || constrDecl.name == SIRType.BuiltinList.Cons.name) && isPairOrTuple2(
                                      typeArgs.head
                                    )
                                then SumPairDataListSirTypeGenerator
                                else SumDataListSirTypeGenerator
                            } else if hasFun then ProductCaseUplcOnlySirTypeGenerator
                            else ProductCaseSirTypeGenerator
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
            case _ =>
                // TODO: implement
                ???
        if debug then println(s"SirTypeUplcGenerator: ${tp} ->${retval.getClass.getSimpleName}")
        retval
    }

    def isPairOrTuple2(tp: SIRType): Boolean =
        ProductCaseClassRepresentation.PairData.isPairOrTuple2(tp)

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
