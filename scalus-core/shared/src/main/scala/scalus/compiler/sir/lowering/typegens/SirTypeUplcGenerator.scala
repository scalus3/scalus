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
            case SIRType.Data =>
                SIRTypeUplcDataGenerator
            case SIRType.Unit =>
                UnitSirTypeGenerator
            case SIRType.SumCaseClass(decl, typeArgs) =>
                val trace = new IdentityHashMap[SIRType, SIRType]()
                if decl.name == "scalus.prelude.List" then
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
            case SIRType.CaseClass(constrDecl, typeArgs, optParent) =>
                if constrDecl.name == "scalus.ledger.api.v1.PubKeyHash"
                    || constrDecl.name == "scalus.ledger.api.v3.TxId"
                then ProductCaseOneElementSirTypeGenerator(SIRTypeUplcByteStringGenerator)
                else if constrDecl.name == "scalus.prelude.AssocMap" || constrDecl.name == "scalus.prelude.SortedMap"
                then MapSirTypeGenerator
                else if constrDecl.name == "scalus.prelude.Varargs" || constrDecl.name == "scalus.ledger.api.v1.Value"
                then {
                    val paramType =
                        SIRType.substitute(
                          constrDecl.params.head.tp,
                          constrDecl.typeParams.zip(typeArgs).toMap,
                          Map.empty
                        )
                    val paramTypeGen = SirTypeUplcGenerator(paramType, debug)
                    ProductCaseOneElementSirTypeGenerator(paramTypeGen)
                } else
                    val hasFun = containsFun(constrDecl, new IdentityHashMap[SIRType, SIRType]())
                    if constrDecl.name == SIRType.List.NilConstr.name || constrDecl.name == SIRType.List.Cons.name
                        || constrDecl.name == SIRType.BuiltinList.Nil.name || constrDecl.name == SIRType.BuiltinList.Cons.name
                    then {
                        if hasFun then SumCaseUplcOnlySirTypeGenerator
                        else if (constrDecl.name == SIRType.List.Cons.name || constrDecl.name == SIRType.BuiltinList.Cons.name) && isPairOrTuple2(
                              typeArgs.head
                            )
                        then SumPairDataListSirTypeGenerator
                        else SumDataListSirTypeGenerator
                    } else if hasFun then ProductCaseUplcOnlySirTypeGenerator
                    else ProductCaseSirTypeGenerator
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

    private def containsFun(
        types: List[SIRType],
        trace: IdentityHashMap[SIRType, SIRType]
    ): Boolean =
        types.exists(tp => containsFun(tp, trace))

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
        constr: SIR.Constr,
        trace: IdentityHashMap[SIRType, SIRType]
    ): Boolean = {
        constr.args.exists(arg => containsFun(arg.tp, trace))
    }

    private def containsFun(
        constrDecl: ConstrDecl,
        trace: IdentityHashMap[SIRType, SIRType]
    ): Boolean = {
        constrDecl.params.exists(tpb => containsFun(tpb.tp, trace)) ||
        constrDecl.typeParams.exists(tp => containsFun(tp, trace))
    }

}
