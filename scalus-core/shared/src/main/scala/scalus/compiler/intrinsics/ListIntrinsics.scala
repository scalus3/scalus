package scalus.compiler.intrinsics

import scalus.compiler.Compile
import scalus.cardano.onchain.plutus.prelude.{List, PairList}
import scalus.compiler.intrinsics.IntrinsicHelpers.*
import scalus.compiler.sir.{SIRPosition, SIRType}
import scalus.compiler.sir.lowering.*
import scalus.uplc.builtin.Builtins.*
import scalus.uplc.builtin.{BuiltinList, BuiltinPair, Data}

/** A repr rule: (outputType, inputRepr, loweringContext) => outputRepr */
type ReprRule =
    (SIRType, LoweredValueRepresentation, LoweringContext) => LoweredValueRepresentation

/** Argument conversion rule for factory intrinsics: (argType, resultType, lctx) => target repr.
  * Returns Some(repr) to convert the argument before substitution, None to leave as-is.
  */
type ArgReprConvertRule =
    (SIRType, SIRType, LoweringContext) => Option[LoweredValueRepresentation]

// ---------------------------------------------------------------------------
//  Repr rules — defined here, near the intrinsics that use them
// ---------------------------------------------------------------------------

object ListReprRules {

    private def listRepr(inRepr: LoweredValueRepresentation): LoweredValueRepresentation =
        inRepr // same list repr

    private def elemRepr(
        inRepr: LoweredValueRepresentation,
        outTp: SIRType,
        lctx: LoweringContext
    ): LoweredValueRepresentation =
        inRepr match
            case SumCaseClassRepresentation.SumBuiltinList(er)                     => er
            case SumCaseClassRepresentation.SumPairBuiltinList(keyRepr, valueRepr) =>
                // Verify key/value reprs match default data representations for the element type
                val (keyType, valueType) =
                    SumCaseClassRepresentation.SumPairBuiltinList.extractKeyValueTypes(outTp)
                val expectedKeyRepr =
                    lctx.typeGenerator(keyType).defaultDataRepresentation(keyType)(using lctx)
                val expectedValueRepr =
                    lctx.typeGenerator(valueType).defaultDataRepresentation(valueType)(using lctx)
                if keyRepr.isCompatibleOn(keyType, expectedKeyRepr, SIRPosition.empty)(using lctx)
                    && valueRepr.isCompatibleOn(valueType, expectedValueRepr, SIRPosition.empty)(
                      using lctx
                    )
                then ProductCaseClassRepresentation.ProdBuiltinPair(keyRepr, valueRepr)
                else lctx.typeGenerator(outTp).defaultRepresentation(outTp)(using lctx)
            case _ => lctx.typeGenerator(outTp).defaultRepresentation(outTp)(using lctx)

    /** isEmpty: List[A] -> Boolean */
    val isEmptyRule: ReprRule = (outTp, _, lctx) =>
        lctx.typeGenerator(outTp).defaultRepresentation(outTp)(using lctx)

    /** head: List[A] -> A */
    val headRule: ReprRule = (outTp, inRepr, lctx) => elemRepr(inRepr, outTp, lctx)

    /** tail: List[A] -> List[A] */
    val tailRule: ReprRule = (_, inRepr, _) => listRepr(inRepr)

    /** drop: List[A] -> (BigInt -> List[A]) */
    val dropRule: ReprRule = (outTp, inRepr, _) =>
        outTp match
            case SIRType.Fun(argTp, retTp) =>
                LambdaRepresentation(
                  outTp,
                  InOutRepresentationPair(PrimitiveRepresentation.Constant, listRepr(inRepr))
                )
            case _ => listRepr(inRepr)

    /** at: List[A] -> (BigInt -> A) */
    val atRule: ReprRule = (outTp, inRepr, lctx) =>
        outTp match
            case SIRType.Fun(argTp, retTp) =>
                LambdaRepresentation(
                  outTp,
                  InOutRepresentationPair(
                    PrimitiveRepresentation.Constant,
                    elemRepr(inRepr, retTp, lctx)
                  )
                )
            case _ => elemRepr(inRepr, outTp, lctx)

    val listRules: Map[String, ReprRule] = Map(
      "isEmpty" -> isEmptyRule,
      "head" -> headRule,
      "tail" -> tailRule,
      "drop" -> dropRule,
      "at" -> atRule
    )

    val pairListRules: Map[String, ReprRule] = listRules
}

/** Repr rules for native list intrinsics (IntrinsicsNativeList). */
object NativeListReprRules {
    import ListReprRules.{headRule, isEmptyRule, tailRule}

    private def listRepr(inRepr: LoweredValueRepresentation): LoweredValueRepresentation = inRepr

    /** map/filter/etc: after self substitution, outTp is the remaining curried function. Use
      * defaultRepresentation which handles Fun types correctly.
      */
    val mapRule: ReprRule = (outTp, inRepr, lctx) =>
        lctx.typeGenerator(outTp).defaultRepresentation(outTp)(using lctx)

    val filterRule: ReprRule = (outTp, inRepr, lctx) =>
        lctx.typeGenerator(outTp).defaultRepresentation(outTp)(using lctx)

    /** foldLeft: List[A] → B — output depends on return type */
    val foldLeftRule: ReprRule = (outTp, _, lctx) =>
        lctx.typeGenerator(outTp).defaultRepresentation(outTp)(using lctx)

    /** foldRight: same as foldLeft */
    val foldRightRule: ReprRule = foldLeftRule

    /** find: List[A] → Option[A] */
    val findRule: ReprRule = (outTp, _, lctx) =>
        lctx.typeGenerator(outTp).defaultRepresentation(outTp)(using lctx)

    /** contains: List[A] → A → Eq[A] → Boolean */
    val containsRule: ReprRule = (outTp, _, lctx) =>
        lctx.typeGenerator(outTp).defaultRepresentation(outTp)(using lctx)

    val rules: Map[String, ReprRule] = Map(
      "isEmpty" -> isEmptyRule,
      "head" -> headRule,
      "tail" -> tailRule,
      "map" -> mapRule,
      "filter" -> filterRule,
      "foldLeft" -> foldLeftRule,
      "foldRight" -> foldRightRule,
      "find" -> findRule
    )
}

/** Repr rules for UplcConstr list intrinsics (IntrinsicsUplcConstrList).
  *
  * Lists with SumUplcConstr representation use Constr(0, [h, t]) / Constr(1, []) instead of builtin
  * lists. Operations use Case-based pattern matching. No repr rules for contains — the intrinsic
  * body handles representation conversion internally via toDefaultTypeVarRepr.
  */
object UplcConstrListReprRules {
    import ListReprRules.{headRule, isEmptyRule, tailRule}

    /** For operations returning a scalar type (not a list). */
    val scalarRule: ReprRule = (outTp, _, lctx) =>
        lctx.typeGenerator(outTp).defaultRepresentation(outTp)(using lctx)

    /** For operations returning the same list type as the input. Walks curried Fun types,
      * preserving inRepr in the final return position.
      */
    private def sameListReturnRepr(
        outTp: SIRType,
        inRepr: LoweredValueRepresentation,
        lctx: LoweringContext
    ): LoweredValueRepresentation =
        outTp match
            case SIRType.TypeLambda(_, body) => sameListReturnRepr(body, inRepr, lctx)
            case SIRType.Fun(argTp, retTp) =>
                val argRepr =
                    lctx.typeGenerator(argTp).defaultRepresentation(argTp)(using lctx)
                val retRepr = sameListReturnRepr(retTp, inRepr, lctx)
                LambdaRepresentation(outTp, InOutRepresentationPair(argRepr, retRepr))
            case _ => inRepr

    val sameListRule: ReprRule = (outTp, inRepr, lctx) => sameListReturnRepr(outTp, inRepr, lctx)

    /** For append-like operations: the second arg is also a list of the same type, so use inRepr
      * for the arg (instead of defaultRepresentation on the original Fixed TypeVar type).
      */
    private def appendListReturnRepr(
        outTp: SIRType,
        inRepr: LoweredValueRepresentation,
        lctx: LoweringContext
    ): LoweredValueRepresentation =
        outTp match
            case SIRType.TypeLambda(_, body) => appendListReturnRepr(body, inRepr, lctx)
            case SIRType.Fun(_, retTp) =>
                val retRepr = sameListReturnRepr(retTp, inRepr, lctx)
                LambdaRepresentation(outTp, InOutRepresentationPair(inRepr, retRepr))
            case _ => inRepr

    val appendListRule: ReprRule = (outTp, inRepr, lctx) =>
        appendListReturnRepr(outTp, inRepr, lctx)

    val rules: Map[String, ReprRule] = Map(
      "isEmpty" -> isEmptyRule,
      "head" -> headRule,
      "tail" -> tailRule,
      // map: no repr rule — body's own repr (SumUplcConstr with Transparent TypeVars)
      // flows through; reprFun resolves TypeVars at the call site
      "filter" -> sameListRule,
      // foldLeft, foldRight, find: no repr rule — body's Transparent TypeVar repr
      // flows through; reprFun resolves at call site
      // contains omitted — equalsRepr handles comparison in intrinsic body
      "length" -> scalarRule,
      "reverse" -> sameListRule,
      "append" -> appendListRule,
      "appendedAll" -> appendListRule,
      "drop" -> sameListRule,
      "prepended" -> sameListRule,
      "dropRight" -> sameListRule,
      "init" -> sameListRule
    )
}

// ---------------------------------------------------------------------------
//  List[A] intrinsics — unified for all SumBuiltinList(*) representations
// ---------------------------------------------------------------------------

@Compile
object BuiltinListOperations {

    def isEmpty[A](self: List[A]): Boolean =
        nullList(typeProxy[BuiltinList[A]](self))

    def head[A](self: List[A]): A =
        headList(typeProxy[BuiltinList[A]](self))

    def tail[A](self: List[A]): List[A] =
        typeProxy[List[A]](
          tailList(typeProxy[BuiltinList[A]](self))
        )

}

@Compile
object BuiltinListOperationsV11 {

    def drop[A](self: List[A], n: BigInt): List[A] =
        typeProxy[List[A]](
          dropList(n, typeProxy[BuiltinList[A]](self))
        )

    def at[A](self: List[A], index: BigInt): A =
        headList(dropList(index, typeProxy[BuiltinList[A]](self)))

}

// ---------------------------------------------------------------------------
//  PairList[A,B] intrinsics — same structure, different arity
// ---------------------------------------------------------------------------

@Compile
object BuiltinPairListOperations {

    def isEmpty[A, B](self: PairList[A, B]): Boolean =
        nullList(typeProxy[BuiltinList[BuiltinPair[Data, Data]]](self))

    def head[A, B](self: PairList[A, B]): (A, B) =
        typeProxy[(A, B)](
          headList(typeProxy[BuiltinList[BuiltinPair[Data, Data]]](self))
        )

    def tail[A, B](self: PairList[A, B]): PairList[A, B] =
        typeProxy[PairList[A, B]](
          tailList(typeProxy[BuiltinList[BuiltinPair[Data, Data]]](self))
        )

}

@Compile
object BuiltinPairListOperationsV11 {

    def drop[A, B](self: PairList[A, B], n: BigInt): PairList[A, B] =
        typeProxy[PairList[A, B]](
          dropList(n, typeProxy[BuiltinList[BuiltinPair[Data, Data]]](self))
        )

}
