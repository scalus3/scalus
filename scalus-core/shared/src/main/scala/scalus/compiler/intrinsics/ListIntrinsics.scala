package scalus.compiler.intrinsics

import scalus.Compile
import scalus.cardano.onchain.plutus.prelude.{List, PairList}
import scalus.compiler.intrinsics.IntrinsicHelpers.*
import scalus.compiler.sir.{SIRPosition, SIRType}
import scalus.compiler.sir.lowering.*
import scalus.uplc.builtin.Builtins.*
import scalus.uplc.builtin.{BuiltinList, BuiltinPair, Data}

/** A repr rule: (outputType, inputRepr, loweringContext) => outputRepr */
type ReprRule =
    (SIRType, LoweredValueRepresentation, LoweringContext) => LoweredValueRepresentation

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
            case SumCaseClassRepresentation.SumBuiltinList(er) => er
            case SumCaseClassRepresentation.SumPairBuiltinList(keyRepr, valueRepr) =>
                // Verify key/value reprs match default data representations for the element type
                val (keyType, valueType) =
                    SumCaseClassRepresentation.SumPairBuiltinList.extractKeyValueTypes(outTp)
                val expectedKeyRepr =
                    lctx.typeGenerator(keyType).defaultDataRepresentation(keyType)(using lctx)
                val expectedValueRepr =
                    lctx.typeGenerator(valueType).defaultDataRepresentation(valueType)(using lctx)
                if keyRepr.isCompatibleOn(keyType, expectedKeyRepr, SIRPosition.empty)(using lctx)
                    && valueRepr.isCompatibleOn(valueType, expectedValueRepr, SIRPosition.empty)(using lctx)
                then ProductCaseClassRepresentation.PairData
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
