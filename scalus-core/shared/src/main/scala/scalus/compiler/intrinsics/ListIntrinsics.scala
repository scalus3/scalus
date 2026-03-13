package scalus.compiler.intrinsics

import scalus.Compile
import scalus.cardano.onchain.plutus.prelude.List
import scalus.compiler.intrinsics.IntrinsicHelpers.*
import scalus.compiler.sir.lowering.SumCaseClassRepresentation
import scalus.uplc.builtin.Builtins.*
import scalus.uplc.builtin.{BuiltinList, BuiltinPair, Data}

// ---------------------------------------------------------------------------
//  SumDataList intrinsics — List[A] where elements are packed as Data
// ---------------------------------------------------------------------------

/** Intrinsic implementations for List with SumDataList representation.
  *
  * Available at all protocol versions (changPV+).
  */
@Compile
object BuiltinListSumDataListOperations {

    def isEmpty[A](self: List[A]): Boolean =
        nullList(
          typeProxyRepr[BuiltinList[Data], SumCaseClassRepresentation.SumDataList.type](self)
        )

    def head[A](self: List[A]): A =
        typeProxyRetData[A](
          headList(
            typeProxyRepr[BuiltinList[Data], SumCaseClassRepresentation.SumDataList.type](self)
          )
        )

    def tail[A](self: List[A]): List[A] =
        typeProxyRepr[List[A], SumCaseClassRepresentation.SumDataList.type](
          tailList(
            typeProxyRepr[BuiltinList[Data], SumCaseClassRepresentation.SumDataList.type](self)
          )
        )

}

/** SumDataList intrinsics requiring vanRossemPV (protocol version 11+). */
@Compile
object BuiltinListSumDataListOperationsV11 {

    def drop[A](self: List[A], n: BigInt): List[A] =
        typeProxyRepr[List[A], SumCaseClassRepresentation.SumDataList.type](
          dropList(
            n,
            typeProxyRepr[BuiltinList[Data], SumCaseClassRepresentation.SumDataList.type](self)
          )
        )

    def at[A](self: List[A], index: BigInt): A =
        typeProxyRetData[A](
          BuiltinListSumDataListSupportV11.atSumDataList(
            typeProxyRepr[BuiltinList[Data], SumCaseClassRepresentation.SumDataList.type](self),
            index
          )
        )

}

/** Support module for SumDataList, requiring vanRossemPV (protocol version 11+). */
@Compile
object BuiltinListSumDataListSupportV11 {

    def atSumDataList(self: BuiltinList[Data], n: BigInt): Data = {
        dropList(n, self).head
    }

}

// ---------------------------------------------------------------------------
//  SumDataPairList intrinsics — List[BuiltinPair[A,B]] where elements are
//  packed as BuiltinPair[Data,Data]
// ---------------------------------------------------------------------------

/** Intrinsic implementations for List with SumDataPairList representation.
  *
  * Available at all protocol versions (changPV+).
  */
@Compile
object BuiltinListSumDataPairListOperations {

    def isEmpty[A](self: List[A]): Boolean =
        nullList(
          typeProxyRepr[BuiltinList[
            BuiltinPair[Data, Data]
          ], SumCaseClassRepresentation.SumDataPairList.type](
            self
          )
        )

    def head[A](self: List[A]): A =
        typeProxyRetData[A](
          headList(
            typeProxyRepr[BuiltinList[
              BuiltinPair[Data, Data]
            ], SumCaseClassRepresentation.SumDataPairList.type](
              self
            )
          )
        )

    def tail[A](self: List[A]): List[A] =
        typeProxyRepr[List[A], SumCaseClassRepresentation.SumDataPairList.type](
          tailList(
            typeProxyRepr[BuiltinList[
              BuiltinPair[Data, Data]
            ], SumCaseClassRepresentation.SumDataPairList.type](
              self
            )
          )
        )

}

/** SumDataPairList intrinsics requiring vanRossemPV (protocol version 11+). */
@Compile
object BuiltinListSumDataPairListOperationsV11 {

    def drop[A](self: List[A], n: BigInt): List[A] =
        typeProxyRepr[List[A], SumCaseClassRepresentation.SumDataPairList.type](
          dropList(
            n,
            typeProxyRepr[BuiltinList[
              BuiltinPair[Data, Data]
            ], SumCaseClassRepresentation.SumDataPairList.type](
              self
            )
          )
        )

    def at[A](self: List[A], index: BigInt): A =
        typeProxyRetData[A](
          headList(
            dropList(
              index,
              typeProxyRepr[BuiltinList[
                BuiltinPair[Data, Data]
              ], SumCaseClassRepresentation.SumDataPairList.type](
                self
              )
            )
          )
        )

}
