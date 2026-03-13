package scalus.compiler.intrinsics

import scalus.Compile
import scalus.cardano.onchain.plutus.prelude.List
import scalus.compiler.intrinsics.IntrinsicHelpers.*
import scalus.compiler.sir.lowering.SumCaseClassRepresentation
import scalus.uplc.builtin.Builtins.*
import scalus.uplc.builtin.{BuiltinList, Data}

/** Intrinsic implementations for List with SumDataList representation.
  *
  * These methods use UPLC builtins directly for optimal code generation. The IntrinsicResolver
  * substitutes these implementations when the list argument has SumDataList representation.
  *
  * Methods must match the signature of the original extension methods in List companion. Provider
  * methods should only use builtins and typeProxyRepr to prevent complex resolution chains.
  *
  * Available at all protocol versions (changPV+).
  */
@Compile
object BuiltinListOperations {

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

/** Intrinsic implementations for List builtins requiring vanRossemPV (protocol version 11+).
  *
  * Note: `drop` has two parameters (self, n). The resolver handles multi-param providers via
  * curried application — it substitutes only the `self` parameter, returning a lambda that accepts
  * `n`.
  */
@Compile
object BuiltinListOperationsV11 {

    def drop[A](self: List[A], n: BigInt): List[A] =
        typeProxyRepr[List[A], SumCaseClassRepresentation.SumDataList.type](
          dropList(
            n,
            typeProxyRepr[BuiltinList[Data], SumCaseClassRepresentation.SumDataList.type](self)
          )
        )

    def at[A](self: List[A], index: BigInt): A =
        typeProxyRetData[A](
          BuiltinListLongOperationsV11.atSumDataList(
            typeProxyRepr[BuiltinList[Data], SumCaseClassRepresentation.SumDataList.type](self),
            index
          )
        )

}

/** implementations for List builtins substitutions vanRossemPV (protocol version 11+).
  */
@Compile
object BuiltinListLongOperationsV11 {

    def atSumDataList(self: BuiltinList[Data], n: BigInt): Data = {
        dropList(n, self).head
    }

}
