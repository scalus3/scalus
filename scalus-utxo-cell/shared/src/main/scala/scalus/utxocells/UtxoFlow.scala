package scalus.utxocells

import cps.*
import scalus.uplc.builtin.{Data, FromData}
import scalus.cardano.onchain.plutus.prelude.Option as POption

/** Free monad for multi-transaction flows.
  *
  * `UtxoFlow[A]` represents a computation that may suspend at transaction boundaries (via
  * `suspend`). The `UtxoFlow.define` macro calls dotty-cps-async to CPS-transform the developer's
  * sequential code into a `flatMap`/`suspend` chain, then defunctionalizes it into chunk-based
  * dispatch.
  *
  * This type exists only for the macro's type-level analysis. It is never compiled to SIR or
  * executed at runtime — the macro eliminates it during defunctionalization.
  */
sealed trait UtxoFlow[+A]

object UtxoFlow {
    case class Pure[A](value: A) extends UtxoFlow[A]
    case class Suspend[A](fromData: Data => A) extends UtxoFlow[A]
    case class FlatMap[A, B](fa: UtxoFlow[A], f: A => UtxoFlow[B]) extends UtxoFlow[B]
    case class MapFlow[A, B](fa: UtxoFlow[A], f: A => B) extends UtxoFlow[B]
    case class RaiseError(ex: Throwable) extends UtxoFlow[Nothing]

    /** Suspend the flow at a transaction boundary. The next transaction provides a value of type
      * `A` as the redeemer.
      */
    def suspend[A: FromData]: UtxoFlow[A] = Suspend(summon[FromData[A]].apply)

    given utxoFlowMonad: CpsThrowMonad[UtxoFlow] with CpsThrowMonadInstanceContext[UtxoFlow] with {
        override def pure[A](a: A): UtxoFlow[A] = Pure(a)
        override def map[A, B](fa: UtxoFlow[A])(f: A => B): UtxoFlow[B] = MapFlow(fa, f)
        override def flatMap[A, B](fa: UtxoFlow[A])(f: A => UtxoFlow[B]): UtxoFlow[B] =
            FlatMap(fa, f)
        override def error[A](e: Throwable): UtxoFlow[A] = RaiseError(e)
    }

    /** Define a multi-transaction flow. The macro:
      *   1. Calls dotty-cps-async to CPS-transform the body
      *   1. Walks the resulting monadic tree to find suspend boundaries
      *   1. Generates Scalus-compilable dispatch code with datum field tracking
      *
      * Returns a dispatch function `(Data, Data, CellContext) => POption[Data]` that is
      * Scalus-compilable. When placed inside a `@Compile` object, the plugin compiles it to SIR →
      * UPLC. Off-chain, the same function runs on JVM with POption → scala.Option conversion at the
      * boundary.
      *
      * Wrap in a [[UtxoFlowDef]] together with a compiled script and token name for full flow
      * operations.
      */
    transparent inline def define(
        inline body: CpsMonadContext[UtxoFlow] ?=> CellContext => Unit
    ): (Data, Data, CellContext) => POption[Data] = ${ UtxoFlowMacros.defineImpl('body) }
}
