package scalus

import scalus.cardano.onchain.plutus.prelude as P
import scalus.cardano.onchain.plutus.prelude.{Eq, Ord, Order, Show}

/** Deprecated: use [[scalus.cardano.onchain.plutus.prelude]] instead.
  *
  * Kept so that code importing `scalus.prelude.*` still compiles. Every member forwards to the
  * member of the same name in [[scalus.cardano.onchain.plutus.prelude]].
  */
package object prelude {
    extension [A](x: A)
        @deprecated("use scalus.cardano.onchain.plutus.prelude.=== instead", "1.3.0")
        inline infix def ===(inline y: A)(using inline eq: Eq[A]): Boolean = P.===(x)(y)
        @deprecated("use scalus.cardano.onchain.plutus.prelude.!== instead", "1.3.0")
        inline infix def !==(inline y: A)(using inline eq: Eq[A]): Boolean = P.!==(x)(y)

    extension [A: Ord](self: A)
        @deprecated("use scalus.cardano.onchain.plutus.prelude.<=> instead", "1.3.0")
        inline infix def <=>(inline other: A): Order = P.<=>(self)(other)

    extension (inline x: Boolean)
        @deprecated("use scalus.cardano.onchain.plutus.prelude.? instead", "1.3.0")
        inline def ? : Boolean = P.?(x)

    extension [A](self: scala.Seq[A])
        @deprecated("use scalus.cardano.onchain.plutus.prelude.asScalus instead", "1.3.0")
        def asScalus: P.List[A] = P.asScalus(self)

    extension [A](self: scala.Option[A])
        @deprecated("use scalus.cardano.onchain.plutus.prelude.asScalus instead", "1.3.0")
        def asScalus: P.Option[A] = P.asScalus(self)

    extension [T](seq: scala.collection.immutable.Seq[T])
        @deprecated("use scalus.cardano.onchain.plutus.prelude.list instead", "1.3.0")
        def list: P.List[T] = P.list(seq)

    extension [A: Show](self: A)
        @deprecated("use scalus.cardano.onchain.plutus.prelude.show instead", "1.3.0")
        inline def show: String = P.show(self)

    extension (self: BigInt)
        @deprecated("use scalus.cardano.onchain.plutus.prelude.absolute instead", "1.3.0")
        inline def absolute: BigInt = P.absolute(self)
        @deprecated("use scalus.cardano.onchain.plutus.prelude.minimum instead", "1.3.0")
        inline def minimum(other: BigInt): BigInt = P.minimum(self)(other)
        @deprecated("use scalus.cardano.onchain.plutus.prelude.maximum instead", "1.3.0")
        inline def maximum(other: BigInt): BigInt = P.maximum(self)(other)
        @deprecated("use scalus.cardano.onchain.plutus.prelude.clamp instead", "1.3.0")
        inline def clamp(min: BigInt, max: BigInt): BigInt = P.clamp(self)(min, max)
        @deprecated("use scalus.cardano.onchain.plutus.prelude.gcf instead", "1.3.0")
        inline def gcf(other: BigInt): BigInt = P.gcf(self)(other)
        @deprecated("use scalus.cardano.onchain.plutus.prelude.sqRoot instead", "1.3.0")
        inline def sqRoot: BigInt = P.sqRoot(self)
        @deprecated("use scalus.cardano.onchain.plutus.prelude.isSqrt instead", "1.3.0")
        inline def isSqrt(x: BigInt): Boolean = P.isSqrt(self)(x)
        @deprecated("use scalus.cardano.onchain.plutus.prelude.pow instead", "1.3.0")
        inline def pow(exp: BigInt): BigInt = P.pow(self)(exp)
        @deprecated("use scalus.cardano.onchain.plutus.prelude.exp2 instead", "1.3.0")
        inline def exp2: BigInt = P.exp2(self)
        @deprecated("use scalus.cardano.onchain.plutus.prelude.log2 instead", "1.3.0")
        inline def log2: BigInt = P.log2(self)
        @deprecated("use scalus.cardano.onchain.plutus.prelude.logarithm instead", "1.3.0")
        inline def logarithm(base: BigInt): BigInt = P.logarithm(self)(base)
}
