package scalus.testing

import magnolia1.{AutoDerivation, CaseClass, Monadic, SealedTrait}
import org.scalacheck.{Arbitrary, Gen}

/** Derives an `Arbitrary` instance for a given type `T` using Magnolia.
  *
  * @example
  *   {{{
  *     given Arbitrary[AddrKeyHash] = ArbitraryDerivation.autoDerived[AddrKeyHash]
  *   }}}
  */
object ArbitraryDerivation extends AutoDerivation[Arbitrary] {
    private given Monadic[Gen] = new Monadic[Gen] {
        override def point[A](value: A): Gen[A] = Gen.const(value)

        override def map[A, B](from: Gen[A])(fn: A => B): Gen[B] = from.map(fn)

        override def flatMap[A, B](from: Gen[A])(fn: A => Gen[B]): Gen[B] = from.flatMap(fn)
    }

    /** Magnolia callback: derives an `Arbitrary[T]` for a product type by recursively combining
      * generators for each field, each at one size less.
      *
      * Below size 0 the fields are drawn at size 0. A product cannot recurse without a sum type,
      * and `split` stops at size 0, so recursion still ends. Before D5 this branch returned the
      * `Fallback[T]`, which is always `Gen.fail` here (see `split`): every derived product nested
      * under another one discarded its sample at the smallest sizes
      * (`[gen.size.derived-fallback]`).
      */
    def join[T](caseClass: CaseClass[Arbitrary, T]): Arbitrary[T] = Arbitrary {
        Gen.lzy(Gen.sized { size =>
            Gen.resize((size - 1).max(0), caseClass.constructMonadic(_.typeclass.arbitrary))
        })
    }

    /** Magnolia callback: derives an `Arbitrary[T]` for a sum type by uniformly choosing one of its
      * subtypes and delegating to that subtype's generator. Falls back to `Fallback[T]` once the
      * size budget is exhausted to avoid unbounded recursion.
      *
      * The `summon` below runs inside this generic method, so it always finds the default
      * `NoFallback` (`Gen.fail`), never a `given Fallback[T]` at the call site of `autoDerived`. A
      * derived sum type that must give a value at size 0 wraps its instance instead, as
      * `withFallback` in `cardano.ledger.ArbitraryInstances` does.
      */
    def split[T](sealedTrait: SealedTrait[Arbitrary, T]): Arbitrary[T] = Arbitrary {
        Gen.sized { size =>
            if size > 0 then
                Gen.resize(
                  size - 1,
                  Gen.oneOf(sealedTrait.subtypes.map(_.typeclass.arbitrary))
                      .flatMap(identity)
                )
            else summon[Fallback[T]].get
        }
    }

    sealed trait Fallback[+T] extends Serializable {
        def get: Gen[T]
    }

    object Fallback {

        object NoFallback extends Fallback[Nothing] {
            override def get: Gen[Nothing] = Gen.fail
        }

        def apply[T](g: Gen[T]): Fallback[T] = new Fallback[T] {
            override def get: Gen[T] = g
        }

        def apply[T](v: T): Fallback[T] = Fallback[T](Gen.const(v))

        def apply[T](using arb: Arbitrary[T]): Fallback[T] = Fallback[T](arb.arbitrary)

        given defaultFallback[T]: Fallback[T] = NoFallback
    }
}
