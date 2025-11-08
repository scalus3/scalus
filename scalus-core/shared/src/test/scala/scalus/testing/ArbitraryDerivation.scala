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

    def join[T](caseClass: CaseClass[Arbitrary, T]): Arbitrary[T] = Arbitrary {
        Gen.lzy(Gen.sized { size =>
            if size >= 0 then {
                Gen.resize(
                  size - 1,
                  caseClass.constructMonadic(_.typeclass.arbitrary)
                )
            } else {
                summon[Fallback[T]].get
            }
        })
    }

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
