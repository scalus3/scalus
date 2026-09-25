package scalus.verify

import org.scalacheck.Gen
import scalus.cardano.onchain.plutus.prelude.List as PList
import scalus.uplc.builtin.{ByteString, Data}

/** Evidence that a type can be quantified over in a [[Prop]].
  *
  * The proving tactics take the static facts they need about `A` (its `SIRType`, its Lean type, how
  * a value becomes a UPLC constant) from the type itself, at reification. This type class serves
  * the runtime interpretation of a statement, which draws values instead of proving: [[edgeCases]]
  * are always tried first, in order, and [[gen]] supplies the rest.
  *
  * Trying edge cases first makes the classic counterexamples (zero, the empty string, the empty
  * list) deterministic rather than a matter of luck.
  */
trait Quantifiable[A] {

    /** Values that tend to break properties, tried before any random value. */
    def edgeCases: List[A]

    /** Random values, drawn after the edge cases. */
    def gen: Gen[A]
}

object Quantifiable {

    def apply[A](edges: List[A], generator: Gen[A]): Quantifiable[A] = new Quantifiable[A] {
        val edgeCases: List[A] = edges
        val gen: Gen[A] = generator
    }

    private val two64 = BigInt(2).pow(64)

    private val genBigInt: Gen[BigInt] = Gen.frequency(
      4 -> Gen.choose(-1000L, 1000L).map(BigInt(_)),
      2 -> Gen.choose(Long.MinValue, Long.MaxValue).map(BigInt(_)),
      1 -> Gen
          .zip(Gen.choose(Long.MinValue, Long.MaxValue), Gen.choose(0L, Long.MaxValue))
          .map((hi, lo) => BigInt(hi) * two64 + BigInt(lo))
    )

    private val genByteString: Gen[ByteString] = Gen.sized { size =>
        Gen.choose(0, size.min(64))
            .flatMap(n =>
                Gen.listOfN(n, Gen.choose(Byte.MinValue, Byte.MaxValue))
                    .map(bytes => ByteString.fromArray(bytes.toArray))
            )
    }

    /** `Data` trees of bounded depth, so a draw stays small. */
    private def genData(depth: Int): Gen[Data] = {
        val leaf = Gen.oneOf(genBigInt.map(Data.I(_)), genByteString.map(Data.B(_)))
        if depth <= 0 then leaf
        else
            val child = genData(depth - 1)
            val children = Gen.choose(0, 3).flatMap(n => Gen.listOfN(n, child))
            Gen.frequency(
              3 -> leaf,
              1 -> children.map(xs => Data.List(PList.from(xs))),
              1 -> Gen
                  .zip(Gen.choose(0L, 7L), children)
                  .map((tag, xs) => Data.Constr(BigInt(tag), PList.from(xs))),
              1 -> Gen
                  .choose(0, 3)
                  .flatMap(n => Gen.listOfN(n, Gen.zip(child, child)))
                  .map(kvs => Data.Map(PList.from(kvs)))
            )
    }

    given Quantifiable[BigInt] = Quantifiable(
      List(0, 1, -1, 2, -2, 255, 256, -256).map(BigInt(_)) ++
          List(BigInt(Long.MaxValue), BigInt(Long.MinValue), two64, -two64),
      genBigInt
    )

    given Quantifiable[Boolean] = Quantifiable(List(false, true), Gen.oneOf(false, true))

    given Quantifiable[ByteString] = Quantifiable(
      List(ByteString.empty, ByteString(0), ByteString(-1)),
      genByteString
    )

    given Quantifiable[Data] = Quantifiable(
      List(
        Data.I(0),
        Data.B(ByteString.empty),
        Data.List(PList.empty),
        Data.Constr(0, PList.empty),
        Data.Map(PList.empty)
      ),
      genData(3)
    )
}
