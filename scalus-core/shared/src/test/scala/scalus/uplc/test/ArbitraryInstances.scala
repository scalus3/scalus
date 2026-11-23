package scalus.uplc
package test

import org.scalacheck.{Arbitrary, Gen, Shrink}
import scalus.*
import scalus.uplc.builtin.Builtins
import scalus.uplc.builtin.ByteString
import scalus.uplc.builtin.ByteString.utf8
import scalus.uplc.builtin.Data
import scalus.uplc.builtin.Data.{B, Constr, I, List, Map}
import scalus.cardano.ledger.Word64
import scalus.cardano.onchain.plutus.prelude.List as PList
import scalus.uplc.DefaultUni.{ProtoList, ProtoPair}
import scalus.uplc.Term.*
import scalus.cardano.onchain.plutus.prelude.{asScalus, Eq, Ord}

import scala.collection.immutable
import scala.annotation.nowarn
import scalus.uplc.Constant.BLS12_381_G1_Element
import scalus.uplc.Constant.BLS12_381_G2_Element
import scalus.uplc.Constant.BLS12_381_MlResult

/** Import these instances with `import scalus.uplc.test.ArbitraryInstances.given`, or mix in the
  * trait. The other generator files follow the same pattern.
  */
object ArbitraryInstances extends ArbitraryInstances

// ScalaCheck uses Stream for shrinking, which is deprecated
// Remove the deprecation warning for now
@nowarn("cat=deprecation")
trait ArbitraryInstances:

    /** A value in `[min, max]` with a uniform bit length (spec `[gen.magnitude.range]`,
      * `[gen.magnitude.sign-split]`, `[gen.magnitude.bit-length]` of
      * docs/internal/test-refactor/d5-generators.md).
      *
      * `genMagnitude(0, 1000)` gives each bit length 0 to 10 with chance 1/11, so the band 0 to 1
      * is as likely as the band 512 to 1000. `Gen.choose(0, 1000)` gives a value below 8 with
      * chance 0.8 %. A range with both signs first picks the negative or the non-negative part with
      * equal chance.
      */
    def genMagnitude(min: BigInt, max: BigInt): Gen[BigInt] = {
        require(min <= max, s"genMagnitude: min $min > max $max")
        // A value in [lo, hi], 0 <= lo <= hi: pick a bit length, then a value of that length.
        // The ends are clipped, so the band of lo's or hi's bit length can be one value.
        def byBitLength(lo: BigInt, hi: BigInt): Gen[BigInt] =
            Gen.choose(lo.bitLength, hi.bitLength).flatMap { bits =>
                val shortest = if bits == 0 then BigInt(0) else BigInt(1) << (bits - 1)
                val longest = (BigInt(1) << bits) - 1
                Gen.choose(lo.max(shortest), hi.min(longest))
            }
        if min >= 0 then byBitLength(min, max)
        else if max < 0 then byBitLength(-max, -min).map(-_)
        else Gen.oneOf(byBitLength(1, -min).map(-_), byBitLength(0, max))
    }

    /** The `Long` form of `genMagnitude`: the same values for the same seed. */
    def genMagnitude(min: Long, max: Long): Gen[Long] =
        genMagnitude(BigInt(min), BigInt(max)).map(_.toLong)

    def genByteStringOfN(n: Int): Gen[ByteString] = {
        Gen
            .containerOfN[Array, Byte](n, Arbitrary.arbitrary[Byte])
            .map(a => ByteString.unsafeFromArray(a))
    }

    /** The generator behind `byteStringArb` (`[gen.family.given-calls-gen]`), in three parts
      * (`[gen.cover.three-parts]`): 50 % 0 to 64 bytes, 25 % the edge lengths 0, 1, 28, 32, 64 and
      * 65 (`[gen.cover.encoding-edges]`: hash sizes, and the 64-byte chunk limit of CBOR bytes), 25
      * % any length up to 256 with a uniform bit length.
      */
    def genByteString: Gen[ByteString] =
        Gen.frequency(
          2 -> Gen.choose(0, 64),
          1 -> Gen.oneOf(0, 1, 28, 32, 64, 65),
          1 -> genMagnitude(0L, 256L).map(_.toInt)
        ).flatMap(genByteStringOfN)

    given byteStringArb: Arbitrary[scalus.uplc.builtin.ByteString] = Arbitrary(genByteString)

    given g1ElementArbitrary: Arbitrary[scalus.uplc.builtin.bls12_381.G1Element] = Arbitrary(
      for bs <- Arbitrary.arbitrary[ByteString]
      yield Builtins.bls12_381_G1_hashToGroup(bs, dst = utf8"Test")
    )

    given g2ElementArbitrary: Arbitrary[scalus.uplc.builtin.bls12_381.G2Element] = Arbitrary(
      for bs <- Arbitrary.arbitrary[ByteString]
      yield Builtins.bls12_381_G2_hashToGroup(bs, dst = utf8"Test")
    )

    private lazy val TwoPow64 = BigInt(2).pow(64)

    /** The edges of `Data.I` (`[gen.cover.encoding-edges]`): the boundaries of the CBOR integer
      * heads (23/24, 255/256, 2^16, 2^32), the `Long` range, and the 2^64 boundary where CBOR
      * switches to a bignum, each with both signs.
      */
    private lazy val dataIntegerEdges: Seq[BigInt] = {
        val positive = Seq[BigInt](1, 23, 24, 255, 256, 65_536, BigInt(1L << 32), Long.MaxValue)
        Seq[BigInt](0, Long.MinValue, TwoPow64 - 1, TwoPow64, -TwoPow64, -TwoPow64 - 1) ++
            positive ++ positive.map(-_)
    }

    /** The generator behind `iArb`: 50 % -1 000 to 10^6, 25 % the edges of `dataIntegerEdges`, 25 %
      * any integer in `[-2^128, 2^128]` with a uniform bit length (`[gen.cover.three-parts]`).
      */
    def genI: Gen[I] =
        Gen.frequency(
          2 -> Gen.choose(BigInt(-1000), BigInt(1_000_000)),
          1 -> Gen.oneOf(dataIntegerEdges),
          1 -> genMagnitude(-BigInt(2).pow(128), BigInt(2).pow(128))
        ).map(I(_))

    given iArb: Arbitrary[I] = Arbitrary(genI)
    given bArb: Arbitrary[B] = Arbitrary(
      Arbitrary.arbitrary[scalus.uplc.builtin.ByteString].map(B.apply)
    )

    given Shrink[Data] =
        Shrink {
            case I(i) =>
                if i == 0 then Stream.empty else Stream(I(i / 2))
            case B(b) =>
                if b.bytes.isEmpty then Stream.empty
                else
                    Stream(
                      B(
                        scalus.uplc.builtin.ByteString.unsafeFromArray(
                          b.bytes.slice(0, b.size / 2)
                        )
                      )
                    )
            case Constr(c, args) =>
                val scalaArgs = args.toScalaList
                if scalaArgs.isEmpty then Stream.empty
                else Shrink.shrink(scalaArgs).map(a => Constr(c, PList.from(a)))
            case List(args) =>
                val scalaArgs = args.toScalaList
                if scalaArgs.isEmpty then Stream.empty
                else Shrink.shrink(scalaArgs).map(a => List(PList.from(a)))
            case Map(args) =>
                val scalaArgs = args.toScalaList
                if scalaArgs.isEmpty then Stream.empty
                else Shrink.shrink(scalaArgs).map(a => Map(PList.from(a)))
        }

    /** A `Data.Constr` tag: 50 % 0 to 6 (the tags with a compact CBOR form 121 to 127), 25 % the
      * edges 0, 6, 7, 127, 128 and 2^64 − 1 (`[gen.cover.encoding-edges]`: tag 7 and 128 start the
      * other two CBOR forms), 25 % any tag up to 2^64 − 1 with a uniform bit length.
      */
    def genConstrTag: Gen[BigInt] =
        Gen.frequency(
          2 -> Gen.choose(BigInt(0), BigInt(6)),
          1 -> Gen.oneOf[BigInt](0, 6, 7, 127, 128, TwoPow64 - 1),
          1 -> genMagnitude(BigInt(0), TwoPow64 - 1)
        )

    /** The generator behind `arbData` (`[gen.family.given-calls-gen]`). */
    def genData: Gen[Data] = {
        // The size budget `sz` is distributed across children: a container with n
        // children generates each child at size sz/n, keeping the total node count
        // O(sz) in the worst case. The previous scheme (n in [sz/3, sz/2] children,
        // EACH at size sz/2) had a multiplicative heavy tail that occasionally
        // produced multi-million-node values and OOM'd the test JVM.
        def constrGen(sz: Int): Gen[Constr] = for
            c <- genConstrTag
            n <- Gen.choose(0, (sz / 4).max(1))
            args <- Gen.listOfN(n, sizedTree(sz / n.max(2)))
        yield Constr(c, PList.from(args))

        def listGen(sz: Int): Gen[List] = for
            n <- Gen.choose(0, (sz / 4).max(1))
            args <- Gen.listOfN(n, sizedTree(sz / n.max(2)))
        yield List(PList.from(args))

        def mapGen(sz: Int): Gen[Map] = for
            n <- Gen.choose(0, (sz / 4).max(1))
            tuple = Gen.zip(sizedTree(sz / (2 * n.max(1))), sizedTree(sz / (2 * n.max(1))))
            args <- Gen.mapOfN(n, tuple)
        yield Map(PList.from(args.toList))

        def sizedTree(sz: Int): Gen[Data] =
            if sz <= 0 then Gen.oneOf(iArb.arbitrary, bArb.arbitrary)
            else
                Gen.frequency(
                  (1, iArb.arbitrary),
                  (1, bArb.arbitrary),
                  (1, Gen.oneOf(constrGen(sz), listGen(sz), mapGen(sz)))
                )

        Gen.sized(sizedTree)
    }

    given arbData: Arbitrary[Data] = Arbitrary(genData)

    /** The edges of `Data` as fixed values, for the edge table of a property
      * (`[gen.runs.edge-table]`): each `Constr` tag edge of `genConstrTag`, each integer edge of
      * `genI`, and a byte string of each edge length of `genByteString`.
      */
    lazy val dataEdges: Seq[Data] = {
        val tags = Seq[BigInt](
          0,
          6,
          7,
          127,
          128,
          BigInt(Long.MaxValue),
          BigInt(Long.MaxValue) + 1,
          TwoPow64 - 1
        )
        val bytes = Seq(0, 1, 28, 32, 64, 65).map(n =>
            B(ByteString.unsafeFromArray(Array.fill(n)(n.toByte)))
        )
        tags.map(Constr(_, PList.Nil)) ++ dataIntegerEdges.map(I(_)) ++ bytes
    }

    given arbitraryDefaultUni: Arbitrary[DefaultUni] = Arbitrary {
        def listGen(sz: Int): Gen[DefaultUni] = for a <- sizedTree(sz / 2)
        yield DefaultUni.Apply(ProtoList, a)

        def pairGen(sz: Int): Gen[DefaultUni] = for
            a <- sizedTree(sz / 2)
            b <- sizedTree(sz / 2)
        yield DefaultUni.Apply(DefaultUni.Apply(ProtoPair, a), b)

        def sizedTree(sz: Int): Gen[DefaultUni] =
            val simple = Gen.oneOf(
              DefaultUni.Bool,
              DefaultUni.ByteString,
              DefaultUni.Data,
              DefaultUni.Integer,
              DefaultUni.String,
              // BLS constants stay out because Flat has only a stub for them; BuiltinValue stays
              // out until a generator of non-empty values exists (`arbConstantByType` gives only
              // the empty one).
              DefaultUni.Unit
            )
            if sz <= 0 then simple
            else
                Gen.frequency(
                  (3, simple),
                  (1, Gen.oneOf(listGen(sz), pairGen(sz)))
                )
        Gen.sized(sizedTree)
    }

    def arbConstantByType(t: DefaultUni): Gen[Constant] =
        t match
            case DefaultUni.Integer => Arbitrary.arbitrary[BigInt].map(Constant.Integer.apply)
            case DefaultUni.ByteString =>
                Arbitrary.arbitrary[scalus.uplc.builtin.ByteString].map(Constant.ByteString.apply)
            case DefaultUni.String => Arbitrary.arbitrary[String].map(Constant.String.apply)
            case DefaultUni.Data   => Arbitrary.arbitrary[Data].map(Constant.Data.apply)
            case DefaultUni.Unit   => Gen.const(Constant.Unit)
            case DefaultUni.Bool   => Gen.oneOf(Constant.Bool(true), Constant.Bool(false))
            case DefaultUni.Apply(ProtoList, arg) =>
                // Split the size budget across elements: each of the n elements is
                // generated at size sz/n. Nested list/pair types would otherwise
                // multiply full-size draws (e.g. List[List[Data]] -> up to 100
                // full-size Data values) with pathological worst cases.
                Gen.sized { sz =>
                    for
                        n <- Gen.choose(0, 10)
                        elems <- Gen.listOfN(n, Gen.resize(sz / n.max(1), arbConstantByType(arg)))
                    yield Constant.List(arg, elems)
                }
            case DefaultUni.Apply(DefaultUni.Apply(ProtoPair, a), b) =>
                Gen.sized { sz =>
                    for
                        vala <- Gen.resize(sz / 2, arbConstantByType(a))
                        valb <- Gen.resize(sz / 2, arbConstantByType(b))
                    yield Constant.Pair(vala, valb)
                }
            case DefaultUni.BLS12_381_G1_Element =>
                Arbitrary
                    .arbitrary[scalus.uplc.builtin.bls12_381.G1Element]
                    .map(Constant.BLS12_381_G1_Element.apply)
            case DefaultUni.BLS12_381_G2_Element =>
                Arbitrary
                    .arbitrary[scalus.uplc.builtin.bls12_381.G2Element]
                    .map(Constant.BLS12_381_G2_Element.apply)
            case DefaultUni.BLS12_381_MlResult =>
                throw new IllegalArgumentException(
                  "BLS12_381_MlResult is not a valid constant type, it should be used in terms only"
                )
            case DefaultUni.BuiltinValue =>
                Gen.const(Constant.BuiltinValue(scalus.uplc.builtin.BuiltinValue.empty))
            case DefaultUni.Apply(_, _) =>
                // This case should not happen, as we only generate constants for the known types
                throw new IllegalArgumentException(
                  s"Unexpected DefaultUni type for constant generation $t"
                )
            case DefaultUni.ProtoList | DefaultUni.ProtoPair | DefaultUni.ProtoArray =>
                throw new IllegalArgumentException(
                  s"Unexpected DefaultUni type for constant generation $t"
                )

    given arbitraryConstant: Arbitrary[Constant] = Arbitrary(
      for
          a <- arbitraryDefaultUni.arbitrary
          value <- arbConstantByType(a)
      yield value
    )

    given Shrink[Constant] = Shrink {
        case Constant.Integer(i) => Shrink.shrink(i).map(Constant.Integer.apply)
        case Constant.ByteString(b) =>
            Shrink.shrink(b).map(Constant.ByteString.apply)
        case Constant.String(s) => Shrink.shrink(s).map(Constant.String.apply)
        case Constant.Data(d)   => Shrink.shrink(d).map(Constant.Data.apply)
        case Constant.Unit      => Stream.empty
        case Constant.Bool(b)   => Stream.empty
        case Constant.List(t, elems) =>
            val elemsShrunk = Shrink.shrink(elems).map(Constant.List(t, _))
            elemsShrunk
        case Constant.Array(t, elems) =>
            val elemsShrunk = Shrink.shrink(elems).map(Constant.Array(t, _))
            elemsShrunk
        case Constant.Pair(a, b) =>
            val aShrunk = Shrink.shrink(a).map(Constant.Pair(_, b))
            val bShrunk = Shrink.shrink(b).map(Constant.Pair(a, _))
            aShrunk ++ bShrunk
        case BLS12_381_G1_Element(value)  => Stream.empty
        case BLS12_381_G2_Element(value)  => Stream.empty
        case BLS12_381_MlResult(value)    => Stream.empty
        case Constant.BuiltinValue(value) => Stream.empty
    }

    /** A term. With `withConstrCase = false` it holds no `Constr` and no `Case`, so a program of
      * version 1.0.0 can hold it (`[gen.family.program-v100-terms]`). With `true` it gives the
      * values of `arbitraryTerm` before D5 for the same seed.
      */
    def genTerm(withConstrCase: Boolean): Gen[Term] = {
        val nameGen = for
            alpha <- Gen.alphaChar
            n <- Gen.choose(0, 10)
            rest <- Gen
                .listOfN(n, Gen.oneOf(Gen.alphaNumChar, Gen.const("_"), Gen.const("'")))
                .map(_.mkString)
        yield s"$alpha$rest"

        def varGen(env: immutable.List[String]) = Gen.oneOf(env).map(n => Var(NamedDeBruijn(n)))
        // Exclude Array builtins that are not yet supported by the uplc tool
        val supportedBuiltins = DefaultFun.values.filterNot(b =>
            b == DefaultFun.ListToArray ||
                b == DefaultFun.IndexArray ||
                b == DefaultFun.LengthOfArray
        )
        val builtinGen: Gen[Term] =
            for b <- Gen.oneOf(supportedBuiltins.toSeq) yield Term.Builtin(b)
        val constGen: Gen[Term] = for c <- Arbitrary.arbitrary[Constant] yield Term.Const(c)

        def sizedTermGen(sz: Int, env: immutable.List[String]): Gen[Term] =
            val maybeVarTerm = if env.isEmpty then Seq.empty else Seq(varGen(env))
            val simple = Gen.oneOf(
              Gen.const(Term.Error()),
              builtinGen,
              Seq(constGen) ++ maybeVarTerm*
            )
            if sz <= 0 then simple
            else
                // Gen.lzy builds a branch only when oneOf picks it. Built eagerly, every call
                // constructed all six recursive branches, and each of them called sizedTermGen
                // again: thousands of Gen objects per sample. Same values for the same seed.
                val common = Seq(
                  simple,
                  Gen.lzy(forceGen(sz, env)),
                  Gen.lzy(delayGen(sz, env)),
                  Gen.lzy(lamGen(sz, env)),
                  Gen.lzy(appGen(sz, env))
                )
                val sop =
                    if withConstrCase then
                        Seq(Gen.lzy(constrGen(sz, env)), Gen.lzy(caseGen(sz, env)))
                    else Seq.empty
                val branches = common ++ sop
                Gen.oneOf(branches(0), branches(1), branches.drop(2)*)

        def forceGen(sz: Int, env: immutable.List[String]): Gen[Term] =
            for t <- sizedTermGen(sz / 2, env)
            yield Term.Force(t)
        def delayGen(sz: Int, env: immutable.List[String]): Gen[Term] =
            for t <- sizedTermGen(sz / 2, env)
            yield Term.Delay(t)
        def lamGen(sz: Int, env: immutable.List[String]): Gen[Term] = for
            name <- nameGen
            t <- sizedTermGen(sz / 2, name :: env)
        yield Term.LamAbs(name, t)
        def appGen(sz: Int, env: immutable.List[String]): Gen[Term] = for
            t1 <- sizedTermGen(sz / 2, env)
            t2 <- sizedTermGen(sz / 2, env)
        yield Term.Apply(t1, t2)

        def constrGen(sz: Int, env: immutable.List[String]): Gen[Term] = for
            tag <- Gen.choose(0, 10)
            args <- Gen.listOfN(3, sizedTermGen(sz / 2, env))
        yield Term.Constr(Word64(tag), args)

        def caseGen(sz: Int, env: immutable.List[String]): Gen[Term] = for
            arg <- sizedTermGen(sz / 2, env)
            cases <- Gen.listOfN(3, sizedTermGen(sz / 2, env))
        yield Term.Case(arg, cases)

        Gen.sized(sizedTermGen(_, Nil))
    }

    /** The generator behind `arbitraryTerm`: terms with `Constr` and `Case`. */
    def genTerm: Gen[Term] = genTerm(withConstrCase = true)

    given arbitraryTerm: Arbitrary[Term] = Arbitrary(genTerm)

    given TermShrink: Shrink[Term] =
        given Shrink[Term] = TermShrink
        Shrink {
            case _: Term.Error        => Stream.empty
            case Term.Builtin(_, _)   => Stream.empty
            case Term.Const(const, _) => Shrink.shrink(const).map(Term.Const(_))
            case Term.Var(_, _)       => Stream.empty
            case Term.Force(t, _)     => Shrink.shrink(t).map(Term.Force(_))
            case Term.Delay(t, _)     => Shrink.shrink(t).map(Term.Delay(_))
            case Term.LamAbs(n, t, _) =>
                val tShrunk = Shrink.shrink(t).map(Term.LamAbs(n, _))
                tShrunk
            case Term.Apply(t1, t2, _) =>
                val t1Shrunk = Shrink.shrink(t1).map(Term.Apply(_, t2))
                val t2Shrunk = Shrink.shrink(t2).map(Term.Apply(t1, _))
                t1Shrunk ++ t2Shrunk
            // case Term.Constr(tag, args) => Shrink.shrink(args).map(Term.Constr(tag, _))
            // case Term.Case(arg, cases) =>
            // Shrink.shrink(arg) ++ Shrink.shrink(cases).map(Term.Case(arg, _))
            case _ => Stream.empty
        }

    /** The generator behind `Arbitrary[Program]` (`[gen.family.program-versions]`): 25 % version
      * 1.0.0 with a term without `Constr` and `Case` (`[gen.family.program-v100-terms]`), 25 %
      * version 1.1.0, and 50 % any version with a major and a minor of at least 1, so that every
      * construct parses.
      */
    def genProgram: Gen[Program] = {
        val anyVersion =
            for
                maj <- Gen.choose(1, 100)
                min <- Gen.choose(1, 100)
                patch <- Gen.posNum[Int]
            yield (maj, min, patch)
        Gen.frequency(
          1 -> genTerm(withConstrCase = false).map(Program((1, 0, 0), _)),
          1 -> genTerm.map(Program((1, 1, 0), _)),
          2 -> Gen.zip(anyVersion, genTerm).map(Program(_, _))
        )
    }

    given Arbitrary[Program] = Arbitrary(genProgram)

    given Shrink[Program] = Shrink { case Program(v, t) =>
        val tShrunk = Shrink.shrink(t).map(Program(v, _))
        tShrunk
    }

    given arbList[A: Arbitrary]: Arbitrary[scalus.cardano.onchain.plutus.prelude.List[A]] =
        Arbitrary {
            for lst <- Arbitrary.arbitrary[immutable.List[A]]
            yield scalus.cardano.onchain.plutus.prelude.List(lst*)
        }

    given arbOption[A: Arbitrary]: Arbitrary[scalus.cardano.onchain.plutus.prelude.Option[A]] =
        Arbitrary {
            for o <- Arbitrary.arbitrary[Option[A]]
            yield o.asScalus
        }

    given arbAssocMap[A: Arbitrary: Eq, B: Arbitrary]
        : Arbitrary[scalus.cardano.onchain.plutus.prelude.AssocMap[A, B]] =
        Arbitrary {
            for list <- Arbitrary.arbitrary[scalus.cardano.onchain.plutus.prelude.List[(A, B)]]
            yield scalus.cardano.onchain.plutus.prelude.AssocMap.fromList(list)
        }

    given arbSortedMap[A: Arbitrary: Ord, B: Arbitrary]
        : Arbitrary[scalus.cardano.onchain.plutus.prelude.SortedMap[A, B]] =
        Arbitrary {
            for list <- Arbitrary.arbitrary[scalus.cardano.onchain.plutus.prelude.List[(A, B)]]
            yield scalus.cardano.onchain.plutus.prelude.SortedMap.fromList(list)
        }
