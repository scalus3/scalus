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

import scala.math.pow

// ScalaCheck uses Stream for shrinking, which is deprecated
// Remove the deprecation warning for now
@nowarn("cat=deprecation")
trait ArbitraryInstances:

    def genByteStringOfN(n: Int): Gen[ByteString] = {
        Gen
            .containerOfN[Array, Byte](n, Arbitrary.arbitrary[Byte])
            .map(a => ByteString.unsafeFromArray(a))
    }

    given byteStringArb: Arbitrary[scalus.uplc.builtin.ByteString] = Arbitrary(
      for
          sz <- Gen.frequency((0, Gen.choose(1000, 10000)), (10, Gen.choose(0, 100)))
          bytes <- Gen.containerOfN[Array, Byte](sz, Arbitrary.arbitrary)
      yield scalus.uplc.builtin.ByteString.unsafeFromArray(bytes)
    )

    given Arbitrary[builtin.BLS12_381_G1_Element] = Arbitrary(
      for bs <- Arbitrary.arbitrary[ByteString]
      yield Builtins.bls12_381_G1_hashToGroup(bs, dst = utf8"Test")
    )

    given Arbitrary[builtin.BLS12_381_G2_Element] = Arbitrary(
      for bs <- Arbitrary.arbitrary[ByteString]
      yield Builtins.bls12_381_G2_hashToGroup(bs, dst = utf8"Test")
    )

    // from: https://stackoverflow.com/questions/24834074/how-to-create-a-bigint-by-rounding-from-a-double-in-scala
    private def round(d: Double) =
        BigDecimal(d).setScale(0, BigDecimal.RoundingMode.HALF_UP).toBigInt
    given iArb: Arbitrary[I] = Arbitrary(
      for n <- Gen.oneOf[BigInt](
            Gen.const[BigInt](0),
            Gen.const[BigInt](-1),
            Gen.const[BigInt](1),
            Gen.const[BigInt](-1000),
            Gen.const[BigInt](1000),
            Gen.const[BigInt](Int.MaxValue),
            Gen.const[BigInt](Int.MinValue),
            Gen.const[BigInt](Long.MaxValue),
            Gen.const[BigInt](Long.MinValue),
            Gen.choose[BigInt](round(pow(2, 128)) * -1, round(pow(2, 128)))
          )
      yield I(n)
    )
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

    given arbData: Arbitrary[Data] = Arbitrary {
        def constrGen(sz: Int): Gen[Constr] = for
            c <- Gen.posNum[Long]
            n <- Gen.choose(sz / 3, sz / 2)
            args <- Gen.listOfN(n, sizedTree(sz / 2))
        yield Constr(c, PList.from(args))

        def listGen(sz: Int): Gen[List] = for
            n <- Gen.choose(sz / 3, sz / 2)
            args <- Gen.listOfN(n, sizedTree(sz / 2))
        yield List(PList.from(args))

        def mapGen(sz: Int): Gen[Map] = for
            n <- Gen.choose(sz / 3, sz / 2)
            tuple = Gen.zip(sizedTree(sz / 2), sizedTree(sz / 2))
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
              // FIXME: don't generate strings because we don't handle Haskell style escapes
              // and we get errors parsing results of calling uplc on generated terms
//        DefaultUni.String,
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
                for
                    n <- Gen.choose(0, 10)
                    elems <- Gen.listOfN(n, arbConstantByType(arg))
                yield Constant.List(arg, elems)
            case DefaultUni.Apply(DefaultUni.Apply(ProtoPair, a), b) =>
                for
                    vala <- arbConstantByType(a)
                    valb <- arbConstantByType(b)
                yield Constant.Pair(vala, valb)
            case DefaultUni.BLS12_381_G1_Element =>
                Arbitrary
                    .arbitrary[builtin.BLS12_381_G1_Element]
                    .map(Constant.BLS12_381_G1_Element.apply)
            case DefaultUni.BLS12_381_G2_Element =>
                Arbitrary
                    .arbitrary[builtin.BLS12_381_G2_Element]
                    .map(Constant.BLS12_381_G2_Element.apply)
            case DefaultUni.BLS12_381_MlResult =>
                throw new IllegalArgumentException(
                  "BLS12_381_MlResult is not a valid constant type, it should be used in terms only"
                )
            case DefaultUni.BuiltinValue =>
                Gen.const(Constant.BuiltinValue(builtin.BuiltinValue.empty))
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

    given arbitraryTerm: Arbitrary[Term] = Arbitrary {
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
                b == DefaultFun.MultiIndexArray ||
                b == DefaultFun.LengthOfArray
        )
        val builtinGen: Gen[Term] =
            for b <- Gen.oneOf(supportedBuiltins.toSeq) yield Term.Builtin(b)
        val constGen: Gen[Term] = for c <- Arbitrary.arbitrary[Constant] yield Term.Const(c)

        def sizedTermGen(sz: Int, env: immutable.List[String]): Gen[Term] =
            val maybeVarTerm = if env.isEmpty then Seq.empty else Seq(varGen(env))
            val simple = Gen.oneOf(
              Gen.const(Term.Error),
              builtinGen,
              Seq(constGen) ++ maybeVarTerm*
            )
            if sz <= 0 then simple
            else
                Gen.oneOf(
                  simple,
                  forceGen(sz, env),
                  delayGen(sz, env),
                  lamGen(sz, env),
                  appGen(sz, env),
                  constrGen(sz, env),
                  caseGen(sz, env)
                )

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

    given TermShrink: Shrink[Term] =
        given Shrink[Term] = TermShrink
        Shrink {
            case Term.Error        => Stream.empty
            case Term.Builtin(_)   => Stream.empty
            case Term.Const(const) => Shrink.shrink(const).map(Term.Const.apply)
            case Term.Var(_)       => Stream.empty
            case Term.Force(t)     => Shrink.shrink(t).map(Term.Force.apply)
            case Term.Delay(t)     => Shrink.shrink(t).map(Term.Delay.apply)
            case Term.LamAbs(n, t) =>
                val tShrunk = Shrink.shrink(t).map(Term.LamAbs(n, _))
                tShrunk
            case Term.Apply(t1, t2) =>
                val t1Shrunk = Shrink.shrink(t1).map(Term.Apply(_, t2))
                val t2Shrunk = Shrink.shrink(t2).map(Term.Apply(t1, _))
                t1Shrunk ++ t2Shrunk
            // case Term.Constr(tag, args) => Shrink.shrink(args).map(Term.Constr(tag, _))
            // case Term.Case(arg, cases) =>
            // Shrink.shrink(arg) ++ Shrink.shrink(cases).map(Term.Case(arg, _))
            case _ => Stream.empty
        }

    given Arbitrary[Program] = Arbitrary {
        for
            // The parser will reject uses of new constructs if the version is not high enough
            // In order to keep our lives simple, we just generate a version that is always high
            // enough to support everything. That gives us less coverage of parsing versions, but
            // that's not likely to be the place where things go wrong
            maj <- Gen.choose(1, 100)
            min <- Gen.choose(1, 100)
            patch <- Gen.posNum[Int]
            term <- Arbitrary.arbitrary[Term]
        yield Program((maj, min, patch), term)
    }

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
