package scalus.uplc
package transform

import org.scalatest.funsuite.AnyFunSuite
import scalus.uplc.Term.*
import scalus.cardano.onchain.plutus.prelude.bls12_381.{G1, G2}
import scalus.uplc.builtin.{platform, BuiltinValue, ByteString, Data}
import scalus.uplc.eval.PlutusVM
import scalus.uplc.transform.TermAnalysis.{isPure, isValueForm}

import scala.util.Try

/** Verifies [[TermAnalysis.isPure]] and [[TermAnalysis.isValueForm]] by induction on every
  * builtin's [[TypeScheme]], cross-checked against the CEK machine.
  *
  * For each builtin with `F = numTypeVars` and `A = arity`, every well-ordered chain
  * `Apply^a(Force^f(Builtin))` for `f ∈ [0, F+1]`, `a ∈ [0, A+1]` falls into exactly one class:
  *
  *   - `f ≤ F, a = 0` or `f = F, a < A`: a value — evaluates to itself, isValueForm, isPure
  *   - `f = F, a = A`: saturated — computes; pure iff the builtin is total
  *   - anything else (over-forced, under-forced with args, over-applied): a runtime error — neither
  *     value nor pure
  *
  * Interleaved chains (a Force outside an Apply) always error because builtin signatures are
  * prenex.
  *
  * Saturated cells execute the builtin, so arguments are derived structurally from the TypeScheme's
  * argument types (type variables default to Integer). Unsaturated and ill-formed cells never
  * execute the builtin — the CEK accumulates arguments without type-checking until saturation — so
  * argument values are irrelevant there.
  */
class BuiltinTypeSchemeChainTest extends AnyFunSuite:

    private given vm: PlutusVM = PlutusVM.makePlutusV3VM()

    private def evalEither(t: Term): Either[Throwable, Term] =
        try Right(t.evaluate)
        catch case e: Exception => Left(e)

    // ------------------------------------------------------------------
    // Default argument derivation
    // ------------------------------------------------------------------

    private lazy val g1 = G1.generator
    private lazy val g2 = G2.generator
    private lazy val ml = platform.bls12_381_millerLoop(g1, g2)

    /** MlResult constants are unprintable by design (no UPLC syntax), and some derived arguments
      * here are MlResults — guard clue rendering so a failing chain still reports its discrepancy
      * instead of crashing on Term.show.
      */
    private def safeShow(t: Term): String =
        try t.show
        catch case _: Exception => t.toString

    private def uniOf(ts: TypeScheme): DefaultUni = ts match
        case TypeScheme.Type(u)   => u
        case TypeScheme.TVar(_)   => DefaultUni.Integer
        case TypeScheme.App(f, a) => DefaultUni.Apply(uniOf(f), uniOf(a))
        case other => throw new IllegalArgumentException(s"unexpected argument type: $other")

    private def defaultForUni(u: DefaultUni): Constant = u match
        case DefaultUni.Integer                         => Constant.Integer(1)
        case DefaultUni.ByteString                      => Constant.ByteString(ByteString.empty)
        case DefaultUni.String                          => Constant.String("s")
        case DefaultUni.Bool                            => Constant.Bool(true)
        case DefaultUni.Unit                            => Constant.Unit
        case DefaultUni.Data                            => Constant.Data(Data.I(0))
        case DefaultUni.BLS12_381_G1_Element            => Constant.BLS12_381_G1_Element(g1)
        case DefaultUni.BLS12_381_G2_Element            => Constant.BLS12_381_G2_Element(g2)
        case DefaultUni.BLS12_381_MlResult              => Constant.BLS12_381_MlResult(ml)
        case DefaultUni.BuiltinValue                    => Constant.BuiltinValue(BuiltinValue.empty)
        case DefaultUni.Apply(DefaultUni.ProtoList, e)  => Constant.List(e, List())
        case DefaultUni.Apply(DefaultUni.ProtoArray, e) => Constant.Array(e, IndexedSeq())
        case DefaultUni.Apply(DefaultUni.Apply(DefaultUni.ProtoPair, a), b) =>
            Constant.Pair(defaultForUni(a), defaultForUni(b))
        case other => throw new IllegalArgumentException(s"no default constant for: $other")

    private def argTypes(ts: TypeScheme): List[TypeScheme] = ts match
        case TypeScheme.All(_, t)   => argTypes(t)
        case TypeScheme.Arrow(a, t) => a :: argTypes(t)
        case _                      => Nil

    // ------------------------------------------------------------------
    // Chain construction
    // ------------------------------------------------------------------

    private def chain(bn: DefaultFun, forces: Int, args: List[Term]): Term =
        val forced = (0 until forces).foldLeft(Builtin(bn): Term)((t, _) => Force(t))
        args.foldLeft(forced)((t, a) => Apply(t, a))

    private case class Row(bn: DefaultFun, numTypeVars: Int, arity: Int, defaults: List[Term])

    private lazy val rows: List[Row] = DefaultFun.values.toList.map { bn =>
        val ts = Meaning.allBuiltins.getBuiltinRuntime(bn).typeScheme
        val defaults = argTypes(ts).map(t => Const(defaultForUni(uniOf(t))): Term)
        Row(bn, ts.numTypeVars, ts.arity, defaults)
    }

    /** Builtins the VM cannot evaluate (missing runtime/cost model). Must stay empty so nothing is
      * excluded silently.
      */
    private lazy val vmUnavailable: Set[DefaultFun] =
        DefaultFun.values.filter(bn => evalEither(Builtin(bn)).isLeft).toSet

    // ------------------------------------------------------------------
    // Tests
    // ------------------------------------------------------------------

    test("every builtin has a runtime and derivable default arguments") {
        val failed = DefaultFun.values.toList.filter { bn =>
            Try {
                val ts = Meaning.allBuiltins.getBuiltinRuntime(bn).typeScheme
                argTypes(ts).map(t => defaultForUni(uniOf(t)))
            }.isFailure
        }
        assert(failed.isEmpty, s"no runtime or default args for: $failed")
    }

    test("builtin type schemes are not tail-swapped") {
        // Scala 3.3 right-associative *extension* methods take operands in the opposite order
        // to 3.4+, so a `->:` extension on DefaultUni silently swapped the last argument type
        // with the result type in every scheme built from two DefaultUni operands.
        import TypeScheme.{Arrow, Type}
        val int = Type(DefaultUni.Integer)
        val bool = Type(DefaultUni.Bool)
        val bs = Type(DefaultUni.ByteString)
        val str = Type(DefaultUni.String)
        val data = Type(DefaultUni.Data)
        def scheme(bn: DefaultFun) = Meaning.allBuiltins.getBuiltinRuntime(bn).typeScheme
        assert(scheme(DefaultFun.EqualsInteger) == Arrow(int, Arrow(int, bool)))
        assert(scheme(DefaultFun.LessThanInteger) == Arrow(int, Arrow(int, bool)))
        assert(scheme(DefaultFun.EqualsByteString) == Arrow(bs, Arrow(bs, bool)))
        assert(scheme(DefaultFun.LengthOfByteString) == Arrow(bs, int))
        assert(scheme(DefaultFun.EncodeUtf8) == Arrow(str, bs))
        assert(scheme(DefaultFun.DecodeUtf8) == Arrow(bs, str))
        assert(scheme(DefaultFun.IData) == Arrow(int, data))
        assert(scheme(DefaultFun.UnIData) == Arrow(data, int))
        assert(scheme(DefaultFun.SerialiseData) == Arrow(data, bs))
        assert(
          scheme(DefaultFun.ByteStringToInteger) == Arrow(bool, Arrow(bs, int))
        )
    }

    test("all builtins are evaluable on the CEK (no silent exclusions)") {
        assert(vmUnavailable.isEmpty, s"builtins unavailable on the VM: $vmUnavailable")
    }

    test("analysis and CEK agree on every (forces, applies) chain over every builtin") {
        val errors = List.newBuilder[String]
        for Row(bn, tvs, arity, defaults) <- rows do
            for f <- 0 to tvs + 1 do
                for a <- 0 to arity + 1 do
                    val args = (0 until a)
                        .map(i =>
                            if i < defaults.length then defaults(i)
                            else Const(Constant.Integer(1)): Term
                        )
                        .toList
                    val t = chain(bn, f, args)
                    val isValueExpected = (a == 0 && f <= tvs) || (f == tvs && a < arity)
                    val saturated = f == tvs && a == arity
                    val expectedPure = isValueExpected || (saturated && bn.isTotal)
                    def clue = s"$bn (typeVars=$tvs, arity=$arity) f=$f a=$a: ${safeShow(t)}"

                    if t.isValueForm != isValueExpected then
                        errors += s"isValueForm=${t.isValueForm}, expected $isValueExpected: $clue"
                    if t.isPure != expectedPure then
                        errors += s"isPure=${t.isPure}, expected $expectedPure: $clue"

                    if !vmUnavailable(bn) then
                        val res = evalEither(t)
                        if isValueExpected then
                            res match
                                case Right(v) =>
                                    if v ~!=~ t then
                                        errors += s"value chain did not evaluate to itself: $clue"
                                case Left(e) => errors += s"value chain failed with '$e': $clue"
                        else if saturated then
                            // total ⇒ any well-typed input succeeds; partial may go either way
                            if bn.isTotal && res.isLeft then
                                errors += s"saturated total builtin failed with " +
                                    s"'${res.swap.getOrElse("")}': $clue"
                        else if res.isRight then
                            errors += s"ill-formed chain unexpectedly succeeded: $clue"
        val found = errors.result()
        assert(found.isEmpty, s"${found.size} discrepancies:\n${found.mkString("\n")}")
    }

    test("ill-typed saturated chains are never classified pure") {
        // For every builtin, wrong-type the first concrete-typed (i.e. unlifted) parameter.
        // isTotal only covers well-typed inputs, so these must all be impure.
        val errors = List.newBuilder[String]
        val skipped = List.newBuilder[DefaultFun]
        for Row(bn, tvs, arity, defaults) <- rows do
            val params = argTypes(Meaning.allBuiltins.getBuiltinRuntime(bn).typeScheme)
            params.indexWhere(!_.isInstanceOf[TypeScheme.TVar]) match
                case -1 => skipped += bn
                case i =>
                    val wrong: Term =
                        if uniOf(params(i)) == DefaultUni.Integer then Const(Constant.Bool(true))
                        else Const(Constant.Integer(1))
                    val t = chain(bn, tvs, defaults.updated(i, wrong))
                    if t.isPure then errors += s"ill-typed saturated chain pure: ${safeShow(t)}"
        assert(skipped.result().isEmpty, s"builtins with no concrete param: ${skipped.result()}")
        val found = errors.result()
        assert(found.isEmpty, s"${found.size} discrepancies:\n${found.mkString("\n")}")
    }

    test("interleaved Force/Apply chains error and are classified non-value and impure") {
        val errors = List.newBuilder[String]
        for Row(bn, tvs, arity, defaults) <- rows if tvs >= 1 && arity >= 1 do
            // an Apply inserted before the last Force: Force(Apply(Force^(F-1)(b), arg))
            val inner = (0 until tvs - 1).foldLeft(Builtin(bn): Term)((t, _) => Force(t))
            val interleaved = List(
              Force(Apply(inner, defaults.head))
            ) ++ (if arity >= 2 then List(Apply(Force(Apply(inner, defaults.head)), defaults(1)))
                  else Nil)
            for t <- interleaved do
                def clue = s"$bn: ${safeShow(t)}"
                if t.isValueForm then errors += s"interleaved chain classified as value: $clue"
                if t.isPure then errors += s"interleaved chain classified as pure: $clue"
                if !vmUnavailable(bn) && evalEither(t).isRight then
                    errors += s"interleaved chain succeeded: $clue"
        val found = errors.result()
        assert(found.isEmpty, s"${found.size} discrepancies:\n${found.mkString("\n")}")
    }
