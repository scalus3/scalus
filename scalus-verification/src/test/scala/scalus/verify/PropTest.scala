package scalus.verify

import org.scalatest.funsuite.AnyFunSuite
import scalus.cardano.onchain.plutus.prelude.Math
import scalus.compiler.sir.{SIR, SIRType}
import Props.*

class PropTest extends AnyFunSuite {

    private val clamp = FunctionDef(Math.clamp)
    private val div10 = FunctionDef.named("div10", (x: BigInt) => BigInt(10) / x)

    test("Boolean leaves are compiled to SIR") {
        val p = Prop(BigInt(1) > 0)
        p match
            case Prop.Bool(sir) =>
                assert(sir.toString.nonEmpty)
            case other => fail(s"expected captured Boolean code, got $other")
    }

    test("forAll captures its binder and Boolean SIR without a Scala closure") {
        val p = forAll[BigInt](x => x > 0)
        p match
            case Prop.Forall(ident, Prop.Bool(PropExpr.SIRExpr(body))) =>
                assert(ident.name.nonEmpty)
                assert(ident.tp == SIRType.Integer)
                assert(body.toString.contains(ident.name))
            case other => fail(s"expected an explicit quantifier, got $other")
    }

    test("exists captures its binder and Boolean SIR without a Scala closure") {
        val p = exists[BigInt](x => x > 0)
        p match
            case Prop.Exists(ident, None, Prop.Bool(PropExpr.SIRExpr(body))) =>
                assert(ident.name.nonEmpty)
                assert(ident.tp == SIRType.Integer)
                assert(body.toString.contains(ident.name))
            case other => fail(s"expected an explicit existential, got $other")
    }

    test("existsLet captures its witness and Boolean SIR without Scala closures") {
        val p = existsLet(BigInt(2))(x => x > 0)
        p match
            case Prop.Exists(
                  ident,
                  Some(PropExpr.SIRExpr(witness)),
                  Prop.Bool(PropExpr.SIRExpr(body))
                ) =>
                assert(ident.tp == SIRType.Integer)
                assert(witness.toString.nonEmpty)
                assert(body.toString.contains(ident.name))
            case other => fail(s"expected an existential with a compiled witness, got $other")
    }

    test("calls capture their arguments and continuations as SIR") {
        val total = call(div10, BigInt(2))(r => r > 0)
        val partial = whenReturnsRef(div10.ref, BigInt(2))(r => r > 0)
        total match
            case Prop.Call(
                  fn,
                  PropExpr.SIRExpr(arg),
                  result,
                  true,
                  Prop.Bool(PropExpr.SIRExpr(body))
                ) =>
                assert(fn == div10.ref)
                assert(arg.toString.nonEmpty)
                assert(result.tp == SIRType.Integer)
                assert(body.toString.contains(result.name))
            case other => fail(s"expected an explicit call, got $other")
        partial match
            case Prop.Call(fn, _, _, false, _) => assert(fn == div10.ref)
            case other                         => fail(s"expected a partial call, got $other")
    }

    test("leaves under nested binders compile closed lambdas over the binders' variables") {
        forAll[BigInt](x => exists[BigInt](y => Prop(y > x))) match
            case Prop.Forall(x, Prop.Exists(y, None, Prop.Bool(PropExpr.SIRExpr(body)))) =>
                assert(x.tp == SIRType.Integer && y.tp == SIRType.Integer)
                assert(x.name != y.name)
                assert(!body.isInstanceOf[SIR.LamAbs], body)
                assert(body.toString.contains(x.name) && body.toString.contains(y.name), body)
            case other => fail(s"expected a universal and an existential quantifier, got $other")

        forAll[BigInt](x => denotes(BigInt(10) / x)) match
            case Prop.Forall(x, Prop.Denotes(PropExpr.SIRExpr(body))) =>
                assert(body.toString.contains(x.name), body)
            case other => fail(s"expected a universal denotes, got $other")
    }

    test("a call's argument and continuation use the variables of enclosing binders") {
        forAll[BigInt, BigInt]((lo, hi) =>
            callRef(clamp.ref, (BigInt(0), lo, hi))(r => lo <= r)
        ) match
            case Prop.Forall(
                  lo,
                  Prop.Forall(
                    hi,
                    Prop.Call(_, PropExpr.SIRExpr(arg), r, true, Prop.Bool(PropExpr.SIRExpr(test)))
                  )
                ) =>
                assert(arg.toString.contains(lo.name) && arg.toString.contains(hi.name), arg)
                assert(test.toString.contains(lo.name) && test.toString.contains(r.name), test)
            case other => fail(s"expected a call under two quantifiers, got $other")
    }

    test("a Boolean body is one test, with its if, match and local values") {
        forAll[Boolean, BigInt]((flag, x) => if flag then x > BigInt(0) else x < BigInt(0)) match
            case Prop.Forall(flag, Prop.Forall(x, Prop.Bool(PropExpr.SIRExpr(body)))) =>
                assert(flag.tp == SIRType.Boolean && x.tp == SIRType.Integer)
                assert(body.toString.contains(flag.name) && body.toString.contains(x.name), body)
            case other => fail(s"expected one test under two quantifiers, got $other")

        forAll[BigInt](x => { val y = x + 1; y > x }) match
            case Prop.Forall(x, Prop.Bool(PropExpr.SIRExpr(body))) =>
                assert(body.toString.contains(x.name), body)
            case other => fail(s"expected one test with a local value, got $other")

        forAll[BigInt](x =>
            callRef(div10.ref, x)(r =>
                (x == BigInt(0)) match
                    case true  => r == BigInt(0)
                    case false => r <= BigInt(10)
            )
        ) match
            case Prop.Forall(x, Prop.Call(_, _, r, true, Prop.Bool(PropExpr.SIRExpr(body)))) =>
                assert(body.toString.contains(x.name) && body.toString.contains(r.name), body)
            case other => fail(s"expected a call continuing with one test, got $other")
    }

    test("a contract states requires ==> whenReturns ensures over the function's parameters") {
        val inRange = contract(clamp)(
          requires = (x, lo, hi) => lo <= hi,
          ensures = (x, lo, hi) => r => lo <= r && r <= hi
        )
        assert(inRange.function == clamp.ref && !inRange.total)
        inRange.prop match
            case Prop.Forall(
                  x,
                  Prop.Forall(
                    lo,
                    Prop.Forall(
                      hi,
                      Prop.Implies(
                        Prop.Bool(PropExpr.SIRExpr(pre)),
                        Prop.Call(
                          fn,
                          PropExpr.SIRExpr(arg),
                          r,
                          false,
                          Prop.Bool(PropExpr.SIRExpr(post))
                        )
                      )
                    )
                  )
                ) =>
                assert(fn == clamp.ref)
                assert(pre.toString.contains(lo.name) && pre.toString.contains(hi.name), pre)
                assert(List(x, lo, hi).forall(v => arg.toString.contains(v.name)), arg)
                // ensures names its own parameters; they are renamed after those of requires
                assert(List(lo, hi, r).forall(v => post.toString.contains(v.name)), post)
            case other => fail(s"expected a contract over three parameters, got $other")

        val total = totalContract(div10)(
          requires = x => x != BigInt(0),
          ensures = x => r => r * x <= BigInt(10)
        )
        assert(total.function == div10.ref && total.total)
        total.prop match
            case Prop.Forall(_, Prop.Implies(_, Prop.Call(fn, _, _, true, _))) =>
                assert(fn == div10.ref)
            case other => fail(s"expected a total contract, got $other")

        // A statement in ensures is renamed as well.
        contract(div10)(
          requires = x => x > BigInt(0),
          ensures = x => r => Prop(r >= BigInt(0)) && Prop(r <= x * BigInt(10))
        ).prop match
            case Prop.Forall(
                  x,
                  Prop.Implies(
                    _,
                    Prop.Call(_, _, _, _, Prop.And(_, Prop.Bool(PropExpr.SIRExpr(bound))))
                  )
                ) =>
                assert(bound.toString.contains(x.name), bound)
            case other => fail(s"expected a contract with a statement in ensures, got $other")
    }

    test("a variable used to compute the statement itself is a compile error") {
        val errors = scala.compiletime.testing.typeCheckErrors(
          """import scalus.verify.*
             import scalus.verify.Props.*
             forAll[BigInt](x => if x > BigInt(0) then Prop(true) else Prop(false))"""
        )
        assert(errors.exists(_.message.contains("is a variable of the statement")), errors)
        // The macro stops at its own error, before it builds a body out of the variable's scope.
        assert(!errors.exists(_.message.contains("outside the scope")), errors)
    }

    test("a body that is a statement in one branch and a Boolean in another is a compile error") {
        val errors = scala.compiletime.testing.typeCheckErrors(
          """import scalus.verify.*
             import scalus.verify.Props.*
             forAll[BigInt](x =>
                 if BigInt(1) > BigInt(0) then denotes(BigInt(10) / x) else x > BigInt(0)
             )"""
        )
        assert(
          errors.exists(_.message.contains("a statement in one branch and a Boolean test")),
          errors
        )
    }

    test("a method reference is named as SIR names it, a lambda by its synthetic name") {
        assert(clamp.name == "scalus.cardano.onchain.plutus.prelude.Math$.clamp", clamp.name)
        assert(clamp.ref.displayName == "clamp")
        assert(div10.name == "div10")
    }

    test("FunctionRef captures a @Compile method without compiling it") {
        val ref = FunctionRef(Math.clamp)
        assert(ref == clamp.ref)
        val p = call(Math.clamp, (BigInt(2), BigInt(0), BigInt(10)))(r => r == BigInt(2))
        p match
            case Prop.Call(fn, _, _, true, _) => assert(fn == ref)
            case other                        => fail(s"expected a call of Math.clamp, got $other")
    }

    test("FunctionRef rejects arbitrary lambdas and methods outside @Compile objects") {
        val methodErrors = scala.compiletime.testing.typeCheckErrors(
          """import scalus.verify.FunctionRef
             object Plain { def id(x: BigInt): BigInt = x }
             FunctionRef(Plain.id)"""
        )
        assert(methodErrors.exists(_.message.contains("not in a @Compile object")))
        val lambdaErrors = scala.compiletime.testing.typeCheckErrors(
          """import scalus.verify.FunctionRef
             FunctionRef((x: BigInt) => x + BigInt(1))"""
        )
        assert(lambdaErrors.exists(_.message.contains("other arguments")))
    }

    test(
      "a compiled entry carries SIR, UPLC and its signature; backend representations can be added"
    ) {
        assert(clamp.available == Set("sir", "uplc", "uplc-signature"))
        assert(clamp(Representation.Uplc).term.toString.nonEmpty)
        clamp(Representation.UplcSignature) match
            case UplcSignature.Represented(parameters, result) =>
                assert(parameters.size == 3 && (result :: parameters).forall(_.show == "Constant"))
            case other => fail(s"expected the V3 lowering's signature, got $other")
        val mapped = clamp.withLeanMapping("fun x lo hi => max lo (min x hi)")
        assert(mapped.available == Set("sir", "uplc", "uplc-signature", "lean-mapping"))
        assertThrows[NoSuchElementException](clamp(Representation.LeanMapping))
    }

    test("a function missing from the table is a setup error") {
        assertThrows[NoSuchElementException](FunctionTable.empty(div10.ref))
    }

    test("two different functions under one name are rejected") {
        val other = FunctionDef.named("div10", (x: BigInt) => x)
        assertThrows[IllegalArgumentException](FunctionTable(div10, other))
    }

    test("a synthetic name cannot look like a qualified one") {
        assertThrows[IllegalArgumentException](FunctionDef.named("my.div", (x: BigInt) => x))
    }
}
