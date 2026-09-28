package scalus.verify

import org.scalatest.funsuite.AnyFunSuite
import scalus.cardano.onchain.plutus.prelude.Math
import scalus.compiler.sir.SIRType
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

    test("a compiled entry carries SIR and UPLC; backend representations can be added") {
        assert(clamp.available == Set("sir", "uplc"))
        assert(clamp(Representation.Uplc).term.toString.nonEmpty)
        val mapped = clamp.withLeanMapping("fun x lo hi => max lo (min x hi)")
        assert(mapped.available == Set("sir", "uplc", "lean-mapping"))
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
