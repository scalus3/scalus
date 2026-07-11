package scalus.compiler.sir

import org.scalatest.funsuite.AnyFunSuite
import scalus.compiler.sir.SIRType.TypeVar

/** Regression tests for fresh TypeVar id generation (audit 2026-07-10, T1/T2).
  *
  * T1: `createMinimalTypeVarGenerationContext` must scan the whole worklist — branches that
  * short-circuit (id-carrying TypeVar, already-visited decls/proxies) must still continue with the
  * rest of the worklist, otherwise `maxCounter` understates and `freshCopy` can mint an id that
  * collides with an existing variable.
  *
  * T2: `retrieveTypeVarGenerationContext` must cache the context in the env, so fresh vars minted
  * by one TypeLambda rename are visible to the next within the same unification session.
  */
class TypeVarGenerationContextTest extends AnyFunSuite {

    test("scan continues past an id-carrying TypeVar") {
        val a = TypeVar("A", Some(1L))
        val b = TypeVar("B", Some(2L))
        val ctx = SIRType.createMinimalTypeVarGenerationContext(0L, List(a, b))
        assert(ctx.maxCounter == 2L)
        assert(ctx.contains(a))
        assert(ctx.contains(b))
        val fresh = ctx.freshCopy(b)
        assert(fresh != b)
    }

    test("scan continues past an already-visited SumCaseClass") {
        val listInt = SIRType.List(SIRType.Integer)
        val sentinel = TypeVar("C", Some(1000L))
        val ctx =
            SIRType.createMinimalTypeVarGenerationContext(0L, List(listInt, listInt, sentinel))
        assert(ctx.maxCounter >= 1000L)
        assert(ctx.contains(sentinel))
    }

    test("scan continues past an already-visited CaseClass") {
        val consInt = SIRType.List.Cons(SIRType.Integer)
        val sentinel = TypeVar("D", Some(1000L))
        val ctx =
            SIRType.createMinimalTypeVarGenerationContext(0L, List(consInt, consInt, sentinel))
        assert(ctx.maxCounter >= 1000L)
        assert(ctx.contains(sentinel))
    }

    test("scan continues past an already-visited TypeProxy") {
        val proxy = SIRType.TypeProxy(SIRType.List.Cons(SIRType.Integer))
        val sentinel = TypeVar("E", Some(1000L))
        val ctx = SIRType.createMinimalTypeVarGenerationContext(0L, List(proxy, proxy, sentinel))
        assert(ctx.maxCounter >= 1000L)
        assert(ctx.contains(sentinel))
    }

    test("retrieveTypeVarGenerationContext caches the context in the env") {
        val env = SIRUnify.Env.topLevel(SIRType.List(SIRType.Integer))
        val ctx1 = SIRUnify.retrieveTypeVarGenerationContext(env)
        val ctx2 = SIRUnify.retrieveTypeVarGenerationContext(env)
        assert(ctx1 eq ctx2)
        assert(env.optTypeVarGenerationContext.exists(_ eq ctx1))
    }

    test("fresh vars minted through one retrieval are visible to the next") {
        val tv = TypeVar("A", Some(1L))
        val env = SIRUnify.Env.topLevel(tv)
        val fresh1 = SIRUnify.retrieveTypeVarGenerationContext(env).freshCopy(tv)
        val fresh2 = SIRUnify.retrieveTypeVarGenerationContext(env).freshCopy(tv)
        assert(fresh1 != fresh2)
    }

}
