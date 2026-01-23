package scalus.compiler.sir

import org.scalatest.funsuite.AnyFunSuite
import scalus.*
import scalus.compiler.compile

class SIRTypeUnifyTest extends AnyFunSuite {

    test("Unification with upcasting [List and Cons]") {
        val list1FunSir = compile { (x: scalus.prelude.List[BigInt]) => x }
        val consFunSir =
            compile((x: BigInt) => scalus.prelude.List.Cons(x, scalus.prelude.List.Nil))

        val listTp = list1FunSir.tp match {
            case SIRType.Fun(x, tp) => tp
            case _                  => fail("Expected a function type")
        }

        val consTp = consFunSir.tp match {
            case SIRType.Fun(x, tp) => tp
            case _                  => fail("Expected a function type")
        }

        // println(s"listTp = ${listTp.show}, consTp = ${consTp.show}")

        val lub = SIRType.leastUpperBound(listTp, consTp)

        // println(s"lub = ${lub.show}")

        assert(lub ~=~ SIRType.List(SIRType.Integer))

    }

    test("Unification with upcasting [List[?] and Cons[A]]") {
        val tA = SIRType.TypeVar("A", Some(11L), false)

        val listTp = SIRType.List(SIRType.FreeUnificator)

        val consTp = SIRType.List.Cons(tA)

        // println(s"listTp = ${listTp.show}, consTp = ${consTp.show}")

        val lub = SIRType.leastUpperBound(listTp, consTp)

        // println(s"lub = ${lub.show}")

        assert(
          lub ~=~ SIRType.List(SIRType.FreeUnificator)
        )

    }

    test("parentSeq fron Cons[A] to List[?]") {
        val tA = SIRType.TypeVar("A", Some(11L), false)
        val consTp = SIRType.List.Cons(tA)
        val listTp = SIRType.List(SIRType.FreeUnificator)

        val parentsSeq =
            SIRUnify.subtypeSeq(consTp, listTp, SIRUnify.Env.empty)

        assert(parentsSeq.nonEmpty)

    }

    test("parentSeq fron Nil:List[Nothing] to List[Tuple2[BigInt, String]]") {
        // pending
        val nilFun = compile { (x: scalus.prelude.List.Nil.type) => x }
        val abList: SIR = compile { (x: scalus.prelude.List[(BigInt, String)]) =>
            x
        }

        val nilListType = nilFun.tp match {
            case SIRType.Fun(_, tp) => tp
            case _                  => fail("Expected a function type")
        }

        val listTupleType = abList.tp match {
            case SIRType.Fun(_, tp) => tp
            case _                  => fail("Expected a function type")
        }

        val parentsSeq =
            SIRUnify.subtypeSeq(nilListType, listTupleType, SIRUnify.Env.empty)

        assert(parentsSeq.nonEmpty)

    }

    test("parentSeq from Nil to List[Tuple]") {
        val nilFun = compile { (x: scalus.prelude.List.Nil.type) => x }
        val abList: SIR = compile { (x: scalus.prelude.List[(BigInt, String)]) =>
            x
        }
        val nilListType = nilFun.tp match {
            case SIRType.Fun(_, tp) => tp
            case _                  => fail("Expected a function type")
        }
        val nilType = nilListType match {
            case SIRType.SumCaseClass(decl, typeArgs) =>
                decl.constrType("scalus.cardano.onchain.plutus.prelude.List$.Nil")
            case c @ SIRType.CaseClass(constrDecl, typeArgs, parent) => c
            case _ => fail("Expected a case class type")
        }

        val listTupleType = abList.tp match {
            case SIRType.Fun(_, tp) => tp
            case _                  => fail("Expected a function type")
        }

        val parentsSeq =
            SIRUnify.subtypeSeq(nilListType, listTupleType, SIRUnify.Env.empty)

        assert(parentsSeq.nonEmpty)

    }

    test("calculating a type for foldLeft") {
        val tpA = SIRType.TypeVar("A", Some(1L), false)
        val tpB = SIRType.TypeVar("B", Some(2L), false)
        // foldLeft: (A,B): List[A] => B => ((B,A) => B) => B

        val tupleBigIntStringFun = compile { (x: (BigInt, String)) => x }
        val tupleBigIntStringType = tupleBigIntStringFun.tp match {
            case SIRType.Fun(_, tp) => tp
            case _                  => fail("Expected a function type")
        }

        val tupleType = tupleBigIntStringType match {
            case SIRType.CaseClass(constrDecl, typeArgs, optParent) =>
                val dataDecl = DataDecl(
                  constrDecl.name,
                  List(constrDecl),
                  constrDecl.typeParams,
                  constrDecl.annotations
                )
                dataDecl.constrType(constrDecl.name)
            case _ => fail("Expected a case class type")
        }

        def tuple2type(a: SIRType, b: SIRType): SIRType =
            SIRType.typeApply(tupleType, List(a, b))

        assert(
          tupleBigIntStringType ~=~ tuple2type(SIRType.Integer, SIRType.String)
        )

        val foldLeftFunType = SIRType.TypeLambda(
          List(tpA, tpB),
          SIRType.Fun(
            SIRType.List(tpA),
            SIRType.Fun(
              tpB,
              SIRType.Fun(
                SIRType.Fun(tuple2type(tpB, tpA), tpB),
                tpB
              )
            )
          )
        )

        val sirFoldLeft = SIR.Var("foldLeft", foldLeftFunType, AnnotationsDecl.empty)

        val foldArg2Type = SIRType.Fun(tupleBigIntStringType, SIRType.Integer)

        val arg0Type = SIRType.List(SIRType.String)

        val l = SIR.Var("l", arg0Type, AnnotationsDecl.empty)

        val apply1 = SIR.Apply(
          sirFoldLeft,
          l,
          SIRType.calculateApplyType(sirFoldLeft.tp, l.tp, Map.empty),
          AnnotationsDecl.empty
        )

        val s0 = SIR.Var("s0", SIRType.Integer, AnnotationsDecl.empty)

        val apply2 = SIR.Apply(
          apply1,
          s0,
          SIRType.calculateApplyType(apply1.tp, s0.tp, Map.empty),
          AnnotationsDecl.empty
        )

        val f = SIR.Var("f", foldArg2Type, AnnotationsDecl.empty)

        val apply3 = SIR.Apply(
          apply2,
          f,
          SIRType.calculateApplyType(apply2.tp, f.tp, Map.empty),
          AnnotationsDecl.empty
        )

        assert(
          foldLeftFunType ~=~ SIRType.Fun(
            SIRType.List(SIRType.String),
            SIRType.Fun(
              SIRType.Integer,
              SIRType.Fun(
                SIRType.Fun(tuple2type(SIRType.Integer, SIRType.String), SIRType.Integer),
                SIRType.Integer
              )
            )
          )
        )

        // println(s"apply1.tp = ${apply1.tp.show}")
        // println(s"apply2.tp = ${apply2.tp.show}")
        // println(s"apply3.tp = ${apply3.tp.show}")

        assert(
          apply3.tp ~=~ SIRType.Integer
        )

        assert(apply3.tp == SIRType.Integer)

    }

    test("Unification with TypeProxy should unroll proxy before unifying") {
        // This test reproduces the Groth16 bug where TypeProxy is not unrolled before unification

        // Create a TypeProxy that points to List[ByteString]
        val listByteString = SIRType.List(SIRType.ByteString)
        val proxy = SIRType.TypeProxy(listByteString)

        // Create the expected type: Proxy -> List[Int] -> ... should unify with List[ByteString] -> List[Int] -> ...
        val proxyFun =
            SIRType.Fun(proxy, SIRType.Fun(SIRType.List(SIRType.Integer), SIRType.Integer))
        val listFun =
            SIRType.Fun(listByteString, SIRType.Fun(SIRType.List(SIRType.Integer), SIRType.Integer))

        // Try to unify - this currently fails because Proxy is not unrolled
        val result = SIRUnify.topLevelUnifyType(
          proxyFun,
          listFun,
          SIRUnify.Env.empty.withUpcasting
        )

        result match {
            case SIRUnify.UnificationSuccess(env, tp) =>
                // Success! The types should unify
                assert(tp ~=~ listFun)
            case SIRUnify.UnificationFailure(path, l, r) =>
                fail(
                  s"TypeProxy should be unrolled before unification. Failure at path=$path, left=$l, right=$r"
                )
        }
    }

    test("Unification should handle TypeProxy in function argument position") {
        // Specific case from Groth16: Fun(Proxy(List[ByteString]), ...) vs Fun(List[ByteString], ...)

        val listByteString = SIRType.List(SIRType.ByteString)
        val proxy = SIRType.TypeProxy(listByteString)

        // The actual case: trying to unify Proxy -> X with List[ByteString] -> X
        val withProxy = SIRType.Fun(proxy, SIRType.Integer)
        val withoutProxy = SIRType.Fun(listByteString, SIRType.Integer)

        val result = SIRUnify.topLevelUnifyType(
          withProxy,
          withoutProxy,
          SIRUnify.Env.empty.withUpcasting
        )

        result match {
            case SIRUnify.UnificationSuccess(env, tp) =>
                assert(tp ~=~ withoutProxy)
            case SIRUnify.UnificationFailure(path, l, r) =>
                fail(
                  s"Should unify Proxy with its referenced type. Failure: path=$path, left=${l}, right=${r}"
                )
        }
    }

}
