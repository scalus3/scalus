package scalus.sir

import org.scalatest.funsuite.AnyFunSuite
import scalus.Compiler.compile
import scalus.*

import scala.annotation.tailrec

object SIRTypingScalaToSIRSpecScope {

    case class ClassA1(a: BigInt)

    case class Wrapper[X](value: X)

    // Test enum with multiple cases
    enum MultiCaseEnum:
        case CaseA(value: BigInt)
        case CaseB(value: String)

    // Test enum with single case
    enum SingleCaseEnum:
        case OnlyCase(value: BigInt)

    sealed trait HierarchicalLevel1[A]
    case class LeafLevel1A[A](a: A) extends HierarchicalLevel1[A]
    sealed trait HierarchicalLevel2[B] extends HierarchicalLevel1[Wrapper[B]]
    case class LeafLevel2A[B](b: Wrapper[B]) extends HierarchicalLevel2[B]
    case class LeafLevel2B[B](ib: Int) extends HierarchicalLevel2[Int]

    // DataDecl(
    //    "scalus.sir.SIRTypingScalaToSIRSpecScope$.HierarchicalLevel1",
    //    List(
    //      ConstrDecl("LeafLevel1A", D, List(SIRType.Var("A",Some(1))),List(SIRType.Var("A",Some(1))))),
    //      ConstrDecl("_narrow_HierarchialLevel2", D, List(SIRType.Var("B",Some(2))),List(SIRType.Future(SIRType.Var("B",Some(1))))),
    //    ),
    //    None
    //  )
    //  DataDecl(
    //    "scalus.sir.SIRTypingScalaToSIRSpecScope$.HierarchicalLevel2",
    //    List(

}

class SIRTypingScalaToSIRTest extends AnyFunSuite {

    test("check that simple case class is mapped to case class in fun") {
        import SIRTypingScalaToSIRSpecScope.*

        val sir = compile { (x: BigInt) =>
            new ClassA1(x)
        }

        sir.tp match {
            case SIRType.Fun(SIRType.Integer, SIRType.CaseClass(constrDecl, Nil, None)) =>
                assert(constrDecl.name == "scalus.sir.SIRTypingScalaToSIRSpecScope$.ClassA1")
            case _ => fail(s"unexpected type ${sir.tp}")
        }

        // println(sir.pretty.render(100))

    }

    test("check that simple case class is mapped to case class") {
        import SIRTypingScalaToSIRSpecScope.*

        val sir = compile {
            new ClassA1(10)
        }

        sir.tp match {
            case SIRType.CaseClass(constrDecl, Nil, _) =>
                assert(constrDecl.name == "scalus.sir.SIRTypingScalaToSIRSpecScope$.ClassA1")
            case _ => fail(s"unexpected type ${sir.tp}")
        }

        // println(sir.pretty.render(100))

    }

    test("check that scalus.prelude.List is mapped to SumCaseClass") {
        val sir = compile {
            scalus.prelude.List.single(BigInt(1))
        }
        sir.tp match
            case SIRType.SumCaseClass(dataDecl, typeArgs) =>
                assert(dataDecl.name == "scalus.prelude.List")
                assert(typeArgs == List(SIRType.Integer))
            case _ => fail(s"unexpected type ${sir.tp}")

    }

    test("check that scalus.prelude.List is mapped to SumCaseClass in fun") {
        val sir = compile { (x: BigInt) =>
            scalus.prelude.List.single(x)
        }
        sir.tp match
            case SIRType.Fun(SIRType.Integer, SIRType.SumCaseClass(dataDecl, typeArgs)) =>
                assert(dataDecl.name == "scalus.prelude.List")
                assert(typeArgs == List(SIRType.Integer))
            case _ => fail(s"unexpected type ${sir.tp}")

    }

    test("check that apply with implicit parameters is mapped to corret SIR.Apply") {
        import scalus.prelude.*
        import scalus.builtin.ByteString
        val sir = compile { (l: scalus.prelude.List[ByteString], v: ByteString) =>
            l.contains(v)
        }
        val lSir = findLastLetBody(sir)
        lSir match {
            case SIR.LamAbs(a, SIR.LamAbs(b, body, _, _), _, _) =>
                body match {
                    case SIR.Apply(
                          SIR.Apply(
                            SIR.Apply(
                              vContains @ SIR.ExternalVar(module, name, tpf, _),
                              arg1,
                              tp1,
                              _
                            ),
                            arg2,
                            tp2,
                            _
                          ),
                          arg3,
                          tp3,
                          _
                        ) =>
                        assert(name == "scalus.prelude.List$.contains")
                        val aTp = new SIRType.TypeVar("A", None, false)
                        val bTp = new SIRType.TypeVar("B", None, false)
                        val tpf1 = tpf match {
                            case SIRType.TypeLambda(params, SIRType.Fun(ltp, rtpf1)) =>
                                rtpf1 match {
                                    case SIRType.TypeLambda(
                                          p1,
                                          SIRType.Fun(rtpf2, SIRType.Fun(v, rtpf3))
                                        ) =>
                                        assert(ltp ~=~ SIRType.List(aTp))
                                        assert(
                                          v ~=~ SIRType.Fun(bTp, SIRType.Fun(bTp, SIRType.Boolean))
                                        )
                                        assert(rtpf3 == SIRType.Boolean)
                                    case _ =>
                                        this.fail(s"unexpected type for tpf1: ${rtpf1.show}")
                                }
                            case _ =>
                                this.fail(s"unexpected type for tpf: ${tpf.show}")
                        }
                        assert(arg1.tp.isInstanceOf[SIRType.SumCaseClass])
                        assert(arg2.tp == SIRType.ByteString)
                        assert(
                          arg3.tp == SIRType.Fun(
                            SIRType.ByteString,
                            SIRType.Fun(SIRType.ByteString, SIRType.Boolean)
                          )
                        )
                    case _ =>
                        this.fail(s"unexpexted tree: ${body}")
                }
            case _ => this.fail(s"unexpected tree: ${lSir}")
        }
    }

    test("sirtype from Eq[B] should be a function") {
        val sir = compile { (x: scalus.prelude.Eq[BigInt]) => x }
        // println(s"sir.tp=${sir.tp.show}")
        sir.tp match {
            case SIRType.Fun(x1, y1) =>
                x1 match {
                    case SIRType.Fun(xp1, SIRType.Fun(xp2, SIRType.Boolean)) =>
                        assert(xp1 == SIRType.Integer)
                        assert(xp2 == SIRType.Integer)
                    case _ => fail(s"unexpected type for first argument, should be Fun ${x1.show}")
                }
            case _ => fail(s"functional type exprected ${sir.tp}")
        }
    }

    test("List.empty[B] should be a List with type B") {
        import scalus.prelude.List
        val sir = compile { (x: BigInt) => List.empty[BigInt] }
        sir.tp match {
            case SIRType.Fun(SIRType.Integer, SIRType.SumCaseClass(dataDecl, typeArgs)) =>
                assert(dataDecl.name == "scalus.prelude.List")
                assert(typeArgs.head ~=~ SIRType.Integer)
            case _ => fail(s"unexpected type ${sir.tp}")
        }
    }

    @tailrec
    private def findLastLetBody(x: SIR): SIR = {
        x match {
            case SIR.Decl(data, term)   => findLastLetBody(term)
            case SIR.Let(_, body, _, _) => findLastLetBody(body)
            case _                      => x
        }
    }

    test("enum with multiple cases: each case should have parent") {
        import SIRTypingScalaToSIRSpecScope.*

        val sir = compile { (x: BigInt) =>
            MultiCaseEnum.CaseA(x)
        }

        sir.tp match {
            case SIRType.Fun(SIRType.Integer, SIRType.SumCaseClass(decl, typeArgs)) =>
                val constrDecl: ConstrDecl = decl.constructors.find(_.name.contains("CaseA")).get
                val caseClass = decl.constrType(constrDecl.name) match {
                    case cc: SIRType.CaseClass => cc
                    case other =>
                        fail(s"Expected CaseClass but got: ${other}")
                }
                assert(
                  constrDecl.name.contains("CaseA"),
                  s"Expected CaseA but got ${constrDecl.name}"
                )
                val parent = caseClass.parent.get
                assert(
                  caseClass.show.contains("MultiCaseEnum"),
                  s"Expected parent MultiCaseEnum but got ${parent.show}"
                )
            case other =>
                fail(s"Expected Fun(Integer, CaseClass(..., Some(parent))) but got: ${other}")
        }
    }

    test("enum with single case: case should have parent") {
        import SIRTypingScalaToSIRSpecScope.*

        val sir = compile { (x: BigInt) =>
            SingleCaseEnum.OnlyCase(x)
        }

        val tp = findLastLetBody(sir) match {
            case SIR.LamAbs(x, body, _, _) =>
                body match {
                    case SIR.Cast(term, tp, _) =>
                        term.tp
                    case _ =>
                        fail(s"Expected Cast but got: ${body}")
                }
            case _ =>
                fail(s"Expected LamAbs but got: ${sir}")
        }

        tp match {
            case SIRType.CaseClass(constrDecl, Nil, Some(parent)) =>
                assert(
                  constrDecl.name.contains("OnlyCase"),
                  s"Expected OnlyCase but got ${constrDecl.name}"
                )
                assert(
                  parent.show.contains("SingleCaseEnum"),
                  s"Expected parent SingleCaseEnum but got ${parent.show}"
                )
            case other =>
                fail(s"Expected Fun(Integer, CaseClass(..., Some(parent))) but got: ${other}")
        }
    }

    test("regular case class should not have parent") {
        import SIRTypingScalaToSIRSpecScope.*

        val sir = compile { (x: BigInt) =>
            ClassA1(x)
        }

        sir.tp match {
            case SIRType.Fun(SIRType.Integer, SIRType.CaseClass(constrDecl, Nil, None)) =>
                assert(
                  constrDecl.name.contains("ClassA1"),
                  s"Expected ClassA1 but got ${constrDecl.name}"
                )
            case other => fail(s"Expected Fun(Integer, CaseClass(..., None)) but got: ${other}")
        }
    }

}
