package scalus.serialization.flat

import org.scalatest.funsuite.AnyFunSuite
import scalus.compiler.sir.*

/** Hash-consed serialization keys start from the 32-bit `hashCode` of a node. Two structurally
  * different nodes can share a hashCode, so the encoder must not dedup on the key alone: it probes
  * for a free key on collision (see `HashConsed.allocKey`). These tests force a collision using
  * strings with equal hashCodes ("Aa" / "BB") and check that both nodes survive a roundtrip.
  */
class HashConsedCollisionTest extends AnyFunSuite {

    private def constrDecl(name: String): ConstrDecl =
        ConstrDecl(name, Nil, Nil, Nil, AnnotationsDecl.emptyModule)

    private def caseClass(name: String): SIRType.CaseClass =
        SIRType.CaseClass(constrDecl(name), Nil, None)

    private def encodeDecode(tp: SIRType): SIRType = {
        val bitSize = ToExprHSSIRTypeFlat.bitSize(tp)
        val encoded = EncoderState(bitSize / 8 + 1)
        ToExprHSSIRTypeFlat.encode(tp, encoded)
        ToExprHSSIRTypeFlat.decode(DecoderState(encoded.buffer))
    }

    test("colliding fixture really collides") {
        assert("Aa".hashCode == "BB".hashCode)
        assert(constrDecl("Aa").hashCode == constrDecl("BB").hashCode)
        assert(constrDecl("Aa") != constrDecl("BB"))
        assert(caseClass("Aa").hashCode == caseClass("BB").hashCode)
        assert(caseClass("Aa") != caseClass("BB"))
    }

    test("SIRType nodes with colliding hashCodes both survive a roundtrip") {
        val decoded = encodeDecode(SIRType.Fun(caseClass("Aa"), caseClass("BB")))
        decoded match
            case SIRType.Fun(in: SIRType.CaseClass, out: SIRType.CaseClass) =>
                assert(in.constrDecl.name == "Aa")
                assert(out.constrDecl.name == "BB")
            case other => fail(s"Expected Fun(CaseClass, CaseClass), got $other")
    }

    test("ConstrDecls with colliding hashCodes both survive a roundtrip") {
        val dataDecl = DataDecl(
          "CollidingData",
          List(constrDecl("Aa"), constrDecl("BB")),
          Nil,
          AnnotationsDecl.emptyModule
        )
        val decoded = encodeDecode(SIRType.SumCaseClass(dataDecl, Nil))
        decoded match
            case SIRType.SumCaseClass(decl, _) =>
                assert(decl.constructors.map(_.name) == List("Aa", "BB"))
            case other => fail(s"Expected SumCaseClass, got $other")
    }

    test("structurally equal nodes still dedup to one encoding") {
        val tp1 = caseClass("Aa")
        val tp2 = caseClass("Aa")
        assert(tp1 ne tp2)
        val together = ToExprHSSIRTypeFlat.bitSize(SIRType.Fun(tp1, tp2))
        val single = ToExprHSSIRTypeFlat.bitSize(SIRType.Fun(tp1, tp1))
        assert(together == single)
        val decoded = encodeDecode(SIRType.Fun(tp1, tp2))
        decoded match
            case SIRType.Fun(in: SIRType.CaseClass, out: SIRType.CaseClass) =>
                assert(in.constrDecl.name == "Aa")
                assert(out.constrDecl.name == "Aa")
            case other => fail(s"Expected Fun(CaseClass, CaseClass), got $other")
    }

}
