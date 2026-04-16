package scalus.compiler.sir.lowering

import org.scalatest.funsuite.AnyFunSuite
import scalus.*
import scalus.cardano.ledger.MajorProtocolVersion
import scalus.cardano.onchain.plutus.prelude.List as PList
import scalus.compiler.{Compile, Options}
import scalus.uplc.{Constant, DefaultUni, PlutusV3, Term}
import scalus.uplc.builtin.ByteString
import scalus.uplc.eval.PlutusVM

/** Fixtures — wrapping the `unboxedNil[A]` call inside `@Compile` methods gives each call site a
  * concrete `List[A]` result type, which is what the call-site interception in `Lowering.scala`
  * keys on.
  */
@Compile
object UnboxedNilFixtures {
    def bigIntUnboxedNil: PList[BigInt] = PList.unboxedNil[BigInt]
    def byteStringUnboxedNil: PList[ByteString] = PList.unboxedNil[ByteString]
    def bigIntEmpty: PList[BigInt] = PList.empty[BigInt]
}

/** Verifies `List.unboxedNil[A]` opts `A` into its native UPLC element representation —
  * `unboxedNil[BigInt]` lowers to `list<integer>` and `unboxedNil[ByteString]` to
  * `list<bytestring>`, distinct from the Data-preferred `list<data>` that `List.empty[A]` produces.
  * UPLC has no covariance, so this distinction matters at the constant level.
  */
class UnboxedNilTest extends AnyFunSuite {

    private given PlutusVM = PlutusVM.makePlutusV3VM(MajorProtocolVersion.vanRossemPV)
    private given Options = Options(
      targetLoweringBackend = scalus.compiler.sir.TargetLoweringBackend.SirToUplcV3Lowering,
      targetProtocolVersion = MajorProtocolVersion.vanRossemPV
    )

    /** Collect the element type of every `Constant.List` reachable in `term`. */
    private def listElemTypes(term: Term): Seq[DefaultUni] = {
        val buf = scala.collection.mutable.ArrayBuffer.empty[DefaultUni]
        def go(t: Term): Unit = t match
            case Term.Const(Constant.List(elemType, _), _)           => buf += elemType
            case Term.Const(_, _)                                    => ()
            case Term.Apply(f, a, _)                                 => go(f); go(a)
            case Term.LamAbs(_, body, _)                             => go(body)
            case Term.Force(x, _)                                    => go(x)
            case Term.Delay(x, _)                                    => go(x)
            case Term.Constr(_, args, _)                             => args.foreach(go)
            case Term.Case(scr, cases, _)                            => go(scr); cases.foreach(go)
            case Term.Var(_, _) | Term.Builtin(_, _) | Term.Error(_) => ()
        go(term)
        buf.toSeq
    }

    test("unboxedNil[BigInt] lowers to list<integer>") {
        val term = PlutusV3.compile(UnboxedNilFixtures.bigIntUnboxedNil.isEmpty).program.term
        val elems = listElemTypes(term)
        assert(
          elems.contains(DefaultUni.Integer),
          s"expected a list<integer> constant; saw element types: $elems"
        )
    }

    test("unboxedNil[ByteString] lowers to list<bytestring>") {
        val term =
            PlutusV3.compile(UnboxedNilFixtures.byteStringUnboxedNil.isEmpty).program.term
        val elems = listElemTypes(term)
        assert(
          elems.contains(DefaultUni.ByteString),
          s"expected a list<bytestring> constant; saw element types: $elems"
        )
    }

    test("List.empty[BigInt] still lowers to list<data> (Data-preferring default)") {
        val term = PlutusV3.compile(UnboxedNilFixtures.bigIntEmpty.isEmpty).program.term
        val elems = listElemTypes(term)
        assert(
          elems.contains(DefaultUni.Data) && !elems.contains(DefaultUni.Integer),
          s"expected list<data> (and not list<integer>); saw element types: $elems"
        )
    }
}
