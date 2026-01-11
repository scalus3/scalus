package scalus.uplc

import scalus.*
import scalus.uplc.eval.PlutusVM

import scala.reflect.ClassTag
import scala.scalajs.js
import scala.scalajs.js.annotation.JSImport

@js.native
@JSImport("child_process", JSImport.Namespace)
object ChildProcess extends js.Object {
    def execSync(command: String, options: js.UndefOr[js.Object] = js.undefined): js.Object |
        String =
        js.native
}

class CekBuiltinsJsTest extends CekBuiltinsTest:
    def evalUplcCli(t: Term): Term =
        import js.Dynamic.global as g

        val program = Program((1, 0, 0), t)
        val cp = g.require("child_process")
        val r = cp.spawnSync(
          "uplc",
          js.Array("evaluate"),
          js.Dynamic.literal(input = program.show)
        )
        if r.status.asInstanceOf[Int] != 0 then throw new Exception(r.stderr.toString)
        Term.parseUplc(r.stdout.toString()) match
            case Left(value)  => throw new Exception(s"Parse error: $value")
            case Right(value) => value

    override def assertTermEvalEq(a: Term, b: Term)(using vm: PlutusVM): Unit =
        // First evaluate with UPLC CLI
        assert(evalUplcCli(a) == b.evaluate, s"UPLC CLI: $a != $b")
        // Then also evaluate with Scalus VM
        super.assertTermEvalEq(a, b)

    override def assertTermEvalThrows[E <: Throwable: ClassTag](term: Term)(using
        vm: PlutusVM
    ): Unit =
        super.assertTermEvalThrows[E](term)
