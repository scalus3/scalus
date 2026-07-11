package scalus.compiler.plugin

import org.scalatest.funsuite.AnyFunSuite

import java.io.{ByteArrayOutputStream, PrintStream}

/** Audit finding M1 (compiler plugin audit): `SIRCompiler.compileDefDef` contained a leftover
  * debug trigger `dd.symbol.fullName.toString == "b"` intended to flood stdout with compiler
  * traces for a method named `b`. Investigation showed a method's `fullName` can never be exactly
  * `"b"` from real source (local defs render as `V$._$b`, members as `Obj.b`), so the trigger was
  * dead code; it was removed under this characterization test, which pins the general invariant:
  * a clean compile with debug off produces no stdout at all.
  */
class NoDebugOutputTest extends AnyFunSuite with SnippetCompilation {

    private def captureStdOut[A](body: => A): (A, String) = {
        val buf = new ByteArrayOutputStream()
        val ps = new PrintStream(buf, true, "UTF-8")
        val oldSystemOut = System.out
        System.setOut(ps)
        try
            val result = Console.withOut(ps)(body)
            ps.flush()
            (result, buf.toString("UTF-8"))
        finally System.setOut(oldSystemOut)
    }

    test("compiling a def named 'b' does not print compiler debug traces") {
        val (errors, output) = captureStdOut {
            compileSnippet(
              """import scalus.compiler.*
                |object W {
                |    val sir = compile {
                |        def b(x: BigInt): BigInt = x + BigInt(1)
                |        b(BigInt(41))
                |    }
                |}
                |""".stripMargin
            )
        }
        assert(errors.isEmpty, s"snippet must compile cleanly, got: $errors")
        assert(
          output.trim.isEmpty,
          s"compiler output leaked to stdout on a clean compile:\n$output"
        )
    }
}
