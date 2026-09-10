package scalus.compiler.plugin

import org.scalatest.funsuite.AnyFunSuite
import scalus.compiler.sir.SIR
import scalus.uplc.Constant
import scalus.uplc.builtin.ByteString

import java.net.URLClassLoader

class LargeSirStringTest extends AnyFunSuite with SnippetCompilation {
    test("large SIR with two-byte modified-UTF-8 characters compiles and roundtrips") {
        val hex = "ff" * 44000
        val (errors, outDir) = compileSnippetWithOutput(
          s"""import scalus.compiler.*
             |import scalus.uplc.builtin.ByteString
             |object LargeSir {
             |  val sir = compile { ByteString.fromHex("$hex") }
             |}
             |""".stripMargin
        )
        assert(errors.isEmpty, s"large SIR must fit JVM string constants, got: $errors")
        val loader = new URLClassLoader(Array(outDir.toUri.toURL), getClass.getClassLoader)
        try {
            val cls = loader.loadClass("LargeSir$")
            val module = cls.getField("MODULE$").get(null)
            val sir = cls.getMethod("sir").invoke(module).asInstanceOf[SIR]
            sir match
                case SIR.Const(Constant.ByteString(value), _, _) =>
                    assert(value == ByteString.fromHex(hex))
                case other => fail(s"expected a ByteString constant, got: ${other.getClass}")
        } finally loader.close()
    }
}
