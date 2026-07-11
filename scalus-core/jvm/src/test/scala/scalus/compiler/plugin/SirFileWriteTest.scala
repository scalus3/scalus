package scalus.compiler.plugin

import org.scalatest.funsuite.AnyFunSuite
import scalus.compiler.sir.Module
import scalus.serialization.flat.DecoderState
import scalus.serialization.flat.FlatInstances.given

import java.nio.file.Files

/** Audit finding M8 (compiler plugin audit): `SIRCompiler.writeModule` opened the output stream
  * before encoding and verifying the module, without try/finally — a failure in encode or the
  * roundtrip self-check leaked the stream and left a truncated `.sir` file that downstream units
  * would link against. The write was reordered to open the file only after a successful encode and
  * self-check. This characterization test pins the happy path: a compiled `@Compile` trait writes
  * a decodable `.sir` file.
  */
class SirFileWriteTest extends AnyFunSuite with SnippetCompilation {

    test("@Compile trait writes a decodable .sir module file") {
        val (errors, outDir) = compileSnippetWithOutput(
          """import scalus.compiler.Compile
            |@Compile
            |trait T {
            |    def one: BigInt = BigInt(1)
            |}
            |""".stripMargin
        )
        assert(errors.isEmpty, s"snippet must compile cleanly, got: $errors")
        val sirFile = outDir.resolve("T.sir")
        assert(Files.exists(sirFile), s"expected ${sirFile} to exist")
        val bytes = Files.readAllBytes(sirFile)
        assert(bytes.nonEmpty, ".sir file must not be empty")
        val decoded = summon[scalus.serialization.flat.Flat[Module]].decode(DecoderState(bytes))
        assert(decoded.name == "T", s"decoded module name: ${decoded.name}")
        assert(decoded.defs.exists(_.name.contains("one")), s"defs: ${decoded.defs.map(_.name)}")
    }
}
