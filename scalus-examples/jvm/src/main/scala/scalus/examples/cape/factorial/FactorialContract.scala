package scalus.examples.cape.factorial

import scalus.compiler.Options
import scalus.examples.cape.CapeMetadata
import scalus.uplc.PlutusV3
import scalus.uplc.Program
import scalus.uplc.transform.CaseConstrApply
import scalus.utils.BuildInfo

import java.nio.file.Files
import java.time.Instant

object FactorialContract {
    private given Options = Options.release

    lazy val baseCompiled = PlutusV3.compile(FactorialBase.factorial)
    lazy val baseProgram: Program = baseCompiled.program

    lazy val openProgram: Program = CaseConstrApply(FactorialOpen.term).plutusV3

    @main def compileFactorial(): Unit = {
        val version = BuildInfo.version
        val now = Instant.now().toString
        val sourceUrl =
            "https://github.com/AncientMariner/scalus2/tree/master/scalus-examples/jvm/src/main/scala/scalus/examples/cape/factorial/"

        // Base (naive recursion)
        val baseDir = java.nio.file.Paths.get("cape-submissions", "factorial_naive_recursion")
        Files.createDirectories(baseDir)
        Files.writeString(baseDir.resolve("factorial_naive_recursion.uplc"), baseProgram.show)
        Files.writeString(
          baseDir.resolve("metadata.json"),
          CapeMetadata(
            version = version,
            date = now,
            sourceUrl = sourceUrl,
            notes = "Scalus @Compile with Options.release (all optimizations, no traces)"
          )
        )
        println(s"Base script size: ${baseProgram.cborByteString.length} bytes")
        println(s"Base UPLC written to: ${baseDir.resolve("factorial_naive_recursion.uplc")}")

        // Open
        val openDir = java.nio.file.Paths.get("cape-submissions", "factorial")
        Files.createDirectories(openDir)
        Files.writeString(openDir.resolve("factorial.uplc"), openProgram.show)
        Files.writeString(
          openDir.resolve("metadata.json"),
          CapeMetadata(
            version = version,
            date = now,
            sourceUrl = sourceUrl,
            notes = "Hand-crafted UPLC with pfix combinator and CaseConstrApply optimization"
          )
        )
        println(s"Open script size: ${openProgram.cborByteString.length} bytes")
        println(s"Open UPLC written to: ${openDir.resolve("factorial.uplc")}")
    }
}
