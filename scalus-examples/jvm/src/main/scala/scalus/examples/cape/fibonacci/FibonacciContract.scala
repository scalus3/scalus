package scalus.examples.cape.fibonacci

import scalus.compiler.Options
import scalus.examples.cape.CapeMetadata
import scalus.uplc.PlutusV3
import scalus.uplc.Program
import scalus.uplc.transform.CaseConstrApply
import scalus.utils.BuildInfo

import java.nio.file.Files
import java.time.Instant

object FibonacciContract {
    private given Options = Options.releaseUntagged

    lazy val baseCompiled = PlutusV3.compile(FibonacciBase.fibonacci)
    lazy val baseProgram: Program = baseCompiled.program

    lazy val openProgram: Program = CaseConstrApply(FibonacciOpen.term).plutusV3

    @main def compileFibonacci(): Unit = {
        val version = BuildInfo.version
        val now = Instant.now().toString
        val sourceUrl =
            "https://github.com/AncientMariner/scalus2/tree/master/scalus-examples/jvm/src/main/scala/scalus/examples/cape/fibonacci/"

        // Base (naive recursion)
        val baseDir = java.nio.file.Paths.get("cape-submissions", "fibonacci_naive_recursion")
        Files.createDirectories(baseDir)
        Files.writeString(baseDir.resolve("fibonacci_naive_recursion.uplc"), baseProgram.show)
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
        println(s"Base UPLC written to: ${baseDir.resolve("fibonacci_naive_recursion.uplc")}")

        // Open
        val openDir = java.nio.file.Paths.get("cape-submissions", "fibonacci")
        Files.createDirectories(openDir)
        Files.writeString(openDir.resolve("fibonacci.uplc"), openProgram.show)
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
        println(s"Open UPLC written to: ${openDir.resolve("fibonacci.uplc")}")
    }
}
