package scalus.examples.cape.twopartyescrow

import scalus.compiler.Options
import scalus.examples.cape.CapeMetadata
import scalus.uplc.PlutusV3
import scalus.utils.BuildInfo

import java.nio.file.Files
import java.time.Instant

object TwoPartyEscrowContract {
    private given Options = Options.releaseUntagged
    lazy val compiled = PlutusV3.compile(TwoPartyEscrowValidator.validate)

    @main def compileTwoPartyEscrow(): Unit = {
        val program = compiled.program
        val scriptSize = compiled.script.script.size

        val outputDir = java.nio.file.Paths.get("cape-submissions", "two_party_escrow")
        Files.createDirectories(outputDir)

        Files.writeString(outputDir.resolve("two_party_escrow.uplc"), program.show)
        Files.writeString(
          outputDir.resolve("metadata.json"),
          CapeMetadata(
            version = BuildInfo.version,
            date = Instant.now().toString,
            sourceUrl =
                "https://github.com/AncientMariner/scalus2/tree/master/scalus-examples/jvm/src/main/scala/scalus/examples/cape/twopartyescrow/",
            notes = "Scalus @Compile with Options.release (all optimizations, no traces)"
          )
        )

        println(s"Script size: $scriptSize bytes (Plinth baseline: 3233 bytes)")
        println(s"UPLC written to: ${outputDir.resolve("two_party_escrow.uplc")}")
        println(s"Metadata written to: ${outputDir.resolve("metadata.json")}")
    }
}
