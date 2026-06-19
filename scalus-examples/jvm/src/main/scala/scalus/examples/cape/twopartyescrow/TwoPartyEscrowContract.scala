package scalus.examples.cape.twopartyescrow

import scalus.cardano.blueprint.{Blueprint, Contract}
import scalus.compiler.Options
import scalus.examples.cape.CapeMetadata
import scalus.uplc.PlutusV3
import scalus.utils.BuildInfo

import java.nio.file.Files
import java.time.Instant

object TwoPartyEscrowContract extends Contract {
    private given Options = Options.releaseUntagged
    lazy val compiled = PlutusV3.compile(TwoPartyEscrowValidator.validate)

    lazy val blueprint = Blueprint.plutusV3[EscrowDatum, BigInt](
      title = "Two-party escrow (CAPE)",
      description = "CAPE two-party escrow: the seller deposits, then the buyer accepts (releasing funds to " +
          "the seller) or the seller refunds after the deposit window. The redeemer selects the " +
          "action (0 = deposit, 1 = accept, 2 = refund).",
      version = "1.0.0",
      license = Some("Apache-2.0"),
      compiled = compiled
    )

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
