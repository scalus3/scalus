package scalus.examples.cape.twopartyescrow

import scalus.compiler.Options
import scalus.uplc.PlutusV3

private given Options = Options.release
lazy val TwoPartyEscrowContract = PlutusV3.compile(TwoPartyEscrowValidator.validate)

@main def compileTwoPartyEscrow(): Unit = {
    val compiled = TwoPartyEscrowContract
    val program = compiled.program
    val scriptSize = compiled.script.script.size

    // Write UPLC text format
    val outputDir = java.nio.file.Paths.get("cape-submissions", "two_party_escrow")
    java.nio.file.Files.createDirectories(outputDir)

    val uplcPath = outputDir.resolve("two_party_escrow.uplc")
    java.nio.file.Files.writeString(uplcPath, program.show)

    // Write metadata.json
    val version = "0.16.0" // Update to match scalus version
    val metadata =
        s"""|{
            |  "compiler": "Scalus",
            |  "version": "$version",
            |  "options": "Options.release (all optimizations, no traces)",
            |  "source": "https://github.com/AncientMariner/scalus2/tree/master/scalus-examples/jvm/src/main/scala/scalus/examples/cape/twopartyescrow/"
            |}""".stripMargin
    val metadataPath = outputDir.resolve("metadata.json")
    java.nio.file.Files.writeString(metadataPath, metadata)

    println(s"Script size: $scriptSize bytes (Plinth baseline: 3233 bytes)")
    println(s"UPLC written to: $uplcPath")
    println(s"Metadata written to: $metadataPath")
}
