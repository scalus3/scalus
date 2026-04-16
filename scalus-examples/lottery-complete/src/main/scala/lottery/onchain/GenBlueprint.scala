package lottery.onchain

import java.nio.file.{Files, Path}

/** Generates a CIP-57 plutus.json blueprint for the lottery validator. */
@main def genBlueprint(): Unit = {
    val json = LotteryContract.blueprint.toJson(2)
    Files.writeString(Path.of("plutus.json"), json)
    println(s"Generated plutus.json (${LotteryContract.compiled.script.script.size} bytes)")
}
