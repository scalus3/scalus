package scalus.examples.setbench

import scalus.Compile
import scalus.uplc.builtin.ByteString
import scalus.uplc.builtin.ByteString.hex

/** Hardcoded G2 CRS (Common Reference String) for accumulator benchmark.
  *
  * First 2 G2 powers of tau from the Ethereum KZG ceremony (trusted_setup_32768.json). These are
  * compressed G2 points (96 bytes each).
  */
@Compile
object SetBenchCRS {
    val g2_0: ByteString =
        hex"93e02b6052719f607dacd3a088274f65596bd0d09920b61ab5da61bbdc7f5049334cf11213945d57e5ac7d055d042b7e024aa2b2f08f0a91260805272dc51051c6e47ad4fa403b02b4510b647ae3d1770bac0326a805bbefd48056c8c121bdb8"

    val g2_1: ByteString =
        hex"a78b94342f7d47a92f8618d0cf60cd3f8c77279ffafb2f0d71e4be074979f1b2f536007e9dcd236abaabcac3769930791224556839c0c3b5bf3f3bad9727dfc5c3326539883a6b798bef5302776ede7b939374a236e96658b269c3f4a2ea859e"
}
