package scalus.crypto.accumulator

import com.github.plokhotnyuk.jsoniter_scala.core.*
import com.github.plokhotnyuk.jsoniter_scala.macros.*
import scalus.uplc.builtin.ByteString
import scalus.uplc.builtin.bls12_381.{G1Element, G2Element}

/** Loads the Ethereum KZG ceremony trusted setup (32768 G1 points, 65 G2 points) and converts it
  * to a [[BilinearAccumulatorProver.Setup]].
  */
object EthereumKzgCeremony {

    /** Load Setup from the Ethereum KZG ceremony, supporting up to maxElements set elements.
      * maxElements must be <= 32767 (the ceremony has 32768 G1 points).
      */
    def loadSetup(maxElements: Int = 32000): BilinearAccumulatorProver.Setup = {
        require(
          maxElements > 0 && maxElements <= 32767,
          s"maxElements must be in 1..32767, got $maxElements"
        )
        val ceremony = loadCeremony()
        BilinearAccumulatorProver.Setup.fromPoints(
          ceremony.g1Monomial.toVector,
          ceremony.g2Monomial.toVector
        )
    }

    /** Raw ceremony data — exposed for consumers that need direct access to the G1/G2 points. */
    def loadCeremony(): EthereumCeremony = {
        val input = getClass.getResourceAsStream("/trusted_setup_32768.json")
        require(input != null, "trusted_setup_32768.json not found on classpath")
        try readFromStream[EthereumCeremony](input)
        finally input.close()
    }

    case class EthereumCeremony(
        g1Monomial: List[G1Element],
        g2Monomial: List[G2Element]
    )

    private given JsonValueCodec[G1Element] = new JsonValueCodec[G1Element] {
        def decodeValue(in: JsonReader, default: G1Element): G1Element = {
            val hex = in.readString("")
            G1Element(ByteString.fromHex(hex.substring(2))) // strip "0x" prefix
        }
        def encodeValue(x: G1Element, out: JsonWriter): Unit = ???
        def nullValue: G1Element = null.asInstanceOf[G1Element]
    }

    private given JsonValueCodec[G2Element] = new JsonValueCodec[G2Element] {
        def decodeValue(in: JsonReader, default: G2Element): G2Element = {
            val hex = in.readString("")
            G2Element(ByteString.fromHex(hex.substring(2))) // strip "0x" prefix
        }
        def encodeValue(x: G2Element, out: JsonWriter): Unit = ???
        def nullValue: G2Element = null.asInstanceOf[G2Element]
    }

    private given JsonValueCodec[EthereumCeremony] = JsonCodecMaker.make(
      CodecMakerConfig
          .withFieldNameMapper(JsonCodecMaker.enforce_snake_case2)
          .withSkipUnexpectedFields(false)
    )
}
