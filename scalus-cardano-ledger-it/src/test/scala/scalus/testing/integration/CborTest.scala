package scalus.testing.integration

import com.bloxbean.cardano.yaci.core.model.serializers.util.WitnessUtil
import io.bullet.borer.Cbor
import org.scalatest.funsuite.AnyFunSuite
import scalus.*
import scalus.builtin.{ByteString, Data}
import scalus.cardano.ledger.{BlockFile, OriginalCborByteArray}
import scalus.testing.integration.BlocksTestUtils.*

import java.math.BigInteger
import java.nio.file.Files
import scala.jdk.CollectionConverters.*

class CborTest extends AnyFunSuite {

    val badBlock = 11545396

    ignore(s"bloxbean WitnessUtil has problem") {
        val block = Files.readAllBytes(blockPath(badBlock))
        for
            witness <- WitnessUtil.getWitnessRawData(block).asScala
            field <- Option(WitnessUtil.getWitnessFields(witness).get(BigInteger.valueOf(4L))).toSeq
            datum <- WitnessUtil.getArrayBytes(field).asScala
        do
            println(ByteString.fromArray(datum))
            val cbor = Cbor.decode(datum).to[Data]
            val data = cbor.value // throws "Expected End-of-Input but got Int (input position 1)"
            assert(data != null)
    }

    test(s"Scalus OK") {
        val block = Files.readAllBytes(blockPath(badBlock))
        given OriginalCborByteArray = OriginalCborByteArray(block)
        val blockFile = BlockFile.fromCborArray(block)
        for
            tx <- blockFile.block.transactions
            data <- tx.witnessSet.plutusData.value.toMap.values
        do {
//            println(ByteString.fromArray(data.raw))
            assert(data.value != null)
        }
    }

    test(s"Scalus OK all") {
        for path <- getAllBlocksPaths()
        do {
            val block = Files.readAllBytes(path)
            given OriginalCborByteArray = OriginalCborByteArray(block)
            val blockFile = BlockFile.fromCborArray(block)
            for
                tx <- blockFile.block.transactions
                data <- tx.witnessSet.plutusData.value.toMap.values
            do assert(data.value != null)
        }
    }

}
