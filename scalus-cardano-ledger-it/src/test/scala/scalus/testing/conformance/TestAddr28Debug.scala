package scalus.testing.conformance

import org.scalatest.funsuite.AnyFunSuite
import scalus.cardano.address.Network

class TestAddr28Debug extends AnyFunSuite {
    test("debug tag 2 network parsing") {
        println("=== Tag 2 Example ===")
        // Tag 2 example from user
        val hexInput = "020100adf84fd242cd66edf5d9c60026a50a521408eaf8a7fcfe833180779fb4faeb54dda0db6c564d59d7d9c4c909d5b698a681cbf0010000006a92d36a00cff7e8afa897db1e"
        val bytes = hexToBytes(hexInput)

        println(s"Total bytes: ${bytes.length}")
        println(s"Tag byte: 0x${"%02x".format(bytes(0))}")

        // Parse the transaction output
        val txOut = MempackParser.parseTransactionOutput(bytes)

        println(s"Address: ${txOut.address}")
        println(s"Network: ${txOut.address.getNetwork}")
        println(s"Value: ${txOut.value}")

        // Also print if it's a ShelleyAddress with the network field
        txOut.address match {
            case addr: scalus.cardano.address.ShelleyAddress =>
                println(s"ShelleyAddress Network: ${addr.network}")
            case _ =>
                println(s"Not a ShelleyAddress")
        }

        // Manually decode the Addr28Extra to see the bit values
        var offset = 0
        offset += 1 // skip tag

        // Skip staking credential (29 bytes)
        println(s"\nStaking cred discriminator: 0x${"%02x".format(bytes(offset))}")
        offset += 29

        // Get Addr28Extra bytes
        val addr28Bytes = bytes.slice(offset, offset + 32)
        println(s"\nAddr28Extra (32 bytes): ${bytesToHex(addr28Bytes)}")

        // Parse as 4 Word64s
        val buffer = java.nio.ByteBuffer.wrap(addr28Bytes).order(java.nio.ByteOrder.BIG_ENDIAN)
        val w0 = buffer.getLong()
        val w1 = buffer.getLong()
        val w2 = buffer.getLong()
        val w3 = buffer.getLong()

        println(s"\nw0: 0x${"%016x".format(w0)} = $w0")
        println(s"w1: 0x${"%016x".format(w1)} = $w1")
        println(s"w2: 0x${"%016x".format(w2)} = $w2")
        println(s"w3: 0x${"%016x".format(w3)} = $w3")

        println(s"\nw3 lowest byte: 0x${"%02x".format(w3 & 0xff)} = ${w3 & 0xff}")
        println(s"w3 bit 0 (isKeyHash): ${(w3 & 0x1) != 0}")
        println(s"w3 bit 1 (isMainnet): ${(w3 & 0x2) != 0}")

        // Check last byte directly
        val lastByte = addr28Bytes(31) & 0xff
        println(s"\nLast byte of Addr28Extra: 0x${"%02x".format(lastByte)} = $lastByte")
        println(s"Last byte bit 0: ${(lastByte & 0x1) != 0}")
        println(s"Last byte bit 1: ${(lastByte & 0x2) != 0}")
    }

    test("debug new example network parsing") {
        println("\n=== New Example ===")
        val hexInput = "011d706ba8f502e9f994ed5518d3007f705452969a10b01901259a1141405600bce21ae88bd757ad5b9bedf372d8d3f0cf6c962a469db61a265f6418e1ffed86da29ec"
        val bytes = hexToBytes(hexInput)

        println(s"Total bytes: ${bytes.length}")
        println(s"Tag byte: 0x${"%02x".format(bytes(0))}")

        // Parse the transaction output
        val txOut = MempackParser.parseTransactionOutput(bytes)

        println(s"Address: ${txOut.address}")
        println(s"Network: ${txOut.address.getNetwork}")
        println(s"Value: ${txOut.value}")

        // Also print if it's a ShelleyAddress with the network field
        txOut.address match {
            case addr: scalus.cardano.address.ShelleyAddress =>
                println(s"ShelleyAddress Network: ${addr.network}")

                // Decode the address bytes to check the network in the header
                val addrBytes = addr.toBytes.bytes
                val headerByte = addrBytes(0) & 0xff
                val networkBits = headerByte & 0x0f
                println(s"Address header byte: 0x${"%02x".format(headerByte)}")
                println(s"Network bits from header: $networkBits (0=Testnet, 1=Mainnet)")
            case _ =>
                println(s"Not a ShelleyAddress")
        }
    }

    private def hexToBytes(hex: String): Array[Byte] = {
        hex.sliding(2, 2).toArray.map(Integer.parseInt(_, 16).toByte)
    }

    private def bytesToHex(bytes: Array[Byte]): String = {
        bytes.map(b => "%02x".format(b & 0xff)).mkString
    }
}
