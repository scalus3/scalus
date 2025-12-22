package scalus.builtin

import org.scalatest.funsuite.AnyFunSuite
import scalus.serialization.flat.Flat
import scalus.builtin.Data.*
import scalus.prelude.List as PList
import scalus.utils.Hex.*

class TestFlatDataTest extends AnyFunSuite {
    test("Which Flat[Data] is in scope") {
        // Get the Flat[Data] that would be used
        val flatData = summon[Flat[Data]]

        // Create a simple Data.List
        val data = Data.List(PList(I(BigInt(1)), I(BigInt(2))))

        // Encode to flat (which internally uses CBOR for the DataApi version)
        val encoded = new scalus.serialization.flat.EncoderState(100)
        flatData.encode(data, encoded)
        encoded.filler()
        val bytes = encoded.result

        // The CBOR portion starts after the flat prefix
        // For DataApi's Flat[Data], it encodes to CBOR first
        // For FlatInstances' Flat[Data], it encodes directly to flat

        println(s"Flat[Data] class: ${flatData.getClass.getName}")
        println(s"Encoded hex: ${bytes.toHex}")

        // Check if it looks like CBOR (will have array markers)
        // Definite-length array of 2: 82
        // Indefinite-length array: 9f ... ff
        val hexStr = bytes.toHex

        if hexStr.contains("82") then println("=> Uses CBOR with DEFINITE-length encoding")
        else if hexStr.contains("9f") then println("=> Uses CBOR with INDEFINITE-length encoding")
        else println("=> Uses direct Flat encoding (not CBOR)")
    }
}
