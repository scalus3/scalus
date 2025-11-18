package scalus.bloxbean

import com.bloxbean.cardano.client.plutus.spec.*
import org.scalatest.funsuite.AnyFunSuite
import scalus.builtin.{ByteString, Data}

class InteropMapOrderingTest extends AnyFunSuite:

    test("MapPlutusData deserialization preserves CBOR key ordering") {
        pending
        // Create a map with specific byte string keys in sorted order
        val key1 =
            ByteString.fromHex("0292cad364cf66a04f9ade0238e96a8978e8bc01c65301000000000000000000")
        val key2 =
            ByteString.fromHex("2bb5cb6c73eec42434973626acdb6003e56e5b5f784802000000000000000000")
        val key3 =
            ByteString.fromHex("7869d9f70ad5afaf66466dbec409fb1a92818cb2e3e901000000000000000000")
        val key4 =
            ByteString.fromHex("7901e6db76b30fd6e5cc1a14c9c5ab05fbbadb81745202000000000000000000")
        val key5 =
            ByteString.fromHex("b00c3f7b645c66bb3d35e7e084e6f0ac5184ab6d6eee01000000000000000000")

        val value1 = Data.Constr(0, List.empty)
        val value2 = Data.Constr(0, List.empty)
        val value3 = Data.Constr(0, List.empty)
        val value4 = Data.Constr(0, List.empty)
        val value5 = Data.Constr(0, List.empty)

        // Create map with entries in sorted order
        val originalEntries = List(
          (Data.B(key1), value1),
          (Data.B(key2), value2),
          (Data.B(key3), value3),
          (Data.B(key4), value4),
          (Data.B(key5), value5)
        )

        // Get the original key order
        val originalKeys = originalEntries.map(_._1)

        // Convert to PlutusData
        val mapData = Data.Map(originalEntries)
        val plutusData = Interop.toPlutusData(mapData)

        // Serialize to CBOR (this should preserve deterministic ordering)
        val cbor = plutusData.serializeToBytes()

        // Deserialize back from CBOR
        val deserializedPlutusData = PlutusData.deserialize(cbor)

        // Convert back to Scalus Data
        val deserializedData = Interop.toScalusData(deserializedPlutusData)

        // Extract the keys from the deserialized map
        val deserializedKeys = deserializedData match {
            case Data.Map(entries) => entries.map(_._1)
            case _                 => fail("Expected Data.Map")
        }

        // Verify that the key order is preserved
        assert(
          originalKeys == deserializedKeys,
          s"Map key order was not preserved!\nOriginal:     ${originalKeys.map(keyToHex).mkString(", ")}\nDeserialized: ${deserializedKeys.map(keyToHex).mkString(", ")}"
        )
    }

    test("MapPlutusData with multiple entries maintains consistent ordering") {
        pending
        // Create a map with 10 random-ish keys to stress test ordering
        val keys = List(
          "0292cad364cf66a04f9ade0238e96a8978e8bc01c65301000000000000000000",
          "2bb5cb6c73eec42434973626acdb6003e56e5b5f784802000000000000000000",
          "4f5e8a9b1c2d3e4f5a6b7c8d9e0f1a2b3c4d5e6f7a8b9c0d1e2f3a4b5c6d7e8f",
          "7869d9f70ad5afaf66466dbec409fb1a92818cb2e3e901000000000000000000",
          "7901e6db76b30fd6e5cc1a14c9c5ab05fbbadb81745202000000000000000000",
          "90f7896a652985c6d3ca80412fabcd3182722d26447101000000000000000000",
          "96ad9422adc8678c248dded4b21477aa7647c274041e02000000000000000000",
          "a1b2c3d4e5f6071829384a5b6c7d8e9f0a1b2c3d4e5f6071829384a5b6c7d8e9",
          "b00c3f7b645c66bb3d35e7e084e6f0ac5184ab6d6eee01000000000000000000",
          "c0d1e2f3a4b5c6d7e8f90a1b2c3d4e5f6a7b8c9d0e1f2a3b4c5d6e7f8a9b0c1d"
        ).map(ByteString.fromHex).sorted(using Ordering.by((bs: ByteString) => bs))

        val entries = keys.zipWithIndex.map { case (key, i) =>
            (Data.B(key), Data.I(BigInt(i)))
        }.toList

        val originalMap = Data.Map(entries)

        // Serialize and deserialize
        val plutusData = Interop.toPlutusData(originalMap)
        val cbor = plutusData.serializeToBytes()
        val deserializedPlutusData = PlutusData.deserialize(cbor)
        val deserializedData = Interop.toScalusData(deserializedPlutusData)

        // Extract keys
        val (originalKeys, deserializedKeys) = (originalMap, deserializedData) match {
            case (Data.Map(origEntries), Data.Map(deserEntries)) =>
                (origEntries.map(_._1), deserEntries.map(_._1))
            case _ => fail("Expected Data.Map")
        }

        // Verify ordering
        assert(
          originalKeys == deserializedKeys,
          s"Map key order was not preserved for ${keys.length} entries!\n" +
              s"Original:     ${originalKeys.map(keyToHex).take(5).mkString(", ")} ...\n" +
              s"Deserialized: ${deserializedKeys.map(keyToHex).take(5).mkString(", ")} ..."
        )
    }

    private def keyToHex(data: Data): String = data match {
        case Data.B(bytes) => bytes.toHex.take(8) + "..."
        case _             => "???"
    }
