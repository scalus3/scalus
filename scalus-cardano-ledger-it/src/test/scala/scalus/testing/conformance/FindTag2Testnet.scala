package scalus.testing.conformance

import org.scalatest.funsuite.AnyFunSuite
import scalus.cardano.ledger.*
import scalus.cardano.address.Network
import scalus.testing.conformance.CardanoLedgerVectors.*
import scalus.utils.Hex

import java.nio.file.{Files, Path}
import scala.jdk.CollectionConverters.*

class FindTag2Testnet extends AnyFunSuite {
    test("find vectors with tag 2 testnet addresses") {
        assume(vectorsExist, "Conformance test vectors directory not found")

        // Search through all test vectors
        val allVectorDirs = Files.walk(conformanceVectorsPath, 2)
            .iterator()
            .asScala
            .filter(p => Files.isDirectory(p) && p != conformanceVectorsPath)
            .toList

        println(s"Searching ${allVectorDirs.size} vector directories...\n")

        var foundCount = 0

        allVectorDirs.foreach { vectorDir =>
            try {
                val vectorName = vectorDir.getFileName.toString

                for case (path, vector) <- loadAllVectors(vectorName) do {
                    val state = LedgerState.fromCbor(Hex.hexToBytes(vector.oldLedgerState)).ruleState

                    // Check each UTxO for tag 2 testnet addresses
                    state.utxos.foreach { case (txIn, txOut) =>
                        // Check if this might be a tag 2 address (Addr28)
                        txOut.address.getNetwork match {
                            case Some(Network.Testnet) =>
                                // Try to identify if this came from tag 2 by checking the address structure
                                txOut.address match {
                                    case addr: scalus.cardano.address.ShelleyAddress =>
                                        // Tag 2 addresses are compact format with AddrHash28
                                        // They would have been constructed via the Addr28Extra path
                                        val addrBytes = addr.toBytes.bytes
                                        if addrBytes.length == 29 then { // Compact address length
                                            foundCount += 1
                                            if foundCount <= 5 then {
                                                println(s"Found potential tag 2 Testnet address:")
                                                println(s"  Vector: $vectorName")
                                                println(s"  Path: $path")
                                                println(s"  TxIn: ${txIn.transactionId.toHex.take(16)}...#${txIn.index}")
                                                println(s"  Address: ${addr.toHex.take(80)}...")
                                                println(s"  Network: ${addr.network}")
                                                println()
                                            }
                                        }
                                    case _ =>
                                }
                            case _ =>
                        }
                    }
                }
            } catch {
                case e: Exception =>
                    // Skip vectors that fail to parse
            }
        }

        println(s"\nTotal potential tag 2 Testnet addresses found: $foundCount")
        println("\nNow let me search for tag 2 specifically by examining the mempack encoded UTxO...")
    }

    test("find tag 2 by examining raw mempack bytes") {
        assume(vectorsExist, "Conformance test vectors directory not found")

        // Load the raw JSON files to examine the mempack encoded data
        val allVectorDirs = Files.walk(conformanceVectorsPath, 2)
            .iterator()
            .asScala
            .filter(p => Files.isDirectory(p) && p != conformanceVectorsPath)
            .toList
            .take(50) // Limit search for performance

        println(s"Searching ${allVectorDirs.size} vector directories for tag byte 0x02...\n")

        var foundCount = 0

        allVectorDirs.foreach { vectorDir =>
            try {
                val vectorName = vectorDir.getFileName.toString

                for case (path, vector) <- loadAllVectors(vectorName) do {
                    val stateBytes = Hex.hexToBytes(vector.oldLedgerState)

                    // Search for tag byte 0x02 in the hex string
                    // Tag 2 would appear as the first byte of a TxOut in the mempack format
                    val hexStr = vector.oldLedgerState

                    // Look for patterns that might indicate tag 2
                    // Tag 2 is followed by: staking credential (29 bytes) + Addr28Extra (32 bytes) + coin
                    var idx = 0
                    while idx < hexStr.length - 2 do {
                        val twoChars = hexStr.substring(idx, idx + 2)
                        if twoChars == "02" then {
                            // Found a 0x02 byte - could be tag 2
                            // Check if there's enough data after it for tag 2 structure
                            val remainingHex = hexStr.length - idx
                            if remainingHex >= 130 then { // 1 tag + 29 cred + 32 addr28 = 62 bytes = 124 hex chars minimum
                                foundCount += 1
                                if foundCount <= 10 then {
                                    println(s"Found byte 0x02 in:")
                                    println(s"  Vector: $vectorName")
                                    println(s"  Position in hex: $idx")
                                    println(s"  Context: ...${hexStr.substring(Math.max(0, idx-10), Math.min(hexStr.length, idx+140))}...")

                                    // Try to parse this as a transaction output
                                    try {
                                        val startByte = idx / 2
                                        val txOutBytes = stateBytes.slice(startByte, Math.min(stateBytes.length, startByte + 100))
                                        if txOutBytes(0) == 0x02 then {
                                            val txOut = MempackParser.parseTransactionOutput(txOutBytes)
                                            txOut.address.getNetwork match {
                                                case Some(Network.Testnet) =>
                                                    println(s"  ✓ Parsed as tag 2 with TESTNET!")
                                                    println(s"  Address: ${txOut.address}")
                                                case Some(Network.Mainnet) =>
                                                    println(s"  Parsed as tag 2 with Mainnet")
                                                case _ =>
                                                    println(s"  Parsed but no network")
                                            }
                                        }
                                    } catch {
                                        case e: Exception =>
                                            println(s"  (Could not parse as TxOut)")
                                    }
                                    println()
                                }
                            }
                        }
                        idx += 2
                    }
                }
            } catch {
                case e: Exception =>
                    // Skip vectors that fail
            }
        }

        println(s"\nTotal 0x02 bytes found: $foundCount")
    }
}
