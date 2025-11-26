package scalus.testing.conformance

import org.scalatest.funsuite.AnyFunSuite
import scalus.cardano.ledger.*
import scalus.testing.conformance.CardanoLedgerVectors.*
import scalus.utils.Hex

class TestNetworkConsistency extends AnyFunSuite {
    test("check network consistency in Invalid plutus script fails in phase 2") {
        assume(vectorsExist, "Conformance test vectors directory not found")
        val vectorName = "Conway.Imp.AlonzoImpSpec.UTXOS.PlutusV1.Invalid plutus script fails in phase 2"

        for case (path, vector) <- loadAllVectors(vectorName) do {
            println(s"\n=== Analyzing: $path ===")

            val state = LedgerState.fromCbor(Hex.hexToBytes(vector.oldLedgerState)).ruleState

            // Collect all networks from UTxO outputs
            val networks = state.utxos.flatMap { case (txIn, txOut) =>
                txOut.address.getNetwork.map { network =>
                    (txIn, txOut, network)
                }
            }

            // Group by network
            val byNetwork = networks.groupBy(_._3)

            println(s"Total UTxO entries: ${state.utxos.size}")
            println(s"Networks found:")
            byNetwork.foreach { case (network, entries) =>
                println(s"  $network: ${entries.size} outputs")
                // Show first few examples
                entries.take(2).foreach { case (txIn, txOut, _) =>
                    println(s"    TxIn: ${txIn.transactionId.toHex.take(16)}...#${txIn.index}")
                    println(s"    Address type: ${txOut.address.getClass.getSimpleName}")
                    txOut.address match {
                        case addr: scalus.cardano.address.ShelleyAddress =>
                            println(s"    Address network field: ${addr.network}")
                            val headerByte = addr.toBytes.bytes(0) & 0xff
                            val networkBits = headerByte & 0x0f
                            println(s"    Address header: 0x${"%02x".format(headerByte)}, network bits: $networkBits")
                        case _ =>
                            println(s"    Not a ShelleyAddress")
                    }
                }
            }

            // Check if there are mixed networks
            if (byNetwork.size > 1) {
                println(s"\n⚠️  WARNING: Mixed networks detected!")
                println(s"   This test case has ${byNetwork.keys.mkString(" and ")} addresses")
            } else {
                println(s"\n✓ All addresses use the same network: ${byNetwork.keys.headOption.getOrElse("none")}")
            }
        }
    }
}
