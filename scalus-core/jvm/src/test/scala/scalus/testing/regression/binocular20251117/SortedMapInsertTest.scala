package scalus.testing.regression.binocular20251117

import org.scalatest.funsuite.AnyFunSuite
import scalus.*
import scalus.builtin.ByteString
import scalus.compiler.{compile, Options}
import scalus.compiler.sir.TargetLoweringBackend
import scalus.prelude.{List, SortedMap}

/** Regression test for SortedMap.insert behavior with ByteString keys
  *
  * Issue: When using ByteString keys (32-byte block hashes), inserting a new entry and then
  * updating an existing entry produces different results on-chain vs off-chain.
  *
  * From binocular Bitcoin oracle test:
  *   - OFF-CHAIN (JVM): 20 entries after batch 2
  *   - ON-CHAIN (PlutusVM): 21 entries after batch 2
  *
  * This test uses the exact 20 ByteString keys from binocular that trigger the bug.
  */
class SortedMapInsertTest extends AnyFunSuite {

    given Options = Options(
      targetLoweringBackend = TargetLoweringBackend.SirToUplcV3Lowering,
      generateErrorTraces = true,
      optimizeUplc = false,
      debug = false
    )

    // The exact 20 keys from binocular batch 2 that produce 20 entries off-chain but 21 on-chain
    val batch2Keys = Seq(
      "0292cad364cf66a04f9ade0238e96a8978e8bc01c65301000000000000000000",
      "2bb5cb6c73eec42434973626acdb6003e56e5b5f784802000000000000000000",
      "7869fd052441acdd14f9209e56c30c2fce5aaa355cb400000000000000000000",
      "790156a97710a1c128d0edbe47a1e198984f119a4dfa00000000000000000000",
      "7c5112172e79033758516f1bfdd8045f84e3904f316d01000000000000000000",
      "7e8dd331fdd1ec5fd7f2a9fdbdf2028c22ebb564a6df00000000000000000000",
      "80956eecfe8b37d65beabfcccea06fe4c78b200a990f02000000000000000000",
      "850eb9025de6b082ca57fc8c342b956aaa479ec2cfd501000000000000000000",
      "8a1630a80f344c3e160825074399067c270c9529e69802000000000000000000",
      "90f7896a652985c6d3ca80412fabcd3182722d26447101000000000000000000",
      "96ad9422adc8678c248dded4b21477aa7647c274041e02000000000000000000",
      "b00c3f7b645c66bb3d35e7e084e6f0ac5184ab6d6eee01000000000000000000",
      "b2006eaa7a639b2ca40943bb2e35449f8a313cc7fe3c00000000000000000000",
      "b3c3b979fb69af53f2972e4e3b3991e0565bde78ad9d01000000000000000000",
      "c17c225605f57e2dea73c48f689abdff164ac868fdda02000000000000000000",
      "c3eb4fb02f30fabd602b70d69d1c804844dfd77f603c01000000000000000000",
      "c86164ff51d5276501e0515a78bc1c7ddc2984b627fd00000000000000000000",
      "c8ba5781f64feec627112ae23afc621346c67100acfa01000000000000000000",
      "e286b34277477191f06fb27ed4fc43bc2e3178d5673901000000000000000000",
      "feededad598bbcbdcd8b4e84517a6ab4ce009875a8a701000000000000000000"
    ).map(hex => ByteString.fromHex(hex))

    test("SortedMap.insert should update existing entry - simple test") {
        // Simple test: insert, then update
        var map = SortedMap.empty[BigInt, BigInt]
        map = map.insert(BigInt(1), BigInt(100))
        map = map.insert(BigInt(2), BigInt(200))

        // Update entry 1
        map = map.insert(BigInt(1), BigInt(111))

        val size = map.toList.size
        assert(size == 2, s"Expected 2 entries after update, got $size")

        // Verify the value was actually updated
        val value = map.find((k, _) => k == BigInt(1)) match {
            case scalus.prelude.Option.Some((_, v)) => v
            case _                                  => BigInt(-1)
        }
        assert(value == BigInt(111), s"Expected value 111, got $value")
    }

    test("SortedMap with exact binocular ByteString keys - CUMULATIVE batch 1 then batch 2") {
        import scalus.uplc.eval.PlutusVM
        given PlutusVM = PlutusVM.makePlutusV3VM()

        val sir = compile {
            // Batch 1 keys (10 keys)
            val b1k0 = ByteString.fromHex(
              "0292cad364cf66a04f9ade0238e96a8978e8bc01c65301000000000000000000"
            )
            val b1k1 = ByteString.fromHex(
              "2bb5cb6c73eec42434973626acdb6003e56e5b5f784802000000000000000000"
            )
            val b1k2 = ByteString.fromHex(
              "90f7896a652985c6d3ca80412fabcd3182722d26447101000000000000000000"
            )
            val b1k3 = ByteString.fromHex(
              "96ad9422adc8678c248dded4b21477aa7647c274041e02000000000000000000"
            )
            val b1k4 = ByteString.fromHex(
              "b00c3f7b645c66bb3d35e7e084e6f0ac5184ab6d6eee01000000000000000000"
            )
            val b1k5 = ByteString.fromHex(
              "b3c3b979fb69af53f2972e4e3b3991e0565bde78ad9d01000000000000000000"
            )
            val b1k6 = ByteString.fromHex(
              "c17c225605f57e2dea73c48f689abdff164ac868fdda02000000000000000000"
            )
            val b1k7 = ByteString.fromHex(
              "c86164ff51d5276501e0515a78bc1c7ddc2984b627fd00000000000000000000"
            )
            val b1k8 = ByteString.fromHex(
              "e286b34277477191f06fb27ed4fc43bc2e3178d5673901000000000000000000"
            )
            val b1k9 = ByteString.fromHex(
              "feededad598bbcbdcd8b4e84517a6ab4ce009875a8a701000000000000000000"
            )

            // Insert batch 1 (10 keys)
            val m0 = SortedMap.empty[ByteString, BigInt]
            val m1 = m0.insert(b1k0, BigInt(100))
            val m2 = m1.insert(b1k1, BigInt(101))
            val m3 = m2.insert(b1k2, BigInt(102))
            val m4 = m3.insert(b1k3, BigInt(103))
            val m5 = m4.insert(b1k4, BigInt(104))
            val m6 = m5.insert(b1k5, BigInt(105))
            val m7 = m6.insert(b1k6, BigInt(106))
            val m8 = m7.insert(b1k7, BigInt(107))
            val m9 = m8.insert(b1k8, BigInt(108))
            val batch1Map = m9.insert(b1k9, BigInt(109))

            // Batch 2 NEW keys (10 new keys that weren't in batch 1)
            val b2k0 = ByteString.fromHex(
              "7869fd052441acdd14f9209e56c30c2fce5aaa355cb400000000000000000000"
            )
            val b2k1 = ByteString.fromHex(
              "790156a97710a1c128d0edbe47a1e198984f119a4dfa00000000000000000000"
            )
            val b2k2 = ByteString.fromHex(
              "7c5112172e79033758516f1bfdd8045f84e3904f316d01000000000000000000"
            )
            val b2k3 = ByteString.fromHex(
              "7e8dd331fdd1ec5fd7f2a9fdbdf2028c22ebb564a6df00000000000000000000"
            )
            val b2k4 = ByteString.fromHex(
              "80956eecfe8b37d65beabfcccea06fe4c78b200a990f02000000000000000000"
            )
            val b2k5 = ByteString.fromHex(
              "850eb9025de6b082ca57fc8c342b956aaa479ec2cfd501000000000000000000"
            )
            val b2k6 = ByteString.fromHex(
              "8a1630a80f344c3e160825074399067c270c9529e69802000000000000000000"
            )
            val b2k7 = ByteString.fromHex(
              "b2006eaa7a639b2ca40943bb2e35449f8a313cc7fe3c00000000000000000000"
            )
            val b2k8 = ByteString.fromHex(
              "c3eb4fb02f30fabd602b70d69d1c804844dfd77f603c01000000000000000000"
            )
            val b2k9 = ByteString.fromHex(
              "c8ba5781f64feec627112ae23afc621346c67100acfa01000000000000000000"
            )

            // Add batch 2 keys to the existing map (some may update existing entries)
            val n1 = batch1Map.insert(b2k0, BigInt(200))
            val n2 = n1.insert(b2k1, BigInt(201))
            val n3 = n2.insert(b2k2, BigInt(202))
            val n4 = n3.insert(b2k3, BigInt(203))
            val n5 = n4.insert(b2k4, BigInt(204))
            val n6 = n5.insert(b2k5, BigInt(205))
            val n7 = n6.insert(b2k6, BigInt(206))
            val n8 = n7.insert(b2k7, BigInt(207))
            val n9 = n8.insert(b2k8, BigInt(208))
            val batch2Map = n9.insert(b2k9, BigInt(209))

            // Also update some batch 1 keys (simulating parent updates when children are added)
            val u1 = batch2Map.insert(b1k0, BigInt(300)) // Update
            val u2 = u1.insert(b1k3, BigInt(301)) // Update
            val finalMap = u2.insert(b1k7, BigInt(302)) // Update

            // Should be 20 entries (10 from batch1 + 10 new from batch2)
            finalMap.toList.size
        }

        val uplc = sir.toUplcOptimized()

        uplc.evaluateDebug match {
            case scalus.uplc.eval.Result.Success(result, budget, costs, logs) =>
                // Extract integer result
                val resultInt = result match {
                    case scalus.uplc.Term.Const(scalus.uplc.Constant.Integer(v)) => v
                    case _ =>
                        throw new RuntimeException(s"Expected Integer Const term, got: $result")
                }

                println(s"Off-chain expected: 20, PlutusVM got: $resultInt, budget: $budget")
                if logs.nonEmpty then {
                    println(s"Logs:\n${logs.mkString("\n")}")
                }

                // This is the bug: binocular shows 20 off-chain but 21 on-chain
                // The bug happens when inserting into existing map with updates
                if resultInt == 21 then {
                    fail(
                      s"BUG REPRODUCED! PlutusVM produced 21 entries instead of 20 (duplicate instead of update)"
                    )
                }
                assert(resultInt == 20, s"PlutusVM: expected 20 entries, got $resultInt")

            case scalus.uplc.eval.Result.Failure(exception, budget, costs, logs) =>
                fail(s"Evaluation failed: $exception")
        }
    }

}
