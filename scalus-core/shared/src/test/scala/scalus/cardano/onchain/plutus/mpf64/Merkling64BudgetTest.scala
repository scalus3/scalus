package scalus.cardano.onchain.plutus.mpf64

import org.scalatest.funsuite.AnyFunSuite
import scalus.cardano.ledger.ExUnits
import scalus.compiler.Options
import scalus.compiler.sir.TargetLoweringBackend
import scalus.uplc.builtin.Builtins.*
import scalus.uplc.builtin.ByteString
import scalus.uplc.builtin.Data
import scalus.uplc.builtin.Data.{B, I}
import scalus.uplc.eval.{PlutusVM, Result}
import scalus.uplc.{PlutusV3, Term}
import scalus.uplc.Term.asTerm

/** Microbenchmark comparing extraction and merkle costs across radix-16, radix-64, and radix-256.
  *
  * Measures individual operations in isolation to determine whether MPF-64 with table-based
  * extraction and unrolled merkle64 can beat MPF-16's nibble extraction + unrolled merkle16.
  */
class Merkling64BudgetTest extends AnyFunSuite {

    private given Options = Options(
      targetLoweringBackend = TargetLoweringBackend.SirToUplcV3Lowering,
      generateErrorTraces = false,
      optimizeUplc = true,
      debug = false
    )

    private given PlutusVM = PlutusVM.makePlutusV3VM()

    // Test path: blake2b_256("apple")
    private val testPath: ByteString = blake2b_256(ByteString.fromString("apple"))

    // Some random hashes for merkle tree neighbors
    private val h1: ByteString = blake2b_256(ByteString.fromString("h1"))
    private val h2: ByteString = blake2b_256(ByteString.fromString("h2"))
    private val h3: ByteString = blake2b_256(ByteString.fromString("h3"))
    private val h4: ByteString = blake2b_256(ByteString.fromString("h4"))
    private val h5: ByteString = blake2b_256(ByteString.fromString("h5"))
    private val h6: ByteString = blake2b_256(ByteString.fromString("h6"))
    private val h7: ByteString = blake2b_256(ByteString.fromString("h7"))
    private val h8: ByteString = blake2b_256(ByteString.fromString("h8"))

    private def measure(program: PlutusV3[?], args: Term*): ExUnits = {
        var applied = program.program.term
        for arg <- args do applied = applied $ arg
        applied.evaluateDebug match
            case Result.Success(_, exunits, _, _) => exunits
            case Result.Failure(ex, _, _, _) =>
                fail(s"Evaluation failed: ${ex.getMessage}")
    }

    // ========== EXTRACTION BENCHMARKS ==========

    // MPF-16: nibble extraction
    private val nibbleProgram = PlutusV3.compile { (pathD: Data, indexD: Data) =>
        import scalus.cardano.onchain.plutus.mpf.Merkling.nibble
        nibble(unBData(pathD), unIData(indexD))
    }

    // MPF-64: sixit extraction (table-based, branching)
    private val sixitProgram = PlutusV3.compile { (pathD: Data, indexD: Data) =>
        import scalus.cardano.onchain.plutus.mpf64.Merkling.sixit
        sixit(unBData(pathD), unIData(indexD))
    }

    // MPF-64: sixit extraction (arithmetic, same as sixitProgram — kept for test compatibility)
    private val sixitShiftProgram = sixitProgram

    // MPF-256: byteAt extraction
    private val byteAtProgram = PlutusV3.compile { (pathD: Data, indexD: Data) =>
        import scalus.cardano.onchain.plutus.mpf256.Merkling.byteAt
        byteAt(unBData(pathD), unIData(indexD))
    }

    test("extraction cost: nibble vs sixit vs sixitShift vs byteAt") {
        info("Comparing path unit extraction cost across radixes")
        info("  nibble:     quotientInteger/modInteger on byte content")
        info("  sixit:      table-based lookup with if-else chain")
        info("  sixitShift: branchless shift+mask (Plutus V3 bitwise ops)")
        info("  byteAt:     trivial indexByteString")
        info("")

        val header =
            f"${"Position"}%-12s | ${"nibble cpu"}%12s | ${"sixit cpu"}%12s | ${"shift cpu"}%12s | ${"byteAt cpu"}%12s | ${"shift/nibble"}%13s"
        info(header)
        info("-" * header.length)

        // Test multiple positions to cover different alignment cases
        // nibble: even/odd positions, sixit: positions 0-3 in group
        val positions = Seq(
          ("pos 0 (aligned)", 0, 0, 0),
          ("pos 1", 1, 1, -1),
          ("pos 2", 2, 2, 1),
          ("pos 3", 3, 3, -1),
          ("pos 10", 10, 10, 5),
          ("pos 20", 20, 20, 10),
          ("pos 30", 30, 30, 15),
          ("pos 40", 40, 40, 20),
          ("pos 60", 60, 41, 30)
        )

        var totalNibble = ExUnits(0, 0)
        var totalSixit = ExUnits(0, 0)
        var totalShift = ExUnits(0, 0)
        var totalByteAt = ExUnits(0, 0)
        var count = 0

        for (label, nibbleIdx, sixitIdx, byteIdx) <- positions do
            val nb = measure(nibbleProgram, B(testPath).asTerm, I(nibbleIdx).asTerm)
            val sx = measure(sixitProgram, B(testPath).asTerm, I(sixitIdx).asTerm)
            val sh = measure(sixitShiftProgram, B(testPath).asTerm, I(sixitIdx).asTerm)
            val ba =
                if byteIdx >= 0 then measure(byteAtProgram, B(testPath).asTerm, I(byteIdx).asTerm)
                else ExUnits(0, 0)

            totalNibble = ExUnits(totalNibble.memory + nb.memory, totalNibble.steps + nb.steps)
            totalSixit = ExUnits(totalSixit.memory + sx.memory, totalSixit.steps + sx.steps)
            totalShift = ExUnits(totalShift.memory + sh.memory, totalShift.steps + sh.steps)
            if byteIdx >= 0 then
                totalByteAt = ExUnits(totalByteAt.memory + ba.memory, totalByteAt.steps + ba.steps)
            count += 1

            val shRatio =
                if nb.steps > 0 then f"${sh.steps.toDouble / nb.steps}%13.3f" else "         N/A"
            val baStr = if byteIdx >= 0 then f"${ba.steps}%12d" else "         N/A"
            info(
              f"${label}%-12s | ${nb.steps}%12d | ${sx.steps}%12d | ${sh.steps}%12d | $baStr | $shRatio"
            )

        info("")
        info(
          f"Average nibble:     cpu=${totalNibble.steps / count}%d"
        )
        info(
          f"Average sixit:      cpu=${totalSixit.steps / count}%d  (${totalSixit.steps.toDouble / totalNibble.steps}%.3fx nibble)"
        )
        info(
          f"Average sixitShift: cpu=${totalShift.steps / count}%d  (${totalShift.steps.toDouble / totalNibble.steps}%.3fx nibble)"
        )
    }

    // ========== MERKLE BENCHMARKS ==========

    // MPF-16: unrolled merkle16 (4 neighbors)
    private val merkle16Program = PlutusV3.compile {
        (branchD: Data, rootD: Data, n8D: Data, n4D: Data, n2D: Data, n1D: Data) =>
            import scalus.cardano.onchain.plutus.mpf.Merkling.merkle16
            merkle16(
              unIData(branchD),
              unBData(rootD),
              unBData(n8D),
              unBData(n4D),
              unBData(n2D),
              unBData(n1D)
            )
    }

    // MPF-64: unrolled merkle64 (6 neighbors)
    private val merkle64Program = PlutusV3.compile {
        (branchD: Data, rootD: Data, neighborsD: Data) =>
            import scalus.cardano.onchain.plutus.mpf64.Merkling.merkle64
            val neighbors = unBData(neighborsD)
            merkle64(
              unIData(branchD),
              unBData(rootD),
              sliceByteString(0, 32, neighbors),
              sliceByteString(32, 32, neighbors),
              sliceByteString(64, 32, neighbors),
              sliceByteString(96, 32, neighbors),
              sliceByteString(128, 32, neighbors),
              sliceByteString(160, 32, neighbors)
            )
    }

    // MPF-256: generic recursive merkle (8 neighbors)
    private val merkle256Program = PlutusV3.compile {
        (branchD: Data, rootD: Data, neighborsD: Data) =>
            import scalus.cardano.onchain.plutus.mpf256.Merkling.merkle
            merkle(128, unIData(branchD), unBData(rootD), unBData(neighborsD))
    }

    test("merkle cost: merkle16 vs merkle64 vs merkle256") {
        info("Comparing merkle tree reconstruction cost across radixes")
        info("  merkle16:  unrolled 4 levels (MPF-16)")
        info("  merkle64:  unrolled 6 levels (MPF-64)")
        info("  merkle256: generic recursive 8 levels (MPF-256)")
        info("")

        // Neighbors packed as ByteStrings
        val neighbors6 = h1.concat(h2).concat(h3).concat(h4).concat(h5).concat(h6) // 192 bytes
        val neighbors8 =
            h1.concat(h2)
                .concat(h3)
                .concat(h4)
                .concat(h5)
                .concat(h6)
                .concat(h7)
                .concat(h8) // 256 bytes

        val branches16 = Seq(0, 5, 10, 15)
        val branches64 = Seq(0, 17, 42, 63)
        val branches256 = Seq(0, 85, 170, 255)

        val header =
            f"${"Branch"}%-10s | ${"m16 mem"}%8s | ${"m16 cpu"}%12s | ${"m64 mem"}%8s | ${"m64 cpu"}%12s | ${"m256 mem"}%9s | ${"m256 cpu"}%12s | ${"m64/m16"}%8s | ${"m256/m16"}%9s"
        info(header)
        info("-" * header.length)

        var total16 = ExUnits(0, 0)
        var total64 = ExUnits(0, 0)
        var total256 = ExUnits(0, 0)

        for i <- branches16.indices do
            val b16 = branches16(i)
            val b64 = branches64(i)
            val b256 = branches256(i)

            val m16 = measure(
              merkle16Program,
              I(b16).asTerm,
              B(h1).asTerm,
              B(h2).asTerm,
              B(h3).asTerm,
              B(h4).asTerm,
              B(h5).asTerm
            )
            val m64 = measure(
              merkle64Program,
              I(b64).asTerm,
              B(h1).asTerm,
              B(neighbors6).asTerm
            )
            val m256 = measure(
              merkle256Program,
              I(b256).asTerm,
              B(h1).asTerm,
              B(neighbors8).asTerm
            )

            total16 = ExUnits(total16.memory + m16.memory, total16.steps + m16.steps)
            total64 = ExUnits(total64.memory + m64.memory, total64.steps + m64.steps)
            total256 = ExUnits(total256.memory + m256.memory, total256.steps + m256.steps)

            info(
              f"${b16}/${b64}/${b256}%-10s | ${m16.memory}%8d | ${m16.steps}%12d | ${m64.memory}%8d | ${m64.steps}%12d | ${m256.memory}%9d | ${m256.steps}%12d | ${m64.steps.toDouble / m16.steps}%8.3f | ${m256.steps.toDouble / m16.steps}%9.3f"
            )

        info("")
        info(f"Average merkle16:  mem=${total16.memory / 4}%d cpu=${total16.steps / 4}%d")
        info(f"Average merkle64:  mem=${total64.memory / 4}%d cpu=${total64.steps / 4}%d")
        info(f"Average merkle256: mem=${total256.memory / 4}%d cpu=${total256.steps / 4}%d")
        info(f"merkle64/merkle16 cpu ratio:  ${total64.steps.toDouble / total16.steps}%.3f")
        info(f"merkle256/merkle16 cpu ratio: ${total256.steps.toDouble / total16.steps}%.3f")
    }

    // ========== SPARSE MERKLE BENCHMARKS ==========

    // MPF-16: sparseMerkle16
    private val sparse16Program = PlutusV3.compile {
        (meD: Data, meHD: Data, nbD: Data, nbHD: Data) =>
            import scalus.cardano.onchain.plutus.mpf.Merkling.sparseMerkle16
            sparseMerkle16(unIData(meD), unBData(meHD), unIData(nbD), unBData(nbHD))
    }

    // MPF-64: sparseMerkle64
    private val sparse64Program = PlutusV3.compile {
        (meD: Data, meHD: Data, nbD: Data, nbHD: Data) =>
            import scalus.cardano.onchain.plutus.mpf64.Merkling.sparseMerkle64
            sparseMerkle64(unIData(meD), unBData(meHD), unIData(nbD), unBData(nbHD))
    }

    // MPF-256: generic sparseMerkle
    private val sparse256Program = PlutusV3.compile {
        (meD: Data, meHD: Data, nbD: Data, nbHD: Data) =>
            import scalus.cardano.onchain.plutus.mpf256.Merkling.sparseMerkle
            sparseMerkle(128, 7, unIData(meD), unBData(meHD), unIData(nbD), unBData(nbHD))
    }

    test("sparseMerkle cost: sparse16 vs sparse64 vs sparse256") {
        info("Comparing sparse merkle (2-element placement) cost across radixes")
        info("  sparse16:  unrolled 4 levels (MPF-16)")
        info("  sparse64:  unrolled 6 levels (MPF-64)")
        info("  sparse256: generic recursive 8 levels (MPF-256)")
        info("")

        // Test cases: (me, neighbor) pairs at different distances
        val cases16 = Seq((0, 15), (3, 12), (7, 8), (1, 2))
        val cases64 = Seq((0, 63), (10, 53), (31, 32), (1, 2))
        val cases256 = Seq((0, 255), (42, 213), (127, 128), (1, 2))

        val header =
            f"${"Positions"}%-16s | ${"s16 mem"}%8s | ${"s16 cpu"}%12s | ${"s64 mem"}%8s | ${"s64 cpu"}%12s | ${"s256 mem"}%9s | ${"s256 cpu"}%12s | ${"s64/s16"}%8s | ${"s256/s16"}%9s"
        info(header)
        info("-" * header.length)

        var total16 = ExUnits(0, 0)
        var total64 = ExUnits(0, 0)
        var total256 = ExUnits(0, 0)

        for i <- cases16.indices do
            val (me16, nb16) = cases16(i)
            val (me64, nb64) = cases64(i)
            val (me256, nb256) = cases256(i)

            val s16 = measure(
              sparse16Program,
              I(me16).asTerm,
              B(h1).asTerm,
              I(nb16).asTerm,
              B(h2).asTerm
            )
            val s64 = measure(
              sparse64Program,
              I(me64).asTerm,
              B(h1).asTerm,
              I(nb64).asTerm,
              B(h2).asTerm
            )
            val s256 = measure(
              sparse256Program,
              I(me256).asTerm,
              B(h1).asTerm,
              I(nb256).asTerm,
              B(h2).asTerm
            )

            total16 = ExUnits(total16.memory + s16.memory, total16.steps + s16.steps)
            total64 = ExUnits(total64.memory + s64.memory, total64.steps + s64.steps)
            total256 = ExUnits(total256.memory + s256.memory, total256.steps + s256.steps)

            info(
              f"${me16},${nb16}/${me64},${nb64}/${me256},${nb256}%-16s | ${s16.memory}%8d | ${s16.steps}%12d | ${s64.memory}%8d | ${s64.steps}%12d | ${s256.memory}%9d | ${s256.steps}%12d | ${s64.steps.toDouble / s16.steps}%8.3f | ${s256.steps.toDouble / s16.steps}%9.3f"
            )

        info("")
        info(f"Average sparse16:  mem=${total16.memory / 4}%d cpu=${total16.steps / 4}%d")
        info(f"Average sparse64:  mem=${total64.memory / 4}%d cpu=${total64.steps / 4}%d")
        info(f"Average sparse256: mem=${total256.memory / 4}%d cpu=${total256.steps / 4}%d")
        info(f"sparse64/sparse16 cpu ratio:  ${total64.steps.toDouble / total16.steps}%.3f")
        info(f"sparse256/sparse16 cpu ratio: ${total256.steps.toDouble / total16.steps}%.3f")
    }

    // ========== SUFFIX BENCHMARKS ==========

    // MPF-16: suffix
    private val suffix16Program = PlutusV3.compile { (pathD: Data, cursorD: Data) =>
        import scalus.cardano.onchain.plutus.mpf.Merkling.suffix
        suffix(unBData(pathD), unIData(cursorD))
    }

    // MPF-64: suffix
    private val suffix64Program = PlutusV3.compile { (pathD: Data, cursorD: Data) =>
        import scalus.cardano.onchain.plutus.mpf64.Merkling.suffix
        suffix(unBData(pathD), unIData(cursorD))
    }

    // MPF-256: suffix
    private val suffix256Program = PlutusV3.compile { (pathD: Data, cursorD: Data) =>
        import scalus.cardano.onchain.plutus.mpf256.Merkling.suffix
        suffix(unBData(pathD), unIData(cursorD))
    }

    test("suffix cost: suffix16 vs suffix64 vs suffix256") {
        info("Comparing suffix encoding cost across radixes")
        info("")

        val cursors16 = Seq(0, 1, 10, 30, 60)
        val cursors64 = Seq(0, 1, 4, 12, 40)
        val cursors256 = Seq(0, 1, 5, 15, 30)

        val header =
            f"${"Cursor"}%-12s | ${"s16 mem"}%8s | ${"s16 cpu"}%12s | ${"s64 mem"}%8s | ${"s64 cpu"}%12s | ${"s256 mem"}%9s | ${"s256 cpu"}%12s"
        info(header)
        info("-" * header.length)

        var total16 = ExUnits(0, 0)
        var total64 = ExUnits(0, 0)
        var total256 = ExUnits(0, 0)

        for i <- cursors16.indices do
            val c16 = cursors16(i)
            val c64 = cursors64(i)
            val c256 = cursors256(i)

            val s16 = measure(suffix16Program, B(testPath).asTerm, I(c16).asTerm)
            val s64 = measure(suffix64Program, B(testPath).asTerm, I(c64).asTerm)
            val s256 = measure(suffix256Program, B(testPath).asTerm, I(c256).asTerm)

            total16 = ExUnits(total16.memory + s16.memory, total16.steps + s16.steps)
            total64 = ExUnits(total64.memory + s64.memory, total64.steps + s64.steps)
            total256 = ExUnits(total256.memory + s256.memory, total256.steps + s256.steps)

            info(
              f"${c16}/${c64}/${c256}%-12s | ${s16.memory}%8d | ${s16.steps}%12d | ${s64.memory}%8d | ${s64.steps}%12d | ${s256.memory}%9d | ${s256.steps}%12d"
            )

        info("")
        info(f"Average suffix16:  mem=${total16.memory / 5}%d cpu=${total16.steps / 5}%d")
        info(f"Average suffix64:  mem=${total64.memory / 5}%d cpu=${total64.steps / 5}%d")
        info(f"Average suffix256: mem=${total256.memory / 5}%d cpu=${total256.steps / 5}%d")
        info(f"suffix64/suffix16 cpu ratio:  ${total64.steps.toDouble / total16.steps}%.3f")
        info(f"suffix256/suffix16 cpu ratio: ${total256.steps.toDouble / total16.steps}%.3f")
    }

    // ========== COMBINED STEP COST ESTIMATE ==========

    test("estimated per-step cost: extraction + merkle + prefix combine") {
        info("Estimating total per-step cost for a Branch proof step")
        info("Per step = extraction + suffix/prefix + merkle reconstruction")
        info("")
        info("MPF-16:  1 nibble extraction + 1 combine(prefix, merkle16(...)) = 5 blake2b total")
        info("MPF-64:  1 sixit extraction  + combine3(prefix, merkle32, n32)  = 6 blake2b total")
        info("MPF-256: 1 byteAt extraction + 1 combine(prefix, merkle256(...))= 9 blake2b total")
        info("")

        // Simulate a Branch step: extract + merkle + combine prefix
        // All neighbors passed as a packed ByteString to avoid referencing class fields
        val neighbors4 = h1.concat(h2).concat(h3).concat(h4) // 128 bytes for merkle16
        val neighbors6 = h1.concat(h2).concat(h3).concat(h4).concat(h5).concat(h6) // 192 bytes
        val neighbors8 =
            h1.concat(h2)
                .concat(h3)
                .concat(h4)
                .concat(h5)
                .concat(h6)
                .concat(h7)
                .concat(h8) // 256 bytes

        // MPF-16 step: nibble + merkle16 + combine prefix (4 neighbors from packed ByteString)
        val step16Program = PlutusV3.compile { (pathD: Data, neighborsD: Data) =>
            import scalus.cardano.onchain.plutus.mpf.Merkling.*
            val path = unBData(pathD)
            val neighbors = unBData(neighborsD)
            val branch = nibble(path, 5)
            val root = blake2b_256(suffix(path, 6))
            val prefix = consByteString(4, ByteString.empty)
            combine(
              prefix,
              merkle16(
                branch,
                root,
                sliceByteString(0, 32, neighbors),
                sliceByteString(32, 32, neighbors),
                sliceByteString(64, 32, neighbors),
                sliceByteString(96, 32, neighbors)
              )
            )
        }

        // MPF-64 step: sixit + combine(prefix, merkle64(...)) (6 neighbors)
        val step64Program = PlutusV3.compile { (pathD: Data, neighborsD: Data) =>
            import scalus.cardano.onchain.plutus.mpf64.Merkling.*
            val path = unBData(pathD)
            val neighbors = unBData(neighborsD)
            val branch = sixit(path, 5)
            val root = blake2b_256(suffix(path, 6))
            val prefix = consByteString(4, ByteString.empty)
            combine(
              prefix,
              merkle64(
                branch,
                root,
                sliceByteString(0, 32, neighbors),
                sliceByteString(32, 32, neighbors),
                sliceByteString(64, 32, neighbors),
                sliceByteString(96, 32, neighbors),
                sliceByteString(128, 32, neighbors),
                sliceByteString(160, 32, neighbors)
              )
            )
        }

        // MPF-256 step: byteAt + merkle(generic) + combine prefix (8 neighbors)
        val step256Program = PlutusV3.compile { (pathD: Data, neighborsD: Data) =>
            import scalus.cardano.onchain.plutus.mpf256.Merkling.*
            val path = unBData(pathD)
            val branch = byteAt(path, 5)
            val root = blake2b_256(suffix(path, 6))
            val prefix = consByteString(4, ByteString.empty)
            combine(prefix, merkle(128, branch, root, unBData(neighborsD)))
        }

        val s16 = measure(step16Program, B(testPath).asTerm, B(neighbors4).asTerm)
        val s64 = measure(step64Program, B(testPath).asTerm, B(neighbors6).asTerm)
        val s256 = measure(step256Program, B(testPath).asTerm, B(neighbors8).asTerm)

        val header =
            f"${"System"}%-8s | ${"mem"}%8s | ${"cpu"}%12s | ${"ratio vs m16"}%14s"
        info(header)
        info("-" * header.length)
        info(f"${"MPF-16"}%-8s | ${s16.memory}%8d | ${s16.steps}%12d | ${1.0}%14.3f")
        info(
          f"${"MPF-64"}%-8s | ${s64.memory}%8d | ${s64.steps}%12d | ${s64.steps.toDouble / s16.steps}%14.3f"
        )
        info(
          f"${"MPF-256"}%-8s | ${s256.memory}%8d | ${s256.steps}%12d | ${s256.steps.toDouble / s16.steps}%14.3f"
        )

        // Projected total cost for typical proof (30 elements)
        // MPF-16: ~2 steps avg, MPF-64: ~1.5 steps avg, MPF-256: ~1.5 steps avg
        info("")
        info("Projected total cost (typical proof for 30 elements):")
        info(f"  MPF-16  (~2.1 steps):  ${s16.steps * 2.1}%.0f cpu")
        info(f"  MPF-64  (~1.7 steps):  ${s64.steps * 1.7}%.0f cpu")
        info(f"  MPF-256 (~1.5 steps):  ${s256.steps * 1.5}%.0f cpu")
    }
}
