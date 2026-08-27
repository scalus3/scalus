package scalus.examples.cape

import org.scalatest.funsuite.AnyFunSuite

class CapeMetadataTest extends AnyFunSuite {
    private val hash = "a" * 40
    private val json = ujson.read(
      CapeMetadata.render(
        version = "1.1.0",
        compilerCommit = hash,
        date = "2026-08-24T00:00:00Z",
        sourceCommit = hash,
        notes = "Some \"quoted\" notes"
      )
    )

    test("schema-required fields are present and well-formed") {
        assert(json("compiler")("name").str == "Scalus")
        assert(json("compiler")("version").str == "1.1.0")
        assert(json("compiler")("commit_hash").str.matches("^[a-f0-9]{40}$"))
        assert(json("compilation_config")("target").str == "uplc")
        assert(json("submission")("date").str == "2026-08-24T00:00:00Z")
        assert(json("submission")("source_available").bool)
        assert(json("submission")("source_repository").str.startsWith("https://github.com/"))
        assert(json("submission")("source_commit_hash").str.matches("^[a-f0-9]{40}$"))
        assert(json("submission")("implementation_notes").str.contains("\"quoted\""))
        assert(json("contributors").arr.nonEmpty)
    }

    test("min_plutus_version is absent from compilation_config when not set") {
        assert(!json("compilation_config").obj.contains("min_plutus_version"))
    }

    test("min_plutus_version is present and schema-valid when set") {
        val withVersion = ujson.read(
          CapeMetadata.render(
            version = "1.1.0",
            compilerCommit = hash,
            date = "2026-08-24T00:00:00Z",
            sourceCommit = hash,
            notes = "Some notes",
            minPlutusVersion = Some("1.60.0.0")
          )
        )
        val minVer = withVersion("compilation_config")("min_plutus_version").str
        assert(minVer == "1.60.0.0")
        assert(minVer.matches("^[0-9]+\\.[0-9]+\\.[0-9]+\\.[0-9]+$"))
    }

    test("registry covers all 8 scenarios with unique names") {
        val names = CapeScenarios.all.map(_.name)
        assert(
          names.sorted == scala.List(
            "ecd",
            "factorial",
            "factorial_naive_recursion",
            "fibonacci",
            "fibonacci_naive_recursion",
            "htlc",
            "linear_vesting",
            "two_party_escrow"
          )
        )
    }

    test("scenarios needing PV11 features carry a schema-valid min_plutus_version") {
        // All 8 scenarios are gated, including both hand-crafted open-mode scenarios
        // (`fibonacci`, `factorial`) -- both bake their lookup table in as PV11
        // case-on-builtin-integer branches, cased directly on the input Integer, so
        // "hand-crafted" doesn't mean "ungated". `factorial` moved into this set when its
        // `FactorialOpen.termB` (PV11 case-on-builtins) replaced the PV9-compatible `termA`
        // (sliceByteString/byteStringToInteger) as the adopted `term`: termB measured ~4x cheaper
        // (4,545,903 vs 18,413,280 summed steps across the 10 fixture cases) and a smaller script
        // (91 vs 109 bytes), at the cost of moving off CAPE's production evaluator track onto its
        // preview track like the other 7 scenarios -- see FactorialOpen.scala and
        // docs/internal/CAPE_COMPETITIVE_ANALYSIS.md.
        val gated = Set(
          "factorial",
          "factorial_naive_recursion",
          "fibonacci_naive_recursion",
          "fibonacci",
          "ecd",
          "htlc",
          "linear_vesting",
          "two_party_escrow"
        )
        val ungated = Set.empty[String]
        // Exhaustiveness guard: every registered scenario must be classified as gated or ungated, so
        // a future 9th scenario can't silently skip both branches of the loop below.
        assert((gated ++ ungated) == CapeScenarios.all.map(_.name).toSet)
        for s <- CapeScenarios.all do {
            if gated.contains(s.name) then {
                assert(s.minPlutusVersion.isDefined, s"${s.name} should have minPlutusVersion set")
                assert(
                  s.minPlutusVersion.get.matches("^[0-9]+\\.[0-9]+\\.[0-9]+\\.[0-9]+$"),
                  s"${s.name}'s minPlutusVersion must match the schema pattern"
                )
            } else if ungated.contains(s.name) then {
                assert(
                  s.minPlutusVersion.isEmpty,
                  s"${s.name} should NOT have minPlutusVersion set"
                )
            }
        }
    }
}
