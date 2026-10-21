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

    test("compilation_config requires protocol version 11 and no preview evaluator") {
        assert(json("compilation_config")("min_protocol_version").num == 11)
        assert(!json("compilation_config").obj.contains("min_plutus_version"))
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
}
