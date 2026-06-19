package scalus.cardano.ledger

import org.scalatest.funsuite.AnyFunSuite

class EvaluatorReportConfigTest extends AnyFunSuite {

    test("disabled is the default and dumps nothing") {
        val cfg = EvaluatorReportConfig.disabled
        assert(!cfg.enabled)
        DumpArtifact.values.foreach(a => assert(!cfg.dumps(a)))
    }

    test("fromLegacyBoolean reproduces the historical artifact set") {
        val on = EvaluatorReportConfig.fromLegacyBoolean(true)
        assert(on.enabled)
        assert(on.artifacts == Set(DumpArtifact.Flat, DumpArtifact.BudgetLog))
        assert(on.profile == ProfileLevel.Off)

        assert(EvaluatorReportConfig.fromLegacyBoolean(false) == EvaluatorReportConfig.disabled)
    }

    test("fromEnv leaves base untouched when no relevant vars are present") {
        val base = EvaluatorReportConfig.fromLegacyBoolean(true)
        assert(EvaluatorReportConfig.fromEnv(base, Map.empty) == base)
        assert(EvaluatorReportConfig.fromEnv(base, Map("UNRELATED" -> "x")) == base)
    }

    test("SCALUS_DUMP enables and selects artifacts") {
        val cfg = EvaluatorReportConfig.fromEnv(
          EvaluatorReportConfig.disabled,
          Map("SCALUS_DUMP" -> "flat,budget")
        )
        assert(cfg.enabled)
        assert(cfg.artifacts == Set(DumpArtifact.Flat, DumpArtifact.BudgetLog))
    }

    test("SCALUS_DUMP=off is a kill-switch over an enabled base") {
        val cfg = EvaluatorReportConfig.fromEnv(
          EvaluatorReportConfig.fromLegacyBoolean(true),
          Map("SCALUS_DUMP" -> "off")
        )
        assert(!cfg.enabled)
    }

    test("SCALUS_DUMP_DIR overrides only the output directory") {
        val base = EvaluatorReportConfig.fromLegacyBoolean(true)
        val cfg = EvaluatorReportConfig.fromEnv(base, Map("SCALUS_DUMP_DIR" -> "target/dump"))
        assert(cfg.outputDir == "target/dump")
        assert(cfg.artifacts == base.artifacts)
        assert(cfg.enabled == base.enabled)
    }

    test("SCALUS_PROFILE sets level and enables") {
        val cfg = EvaluatorReportConfig.fromEnv(
          EvaluatorReportConfig.disabled,
          Map("SCALUS_PROFILE" -> "full")
        )
        assert(cfg.enabled)
        assert(cfg.profile == ProfileLevel.Full)
    }

    test("Full level derives HTML, JSON and CSV file outputs") {
        // The JSON sibling is the machine-readable rendering tools (e.g. the VS Code extension)
        // consume; keep it in the default Full set alongside the human-facing HTML/CSV.
        val outs = EvaluatorReportConfig(profile = ProfileLevel.Full).effectiveProfileOutputs
        assert(
          outs.map(_.format).toSet == Set(ProfileFormat.Html, ProfileFormat.Json, ProfileFormat.Csv)
        )
        assert(
          outs.contains(ProfileOutput(ProfileFormat.Json, ProfileDestination.File("profile.json")))
        )
    }

    test("SCALUS_PROFILE=off clears level and derived outputs") {
        val base = EvaluatorReportConfig(
          enabled = true,
          profile = ProfileLevel.Full,
          profileOutputs = Seq(ProfileOutput(ProfileFormat.Html, ProfileDestination.File("p.html")))
        )
        val cfg = EvaluatorReportConfig.fromEnv(base, Map("SCALUS_PROFILE" -> "off"))
        assert(cfg.profile == ProfileLevel.Off)
        assert(cfg.profileOutputs.isEmpty)
        assert(cfg.effectiveProfileOutputs.isEmpty)
    }

    test("numeric overrides parse, bad values are ignored") {
        val cfg = EvaluatorReportConfig.fromEnv(
          EvaluatorReportConfig.disabled,
          Map("SCALUS_PROFILE_THRESHOLD" -> "0.25", "SCALUS_PROFILE_MAX_ROWS" -> "nan")
        )
        assert(cfg.profileThreshold == 0.25)
        assert(cfg.maxRows == EvaluatorReportConfig.disabled.maxRows)
    }
}
