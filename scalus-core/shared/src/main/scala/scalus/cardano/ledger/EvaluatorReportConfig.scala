package scalus.cardano.ledger

/** Which diagnostic artifacts the evaluator writes when reporting is enabled.
  *
  * @see
  *   [[EvaluatorReportConfig]]
  */
enum DumpArtifact:
    /** Fully-applied `.flat` program, one file per script. */
    case Flat

    /** Transaction + inputs + outputs CBOR (for `aiken tx simulate`). Wired in a later step. */
    case TxCbor

    /** Per-builtin cost log (`VALIDATE` mode only). */
    case BudgetLog

    /** Rendered [[scalus.uplc.eval.ProfilingData]] report. Wired in a later step. */
    case Profile

/** How verbose a rendered profile is. */
enum ProfileLevel:
    case Off, Summary, Full

/** Output format for a rendered profile. */
enum ProfileFormat:
    case Text, Csv, Html, Json

/** Where a single rendered profile is written. */
enum ProfileDestination:
    /** Standard out / log — "display". */
    case Console

    /** A file name resolved under [[EvaluatorReportConfig.outputDir]] if relative. */
    case File(name: String)

    /** An exact path, ignoring [[EvaluatorReportConfig.outputDir]]. */
    case AbsoluteFile(path: String)

/** One profile rendering: a format plus where it lands. */
final case class ProfileOutput(format: ProfileFormat, destination: ProfileDestination)

/** Controls all diagnostic output of [[PlutusScriptEvaluator]].
  *
  * Replaces the former lone `debugDumpFilesForTesting: Boolean`. Construct one directly, derive one
  * from the legacy boolean with [[EvaluatorReportConfig.fromLegacyBoolean]], or layer environment
  * overrides on top with [[EvaluatorReportConfig.fromEnv]].
  *
  * @param enabled
  *   master switch; when `false` the evaluator writes nothing
  * @param outputDir
  *   directory artifacts are written to (plain string so the type stays cross-platform; created via
  *   `platform.createDirectories`). Default `"."` is the current working directory, matching the
  *   historical dump location.
  * @param artifacts
  *   which [[DumpArtifact]]s to write
  * @param profile
  *   profile verbosity (rendering is wired in a later step)
  * @param profileOutputs
  *   explicit profile renderings; empty ⇒ derived from `profile`
  * @param profileThreshold
  *   rows below this fraction of total budget collapse into "... and N more"
  * @param maxRows
  *   row cap per profile section
  */
final case class EvaluatorReportConfig(
    enabled: Boolean = false,
    outputDir: String = ".",
    artifacts: Set[DumpArtifact] = Set(DumpArtifact.Flat),
    profile: ProfileLevel = ProfileLevel.Off,
    profileOutputs: Seq[ProfileOutput] = Nil,
    profileThreshold: Double = 0.01,
    maxRows: Int = 50
) {

    /** True when reporting is enabled and the given artifact is requested. */
    def dumps(artifact: DumpArtifact): Boolean = enabled && artifacts.contains(artifact)

    /** The profile renderings to produce: explicit [[profileOutputs]] when set, otherwise derived
      * from the [[profile]] level (Summary ⇒ compact text to the console; Full ⇒ HTML + JSON + CSV
      * files). The `profile.json` is the machine-readable rendering editors/tools consume — e.g.
      * the Scalus VS Code extension annotates source lines with per-line cost from it.
      */
    def effectiveProfileOutputs: Seq[ProfileOutput] =
        if profileOutputs.nonEmpty then profileOutputs
        else
            profile match
                case ProfileLevel.Off => Nil
                case ProfileLevel.Summary =>
                    Seq(ProfileOutput(ProfileFormat.Text, ProfileDestination.Console))
                case ProfileLevel.Full =>
                    Seq(
                      ProfileOutput(ProfileFormat.Html, ProfileDestination.File("profile.html")),
                      ProfileOutput(ProfileFormat.Json, ProfileDestination.File("profile.json")),
                      ProfileOutput(ProfileFormat.Csv, ProfileDestination.File("profile.csv"))
                    )
}

object EvaluatorReportConfig {

    /** Reporting fully off — the default. */
    val disabled: EvaluatorReportConfig = EvaluatorReportConfig()

    /** Map the legacy `debugDumpFilesForTesting` boolean onto a config.
      *
      * `true` reproduces the historical artifact set: the fully-applied `.flat` files plus the
      * per-builtin budget log (the latter only takes effect in `VALIDATE` mode). The on-disk layout
      * changes — stable, overwriting filenames plus a `manifest.json` — but the set of artifacts is
      * unchanged.
      */
    def fromLegacyBoolean(debugDumpFilesForTesting: Boolean): EvaluatorReportConfig =
        if debugDumpFilesForTesting then
            EvaluatorReportConfig(
              enabled = true,
              artifacts = Set(DumpArtifact.Flat, DumpArtifact.BudgetLog)
            )
        else disabled

    /** Layer environment overrides on top of `base`, field by field.
      *
      * Only variables that are present change anything; absent variables leave `base` untouched.
      * Presence of `SCALUS_DUMP` / `SCALUS_PROFILE` flips `enabled = true`; `SCALUS_DUMP=off` (also
      * `false`/`none`) is an explicit kill-switch that forces `enabled = false`.
      *
      *   - `SCALUS_DUMP` — comma list of `flat`,`cbor`,`budget`,`profile` (or `off`)
      *   - `SCALUS_DUMP_DIR` — output directory
      *   - `SCALUS_PROFILE` — `off` | `summary` | `full`
      *   - `SCALUS_PROFILE_OUT` — comma list of destinations: `-`/`console` to display, or a file
      *     name (format inferred from `.html`/`.csv`/`.json`/`.txt`); enables profiling
      *   - `SCALUS_PROFILE_THRESHOLD` — budget fraction (Double)
      *   - `SCALUS_PROFILE_MAX_ROWS` — Int
      *
      * `env` is injectable so the precedence rules can be unit-tested without touching the process
      * environment.
      */
    def fromEnv(
        base: EvaluatorReportConfig = disabled,
        env: Map[String, String] = sys.env
    ): EvaluatorReportConfig = {
        var cfg = base

        env.get("SCALUS_DUMP").map(_.trim.toLowerCase).foreach {
            case "off" | "false" | "none" | "" =>
                cfg = cfg.copy(enabled = false)
            case value =>
                val parsed = value.split(",").iterator.map(_.trim).flatMap(parseArtifact).toSet
                cfg = cfg.copy(
                  enabled = true,
                  artifacts = if parsed.nonEmpty then parsed else cfg.artifacts
                )
        }

        env.get("SCALUS_DUMP_DIR").map(_.trim).filter(_.nonEmpty).foreach { dir =>
            cfg = cfg.copy(outputDir = dir)
        }

        env.get("SCALUS_PROFILE").map(_.trim.toLowerCase).foreach {
            case "off"     => cfg = cfg.copy(profile = ProfileLevel.Off, profileOutputs = Nil)
            case "summary" => cfg = cfg.copy(enabled = true, profile = ProfileLevel.Summary)
            case "full"    => cfg = cfg.copy(enabled = true, profile = ProfileLevel.Full)
            case _         => // ignore unrecognised value
        }

        env.get("SCALUS_PROFILE_OUT").map(_.trim).filter(_.nonEmpty).foreach { spec =>
            val outs = spec
                .split(",")
                .iterator
                .map(_.trim)
                .filter(_.nonEmpty)
                .map {
                    case "-" | "console" | "stdout" =>
                        ProfileOutput(ProfileFormat.Text, ProfileDestination.Console)
                    case path if path.startsWith("/") =>
                        ProfileOutput(formatFromName(path), ProfileDestination.AbsoluteFile(path))
                    case name =>
                        ProfileOutput(formatFromName(name), ProfileDestination.File(name))
                }
                .toSeq
            cfg = cfg.copy(
              enabled = true,
              profileOutputs = outs,
              profile = if cfg.profile == ProfileLevel.Off then ProfileLevel.Full else cfg.profile
            )
        }

        env.get("SCALUS_PROFILE_THRESHOLD").flatMap(_.trim.toDoubleOption).foreach { t =>
            cfg = cfg.copy(profileThreshold = t)
        }

        env.get("SCALUS_PROFILE_MAX_ROWS").flatMap(_.trim.toIntOption).foreach { n =>
            cfg = cfg.copy(maxRows = n)
        }

        cfg
    }

    private def parseArtifact(name: String): Option[DumpArtifact] = name match
        case "flat"                 => Some(DumpArtifact.Flat)
        case "cbor" | "txcbor"      => Some(DumpArtifact.TxCbor)
        case "budget" | "budgetlog" => Some(DumpArtifact.BudgetLog)
        case "profile"              => Some(DumpArtifact.Profile)
        case _                      => None

    /** Infer a [[ProfileFormat]] from a destination file extension, defaulting to `Text`. */
    private def formatFromName(name: String): ProfileFormat =
        val lower = name.toLowerCase
        if lower.endsWith(".html") || lower.endsWith(".htm") then ProfileFormat.Html
        else if lower.endsWith(".csv") then ProfileFormat.Csv
        else if lower.endsWith(".json") then ProfileFormat.Json
        else ProfileFormat.Text
}
