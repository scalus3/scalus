package scalus.uplc.eval

import com.github.plokhotnyuk.jsoniter_scala.core.*
import com.github.plokhotnyuk.jsoniter_scala.macros.JsonCodecMaker
import scalus.cardano.ledger.{EvaluatorReportConfig, ProfileDestination, ProfileFormat, ProfileLevel}
import scalus.uplc.Term
import scalus.uplc.builtin.platform

import scala.util.control.NonFatal

/** Renders a CEK profile to the destinations an [[scalus.cardano.ledger.EvaluatorReportConfig]]
  * asks for and indexes the files it wrote in `profile-manifest.json`.
  *
  * Extracted from [[scalus.cardano.ledger.PlutusScriptEvaluator]] so that profiles produced outside
  * the ledger — test suites that evaluate UPLC directly, via `ScalusTest.runWithProfileReport` —
  * land on disk in exactly the same shape, and tools such as the Scalus VS Code extension consume
  * both identically.
  *
  * Internal (`private[scalus]`): the on-disk format is the contract, not this API.
  */
private[scalus] object ProfileReportWriter {

    /** Schema version of `profile-manifest.json`. Bump on any incompatible change to its shape so
      * consumers (e.g. the Scalus VS Code extension) can detect and reject manifests they don't
      * understand.
      */
    val ManifestSchemaVersion = 1

    // The profile-manifest.json document, mirrored 1:1 by jsoniter. `file` paths are relative to
    // the manifest's directory for ProfileDestination.File outputs, absolute for AbsoluteFile.
    private final case class ProfileManifest(schemaVersion: Int, runs: Seq[ProfileManifestRun])
    private final case class ProfileManifestRun(
        scriptHash: String,
        language: String,
        redeemer: ProfileManifestRedeemer,
        budget: ProfileManifestBudget,
        files: Seq[ProfileManifestFile]
    )
    private final case class ProfileManifestRedeemer(tag: String, index: Int)
    private final case class ProfileManifestBudget(mem: Long, cpu: Long)
    private final case class ProfileManifestFile(format: String, file: String)
    private given JsonValueCodec[ProfileManifest] = JsonCodecMaker.make

    /** Serializes in-process read-merge-write cycles on `profile-manifest.json`, so concurrent
      * evaluations don't lose each other's runs. The manifest on disk is the only registry —
      * nothing is retained in memory between writes.
      */
    private val profileManifestLock = new Object

    /** Render `data` to each configured [[scalus.cardano.ledger.ProfileOutput]] (console / files).
      * File destinations are prefixed with the script key so per-redeemer profiles don't collide,
      * and are also recorded in `profile-manifest.json` (see [[writeManifest]]). The actual
      * rendering is delegated to the platform-specific [[ProfileReporting]] so that
      * [[ProfileFormatter]] (HTML/CSS/JS templates, Tarjan pass) stays out of the JS bundle; HTML
      * output annotates source lines when the source file is readable from the CWD (JVM only —
      * [[ProfileReporting]] returns `None` on JS).
      *
      * @param onConsole
      *   sink for [[scalus.cardano.ledger.ProfileDestination.Console]] output, so callers keep
      *   their own logger.
      * @param uplcTerm
      *   the evaluated program's term, when the caller has it in annotated form. With
      *   [[scalus.cardano.ledger.ProfileLevel.Full]], and only alongside at least one rendered
      *   profile file, it adds a `<key>.uplc.json` source map (the UPLC text plus text-range →
      *   Scala-source spans) to the report, indexed as format `"uplc"`. It is not a
      *   [[scalus.cardano.ledger.ProfileFormat]] because those are rendered from [[ProfilingData]],
      *   which carries no term. Terms decoded from CBOR carry no annotations, and nothing is
      *   written for them.
      */
    def write(
        data: ProfilingData,
        report: EvaluatorReportConfig,
        scriptHash: String,
        language: String,
        redeemerTag: String,
        redeemerIndex: Int,
        onConsole: String => Unit,
        uplcTerm: Option[Term] = None
    ): Unit = {
        val key = s"$scriptHash-$redeemerTag-$redeemerIndex"
        val outputs = report.effectiveProfileOutputs
        // Self-sufficient: callers outside the ledger (e.g. ScalusTest) have no evaluation to
        // prepare the directory for them, and creating it is idempotent for the ones that do.
        if outputs.exists(_.destination.isInstanceOf[ProfileDestination.File]) then
            platform.createDirectories(report.outputDir)
        val written = Seq.newBuilder[(String, String)]
        outputs.foreach { out =>
            ProfileReporting.render(data, out.format, report.profile, report.maxRows).foreach {
                content =>
                    out.destination match
                        case ProfileDestination.Console =>
                            onConsole(s"Profile $key:\n$content")
                        case ProfileDestination.File(name) =>
                            val file = s"$key.$name"
                            platform.writeFile(reportPath(report, file), content.getBytes("UTF-8"))
                            written += formatLabel(out.format) -> file
                        case ProfileDestination.AbsoluteFile(path) =>
                            val sep = math.max(path.lastIndexOf('/'), path.lastIndexOf('\\'))
                            if sep > 0 then platform.createDirectories(path.substring(0, sep))
                            platform.writeFile(path, content.getBytes("UTF-8"))
                            written += formatLabel(out.format) -> path
            }
        }
        val profileFiles = written.result()
        // The source map only ever accompanies rendered profile files. A run that wrote none (a
        // console-only report) must stay off disk entirely: writing one would create the output
        // directory unasked and, worse, replace this script's manifest run – keyed by
        // (scriptHash, tag, index) – with an entry listing the source map alone, hiding the
        // profile files an earlier run had indexed there.
        val uplcFiles = uplcTerm match
            case Some(term)
                if profileFiles.nonEmpty && report.profile == ProfileLevel.Full &&
                    UplcSourceMapRenderer.hasSourceInfo(term) =>
                val file = s"$key.uplc.json"
                platform.createDirectories(report.outputDir)
                platform.writeFile(
                  reportPath(report, file),
                  UplcSourceMapRenderer.toJson(UplcSourceMapRenderer.render(term))
                )
                Seq("uplc" -> file)
            case _ => Nil
        val files = profileFiles ++ uplcFiles
        if files.nonEmpty then
            writeManifest(
              report,
              ProfileManifestRun(
                scriptHash,
                language,
                ProfileManifestRedeemer(redeemerTag, redeemerIndex),
                ProfileManifestBudget(data.totalBudget.memory, data.totalBudget.steps),
                files.map((fmt, f) => ProfileManifestFile(fmt, f))
              )
            )
    }

    /** Path under `report`'s output directory, or the bare name when the dir is the CWD. */
    private def reportPath(report: EvaluatorReportConfig, name: String): String =
        if report.outputDir.isEmpty || report.outputDir == "." then name
        else s"${report.outputDir}/$name"

    /** Lower-case manifest label for a profile format: "text", "csv", "html", "json". */
    private def formatLabel(format: ProfileFormat): String = format.toString.toLowerCase

    /** Merge `run` into `profile-manifest.json`: the machine-readable entry point (schema version
      * 1) listing every profile run rendered to files in this report's output directory, so tools
      * (e.g. the Scalus VS Code extension) can discover profiles without guessing file names. The
      * manifest on disk is the only registry: each write re-reads it and replaces the entry with
      * the same (scriptHash, tag, index) key — the key the overwriting file names use — so
      * fee-balancing re-evaluations update in place while other scripts' runs (from any evaluator
      * instance or earlier process) are preserved. An absent, foreign or unsupported-version
      * manifest is started fresh.
      */
    private def writeManifest(report: EvaluatorReportConfig, run: ProfileManifestRun): Unit =
        profileManifestLock.synchronized {
            val path = reportPath(report, "profile-manifest.json")
            val existing =
                try
                    val parsed = readFromArray[ProfileManifest](platform.readFile(path))
                    if parsed.schemaVersion == ManifestSchemaVersion then parsed.runs
                    else Seq.empty
                catch case NonFatal(_) => Seq.empty
            def key(r: ProfileManifestRun) = (r.scriptHash, r.redeemer.tag, r.redeemer.index)
            val runs = (existing.filterNot(r => key(r) == key(run)) :+ run).sortBy(key)
            val manifest = ProfileManifest(ManifestSchemaVersion, runs)
            platform.writeFile(path, writeToArray(manifest, WriterConfig.withIndentionStep(2)))
        }
}
