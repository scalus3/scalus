package scalus.cardano.ledger

import io.bullet.borer.Cbor
import org.openjdk.jmh.annotations.*
import scalus.utils.Hex.hexToBytes

import java.nio.charset.StandardCharsets
import java.nio.file.{FileSystemNotFoundException, FileSystems, Files, Path}
import java.util
import java.util.concurrent.TimeUnit
import java.util.zip.GZIPInputStream
import scala.annotation.nowarn
import scala.jdk.CollectionConverters.*
import scala.util.Using

/** Replays recorded evaluator calls: the requests Midgard's TypeScript suites made on 2026-09-23,
  * one JSON file (optionally gzipped) per `eval_phase_two_raw` call, the same files the JavaScript
  * benchmarks in `bench/js` replay. One operation evaluates every loaded request once.
  *
  * `dir` is a directory of such files, or the name of a sample of 10 requests bundled under
  * `midgard/` in this module's resources: `deep-deposit`, `mint-authorization` or
  * `value-conservation`. The full recordings are not in the repository; `bench/js/README.md` says
  * how to record new ones.
  *
  * `evaluateFromCbor` does the work `TxEvaluator.evaluate` does in JavaScript (`replay-js.mjs`):
  * decode the transaction and its inputs, then evaluate with an evaluator kept per cost models,
  * slot configuration and budget. `evaluate` reuses the decoded transactions, so it measures the
  * machine alone.
  *
  * Set `-Dscalus.bench.results=<prefix>` to write every request's redeemer budgets to
  * `<prefix>.<dump dir name>.txt`, one line per request, in the format `replay-js.mjs --results`
  * writes, so the two platforms can be diffed.
  *
  * {{{
  * sbtn "bench/Jmh/run -i 5 -wi 3 -f 1 -t 1 .*MidgardReplayBenchmark.*"                       # the samples
  * sbtn "bench/Jmh/run -i 5 -wi 3 -f 1 -t 1 -p dir=<dump dir> -p limit=300 .*MidgardReplayBenchmark.*"
  * # with async-profiler: add  -prof async:libPath=<libasyncProfiler.dylib>;output=flamegraph;event=cpu
  * }}}
  */
@State(Scope.Benchmark)
@BenchmarkMode(Array(Mode.AverageTime))
@OutputTimeUnit(TimeUnit.MILLISECONDS)
class MidgardReplayBenchmark {

    @Param(Array("deep-deposit", "mint-authorization", "value-conservation"))
    @nowarn("msg=unset private variable")
    private var dir: String = ""

    @Param(Array("300"))
    @nowarn("msg=unset private variable")
    private var limit: Int = 0

    private case class Request(
        name: String,
        evaluator: PlutusScriptEvaluator,
        txBytes: Array[Byte],
        utxoBytes: IndexedSeq[(Array[Byte], Array[Byte])],
        tx: Transaction,
        utxos: Utxos
    )

    private var requests: IndexedSeq[Request] = IndexedSeq.empty

    @Setup
    def setup(): Unit = {
        val files = requestFiles()
        // One evaluator per distinct (cost models, slot configuration, budget), as an SDK keeps them.
        val evaluators = collection.mutable.Map.empty[String, PlutusScriptEvaluator]
        requests = files.map { file =>
            val d = ujson.read(read(file))
            val slotConfig = SlotConfig(
              zeroTime = d("zeroTime").str.toLong,
              zeroSlot = d("zeroSlot").str.toLong,
              slotLength = d("slotLength").num.toInt
            )
            val budget = ExUnits(d("maxMemory").str.toLong, d("maxSteps").str.toLong)
            val key = s"${d("costModels").str}|$slotConfig|$budget"
            val evaluator = evaluators.getOrElseUpdate(
              key,
              PlutusScriptEvaluator(
                slotConfig = slotConfig,
                initialBudget = budget,
                protocolMajorVersion = MajorProtocolVersion(11),
                costModels = CostModels(
                  Cbor.decode(d("costModels").str.hexToBytes)
                      .to[Map[Int, IndexedSeq[Long]]]
                      .value
                ),
                mode = EvaluatorMode.EvaluateAndComputeCost
              )
            )
            val txBytes = d("tx").str.hexToBytes
            val utxoBytes = d("inputs").arr
                .zip(d("outputs").arr)
                .map((in, out) => (in.str.hexToBytes, out.str.hexToBytes))
                .toIndexedSeq
            Request(
              file.getFileName.toString,
              evaluator,
              txBytes,
              utxoBytes,
              Transaction.fromCbor(txBytes),
              decodeUtxos(utxoBytes)
            )
        }
        sys.props
            .get("scalus.bench.results")
            .foreach(prefix => writeResults(s"$prefix.${Path.of(dir).getFileName}.txt"))
    }

    /** The request files `dir` names: its own, or those of the bundled sample of that name. */
    private def requestFiles(): IndexedSeq[Path] = {
        val folder =
            if Files.isDirectory(Path.of(dir)) then Path.of(dir)
            else
                val sample = getClass.getResource(s"/midgard/$dir")
                if sample == null then
                    throw new IllegalArgumentException(
                      s"dir=$dir is neither a directory nor a bundled sample " +
                          "(deep-deposit, mint-authorization, value-conservation)"
                    )
                val uri = sample.toURI
                // JMH runs from jars: a resource directory is reachable through the jar's file system
                if uri.getScheme == "jar" then
                    try FileSystems.getFileSystem(uri)
                    catch
                        case _: FileSystemNotFoundException =>
                            FileSystems.newFileSystem(uri, util.Map.of[String, Any]())
                Path.of(uri)
        val files = Files
            .list(folder)
            .iterator()
            .asScala
            .filter { f =>
                val name = f.getFileName.toString
                name.endsWith(".json") || name.endsWith(".json.gz")
            }
            .toIndexedSeq
            .sorted
            .take(limit)
        require(files.nonEmpty, s"no .json or .json.gz requests in $folder")
        files
    }

    private def read(file: Path): String =
        if file.getFileName.toString.endsWith(".gz") then
            Using.resource(GZIPInputStream(Files.newInputStream(file))) { in =>
                String(in.readAllBytes(), StandardCharsets.UTF_8)
            }
        else Files.readString(file)

    private def decodeUtxos(utxoBytes: IndexedSeq[(Array[Byte], Array[Byte])]): Utxos =
        utxoBytes.map { (in, out) =>
            Cbor.decode(in).to[TransactionInput].value -> Cbor
                .decode(out)
                .to[TransactionOutput]
                .value
        }.toMap

    /** What one request evaluates to: its redeemers' budgets, sorted, or the failure code. */
    private def result(evaluate: => Seq[Redeemer]): String =
        try
            evaluate
                .map(r => s"${r.tag}:${r.index}:${r.exUnits.memory}:${r.exUnits.steps}")
                .sorted
                .mkString(" ")
        catch case _: PlutusScriptEvaluationException => "SCRIPT_FAILURE"

    private def writeResults(file: String): Unit = {
        val lines = requests.map { r =>
            s"${r.name} ${result(r.evaluator.evalPlutusScripts(r.tx, r.utxos))}"
        }
        Files.writeString(Path.of(file), lines.mkString("", "\n", "\n"))
    }

    /** Evaluates the decoded transactions. Scripts stay decoded on their `Transaction` after the
      * first iteration, so this measures the machine, not decoding.
      */
    @Benchmark
    def evaluate(): Int = {
        var redeemers = 0
        for r <- requests do
            try redeemers += r.evaluator.evalPlutusScripts(r.tx, r.utxos).size
            catch case _: PlutusScriptEvaluationException => ()
        redeemers
    }

    /** Decodes each transaction and its inputs, then evaluates: the work of the JavaScript
      * `TxEvaluator.evaluate`.
      */
    @Benchmark
    def evaluateFromCbor(): Int = {
        var redeemers = 0
        for r <- requests do
            try
                val tx = Transaction.fromCbor(r.txBytes)
                redeemers += r.evaluator.evalPlutusScripts(tx, decodeUtxos(r.utxoBytes)).size
            catch case _: PlutusScriptEvaluationException => ()
        redeemers
    }
}
