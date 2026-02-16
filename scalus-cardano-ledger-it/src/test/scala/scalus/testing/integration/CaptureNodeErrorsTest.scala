package scalus.testing.integration

import org.scalatest.funsuite.AnyFunSuite
import scalus.uplc.builtin.Data
import scalus.cardano.ledger.*
import scalus.cardano.txbuilder.*
import scalus.compiler.Options
import scalus.testing.yaci.YaciDevKit
import scalus.uplc.PlutusV3
import scalus.utils.await
import sttp.client4.*

import java.io.{File, PrintWriter}
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Future
import scala.concurrent.duration.*

/** Capture real Yaci DevKit script failure responses for use as test fixtures.
  *
  * These tests bypass local script evaluation using `TxBuilder.withConstMaxBudgetEvaluator` and
  * submit deliberately-failing scripts to the node. The raw HTTP JSON response is captured and saved
  * to resources for use in `SubmitErrorTest`.
  *
  * Run with: `sbtn "scalusCardanoLedgerIt/testOnly *CaptureNodeErrorsTest"`
  */
class CaptureNodeErrorsTest extends AnyFunSuite with YaciDevKit {

    private lazy val ctx = createYaciContext()

    private def baseUrl: String = container.getYaciStoreApiUrl.stripSuffix("/")

    /** Submit a transaction via raw HTTP POST and return the status code + response body. */
    private def rawSubmit(tx: Transaction): (Int, String) = {
        given backend: Backend[Future] = DefaultFutureBackend()
        val url = s"$baseUrl/tx/submit"
        val response = basicRequest
            .post(uri"$url")
            .headers(Map("Content-Type" -> "application/cbor"))
            .body(tx.toCbor)
            .send(backend)
            .await(30.seconds)
        val body = response.body match
            case Right(b) => b
            case Left(b)  => b
        (response.code.code, body)
    }

    /** Save content to the integration test resources directory. */
    private def saveToResources(filename: String, content: String): Unit = {
        // Try both paths: from project root and from subproject root (forked JVM)
        val candidates = Seq(
          new File("scalus-cardano-ledger-it/src/test/resources"),
          new File("src/test/resources")
        )
        val dir = candidates.find(_.isDirectory).getOrElse {
            println("Warning: resources directory not found, saving to current directory")
            new File(".")
        }
        val file = new File(dir, filename)
        val pw = new PrintWriter(file)
        try pw.write(content)
        finally pw.close()
        println(s"Saved to: ${file.getAbsolutePath}")
    }

    test("capture script failure from node") {
        val env = ctx.cardanoInfo

        // Simple always-failing validator with error traces
        given Options = Options.debug
        val failingContract = PlutusV3.compile { (_: Data) =>
            import scalus.cardano.onchain.plutus.prelude.*
            require(false, "test_script_failure")
        }
        val script = failingContract.script
        val scriptAddress = failingContract.address(env.network)

        // Step 1: Lock funds at script address with inline datum
        val aliceUtxos = ctx.provider.findUtxos(ctx.alice.address).await().toOption.get
        val lockTx = TxBuilder(env)
            .payTo(scriptAddress, Value.ada(10), Data.unit)
            .complete(availableUtxos = aliceUtxos, sponsor = ctx.alice.address)
            .sign(ctx.alice.signer)
            .transaction

        val lockResult = ctx.submit(lockTx).await()
        assert(lockResult.isRight, s"Lock failed: $lockResult")

        // Step 2: Find locked UTxO
        val scriptUtxos = ctx.provider.findUtxos(scriptAddress).await().toOption.get
        val lockedUtxo = Utxo(scriptUtxos.head)

        // Step 3: Build spend tx bypassing local evaluation
        val bobUtxos = ctx.provider.findUtxos(ctx.bob.address).await().toOption.get
        val tx = TxBuilder
            .withConstMaxBudgetEvaluator(env)
            .spend(lockedUtxo, Data.unit, script, Set(ctx.bob.addrKeyHash))
            .payTo(ctx.bob.address, lockedUtxo.output.value)
            .complete(availableUtxos = bobUtxos, sponsor = ctx.bob.address)
            .sign(ctx.bob.signer)
            .transaction

        // Step 4: Raw HTTP submit to capture full response
        val (statusCode, responseBody) = rawSubmit(tx)
        println(s"=== Script Failure Response ===")
        println(s"Status: $statusCode")
        println(s"Body: $responseBody")

        saveToResources("yaci-error-script-failure.json", responseBody)

        // Also submit via provider to see the parsed SubmitError
        val submitResult = ctx.provider.submit(tx).await()
        println(s"Parsed SubmitError: $submitResult")

        assert(statusCode >= 400, s"Expected error status but got $statusCode")
    }

    test("capture script failure with log() trace output") {
        val env = ctx.cardanoInfo

        // Always-failing validator that emits multiple log() lines before failing
        given Options = Options.debug
        val loggingContract = PlutusV3.compile { (_: Data) =>
            import scalus.cardano.onchain.plutus.prelude.*
            log("log_line_1")
            log("log_line_2")
            log("log_line_3")
            require(false, "always_fail")
        }
        val loggingScript: Script.PlutusV3 = loggingContract.script
        val scriptAddress = loggingContract.address(env.network)

        // Step 1: Lock funds at script address with inline datum
        val aliceUtxos = ctx.provider.findUtxos(ctx.alice.address).await().toOption.get
        val lockTx = TxBuilder(env)
            .payTo(scriptAddress, Value.ada(10), Data.unit)
            .complete(availableUtxos = aliceUtxos, sponsor = ctx.alice.address)
            .sign(ctx.alice.signer)
            .transaction

        val lockResult = ctx.submit(lockTx).await()
        assert(lockResult.isRight, s"Lock failed: $lockResult")

        // Step 2: Find locked UTxO
        val scriptUtxos = ctx.provider.findUtxos(scriptAddress).await().toOption.get
        val lockedUtxo = Utxo(scriptUtxos.head)

        // Step 3: Build spend tx with const max budget evaluator (bypasses local eval)
        val bobUtxos = ctx.provider.findUtxos(ctx.bob.address).await().toOption.get
        val tx = TxBuilder
            .withConstMaxBudgetEvaluator(env)
            .spend(lockedUtxo, Data.unit, loggingScript, Set(ctx.bob.addrKeyHash))
            .payTo(ctx.bob.address, lockedUtxo.output.value)
            .complete(availableUtxos = bobUtxos, sponsor = ctx.bob.address)
            .sign(ctx.bob.signer)
            .transaction

        // Step 4: Raw HTTP submit to capture full response
        val (statusCode, responseBody) = rawSubmit(tx)
        println(s"=== Logging Script Failure Response ===")
        println(s"Status: $statusCode")
        println(s"Body: $responseBody")

        saveToResources("yaci-error-script-failure-with-logs.json", responseBody)

        // Also submit via provider to see the parsed SubmitError
        val submitResult = ctx.provider.submit(tx).await()
        println(s"Parsed SubmitError: $submitResult")

        assert(statusCode >= 400, s"Expected error status but got $statusCode")
    }
}
