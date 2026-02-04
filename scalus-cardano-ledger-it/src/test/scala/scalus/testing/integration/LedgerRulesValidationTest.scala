package scalus.testing.integration

import org.scalatest.funsuite.AnyFunSuite
import scalus.cardano.ledger.*
import scalus.cardano.ledger.rules.*
import scalus.testing.integration.BlocksTestUtils.*
import scalus.bloxbean.StakeStateResolver
import scalus.cardano.ledger.TransactionException.*

import java.nio.file.Files
import java.util.concurrent.atomic.AtomicInteger
import scala.util.Try
import scala.util.chaining.*

class LedgerRulesValidationTest extends AnyFunSuite {

    private lazy val stakeStateResolver =
        StakeStateResolver(apiKey, resourcesPath.resolve("stake"))

    test("validate transactions") {
        val transactionsCount = AtomicInteger()
        val utxosResolvedCount = AtomicInteger()

        val blocks = getAllBlocksPaths().take(1000)

        println(s"Validate ${blocks.size} blocks ...")

        val failed = for
            path <- blocks
            bytes = Files.readAllBytes(path)
            block = BlockFile.fromCborArray(bytes).block
            transaction <- block.transactions(using OriginalCborByteArray(bytes))
            _ = transactionsCount.incrementAndGet()
            utxos <- Try(scalusUtxoResolver.resolveUtxos(transaction)).toOption
            _ = utxosResolvedCount.incrementAndGet()
            certState = stakeStateResolver.resolveForTx(transaction, epochMagic)
            state = State(utxos = utxos, certState = certState)
            result <- CardanoMutator
                .transit(Context.testMainnet(block.slot), state, transaction)
                .swap
                .toOption
        yield (path.getFileName, transaction, result)
            .tap(x => println(pprint(x)))

        val groups = failed
            .groupBy(_._3.getClass.getSimpleName)
            .toSeq
            .sortBy(-_._2.length)

        println()
        println(s"Transactions count: ${transactionsCount.get()}")
        println(s"UTXOs resolved count: ${utxosResolvedCount.get()}")
        println(s"Failed transactions: ${failed.size}")
        println(s"Failures rate: ${(100 * failed.size) / utxosResolvedCount.get()}%")
        println(s"\nFailures by exception type:")
        groups.foreach { case (exType, failures) =>
            println(s"  $exType: ${failures.length}")
        }


        // Expected failure counts by exception type
        // These are due to incomplete ledger rule implementation or inaccurate stake state resolution
        val expectedCounts = Map(
            classOf[WithdrawalsNotInRewardsException] -> 26,
            classOf[ValueNotConservedUTxOException] -> 1,
            classOf[StakeCertificatesException] -> 5
        )

        val actualCounts = failed.groupBy(_._3.getClass).view.mapValues(_.size).toMap

        expectedCounts.foreach { case (exClass, expectedCount) =>
            val actualCount = actualCounts.getOrElse(exClass, 0)
            assert(
              actualCount == expectedCount,
              s"Expected $expectedCount ${exClass.getSimpleName} failures, got $actualCount"
            )
        }

        // Check for unexpected exception types
        val unexpectedTypes = actualCounts.keySet -- expectedCounts.keySet
        assert(
          unexpectedTypes.isEmpty,
          s"Unexpected exception types: ${unexpectedTypes.map(_.getSimpleName).mkString(", ")}"
        )
    }
}
