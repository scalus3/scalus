package scalus.testing.integration

import org.scalatest.funsuite.AnyFunSuite
import scalus.cardano.ledger.*
import scalus.cardano.ledger.rules.*
import scalus.testing.integration.BlocksTestUtils.*

import java.nio.file.Files
import java.util.concurrent.atomic.AtomicInteger
import scala.util.Try
import scala.util.chaining.*

class LedgerRulesValidationTest extends AnyFunSuite {

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
            state = State(utxos = utxos)
            result <- CardanoMutator
                .transit(Context.testMainnet(block.slot), state, transaction)
                .swap
                .toOption
        yield (path.getFileName, transaction.id, result)
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
    }
}
