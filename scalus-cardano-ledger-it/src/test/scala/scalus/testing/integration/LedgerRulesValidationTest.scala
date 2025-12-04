package scalus.testing.integration

import org.scalatest.funsuite.AnyFunSuite
import scalus.cardano.ledger.rules.*
import scalus.cardano.ledger.*
import scalus.testing.integration.BlocksTestUtils.*

import java.nio.file.Files
import java.util.concurrent.atomic.AtomicInteger
import scala.util.Try
import scala.util.chaining.*

class LedgerRulesValidationTest extends AnyFunSuite {

    test("validate transactions") {
        val transactionsCount = AtomicInteger()

        val blocks = getAllBlocksPaths().take(1000)

        println(s"Validate ${blocks.size} blocks ...")

        val failed = for
            path <- blocks
            bytes = Files.readAllBytes(path)
            transaction <- BlockFile
                .fromCborArray(bytes)
                .block
                .transactions(using OriginalCborByteArray(bytes))
            utxos <- Try(scalusUtxoResolver.resolveUtxos(transaction)).toOption
            state = State(utxos = utxos)
            _ = transactionsCount.incrementAndGet()
            result <- CardanoMutator
                .transit(Context.testMainnet(), state, transaction)
                .swap
                .toOption
        yield (path.getFileName, transaction.id, result)
            .tap(x => println(pprint(x)))

        val groups = failed
            .groupBy {
                case (_, _, ex) => ex.getClass.getSimpleName
                case _          => "Unexpected success"
            }
            .toSeq
            .sortBy(-_._2.length)

        println(s"Transactions count: ${transactionsCount.get()}")
        println(s"Failed transactions: ${failed.size}")
        println(s"Failures rate: ${(100 * failed.size) / transactionsCount.get()}%")
        println(s"\nFailures by exception type:")
        groups.foreach { case (exType, failures) =>
            println(s"  $exType: ${failures.length}")
        }
    }
}
