package scalus.testing.integration

import org.scalatest.funsuite.AnyFunSuite
import scalus.cardano.ledger.{OriginalCborByteArray, *}
import scalus.cardano.ledger.rules.*
import scalus.testing.integration.BlocksTestUtils.*

import java.nio.file.Files
import scala.util.Try
import scala.util.chaining.*

class LedgerRulesValidationTest extends AnyFunSuite {

    test("validate transactions") {

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

        println(s"Failed transactions: ${failed.size}")
        println(s"\nFailures by exception type:")
        groups.foreach { case (exType, failures) =>
            println(s"  $exType: ${failures.length}")
        }
    }
}
