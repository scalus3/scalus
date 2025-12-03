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

        val failures = for
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

        println(s"Failed transactions: ${failures.size}")

    }
}
