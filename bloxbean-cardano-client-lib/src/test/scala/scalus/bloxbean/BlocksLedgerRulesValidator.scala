package scalus.bloxbean

import java.nio.file.{Files, Paths}
import scala.jdk.CollectionConverters.*
import scalus.cardano.ledger.*
import scalus.cardano.ledger.rules.*
import org.scalatest.funsuite.AnyFunSuite

class BlocksLedgerRulesValidator(
    val rules: Map[String, STS],
    val context: Context = Context.testMainnet(),
    val state: State = State()
) extends AnyFunSuite {
    import BlocksLedgerRulesValidator.utxoResolver

    def this(rule: STS) = this(Map(rule.name -> rule))

    test("check commited blocks") {
        val url = getClass.getResource("/blocks/")
        val dirPath = Paths.get(url.toURI)

        val files = Files.list(dirPath).iterator().asScala.toList
        for
            (ruleName, rule) <- rules
            file <- files
        do
            val bytes = Files.readAllBytes(file)
            given OriginalCborByteArray = OriginalCborByteArray(bytes)
            val block = BlockFile.fromCborArray(bytes).block
            for transaction <- block.transactions
            do
                try
                    val utxo = utxoResolver.resolveUtxos(transaction)
//                    val utxo = bloxbeanResolveUtxo(transaction)

                    val result = rule(context, state.copy(utxos = state.utxos ++ utxo), transaction)
                    assert(
                      result.isRight,
                      s"Transaction ${transaction.id} in block ${block.header.blockNumber} failed $ruleName with error: ${result.swap.getOrElse("")}"
                    )

//                    println(
//                      s"Transaction ${transaction.id} in block ${block.header.blockNumber} passed ${sts.getClass.getSimpleName}"
//                    )

                catch
                    case e: IllegalStateException
                        if e.getMessage.startsWith("UTXO not found for input") =>

//                        println(
//                          s"Skipping transaction ${transaction.id} in block ${block.header.blockNumber} due to missing UTXO"
//                        )
    }
}

object BlocksLedgerRulesValidator {
    private val utxoResolver = ResourcesUtxoResolver()
}
