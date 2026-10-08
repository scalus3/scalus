package scalus.testing.integration

import org.scalatest.funsuite.AnyFunSuite
import scalus.cardano.ledger.*
import scalus.cardano.ledger.rules.*
import scalus.testing.integration.BlocksTestUtils.*
import scalus.bloxbean.StakeStateResolver

import java.nio.file.Files
import java.util.concurrent.atomic.AtomicInteger
import scala.util.Try

class LedgerRulesValidationTest extends AnyFunSuite {

    private lazy val stakeStateResolver =
        StakeStateResolver(apiKey, resourcesPath.resolve("stake"))

    /** The it-data blocks were produced around epoch 544 (Plomin), so they must be validated with
      * the protocol parameters of that era: the current mainnet params carry the van Rossem cost
      * models, which change the language view (script integrity hash) and execution costs.
      */
    private lazy val blocksEraParams = ProtocolParams.fromBlockfrostJson(
      getClass.getResourceAsStream("/blockfrost-params-epoch-544.json")
    )

    /** The context of a tx at `slot`, with the treasury at the start of its epoch. */
    private def blocksEraContext(slot: SlotNo): Context = {
        val treasury = stakeStateResolver.treasuryAt(SlotConfig.mainnet.epochOf(slot).toInt)
        Context(env =
            UtxoEnv(slot, blocksEraParams, CertState.empty, scalus.cardano.address.Network.Mainnet, treasury)
        )
    }

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
            certState = stakeStateResolver.resolveForTx(transaction, block.slot)
            state = State(utxos = utxos, certState = certState)
            result <- CardanoMutator
                .transit(blocksEraContext(block.slot), state, transaction)
                .swap
                .toOption
        yield (path.getFileName, transaction, result)

        println(s"Transactions count: ${transactionsCount.get()}")
        println(s"UTXOs resolved count: ${utxosResolvedCount.get()}")
        println(s"Failed transactions: ${failed.size}")
        failed.foreach { case (path, tx, error) =>
            println(s"  ${tx.id.toHex} ($path): ${error.getClass.getSimpleName}: ${error.getMessage.replace('\n', ' ')}")
        }

        // Mainnet blocks hold only valid transactions, so with the stake state each saw, from
        // StakeStateResolver.resolveForTx(tx, slot), and the treasury of its epoch, every one passes.
        assert(failed.isEmpty, s"Expected no failed transactions, got ${failed.size}")
    }
}
