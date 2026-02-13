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

        println(s"Transactions count: ${transactionsCount.get()}")
        println(s"UTXOs resolved count: ${utxosResolvedCount.get()}")
        println(s"Failed transactions: ${failed.size}")

        val (withdrawalErrors, otherErrors) = failed.partitionMap {
            case (path, tx, e: WithdrawalsNotInRewardsException) => Left((path, tx, e))
            case other => Right(other)
        }

        val (nonDrainingErrors, missingAccountErrors) = withdrawalErrors.partition {
            case (_, _, e) => e.missingRewardAccounts.isEmpty && e.nonDrainingWithdrawals.nonEmpty
        }

        val (stakeCertErrors, remainingErrors) = otherErrors.partitionMap {
            case (path, tx, e: StakeCertificatesException) => Left((path, tx, e))
            case other => Right(other)
        }
        val (valueNotConservedErrors, unexpectedErrors) = remainingErrors.partitionMap {
            case (path, tx, e: ValueNotConservedUTxOException) => Left((path, tx, e))
            case other => Right(other)
        }

        println(s"\nNon-draining withdrawals: ${nonDrainingErrors.size}")
        println(s"Missing account withdrawals: ${missingAccountErrors.size}")
        println(s"Stake certificate errors: ${stakeCertErrors.size}")
        println(s"Value not conserved errors: ${valueNotConservedErrors.size}")
        println(s"Unexpected errors: ${unexpectedErrors.size}")
        unexpectedErrors.foreach { case (path, tx, error) =>
            println(s"  ${tx.id.toHex} ($path): ${error.getClass.getSimpleName}: ${error.getMessage.takeWhile(_ != '\n')}")
        }

        // Non-draining withdrawal errors: StakeStateResolver.computeRewardBalanceAtEpoch sums all
        // historical rewards but doesn't subtract prior withdrawals, so the resolved reward balance
        // is too high and doesn't match the actual withdrawal amount.
        assert(
          nonDrainingErrors.size == 725,
          s"Expected 725 non-draining withdrawal errors, got ${nonDrainingErrors.size}"
        )
        assert(
          missingAccountErrors.isEmpty,
          s"Expected no missing-account withdrawal errors, got ${missingAccountErrors.size}"
        )
        // Stake certificate and value-not-conserved errors both stem from StakeStateResolver not
        // populating DelegationState.deposits accurately. StakeCertificatesValidator fails when
        // deposit/refund/registration state is wrong; ValueNotConservedUTxOValidator fails when
        // deregistration refunds (looked up from deposits) are missing, making consumed < produced.
        // Which validator fires first varies, so we assert on the combined total.
        val depositRelatedErrors = stakeCertErrors.size + valueNotConservedErrors.size
        assert(
          depositRelatedErrors == 94,
          s"Expected 94 deposit-related errors (stakeCert + valueNotConserved), got $depositRelatedErrors"
        )
        assert(
          unexpectedErrors.isEmpty,
          s"Expected no unexpected errors, got ${unexpectedErrors.size}"
        )
    }
}
