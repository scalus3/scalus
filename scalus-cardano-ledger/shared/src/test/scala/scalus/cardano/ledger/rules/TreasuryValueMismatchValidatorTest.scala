package scalus.cardano.ledger.rules

import org.scalatest.funsuite.AnyFunSuite
import scalus.cardano.address.Network
import scalus.cardano.ledger.*

import scala.annotation.nowarn

/** The Conway LEDGER `validateTreasuryValue` check, spec [SC-7], [SC-7e]. */
class TreasuryValueMismatchValidatorTest extends AnyFunSuite {

    private val cardanoInfo = CardanoInfo.preprod

    private def context(treasury: Coin): Context = Context(
      env = UtxoEnv(0, cardanoInfo.protocolParams, CertState.empty, Network.Testnet, treasury),
      slotConfig = cardanoInfo.slotConfig
    )

    private def tx(currentTreasuryValue: Option[Coin]): Transaction =
        Transaction(
          TransactionBody(
            inputs = TaggedSortedSet.empty[TransactionInput],
            outputs = IndexedSeq.empty[Sized[TransactionOutput]],
            fee = Coin.zero,
            currentTreasuryValue = currentTreasuryValue
          )
        )

    private def validate(treasury: Coin, transaction: Transaction) =
        TreasuryValueMismatchValidator.validate(context(treasury), State(), transaction)

    test("accepts a tx that states no treasury value") {
        assert(validate(Coin(1000), tx(None)) == Right(()))
    }

    test("accepts a tx whose treasury value equals the env treasury") {
        assert(validate(Coin(1000), tx(Some(Coin(1000)))) == Right(()))
    }

    test("rejects a tx whose treasury value differs from the env treasury") {
        val transaction = tx(Some(Coin(1005)))
        assert(
          validate(Coin(1000), transaction) == Left(
            TransactionException.TreasuryValueMismatchException(
              transaction.id,
              supplied = Coin(1005),
              expected = Coin(1000)
            )
          )
        )
    }

    test("skips the check for a phase-2-invalid tx") {
        // spec [SC-7e]: Conway LEDGER checks the treasury value only when isValid is true
        val transaction = tx(Some(Coin(1005))).copy(isValid = false)
        assert(validate(Coin(1000), transaction) == Right(()))
    }

    test("is one of the default validators") {
        assert(DefaultValidators.all.contains(TreasuryValueMismatchValidator))
    }

    test("names its rule TreasuryValueMismatch") {
        val error = TransactionException.TreasuryValueMismatchException(
          tx(None).id,
          supplied = Coin(1),
          expected = Coin(2)
        )
        assert(TransactionException.ruleName(error) == "TreasuryValueMismatch")
    }

    // 1.3 binary call sites construct `UtxoEnv` with 4 arguments
    @nowarn("cat=deprecation")
    private def oldPositional =
        new UtxoEnv(7, cardanoInfo.protocolParams, CertState.empty, Network.Testnet)

    @nowarn("cat=deprecation")
    private def oldNamed = new UtxoEnv(
      network = Network.Testnet,
      slot = 7,
      certState = CertState.empty,
      params = cardanoInfo.protocolParams
    )

    test("the deprecated 4-argument constructor sets an empty treasury") {
        val expected =
            UtxoEnv(7, cardanoInfo.protocolParams, CertState.empty, Network.Testnet, Coin.zero)
        assert(oldPositional == expected)
        assert(oldNamed == expected)
    }
}
