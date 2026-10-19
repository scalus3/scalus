package scalus.cardano.ledger.rules

import org.scalatest.funsuite.AnyFunSuite
import scalus.cardano.address.{Network, StakeAddress, StakePayload}
import scalus.cardano.ledger.*

/** The deposit check of the Conway GOV rule: each proposal deposit equals `govActionDeposit`. */
class ProposalDepositValidatorTest extends AnyFunSuite {

    private val cardanoInfo = CardanoInfo.preprod
    private val govActionDeposit = Coin(cardanoInfo.protocolParams.govActionDeposit)

    private val context = Context(
      env = UtxoEnv(0, cardanoInfo.protocolParams, CertState.empty, Network.Testnet, Coin.zero),
      slotConfig = cardanoInfo.slotConfig
    )

    private val rewardAccount =
        RewardAccount(
          StakeAddress(Network.Testnet, StakePayload.Stake(StakeKeyHash.fromHex("b" * 56)))
        )

    /** Distinct anchors keep equal proposals apart in the `TaggedOrderedSet`. */
    private def proposal(deposit: Coin, index: Int): ProposalProcedure = ProposalProcedure(
      deposit = deposit,
      rewardAccount = rewardAccount,
      govAction = GovAction.InfoAction,
      anchor = Anchor(s"https://example.org/$index", DataHash.fromHex("00" * 32))
    )

    private def tx(deposits: Coin*): Transaction =
        Transaction(
          TransactionBody(
            inputs = TaggedSortedSet.empty[TransactionInput],
            outputs = IndexedSeq.empty[Sized[TransactionOutput]],
            fee = Coin.zero,
            proposalProcedures = TaggedOrderedSet.from(deposits.zipWithIndex.map(proposal))
          )
        )

    private def validate(transaction: Transaction) =
        ProposalDepositValidator.validate(context, State(), transaction)

    test("accepts a tx without proposals") {
        assert(validate(tx()) == Right(()))
    }

    test("accepts proposals whose deposits equal govActionDeposit") {
        assert(validate(tx(govActionDeposit, govActionDeposit)) == Right(()))
    }

    test("rejects a proposal whose deposit differs from govActionDeposit") {
        val transaction = tx(govActionDeposit, Coin.ada(1))
        assert(
          validate(transaction) == Left(
            TransactionException.ProposalDepositIncorrectException(
              transaction.id,
              supplied = Coin.ada(1),
              expected = govActionDeposit
            )
          )
        )
    }

    test("skips the check for a phase-2-invalid tx") {
        // Conway LEDGER runs GOV only when isValid is true
        val transaction = tx(Coin.ada(1)).copy(isValid = false)
        assert(validate(transaction) == Right(()))
    }

    test("is one of the default validators") {
        assert(DefaultValidators.all.contains(ProposalDepositValidator))
    }

    test("names its rule ProposalDepositIncorrect") {
        val error = TransactionException.ProposalDepositIncorrectException(
          tx().id,
          supplied = Coin(1),
          expected = Coin(2)
        )
        assert(TransactionException.ruleName(error) == "ProposalDepositIncorrect")
    }
}
