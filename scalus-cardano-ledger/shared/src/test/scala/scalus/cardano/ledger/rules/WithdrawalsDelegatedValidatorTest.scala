package scalus.cardano.ledger.rules

import org.scalatest.EitherValues
import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers
import scalus.cardano.address.{Network, StakeAddress, StakePayload}
import scalus.cardano.ledger.*

/** The Conway LEDGER `validateWithdrawalsDelegated` check, spec [SC-23]. */
class WithdrawalsDelegatedValidatorTest
    extends AnyFunSuite
    with Matchers
    with EitherValues
    with CertsTestKit {

    private val keyAccount = RewardAccount(
      StakeAddress(Network.Testnet, StakePayload.Stake(StakeKeyHash.fromHex("b" * 56)))
    )
    private val keyCredential = keyAccount.address.credential
    private val scriptAccount = scriptRewardAccount(ScriptHash.fromHex("d" * 56))

    private def stateWith(accounts: (RewardAccount, Option[DRep])*): State =
        State(certState = certState(accounts = accounts.map { (account, drep) =>
            account.address.credential -> accountState(Coin.zero, drep = drep)
        }.toMap))

    /** A transaction that withdraws zero from `account`. */
    private def tx(account: RewardAccount, certs: Certificate*): Transaction =
        certTx(Map(account -> Coin.zero), certs*)

    private def validate(state: State, transaction: Transaction, major: Int = 10) =
        WithdrawalsDelegatedValidator.validate(context(protocolMajor = major), state, transaction)

    test("rejects a withdrawal from a key account without a DRep") {
        val transaction = tx(keyAccount)
        validate(stateWith(keyAccount -> None), transaction) shouldBe Left(
          TransactionException.WithdrawalsNotDelegatedToDRepException(
            transaction.id,
            keyCredential.keyHashOption.toSet
          )
        )
    }

    test("rejects a withdrawal from an unregistered key account") {
        // Haskell finds no account state, so no DRep delegation either
        validate(State(), tx(keyAccount)).left.value shouldBe a[
          TransactionException.WithdrawalsNotDelegatedToDRepException
        ]
    }

    test("accepts a withdrawal from a key account delegated to a DRep") {
        val drep = DRep.KeyHash(AddrKeyHash.fromHex("c" * 56))
        validate(stateWith(keyAccount -> Some(drep)), tx(keyAccount)) shouldBe Right(())
        validate(stateWith(keyAccount -> Some(DRep.AlwaysAbstain)), tx(keyAccount)) shouldBe
            Right(())
    }

    test("accepts the withdraw-zero trick from a script account without a DRep") {
        validate(stateWith(scriptAccount -> None), tx(scriptAccount)) shouldBe Right(())
    }

    test("skips the check in the Conway bootstrap phase, protocol version 9") {
        validate(stateWith(keyAccount -> None), tx(keyAccount), major = 9) shouldBe Right(())
    }

    test("skips the check for a phase-2-invalid tx") {
        val transaction = tx(keyAccount).copy(isValid = false)
        validate(stateWith(keyAccount -> None), transaction) shouldBe Right(())
    }

    test("a DRep delegation in the same tx does not count") {
        // Haskell checks the cert state before the tx's certificates
        val transaction =
            tx(keyAccount, Certificate.VoteDelegCert(keyCredential, DRep.AlwaysAbstain))
        val result = STS.Mutator.transit[TransactionException](
          Seq(WithdrawalsDelegatedValidator),
          DefaultMutators.all,
          context(protocolMajor = 10),
          stateWith(keyAccount -> None),
          transaction
        )
        result.left.value shouldBe a[TransactionException.WithdrawalsNotDelegatedToDRepException]
    }

    test("is one of the default validators") {
        DefaultValidators.all should contain(WithdrawalsDelegatedValidator)
    }

    test("names its rule WithdrawalsNotDelegatedToDRep") {
        val error = TransactionException.WithdrawalsNotDelegatedToDRepException(
          tx(keyAccount).id,
          keyCredential.keyHashOption.toSet
        )
        TransactionException.ruleName(error) shouldBe "WithdrawalsNotDelegatedToDRep"
    }
}
