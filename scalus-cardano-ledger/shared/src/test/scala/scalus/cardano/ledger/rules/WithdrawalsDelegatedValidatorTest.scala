package scalus.cardano.ledger.rules

import org.scalatest.EitherValues
import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers
import scalus.cardano.address.{Network, StakeAddress, StakePayload}
import scalus.cardano.ledger.*

import scala.collection.immutable.SortedMap

/** The Conway LEDGER `validateWithdrawalsDelegated` check, spec [SC-23]. */
class WithdrawalsDelegatedValidatorTest extends AnyFunSuite with Matchers with EitherValues {

    private val cardanoInfo = CardanoInfo.preprod
    private val params = cardanoInfo.protocolParams
    private val keyDeposit = Coin(params.stakeAddressDeposit)

    private val stakeKeyHash = StakeKeyHash.fromHex("b" * 56)
    private val keyAccount =
        RewardAccount(StakeAddress(Network.Testnet, StakePayload.Stake(stakeKeyHash)))
    private val keyCredential = keyAccount.address.credential
    private val scriptAccount =
        RewardAccount(
          StakeAddress(Network.Testnet, StakePayload.Script(ScriptHash.fromHex("d" * 56)))
        )

    private def stateWith(accounts: (RewardAccount, Option[DRep])*): State = State(certState =
        CertState.empty.copy(dstate = DelegationState(accounts.map { (account, drep) =>
            account.address.credential -> ConwayAccountState(Coin.zero, keyDeposit, None, drep)
        }.toMap))
    )

    private def tx(account: RewardAccount, certs: Certificate*): Transaction =
        Transaction(
          TransactionBody(
            inputs = TaggedSortedSet.empty[TransactionInput],
            outputs = IndexedSeq.empty[Sized[TransactionOutput]],
            fee = Coin.zero,
            certificates =
                if certs.isEmpty then TaggedOrderedStrictSet.empty
                else TaggedOrderedStrictSet.from(certs),
            withdrawals = Some(Withdrawals(SortedMap(account -> Coin.zero)))
          )
        )

    private def context(major: Int): Context = Context(
      env = UtxoEnv(
        0,
        params.copy(protocolVersion = ProtocolVersion(major, 0)),
        CertState.empty,
        Network.Testnet,
        Coin.zero
      ),
      slotConfig = cardanoInfo.slotConfig
    )

    private def validate(state: State, transaction: Transaction, major: Int = 10) =
        WithdrawalsDelegatedValidator.validate(context(major), state, transaction)

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
          context(10),
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
