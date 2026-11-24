package scalus.cardano.ledger
package rules

import org.scalatest.funsuite.AnyFunSuite
import scalus.cardano.address.{Network, StakeAddress, StakePayload}
import scalus.testing.kit.Party
import scalus.testing.kit.Party.{Alice, Bob, Charles}

import scala.collection.immutable.SortedMap

class MissingKeyHashesValidatorTest extends AnyFunSuite, ValidatorRulesTestKit {

    /** Adds a vkey witness of each party. The rule reads only the key hashes of the witnesses, but
      * each party signs the transaction id, as a real witness does.
      */
    private def signedBy(transaction: Transaction, parties: Party*): Transaction =
        parties.foldLeft(transaction)((t, party) => party.signer.sign(t))

    // The outputs stay at stake addresses (F-35 is out of scope): the rule takes the key hash of
    // any address, so the stake key hash of `party` is the hash that must have a witness.
    private def stakeUtxo(
        input: TransactionInput,
        party: Party
    ): (TransactionInput, TransactionOutput) =
        input -> Output(
          StakeAddress(Network.Testnet, StakePayload.Stake(Hash.stakeKeyHash(party.addrKeyHash))),
          Value(Coin(1000000L))
        )

    // The rule reads only the voters, so each voter casts no vote.
    private def votingProcedures(
        committee: AddrKeyHash,
        pool: AddrKeyHash,
        drep: AddrKeyHash
    ): VotingProcedures = {
        val noVotes = SortedMap.empty[GovActionId, VotingProcedure]
        VotingProcedures(
          SortedMap(
            Voter.ConstitutionalCommitteeHotKey(committee) -> noVotes,
            Voter.StakingPoolKey(pool) -> noVotes,
            Voter.DRepKey(drep) -> noVotes,
            Voter.ConstitutionalCommitteeHotScript(scriptHash(1)) -> noVotes,
            Voter.DRepScript(scriptHash(2)) -> noVotes
          )
        )
    }

    private def withdrawals(parties: Party*): Withdrawals =
        Withdrawals(
          SortedMap.from(parties.zipWithIndex.map { (party, i) =>
              RewardAccount(
                StakeAddress(
                  Network.Testnet,
                  StakePayload.Stake(Hash.stakeKeyHash(party.addrKeyHash))
                )
              ) -> Coin(1000000L * (i + 1))
          })
        )

    // One certificate of each kind, each with the key of `party` where the rule takes a key hash.
    // The registrations carry a deposit: a `RegCert` without one needs no witness.
    private def certificates(party: Party): TaggedOrderedStrictSet[Certificate] = {
        val keyHash = party.addrKeyHash
        val credential = Credential.KeyHash(keyHash)
        val drep = DRep.KeyHash(addrKeyHash(2))
        val deposit = Coin.ada(2)
        TaggedOrderedStrictSet(
          Certificate.StakeDelegation(credential, poolKeyHash(1)),
          Certificate.PoolRegistration(
            keyHash,
            VrfKeyHash.fromHex("a" * 64),
            Coin.ada(1),
            Coin.ada(340),
            UnitInterval.one,
            rewardAccount(1),
            Set(keyHash),
            IndexedSeq.empty,
            None
          ),
          Certificate.PoolRetirement(PoolKeyHash.fromByteString(keyHash), 1),
          Certificate.RegCert(credential, Some(deposit)),
          Certificate.UnregCert(credential, Some(deposit)),
          Certificate.VoteDelegCert(credential, drep),
          Certificate.StakeVoteDelegCert(credential, poolKeyHash(1), drep),
          Certificate.StakeRegDelegCert(credential, poolKeyHash(1), deposit),
          Certificate.VoteRegDelegCert(credential, drep, deposit),
          Certificate.StakeVoteRegDelegCert(credential, poolKeyHash(1), drep, deposit),
          Certificate.AuthCommitteeHotCert(credential, Credential.KeyHash(addrKeyHash(3))),
          Certificate.ResignCommitteeColdCert(credential, None),
          Certificate.RegDRepCert(credential, deposit, None),
          Certificate.UnregDRepCert(credential, deposit),
          Certificate.UpdateDRepCert(credential, None)
        )
    }

    test(
      "MissingKeyHashesValidator success for no inputs, collateralInputs, votingProcedures, certificates, withdrawals, requiredSigners"
    ) {
        val transaction = Transaction(emptyBody)

        val result = validate(MissingKeyHashesValidator, Map.empty, transaction)
        assert(result.isRight)
    }

    test("MissingKeyHashesValidator Inputs rule success") {
        val unsigned = txSpending(input(1), input(2))(TransactionWitnessSet.empty)
        val transaction = signedBy(unsigned, Alice, Bob)
        val utxos = Map(stakeUtxo(input(1), Alice), stakeUtxo(input(2), Bob))

        val result = validate(MissingKeyHashesValidator, utxos, transaction)
        assert(result.isRight)
    }

    test("MissingKeyHashesValidator Inputs rule failure") {
        val unsigned = txSpending(input(1), input(2))(TransactionWitnessSet.empty)
        val transaction = signedBy(unsigned, Alice)
        val utxos = Map(stakeUtxo(input(1), Alice), stakeUtxo(input(2), Bob))

        val result = validate(MissingKeyHashesValidator, utxos, transaction)
        assert(result.isLeft)
    }

    test("MissingKeyHashesValidator CollateralInputs rule success") {
        val unsigned = Transaction(
          emptyBody.copy(collateralInputs = TaggedSortedSet(input(1), input(2)))
        )
        val transaction = signedBy(unsigned, Alice, Bob)
        val utxos = Map(stakeUtxo(input(1), Alice), stakeUtxo(input(2), Bob))

        val result = validate(MissingKeyHashesValidator, utxos, transaction)
        assert(result.isRight)
    }

    test("MissingKeyHashesValidator CollateralInputs rule failure") {
        val unsigned = Transaction(
          emptyBody.copy(collateralInputs = TaggedSortedSet(input(1), input(2)))
        )
        val transaction = signedBy(unsigned, Alice)
        val utxos = Map(stakeUtxo(input(1), Alice), stakeUtxo(input(2), Bob))

        val result = validate(MissingKeyHashesValidator, utxos, transaction)
        assert(result.isLeft)
    }

    test("MissingKeyHashesValidator VotingProcedures success") {
        val unsigned = Transaction(
          emptyBody.copy(votingProcedures =
              Some(votingProcedures(Alice.addrKeyHash, Bob.addrKeyHash, Charles.addrKeyHash))
          )
        )
        val transaction = signedBy(unsigned, Alice, Bob, Charles)

        val result = validate(MissingKeyHashesValidator, Map.empty, transaction)
        assert(result.isRight)
    }

    test("MissingKeyHashesValidator VotingProcedures failure") {
        val unsigned = Transaction(
          emptyBody.copy(votingProcedures =
              Some(votingProcedures(Alice.addrKeyHash, Bob.addrKeyHash, Charles.addrKeyHash))
          )
        )
        val transaction = signedBy(unsigned, Alice, Bob)

        val result = validate(MissingKeyHashesValidator, Map.empty, transaction)
        assert(result.isLeft)
    }

    test("MissingKeyHashesValidator Withdrawals rule success") {
        val unsigned = Transaction(emptyBody.copy(withdrawals = Some(withdrawals(Alice, Bob))))
        val transaction = signedBy(unsigned, Alice, Bob)

        val result = validate(MissingKeyHashesValidator, Map.empty, transaction)
        assert(result.isRight)
    }

    test("MissingKeyHashesValidator Withdrawals rule failure") {
        val unsigned = Transaction(emptyBody.copy(withdrawals = Some(withdrawals(Alice, Bob))))
        val transaction = signedBy(unsigned, Alice)

        val result = validate(MissingKeyHashesValidator, Map.empty, transaction)
        assert(result.isLeft)
    }

    test("MissingKeyHashesValidator Certificates rule success") {
        val unsigned = Transaction(emptyBody.copy(certificates = certificates(Alice)))
        val transaction = signedBy(unsigned, Alice)

        val result = validate(MissingKeyHashesValidator, Map.empty, transaction)
        assert(result.isRight)
    }

    test("MissingKeyHashesValidator Certificates rule failure") {
        val transaction = Transaction(emptyBody.copy(certificates = certificates(Alice)))

        val result = validate(MissingKeyHashesValidator, Map.empty, transaction)
        assert(result.isLeft)
    }

    test("MissingKeyHashesValidator RequiredSigners rule success") {
        val unsigned = Transaction(
          emptyBody.copy(requiredSigners = TaggedSortedSet(Alice.addrKeyHash, Bob.addrKeyHash))
        )
        val transaction = signedBy(unsigned, Alice, Bob)

        val result = validate(MissingKeyHashesValidator, Map.empty, transaction)
        assert(result.isRight)
    }

    test("MissingKeyHashesValidator RequiredSigners rule failure") {
        val unsigned = Transaction(
          emptyBody.copy(requiredSigners = TaggedSortedSet(Alice.addrKeyHash, Bob.addrKeyHash))
        )
        val transaction = signedBy(unsigned, Alice)

        val result = validate(MissingKeyHashesValidator, Map.empty, transaction)
        assert(result.isLeft)
    }
}
