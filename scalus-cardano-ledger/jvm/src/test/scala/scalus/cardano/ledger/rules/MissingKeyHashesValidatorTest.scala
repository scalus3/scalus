package scalus.cardano.ledger
package rules

import org.scalacheck.Arbitrary
import scalus.builtin.platform
import scalus.cardano.address.{StakeAddress, StakePayload}
import org.scalatest.funsuite.AnyFunSuite
import scala.collection.immutable.SortedMap

class MissingKeyHashesValidatorTest extends AnyFunSuite, ValidatorRulesTestKit {
    test("MissingKeyHashesValidator Inputs rule success") {
        val context = Context()

        val (privateKey1, publicKey1) = generateKeyPair()
        val (privateKey2, publicKey2) = generateKeyPair()

        val input1 = Arbitrary.arbitrary[TransactionInput].sample.get
        val input2 = Arbitrary.arbitrary[TransactionInput].sample.get

        val transaction = {
            val tx = randomTransactionWithIsValidField
            tx.copy(
              body = KeepRaw(
                tx.body.value.copy(
                  inputs = TaggedSortedSet.from(Set(input1, input2)),
                  collateralInputs = TaggedSortedSet.empty,
                  referenceInputs = TaggedSortedSet.empty,
                  votingProcedures = None,
                  certificates = TaggedOrderedSet.empty,
                  withdrawals = None,
                  requiredSigners = TaggedSortedSet.empty
                )
              ),
              witnessSet = tx.witnessSet.copy(
                vkeyWitnesses = TaggedSortedSet(
                  VKeyWitness(publicKey1, platform.signEd25519(privateKey1, tx.id)),
                  VKeyWitness(publicKey2, platform.signEd25519(privateKey2, tx.id))
                )
              )
            )
        }

        val state = State(
          utxos = Map(
            input1 -> TransactionOutput(
              Arbitrary
                  .arbitrary[StakeAddress]
                  .sample
                  .get
                  .copy(payload = StakePayload.Stake(Hash(platform.blake2b_224(publicKey1)))),
              Value(Coin(1000000L))
            ),
            input2 -> TransactionOutput(
              Arbitrary
                  .arbitrary[StakeAddress]
                  .sample
                  .get
                  .copy(
                    payload = StakePayload.Stake(
                      Hash(platform.blake2b_224(publicKey2))
                    )
                  ),
              Value(Coin(1000000L))
            )
          )
        )

        val result = MissingKeyHashesValidator.validate(context, state, transaction)
        assert(result.isRight)
    }

    test("MissingKeyHashesValidator Inputs rule failure") {
        val context = Context()

        val (privateKey1, publicKey1) = generateKeyPair()
        val (privateKey2, publicKey2) = generateKeyPair()

        val input1 = Arbitrary.arbitrary[TransactionInput].sample.get
        val input2 = Arbitrary.arbitrary[TransactionInput].sample.get

        val transaction = {
            val tx = randomTransactionWithIsValidField
            tx.copy(
              body = KeepRaw(
                tx.body.value.copy(
                  inputs = TaggedSortedSet.from(Set(input1, input2)),
                  collateralInputs = TaggedSortedSet.empty,
                  referenceInputs = TaggedSortedSet.empty,
                  votingProcedures = None,
                  certificates = TaggedOrderedSet.empty,
                  withdrawals = None,
                  requiredSigners = TaggedSortedSet.empty
                )
              ),
              witnessSet = tx.witnessSet.copy(
                vkeyWitnesses = TaggedSortedSet(
                  VKeyWitness(publicKey1, platform.signEd25519(privateKey1, tx.id))
                )
              )
            )
        }

        val state = State(
          utxos = Map(
            input1 -> TransactionOutput(
              Arbitrary
                  .arbitrary[StakeAddress]
                  .sample
                  .get
                  .copy(
                    payload = StakePayload.Stake(
                      Hash(platform.blake2b_224(publicKey1))
                    )
                  ),
              Value(Coin(1000000L))
            ),
            input2 -> TransactionOutput(
              Arbitrary
                  .arbitrary[StakeAddress]
                  .sample
                  .get
                  .copy(
                    payload = StakePayload.Stake(
                      Hash(platform.blake2b_224(publicKey2))
                    )
                  ),
              Value(Coin(1000000L))
            )
          )
        )

        val result = MissingKeyHashesValidator.validate(context, state, transaction)
        assert(result.isLeft)
    }

    test("MissingKeyHashesValidator CollateralInputs rule success") {
        val context = Context()

        val (privateKey1, publicKey1) = generateKeyPair()
        val (privateKey2, publicKey2) = generateKeyPair()

        val collateralInput1 = Arbitrary.arbitrary[TransactionInput].sample.get
        val collateralInput2 = Arbitrary.arbitrary[TransactionInput].sample.get

        val transaction = {
            val tx = randomTransactionWithIsValidField
            tx.copy(
              body = KeepRaw(
                tx.body.value.copy(
                  inputs = TaggedSortedSet.empty,
                  collateralInputs = TaggedSortedSet.from(Set(collateralInput1, collateralInput2)),
                  referenceInputs = TaggedSortedSet.empty,
                  votingProcedures = None,
                  certificates = TaggedOrderedSet.empty,
                  withdrawals = None,
                  requiredSigners = TaggedSortedSet.empty
                )
              ),
              witnessSet = tx.witnessSet.copy(
                vkeyWitnesses = TaggedSortedSet(
                  VKeyWitness(publicKey1, platform.signEd25519(privateKey1, tx.id)),
                  VKeyWitness(publicKey2, platform.signEd25519(privateKey2, tx.id))
                )
              )
            )
        }

        val state = State(
          utxos = Map(
            collateralInput1 -> TransactionOutput(
              Arbitrary
                  .arbitrary[StakeAddress]
                  .sample
                  .get
                  .copy(
                    payload = StakePayload.Stake(
                      Hash(platform.blake2b_224(publicKey1))
                    )
                  ),
              Value(Coin(1000000L))
            ),
            collateralInput2 -> TransactionOutput(
              Arbitrary
                  .arbitrary[StakeAddress]
                  .sample
                  .get
                  .copy(
                    payload = StakePayload.Stake(
                      Hash(platform.blake2b_224(publicKey2))
                    )
                  ),
              Value(Coin(1000000L))
            )
          )
        )

        val result = MissingKeyHashesValidator.validate(context, state, transaction)
        assert(result.isRight)
    }

    test("MissingKeyHashesValidator CollateralInputs rule failure") {
        val context = Context()

        val (privateKey1, publicKey1) = generateKeyPair()
        val (privateKey2, publicKey2) = generateKeyPair()

        val collateralInput1 = Arbitrary.arbitrary[TransactionInput].sample.get
        val collateralInput2 = Arbitrary.arbitrary[TransactionInput].sample.get

        val transaction = {
            val tx = randomTransactionWithIsValidField
            tx.copy(
              body = KeepRaw(
                tx.body.value.copy(
                  inputs = TaggedSortedSet.empty,
                  collateralInputs = TaggedSortedSet.from(Set(collateralInput1, collateralInput2)),
                  referenceInputs = TaggedSortedSet.empty,
                  votingProcedures = None,
                  certificates = TaggedOrderedSet.empty,
                  withdrawals = None,
                  requiredSigners = TaggedSortedSet.empty
                )
              ),
              witnessSet = tx.witnessSet.copy(
                vkeyWitnesses = TaggedSortedSet(
                  VKeyWitness(publicKey1, platform.signEd25519(privateKey1, tx.id)),
                )
              )
            )
        }

        val state = State(
          utxos = Map(
            collateralInput1 -> TransactionOutput(
              Arbitrary
                  .arbitrary[StakeAddress]
                  .sample
                  .get
                  .copy(
                    payload = StakePayload.Stake(
                      Hash(platform.blake2b_224(publicKey1))
                    )
                  ),
              Value(Coin(1000000L))
            ),
            collateralInput2 -> TransactionOutput(
              Arbitrary
                  .arbitrary[StakeAddress]
                  .sample
                  .get
                  .copy(payload = StakePayload.Stake(Hash(platform.blake2b_224(publicKey2)))),
              Value(Coin(1000000L))
            )
          )
        )

        val result = MissingKeyHashesValidator.validate(context, state, transaction)
        assert(result.isLeft)
    }

    test("MissingKeyHashesValidator VotingProcedures success") {
        val context = Context()

        val (privateKey1, publicKey1) = generateKeyPair()
        val (privateKey2, publicKey2) = generateKeyPair()
        val (privateKey3, publicKey3) = generateKeyPair()

        val transaction = {
            val tx = randomTransactionWithIsValidField
            tx.copy(
              body = KeepRaw(
                tx.body.value.copy(
                  inputs = TaggedSortedSet.empty,
                  collateralInputs = TaggedSortedSet.empty,
                  referenceInputs = TaggedSortedSet.empty,
                  votingProcedures = Some(
                    VotingProcedures(
                      SortedMap(
                        Voter.ConstitutionalCommitteeHotKey(
                          Hash(platform.blake2b_224(publicKey1))
                        ) -> SortedMap.from(genMapOfSizeFromArbitrary(0, 4).sample.get),
                        Voter.StakingPoolKey(
                          Hash(platform.blake2b_224(publicKey2))
                        ) -> SortedMap.from(genMapOfSizeFromArbitrary(0, 4).sample.get),
                        Voter.DRepKey(
                          Hash(platform.blake2b_224(publicKey3))
                        ) -> SortedMap.from(genMapOfSizeFromArbitrary(0, 4).sample.get),
                        Voter.ConstitutionalCommitteeHotScript(
                          Arbitrary.arbitrary[ScriptHash].sample.get
                        ) -> SortedMap.from(genMapOfSizeFromArbitrary(0, 4).sample.get),
                        Voter.DRepScript(
                          Arbitrary.arbitrary[ScriptHash].sample.get
                        ) -> SortedMap.from(genMapOfSizeFromArbitrary(0, 4).sample.get)
                      )
                    )
                  ),
                  certificates = TaggedOrderedSet.empty,
                  withdrawals = None,
                  requiredSigners = TaggedSortedSet.empty
                )
              ),
              witnessSet = tx.witnessSet.copy(
                vkeyWitnesses = TaggedSortedSet(
                  VKeyWitness(publicKey1, platform.signEd25519(privateKey1, tx.id)),
                  VKeyWitness(publicKey2, platform.signEd25519(privateKey2, tx.id)),
                  VKeyWitness(publicKey3, platform.signEd25519(privateKey3, tx.id))
                )
              )
            )
        }

        val state = State()

        val result = MissingKeyHashesValidator.validate(context, state, transaction)
        assert(result.isRight)
    }

    test("MissingKeyHashesValidator VotingProcedures failure") {
        val context = Context()

        val (privateKey1, publicKey1) = generateKeyPair()
        val (privateKey2, publicKey2) = generateKeyPair()
        val (privateKey3, publicKey3) = generateKeyPair()

        val transaction = {
            val tx = randomTransactionWithIsValidField
            tx.copy(
              body = KeepRaw(
                tx.body.value.copy(
                  inputs = TaggedSortedSet.empty,
                  collateralInputs = TaggedSortedSet.empty,
                  referenceInputs = TaggedSortedSet.empty,
                  votingProcedures = Some(
                    VotingProcedures(
                      SortedMap(
                        Voter.ConstitutionalCommitteeHotKey(
                          Hash(platform.blake2b_224(publicKey1))
                        ) -> SortedMap.from(genMapOfSizeFromArbitrary(0, 4).sample.get),
                        Voter.StakingPoolKey(
                          Hash(platform.blake2b_224(publicKey2))
                        ) -> SortedMap.from(genMapOfSizeFromArbitrary(0, 4).sample.get),
                        Voter.DRepKey(
                          Hash(platform.blake2b_224(publicKey3))
                        ) -> SortedMap.from(genMapOfSizeFromArbitrary(0, 4).sample.get),
                        Voter.ConstitutionalCommitteeHotScript(
                          Arbitrary.arbitrary[ScriptHash].sample.get
                        ) -> SortedMap.from(genMapOfSizeFromArbitrary(0, 4).sample.get),
                        Voter.DRepScript(
                          Arbitrary.arbitrary[ScriptHash].sample.get
                        ) -> SortedMap.from(genMapOfSizeFromArbitrary(0, 4).sample.get)
                      )
                    )
                  ),
                  certificates = TaggedOrderedSet.empty,
                  withdrawals = None,
                  requiredSigners = TaggedSortedSet.empty
                )
              ),
              witnessSet = tx.witnessSet.copy(
                vkeyWitnesses = TaggedSortedSet(
                  VKeyWitness(publicKey1, platform.signEd25519(privateKey1, tx.id)),
                  VKeyWitness(publicKey2, platform.signEd25519(privateKey2, tx.id))
                )
              )
            )
        }

        val state = State()

        val result = MissingKeyHashesValidator.validate(context, state, transaction)
        assert(result.isLeft)
    }

    test("MissingKeyHashesValidator Withdrawals rule success") {
        val context = Context()

        val (privateKey1, publicKey1) = generateKeyPair()
        val (privateKey2, publicKey2) = generateKeyPair()

        val transaction = {
            val tx = randomTransactionWithIsValidField
            tx.copy(
              body = KeepRaw(
                tx.body.value.copy(
                  inputs = TaggedSortedSet.empty,
                  collateralInputs = TaggedSortedSet.empty,
                  referenceInputs = TaggedSortedSet.empty,
                  votingProcedures = None,
                  certificates = TaggedOrderedSet.empty,
                  withdrawals = Some(
                    Withdrawals(
                      SortedMap(
                        RewardAccount(
                          Arbitrary
                              .arbitrary[StakeAddress]
                              .sample
                              .get
                              .copy(
                                payload = StakePayload.Stake(
                                  Hash(platform.blake2b_224(publicKey1))
                                )
                              )
                        ) -> Coin(1000000L),
                        RewardAccount(
                          Arbitrary
                              .arbitrary[StakeAddress]
                              .sample
                              .get
                              .copy(
                                payload = StakePayload.Stake(
                                  Hash(platform.blake2b_224(publicKey2))
                                )
                              )
                        ) -> Coin(2000000L)
                      )
                    )
                  ),
                  requiredSigners = TaggedSortedSet.empty
                )
              ),
              witnessSet = tx.witnessSet.copy(
                vkeyWitnesses = TaggedSortedSet(
                  VKeyWitness(publicKey1, platform.signEd25519(privateKey1, tx.id)),
                  VKeyWitness(publicKey2, platform.signEd25519(privateKey2, tx.id))
                )
              )
            )
        }

        val state = State()

        val result = MissingKeyHashesValidator.validate(context, state, transaction)
        assert(result.isRight)
    }

    test("MissingKeyHashesValidator Withdrawals rule failure") {
        val context = Context()

        val (privateKey1, publicKey1) = generateKeyPair()
        val (privateKey2, publicKey2) = generateKeyPair()

        val transaction = {
            val tx = randomTransactionWithIsValidField
            tx.copy(
              body = KeepRaw(
                tx.body.value.copy(
                  inputs = TaggedSortedSet.empty,
                  collateralInputs = TaggedSortedSet.empty,
                  referenceInputs = TaggedSortedSet.empty,
                  votingProcedures = None,
                  certificates = TaggedOrderedSet.empty,
                  withdrawals = Some(
                    Withdrawals(
                      SortedMap(
                        RewardAccount(
                          Arbitrary
                              .arbitrary[StakeAddress]
                              .sample
                              .get
                              .copy(
                                payload = StakePayload.Stake(
                                  Hash(platform.blake2b_224(publicKey1))
                                )
                              )
                        ) -> Coin(1000000L),
                        RewardAccount(
                          Arbitrary
                              .arbitrary[StakeAddress]
                              .sample
                              .get
                              .copy(
                                payload = StakePayload.Stake(
                                  Hash(platform.blake2b_224(publicKey2))
                                )
                              )
                        ) -> Coin(2000000L)
                      )
                    )
                  ),
                  requiredSigners = TaggedSortedSet.empty
                )
              ),
              witnessSet = tx.witnessSet.copy(
                vkeyWitnesses = TaggedSortedSet(
                  VKeyWitness(publicKey1, platform.signEd25519(privateKey1, tx.id))
                )
              )
            )
        }

        val state = State()

        val result = MissingKeyHashesValidator.validate(context, state, transaction)
        assert(result.isLeft)
    }

    test("MissingKeyHashesValidator Certificates rule success") {
        val context = Context()

        val (privateKey, publicKey) = generateKeyPair()
        val credential = Credential.KeyHash(
          Hash(platform.blake2b_224(publicKey))
        )

        val transaction = {
            val tx = randomTransactionWithIsValidField
            tx.copy(
              body = KeepRaw(
                tx.body.value.copy(
                  inputs = TaggedSortedSet.empty,
                  collateralInputs = TaggedSortedSet.empty,
                  referenceInputs = TaggedSortedSet.empty,
                  votingProcedures = None,
                  certificates = TaggedOrderedSet(
                    Certificate
                        .StakeDelegation(credential, Arbitrary.arbitrary[PoolKeyHash].sample.get),
                    Certificate.PoolRegistration(
                      Hash(platform.blake2b_224(publicKey)),
                      Arbitrary.arbitrary[VrfKeyHash].sample.get,
                      Arbitrary.arbitrary[Coin].sample.get,
                      Arbitrary.arbitrary[Coin].sample.get,
                      Arbitrary.arbitrary[UnitInterval].sample.get,
                      Arbitrary.arbitrary[RewardAccount].sample.get,
                      Set(Hash(platform.blake2b_224(publicKey))),
                      Arbitrary.arbitrary[IndexedSeq[Relay]].sample.get,
                      Arbitrary.arbitrary[Option[PoolMetadata]].sample.get
                    ),
                    Certificate
                        .PoolRetirement(Hash(platform.blake2b_224(publicKey)), 1),
                    Certificate.RegCert(credential, Arbitrary.arbitrary[Option[Coin]].sample.get),
                    Certificate.UnregCert(credential, Arbitrary.arbitrary[Option[Coin]].sample.get),
                    Certificate.VoteDelegCert(credential, Arbitrary.arbitrary[DRep].sample.get),
                    Certificate.StakeVoteDelegCert(
                      credential,
                      Arbitrary.arbitrary[PoolKeyHash].sample.get,
                      Arbitrary.arbitrary[DRep].sample.get
                    ),
                    Certificate.StakeRegDelegCert(
                      credential,
                      Arbitrary.arbitrary[PoolKeyHash].sample.get,
                      Arbitrary.arbitrary[Coin].sample.get
                    ),
                    Certificate.VoteRegDelegCert(
                      credential,
                      Arbitrary.arbitrary[DRep].sample.get,
                      Arbitrary.arbitrary[Coin].sample.get
                    ),
                    Certificate.StakeVoteRegDelegCert(
                      credential,
                      Arbitrary.arbitrary[PoolKeyHash].sample.get,
                      Arbitrary.arbitrary[DRep].sample.get,
                      Arbitrary.arbitrary[Coin].sample.get
                    ),
                    Certificate.AuthCommitteeHotCert(
                      credential,
                      Arbitrary.arbitrary[Credential].sample.get
                    ),
                    Certificate.ResignCommitteeColdCert(
                      credential,
                      Arbitrary.arbitrary[Option[Anchor]].sample.get
                    ),
                    Certificate.RegDRepCert(
                      credential,
                      Arbitrary.arbitrary[Coin].sample.get,
                      Arbitrary.arbitrary[Option[Anchor]].sample.get
                    ),
                    Certificate.UnregDRepCert(credential, Arbitrary.arbitrary[Coin].sample.get),
                    Certificate.UpdateDRepCert(
                      credential,
                      Arbitrary.arbitrary[Option[Anchor]].sample.get
                    )
                  ),
                  withdrawals = None,
                  requiredSigners = TaggedSortedSet.empty
                )
              ),
              witnessSet = tx.witnessSet.copy(
                vkeyWitnesses = TaggedSortedSet(
                  VKeyWitness(publicKey, platform.signEd25519(privateKey, tx.id))
                )
              )
            )
        }

        val state = State()

        val result = MissingKeyHashesValidator.validate(context, state, transaction)
        assert(result.isRight)
    }

    test("MissingKeyHashesValidator Certificates rule failure") {
        val context = Context()

        val (privateKey, publicKey) = generateKeyPair()
        val credential = Credential.KeyHash(
          Hash(platform.blake2b_224(publicKey))
        )

        val transaction = {
            val tx = randomTransactionWithIsValidField
            tx.copy(
              body = KeepRaw(
                tx.body.value.copy(
                  inputs = TaggedSortedSet.empty,
                  collateralInputs = TaggedSortedSet.empty,
                  referenceInputs = TaggedSortedSet.empty,
                  votingProcedures = None,
                  certificates = TaggedOrderedSet(
                    Certificate
                        .StakeDelegation(credential, Arbitrary.arbitrary[PoolKeyHash].sample.get),
                    Certificate.PoolRegistration(
                      Hash(platform.blake2b_224(publicKey)),
                      Arbitrary.arbitrary[VrfKeyHash].sample.get,
                      Arbitrary.arbitrary[Coin].sample.get,
                      Arbitrary.arbitrary[Coin].sample.get,
                      Arbitrary.arbitrary[UnitInterval].sample.get,
                      Arbitrary.arbitrary[RewardAccount].sample.get,
                      Set(Hash(platform.blake2b_224(publicKey))),
                      Arbitrary.arbitrary[IndexedSeq[Relay]].sample.get,
                      Arbitrary.arbitrary[Option[PoolMetadata]].sample.get
                    ),
                    Certificate
                        .PoolRetirement(Hash(platform.blake2b_224(publicKey)), 1),
                    Certificate.RegCert(credential, Arbitrary.arbitrary[Option[Coin]].sample.get),
                    Certificate.UnregCert(credential, Arbitrary.arbitrary[Option[Coin]].sample.get),
                    Certificate.VoteDelegCert(credential, Arbitrary.arbitrary[DRep].sample.get),
                    Certificate.StakeVoteDelegCert(
                      credential,
                      Arbitrary.arbitrary[PoolKeyHash].sample.get,
                      Arbitrary.arbitrary[DRep].sample.get
                    ),
                    Certificate.StakeRegDelegCert(
                      credential,
                      Arbitrary.arbitrary[PoolKeyHash].sample.get,
                      Arbitrary.arbitrary[Coin].sample.get
                    ),
                    Certificate.VoteRegDelegCert(
                      credential,
                      Arbitrary.arbitrary[DRep].sample.get,
                      Arbitrary.arbitrary[Coin].sample.get
                    ),
                    Certificate.StakeVoteRegDelegCert(
                      credential,
                      Arbitrary.arbitrary[PoolKeyHash].sample.get,
                      Arbitrary.arbitrary[DRep].sample.get,
                      Arbitrary.arbitrary[Coin].sample.get
                    ),
                    Certificate.AuthCommitteeHotCert(
                      credential,
                      Arbitrary.arbitrary[Credential].sample.get
                    ),
                    Certificate.ResignCommitteeColdCert(
                      credential,
                      Arbitrary.arbitrary[Option[Anchor]].sample.get
                    ),
                    Certificate.RegDRepCert(
                      credential,
                      Arbitrary.arbitrary[Coin].sample.get,
                      Arbitrary.arbitrary[Option[Anchor]].sample.get
                    ),
                    Certificate.UnregDRepCert(credential, Arbitrary.arbitrary[Coin].sample.get),
                    Certificate.UpdateDRepCert(
                      credential,
                      Arbitrary.arbitrary[Option[Anchor]].sample.get
                    )
                  ),
                  withdrawals = None,
                  requiredSigners = TaggedSortedSet.empty
                )
              ),
              witnessSet = tx.witnessSet.copy(
                vkeyWitnesses = TaggedSortedSet.empty
              )
            )
        }

        val state = State()

        val result = MissingKeyHashesValidator.validate(context, state, transaction)
        assert(result.isLeft)
    }

    test("MissingKeyHashesValidator RequiredSigners rule success") {
        val context = Context()

        val (privateKey1, publicKey1) = generateKeyPair()
        val (privateKey2, publicKey2) = generateKeyPair()

        val transaction = {
            val tx = randomTransactionWithIsValidField
            tx.copy(
              body = KeepRaw(
                tx.body.value.copy(
                  inputs = TaggedSortedSet.empty,
                  collateralInputs = TaggedSortedSet.empty,
                  referenceInputs = TaggedSortedSet.empty,
                  votingProcedures = None,
                  certificates = TaggedOrderedSet.empty,
                  withdrawals = None,
                  requiredSigners = TaggedSortedSet.from(
                    Set(
                      Hash(platform.blake2b_224(publicKey1)),
                      Hash(platform.blake2b_224(publicKey2))
                    )
                  )
                )
              ),
              witnessSet = tx.witnessSet.copy(
                vkeyWitnesses = TaggedSortedSet(
                  VKeyWitness(publicKey1, platform.signEd25519(privateKey1, tx.id)),
                  VKeyWitness(publicKey2, platform.signEd25519(privateKey2, tx.id))
                )
              )
            )
        }

        val state = State()

        val result = MissingKeyHashesValidator.validate(context, state, transaction)
        assert(result.isRight)
    }

    test("MissingKeyHashesValidator RequiredSigners rule failure") {
        val context = Context()

        val (privateKey1, publicKey1) = generateKeyPair()
        val (privateKey2, publicKey2) = generateKeyPair()

        val transaction = {
            val tx = randomTransactionWithIsValidField
            tx.copy(
              body = KeepRaw(
                tx.body.value.copy(
                  inputs = TaggedSortedSet.empty,
                  collateralInputs = TaggedSortedSet.empty,
                  referenceInputs = TaggedSortedSet.empty,
                  votingProcedures = None,
                  certificates = TaggedOrderedSet.empty,
                  withdrawals = None,
                  requiredSigners = TaggedSortedSet.from(
                    Set(
                      Hash(platform.blake2b_224(publicKey1)),
                      Hash(platform.blake2b_224(publicKey2))
                    )
                  )
                )
              ),
              witnessSet = tx.witnessSet.copy(
                vkeyWitnesses = TaggedSortedSet(
                  VKeyWitness(publicKey1, platform.signEd25519(privateKey1, tx.id))
                )
              )
            )
        }

        val state = State()

        val result = MissingKeyHashesValidator.validate(context, state, transaction)
        assert(result.isLeft)
    }
}
