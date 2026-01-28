package scalus.cardano.ledger.rules

import org.scalacheck.Arbitrary
import org.scalatest.funsuite.AnyFunSuite
import scalus.cardano.address.{StakeAddress, StakePayload}
import scalus.cardano.ledger.*
import scalus.cardano.node.TestEmulatorFactory
import scalus.uplc.builtin.platform

import scala.collection.immutable.SortedMap

class MissingKeyHashesValidatorTest extends AnyFunSuite with ArbitraryInstances {

    test(
      "MissingKeyHashesValidator success for no inputs, collateralInputs, votingProcedures, certificates, withdrawals, requiredSigners"
    ) {
        val transaction = Transaction(
          TransactionBody(
            inputs = TaggedSortedSet.empty,
            collateralInputs = TaggedSortedSet.empty,
            votingProcedures = None,
            certificates = TaggedOrderedStrictSet.empty,
            withdrawals = None,
            requiredSigners = TaggedSortedSet.empty,
            outputs = IndexedSeq.empty,
            fee = Coin.zero
          )
        )

        val emulator = TestEmulatorFactory.create(
          validators = Seq(MissingKeyHashesValidator),
          mutators = Seq.empty
        )
        val result = emulator.submitSync(transaction)
        assert(result.isRight)
    }

    test("MissingKeyHashesValidator Inputs rule success") {
        val (privateKey1, publicKey1) = TestEmulatorFactory.generateKeyPair()
        val (privateKey2, publicKey2) = TestEmulatorFactory.generateKeyPair()

        val input1 = Arbitrary.arbitrary[TransactionInput].sample.get
        val input2 = Arbitrary.arbitrary[TransactionInput].sample.get

        val transaction = {
            val tx = randomTransactionWithIsValidField
            tx.withWitness(
              _.copy(
                vkeyWitnesses = TaggedSortedSet(
                  VKeyWitness(publicKey1, platform.signEd25519(privateKey1, tx.id)),
                  VKeyWitness(publicKey2, platform.signEd25519(privateKey2, tx.id))
                )
              )
            ).copy(
              body = KeepRaw(
                tx.body.value.copy(
                  inputs = TaggedSortedSet.from(Set(input1, input2)),
                  collateralInputs = TaggedSortedSet.empty,
                  referenceInputs = TaggedSortedSet.empty,
                  votingProcedures = None,
                  certificates = TaggedOrderedStrictSet.empty,
                  withdrawals = None,
                  requiredSigners = TaggedSortedSet.empty
                )
              )
            )
        }

        val utxos = Map(
          input1 -> Output(
            Arbitrary
                .arbitrary[StakeAddress]
                .sample
                .get
                .copy(payload = StakePayload.Stake(Hash(platform.blake2b_224(publicKey1)))),
            Value(Coin(1000000L))
          ),
          input2 -> Output(
            Arbitrary
                .arbitrary[StakeAddress]
                .sample
                .get
                .copy(payload = StakePayload.Stake(Hash(platform.blake2b_224(publicKey2)))),
            Value(Coin(1000000L))
          )
        )

        val emulator = TestEmulatorFactory.create(
          utxos = utxos,
          validators = Seq(MissingKeyHashesValidator),
          mutators = Seq.empty
        )
        val result = emulator.submitSync(transaction)
        assert(result.isRight)
    }

    test("MissingKeyHashesValidator Inputs rule failure") {
        val (privateKey1, publicKey1) = TestEmulatorFactory.generateKeyPair()
        val (_, publicKey2) = TestEmulatorFactory.generateKeyPair()

        val input1 = Arbitrary.arbitrary[TransactionInput].sample.get
        val input2 = Arbitrary.arbitrary[TransactionInput].sample.get

        val transaction = {
            val tx = randomTransactionWithIsValidField
            tx.withWitness(
              _.copy(
                vkeyWitnesses = TaggedSortedSet(
                  VKeyWitness(publicKey1, platform.signEd25519(privateKey1, tx.id))
                )
              )
            ).copy(
              body = KeepRaw(
                tx.body.value.copy(
                  inputs = TaggedSortedSet.from(Set(input1, input2)),
                  collateralInputs = TaggedSortedSet.empty,
                  referenceInputs = TaggedSortedSet.empty,
                  votingProcedures = None,
                  certificates = TaggedOrderedStrictSet.empty,
                  withdrawals = None,
                  requiredSigners = TaggedSortedSet.empty
                )
              )
            )
        }

        val utxos = Map(
          input1 -> Output(
            Arbitrary
                .arbitrary[StakeAddress]
                .sample
                .get
                .copy(payload = StakePayload.Stake(Hash(platform.blake2b_224(publicKey1)))),
            Value(Coin(1000000L))
          ),
          input2 -> Output(
            Arbitrary
                .arbitrary[StakeAddress]
                .sample
                .get
                .copy(payload = StakePayload.Stake(Hash(platform.blake2b_224(publicKey2)))),
            Value(Coin(1000000L))
          )
        )

        val emulator = TestEmulatorFactory.create(
          utxos = utxos,
          validators = Seq(MissingKeyHashesValidator),
          mutators = Seq.empty
        )
        val result = emulator.submitSync(transaction)
        assert(result.isLeft)
    }

    test("MissingKeyHashesValidator CollateralInputs rule success") {
        val (privateKey1, publicKey1) = TestEmulatorFactory.generateKeyPair()
        val (privateKey2, publicKey2) = TestEmulatorFactory.generateKeyPair()

        val collateralInput1 = Arbitrary.arbitrary[TransactionInput].sample.get
        val collateralInput2 = Arbitrary.arbitrary[TransactionInput].sample.get

        val transaction = {
            val tx = randomTransactionWithIsValidField
            tx.withWitness(
              _.copy(
                vkeyWitnesses = TaggedSortedSet(
                  VKeyWitness(publicKey1, platform.signEd25519(privateKey1, tx.id)),
                  VKeyWitness(publicKey2, platform.signEd25519(privateKey2, tx.id))
                )
              )
            ).copy(
              body = KeepRaw(
                tx.body.value.copy(
                  inputs = TaggedSortedSet.empty,
                  collateralInputs = TaggedSortedSet.from(Set(collateralInput1, collateralInput2)),
                  referenceInputs = TaggedSortedSet.empty,
                  votingProcedures = None,
                  certificates = TaggedOrderedStrictSet.empty,
                  withdrawals = None,
                  requiredSigners = TaggedSortedSet.empty
                )
              )
            )
        }

        val utxos = Map(
          collateralInput1 -> Output(
            Arbitrary
                .arbitrary[StakeAddress]
                .sample
                .get
                .copy(payload = StakePayload.Stake(Hash(platform.blake2b_224(publicKey1)))),
            Value(Coin(1000000L))
          ),
          collateralInput2 -> Output(
            Arbitrary
                .arbitrary[StakeAddress]
                .sample
                .get
                .copy(payload = StakePayload.Stake(Hash(platform.blake2b_224(publicKey2)))),
            Value(Coin(1000000L))
          )
        )

        val emulator = TestEmulatorFactory.create(
          utxos = utxos,
          validators = Seq(MissingKeyHashesValidator),
          mutators = Seq.empty
        )
        val result = emulator.submitSync(transaction)
        assert(result.isRight)
    }

    test("MissingKeyHashesValidator CollateralInputs rule failure") {
        val (privateKey1, publicKey1) = TestEmulatorFactory.generateKeyPair()
        val (_, publicKey2) = TestEmulatorFactory.generateKeyPair()

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
                  certificates = TaggedOrderedStrictSet.empty,
                  withdrawals = None,
                  requiredSigners = TaggedSortedSet.empty
                )
              ),
              witnessSetRaw = KeepRaw(
                tx.witnessSet.copy(
                  vkeyWitnesses = TaggedSortedSet(
                    VKeyWitness(publicKey1, platform.signEd25519(privateKey1, tx.id))
                  )
                )
              )
            )
        }

        val utxos = Map(
          collateralInput1 -> Output(
            Arbitrary
                .arbitrary[StakeAddress]
                .sample
                .get
                .copy(payload = StakePayload.Stake(Hash(platform.blake2b_224(publicKey1)))),
            Value(Coin(1000000L))
          ),
          collateralInput2 -> Output(
            Arbitrary
                .arbitrary[StakeAddress]
                .sample
                .get
                .copy(payload = StakePayload.Stake(Hash(platform.blake2b_224(publicKey2)))),
            Value(Coin(1000000L))
          )
        )

        val emulator = TestEmulatorFactory.create(
          utxos = utxos,
          validators = Seq(MissingKeyHashesValidator),
          mutators = Seq.empty
        )
        val result = emulator.submitSync(transaction)
        assert(result.isLeft)
    }

    test("MissingKeyHashesValidator VotingProcedures success") {
        val (privateKey1, publicKey1) = TestEmulatorFactory.generateKeyPair()
        val (privateKey2, publicKey2) = TestEmulatorFactory.generateKeyPair()
        val (privateKey3, publicKey3) = TestEmulatorFactory.generateKeyPair()

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
                  certificates = TaggedOrderedStrictSet.empty,
                  withdrawals = None,
                  requiredSigners = TaggedSortedSet.empty
                )
              ),
              witnessSetRaw = KeepRaw(
                tx.witnessSet.copy(
                  vkeyWitnesses = TaggedSortedSet(
                    VKeyWitness(publicKey1, platform.signEd25519(privateKey1, tx.id)),
                    VKeyWitness(publicKey2, platform.signEd25519(privateKey2, tx.id)),
                    VKeyWitness(publicKey3, platform.signEd25519(privateKey3, tx.id))
                  )
                )
              )
            )
        }

        val emulator = TestEmulatorFactory.create(
          validators = Seq(MissingKeyHashesValidator),
          mutators = Seq.empty
        )
        val result = emulator.submitSync(transaction)
        assert(result.isRight)
    }

    test("MissingKeyHashesValidator VotingProcedures failure") {
        val (privateKey1, publicKey1) = TestEmulatorFactory.generateKeyPair()
        val (privateKey2, publicKey2) = TestEmulatorFactory.generateKeyPair()
        val (_, publicKey3) = TestEmulatorFactory.generateKeyPair()

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
                  certificates = TaggedOrderedStrictSet.empty,
                  withdrawals = None,
                  requiredSigners = TaggedSortedSet.empty
                )
              ),
              witnessSetRaw = KeepRaw(
                tx.witnessSet.copy(
                  vkeyWitnesses = TaggedSortedSet(
                    VKeyWitness(publicKey1, platform.signEd25519(privateKey1, tx.id)),
                    VKeyWitness(publicKey2, platform.signEd25519(privateKey2, tx.id))
                  )
                )
              )
            )
        }

        val emulator = TestEmulatorFactory.create(
          validators = Seq(MissingKeyHashesValidator),
          mutators = Seq.empty
        )
        val result = emulator.submitSync(transaction)
        assert(result.isLeft)
    }

    test("MissingKeyHashesValidator Withdrawals rule success") {
        val (privateKey1, publicKey1) = TestEmulatorFactory.generateKeyPair()
        val (privateKey2, publicKey2) = TestEmulatorFactory.generateKeyPair()

        val transaction = {
            val tx = randomTransactionWithIsValidField
            tx.copy(
              body = KeepRaw(
                tx.body.value.copy(
                  inputs = TaggedSortedSet.empty,
                  collateralInputs = TaggedSortedSet.empty,
                  referenceInputs = TaggedSortedSet.empty,
                  votingProcedures = None,
                  certificates = TaggedOrderedStrictSet.empty,
                  withdrawals = Some(
                    Withdrawals(
                      SortedMap(
                        RewardAccount(
                          Arbitrary
                              .arbitrary[StakeAddress]
                              .sample
                              .get
                              .copy(payload =
                                  StakePayload.Stake(Hash(platform.blake2b_224(publicKey1)))
                              )
                        ) -> Coin(1000000L),
                        RewardAccount(
                          Arbitrary
                              .arbitrary[StakeAddress]
                              .sample
                              .get
                              .copy(payload =
                                  StakePayload.Stake(Hash(platform.blake2b_224(publicKey2)))
                              )
                        ) -> Coin(2000000L)
                      )
                    )
                  ),
                  requiredSigners = TaggedSortedSet.empty
                )
              ),
              witnessSetRaw = KeepRaw(
                tx.witnessSet.copy(
                  vkeyWitnesses = TaggedSortedSet(
                    VKeyWitness(publicKey1, platform.signEd25519(privateKey1, tx.id)),
                    VKeyWitness(publicKey2, platform.signEd25519(privateKey2, tx.id))
                  )
                )
              )
            )
        }

        val emulator = TestEmulatorFactory.create(
          validators = Seq(MissingKeyHashesValidator),
          mutators = Seq.empty
        )
        val result = emulator.submitSync(transaction)
        assert(result.isRight)
    }

    test("MissingKeyHashesValidator Withdrawals rule failure") {
        val (privateKey1, publicKey1) = TestEmulatorFactory.generateKeyPair()
        val (_, publicKey2) = TestEmulatorFactory.generateKeyPair()

        val transaction = {
            val tx = randomTransactionWithIsValidField
            tx.copy(
              body = KeepRaw(
                tx.body.value.copy(
                  inputs = TaggedSortedSet.empty,
                  collateralInputs = TaggedSortedSet.empty,
                  referenceInputs = TaggedSortedSet.empty,
                  votingProcedures = None,
                  certificates = TaggedOrderedStrictSet.empty,
                  withdrawals = Some(
                    Withdrawals(
                      SortedMap(
                        RewardAccount(
                          Arbitrary
                              .arbitrary[StakeAddress]
                              .sample
                              .get
                              .copy(payload =
                                  StakePayload.Stake(Hash(platform.blake2b_224(publicKey1)))
                              )
                        ) -> Coin(1000000L),
                        RewardAccount(
                          Arbitrary
                              .arbitrary[StakeAddress]
                              .sample
                              .get
                              .copy(payload =
                                  StakePayload.Stake(Hash(platform.blake2b_224(publicKey2)))
                              )
                        ) -> Coin(2000000L)
                      )
                    )
                  ),
                  requiredSigners = TaggedSortedSet.empty
                )
              ),
              witnessSetRaw = KeepRaw(
                tx.witnessSet.copy(
                  vkeyWitnesses = TaggedSortedSet(
                    VKeyWitness(publicKey1, platform.signEd25519(privateKey1, tx.id))
                  )
                )
              )
            )
        }

        val emulator = TestEmulatorFactory.create(
          validators = Seq(MissingKeyHashesValidator),
          mutators = Seq.empty
        )
        val result = emulator.submitSync(transaction)
        assert(result.isLeft)
    }

    test("MissingKeyHashesValidator Certificates rule success") {
        val (privateKey, publicKey) = TestEmulatorFactory.generateKeyPair()
        val credential = Credential.KeyHash(Hash(platform.blake2b_224(publicKey)))

        val transaction = {
            val tx = randomTransactionWithIsValidField
            tx.copy(
              body = KeepRaw(
                tx.body.value.copy(
                  inputs = TaggedSortedSet.empty,
                  collateralInputs = TaggedSortedSet.empty,
                  referenceInputs = TaggedSortedSet.empty,
                  votingProcedures = None,
                  certificates = TaggedOrderedStrictSet(
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
                    Certificate.PoolRetirement(Hash(platform.blake2b_224(publicKey)), 1),
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
              witnessSetRaw = KeepRaw(
                tx.witnessSet.copy(
                  vkeyWitnesses = TaggedSortedSet(
                    VKeyWitness(publicKey, platform.signEd25519(privateKey, tx.id))
                  )
                )
              )
            )
        }

        val emulator = TestEmulatorFactory.create(
          validators = Seq(MissingKeyHashesValidator),
          mutators = Seq.empty
        )
        val result = emulator.submitSync(transaction)
        assert(result.isRight)
    }

    test("MissingKeyHashesValidator Certificates rule failure") {
        val (_, publicKey) = TestEmulatorFactory.generateKeyPair()
        val credential = Credential.KeyHash(Hash(platform.blake2b_224(publicKey)))

        val transaction = {
            val tx = randomTransactionWithIsValidField
            tx.copy(
              body = KeepRaw(
                tx.body.value.copy(
                  inputs = TaggedSortedSet.empty,
                  collateralInputs = TaggedSortedSet.empty,
                  referenceInputs = TaggedSortedSet.empty,
                  votingProcedures = None,
                  certificates = TaggedOrderedStrictSet(
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
                    Certificate.PoolRetirement(Hash(platform.blake2b_224(publicKey)), 1),
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
              witnessSetRaw = KeepRaw(
                tx.witnessSet.copy(
                  vkeyWitnesses = TaggedSortedSet.empty
                )
              )
            )
        }

        val emulator = TestEmulatorFactory.create(
          validators = Seq(MissingKeyHashesValidator),
          mutators = Seq.empty
        )
        val result = emulator.submitSync(transaction)
        assert(result.isLeft)
    }

    test("MissingKeyHashesValidator RequiredSigners rule success") {
        val (privateKey1, publicKey1) = TestEmulatorFactory.generateKeyPair()
        val (privateKey2, publicKey2) = TestEmulatorFactory.generateKeyPair()

        val transaction = {
            val tx = randomTransactionWithIsValidField
            tx.copy(
              body = KeepRaw(
                tx.body.value.copy(
                  inputs = TaggedSortedSet.empty,
                  collateralInputs = TaggedSortedSet.empty,
                  referenceInputs = TaggedSortedSet.empty,
                  votingProcedures = None,
                  certificates = TaggedOrderedStrictSet.empty,
                  withdrawals = None,
                  requiredSigners = TaggedSortedSet.from(
                    Set(
                      Hash(platform.blake2b_224(publicKey1)),
                      Hash(platform.blake2b_224(publicKey2))
                    )
                  )
                )
              ),
              witnessSetRaw = KeepRaw(
                tx.witnessSet.copy(
                  vkeyWitnesses = TaggedSortedSet(
                    VKeyWitness(publicKey1, platform.signEd25519(privateKey1, tx.id)),
                    VKeyWitness(publicKey2, platform.signEd25519(privateKey2, tx.id))
                  )
                )
              )
            )
        }

        val emulator = TestEmulatorFactory.create(
          validators = Seq(MissingKeyHashesValidator),
          mutators = Seq.empty
        )
        val result = emulator.submitSync(transaction)
        assert(result.isRight)
    }

    test("MissingKeyHashesValidator RequiredSigners rule failure") {
        val (privateKey1, publicKey1) = TestEmulatorFactory.generateKeyPair()
        val (_, publicKey2) = TestEmulatorFactory.generateKeyPair()

        val transaction = {
            val tx = randomTransactionWithIsValidField
            tx.copy(
              body = KeepRaw(
                tx.body.value.copy(
                  inputs = TaggedSortedSet.empty,
                  collateralInputs = TaggedSortedSet.empty,
                  referenceInputs = TaggedSortedSet.empty,
                  votingProcedures = None,
                  certificates = TaggedOrderedStrictSet.empty,
                  withdrawals = None,
                  requiredSigners = TaggedSortedSet.from(
                    Set(
                      Hash(platform.blake2b_224(publicKey1)),
                      Hash(platform.blake2b_224(publicKey2))
                    )
                  )
                )
              ),
              witnessSetRaw = KeepRaw(
                tx.witnessSet.copy(
                  vkeyWitnesses = TaggedSortedSet(
                    VKeyWitness(publicKey1, platform.signEd25519(privateKey1, tx.id))
                  )
                )
              )
            )
        }

        val emulator = TestEmulatorFactory.create(
          validators = Seq(MissingKeyHashesValidator),
          mutators = Seq.empty
        )
        val result = emulator.submitSync(transaction)
        assert(result.isLeft)
    }
}
