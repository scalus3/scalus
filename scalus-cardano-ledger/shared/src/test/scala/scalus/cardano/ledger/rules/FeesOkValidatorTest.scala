package scalus.cardano.ledger.rules

import org.scalacheck.{Arbitrary, Gen}
import org.scalatest.funsuite.AnyFunSuite
import scalus.cardano.address.{ShelleyAddress, ShelleyPaymentPart}
import scalus.cardano.ledger.*
import scalus.cardano.node.TestEmulatorFactory
import scalus.uplc.builtin.platform

class FeesOkValidatorTest extends AnyFunSuite with ArbitraryInstances {
    test("FeesOkValidator rule success") {
        given Arbitrary[scalus.uplc.builtin.Data] = Arbitrary(
          Gen.const(scalus.uplc.builtin.Data.unit) // Simplified for testing
        )

        val (_, publicKey1) = TestEmulatorFactory.generateKeyPair()
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
                  collateralReturnOutput = Some(
                    Sized(
                      Output(
                        Arbitrary.arbitrary[ShelleyAddress].sample.get,
                        Value(Coin(20000000L), Arbitrary.arbitrary[MultiAsset].sample.get)
                      )
                    )
                  ),
                  // totalCollateral = net collateral (inputs - return)
                  // 30000000 + 30000000 - 20000000 = 40000000
                  totalCollateral = Some(Coin(40000000L)),
                  fee = Coin(10000000L),
                  referenceInputs = TaggedSortedSet.empty,
                  outputs = IndexedSeq.empty,
                  mint = None,
                  votingProcedures = None,
                  withdrawals = None,
                  proposalProcedures = TaggedOrderedSet.empty,
                  certificates = TaggedOrderedStrictSet.empty,
                  requiredSigners = TaggedSortedSet.empty
                )
              ),
              auxiliaryData = None,
              witnessSetRaw = KeepRaw(
                tx.witnessSet.copy(
                  vkeyWitnesses = TaggedSortedSet.empty,
                  bootstrapWitnesses = TaggedSortedSet.empty,
                  nativeScripts = TaggedSortedMap.empty,
                  plutusV1Scripts = TaggedSortedStrictMap.empty,
                  plutusV2Scripts = TaggedSortedStrictMap.empty,
                  plutusV3Scripts = TaggedSortedStrictMap.empty,
                  plutusData = KeepRaw(TaggedSortedMap.empty),
                  redeemers = Some(
                    KeepRaw(
                      Redeemers.Array(
                        IndexedSeq(
                          Redeemer(
                            tag = Arbitrary.arbitrary[RedeemerTag].sample.get,
                            index = Gen.choose(0, Int.MaxValue).sample.get,
                            data = scalus.uplc.builtin.Data.unit,
                            exUnits = Arbitrary.arbitrary[ExUnits].sample.get
                          )
                        )
                      )
                    )
                  )
                )
              )
            )
        }

        val utxos = Map(
          collateralInput1 -> Output(
            Arbitrary
                .arbitrary[ShelleyAddress]
                .sample
                .get
                .copy(payment =
                    ShelleyPaymentPart.Key(
                      Hash(platform.blake2b_224(publicKey1))
                    )
                ),
            Value(Coin(30000000L))
          ),
          collateralInput2 -> Output(
            Arbitrary
                .arbitrary[ShelleyAddress]
                .sample
                .get
                .copy(payment =
                    ShelleyPaymentPart.Key(
                      Hash(platform.blake2b_224(publicKey2))
                    )
                ),
            Value(
              Coin(30000000L),
              transaction.body.value.collateralReturnOutput
                  .map { _.value.value.assets }
                  .getOrElse(MultiAsset.empty)
            )
          )
        )

        val emulator = TestEmulatorFactory.create(
          utxos = utxos,
          validators = Seq(FeesOkValidator),
          mutators = Seq.empty
        )
        val result = emulator.submitSync(transaction)
        assert(result.isRight)
    }

    test(
      "FeesOkValidator If the total ExUnits are 0 in both Memory and Steps, no further part needs to be checked"
    ) {
        given Arbitrary[scalus.uplc.builtin.Data] = Arbitrary(
          Gen.const(scalus.uplc.builtin.Data.unit) // Simplified for testing
        )

        val transaction = {
            val tx = randomTransactionWithIsValidField
            tx.copy(
              body = KeepRaw(
                tx.body.value.copy(
                  inputs = TaggedSortedSet.empty,
                  collateralInputs = TaggedSortedSet.empty,
                  collateralReturnOutput = None,
                  totalCollateral = None,
                  fee = Coin(10000000L),
                  referenceInputs = TaggedSortedSet.empty,
                  outputs = IndexedSeq.empty,
                  mint = None,
                  votingProcedures = None,
                  withdrawals = None,
                  proposalProcedures = TaggedOrderedSet.empty,
                  certificates = TaggedOrderedStrictSet.empty,
                  requiredSigners = TaggedSortedSet.empty
                )
              ),
              auxiliaryData = None,
              witnessSetRaw = KeepRaw(
                tx.witnessSet.copy(
                  vkeyWitnesses = TaggedSortedSet.empty,
                  bootstrapWitnesses = TaggedSortedSet.empty,
                  nativeScripts = TaggedSortedMap.empty,
                  plutusV1Scripts = TaggedSortedStrictMap.empty,
                  plutusV2Scripts = TaggedSortedStrictMap.empty,
                  plutusV3Scripts = TaggedSortedStrictMap.empty,
                  plutusData = KeepRaw(TaggedSortedMap.empty),
                  redeemers = None
                )
              )
            )
        }

        val emulator = TestEmulatorFactory.create(
          validators = Seq(FeesOkValidator),
          mutators = Seq.empty
        )
        val result = emulator.submitSync(transaction)
        assert(result.isRight)
    }

    test("FeesOkValidator feePaidIsGreeterOrEqualThanMinimumFee rule failure") {
        given Arbitrary[scalus.uplc.builtin.Data] = Arbitrary(
          Gen.const(scalus.uplc.builtin.Data.unit) // Simplified for testing
        )

        val (_, publicKey1) = TestEmulatorFactory.generateKeyPair()
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
                  collateralReturnOutput = Some(
                    Sized(
                      Output(
                        Arbitrary.arbitrary[ShelleyAddress].sample.get,
                        Value(Coin(20000000L))
                      )
                    )
                  ),
                  totalCollateral = Some(Coin(60000000L)),
                  fee = Coin(1L),
                  referenceInputs = TaggedSortedSet.empty,
                  outputs = IndexedSeq.empty,
                  mint = None,
                  votingProcedures = None,
                  withdrawals = None,
                  proposalProcedures = TaggedOrderedSet.empty,
                  certificates = TaggedOrderedStrictSet.empty,
                  requiredSigners = TaggedSortedSet.empty
                )
              ),
              auxiliaryData = None,
              witnessSetRaw = KeepRaw(
                tx.witnessSet.copy(
                  vkeyWitnesses = TaggedSortedSet.empty,
                  bootstrapWitnesses = TaggedSortedSet.empty,
                  nativeScripts = TaggedSortedMap.empty,
                  plutusV1Scripts = TaggedSortedStrictMap.empty,
                  plutusV2Scripts = TaggedSortedStrictMap.empty,
                  plutusV3Scripts = TaggedSortedStrictMap.empty,
                  plutusData = KeepRaw(TaggedSortedMap.empty),
                  redeemers = Some(
                    KeepRaw(
                      Redeemers.Array(
                        IndexedSeq(
                          Redeemer(
                            tag = Arbitrary.arbitrary[RedeemerTag].sample.get,
                            index = Gen.choose(0, Int.MaxValue).sample.get,
                            data = scalus.uplc.builtin.Data.unit,
                            exUnits = Arbitrary.arbitrary[ExUnits].sample.get
                          )
                        )
                      )
                    )
                  )
                )
              )
            )
        }

        val utxos = Map(
          collateralInput1 -> Output(
            Arbitrary
                .arbitrary[ShelleyAddress]
                .sample
                .get
                .copy(payment =
                    ShelleyPaymentPart.Key(
                      Hash(platform.blake2b_224(publicKey1))
                    )
                ),
            Value(Coin(30000000L))
          ),
          collateralInput2 -> Output(
            Arbitrary
                .arbitrary[ShelleyAddress]
                .sample
                .get
                .copy(payment =
                    ShelleyPaymentPart.Key(
                      Hash(platform.blake2b_224(publicKey2))
                    )
                ),
            Value(Coin(30000000L))
          )
        )

        val emulator = TestEmulatorFactory.create(
          utxos = utxos,
          validators = Seq(FeesOkValidator),
          mutators = Seq.empty
        )
        val result = emulator.submitSync(transaction)
        assert(result.isLeft)
    }

    test("FeesOkValidator collateralConsistsOnlyOfVKeyAddress rule failure") {
        given Arbitrary[scalus.uplc.builtin.Data] = Arbitrary(
          Gen.const(scalus.uplc.builtin.Data.unit) // Simplified for testing
        )

        val (_, publicKey1) = TestEmulatorFactory.generateKeyPair()

        val collateralInput1 = Arbitrary.arbitrary[TransactionInput].sample.get
        val collateralInput2 = Arbitrary.arbitrary[TransactionInput].sample.get

        val transaction = {
            val tx = randomTransactionWithIsValidField
            tx.copy(
              body = KeepRaw(
                tx.body.value.copy(
                  inputs = TaggedSortedSet.empty,
                  collateralInputs = TaggedSortedSet.from(Set(collateralInput1, collateralInput2)),
                  collateralReturnOutput = None,
                  totalCollateral = None,
                  fee = Coin(10000000L),
                  referenceInputs = TaggedSortedSet.empty,
                  outputs = IndexedSeq.empty,
                  mint = None,
                  votingProcedures = None,
                  withdrawals = None,
                  proposalProcedures = TaggedOrderedSet.empty,
                  certificates = TaggedOrderedStrictSet.empty,
                  requiredSigners = TaggedSortedSet.empty
                )
              ),
              auxiliaryData = None,
              witnessSetRaw = KeepRaw(
                tx.witnessSet.copy(
                  vkeyWitnesses = TaggedSortedSet.empty,
                  bootstrapWitnesses = TaggedSortedSet.empty,
                  nativeScripts = TaggedSortedMap.empty,
                  plutusV1Scripts = TaggedSortedStrictMap.empty,
                  plutusV2Scripts = TaggedSortedStrictMap.empty,
                  plutusV3Scripts = TaggedSortedStrictMap.empty,
                  plutusData = KeepRaw(TaggedSortedMap.empty),
                  redeemers = Some(
                    KeepRaw(
                      Redeemers.Array(
                        IndexedSeq(
                          Redeemer(
                            tag = Arbitrary.arbitrary[RedeemerTag].sample.get,
                            index = Gen.choose(0, Int.MaxValue).sample.get,
                            data = scalus.uplc.builtin.Data.unit,
                            exUnits = Arbitrary.arbitrary[ExUnits].sample.get
                          )
                        )
                      )
                    )
                  )
                )
              )
            )
        }

        val utxos = Map(
          collateralInput1 -> Output(
            Arbitrary
                .arbitrary[ShelleyAddress]
                .sample
                .get
                .copy(payment =
                    ShelleyPaymentPart.Key(
                      Hash(platform.blake2b_224(publicKey1))
                    )
                ),
            Value(Coin(20000000L))
          ),
          collateralInput2 -> Output(
            Arbitrary
                .arbitrary[ShelleyAddress]
                .sample
                .get
                .copy(payment =
                    ShelleyPaymentPart.Script(
                      Arbitrary.arbitrary[ScriptHash].sample.get
                    )
                ),
            Value(Coin(20000000L))
          )
        )

        val emulator = TestEmulatorFactory.create(
          utxos = utxos,
          validators = Seq(FeesOkValidator),
          mutators = Seq.empty
        )
        val result = emulator.submitSync(transaction)
        assert(result.isLeft)
    }

    test(
      "FeesOkValidator collateralDoesNotContainAnyNonADA rule failure when collateralReturnOutput has more tokens than collaterals"
    ) {
        given Arbitrary[scalus.uplc.builtin.Data] = Arbitrary(
          Gen.const(scalus.uplc.builtin.Data.unit) // Simplified for testing
        )

        val (_, publicKey1) = TestEmulatorFactory.generateKeyPair()
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
                  collateralReturnOutput = Some(
                    Sized(
                      Output(
                        Arbitrary.arbitrary[ShelleyAddress].sample.get,
                        Value(
                          Coin(20000000L),
                          genMultiAsset(
                            minPolicies = 1,
                            maxPolicies = 4,
                            minAssets = 1,
                            maxAssets = 4
                          ).sample.get
                        )
                      )
                    )
                  ),
                  totalCollateral = Some(Coin(60000000L)),
                  fee = Coin(10000000L),
                  referenceInputs = TaggedSortedSet.empty,
                  outputs = IndexedSeq.empty,
                  mint = None,
                  votingProcedures = None,
                  withdrawals = None,
                  proposalProcedures = TaggedOrderedSet.empty,
                  certificates = TaggedOrderedStrictSet.empty,
                  requiredSigners = TaggedSortedSet.empty
                )
              ),
              auxiliaryData = None,
              witnessSetRaw = KeepRaw(
                tx.witnessSet.copy(
                  vkeyWitnesses = TaggedSortedSet.empty,
                  bootstrapWitnesses = TaggedSortedSet.empty,
                  nativeScripts = TaggedSortedMap.empty,
                  plutusV1Scripts = TaggedSortedStrictMap.empty,
                  plutusV2Scripts = TaggedSortedStrictMap.empty,
                  plutusV3Scripts = TaggedSortedStrictMap.empty,
                  plutusData = KeepRaw(TaggedSortedMap.empty),
                  redeemers = Some(
                    KeepRaw(
                      Redeemers.Array(
                        IndexedSeq(
                          Redeemer(
                            tag = Arbitrary.arbitrary[RedeemerTag].sample.get,
                            index = Gen.choose(0, Int.MaxValue).sample.get,
                            data = scalus.uplc.builtin.Data.unit,
                            exUnits = Arbitrary.arbitrary[ExUnits].sample.get
                          )
                        )
                      )
                    )
                  )
                )
              )
            )
        }

        val utxos = Map(
          collateralInput1 -> Output(
            Arbitrary
                .arbitrary[ShelleyAddress]
                .sample
                .get
                .copy(payment =
                    ShelleyPaymentPart.Key(
                      Hash(platform.blake2b_224(publicKey1))
                    )
                ),
            Value(Coin(30000000L))
          ),
          collateralInput2 -> Output(
            Arbitrary
                .arbitrary[ShelleyAddress]
                .sample
                .get
                .copy(payment =
                    ShelleyPaymentPart.Key(
                      Hash(platform.blake2b_224(publicKey2))
                    )
                ),
            Value(Coin(30000000L))
          )
        )

        val emulator = TestEmulatorFactory.create(
          utxos = utxos,
          validators = Seq(FeesOkValidator),
          mutators = Seq.empty
        )
        val result = emulator.submitSync(transaction)
        assert(result.isLeft)
    }

    test(
      "FeesOkValidator collateralDoesNotContainAnyNonADA rule failure when collateralReturnOutput has less tokens than collaterals"
    ) {
        given Arbitrary[scalus.uplc.builtin.Data] = Arbitrary(
          Gen.const(scalus.uplc.builtin.Data.unit) // Simplified for testing
        )

        val (_, publicKey1) = TestEmulatorFactory.generateKeyPair()
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
                  collateralReturnOutput = Some(
                    Sized(
                      Output(
                        Arbitrary.arbitrary[ShelleyAddress].sample.get,
                        Value(
                          Coin(20000000L)
                        )
                      )
                    )
                  ),
                  totalCollateral = Some(Coin(60000000L)),
                  fee = Coin(10000000L),
                  referenceInputs = TaggedSortedSet.empty,
                  outputs = IndexedSeq.empty,
                  mint = None,
                  votingProcedures = None,
                  withdrawals = None,
                  proposalProcedures = TaggedOrderedSet.empty,
                  certificates = TaggedOrderedStrictSet.empty,
                  requiredSigners = TaggedSortedSet.empty
                )
              ),
              auxiliaryData = None,
              witnessSetRaw = KeepRaw(
                tx.witnessSet.copy(
                  vkeyWitnesses = TaggedSortedSet.empty,
                  bootstrapWitnesses = TaggedSortedSet.empty,
                  nativeScripts = TaggedSortedMap.empty,
                  plutusV1Scripts = TaggedSortedStrictMap.empty,
                  plutusV2Scripts = TaggedSortedStrictMap.empty,
                  plutusV3Scripts = TaggedSortedStrictMap.empty,
                  plutusData = KeepRaw(TaggedSortedMap.empty),
                  redeemers = Some(
                    KeepRaw(
                      Redeemers.Array(
                        IndexedSeq(
                          Redeemer(
                            tag = Arbitrary.arbitrary[RedeemerTag].sample.get,
                            index = Gen.choose(0, Int.MaxValue).sample.get,
                            data = scalus.uplc.builtin.Data.unit,
                            exUnits = Arbitrary.arbitrary[ExUnits].sample.get
                          )
                        )
                      )
                    )
                  )
                )
              )
            )
        }

        val utxos = Map(
          collateralInput1 -> Output(
            Arbitrary
                .arbitrary[ShelleyAddress]
                .sample
                .get
                .copy(payment =
                    ShelleyPaymentPart.Key(
                      Hash(platform.blake2b_224(publicKey1))
                    )
                ),
            Value(Coin(30000000L))
          ),
          collateralInput2 -> Output(
            Arbitrary
                .arbitrary[ShelleyAddress]
                .sample
                .get
                .copy(payment =
                    ShelleyPaymentPart.Key(
                      Hash(platform.blake2b_224(publicKey2))
                    )
                ),
            Value(
              Coin(30000000L),
              genMultiAsset(
                minPolicies = 1,
                maxPolicies = 4,
                minAssets = 1,
                maxAssets = 4
              ).sample.get
            )
          )
        )

        val emulator = TestEmulatorFactory.create(
          utxos = utxos,
          validators = Seq(FeesOkValidator),
          mutators = Seq.empty
        )
        val result = emulator.submitSync(transaction)
        assert(result.isLeft)
    }

    test("FeesOkValidator totalSumOfCollateralCoinsIsSufficient rule failure") {
        given Arbitrary[scalus.uplc.builtin.Data] = Arbitrary(
          Gen.const(scalus.uplc.builtin.Data.unit) // Simplified for testing
        )

        val (_, publicKey1) = TestEmulatorFactory.generateKeyPair()
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
                  collateralReturnOutput = Some(
                    Sized(
                      Output(
                        Arbitrary.arbitrary[ShelleyAddress].sample.get,
                        Value(Coin(60000000L))
                      )
                    )
                  ),
                  totalCollateral = Some(Coin(60000000L)),
                  fee = Coin(10000000L),
                  referenceInputs = TaggedSortedSet.empty,
                  outputs = IndexedSeq.empty,
                  mint = None,
                  votingProcedures = None,
                  withdrawals = None,
                  proposalProcedures = TaggedOrderedSet.empty,
                  certificates = TaggedOrderedStrictSet.empty,
                  requiredSigners = TaggedSortedSet.empty
                )
              ),
              auxiliaryData = None,
              witnessSetRaw = KeepRaw(
                tx.witnessSet.copy(
                  vkeyWitnesses = TaggedSortedSet.empty,
                  bootstrapWitnesses = TaggedSortedSet.empty,
                  nativeScripts = TaggedSortedMap.empty,
                  plutusV1Scripts = TaggedSortedStrictMap.empty,
                  plutusV2Scripts = TaggedSortedStrictMap.empty,
                  plutusV3Scripts = TaggedSortedStrictMap.empty,
                  plutusData = KeepRaw(TaggedSortedMap.empty),
                  redeemers = Some(
                    KeepRaw(
                      Redeemers.Array(
                        IndexedSeq(
                          Redeemer(
                            tag = Arbitrary.arbitrary[RedeemerTag].sample.get,
                            index = Gen.choose(0, Int.MaxValue).sample.get,
                            data = scalus.uplc.builtin.Data.unit,
                            exUnits = Arbitrary.arbitrary[ExUnits].sample.get
                          )
                        )
                      )
                    )
                  )
                )
              )
            )
        }

        val utxos = Map(
          collateralInput1 -> Output(
            Arbitrary
                .arbitrary[ShelleyAddress]
                .sample
                .get
                .copy(payment =
                    ShelleyPaymentPart.Key(
                      Hash(platform.blake2b_224(publicKey1))
                    )
                ),
            Value(Coin(30000000L))
          ),
          collateralInput2 -> Output(
            Arbitrary
                .arbitrary[ShelleyAddress]
                .sample
                .get
                .copy(payment =
                    ShelleyPaymentPart.Key(
                      Hash(platform.blake2b_224(publicKey2))
                    )
                ),
            Value(Coin(30000000L))
          )
        )

        val emulator = TestEmulatorFactory.create(
          utxos = utxos,
          validators = Seq(FeesOkValidator),
          mutators = Seq.empty
        )
        val result = emulator.submitSync(transaction)
        assert(result.isLeft)
    }

    test("FeesOkValidator totalSumOfCollateralCoinsIsEquivalentToTotalCollateral rule failure") {
        given Arbitrary[scalus.uplc.builtin.Data] = Arbitrary(
          Gen.const(scalus.uplc.builtin.Data.unit) // Simplified for testing
        )

        val (_, publicKey1) = TestEmulatorFactory.generateKeyPair()
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
                  collateralReturnOutput = Some(
                    Sized(
                      Output(
                        Arbitrary.arbitrary[ShelleyAddress].sample.get,
                        Value(Coin(20000000L))
                      )
                    )
                  ),
                  totalCollateral = Some(Coin(50000000L)),
                  fee = Coin(10000000L),
                  referenceInputs = TaggedSortedSet.empty,
                  outputs = IndexedSeq.empty,
                  mint = None,
                  votingProcedures = None,
                  withdrawals = None,
                  proposalProcedures = TaggedOrderedSet.empty,
                  certificates = TaggedOrderedStrictSet.empty,
                  requiredSigners = TaggedSortedSet.empty
                )
              ),
              auxiliaryData = None,
              witnessSetRaw = KeepRaw(
                tx.witnessSet.copy(
                  vkeyWitnesses = TaggedSortedSet.empty,
                  bootstrapWitnesses = TaggedSortedSet.empty,
                  nativeScripts = TaggedSortedMap.empty,
                  plutusV1Scripts = TaggedSortedStrictMap.empty,
                  plutusV2Scripts = TaggedSortedStrictMap.empty,
                  plutusV3Scripts = TaggedSortedStrictMap.empty,
                  plutusData = KeepRaw(TaggedSortedMap.empty),
                  redeemers = Some(
                    KeepRaw(
                      Redeemers.Array(
                        IndexedSeq(
                          Redeemer(
                            tag = Arbitrary.arbitrary[RedeemerTag].sample.get,
                            index = Gen.choose(0, Int.MaxValue).sample.get,
                            data = scalus.uplc.builtin.Data.unit,
                            exUnits = Arbitrary.arbitrary[ExUnits].sample.get
                          )
                        )
                      )
                    )
                  )
                )
              )
            )
        }

        val utxos = Map(
          collateralInput1 -> Output(
            Arbitrary
                .arbitrary[ShelleyAddress]
                .sample
                .get
                .copy(payment =
                    ShelleyPaymentPart.Key(
                      Hash(platform.blake2b_224(publicKey1))
                    )
                ),
            Value(Coin(30000000L))
          ),
          collateralInput2 -> Output(
            Arbitrary
                .arbitrary[ShelleyAddress]
                .sample
                .get
                .copy(payment =
                    ShelleyPaymentPart.Key(
                      Hash(platform.blake2b_224(publicKey2))
                    )
                ),
            Value(Coin(30000000L))
          )
        )

        val emulator = TestEmulatorFactory.create(
          utxos = utxos,
          validators = Seq(FeesOkValidator),
          mutators = Seq.empty
        )
        val result = emulator.submitSync(transaction)
        assert(result.isLeft)
    }

    test("FeesOkValidator isAtLeastOneCollateralInput rule failure") {
        given Arbitrary[scalus.uplc.builtin.Data] = Arbitrary(
          Gen.const(scalus.uplc.builtin.Data.unit) // Simplified for testing
        )

        val transaction = {
            val tx = randomTransactionWithIsValidField
            tx.copy(
              body = KeepRaw(
                tx.body.value.copy(
                  inputs = TaggedSortedSet.empty,
                  collateralInputs = TaggedSortedSet.empty,
                  collateralReturnOutput = None,
                  totalCollateral = None,
                  fee = Coin(10000000L),
                  referenceInputs = TaggedSortedSet.empty,
                  outputs = IndexedSeq.empty,
                  mint = None,
                  votingProcedures = None,
                  withdrawals = None,
                  proposalProcedures = TaggedOrderedSet.empty,
                  certificates = TaggedOrderedStrictSet.empty,
                  requiredSigners = TaggedSortedSet.empty
                )
              ),
              auxiliaryData = None,
              witnessSetRaw = KeepRaw(
                tx.witnessSet.copy(
                  vkeyWitnesses = TaggedSortedSet.empty,
                  bootstrapWitnesses = TaggedSortedSet.empty,
                  nativeScripts = TaggedSortedMap.empty,
                  plutusV1Scripts = TaggedSortedStrictMap.empty,
                  plutusV2Scripts = TaggedSortedStrictMap.empty,
                  plutusV3Scripts = TaggedSortedStrictMap.empty,
                  plutusData = KeepRaw(TaggedSortedMap.empty),
                  redeemers = Some(
                    KeepRaw(
                      Redeemers.Array(
                        IndexedSeq(
                          Redeemer(
                            tag = Arbitrary.arbitrary[RedeemerTag].sample.get,
                            index = Gen.choose(0, Int.MaxValue).sample.get,
                            data = scalus.uplc.builtin.Data.unit,
                            exUnits = Arbitrary.arbitrary[ExUnits].sample.get
                          )
                        )
                      )
                    )
                  )
                )
              )
            )
        }

        val emulator = TestEmulatorFactory.create(
          validators = Seq(FeesOkValidator),
          mutators = Seq.empty
        )
        val result = emulator.submitSync(transaction)
        assert(result.isLeft)
    }
}
