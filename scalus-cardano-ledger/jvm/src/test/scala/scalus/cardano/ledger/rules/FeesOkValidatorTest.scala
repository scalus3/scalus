package scalus.cardano.ledger
package rules

import org.scalacheck.{Arbitrary, Gen}
import scalus.cardano.address.{ShelleyAddress, ShelleyPaymentPart}
import scalus.builtin.platform
import org.scalatest.funsuite.AnyFunSuite
import TransactionWitnessSet.given

class FeesOkValidatorTest extends AnyFunSuite, ValidatorRulesTestKit {
    test("FeesOkValidator rule success") {
        given Arbitrary[scalus.builtin.Data] = Arbitrary(
          Gen.const(scalus.builtin.Data.unit) // Simplified for testing
        )

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
                  collateralReturnOutput = Some(
                    Sized(
                      TransactionOutput(
                        Arbitrary.arbitrary[ShelleyAddress].sample.get,
                        Value(Coin(20000000L), Arbitrary.arbitrary[MultiAsset].sample.get)
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
                  certificates = TaggedOrderedSet.empty,
                  requiredSigners = TaggedSortedSet.empty
                )
              ),
              auxiliaryData = None,
              witnessSet = tx.witnessSet.copy(
                vkeyWitnesses = TaggedSortedSet.empty,
                bootstrapWitnesses = TaggedSortedSet.empty,
                nativeScripts = TaggedSortedSet.empty,
                plutusV1Scripts = TaggedSortedSet.empty,
                plutusV2Scripts = TaggedSortedSet.empty,
                plutusV3Scripts = TaggedSortedSet.empty,
                plutusData = KeepRaw(TaggedSet.empty),
                redeemers = Some(
                  KeepRaw(
                    Redeemers.Array(
                      IndexedSeq(
                        Redeemer(
                          tag = Arbitrary.arbitrary[RedeemerTag].sample.get,
                          index = Gen.choose(0, Int.MaxValue).sample.get,
                          data = scalus.builtin.Data.unit,
                          exUnits = Arbitrary.arbitrary[ExUnits].sample.get
                        )
                      )
                    )
                  )
                )
              )
            )
        }

        val state = State(
          utxos = Map(
            collateralInput1 -> TransactionOutput(
              Arbitrary
                  .arbitrary[ShelleyAddress]
                  .sample
                  .get
                  .copy(payment =
                      ShelleyPaymentPart.keyHash(
                        Hash(platform.blake2b_224(publicKey1))
                      )
                  ),
              Value(Coin(30000000L))
            ),
            collateralInput2 -> TransactionOutput(
              Arbitrary
                  .arbitrary[ShelleyAddress]
                  .sample
                  .get
                  .copy(payment =
                      ShelleyPaymentPart.keyHash(
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
        )

        val result = FeesOkValidator.validate(context, state, transaction)
        assert(result.isRight)
    }

    test("FeesOkValidator feePaidIsGreeterOrEqualThanMinimumFee rule failure") {
        given Arbitrary[scalus.builtin.Data] = Arbitrary(
          Gen.const(scalus.builtin.Data.unit) // Simplified for testing
        )

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
                  collateralReturnOutput = Some(
                    Sized(
                      TransactionOutput(
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
                  certificates = TaggedOrderedSet.empty,
                  requiredSigners = TaggedSortedSet.empty
                )
              ),
              auxiliaryData = None,
              witnessSet = tx.witnessSet.copy(
                vkeyWitnesses = TaggedSortedSet.empty,
                bootstrapWitnesses = TaggedSortedSet.empty,
                nativeScripts = TaggedSortedSet.empty,
                plutusV1Scripts = TaggedSortedSet.empty,
                plutusV2Scripts = TaggedSortedSet.empty,
                plutusV3Scripts = TaggedSortedSet.empty,
                plutusData = KeepRaw(TaggedSet.empty),
                redeemers = Some(
                  KeepRaw(
                    Redeemers.Array(
                      IndexedSeq(
                        Redeemer(
                          tag = Arbitrary.arbitrary[RedeemerTag].sample.get,
                          index = Gen.choose(0, Int.MaxValue).sample.get,
                          data = scalus.builtin.Data.unit,
                          exUnits = Arbitrary.arbitrary[ExUnits].sample.get
                        )
                      )
                    )
                  )
                )
              )
            )
        }

        val state = State(
          utxos = Map(
            collateralInput1 -> TransactionOutput(
              Arbitrary
                  .arbitrary[ShelleyAddress]
                  .sample
                  .get
                  .copy(payment =
                      ShelleyPaymentPart.keyHash(
                        Hash(platform.blake2b_224(publicKey1))
                      )
                  ),
              Value(Coin(30000000L))
            ),
            collateralInput2 -> TransactionOutput(
              Arbitrary
                  .arbitrary[ShelleyAddress]
                  .sample
                  .get
                  .copy(payment =
                      ShelleyPaymentPart.keyHash(
                        Hash(platform.blake2b_224(publicKey2))
                      )
                  ),
              Value(Coin(30000000L))
            )
          )
        )

        val result = FeesOkValidator.validate(context, state, transaction)
        assert(result.isLeft)
    }

    test("FeesOkValidator collateralConsistsOnlyOfVKeyAddress rule failure") {
        given Arbitrary[scalus.builtin.Data] = Arbitrary(
          Gen.const(scalus.builtin.Data.unit) // Simplified for testing
        )

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
                  collateralReturnOutput = None,
                  totalCollateral = None,
                  fee = Coin(10000000L),
                  referenceInputs = TaggedSortedSet.empty,
                  outputs = IndexedSeq.empty,
                  mint = None,
                  votingProcedures = None,
                  withdrawals = None,
                  proposalProcedures = TaggedOrderedSet.empty,
                  certificates = TaggedOrderedSet.empty,
                  requiredSigners = TaggedSortedSet.empty
                )
              ),
              auxiliaryData = None,
              witnessSet = tx.witnessSet.copy(
                vkeyWitnesses = TaggedSortedSet.empty,
                bootstrapWitnesses = TaggedSortedSet.empty,
                nativeScripts = TaggedSortedSet.empty,
                plutusV1Scripts = TaggedSortedSet.empty,
                plutusV2Scripts = TaggedSortedSet.empty,
                plutusV3Scripts = TaggedSortedSet.empty,
                plutusData = KeepRaw(TaggedSet.empty),
                redeemers = Some(
                  KeepRaw(
                    Redeemers.Array(
                      IndexedSeq(
                        Redeemer(
                          tag = Arbitrary.arbitrary[RedeemerTag].sample.get,
                          index = Gen.choose(0, Int.MaxValue).sample.get,
                          data = scalus.builtin.Data.unit,
                          exUnits = Arbitrary.arbitrary[ExUnits].sample.get
                        )
                      )
                    )
                  )
                )
              )
            )
        }

        val state = State(
          utxos = Map(
            collateralInput1 -> TransactionOutput(
              Arbitrary
                  .arbitrary[ShelleyAddress]
                  .sample
                  .get
                  .copy(payment =
                      ShelleyPaymentPart.keyHash(
                        Hash(platform.blake2b_224(publicKey1))
                      )
                  ),
              Value(Coin(20000000L))
            ),
            collateralInput2 -> TransactionOutput(
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
        )

        val result = FeesOkValidator.validate(context, state, transaction)
        assert(result.isLeft)
    }

    test("FeesOkValidator collateralDoesNotContainAnyNonADA rule failure") {
        given Arbitrary[scalus.builtin.Data] = Arbitrary(
          Gen.const(scalus.builtin.Data.unit) // Simplified for testing
        )

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
                  collateralReturnOutput = Some(
                    Sized(
                      TransactionOutput(
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
                  certificates = TaggedOrderedSet.empty,
                  requiredSigners = TaggedSortedSet.empty
                )
              ),
              auxiliaryData = None,
              witnessSet = tx.witnessSet.copy(
                vkeyWitnesses = TaggedSortedSet.empty,
                bootstrapWitnesses = TaggedSortedSet.empty,
                nativeScripts = TaggedSortedSet.empty,
                plutusV1Scripts = TaggedSortedSet.empty,
                plutusV2Scripts = TaggedSortedSet.empty,
                plutusV3Scripts = TaggedSortedSet.empty,
                plutusData = KeepRaw(TaggedSet.empty),
                redeemers = Some(
                  KeepRaw(
                    Redeemers.Array(
                      IndexedSeq(
                        Redeemer(
                          tag = Arbitrary.arbitrary[RedeemerTag].sample.get,
                          index = Gen.choose(0, Int.MaxValue).sample.get,
                          data = scalus.builtin.Data.unit,
                          exUnits = Arbitrary.arbitrary[ExUnits].sample.get
                        )
                      )
                    )
                  )
                )
              )
            )
        }

        val state = State(
          utxos = Map(
            collateralInput1 -> TransactionOutput(
              Arbitrary
                  .arbitrary[ShelleyAddress]
                  .sample
                  .get
                  .copy(payment =
                      ShelleyPaymentPart.keyHash(
                        Hash(platform.blake2b_224(publicKey1))
                      )
                  ),
              Value(Coin(30000000L))
            ),
            collateralInput2 -> TransactionOutput(
              Arbitrary
                  .arbitrary[ShelleyAddress]
                  .sample
                  .get
                  .copy(payment =
                      ShelleyPaymentPart.keyHash(
                        Hash(platform.blake2b_224(publicKey2))
                      )
                  ),
              Value(Coin(30000000L))
            )
          )
        )

        val result = FeesOkValidator.validate(context, state, transaction)
        assert(result.isLeft)
    }

    test("FeesOkValidator totalSumOfCollateralCoinsIsSufficient rule failure") {
        given Arbitrary[scalus.builtin.Data] = Arbitrary(
          Gen.const(scalus.builtin.Data.unit) // Simplified for testing
        )

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
                  collateralReturnOutput = Some(
                    Sized(
                      TransactionOutput(
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
                  certificates = TaggedOrderedSet.empty,
                  requiredSigners = TaggedSortedSet.empty
                )
              ),
              auxiliaryData = None,
              witnessSet = tx.witnessSet.copy(
                vkeyWitnesses = TaggedSortedSet.empty,
                bootstrapWitnesses = TaggedSortedSet.empty,
                nativeScripts = TaggedSortedSet.empty,
                plutusV1Scripts = TaggedSortedSet.empty,
                plutusV2Scripts = TaggedSortedSet.empty,
                plutusV3Scripts = TaggedSortedSet.empty,
                plutusData = KeepRaw(TaggedSet.empty),
                redeemers = Some(
                  KeepRaw(
                    Redeemers.Array(
                      IndexedSeq(
                        Redeemer(
                          tag = Arbitrary.arbitrary[RedeemerTag].sample.get,
                          index = Gen.choose(0, Int.MaxValue).sample.get,
                          data = scalus.builtin.Data.unit,
                          exUnits = Arbitrary.arbitrary[ExUnits].sample.get
                        )
                      )
                    )
                  )
                )
              )
            )
        }

        val state = State(
          utxos = Map(
            collateralInput1 -> TransactionOutput(
              Arbitrary
                  .arbitrary[ShelleyAddress]
                  .sample
                  .get
                  .copy(payment =
                      ShelleyPaymentPart.keyHash(
                        Hash(platform.blake2b_224(publicKey1))
                      )
                  ),
              Value(Coin(30000000L))
            ),
            collateralInput2 -> TransactionOutput(
              Arbitrary
                  .arbitrary[ShelleyAddress]
                  .sample
                  .get
                  .copy(payment =
                      ShelleyPaymentPart.keyHash(
                        Hash(platform.blake2b_224(publicKey2))
                      )
                  ),
              Value(Coin(30000000L))
            )
          )
        )

        val result = FeesOkValidator.validate(context, state, transaction)
        assert(result.isLeft)
    }

    test("FeesOkValidator totalSumOfCollateralCoinsIsEquivalentToTotalCollateral rule failure") {
        given Arbitrary[scalus.builtin.Data] = Arbitrary(
          Gen.const(scalus.builtin.Data.unit) // Simplified for testing
        )

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
                  collateralReturnOutput = Some(
                    Sized(
                      TransactionOutput(
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
                  certificates = TaggedOrderedSet.empty,
                  requiredSigners = TaggedSortedSet.empty
                )
              ),
              auxiliaryData = None,
              witnessSet = tx.witnessSet.copy(
                vkeyWitnesses = TaggedSortedSet.empty,
                bootstrapWitnesses = TaggedSortedSet.empty,
                nativeScripts = TaggedSortedSet.empty,
                plutusV1Scripts = TaggedSortedSet.empty,
                plutusV2Scripts = TaggedSortedSet.empty,
                plutusV3Scripts = TaggedSortedSet.empty,
                plutusData = KeepRaw(TaggedSet.empty),
                redeemers = Some(
                  KeepRaw(
                    Redeemers.Array(
                      IndexedSeq(
                        Redeemer(
                          tag = Arbitrary.arbitrary[RedeemerTag].sample.get,
                          index = Gen.choose(0, Int.MaxValue).sample.get,
                          data = scalus.builtin.Data.unit,
                          exUnits = Arbitrary.arbitrary[ExUnits].sample.get
                        )
                      )
                    )
                  )
                )
              )
            )
        }

        val state = State(
          utxos = Map(
            collateralInput1 -> TransactionOutput(
              Arbitrary
                  .arbitrary[ShelleyAddress]
                  .sample
                  .get
                  .copy(payment =
                      ShelleyPaymentPart.keyHash(
                        Hash(platform.blake2b_224(publicKey1))
                      )
                  ),
              Value(Coin(30000000L))
            ),
            collateralInput2 -> TransactionOutput(
              Arbitrary
                  .arbitrary[ShelleyAddress]
                  .sample
                  .get
                  .copy(payment =
                      ShelleyPaymentPart.keyHash(
                        Hash(platform.blake2b_224(publicKey2))
                      )
                  ),
              Value(Coin(30000000L))
            )
          )
        )

        val result = FeesOkValidator.validate(context, state, transaction)
        assert(result.isLeft)
    }

    test("FeesOkValidator isAtLeastOneCollateralInput rule failure") {
        given Arbitrary[scalus.builtin.Data] = Arbitrary(
          Gen.const(scalus.builtin.Data.unit) // Simplified for testing
        )

        val context = Context()

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
                  certificates = TaggedOrderedSet.empty,
                  requiredSigners = TaggedSortedSet.empty
                )
              ),
              auxiliaryData = None,
              witnessSet = tx.witnessSet.copy(
                vkeyWitnesses = TaggedSortedSet.empty,
                bootstrapWitnesses = TaggedSortedSet.empty,
                nativeScripts = TaggedSortedSet.empty,
                plutusV1Scripts = TaggedSortedSet.empty,
                plutusV2Scripts = TaggedSortedSet.empty,
                plutusV3Scripts = TaggedSortedSet.empty,
                plutusData = KeepRaw(TaggedSet.empty),
                redeemers = Some(
                  KeepRaw(
                    Redeemers.Array(
                      IndexedSeq(
                        Redeemer(
                          tag = Arbitrary.arbitrary[RedeemerTag].sample.get,
                          index = Gen.choose(0, Int.MaxValue).sample.get,
                          data = scalus.builtin.Data.unit,
                          exUnits = Arbitrary.arbitrary[ExUnits].sample.get
                        )
                      )
                    )
                  )
                )
              )
            )
        }

        val state = State()

        val result = FeesOkValidator.validate(context, state, transaction)
        assert(result.isLeft)
    }
}
