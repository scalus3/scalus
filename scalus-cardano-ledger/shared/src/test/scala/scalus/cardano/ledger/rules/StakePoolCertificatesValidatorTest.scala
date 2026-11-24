package scalus.cardano.ledger.rules

import org.scalatest.EitherValues
import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers
import scalus.cardano.address.Network
import scalus.cardano.ledger.*
import scalus.uplc.builtin.ByteString

class StakePoolCertificatesValidatorTest
    extends AnyFunSuite
    with Matchers
    with EitherValues
    with CertsTestKit {

    private val minPoolCost = Coin(params.minPoolCost)

    private val operator = AddrKeyHash.fromHex("f" * 56)
    private val poolId = PoolKeyHash.fromByteString(operator)

    private def mkRegistration(cost: Coin = minPoolCost): Certificate.PoolRegistration =
        poolRegistration(operator, rewardAccount(1), cost = cost, owners = Set(operator))

    private def runValidator(
        certs: Seq[Certificate],
        state: CertState = CertState.empty,
        epoch: Long = slotConfig.zeroEpoch
    ) = StakePoolCertificatesValidator.validate(
      context(state, slot = epochStart(epoch)),
      State(certState = state),
      certTx(certs*)
    )

    extension (error: StakePoolCertificatesValidator.Error)
        private def poolError: TransactionException.StakePoolException = error match
            case e: TransactionException.StakePoolException => e
            case other => fail(s"not a StakePoolException: $other")

    private def withMetadataHash(size: Int): Certificate.PoolRegistration =
        mkRegistration().copy(poolMetadata =
            Some(PoolMetadata("https://pool.example", ByteString.fromArray(new Array[Byte](size))))
        )

    test("pool registration succeeds for new pool") {
        val result = runValidator(Seq(mkRegistration()))
        result.isRight shouldBe true
    }

    test("pool re-registration succeeds") {
        // Despite what you might think, re-registration is ok.
        val existing = mkRegistration()
        val state = certState(pools = Map(poolId -> existing))

        val updated = mkRegistration(cost = minPoolCost + Coin.ada(10))
        val result = runValidator(Seq(updated), state)
        result.isRight shouldBe true
    }

    test("registration fails when reward account network mismatches") {
        val mismatched = mkRegistration().copy(rewardAccount = rewardAccount(1, Network.Mainnet))

        val error = runValidator(Seq(mismatched)).left.value.poolError
        error.rewardAccountNetworkMismatch.exists(_._1 == poolId) shouldBe true
    }

    test("registration fails when cost below minimum") {
        val lowCost = mkRegistration(cost = minPoolCost - Coin.ada(1))
        val error = runValidator(Seq(lowCost)).left.value.poolError
        error.costBelowMinimum should contain key poolId
    }

    test("retirement fails when pool not registered") {
        val error = runValidator(Seq(Certificate.PoolRetirement(poolId, 10))).left.value.poolError
        error.notRegistered should contain(poolId)
    }

    test("retirement succeeds when pool is registered and epoch is valid") {
        val existing = mkRegistration()
        val state = certState(pools = Map(poolId -> existing))
        val currentEpoch = 100L
        val validRetirementEpoch = currentEpoch + 1 // Must be > currentEpoch

        val result = runValidator(
          Seq(Certificate.PoolRetirement(poolId, validRetirementEpoch)),
          state,
          epoch = currentEpoch
        )
        result.isRight shouldBe true
    }

    test("retirement fails when epoch equals current epoch") {
        val existing = mkRegistration()
        val state = certState(pools = Map(poolId -> existing))
        val currentEpoch = 100L

        val error = runValidator(
          Seq(
            Certificate.PoolRetirement(poolId, currentEpoch)
          ), // epochNo == currentEpoch is invalid
          state,
          epoch = currentEpoch
        ).left.value.poolError
        error.invalidRetirementEpochs should contain key poolId
    }

    test("retirement fails when epoch exceeds maximum allowed") {
        val existing = mkRegistration()
        val state = certState(pools = Map(poolId -> existing))
        val currentEpoch = 100L
        val maxRetirementEpoch = currentEpoch + params.poolRetireMaxEpoch
        val tooFarEpoch = maxRetirementEpoch + 1

        val error = runValidator(
          Seq(Certificate.PoolRetirement(poolId, tooFarEpoch)),
          state,
          epoch = currentEpoch
        ).left.value.poolError
        error.invalidRetirementEpochs should contain key poolId
    }

    test("registration fails when the metadata hash is longer than 32 bytes, spec [TX-2]") {
        // PoolMedataHashTooBig, S/Rules/Pool.hs:249-254
        runValidator(Seq(withMetadataHash(33))).left.value match
            case e: TransactionException.PoolMetadataHashTooBigException =>
                e.hashSizes shouldBe Map(poolId -> 33)
                e.maxHashSize shouldBe 32
            case other => fail(s"not a PoolMetadataHashTooBigException: $other")
    }

    test("registration succeeds with a 32-byte or shorter metadata hash, spec [TX-2]") {
        runValidator(Seq(withMetadataHash(32))).isRight shouldBe true
        runValidator(Seq(withMetadataHash(0))).isRight shouldBe true
    }
}
