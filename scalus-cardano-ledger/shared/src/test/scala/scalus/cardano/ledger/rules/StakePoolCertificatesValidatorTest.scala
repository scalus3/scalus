package scalus.cardano.ledger.rules

import org.scalatest.EitherValues
import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers
import scalus.cardano.address.{Network, StakeAddress, StakePayload}
import scalus.cardano.ledger.*

class StakePoolCertificatesValidatorTest extends AnyFunSuite with Matchers with EitherValues {

    private val cardanoInfo = CardanoInfo.preprod
    private val protocolParams = cardanoInfo.protocolParams
    private val minPoolCost = Coin(protocolParams.minPoolCost)

    private val operator = AddrKeyHash.fromHex("f" * 56)
    private val poolId = PoolKeyHash.fromByteString(operator)
    private val vrfKeyHash = VrfKeyHash.fromHex("a" * 64)
    private val stakeKeyHash = StakeKeyHash.fromHex("b" * 56)
    private val rewardAccount =
        RewardAccount(StakeAddress(Network.Testnet, StakePayload.Stake(stakeKeyHash)))

    private def mkRegistration(cost: Coin = minPoolCost): Certificate.PoolRegistration =
        Certificate.PoolRegistration(
          operator = operator,
          vrfKeyHash = vrfKeyHash,
          pledge = Coin.ada(1),
          cost = cost,
          margin = UnitInterval.one,
          rewardAccount = rewardAccount,
          poolOwners = Set(operator),
          relays = IndexedSeq.empty,
          poolMetadata = None
        )

    // Note: slot is used as epoch in these tests due to current implementation limitation.
    // See TODO in StakePoolCertificatesValidator.
    private def mkContext(certState: CertState, epoch: Long = 0): Context =
        new Context(
          env = UtxoEnv(
            slot = epoch, // Using slot field as epoch for pool validation
            params = protocolParams,
            certState = certState,
            network = Network.Testnet
          ),
          slotConfig = cardanoInfo.slotConfig
        )

    private def runValidator(
        certs: Seq[Certificate],
        certState: CertState = CertState.empty,
        epoch: Long = 0
    ) = StakePoolCertificatesValidator.validate(
      mkContext(certState, epoch),
      State(certState = certState),
      Transaction(
        TransactionBody(
          inputs = TaggedSortedSet.empty,
          outputs = IndexedSeq.empty,
          fee = Coin.zero,
          certificates = TaggedOrderedStrictSet.from(certs)
        )
      )
    )

    test("pool registration succeeds for new pool") {
        val result = runValidator(Seq(mkRegistration()))
        result.isRight shouldBe true
    }

    test("pool re-registration succeeds") {
        // Despite what you might think, re-registration is ok.
        val existing = mkRegistration()
        val certState = CertState.empty.copy(
          pstate = PoolsState(stakePools = Map(poolId -> existing))
        )

        val updated = mkRegistration(cost = minPoolCost + Coin.ada(10))
        val result = runValidator(Seq(updated), certState)
        result.isRight shouldBe true
    }

    test("registration fails when reward account network mismatches") {
        val mismatched = mkRegistration().copy(
          rewardAccount = RewardAccount(
            StakeAddress(Network.Mainnet, StakePayload.Stake(stakeKeyHash))
          )
        )

        val error = runValidator(Seq(mismatched)).left.value
        error.rewardAccountNetworkMismatch.exists(_._1 == poolId) shouldBe true
    }

    test("registration fails when cost below minimum") {
        val lowCost = mkRegistration(cost = minPoolCost - Coin.ada(1))
        val error = runValidator(Seq(lowCost)).left.value
        error.costBelowMinimum should contain key poolId
    }

    test("retirement fails when pool not registered") {
        val error = runValidator(Seq(Certificate.PoolRetirement(poolId, 10))).left.value
        error.notRegistered should contain(poolId)
    }

    test("retirement succeeds when pool is registered and epoch is valid") {
        val existing = mkRegistration()
        val certState = CertState.empty.copy(
          pstate = PoolsState(stakePools = Map(poolId -> existing))
        )
        val currentEpoch = 100L
        val validRetirementEpoch = currentEpoch + 1 // Must be > currentEpoch

        val result = runValidator(
          Seq(Certificate.PoolRetirement(poolId, validRetirementEpoch)),
          certState,
          epoch = currentEpoch
        )
        result.isRight shouldBe true
    }

    test("retirement fails when epoch equals current epoch") {
        val existing = mkRegistration()
        val certState = CertState.empty.copy(
          pstate = PoolsState(stakePools = Map(poolId -> existing))
        )
        val currentEpoch = 100L

        val error = runValidator(
          Seq(
            Certificate.PoolRetirement(poolId, currentEpoch)
          ), // epochNo == currentEpoch is invalid
          certState,
          epoch = currentEpoch
        ).left.value
        error.invalidRetirementEpochs should contain key poolId
    }

    test("retirement fails when epoch exceeds maximum allowed") {
        val existing = mkRegistration()
        val certState = CertState.empty.copy(
          pstate = PoolsState(stakePools = Map(poolId -> existing))
        )
        val currentEpoch = 100L
        val maxRetirementEpoch = currentEpoch + protocolParams.poolRetireMaxEpoch
        val tooFarEpoch = maxRetirementEpoch + 1

        val error = runValidator(
          Seq(Certificate.PoolRetirement(poolId, tooFarEpoch)),
          certState,
          epoch = currentEpoch
        ).left.value
        error.invalidRetirementEpochs should contain key poolId
    }
}
