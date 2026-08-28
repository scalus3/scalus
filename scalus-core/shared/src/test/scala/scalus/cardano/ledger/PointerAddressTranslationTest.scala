package scalus.cardano.ledger

import org.scalatest.funsuite.AnyFunSuite
import scalus.cardano.address.*
import scalus.cardano.onchain.plutus.prelude.Option as POption
import scalus.cardano.onchain.plutus.v1
import scalus.uplc.builtin.ByteString

/** Pointer addresses must keep their staking credential in the script context.
  *
  * This is covered here rather than by the golden translation corpus, which contains zero pointer
  * addresses (its generator never produces them), so nothing else in the suite would notice a
  * regression.
  */
class PointerAddressTranslationTest extends AnyFunSuite {

    private val paymentHash = ByteString.fromHex("11" * 28)

    private def shelleyWith(delegation: ShelleyDelegationPart): ShelleyAddress =
        ShelleyAddress(
          network = Network.Mainnet,
          payment = ShelleyPaymentPart.Key(Hash[Blake2b_224, HashPurpose.KeyHash](paymentHash)),
          delegation = delegation
        )

    test("a pointer delegation part becomes StakingPtr, not None") {
        // The ledger maps StakeRefPtr to `Just (PV1.StakingPtr slot txIx certIx)` for every
        // Plutus version (libs/cardano-ledger-core/.../Plutus/TxInfo.hs:133-137).
        val address = shelleyWith(ShelleyDelegationPart.Pointer(Pointer(Slot(2), 3L, 4L)))
        val translated = LedgerToPlutusTranslation.getAddress(address)
        assert(
          translated.stakingCredential == POption.Some(
            v1.StakingCredential.StakingPtr(BigInt(2), BigInt(3), BigInt(4))
          ),
          s"expected StakingPtr(2, 3, 4), got ${translated.stakingCredential}"
        )
    }

    test("a null delegation part still becomes None") {
        val address = shelleyWith(ShelleyDelegationPart.Null)
        assert(LedgerToPlutusTranslation.getAddress(address).stakingCredential == POption.None)
    }

    test("a key delegation part still becomes StakingHash") {
        val stakeHash = ByteString.fromHex("22" * 28)
        val address = shelleyWith(ShelleyDelegationPart.Key(Hash.stakeKeyHash(stakeHash)))
        assert(
          LedgerToPlutusTranslation.getAddress(address).stakingCredential == POption.Some(
            v1.StakingCredential.StakingHash(
              v1.Credential.PubKeyCredential(v1.PubKeyHash(stakeHash))
            )
          )
        )
    }
}
