package scalus.cardano.address

import org.scalacheck.Arbitrary.arbitrary
import org.scalacheck.{Arbitrary, Gen}
import scalus.cardano.ledger.{Hash, Slot}

object ArbitraryInstances extends ArbitraryInstances
trait ArbitraryInstances extends scalus.uplc.test.ArbitraryInstances {
    given Arbitrary[Network] = Arbitrary {
        val genTestnet = Gen.const(Network.Testnet)
        val genMainnet = Gen.const(Network.Mainnet)
        val genOther = Gen.choose(2.toByte, 15.toByte).map(Network.Other.apply(_))
        Gen.frequency(
          2 -> genTestnet,
          2 -> genMainnet,
          1 -> genOther
        )
    }

    // Generate valid Byron addresses with proper CBOR structure
    given Arbitrary[ByronAddress] = Arbitrary {
        for {
            addrRoot <- genByteStringOfN(28) // 28-byte address root
            addrType <- Gen.oneOf(0, 2) // 0 = VerKey, 2 = Redeem
            hasNetworkMagic <- Gen.oneOf(true, false)
            // Use actual Cardano network magic values: mainnet, preprod, preview
            networkMagic: Option[Long] <-
                if hasNetworkMagic then Gen.oneOf(764824073L, 1097911063L, 1L).map(Some(_))
                else Gen.const(None)
        } yield ByronAddress.create(addrRoot, addrType, networkMagic)
    }

    given Arbitrary[StakePayload] = Arbitrary {
        val genStake = genByteStringOfN(28).map(byteStr => StakePayload.Stake(Hash(byteStr)))
        val genScript = genByteStringOfN(28).map(byteStr => StakePayload.Script(Hash(byteStr)))
        Gen.oneOf(genStake, genScript)
    }

    given Arbitrary[StakeAddress] = Arbitrary {
        for
            network <- arbitrary[Network]
            payload <- arbitrary[StakePayload]
        yield StakeAddress(network, payload)
    }

    given Arbitrary[ShelleyPaymentPart] = Arbitrary {
        val genKey = genByteStringOfN(28).map(byteStr => ShelleyPaymentPart.Key(Hash(byteStr)))
        val genScript =
            genByteStringOfN(28).map(byteStr => ShelleyPaymentPart.Script(Hash(byteStr)))
        Gen.oneOf(genKey, genScript)
    }

    given Arbitrary[Pointer] = Arbitrary {
        for
            slot <- Gen.choose(0L, Long.MaxValue)
            txIndex <- Gen.choose(0L, Long.MaxValue)
            certIndex <- Gen.choose(0L, Long.MaxValue)
        yield Pointer(Slot(slot), txIndex, certIndex)
    }

    given Arbitrary[ShelleyDelegationPart] = Arbitrary {
        val genKey = genByteStringOfN(28).map(byteStr => ShelleyDelegationPart.Key(Hash(byteStr)))
        val genScript =
            genByteStringOfN(28).map(byteStr => ShelleyDelegationPart.Script(Hash(byteStr)))
        val genPointer = arbitrary[Pointer].map(ShelleyDelegationPart.Pointer.apply(_))
        val genNull = Gen.const(ShelleyDelegationPart.Null)
        Gen.oneOf(genKey, genScript, genPointer, genNull)
    }

    given Arbitrary[ShelleyAddress] = Arbitrary {
        for
            network <- arbitrary[Network]
            paymentPart <- arbitrary[ShelleyPaymentPart]
            delegationPart <- arbitrary[ShelleyDelegationPart]
        yield ShelleyAddress(network, paymentPart, delegationPart)
    }

    given Arbitrary[Address] = Arbitrary {
        val genByron = arbitrary[ByronAddress]
        val genShelley = arbitrary[ShelleyAddress]
        val genStake = arbitrary[StakeAddress]
        Gen.oneOf(genByron, genShelley, genStake)
    }
}
