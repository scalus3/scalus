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
        // Up to the ledger's widths, every encoded length as often as another, and the values at
        // both ends of each length, where a variable-length encoding goes wrong. A uniform slot
        // would almost always take 5 bytes and almost never 3.
        def upTo(max: Long): Gen[Long] = {
            // One range per encoded length: 0..127, 128..16383, and so on up to max
            val ranges = (1 to VarLen.size(max)).map { n =>
                val low = if n == 1 then 0L else 1L << (7 * (n - 1))
                (low, math.min((1L << (7 * n)) - 1, max))
            }
            Gen.frequency(
              3 -> Gen.oneOf(ranges).flatMap((low, high) => Gen.choose(low, high)),
              1 -> Gen.oneOf(ranges.flatMap((low, high) => Seq(low, high)))
            )
        }
        for
            slot <- upTo(Pointer.MaxSlot)
            txIndex <- upTo(Pointer.MaxIndex)
            certIndex <- upTo(Pointer.MaxIndex)
        yield Pointer(Slot(slot), txIndex, certIndex)
    }

    /** The generator behind `Arbitrary[ShelleyDelegationPart]`, with the weights of cardano-ledger
      * `Arbitrary Addr` (`Core/Arbitrary.hs`): 80 % a stake key or script (a base address), 15 %
      * none (an enterprise address), 5 % a pointer. Before D5 each form had 25 %.
      */
    def genShelleyDelegationPart: Gen[ShelleyDelegationPart] = {
        val genKey = genByteStringOfN(28).map(byteStr => ShelleyDelegationPart.Key(Hash(byteStr)))
        val genScript =
            genByteStringOfN(28).map(byteStr => ShelleyDelegationPart.Script(Hash(byteStr)))
        val genPointer = arbitrary[Pointer].map(ShelleyDelegationPart.Pointer.apply(_))
        val genNull = Gen.const(ShelleyDelegationPart.Null)
        Gen.frequency(8 -> genKey, 8 -> genScript, 3 -> genNull, 1 -> genPointer)
    }

    given Arbitrary[ShelleyDelegationPart] = Arbitrary(genShelleyDelegationPart)

    given Arbitrary[ShelleyAddress] = Arbitrary {
        for
            network <- arbitrary[Network]
            paymentPart <- arbitrary[ShelleyPaymentPart]
            delegationPart <- arbitrary[ShelleyDelegationPart]
        yield ShelleyAddress(network, paymentPart, delegationPart)
    }

    /** An address that can receive an output: a Shelley address on mainnet or a testnet in 80 %, a
      * Byron address in 20 %, the bootstrap share of cardano-ledger `Arbitrary Addr`. Never a stake
      * address and never `Network.Other`: the ledger's `decodeAddr` rejects both in an output
      * (F-35).
      */
    def genPaymentAddress: Gen[Address] = {
        val shelley =
            for
                network <- Gen.oneOf(Network.Mainnet, Network.Testnet)
                paymentPart <- arbitrary[ShelleyPaymentPart]
                delegationPart <- genShelleyDelegationPart
            yield ShelleyAddress(network, paymentPart, delegationPart)
        Gen.frequency(4 -> shelley, 1 -> arbitrary[ByronAddress])
    }

    /** The generator behind `Arbitrary[Address]`: 80 % a payment address of `genPaymentAddress`, 20
      * % a stake address (a reward account). Before D5 each of the three kinds had 33 %.
      */
    def genAddress: Gen[Address] =
        Gen.frequency(4 -> genPaymentAddress, 1 -> arbitrary[StakeAddress])

    given Arbitrary[Address] = Arbitrary(genAddress)
}
