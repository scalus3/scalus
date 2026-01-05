package scalus.testing.kit

import org.scalacheck.{Arbitrary, Gen}
import scalus.cardano.address.Network.Mainnet
import scalus.cardano.address.ShelleyPaymentPart.Key
import scalus.cardano.address.{Address, Network, ShelleyAddress, ShelleyDelegationPart}
import scalus.cardano.ledger.AddrKeyHash
import scalus.cardano.ledger.ArbitraryInstances.given

object TestKit {
    def random[A: Arbitrary]: A = {
        Arbitrary.arbitrary[A].sample.get
    }

    def genPubkeyAddr(
        network: Network = Mainnet,
        delegation: ShelleyDelegationPart = ShelleyDelegationPart.Null
    ): Gen[Address] =
        Arbitrary
            .arbitrary[AddrKeyHash]
            .flatMap(akh =>
                ShelleyAddress(network = network, payment = Key(akh), delegation = delegation)
            )

}
