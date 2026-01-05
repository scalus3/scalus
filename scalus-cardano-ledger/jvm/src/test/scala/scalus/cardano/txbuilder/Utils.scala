package scalus.cardano.txbuilder

import monocle.syntax.all.*
import org.scalacheck.*
import scalus.cardano.address.Network.Mainnet
import scalus.cardano.address.ShelleyPaymentPart.Key
import scalus.cardano.address.{Address, Network, ShelleyAddress, ShelleyDelegationPart}
import scalus.cardano.ledger.*
import scalus.cardano.ledger.ArbitraryInstances.given
import scalus.cardano.ledger.TransactionOutput.Babbage
import scalus.cardano.txbuilder.TransactionBuilder.ensureMinAda

def genPubkeyAddr(
    network: Network = Mainnet,
    delegation: ShelleyDelegationPart = ShelleyDelegationPart.Null
): Gen[Address] =
    Arbitrary
        .arbitrary[AddrKeyHash]
        .flatMap(akh =>
            ShelleyAddress(network = network, payment = Key(akh), delegation = delegation)
        )

/** Generate a positive Ada value greater than or equal to `min`. */
def genAdaOnlyValue(min: Long): Gen[Value] =
    for {
        coin <- Gen.chooseNum(min, Long.MaxValue)
    } yield Value(Coin(coin))

/** Ada-only pub key utxo from the given peer, at least `min` Ada, random tx id, random index, no
  * datum, no script ref
  */
def genAdaOnlyPubKeyUtxo(
    peer: TestPeer,
    params: ProtocolParams = CardanoInfo.mainnet.protocolParams,
    min: Long = 0L
): Gen[(TransactionInput, Babbage)] =
    for txId <- Arbitrary.arbitrary[TransactionInput]
    yield (
      txId,
      ensureMinAda(
        Babbage(
          address = peer.address,
          value = Value(Coin(0L)),
          datumOption = None,
          scriptRef = None
        ),
        params
      ).asInstanceOf[Babbage]
    ).focus(_._2.value).modify(_ + Value.lovelace(min))
