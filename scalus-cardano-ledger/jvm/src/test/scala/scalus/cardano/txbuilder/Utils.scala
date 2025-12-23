package scalus.cardano.txbuilder

import monocle.syntax.all.*
import org.scalacheck.*
import org.scalacheck.Gen.posNum
import scalus.cardano.address.Network.Mainnet
import scalus.cardano.address.ShelleyPaymentPart.Key
import scalus.cardano.address.{Network, ShelleyAddress, ShelleyDelegationPart}
import scalus.cardano.ledger.*
import scalus.cardano.ledger.TransactionOutput.Babbage
import scalus.cardano.txbuilder.TransactionBuilder.ensureMinAda
import scalus.ledger.api.v1.ArbitraryInstances.genByteStringOfN

val blockfrost544Params: ProtocolParams = CardanoInfo.mainnet.protocolParams

val genTransactionInput: Gen[TransactionInput] =
    for {
        txId <- genByteStringOfN(32).map(TransactionHash.fromByteString)
        index <- posNum[Int] // we subtract one below to get a non-negative

    } yield Input(transactionId = txId, index = index - 1)

val genAddrKeyHash: Gen[AddrKeyHash] =
    genByteStringOfN(28).map(AddrKeyHash.fromByteString)

val genScriptHash: Gen[ScriptHash] = genByteStringOfN(28).map(ScriptHash.fromByteString)

val genPolicyId: Gen[PolicyId] = genScriptHash

def genPubkeyAddr(
    network: Network = Mainnet,
    delegation: ShelleyDelegationPart = ShelleyDelegationPart.Null
): Gen[ShelleyAddress] =
    genAddrKeyHash.flatMap(akh =>
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
    params: ProtocolParams = blockfrost544Params,
    min: Long = 0L
): Gen[(TransactionInput, Babbage)] =
    for txId <- genTransactionInput
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
