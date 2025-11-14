package scalus.cardano.txbuilder

import monocle.syntax.all.*
import org.scalacheck.*
import org.scalacheck.Gen.{const, posNum}
import scalus.builtin.Data.toData
import scalus.builtin.{ByteString, Data}
import scalus.cardano.address.Network.Mainnet
import scalus.cardano.address.ShelleyPaymentPart.Key
import scalus.cardano.address.{Network, ShelleyAddress, ShelleyDelegationPart, ShelleyPaymentPart}
import scalus.cardano.ledger.*
import scalus.cardano.ledger.DatumOption.Inline
import scalus.cardano.ledger.TransactionOutput.Babbage
import scalus.cardano.txbuilder.TestPeer.Alice
import scalus.cardano.txbuilder.TransactionBuilder.ensureMinAda
import scalus.ledger.api.v1.ArbitraryInstances.genByteStringOfN
import scalus.prelude.Option as SOption

import scala.language.postfixOps

val blockfrost544Params: ProtocolParams = CardanoInfo.mainnet.protocolParams

val genTransactionInput: Gen[TransactionInput] =
    for {
        txId <- genByteStringOfN(32).map(TransactionHash.fromByteString)
        index <- posNum[Int] // we subtract one below to get a non-negative

    } yield TransactionInput(transactionId = txId, index = index - 1)

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

def genScriptAddr(
    network: Network = Mainnet,
    delegation: ShelleyDelegationPart = ShelleyDelegationPart.Null
): Gen[ShelleyAddress] =
    for {
        sh <- genScriptHash
    } yield ShelleyAddress(
      network = network,
      payment = ShelleyPaymentPart.Script(sh),
      delegation = delegation
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
    for {
        txId <- genTransactionInput
        value <- genAdaOnlyValue
    } yield (
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

// Get the minAda for an Ada only pubkey utxo
def minPubkeyAda(params: ProtocolParams = blockfrost544Params) = {
    val utxo = genAdaOnlyPubKeyUtxo(Alice).sample.get.focus(_._2.value.coin.value).replace(0L)
    ensureMinAda(utxo._2, blockfrost544Params).value.coin
}

def sumUtxoValues(utxos: Seq[(TransactionInput, TransactionOutput)]): Value =
    utxos.map(_._2.value).foldLeft(Value.zero)((acc: Value, v: Value) => acc + v)

/** Generate random bytestring data. Good for testing user-provided, untrusted data against size
  * attacks
  */
def genByteStringData: Gen[Data] =
    Gen.sized(size => genByteStringOfN(size).flatMap(_.toData))

/** Generate an inline datum with random bytestring data. Optionally, set the relative frequencies
  * for an empty datum
  */
def genByteStringInlineDatumOption(
    noneFrequency: Int = 0,
    someFrequency: Int = 1
): Gen[SOption[DatumOption]] =
    Gen.frequency(
      (someFrequency, genByteStringData.map(data => SOption.Some(Inline(data)))),
      (noneFrequency, SOption.None)
    )
