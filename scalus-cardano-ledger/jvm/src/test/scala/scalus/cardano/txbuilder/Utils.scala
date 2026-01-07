package scalus.cardano.txbuilder

import monocle.syntax.all.*
import org.scalacheck.*
import scalus.cardano.ledger.*
import scalus.cardano.ledger.ArbitraryInstances.given
import scalus.cardano.ledger.TransactionOutput.Babbage
import scalus.cardano.txbuilder.TransactionBuilder.ensureMinAda

/** Ada-only pub key utxo from the given peer, at least `min` Ada, random tx id, random index, no
  * datum, no script ref
  */
def genAdaOnlyPubKeyUtxo(
    party: Party,
    params: ProtocolParams = CardanoInfo.mainnet.protocolParams,
    min: Long = 0L
): Gen[(TransactionInput, Babbage)] =
    for txId <- Arbitrary.arbitrary[TransactionInput]
    yield (
      txId,
      ensureMinAda(
        Babbage(
          address = party.address,
          value = Value(Coin(0L)),
          datumOption = None,
          scriptRef = None
        ),
        params
      ).asInstanceOf[Babbage]
    ).focus(_._2.value).modify(_ + Value.lovelace(min))
