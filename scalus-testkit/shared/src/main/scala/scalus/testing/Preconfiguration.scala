package scalus.testing

import scalus.cardano.address.{Address, Network}
import scalus.cardano.ledger.*
import scalus.testing.kit.Party
import scalus.uplc.builtin.ByteString
import upickle.default.*

/** A single UTxO entry in the preconfiguration.
  *
  * @param ada
  *   lovelace amount
  * @param tx_id
  *   optional transaction hash hex; if absent, a genesis hash is used
  * @param idx
  *   optional output index; if absent, auto-assigned sequentially
  */
case class UtxoEntry(
    ada: Long,
    tx_id: Option[String] = None,
    idx: Option[Int] = None
) derives ReadWriter

/** Declarative emulator preconfiguration.
  *
  * Keys in `utxo` are either party names (case-insensitive, e.g. "alice") or raw bech32/base58
  * addresses. Values are sequences of UTxO entries to create at that address.
  *
  * Example JSON:
  * {{{
  * {
  *   "utxo": {
  *     "alice": [{ "ada": 100000000000 }],
  *     "bob":   [{ "ada": 5000000 }],
  *     "addr_test1qz...": [{ "ada": 50000000, "tx_id": "abcd...", "idx": 0 }]
  *   }
  * }
  * }}}
  */
case class Preconfiguration(
    utxo: Map[String, Seq[UtxoEntry]] = Map.empty
) derives ReadWriter

object Preconfiguration {

    /** Parse a JSON string into a [[Preconfiguration]]. */
    def fromJson(json: String): Preconfiguration = read[Preconfiguration](json)

    /** Resolve a preconfiguration into a UTxO map.
      *
      * Party names (case-insensitive) are resolved to their test addresses on the given network.
      * Raw address strings are parsed as bech32 or base58. Entries without `tx_id` use a genesis
      * hash; entries without `idx` get auto-assigned indices.
      */
    def resolveUtxos(
        config: Preconfiguration,
        network: Network = Network.Testnet
    ): Utxos = {
        val genesisHash = TransactionHash.fromByteString(ByteString.fromHex("0" * 64))
        var autoIdx = 0
        config.utxo.flatMap { case (addressKey, entries) =>
            val address = resolveAddress(addressKey, network)
            entries.map { entry =>
                val txHash = entry.tx_id.map(TransactionHash.fromHex).getOrElse(genesisHash)
                val idx = entry.idx.getOrElse {
                    val i = autoIdx
                    autoIdx += 1
                    i
                }
                val value = Value.lovelace(entry.ada)
                Input(txHash, idx) -> Output(address, value)
            }
        }.toMap
    }

    private def resolveAddress(key: String, network: Network): Address =
        Party.values.find(_.toString.equalsIgnoreCase(key)) match
            case Some(party) => party.address(network)
            case None        => Address.fromString(key)
}
