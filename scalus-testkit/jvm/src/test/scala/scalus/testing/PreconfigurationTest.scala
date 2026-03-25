package scalus.testing

import org.scalatest.funsuite.AnyFunSuite
import scalus.cardano.address.Network
import scalus.cardano.ledger.*
import scalus.testing.kit.Party
import scalus.uplc.builtin.ByteString

class PreconfigurationTest extends AnyFunSuite {

    private val network = Network.Testnet

    test("parse and resolve single party with ada") {
        val json = """{ "utxo": { "alice": [{ "ada": 100000000 }] } }"""
        val config = Preconfiguration.fromJson(json)

        assert(config.utxo.size == 1)
        assert(config.utxo.contains("alice"))
        assert(config.utxo("alice").head.ada == 100L)

        val utxos = Preconfiguration.resolveUtxos(config, network)
        assert(utxos.size == 1)

        val (input, output) = utxos.head
        assert(output.address == Party.Alice.address(network))
        assert(output.value == Value.ada(100L))
    }

    test("parse and resolve multiple parties") {
        val json = """{
          "utxo": {
            "alice": [{ "ada": 100000000000 }],
            "bob":   [{ "ada": 50000000 }],
            "charles": [{ "ada": 25000000 }]
          }
        }"""
        val config = Preconfiguration.fromJson(json)
        val utxos = Preconfiguration.resolveUtxos(config, network)

        assert(utxos.size == 3)
        val addresses = utxos.values.map(_.address).toSet
        assert(addresses.contains(Party.Alice.address(network)))
        assert(addresses.contains(Party.Bob.address(network)))
        assert(addresses.contains(Party.Charles.address(network)))
    }

    test("party name resolution is case-insensitive") {
        val json = """{ "utxo": { "Alice": [{ "ada": 1000 }] } }"""
        val utxos = Preconfiguration.resolveUtxos(Preconfiguration.fromJson(json), network)
        val output = utxos.values.head
        assert(output.address == Party.Alice.address(network))
    }

    test("resolve raw bech32 address") {
        val aliceAddr = Party.Alice.address(network)
        val bech32 = aliceAddr.toBech32.get
        val json = s"""{ "utxo": { "$bech32": [{ "ada": 5000000 }] } }"""
        val utxos = Preconfiguration.resolveUtxos(Preconfiguration.fromJson(json), network)
        val output = utxos.values.head
        assert(output.address == aliceAddr)
    }

    test("multiple utxos for same address get distinct indices") {
        val json = """{
          "utxo": {
            "alice": [
              { "ada": 1000000 },
              { "ada": 2000000 },
              { "ada": 3000000 }
            ]
          }
        }"""
        val utxos = Preconfiguration.resolveUtxos(Preconfiguration.fromJson(json), network)
        assert(utxos.size == 3)

        val indices = utxos.keys.map(_.index).toSet
        assert(indices.size == 3)
    }

    test("explicit tx_id and idx are used") {
        val txId = "ab" * 32
        val json = s"""{ "utxo": { "alice": [{ "ada": 5000000, "tx_id": "$txId", "idx": 7 }] } }"""
        val utxos = Preconfiguration.resolveUtxos(Preconfiguration.fromJson(json), network)
        val (input, _) = utxos.head
        assert(input.transactionId == TransactionHash.fromHex(txId))
        assert(input.index == 7)
    }

    test("entries without tx_id use genesis hash") {
        val json = """{ "utxo": { "alice": [{ "ada": 1000000 }] } }"""
        val utxos = Preconfiguration.resolveUtxos(Preconfiguration.fromJson(json), network)
        val genesisHash = TransactionHash.fromByteString(ByteString.fromHex("0" * 64))
        val (input, _) = utxos.head
        assert(input.transactionId == genesisHash)
    }

    test("ImmutableEmulator.fromJson creates working emulator") {
        val json = """{
          "utxo": {
            "alice": [{ "ada": 100000000000 }],
            "bob":   [{ "ada": 50000000000 }]
          }
        }"""
        val emulator = ImmutableEmulator.fromJson(json)
        assert(emulator.utxos.size == 2)
    }

    test("empty preconfiguration") {
        val json = """{ "utxo": {} }"""
        val utxos = Preconfiguration.resolveUtxos(Preconfiguration.fromJson(json), network)
        assert(utxos.isEmpty)
    }

    test("unknown party name throws") {
        val json = """{ "utxo": { "unknown_party": [{ "ada": 1000 }] } }"""
        assertThrows[Exception] {
            Preconfiguration.resolveUtxos(Preconfiguration.fromJson(json), network)
        }
    }
}
