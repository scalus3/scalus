package scalus.examples

import org.scalatest.funsuite.AnyFunSuite
import scalus.cardano.address.*
import scalus.cardano.ledger.*
import scalus.cardano.node.Emulator
import scalus.cardano.txbuilder.TxBuilder
import scalus.crypto.tree.MerkleTree
import scalus.testing.kit.{Party, ScalusTest}
import scalus.uplc.builtin.{ByteString, Data}
import scalus.uplc.builtin.Data.toData
import scalus.utils.await

class MembershipTokenTest extends AnyFunSuite with ScalusTest {

    private given env: CardanoInfo = CardanoInfo.mainnet

    private val contract = MembershipTokenContract.withErrorTraces

    // Build a Merkle tree from Alice, Bob, and some extras (addrKeyHash <: ByteString)
    private val memberPkhs: IndexedSeq[ByteString] = IndexedSeq(
      Party.Alice.addrKeyHash,
      Party.Bob.addrKeyHash,
      Party.Charles.addrKeyHash,
      Party.Dave.addrKeyHash,
      Party.Eve.addrKeyHash
    )
    private val tree = MerkleTree.fromElements(memberPkhs)

    // Apply Merkle root to get the parameterized script
    private val applied = contract(tree.rootHash)
    private val script = applied.script
    private val policyId = script.scriptHash
    private val scriptAddress = ShelleyAddress(
      env.network,
      ShelleyPaymentPart.Script(policyId),
      ShelleyDelegationPart.Null
    )

    test("Alice can mint membership token with valid Merkle proof") {
        val emulator = Emulator.withAddresses(Seq(Party.Alice.address))
        val aliceUtxos = emulator.findUtxos(Party.Alice.address).await().toOption.get

        val alicePkh: ByteString = Party.Alice.addrKeyHash
        val proof = tree.proveMembership(alicePkh)
        val redeemer = MembershipRedeemer.Mint(Data.B(proof)).toData
        val tokenName = AssetName(alicePkh)
        val datum = MembershipDatum(alicePkh)

        val tx = TxBuilder(env)
            .mint(applied, Map(tokenName -> 1L), _ => redeemer)
            .requireSignature(Party.Alice.addrKeyHash)
            .payTo(Party.Alice.address, Value.asset(policyId, tokenName, 1L))
            .payTo(scriptAddress, Value.lovelace(2_000_000L), datum)
            .complete(aliceUtxos, Party.Alice.address)
            .sign(Party.Alice.signer)
            .transaction

        val result = emulator.submit(tx).await()
        assert(result.isRight, s"Mint should succeed: ${result.left.toOption.getOrElse("")}")
    }

    test("Alice can burn membership token and reclaim deposit") {
        val emulator = Emulator.withAddresses(Seq(Party.Alice.address))

        // Step 1: Mint
        val aliceUtxos1 = emulator.findUtxos(Party.Alice.address).await().toOption.get

        val alicePkh: ByteString = Party.Alice.addrKeyHash
        val proof = tree.proveMembership(alicePkh)
        val mintRedeemer = MembershipRedeemer.Mint(Data.B(proof)).toData
        val tokenName = AssetName(alicePkh)
        val datum = MembershipDatum(alicePkh)

        val mintTx = TxBuilder(env)
            .mint(
              applied,
              Map(tokenName -> 1L),
              _ => mintRedeemer
            )
            .requireSignature(Party.Alice.addrKeyHash)
            .payTo(Party.Alice.address, Value.asset(policyId, tokenName, 1L))
            .payTo(scriptAddress, Value.lovelace(2_000_000L), datum)
            .complete(aliceUtxos1, Party.Alice.address)
            .sign(Party.Alice.signer)
            .transaction

        val mintResult = emulator.submit(mintTx).await()
        assert(
          mintResult.isRight,
          s"Mint should succeed: ${mintResult.left.toOption.getOrElse("")}"
        )

        // Step 2: Find the deposit UTxO at the script address
        val depositUtxo = mintTx.utxos
            .find { case (_, txOut) => txOut.address == scriptAddress }
            .map(Utxo(_))
            .getOrElse(fail("No deposit UTxO found"))

        // Step 3: Burn token + reclaim deposit
        val aliceUtxos2 = emulator.findUtxos(Party.Alice.address).await().toOption.get
        val burnRedeemer = MembershipRedeemer.Burn.toData

        val burnTx = TxBuilder(env)
            .mint(
              applied,
              Map(tokenName -> -1L),
              _ => burnRedeemer
            )
            .requireSignature(Party.Alice.addrKeyHash)
            .spend(depositUtxo, burnRedeemer, script)
            .payTo(Party.Alice.address, Value.lovelace(2_000_000L))
            .complete(aliceUtxos2, Party.Alice.address)
            .sign(Party.Alice.signer)
            .transaction

        val burnResult = emulator.submit(burnTx).await()
        assert(
          burnResult.isRight,
          s"Burn should succeed: ${burnResult.left.toOption.getOrElse("")}"
        )
    }

    test("non-member cannot mint membership token") {
        val emulator = Emulator.withAddresses(Seq(Party.Faith.address))

        val faithUtxos = emulator.findUtxos(Party.Faith.address).await().toOption.get

        // Faith is NOT in the Merkle tree — use Alice's proof (wrong for Faith's pkh)
        val alicePkh: ByteString = Party.Alice.addrKeyHash
        val faithPkh: ByteString = Party.Faith.addrKeyHash
        val proof = tree.proveMembership(alicePkh)
        val redeemer = MembershipRedeemer.Mint(Data.B(proof)).toData
        val tokenName = AssetName(faithPkh)
        val datum = MembershipDatum(faithPkh)

        val result = scala.util.Try {
            TxBuilder(env)
                .mint(
                  applied,
                  Map(tokenName -> 1L),
                  _ => redeemer
                )
                .requireSignature(Party.Faith.addrKeyHash)
                .payTo(Party.Faith.address, Value.asset(policyId, tokenName, 1L))
                .payTo(scriptAddress, Value.lovelace(2_000_000L), datum)
                .complete(faithUtxos, Party.Faith.address)
        }

        assert(result.isFailure, "Non-member minting should fail")
    }
}
