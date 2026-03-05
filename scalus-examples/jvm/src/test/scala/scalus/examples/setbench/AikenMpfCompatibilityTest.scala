package scalus.examples.setbench

import org.scalatest.funsuite.AnyFunSuite
import scalus.cardano.address.Address
import scalus.cardano.blueprint.Blueprint
import scalus.cardano.ledger.*
import scalus.cardano.node.Emulator
import scalus.crypto.trie.MerklePatriciaForestry as Mpf16o
import scalus.cardano.onchain.plutus.crypto.trie.MerklePatriciaForestry.*
import scalus.cardano.txbuilder.*
import scalus.testing.kit.Party.{Alice, Bob}
import scalus.testing.kit.{ScalusTest, TestUtil}
import scalus.uplc.Program
import scalus.uplc.builtin.Data.toData
import scalus.uplc.builtin.{ByteString, Data}
import scalus.utils.await

/** Cross-implementation compatibility test: runs our off-chain MPF-16o proofs against an
  * Aiken-compiled MPF validator to verify proof encoding is identical.
  *
  * The Aiken validator was compiled from https://github.com/aiken-lang/merkle-patricia-forestry
  * with a thin SetBench-compatible wrapper (same datum/redeemer types).
  */
class AikenMpfCompatibilityTest extends AnyFunSuite with ScalusTest {

    private given env: CardanoInfo = TestUtil.testEnvironment

    private val K = 2_000_000L

    private lazy val aikenScript: Script.PlutusV3 = {
        val fname = "/scalus/examples/AikenMpfData/plutus.json"
        val inputStream = this.getClass.getResourceAsStream(fname)
        if inputStream == null then throw new RuntimeException(s"Resource not found: $fname")
        val blueprint = Blueprint.fromJson(inputStream)
        val program = blueprint.validators.head.compiledCode.map(Program.fromCborHex).get
        Script.PlutusV3(program.cborByteString)
    }

    private lazy val aikenAddress: Address =
        Address(env.network, Credential.ScriptHash(aikenScript.scriptHash))

    private val builder = TxBuilder(env)

    private def publishAikenScript(
        utxos: Utxos,
        holder: Address,
        sponsor: Address,
        signer: TransactionSigner
    ): Transaction = {
        val scriptOutput = TransactionOutput(
          holder,
          Value.lovelace(10_000_000L),
          None,
          Some(ScriptRef(aikenScript))
        )
        builder
            .output(scriptOutput)
            .complete(availableUtxos = utxos, sponsor = sponsor)
            .sign(signer)
            .transaction
    }

    private def lockAiken(
        utxos: Utxos,
        totalLovelace: Long,
        initialRoot: ByteString,
        sponsor: Address,
        signer: TransactionSigner
    ): Transaction = {
        val datum = SetBenchDatum(BigInt(totalLovelace), initialRoot)
        builder
            .payTo(aikenAddress, Value.lovelace(totalLovelace), datum)
            .complete(availableUtxos = utxos, sponsor = sponsor)
            .sign(signer)
            .transaction
    }

    private def withdrawAiken(
        utxos: Utxos,
        contractUtxo: Utxo,
        refScriptUtxo: Utxo,
        redeemer: Data,
        newDatum: SetBenchDatum,
        k: Long,
        withdrawTo: Address,
        sponsor: Address,
        signer: TransactionSigner
    ): Transaction = {
        val newLovelace = contractUtxo.output.value.coin.value - k
        builder
            .references(refScriptUtxo)
            .spend(contractUtxo, redeemer)
            .payTo(withdrawTo, Value.lovelace(k))
            .payTo(aikenAddress, Value.lovelace(newLovelace), newDatum)
            .complete(availableUtxos = utxos, sponsor = sponsor)
            .sign(signer)
            .transaction
    }

    private def depositAiken(
        utxos: Utxos,
        contractUtxo: Utxo,
        refScriptUtxo: Utxo,
        redeemer: Data,
        newDatum: SetBenchDatum,
        k: Long,
        sponsor: Address,
        signer: TransactionSigner
    ): Transaction = {
        val newLovelace = contractUtxo.output.value.coin.value + k
        builder
            .references(refScriptUtxo)
            .spend(contractUtxo, redeemer)
            .payTo(aikenAddress, Value.lovelace(newLovelace), newDatum)
            .complete(availableUtxos = utxos, sponsor = sponsor)
            .sign(signer)
            .transaction
    }

    private def findContractUtxo(tx: Transaction): Utxo =
        tx.utxos
            .find { case (_, txOut) => txOut.address == aikenAddress }
            .map(Utxo(_))
            .getOrElse(fail("No contract UTxO found"))

    private def findRefScriptUtxo(tx: Transaction): Utxo =
        tx.utxos
            .find { case (_, txOut) => txOut.scriptRef.isDefined }
            .map(Utxo(_))
            .getOrElse(fail("No reference script UTxO found"))

    test("Aiken MPF delete: Scalus off-chain proofs work with Aiken on-chain validator") {
        val n = 100
        val rng = new scala.util.Random(42)
        val elems = Vector.tabulate(n) { i =>
            val key = ByteString.fromString(s"element-${rng.nextInt()}-$i")
            val value = ByteString.fromString(s"value-$i")
            (key, value)
        }

        var trie = Mpf16o.fromList(elems)
        info(s"Trie built with $n elements, root=${trie.rootHash.toHex.take(16)}...")

        val emulator = Emulator.withAddresses(Seq(Alice.address, Bob.address))
        var aliceUtxos = emulator.findUtxos(Alice.address).await().toOption.get

        val publishTx = publishAikenScript(aliceUtxos, Bob.address, Alice.address, Alice.signer)
        assert(emulator.submit(publishTx).await().isRight, "Publish script failed")
        val refScriptUtxo = findRefScriptUtxo(publishTx)

        aliceUtxos = emulator.findUtxos(Alice.address).await().toOption.get
        val lockAmount = 20 * K
        val lockTx = lockAiken(aliceUtxos, lockAmount, trie.rootHash, Alice.address, Alice.signer)
        assert(emulator.submit(lockTx).await().isRight, "Lock tx failed")

        var contractUtxo = findContractUtxo(lockTx)
        var remaining = lockAmount

        // Delete 5 elements
        val sampleIndices = rng.shuffle((0 until n).toList).take(5)
        for (idx, i) <- sampleIndices.zipWithIndex do
            val (key, value) = elems(idx)
            val proofData = trie.proveMembership(key).toData

            trie = trie.delete(key)
            remaining -= K
            val newDatum = SetBenchDatum(BigInt(remaining), trie.rootHash)
            val redeemer = SetBenchRedeemer.Withdraw(key, value, proofData).toData

            val sponsorUtxos = emulator.findUtxos(Alice.address).await().toOption.get
            val tx = withdrawAiken(
              sponsorUtxos,
              contractUtxo,
              refScriptUtxo,
              redeemer,
              newDatum,
              K,
              Bob.address,
              Alice.address,
              Alice.signer
            )

            val result = emulator.submit(tx).await()
            assert(result.isRight, s"Aiken withdraw $i failed: $result")
            info(s"  Aiken delete $i OK (key=${key.toHex.take(16)}...)")

            contractUtxo = findContractUtxo(tx)

        info("All Aiken MPF delete operations succeeded with Scalus-generated proofs")
    }

    test("Aiken MPF insert: Scalus off-chain proofs work with Aiken on-chain validator") {
        val n = 100
        val rng = new scala.util.Random(42)
        val elems = Vector.tabulate(n) { i =>
            val key = ByteString.fromString(s"element-${rng.nextInt()}-$i")
            val value = ByteString.fromString(s"value-$i")
            (key, value)
        }

        var trie = Mpf16o.fromList(elems)
        info(s"Trie built with $n elements, root=${trie.rootHash.toHex.take(16)}...")

        val emulator = Emulator.withAddresses(Seq(Alice.address, Bob.address))
        var aliceUtxos = emulator.findUtxos(Alice.address).await().toOption.get

        val publishTx = publishAikenScript(aliceUtxos, Bob.address, Alice.address, Alice.signer)
        assert(emulator.submit(publishTx).await().isRight, "Publish script failed")
        val refScriptUtxo = findRefScriptUtxo(publishTx)

        aliceUtxos = emulator.findUtxos(Alice.address).await().toOption.get
        val lockAmount = 10 * K
        val lockTx = lockAiken(aliceUtxos, lockAmount, trie.rootHash, Alice.address, Alice.signer)
        assert(emulator.submit(lockTx).await().isRight, "Lock tx failed")

        var contractUtxo = findContractUtxo(lockTx)
        var remaining = lockAmount

        // Insert 5 new elements
        val newElems = Vector.tabulate(5) { i =>
            val key = ByteString.fromString(s"new-element-${rng.nextInt()}-$i")
            val value = ByteString.fromString(s"new-value-$i")
            (key, value)
        }

        for ((key, value), i) <- newElems.zipWithIndex do
            val proofData = trie.proveNonMembership(key).toData

            trie = trie.insert(key, value)
            remaining += K
            val newDatum = SetBenchDatum(BigInt(remaining), trie.rootHash)
            val redeemer = SetBenchRedeemer.Deposit(key, value, proofData).toData

            val sponsorUtxos = emulator.findUtxos(Alice.address).await().toOption.get
            val tx = depositAiken(
              sponsorUtxos,
              contractUtxo,
              refScriptUtxo,
              redeemer,
              newDatum,
              K,
              Alice.address,
              Alice.signer
            )

            val result = emulator.submit(tx).await()
            assert(result.isRight, s"Aiken deposit $i failed: $result")
            info(s"  Aiken insert $i OK (key=${key.toHex.take(16)}...)")

            contractUtxo = findContractUtxo(tx)

        info("All Aiken MPF insert operations succeeded with Scalus-generated proofs")
    }
}
