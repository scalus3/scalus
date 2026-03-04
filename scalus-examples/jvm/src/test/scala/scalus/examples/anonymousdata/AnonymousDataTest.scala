package scalus.examples.anonymousdata

import org.scalatest.funsuite.AnyFunSuite
import scalus.uplc.builtin.ByteString
import scalus.uplc.builtin.ByteString.utf8
import scalus.cardano.ledger.*
import scalus.cardano.ledger.rules.Context
import scalus.cardano.node.Emulator
import scalus.crypto.tree.MerkleTree
import scalus.testing.kit.Party.{Alice, Bob, Charles, Dave, Eve, Faith}
import scalus.testing.kit.ScalusTest
import scalus.testing.kit.TestUtil.{genesisHash, testEnvironment}
import scalus.utils.await

class AnonymousDataTest extends AnyFunSuite with ScalusTest {
    import AnonymousDataTest.{*, given}

    test(
      s"AnonymousData validator size is ${AnonymousDataContract.script.script.size} bytes"
    ) {
        info(s"Validator size: ${AnonymousDataContract.script.script.size} bytes")
    }

    test("Initialize shared UTXO with beacon token") {
        val provider = createProvider()
        val utxos = provider.findUtxos(Alice.address).await().toOption.get
        val txCreator = createTxCreator()

        val tx = txCreator.initialize(
          utxos = utxos,
          participantsRoot = tree.rootHash,
          changeAddress = Alice.address,
          signer = Alice.signer
        )

        val result = provider.submit(tx).await()
        assert(result.isRight, s"Initialize should succeed: $result")

        // Verify beacon at script address
        val scriptOutput = tx.utxos
            .find(_._2.address == txCreator.scriptAddr)
            .getOrElse(fail("Shared UTXO missing from output"))

        val datum = scriptOutput._2.inlineDatum.get.to[AnonymousDataDatum]
        assert(datum.participantsRoot == tree.rootHash, "participantsRoot should match tree root")
        assert(datum.dataMap.isEmpty, "dataMap should be empty initially")
    }

    test("Store data: participant with valid proof can add entry") {
        val provider = createProvider()
        val txCreator = createTxCreator()

        // Initialize
        val utxos1 = provider.findUtxos(Alice.address).await().toOption.get
        val initTx = txCreator.initialize(utxos1, tree.rootHash, Alice.address, Alice.signer)
        provider.submit(initTx).await()
        val sharedUtxo = findSharedUtxo(initTx, txCreator)

        // Bob stores data
        val nonce = utf8"bob_secret_nonce"
        val dataKey = AnonymousDataCrypto.deriveKey(Bob.addrKeyHash, nonce)
        val encKey = AnonymousDataCrypto.deriveEncKey(nonce)
        val plainData = utf8"Bob's secret data"
        val encryptedData = AnonymousDataCrypto.encrypt(plainData, encKey)

        val bobUtxos = provider.findUtxos(Bob.address).await().toOption.get
        val storeTx = txCreator.storeData(
          utxos = bobUtxos,
          sharedUtxo = sharedUtxo,
          tree = tree,
          signerPkh = Bob.addrKeyHash,
          dataKey = dataKey,
          encryptedData = encryptedData,
          changeAddress = Bob.address,
          signer = Bob.signer
        )

        val result = provider.submit(storeTx).await()
        assert(result.isRight, s"Store data should succeed: $result")

        // Verify entry in datum
        val newSharedUtxo = findSharedUtxo(storeTx, txCreator)
        val newDatum = newSharedUtxo.output.inlineDatum.get.to[AnonymousDataDatum]
        assert(newDatum.dataMap.contains(dataKey), "dataMap should contain the new entry")
    }

    test("Store data fails for non-participant") {
        val provider = createProvider()
        val txCreator = createTxCreator()

        // Initialize
        val utxos1 = provider.findUtxos(Alice.address).await().toOption.get
        val initTx = txCreator.initialize(utxos1, tree.rootHash, Alice.address, Alice.signer)
        provider.submit(initTx).await()
        val sharedUtxo = findSharedUtxo(initTx, txCreator)

        // Faith (not in tree) tries to store data using Bob's proof (won't match Faith's pkh)
        val nonce = utf8"faith_nonce"
        val dataKey = AnonymousDataCrypto.deriveKey(Faith.addrKeyHash, nonce)
        val encKey = AnonymousDataCrypto.deriveEncKey(nonce)
        val encryptedData = AnonymousDataCrypto.encrypt(utf8"Faith's data", encKey)

        val faithUtxos = provider.findUtxos(Faith.address).await().toOption.get

        val result = scala.util.Try {
            // Faith is not in the tree, so proveMembership will fail
            txCreator.storeData(
              utxos = faithUtxos,
              sharedUtxo = sharedUtxo,
              tree = tree,
              signerPkh = Faith.addrKeyHash,
              dataKey = dataKey,
              encryptedData = encryptedData,
              changeAddress = Faith.address,
              signer = Faith.signer
            )
        }
        assert(result.isFailure, "Non-participant storing data should fail")
    }

    test("Read data off-chain: decrypt with nonce") {
        val nonce = utf8"test_nonce_123"
        val encKey = AnonymousDataCrypto.deriveEncKey(nonce)
        val plainData = utf8"Hello, anonymous world!"
        val encrypted = AnonymousDataCrypto.encrypt(plainData, encKey)
        val decrypted = AnonymousDataCrypto.decrypt(encrypted, encKey)
        assert(decrypted == plainData, "Decrypted data should match original")
    }

    test("Encryption with data longer than 32 bytes") {
        val nonce = utf8"long_data_nonce"
        val encKey = AnonymousDataCrypto.deriveEncKey(nonce)
        val plainData = ByteString.fromString("A" * 100)
        val encrypted = AnonymousDataCrypto.encrypt(plainData, encKey)
        val decrypted = AnonymousDataCrypto.decrypt(encrypted, encKey)
        assert(decrypted == plainData, "Decrypted long data should match original")
    }

    test("Update data: participant can update their entry") {
        val provider = createProvider()
        val txCreator = createTxCreator()

        // Initialize
        val utxos1 = provider.findUtxos(Alice.address).await().toOption.get
        val initTx = txCreator.initialize(utxos1, tree.rootHash, Alice.address, Alice.signer)
        provider.submit(initTx).await()
        val sharedUtxo1 = findSharedUtxo(initTx, txCreator)

        // Bob stores data
        val nonce = utf8"bob_nonce"
        val dataKey = AnonymousDataCrypto.deriveKey(Bob.addrKeyHash, nonce)
        val encKey = AnonymousDataCrypto.deriveEncKey(nonce)
        val encData1 = AnonymousDataCrypto.encrypt(utf8"version1", encKey)

        val bobUtxos1 = provider.findUtxos(Bob.address).await().toOption.get
        val storeTx = txCreator.storeData(
          bobUtxos1,
          sharedUtxo1,
          tree,
          Bob.addrKeyHash,
          dataKey,
          encData1,
          Bob.address,
          Bob.signer
        )
        provider.submit(storeTx).await()
        val sharedUtxo2 = findSharedUtxo(storeTx, txCreator)

        // Bob updates data
        val encData2 = AnonymousDataCrypto.encrypt(utf8"version2", encKey)
        val bobUtxos2 = provider.findUtxos(Bob.address).await().toOption.get
        val updateTx = txCreator.updateData(
          bobUtxos2,
          sharedUtxo2,
          tree,
          Bob.addrKeyHash,
          dataKey,
          encData2,
          Bob.address,
          Bob.signer
        )

        val result = provider.submit(updateTx).await()
        assert(result.isRight, s"Update data should succeed: $result")

        // Verify updated entry
        val sharedUtxo3 = findSharedUtxo(updateTx, txCreator)
        val datum = sharedUtxo3.output.inlineDatum.get.to[AnonymousDataDatum]
        val storedEnc = datum.dataMap.get(dataKey)
        assert(storedEnc.isDefined, "Entry should exist after update")
    }

    test("Delete data: participant can remove their entry") {
        val provider = createProvider()
        val txCreator = createTxCreator()

        // Initialize
        val utxos1 = provider.findUtxos(Alice.address).await().toOption.get
        val initTx = txCreator.initialize(utxos1, tree.rootHash, Alice.address, Alice.signer)
        provider.submit(initTx).await()
        val sharedUtxo1 = findSharedUtxo(initTx, txCreator)

        // Charles stores data
        val nonce = utf8"charles_nonce"
        val dataKey = AnonymousDataCrypto.deriveKey(Charles.addrKeyHash, nonce)
        val encKey = AnonymousDataCrypto.deriveEncKey(nonce)
        val encData = AnonymousDataCrypto.encrypt(utf8"charles_data", encKey)

        val charlesUtxos1 = provider.findUtxos(Charles.address).await().toOption.get
        val storeTx = txCreator.storeData(
          charlesUtxos1,
          sharedUtxo1,
          tree,
          Charles.addrKeyHash,
          dataKey,
          encData,
          Charles.address,
          Charles.signer
        )
        provider.submit(storeTx).await()
        val sharedUtxo2 = findSharedUtxo(storeTx, txCreator)

        // Charles deletes data
        val charlesUtxos2 = provider.findUtxos(Charles.address).await().toOption.get
        val deleteTx = txCreator.deleteData(
          charlesUtxos2,
          sharedUtxo2,
          tree,
          Charles.addrKeyHash,
          dataKey,
          Charles.address,
          Charles.signer
        )

        val result = provider.submit(deleteTx).await()
        assert(result.isRight, s"Delete data should succeed: $result")

        // Verify entry removed
        val sharedUtxo3 = findSharedUtxo(deleteTx, txCreator)
        val datum = sharedUtxo3.output.inlineDatum.get.to[AnonymousDataDatum]
        assert(!datum.dataMap.contains(dataKey), "Entry should be removed after delete")
    }

    test("Update participants: admin-only") {
        val provider = createProvider()
        val txCreator = createTxCreator()

        // Initialize
        val utxos1 = provider.findUtxos(Alice.address).await().toOption.get
        val initTx = txCreator.initialize(utxos1, tree.rootHash, Alice.address, Alice.signer)
        provider.submit(initTx).await()
        val sharedUtxo = findSharedUtxo(initTx, txCreator)

        // Admin (Alice) updates participants
        val newTree = MerkleTree.fromElements(
          IndexedSeq(
            Alice.addrKeyHash,
            Bob.addrKeyHash,
            Faith.addrKeyHash
          )
        )

        val aliceUtxos = provider.findUtxos(Alice.address).await().toOption.get
        val updateTx = txCreator.updateParticipants(
          aliceUtxos,
          sharedUtxo,
          newTree.rootHash,
          Alice.address,
          Alice.signer
        )

        val result = provider.submit(updateTx).await()
        assert(result.isRight, s"Update participants should succeed: $result")

        // Verify new root
        val newSharedUtxo = findSharedUtxo(updateTx, txCreator)
        val datum = newSharedUtxo.output.inlineDatum.get.to[AnonymousDataDatum]
        assert(datum.participantsRoot == newTree.rootHash, "participantsRoot should be updated")
    }

    test("Multiple entries from different participants") {
        val provider = createProvider()
        val txCreator = createTxCreator()

        // Initialize
        val utxos1 = provider.findUtxos(Alice.address).await().toOption.get
        val initTx = txCreator.initialize(utxos1, tree.rootHash, Alice.address, Alice.signer)
        provider.submit(initTx).await()
        var sharedUtxo = findSharedUtxo(initTx, txCreator)

        // Bob stores entry
        val bobNonce = utf8"bob_n"
        val bobKey = AnonymousDataCrypto.deriveKey(Bob.addrKeyHash, bobNonce)
        val bobEncKey = AnonymousDataCrypto.deriveEncKey(bobNonce)
        val bobEnc = AnonymousDataCrypto.encrypt(utf8"bob_data", bobEncKey)

        val bobUtxos = provider.findUtxos(Bob.address).await().toOption.get
        val storeTx1 = txCreator.storeData(
          bobUtxos,
          sharedUtxo,
          tree,
          Bob.addrKeyHash,
          bobKey,
          bobEnc,
          Bob.address,
          Bob.signer
        )
        provider.submit(storeTx1).await()
        sharedUtxo = findSharedUtxo(storeTx1, txCreator)

        // Charles stores entry
        val charlesNonce = utf8"charles_n"
        val charlesKey = AnonymousDataCrypto.deriveKey(Charles.addrKeyHash, charlesNonce)
        val charlesEncKey = AnonymousDataCrypto.deriveEncKey(charlesNonce)
        val charlesEnc = AnonymousDataCrypto.encrypt(utf8"charles_data", charlesEncKey)

        val charlesUtxos = provider.findUtxos(Charles.address).await().toOption.get
        val storeTx2 = txCreator.storeData(
          charlesUtxos,
          sharedUtxo,
          tree,
          Charles.addrKeyHash,
          charlesKey,
          charlesEnc,
          Charles.address,
          Charles.signer
        )
        val result2 = provider.submit(storeTx2).await()
        assert(result2.isRight, s"Second store should succeed: $result2")

        // Verify both entries
        sharedUtxo = findSharedUtxo(storeTx2, txCreator)
        val datum = sharedUtxo.output.inlineDatum.get.to[AnonymousDataDatum]
        assert(datum.dataMap.contains(bobKey), "Bob's entry should exist")
        assert(datum.dataMap.contains(charlesKey), "Charles's entry should exist")
        assert(datum.dataMap.size == BigInt(2), "Should have exactly 2 entries")
    }

    test(
      s"Gate validator size is ${AnonymousDataGateContract.script.script.size} bytes"
    ) {
        info(s"Gate validator size: ${AnonymousDataGateContract.script.script.size} bytes")
    }

    test("Gate: lock and unlock with valid anonymous data proof") {
        val provider = createProvider()
        val txCreator = createTxCreator()

        // Initialize anonymous data
        val utxos1 = provider.findUtxos(Alice.address).await().toOption.get
        val initTx = txCreator.initialize(utxos1, tree.rootHash, Alice.address, Alice.signer)
        provider.submit(initTx).await()
        val sharedUtxo = findSharedUtxo(initTx, txCreator)

        // Bob stores data
        val nonce = utf8"bob_gate_nonce"
        val dataKey = AnonymousDataCrypto.deriveKey(Bob.addrKeyHash, nonce)
        val encKey = AnonymousDataCrypto.deriveEncKey(nonce)
        val plainData = utf8"secret_gate_data"
        val encryptedData = AnonymousDataCrypto.encrypt(plainData, encKey)

        val bobUtxos1 = provider.findUtxos(Bob.address).await().toOption.get
        val storeTx = txCreator.storeData(
          bobUtxos1,
          sharedUtxo,
          tree,
          Bob.addrKeyHash,
          dataKey,
          encryptedData,
          Bob.address,
          Bob.signer
        )
        provider.submit(storeTx).await()
        val sharedUtxoAfterStore = findSharedUtxo(storeTx, txCreator)

        // Dave creates a gate: lock funds requiring proof of Bob's data
        val expectedDataHash = scalus.uplc.builtin.Builtins.blake2b_256(plainData)
        val daveUtxos1 = provider.findUtxos(Dave.address).await().toOption.get
        val gateTx = txCreator.createGate(
          gateContract = AnonymousDataGateContract.withErrorTraces,
          expectedDataHash = expectedDataHash,
          creatorPkh = Dave.addrKeyHash,
          lockedValue = Value.ada(10),
          utxos = daveUtxos1,
          changeAddress = Dave.address,
          signer = Dave.signer
        )
        val gateResult = provider.submit(gateTx).await()
        assert(gateResult.isRight, s"Create gate should succeed: $gateResult")

        // Find the gate UTXO
        val gateScript =
            AnonymousDataGateContract.withErrorTraces.apply(txCreator.policyId: ByteString)
        val gateAddr = gateScript.address(testEnvironment.network)
        val gateUtxo = gateTx.utxos
            .find(_._2.address == gateAddr)
            .map(Utxo(_))
            .getOrElse(fail("Gate UTXO not found"))

        // Charles unlocks the gate using Bob's decryption key (identity-safe)
        val charlesUtxos = provider.findUtxos(Charles.address).await().toOption.get
        val unlockTx = txCreator.unlockGate(
          gateContract = AnonymousDataGateContract.withErrorTraces,
          gateUtxo = gateUtxo,
          sharedUtxo = sharedUtxoAfterStore,
          dataKey = dataKey,
          decKey = encKey,
          utxos = charlesUtxos,
          changeAddress = Charles.address,
          signer = Charles.signer
        )
        val unlockResult = provider.submit(unlockTx).await()
        assert(unlockResult.isRight, s"Unlock gate should succeed: $unlockResult")
    }

    test("Gate: unlock fails with wrong decKey") {
        val provider = createProvider()
        val txCreator = createTxCreator()

        // Initialize anonymous data
        val utxos1 = provider.findUtxos(Alice.address).await().toOption.get
        val initTx = txCreator.initialize(utxos1, tree.rootHash, Alice.address, Alice.signer)
        provider.submit(initTx).await()
        val sharedUtxo = findSharedUtxo(initTx, txCreator)

        // Bob stores data
        val nonce = utf8"bob_gate_nonce2"
        val dataKey = AnonymousDataCrypto.deriveKey(Bob.addrKeyHash, nonce)
        val encKey = AnonymousDataCrypto.deriveEncKey(nonce)
        val plainData = utf8"secret_data_2"
        val encryptedData = AnonymousDataCrypto.encrypt(plainData, encKey)

        val bobUtxos1 = provider.findUtxos(Bob.address).await().toOption.get
        val storeTx = txCreator.storeData(
          bobUtxos1,
          sharedUtxo,
          tree,
          Bob.addrKeyHash,
          dataKey,
          encryptedData,
          Bob.address,
          Bob.signer
        )
        provider.submit(storeTx).await()
        val sharedUtxoAfterStore = findSharedUtxo(storeTx, txCreator)

        // Dave creates a gate
        val expectedDataHash = scalus.uplc.builtin.Builtins.blake2b_256(plainData)
        val daveUtxos1 = provider.findUtxos(Dave.address).await().toOption.get
        val gateTx = txCreator.createGate(
          gateContract = AnonymousDataGateContract.withErrorTraces,
          expectedDataHash = expectedDataHash,
          creatorPkh = Dave.addrKeyHash,
          lockedValue = Value.ada(10),
          utxos = daveUtxos1,
          changeAddress = Dave.address,
          signer = Dave.signer
        )
        provider.submit(gateTx).await()

        val gateScript =
            AnonymousDataGateContract.withErrorTraces.apply(txCreator.policyId: ByteString)
        val gateAddr = gateScript.address(testEnvironment.network)
        val gateUtxo = gateTx.utxos
            .find(_._2.address == gateAddr)
            .map(Utxo(_))
            .getOrElse(fail("Gate UTXO not found"))

        // Try to unlock with wrong decKey
        val wrongDecKey = AnonymousDataCrypto.deriveEncKey(utf8"wrong_nonce")
        val charlesUtxos = provider.findUtxos(Charles.address).await().toOption.get

        val unlockTx = txCreator.unlockGate(
          gateContract = AnonymousDataGateContract.withErrorTraces,
          gateUtxo = gateUtxo,
          sharedUtxo = sharedUtxoAfterStore,
          dataKey = dataKey,
          decKey = wrongDecKey,
          utxos = charlesUtxos,
          changeAddress = Charles.address,
          signer = Charles.signer
        )
        val result = provider.submit(unlockTx).await()
        assert(result.isLeft, s"Unlock with wrong decKey should fail: $result")
    }

    test("Gate: creator can refund") {
        val provider = createProvider()
        val txCreator = createTxCreator()

        // Initialize anonymous data
        val utxos1 = provider.findUtxos(Alice.address).await().toOption.get
        val initTx = txCreator.initialize(utxos1, tree.rootHash, Alice.address, Alice.signer)
        provider.submit(initTx).await()

        // Dave creates a gate (no data stored — simulates deleted entry scenario)
        val expectedDataHash = scalus.uplc.builtin.Builtins.blake2b_256(utf8"some_data")
        val daveUtxos1 = provider.findUtxos(Dave.address).await().toOption.get
        val gateTx = txCreator.createGate(
          gateContract = AnonymousDataGateContract.withErrorTraces,
          expectedDataHash = expectedDataHash,
          creatorPkh = Dave.addrKeyHash,
          lockedValue = Value.ada(10),
          utxos = daveUtxos1,
          changeAddress = Dave.address,
          signer = Dave.signer
        )
        provider.submit(gateTx).await()

        val gateScript =
            AnonymousDataGateContract.withErrorTraces.apply(txCreator.policyId: ByteString)
        val gateAddr = gateScript.address(testEnvironment.network)
        val gateUtxo = gateTx.utxos
            .find(_._2.address == gateAddr)
            .map(Utxo(_))
            .getOrElse(fail("Gate UTXO not found"))

        // Dave refunds the gate
        val daveUtxos2 = provider.findUtxos(Dave.address).await().toOption.get
        val refundTx = txCreator.refundGate(
          gateContract = AnonymousDataGateContract.withErrorTraces,
          gateUtxo = gateUtxo,
          signerPkh = Dave.addrKeyHash,
          utxos = daveUtxos2,
          changeAddress = Dave.address,
          signer = Dave.signer
        )
        val refundResult = provider.submit(refundTx).await()
        assert(refundResult.isRight, s"Refund gate should succeed: $refundResult")
    }

    test("Gate: non-creator cannot refund") {
        val provider = createProvider()
        val txCreator = createTxCreator()

        // Initialize anonymous data
        val utxos1 = provider.findUtxos(Alice.address).await().toOption.get
        val initTx = txCreator.initialize(utxos1, tree.rootHash, Alice.address, Alice.signer)
        provider.submit(initTx).await()

        // Dave creates a gate
        val expectedDataHash = scalus.uplc.builtin.Builtins.blake2b_256(utf8"some_data")
        val daveUtxos1 = provider.findUtxos(Dave.address).await().toOption.get
        val gateTx = txCreator.createGate(
          gateContract = AnonymousDataGateContract.withErrorTraces,
          expectedDataHash = expectedDataHash,
          creatorPkh = Dave.addrKeyHash,
          lockedValue = Value.ada(10),
          utxos = daveUtxos1,
          changeAddress = Dave.address,
          signer = Dave.signer
        )
        provider.submit(gateTx).await()

        val gateScript =
            AnonymousDataGateContract.withErrorTraces.apply(txCreator.policyId: ByteString)
        val gateAddr = gateScript.address(testEnvironment.network)
        val gateUtxo = gateTx.utxos
            .find(_._2.address == gateAddr)
            .map(Utxo(_))
            .getOrElse(fail("Gate UTXO not found"))

        // Charles (not the creator) tries to refund
        val charlesUtxos = provider.findUtxos(Charles.address).await().toOption.get
        val refundTx = txCreator.refundGate(
          gateContract = AnonymousDataGateContract.withErrorTraces,
          gateUtxo = gateUtxo,
          signerPkh = Charles.addrKeyHash,
          utxos = charlesUtxos,
          changeAddress = Charles.address,
          signer = Charles.signer
        )
        val result = provider.submit(refundTx).await()
        assert(result.isLeft, s"Non-creator refund should fail: $result")
    }

    // ===== Helper methods =====

    private def findSharedUtxo(tx: Transaction, txCreator: AnonymousDataTransactions): Utxo = {
        val entry = tx.utxos
            .find { case (_, out) =>
                out.address == txCreator.scriptAddr &&
                out.value.assets.assets.exists { case (cs, tokens) =>
                    cs == txCreator.policyId && tokens
                        .get(
                          AssetName(AnonymousDataValidator.beaconTokenName)
                        )
                        .exists(_ > 0)
                }
            }
            .getOrElse(fail("Shared UTXO with beacon token not found"))
        Utxo(entry)
    }
}

object AnonymousDataTest extends ScalusTest {
    private given env: CardanoInfo = testEnvironment

    // Participants: Alice (admin), Bob, Charles, Dave, Eve
    private val memberPkhs: IndexedSeq[ByteString] = IndexedSeq(
      Alice.addrKeyHash,
      Bob.addrKeyHash,
      Charles.addrKeyHash,
      Dave.addrKeyHash,
      Eve.addrKeyHash
    )
    val tree: MerkleTree = MerkleTree.fromElements(memberPkhs)

    def createTxCreator(
        evaluator: PlutusScriptEvaluator = PlutusScriptEvaluator.constMaxBudget(env)
    ): AnonymousDataTransactions =
        AnonymousDataTransactions(
          env = env,
          evaluator = evaluator,
          contract = AnonymousDataContract.withErrorTraces,
          adminPkh = Alice.addrKeyHash
        )

    def createProvider(): Emulator = {
        val initialUtxos = Map(
          TransactionInput(genesisHash, 0) -> TransactionOutput.Babbage(
            Alice.address,
            Value.ada(5000)
          ),
          TransactionInput(genesisHash, 1) -> TransactionOutput.Babbage(
            Alice.address,
            Value.ada(5000)
          ),
          TransactionInput(genesisHash, 2) -> TransactionOutput.Babbage(
            Bob.address,
            Value.ada(5000)
          ),
          TransactionInput(genesisHash, 3) -> TransactionOutput.Babbage(
            Bob.address,
            Value.ada(5000)
          ),
          TransactionInput(genesisHash, 4) -> TransactionOutput.Babbage(
            Charles.address,
            Value.ada(5000)
          ),
          TransactionInput(genesisHash, 5) -> TransactionOutput.Babbage(
            Charles.address,
            Value.ada(5000)
          ),
          TransactionInput(genesisHash, 6) -> TransactionOutput.Babbage(
            Dave.address,
            Value.ada(5000)
          ),
          TransactionInput(genesisHash, 7) -> TransactionOutput.Babbage(
            Faith.address,
            Value.ada(5000)
          )
        )

        Emulator(
          initialUtxos = initialUtxos,
          initialContext = Context.testMainnet()
        )
    }
}
