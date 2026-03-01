package scalus.examples.linkedlist

import org.scalatest.funsuite.AnyFunSuite
import org.scalatestplus.scalacheck.ScalaCheckPropertyChecks
import org.scalacheck.Gen
import scalus.uplc.builtin.ByteString
import scalus.uplc.builtin.ByteString.given
import scalus.uplc.builtin.Builtins.appendByteString
import scalus.uplc.builtin.Data.toData
import scalus.cardano.ledger.*
import scalus.cardano.ledger.EvaluatorMode
import scalus.cardano.ledger.rules.{Context, PlutusScriptsTransactionMutator}
import scalus.cardano.node.Emulator
import scalus.testing.kit.Party.{Alice, Bob}
import scalus.testing.kit.TestUtil.genesisHash
import scalus.testing.kit.ScalusTest
import scalus.utils.await

class LinkedListTest extends AnyFunSuite, ScalusTest, ScalaCheckPropertyChecks {
    import LinkedListTest.{*, given}

    test(s"LinkedListValidator size: ${LinkedListContract.script.script.size} bytes") {
        info(s"Validator size: ${LinkedListContract.script.script.size} bytes")
    }

    test("init: valid empty list") {
        val (provider, txCreator) = createSetup()
        val utxos = provider.findUtxos(Alice.address).await().toOption.get
        val tx = txCreator.init(utxos, ().toData, Alice.address, Alice.signer)
        assert(provider.submit(tx).await().isRight, "init should succeed")
    }

    test("deinit: valid empty list") {
        val (provider, txCreator) = createSetup()
        val utxos = provider.findUtxos(Alice.address).await().toOption.get
        val initTx = txCreator.init(utxos, ().toData, Alice.address, Alice.signer)
        provider.submit(initTx).await()

        val rootUtxo = Utxo(initTx.utxos.find(_._2.address == txCreator.scriptAddress).get)
        val utxos2 = provider.findUtxos(Alice.address).await().toOption.get
        val deinitTx = txCreator.deinit(utxos2, rootUtxo, Alice.address, Alice.signer)
        assert(provider.submit(deinitTx).await().isRight, "deinit should succeed")
    }

    test("FAIL: deinit when list has a node") {
        val (provider, txCreator) = createSetup()
        val utxos = provider.findUtxos(Alice.address).await().toOption.get
        val initTx = txCreator.init(utxos, ().toData, Alice.address, Alice.signer)
        provider.submit(initTx).await()

        val rootUtxo = Utxo(initTx.utxos.find(_._2.address == txCreator.scriptAddress).get)
        val utxos2 = provider.findUtxos(Alice.address).await().toOption.get
        val appendTx = txCreator.appendUnordered(
          utxos2,
          rootUtxo,
          nodeKey("alpha"),
          ().toData,
          Alice.address,
          Alice.signer
        )
        provider.submit(appendTx).await()

        // Now try to deinit the list that still has a node -- must fail.
        // Use constMaxBudget so the invalid tx can be built without running scripts.
        val (_, badTxCreator) = createSetup(EvaluatorMode.Validate)
        val rootUtxo2 = Utxo(appendTx.utxos.find { case (_, out) =>
            out.address == txCreator.scriptAddress &&
            out.value.hasAsset(txCreator.policyId, AssetName(rootKeyBytes))
        }.get)
        val utxos3 = provider.findUtxos(Alice.address).await().toOption.get
        val deinitTx = badTxCreator.deinit(utxos3, rootUtxo2, Alice.address, Alice.signer)
        assertSubmitFails(provider, deinitTx)
    }

    test("appendUnordered: valid append to empty list") {
        val (provider, txCreator) = createSetup()
        val utxos = provider.findUtxos(Alice.address).await().toOption.get
        val initTx = txCreator.init(utxos, ().toData, Alice.address, Alice.signer)
        provider.submit(initTx).await()

        val rootUtxo = Utxo(initTx.utxos.find(_._2.address == txCreator.scriptAddress).get)
        val utxos2 = provider.findUtxos(Alice.address).await().toOption.get
        val appendTx = txCreator.appendUnordered(
          utxos2,
          rootUtxo,
          nodeKey("alpha"),
          ().toData,
          Alice.address,
          Alice.signer
        )
        assert(provider.submit(appendTx).await().isRight, "appendUnordered should succeed")
    }

    test("appendUnordered: two nodes") {
        val (provider, txCreator) = createSetup()
        val utxos = provider.findUtxos(Alice.address).await().toOption.get
        val initTx = txCreator.init(utxos, ().toData, Alice.address, Alice.signer)
        provider.submit(initTx).await()

        // First append
        val rootUtxo1 = Utxo(initTx.utxos.find(_._2.address == txCreator.scriptAddress).get)
        val utxos2 = provider.findUtxos(Alice.address).await().toOption.get
        val appendTx1 = txCreator.appendUnordered(
          utxos2,
          rootUtxo1,
          nodeKey("alpha"),
          ().toData,
          Alice.address,
          Alice.signer
        )
        provider.submit(appendTx1).await()

        // Second append -- find the "alpha" node (it's the last element now)
        val alphaUtxo = Utxo(appendTx1.utxos.find { case (_, out) =>
            out.address == txCreator.scriptAddress &&
            out.value.hasAsset(
              txCreator.policyId,
              AssetName(appendByteString(prefixBytes, nodeKey("alpha")))
            )
        }.get)
        val utxos3 = provider.findUtxos(Alice.address).await().toOption.get
        val appendTx2 = txCreator.appendUnordered(
          utxos3,
          alphaUtxo,
          nodeKey("beta"),
          ().toData,
          Alice.address,
          Alice.signer
        )
        assert(provider.submit(appendTx2).await().isRight, "second appendUnordered should succeed")
    }

    test("FAIL: appendUnordered when anchor has a link (not last element)") {
        val (provider, txCreator) = createSetup()
        val utxos = provider.findUtxos(Alice.address).await().toOption.get
        val initTx = txCreator.init(utxos, ().toData, Alice.address, Alice.signer)
        provider.submit(initTx).await()

        // Append two nodes so root -> alpha -> beta
        val rootUtxo1 = Utxo(initTx.utxos.find(_._2.address == txCreator.scriptAddress).get)
        val utxos2 = provider.findUtxos(Alice.address).await().toOption.get
        val appendTx1 = txCreator.appendUnordered(
          utxos2,
          rootUtxo1,
          nodeKey("alpha"),
          ().toData,
          Alice.address,
          Alice.signer
        )
        provider.submit(appendTx1).await()

        val alphaUtxo = Utxo(appendTx1.utxos.find { case (_, out) =>
            out.address == txCreator.scriptAddress &&
            out.value.hasAsset(
              txCreator.policyId,
              AssetName(appendByteString(prefixBytes, nodeKey("alpha")))
            )
        }.get)
        val utxos3 = provider.findUtxos(Alice.address).await().toOption.get
        val appendTx2 = txCreator.appendUnordered(
          utxos3,
          alphaUtxo,
          nodeKey("beta"),
          ().toData,
          Alice.address,
          Alice.signer
        )
        provider.submit(appendTx2).await()

        // Now try to append to root (which already has link = alpha) -- must fail.
        // Root was re-produced by appendTx1 (appendTx2 only touched alpha).
        // Use constMaxBudget so the invalid tx can be built without running scripts.
        val (_, badTxCreator) = createSetup(EvaluatorMode.Validate)
        val rootUtxo2 = Utxo(appendTx1.utxos.find { case (_, out) =>
            out.address == txCreator.scriptAddress &&
            out.value.hasAsset(txCreator.policyId, AssetName(rootKeyBytes))
        }.get)
        val utxos4 = provider.findUtxos(Alice.address).await().toOption.get
        val appendTx3 = badTxCreator.appendUnordered(
          utxos4,
          rootUtxo2,
          nodeKey("gamma"),
          ().toData,
          Alice.address,
          Alice.signer
        )
        assertSubmitFails(provider, appendTx3)
    }

    test("prependUnordered: valid prepend to non-empty list") {
        val (provider, txCreator) = createSetup()
        val utxos = provider.findUtxos(Alice.address).await().toOption.get
        val initTx = txCreator.init(utxos, ().toData, Alice.address, Alice.signer)
        provider.submit(initTx).await()

        // First add a node via append
        val rootUtxo1 = Utxo(initTx.utxos.find(_._2.address == txCreator.scriptAddress).get)
        val utxos2 = provider.findUtxos(Alice.address).await().toOption.get
        val appendTx = txCreator.appendUnordered(
          utxos2,
          rootUtxo1,
          nodeKey("beta"),
          ().toData,
          Alice.address,
          Alice.signer
        )
        provider.submit(appendTx).await()

        // Now prepend a node -- root -> alpha -> beta
        val rootUtxo2 = Utxo(appendTx.utxos.find { case (_, out) =>
            out.address == txCreator.scriptAddress &&
            out.value.hasAsset(txCreator.policyId, AssetName(rootKeyBytes))
        }.get)
        val utxos3 = provider.findUtxos(Alice.address).await().toOption.get
        val prependTx = txCreator.prependUnordered(
          utxos3,
          rootUtxo2,
          nodeKey("alpha"),
          ().toData,
          Alice.address,
          Alice.signer
        )
        assert(provider.submit(prependTx).await().isRight, "prependUnordered should succeed")
    }

    test("FAIL: prependUnordered when anchor is not the root") {
        val (provider, txCreator) = createSetup()
        val utxos = provider.findUtxos(Alice.address).await().toOption.get
        val initTx = txCreator.init(utxos, ().toData, Alice.address, Alice.signer)
        provider.submit(initTx).await()

        // Append two nodes: root -> alpha -> beta
        val rootUtxo1 = Utxo(initTx.utxos.find(_._2.address == txCreator.scriptAddress).get)
        val utxos2 = provider.findUtxos(Alice.address).await().toOption.get
        val appendTx1 = txCreator.appendUnordered(
          utxos2,
          rootUtxo1,
          nodeKey("alpha"),
          ().toData,
          Alice.address,
          Alice.signer
        )
        provider.submit(appendTx1).await()

        val alphaUtxo = Utxo(appendTx1.utxos.find { case (_, out) =>
            out.address == txCreator.scriptAddress &&
            out.value.hasAsset(
              txCreator.policyId,
              AssetName(appendByteString(prefixBytes, nodeKey("alpha")))
            )
        }.get)
        val utxos3 = provider.findUtxos(Alice.address).await().toOption.get
        val appendTx2 = txCreator.appendUnordered(
          utxos3,
          alphaUtxo,
          nodeKey("beta"),
          ().toData,
          Alice.address,
          Alice.signer
        )
        provider.submit(appendTx2).await()

        // Try to prependUnordered using alpha as the anchor (not the root) -- must fail.
        val (_, badTxCreator) = createSetup(EvaluatorMode.Validate)
        val alphaUtxo2 = Utxo(appendTx2.utxos.find { case (_, out) =>
            out.address == txCreator.scriptAddress &&
            out.value.hasAsset(
              txCreator.policyId,
              AssetName(appendByteString(prefixBytes, nodeKey("alpha")))
            )
        }.get)
        val utxos4 = provider.findUtxos(Alice.address).await().toOption.get
        val prependTx = badTxCreator.prependUnordered(
          utxos4,
          alphaUtxo2,
          nodeKey("gamma"),
          ().toData,
          Alice.address,
          Alice.signer
        )
        assertSubmitFails(provider, prependTx)
    }

    test("insert: valid insert after root") {
        val (provider, txCreator) = createSetup()
        val utxos = provider.findUtxos(Alice.address).await().toOption.get
        val initTx = txCreator.init(utxos, ().toData, Alice.address, Alice.signer)
        provider.submit(initTx).await()

        val rootUtxo = Utxo(initTx.utxos.find(_._2.address == txCreator.scriptAddress).get)
        val utxos2 = provider.findUtxos(Alice.address).await().toOption.get
        val insertTx = txCreator.insert(
          utxos2,
          rootUtxo,
          nodeKey("b"),
          ().toData,
          Alice.address,
          Alice.signer
        )
        assert(provider.submit(insertTx).await().isRight, "insert should succeed")
    }

    test("insert: insert between two nodes") {
        val (provider, txCreator) = createSetup()
        val utxos = provider.findUtxos(Alice.address).await().toOption.get
        val initTx = txCreator.init(utxos, ().toData, Alice.address, Alice.signer)
        provider.submit(initTx).await()

        // root -> "c"
        val rootUtxo1 = Utxo(initTx.utxos.find(_._2.address == txCreator.scriptAddress).get)
        val utxos2 = provider.findUtxos(Alice.address).await().toOption.get
        val insertTx1 = txCreator.insert(
          utxos2,
          rootUtxo1,
          nodeKey("c"),
          ().toData,
          Alice.address,
          Alice.signer
        )
        provider.submit(insertTx1).await()

        // root -> "b" -> "c"  (insert "b" after root)
        val rootUtxo2 = Utxo(insertTx1.utxos.find { case (_, out) =>
            out.address == txCreator.scriptAddress &&
            out.value.hasAsset(txCreator.policyId, AssetName(rootKeyBytes))
        }.get)
        val utxos3 = provider.findUtxos(Alice.address).await().toOption.get
        val insertTx2 = txCreator.insert(
          utxos3,
          rootUtxo2,
          nodeKey("b"),
          ().toData,
          Alice.address,
          Alice.signer
        )
        assert(
          provider.submit(insertTx2).await().isRight,
          "insert middle insert should succeed"
        )
    }

    test("FAIL: insert with key greater than successor (wrong order)") {
        val (provider, txCreator) = createSetup()
        val utxos = provider.findUtxos(Alice.address).await().toOption.get
        val initTx = txCreator.init(utxos, ().toData, Alice.address, Alice.signer)
        provider.submit(initTx).await()

        // root -> "b"
        val rootUtxo1 = Utxo(initTx.utxos.find(_._2.address == txCreator.scriptAddress).get)
        val utxos2 = provider.findUtxos(Alice.address).await().toOption.get
        val insertTx1 = txCreator.insert(
          utxos2,
          rootUtxo1,
          nodeKey("b"),
          ().toData,
          Alice.address,
          Alice.signer
        )
        provider.submit(insertTx1).await()

        // Try to insert "c" after root -- but root points to "b", and "c" > "b" so order check fails.
        // Use constMaxBudget so the invalid tx can be built without running scripts.
        val (_, badTxCreator) = createSetup(EvaluatorMode.Validate)
        val rootUtxo2 = Utxo(insertTx1.utxos.find { case (_, out) =>
            out.address == txCreator.scriptAddress &&
            out.value.hasAsset(txCreator.policyId, AssetName(rootKeyBytes))
        }.get)
        val utxos3 = provider.findUtxos(Alice.address).await().toOption.get
        val insertTx2 = badTxCreator.insert(
          utxos3,
          rootUtxo2,
          nodeKey("c"),
          ().toData,
          Alice.address,
          Alice.signer
        )
        assertSubmitFails(provider, insertTx2)
    }

    test("remove: valid node removal") {
        val (provider, txCreator) = createSetup()
        val utxos = provider.findUtxos(Alice.address).await().toOption.get
        val initTx = txCreator.init(utxos, ().toData, Alice.address, Alice.signer)
        provider.submit(initTx).await()

        // Append one node: root -> alpha
        val rootUtxo1 = Utxo(initTx.utxos.find(_._2.address == txCreator.scriptAddress).get)
        val utxos2 = provider.findUtxos(Alice.address).await().toOption.get
        val appendTx = txCreator.appendUnordered(
          utxos2,
          rootUtxo1,
          nodeKey("alpha"),
          ().toData,
          Alice.address,
          Alice.signer
        )
        provider.submit(appendTx).await()

        val rootUtxo2 = Utxo(appendTx.utxos.find { case (_, out) =>
            out.address == txCreator.scriptAddress &&
            out.value.hasAsset(txCreator.policyId, AssetName(rootKeyBytes))
        }.get)
        val alphaUtxo = Utxo(appendTx.utxos.find { case (_, out) =>
            out.address == txCreator.scriptAddress &&
            out.value.hasAsset(
              txCreator.policyId,
              AssetName(appendByteString(prefixBytes, nodeKey("alpha")))
            )
        }.get)
        val utxos3 = provider.findUtxos(Alice.address).await().toOption.get
        val removeTx = txCreator.remove(
          utxos3,
          rootUtxo2,
          alphaUtxo,
          Alice.address,
          Alice.signer
        )
        assert(provider.submit(removeTx).await().isRight, "remove should succeed")
    }

    test("removeHead: valid removal of head node, root datum updated") {
        val (provider, txCreator) = createSetup()
        val utxos = provider.findUtxos(Alice.address).await().toOption.get
        val initTx = txCreator.init(utxos, BigInt(0).toData, Alice.address, Alice.signer)
        provider.submit(initTx).await()

        // Append two nodes: root -> alpha -> beta
        val rootUtxo1 = Utxo(initTx.utxos.find(_._2.address == txCreator.scriptAddress).get)
        val utxos2 = provider.findUtxos(Alice.address).await().toOption.get
        val appendTx1 = txCreator.appendUnordered(
          utxos2,
          rootUtxo1,
          nodeKey("alpha"),
          BigInt(1).toData,
          Alice.address,
          Alice.signer
        )
        provider.submit(appendTx1).await()

        val alphaUtxo = Utxo(appendTx1.utxos.find { case (_, out) =>
            out.address == txCreator.scriptAddress &&
            out.value.hasAsset(
              txCreator.policyId,
              AssetName(appendByteString(prefixBytes, nodeKey("alpha")))
            )
        }.get)
        val utxos3 = provider.findUtxos(Alice.address).await().toOption.get
        val appendTx2 = txCreator.appendUnordered(
          utxos3,
          alphaUtxo,
          nodeKey("beta"),
          BigInt(2).toData,
          Alice.address,
          Alice.signer
        )
        provider.submit(appendTx2).await()

        // removeHead: consume alpha, update root accumulator to 1
        val scriptUtxos =
            provider.findUtxos(txCreator.scriptAddress).await().toOption.get.map(Utxo.apply).toSeq
        val rootUtxo2 = txCreator.findRoot(scriptUtxos)
        val headUtxo = txCreator.findNode(scriptUtxos, nodeKey("alpha"))
        val utxos4 = provider.findUtxos(Alice.address).await().toOption.get
        val removeHeadTx = txCreator.removeHead(
          utxos4,
          rootUtxo2,
          headUtxo,
          BigInt(1).toData,
          Alice.address,
          Alice.signer
        )
        assert(provider.submit(removeHeadTx).await().isRight, "removeHead should succeed")

        // List should now be root -> beta
        val scriptUtxos2 =
            provider.findUtxos(txCreator.scriptAddress).await().toOption.get.map(Utxo.apply).toSeq
        val result = txCreator.readAll(scriptUtxos2)
        assert(result.size == 2, s"expected 2 elements, got ${result.size}")
        assert(result(1)._1 == nodeKey("beta"), s"expected beta as head, got ${result(1)._1}")
    }

    test("membership set: insert fills the gap between two bracketing nodes") {
        // Build root -> "a" -> "b", then prove "ab" belongs between them.
        // "a" < "ab" < "b" holds byte-wise: "ab" starts with "a" so it's greater,
        // and it has no "b" suffix so it's less than "b".
        val (provider, txCreator) = createSetup()
        val scriptUtxos = () =>
            provider.findUtxos(txCreator.scriptAddress).await().toOption.get.map(Utxo.apply).toSeq

        val utxos = provider.findUtxos(Alice.address).await().toOption.get
        provider.submit(txCreator.init(utxos, ().toData, Alice.address, Alice.signer)).await()

        val utxos2 = provider.findUtxos(Alice.address).await().toOption.get
        provider
            .submit(
              txCreator.insert(
                utxos2,
                txCreator.findRoot(scriptUtxos()),
                nodeKey("a"),
                ().toData,
                Alice.address,
                Alice.signer
              )
            )
            .await()

        val utxos3 = provider.findUtxos(Alice.address).await().toOption.get
        provider
            .submit(
              txCreator.insert(
                utxos3,
                txCreator.findNode(scriptUtxos(), nodeKey("a")),
                nodeKey("b"),
                ().toData,
                Alice.address,
                Alice.signer
              )
            )
            .await()

        // root -> "a" -> "b": prove "ab" is absent by inserting it using only "a" as anchor.
        // The validator accepts this iff "a" < "ab" < "b" -- the gap exists and "ab" was not there.
        val utxos4 = provider.findUtxos(Alice.address).await().toOption.get
        val insertTx = txCreator.insert(
          utxos4,
          txCreator.findNode(scriptUtxos(), nodeKey("a")),
          nodeKey("ab"),
          ().toData,
          Alice.address,
          Alice.signer
        )
        assert(provider.submit(insertTx).await().isRight, "inserting into the gap should succeed")
        assert(
          txCreator.readAll(scriptUtxos()).map(_._1) == Seq(
            rootKeyBytes,
            nodeKey("a"),
            nodeKey("ab"),
            nodeKey("b")
          )
        )
    }

    test("membership set: FAIL duplicate key is rejected") {
        // root -> "b": attempting to insert "b" again must fail.
        val (provider, txCreator) = createSetup()
        val scriptUtxos = () =>
            provider.findUtxos(txCreator.scriptAddress).await().toOption.get.map(Utxo.apply).toSeq

        val utxos = provider.findUtxos(Alice.address).await().toOption.get
        provider.submit(txCreator.init(utxos, ().toData, Alice.address, Alice.signer)).await()

        val utxos2 = provider.findUtxos(Alice.address).await().toOption.get
        provider
            .submit(
              txCreator.insert(
                utxos2,
                txCreator.findRoot(scriptUtxos()),
                nodeKey("b"),
                ().toData,
                Alice.address,
                Alice.signer
              )
            )
            .await()

        // Try to insert "b" again using root as anchor -- "b" < "b" is false, so it fails.
        val (_, badTxCreator) = createSetup(EvaluatorMode.Validate)
        val utxos3 = provider.findUtxos(Alice.address).await().toOption.get
        val dupTx = badTxCreator.insert(
          utxos3,
          txCreator.findRoot(scriptUtxos()),
          nodeKey("b"),
          ().toData,
          Alice.address,
          Alice.signer
        )
        assertSubmitFails(provider, dupTx)
    }

    test("property: insert -- readAll keys are always in strict ascending order") {
        forAll(LinkedListTest.distinctNodeKeys) { keys =>
            val (provider, txCreator) = createSetup()

            val walletUtxos = provider.findUtxos(Alice.address).await().toOption.get
            provider
                .submit(txCreator.init(walletUtxos, ().toData, Alice.address, Alice.signer))
                .await()

            for key <- keys do
                val scriptUtxos = provider
                    .findUtxos(txCreator.scriptAddress)
                    .await()
                    .toOption
                    .get
                    .map(Utxo.apply)
                    .toSeq
                val anchor = txCreator.findAnchorFor(scriptUtxos, key)
                val walletUtxos = provider.findUtxos(Alice.address).await().toOption.get
                provider
                    .submit(
                      txCreator
                          .insert(walletUtxos, anchor, key, ().toData, Alice.address, Alice.signer)
                    )
                    .await()

            val scriptUtxos = provider
                .findUtxos(txCreator.scriptAddress)
                .await()
                .toOption
                .get
                .map(Utxo.apply)
                .toSeq
            val resultKeys = txCreator.readAll(scriptUtxos).map(_._1).tail // drop root
            val isSorted = resultKeys
                .zip(resultKeys.tail)
                .forall((a, b) => summon[Ordering[ByteString]].compare(a, b) < 0)
            assert(isSorted, s"Keys not in strict ascending order: $resultKeys")
        }
    }

    test("property: insert -- length equals number of inserted keys") {
        forAll(LinkedListTest.distinctNodeKeys) { keys =>
            val (provider, txCreator) = createSetup()

            val walletUtxos = provider.findUtxos(Alice.address).await().toOption.get
            provider
                .submit(txCreator.init(walletUtxos, ().toData, Alice.address, Alice.signer))
                .await()

            for key <- keys do
                val scriptUtxos = provider
                    .findUtxos(txCreator.scriptAddress)
                    .await()
                    .toOption
                    .get
                    .map(Utxo.apply)
                    .toSeq
                val anchor = txCreator.findAnchorFor(scriptUtxos, key)
                val walletUtxos = provider.findUtxos(Alice.address).await().toOption.get
                provider
                    .submit(
                      txCreator
                          .insert(walletUtxos, anchor, key, ().toData, Alice.address, Alice.signer)
                    )
                    .await()

            val scriptUtxos = provider
                .findUtxos(txCreator.scriptAddress)
                .await()
                .toOption
                .get
                .map(Utxo.apply)
                .toSeq
            val result = txCreator.readAll(scriptUtxos)
            // root + one entry per key
            assert(
              result.size == keys.size + 1,
              s"expected ${keys.size + 1} elements, got ${result.size}"
            )
        }
    }

    test("property: duplicate key is always rejected") {
        forAll(LinkedListTest.distinctNodeKeys) { keys =>
            whenever(keys.nonEmpty) {
                val (provider, txCreator) = createSetup()
                val (_, badTxCreator) = createSetup(EvaluatorMode.Validate)

                val walletUtxos = provider.findUtxos(Alice.address).await().toOption.get
                provider
                    .submit(txCreator.init(walletUtxos, ().toData, Alice.address, Alice.signer))
                    .await()

                for key <- keys do
                    val scriptUtxos = provider
                        .findUtxos(txCreator.scriptAddress)
                        .await()
                        .toOption
                        .get
                        .map(Utxo.apply)
                        .toSeq
                    val anchor = txCreator.findAnchorFor(scriptUtxos, key)
                    val walletUtxos = provider.findUtxos(Alice.address).await().toOption.get
                    provider
                        .submit(
                          txCreator.insert(
                            walletUtxos,
                            anchor,
                            key,
                            ().toData,
                            Alice.address,
                            Alice.signer
                          )
                        )
                        .await()

                // Pick any key already in the list and try to insert it again
                val dupKey = keys.head
                val scriptUtxos = provider
                    .findUtxos(txCreator.scriptAddress)
                    .await()
                    .toOption
                    .get
                    .map(Utxo.apply)
                    .toSeq
                val anchor = txCreator.findAnchorFor(scriptUtxos, dupKey)
                val walletUtxos2 = provider.findUtxos(Alice.address).await().toOption.get
                val dupTx = badTxCreator.insert(
                  walletUtxos2,
                  anchor,
                  dupKey,
                  ().toData,
                  Alice.address,
                  Alice.signer
                )
                assertSubmitFails(provider, dupTx)
            }
        }
    }

    test("property: remove -- order preserved and length decremented") {
        forAll(LinkedListTest.distinctNodeKeys) { keys =>
            whenever(keys.nonEmpty) {
                val (provider, txCreator) = createSetup()

                val walletUtxos = provider.findUtxos(Alice.address).await().toOption.get
                provider
                    .submit(txCreator.init(walletUtxos, ().toData, Alice.address, Alice.signer))
                    .await()

                for key <- keys do
                    val scriptUtxos = provider
                        .findUtxos(txCreator.scriptAddress)
                        .await()
                        .toOption
                        .get
                        .map(Utxo.apply)
                        .toSeq
                    val anchor = txCreator.findAnchorFor(scriptUtxos, key)
                    val walletUtxos = provider.findUtxos(Alice.address).await().toOption.get
                    provider
                        .submit(
                          txCreator.insert(
                            walletUtxos,
                            anchor,
                            key,
                            ().toData,
                            Alice.address,
                            Alice.signer
                          )
                        )
                        .await()

                // Remove the first node in list order (head of the sorted list)
                val scriptUtxos = provider
                    .findUtxos(txCreator.scriptAddress)
                    .await()
                    .toOption
                    .get
                    .map(Utxo.apply)
                    .toSeq
                val rootUtxo = txCreator.findRoot(scriptUtxos)
                val headKey = txCreator.readElement(rootUtxo).link.get // root always has a link
                val headUtxo = txCreator.findNode(scriptUtxos, headKey)
                val walletUtxos2 = provider.findUtxos(Alice.address).await().toOption.get
                provider
                    .submit(
                      txCreator
                          .remove(walletUtxos2, rootUtxo, headUtxo, Alice.address, Alice.signer)
                    )
                    .await()

                val scriptUtxos2 = provider
                    .findUtxos(txCreator.scriptAddress)
                    .await()
                    .toOption
                    .get
                    .map(Utxo.apply)
                    .toSeq
                val resultKeys = txCreator.readAll(scriptUtxos2).map(_._1).tail // drop root
                assert(
                  resultKeys.size == keys.size - 1,
                  s"expected ${keys.size - 1} nodes after remove, got ${resultKeys.size}"
                )
                val isSorted = resultKeys
                    .zip(resultKeys.tail)
                    .forall((a, b) => summon[Ordering[ByteString]].compare(a, b) < 0)
                assert(isSorted, s"Keys not in strict ascending order after remove: $resultKeys")
            }
        }
    }

    test("property: insert then remove is identity") {
        forAll(LinkedListTest.distinctNodeKeys) { keys =>
            whenever(keys.size >= 2) {
                val (provider, txCreator) = createSetup()

                val walletUtxos = provider.findUtxos(Alice.address).await().toOption.get
                provider
                    .submit(txCreator.init(walletUtxos, ().toData, Alice.address, Alice.signer))
                    .await()

                // Insert all but the last key
                val existing = keys.init
                val newKey = keys.last
                for key <- existing do
                    val scriptUtxos = provider
                        .findUtxos(txCreator.scriptAddress)
                        .await()
                        .toOption
                        .get
                        .map(Utxo.apply)
                        .toSeq
                    val anchor = txCreator.findAnchorFor(scriptUtxos, key)
                    val walletUtxos = provider.findUtxos(Alice.address).await().toOption.get
                    provider
                        .submit(
                          txCreator.insert(
                            walletUtxos,
                            anchor,
                            key,
                            ().toData,
                            Alice.address,
                            Alice.signer
                          )
                        )
                        .await()

                // Snapshot keys before insert
                val scriptUtxosBefore = provider
                    .findUtxos(txCreator.scriptAddress)
                    .await()
                    .toOption
                    .get
                    .map(Utxo.apply)
                    .toSeq
                val keysBefore = txCreator.readAll(scriptUtxosBefore).map(_._1)

                // Insert newKey then remove it
                val anchor = txCreator.findAnchorFor(scriptUtxosBefore, newKey)
                val walletUtxos2 = provider.findUtxos(Alice.address).await().toOption.get
                val insertTx = txCreator.insert(
                  walletUtxos2,
                  anchor,
                  newKey,
                  ().toData,
                  Alice.address,
                  Alice.signer
                )
                provider.submit(insertTx).await()

                val scriptUtxosAfterInsert = provider
                    .findUtxos(txCreator.scriptAddress)
                    .await()
                    .toOption
                    .get
                    .map(Utxo.apply)
                    .toSeq
                val anchorAfter = txCreator.findAnchorFor(scriptUtxosAfterInsert, newKey)
                val newNodeUtxo = txCreator.findNode(scriptUtxosAfterInsert, newKey)
                val walletUtxos3 = provider.findUtxos(Alice.address).await().toOption.get
                provider
                    .submit(
                      txCreator.remove(
                        walletUtxos3,
                        anchorAfter,
                        newNodeUtxo,
                        Alice.address,
                        Alice.signer
                      )
                    )
                    .await()

                // Keys after insert+remove must equal keys before
                val scriptUtxosAfter = provider
                    .findUtxos(txCreator.scriptAddress)
                    .await()
                    .toOption
                    .get
                    .map(Utxo.apply)
                    .toSeq
                val keysAfter = txCreator.readAll(scriptUtxosAfter).map(_._1)
                assert(
                  keysAfter == keysBefore,
                  s"Keys changed after insert+remove: before=$keysBefore after=$keysAfter"
                )
            }
        }
    }

    test("property: deinit succeeds iff list is empty") {
        forAll(LinkedListTest.distinctNodeKeys) { keys =>
            whenever(keys.nonEmpty) {
                val (provider, txCreator) = createSetup()
                val (_, badTxCreator) = createSetup(EvaluatorMode.Validate)

                val walletUtxos = provider.findUtxos(Alice.address).await().toOption.get
                provider
                    .submit(txCreator.init(walletUtxos, ().toData, Alice.address, Alice.signer))
                    .await()

                // Insert at least one node
                val key = keys.head
                val scriptUtxos = provider
                    .findUtxos(txCreator.scriptAddress)
                    .await()
                    .toOption
                    .get
                    .map(Utxo.apply)
                    .toSeq
                val anchor = txCreator.findAnchorFor(scriptUtxos, key)
                val walletUtxos2 = provider.findUtxos(Alice.address).await().toOption.get
                provider
                    .submit(
                      txCreator
                          .insert(walletUtxos2, anchor, key, ().toData, Alice.address, Alice.signer)
                    )
                    .await()

                // Deinit must fail while list is non-empty
                val scriptUtxos2 = provider
                    .findUtxos(txCreator.scriptAddress)
                    .await()
                    .toOption
                    .get
                    .map(Utxo.apply)
                    .toSeq
                val rootUtxo = txCreator.findRoot(scriptUtxos2)
                val walletUtxos3 = provider.findUtxos(Alice.address).await().toOption.get
                val deinitTx =
                    badTxCreator.deinit(walletUtxos3, rootUtxo, Alice.address, Alice.signer)
                assertSubmitFails(provider, deinitTx)
            }
        }
    }

    test("readAll: reconstructs list in order") {
        val (provider, txCreator) = createSetup()
        val utxos = provider.findUtxos(Alice.address).await().toOption.get
        val rootData = BigInt(0).toData
        val alphaData = BigInt(1).toData
        val betaData = BigInt(2).toData
        val initTx = txCreator.init(utxos, rootData, Alice.address, Alice.signer)
        provider.submit(initTx).await()

        val rootUtxo1 = Utxo(initTx.utxos.find(_._2.address == txCreator.scriptAddress).get)
        val utxos2 = provider.findUtxos(Alice.address).await().toOption.get
        val appendTx1 = txCreator.appendUnordered(
          utxos2,
          rootUtxo1,
          nodeKey("alpha"),
          alphaData,
          Alice.address,
          Alice.signer
        )
        provider.submit(appendTx1).await()

        val alphaUtxo = Utxo(appendTx1.utxos.find { case (_, out) =>
            out.address == txCreator.scriptAddress &&
            !out.value.hasAsset(txCreator.policyId, AssetName(rootKeyBytes))
        }.get)
        val utxos3 = provider.findUtxos(Alice.address).await().toOption.get
        val appendTx2 = txCreator.appendUnordered(
          utxos3,
          alphaUtxo,
          nodeKey("beta"),
          betaData,
          Alice.address,
          Alice.signer
        )
        provider.submit(appendTx2).await()

        val scriptUtxos = provider.findUtxos(txCreator.scriptAddress).await().toOption.get
        val result = txCreator.readAll(scriptUtxos.map(Utxo.apply).toSeq)

        assert(result.size == 3, s"expected 3 elements, got ${result.size}")
        assert(result(0) == (rootKeyBytes, rootData), s"root mismatch: ${result(0)}")
        assert(result(1) == (nodeKey("alpha"), alphaData), s"alpha mismatch: ${result(1)}")
        assert(result(2) == (nodeKey("beta"), betaData), s"beta mismatch: ${result(2)}")
    }
}

object LinkedListTest extends ScalusTest {
    given env: CardanoInfo = scalus.testing.kit.TestUtil.testEnvironment

    val rootKeyBytes: ByteString = ByteString.fromString("HEAD")
    val prefixBytes: ByteString = ByteString.fromString("N:")

    private val compiledContract = LinkedListContract.withErrorTraces

    def nodeKey(label: String): ByteString = ByteString.fromString(label)

    /** Generates a list of 1–6 distinct node keys, each at most 30 bytes (prefix is 2 bytes,
      * Cardano asset names are capped at 32 bytes total).
      */
    val distinctNodeKeys: Gen[List[ByteString]] =
        Gen
            .listOfN(
              6,
              Gen.chooseNum(1, 30)
                  .flatMap(n => Gen.listOfN(n, Gen.choose(0x20.toByte, 0x7e.toByte)))
            )
            .map(_.map(bytes => ByteString.fromArray(bytes.toArray)).distinct)
            .suchThat(_.nonEmpty)

    /** Create a fresh (provider, txCreator) pair for one test.
      *
      * @param evaluatorMode
      *   `EvaluateAndComputeCost` (default) for valid transactions; `Validate` with
      *   `constMaxBudget` when you need to build an intentionally invalid tx without the TxBuilder
      *   running scripts.
      */
    def createSetup(
        evaluatorMode: EvaluatorMode = EvaluatorMode.EvaluateAndComputeCost
    ): (Emulator, LinkedListOffchain) = {
        val evaluator = evaluatorMode match
            case EvaluatorMode.EvaluateAndComputeCost =>
                PlutusScriptEvaluator(env, EvaluatorMode.EvaluateAndComputeCost)
            case _ =>
                PlutusScriptEvaluator.constMaxBudget(env)

        val provider = Emulator(
          initialUtxos = Map(
            TransactionInput(genesisHash, 0) -> TransactionOutput(Alice.address, Value.ada(10_000)),
            TransactionInput(genesisHash, 1) -> TransactionOutput(Alice.address, Value.ada(10_000)),
            TransactionInput(genesisHash, 2) -> TransactionOutput(Alice.address, Value.ada(10_000)),
            TransactionInput(genesisHash, 3) -> TransactionOutput(Bob.address, Value.ada(10_000))
          ),
          initialContext =
              Context.testMainnet().copy(evaluatorMode = EvaluatorMode.EvaluateAndComputeCost),
          mutators = Set(PlutusScriptsTransactionMutator)
        )

        val txCreator = LinkedListOffchain(
          env = env,
          evaluator = evaluator,
          mintingContract = compiledContract,
          rootKey = rootKeyBytes,
          prefix = prefixBytes
        )

        (provider, txCreator)
    }

    def assertSubmitFails(provider: Emulator, tx: Transaction): Unit =
        provider.submit(tx).await() match {
            case Left(_)  => ()
            case Right(_) => fail("Expected transaction submission to fail but it succeeded")
        }
}
