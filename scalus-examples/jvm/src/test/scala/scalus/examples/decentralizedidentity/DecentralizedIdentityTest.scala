package scalus.examples.decentralizedidentity

import org.scalatest.funsuite.AnyFunSuite
import scalus.uplc.builtin.ByteString
import scalus.uplc.builtin.ByteString.utf8
import scalus.cardano.ledger.*
import scalus.cardano.ledger.rules.Context
import scalus.cardano.node.Emulator

import scalus.cardano.onchain.plutus.v1.PubKeyHash
import scalus.cardano.txbuilder.TxBuilder
import scalus.testing.kit.Party.{Alice, Bob, Charles}
import scalus.testing.kit.ScalusTest
import scalus.testing.kit.TestUtil.{genesisHash, testEnvironment}
import scalus.utils.await

import java.time.Instant
import scala.concurrent.ExecutionContext.Implicits.global

class DecentralizedIdentityTest extends AnyFunSuite, ScalusTest {
    import DecentralizedIdentityTest.{*, given}

    test(
      s"DecentralizedIdentity validator size is ${DecentralizedIdentityContract.script.script.size} bytes"
    ) {
        info(s"Validator size: ${DecentralizedIdentityContract.script.script.size} bytes")
    }

    test("Create identity: mints identity NFT at script address") {
        val provider = createProvider()
        val utxos = provider.findUtxos(Alice.address).await().toOption.get
        val seedUtxo = Utxo(utxos.head)
        val txCreator = createTxCreator(seedUtxo)

        val tx = txCreator.createIdentity(
          utxos = utxos,
          ownerPkh = Alice.addrKeyHash,
          changeAddress = Alice.address,
          signer = Alice.signer
        )

        val result = provider.submit(tx).await()
        assert(result.isRight, s"Create identity should succeed: $result")

        // Verify identity NFT at script address
        val identityOutput = tx.utxos
            .find(_._2.address == txCreator.scriptAddr)
            .getOrElse(fail("Identity NFT missing from output"))

        val datum = identityOutput._2.inlineDatum.get.to[IdentityDatum]
        assert(
          datum.ownerPkh.hash == (Alice.addrKeyHash: ByteString),
          "Datum ownerPkh should match Alice"
        )
    }

    test("Transfer ownership: old and new owner sign, datum updated") {
        val provider = createProvider()
        val utxos = provider.findUtxos(Alice.address).await().toOption.get
        val seedUtxo = Utxo(utxos.head)
        val txCreator = createTxCreator(seedUtxo)

        // Create identity
        val createTx = txCreator.createIdentity(
          utxos = utxos,
          ownerPkh = Alice.addrKeyHash,
          changeAddress = Alice.address,
          signer = Alice.signer
        )
        provider.submit(createTx).await()
        val identityUtxo = Utxo(createTx.utxos.find(_._2.address == txCreator.scriptAddr).get)

        // Transfer ownership to Bob
        val aliceUtxos = provider.findUtxos(Alice.address).await().toOption.get
        val transferTx = txCreator.transferOwnership(
          utxos = aliceUtxos,
          identityUtxo = identityUtxo,
          newOwnerPkh = Bob.addrKeyHash,
          changeAddress = Alice.address,
          ownerSigner = Alice.signer,
          newOwnerSigner = Bob.signer
        )
        val transferResult = provider.submit(transferTx).await()
        assert(transferResult.isRight, s"Transfer should succeed: $transferResult")

        // Verify new owner in datum
        val newIdentityOutput = transferTx.utxos
            .find(_._2.address == txCreator.scriptAddr)
            .getOrElse(fail("Identity NFT missing after transfer"))
        val newDatum = newIdentityOutput._2.inlineDatum.get.to[IdentityDatum]
        assert(
          newDatum.ownerPkh.hash == (Bob.addrKeyHash: ByteString),
          "Datum ownerPkh should match Bob after transfer"
        )
    }

    test("Add delegate: owner mints delegation token at script address") {
        val provider = createProvider()
        val utxos = provider.findUtxos(Alice.address).await().toOption.get
        val seedUtxo = Utxo(utxos.head)
        val txCreator = createTxCreator(seedUtxo)

        // Create identity
        val createTx = txCreator.createIdentity(
          utxos = utxos,
          ownerPkh = Alice.addrKeyHash,
          changeAddress = Alice.address,
          signer = Alice.signer
        )
        provider.submit(createTx).await()
        val identityUtxo = Utxo(createTx.utxos.find(_._2.address == txCreator.scriptAddr).get)

        // Add delegate
        val aliceUtxos = provider.findUtxos(Alice.address).await().toOption.get
        val delegateTx = txCreator.addDelegate(
          utxos = aliceUtxos,
          identityUtxo = identityUtxo,
          delegatePkh = Bob.addrKeyHash,
          validFrom = now,
          validUntil = oneHourLater,
          delegateType = utf8"auth",
          changeAddress = Alice.address,
          signer = Alice.signer
        )
        val delegateResult = provider.submit(delegateTx).await()
        assert(delegateResult.isRight, s"Add delegate should succeed: $delegateResult")

        // Verify delegation token at script address
        val delegationOutputs = delegateTx.utxos.filter(_._2.address == txCreator.scriptAddr)
        assert(delegationOutputs.size >= 1, "Should have delegation output at script address")
    }

    test("Add delegate fails without owner signature") {
        val provider = createProvider()
        val utxos = provider.findUtxos(Alice.address).await().toOption.get
        val seedUtxo = Utxo(utxos.head)
        val txCreator = createTxCreator(seedUtxo)

        // Create identity
        val createTx = txCreator.createIdentity(
          utxos = utxos,
          ownerPkh = Alice.addrKeyHash,
          changeAddress = Alice.address,
          signer = Alice.signer
        )
        provider.submit(createTx).await()
        val identityUtxo = Utxo(createTx.utxos.find(_._2.address == txCreator.scriptAddr).get)

        // Try to add delegate with Bob signing instead of Alice (the owner)
        val bobUtxos = provider.findUtxos(Bob.address).await().toOption.get
        val delegateTx = txCreator.addDelegate(
          utxos = bobUtxos,
          identityUtxo = identityUtxo,
          delegatePkh = Charles.addrKeyHash,
          validFrom = now,
          validUntil = oneHourLater,
          delegateType = utf8"auth",
          changeAddress = Bob.address,
          signer = Bob.signer
        )
        val delegateResult = provider.submit(delegateTx).await()
        assert(delegateResult.isLeft, "Add delegate without owner sig should fail")
    }

    test("Publish attribute: delegate publishes via reference input") {
        val provider = createProvider()
        val utxos = provider.findUtxos(Alice.address).await().toOption.get
        val seedUtxo = Utxo(utxos.head)
        val txCreator = createTxCreator(seedUtxo)

        // Create identity
        val createTx = txCreator.createIdentity(
          utxos = utxos,
          ownerPkh = Alice.addrKeyHash,
          changeAddress = Alice.address,
          signer = Alice.signer
        )
        provider.submit(createTx).await()
        val identityUtxo = Utxo(createTx.utxos.find(_._2.address == txCreator.scriptAddr).get)

        // Add delegate
        val aliceUtxos = provider.findUtxos(Alice.address).await().toOption.get
        val delegateTx = txCreator.addDelegate(
          utxos = aliceUtxos,
          identityUtxo = identityUtxo,
          delegatePkh = Bob.addrKeyHash,
          validFrom = now,
          validUntil = oneHourLater,
          delegateType = utf8"auth",
          changeAddress = Alice.address,
          signer = Alice.signer
        )
        provider.submit(delegateTx).await()

        // Find delegation UTXO (not the identity UTXO)
        val delegationUtxo = findDelegationUtxo(delegateTx, txCreator)

        // Publish attribute as delegate (Bob)
        val bobUtxos = provider.findUtxos(Bob.address).await().toOption.get
        val publishTx = txCreator.publishAttribute(
          utxos = bobUtxos,
          delegationUtxo = delegationUtxo,
          key = utf8"email",
          value = utf8"alice@example.com",
          validFrom = now,
          validUntil = oneHourLater,
          changeAddress = Bob.address,
          signer = Bob.signer
        )
        val publishResult = provider.submit(publishTx).await()
        assert(publishResult.isRight, s"Publish attribute should succeed: $publishResult")

        // Verify attribute at script address
        val attrOutputs = publishTx.utxos.filter(_._2.address == txCreator.scriptAddr)
        assert(attrOutputs.nonEmpty, "Should have attribute output at script address")
        val attrDatum = attrOutputs.head._2.inlineDatum.get.to[AttributeDatum]
        assert(attrDatum.key == utf8"email", "Attribute key should match")
        assert(attrDatum.value == utf8"alice@example.com", "Attribute value should match")
    }

    test("Publish attribute rejects a forged delegation that lacks the delegation token") {
        val provider = createProvider()
        val utxos = provider.findUtxos(Alice.address).await().toOption.get
        val seedUtxo = Utxo(utxos.head)
        val txCreator = createTxCreator(seedUtxo)

        // Attacker (Charles) plants a UTxO at the script address with a forged DelegationDatum
        // naming himself as delegate for Alice's identity — but mints NO delegation token. Anyone
        // can pay a datum to a script address, so this needs no owner authorization. Without a
        // token-possession check, PublishAttribute would trust this fake delegation.
        val forgedDatum = DelegationDatum(
          identityTokenName = txCreator.identityTokenName,
          delegatePkh = PubKeyHash(Charles.addrKeyHash),
          validFrom = BigInt(now.toEpochMilli),
          validUntil = BigInt(oneHourLater.toEpochMilli),
          delegateType = utf8"auth"
        )
        val charlesUtxos = provider.findUtxos(Charles.address).await().toOption.get
        val plantTx = TxBuilder(env)
            .payTo(txCreator.scriptAddr, Value.ada(2), forgedDatum)
            .complete(availableUtxos = charlesUtxos, Charles.address)
            .sign(Charles.signer)
            .transaction
        assert(provider.submit(plantTx).await().isRight, "Planting the forged delegation should succeed")
        val forgedDelegation = Utxo(plantTx.utxos.find(_._2.address == txCreator.scriptAddr).get)

        // Charles tries to publish an attribute about Alice's identity via the forged delegation.
        val charlesUtxos2 = provider.findUtxos(Charles.address).await().toOption.get
        val publishTx = txCreator.publishAttribute(
          utxos = charlesUtxos2,
          delegationUtxo = forgedDelegation,
          key = utf8"email",
          value = utf8"forged@evil.com",
          validFrom = now,
          validUntil = oneHourLater,
          changeAddress = Charles.address,
          signer = Charles.signer
        )
        val result = provider.submit(publishTx).await()
        assert(result.isLeft, s"Publishing via a token-less forged delegation must fail: $result")
    }

    test("Revoke delegate: owner burns delegation token") {
        val provider = createProvider()
        val utxos = provider.findUtxos(Alice.address).await().toOption.get
        val seedUtxo = Utxo(utxos.head)
        val txCreator = createTxCreator(
          seedUtxo,
          PlutusScriptEvaluator(env, EvaluatorMode.EvaluateAndComputeCost)
        )

        // Create identity
        val createTx = txCreator.createIdentity(
          utxos = utxos,
          ownerPkh = Alice.addrKeyHash,
          changeAddress = Alice.address,
          signer = Alice.signer
        )
        provider.submit(createTx).await()
        val identityUtxo = Utxo(createTx.utxos.find(_._2.address == txCreator.scriptAddr).get)

        // Add delegate
        val aliceUtxos = provider.findUtxos(Alice.address).await().toOption.get
        val delegateTx = txCreator.addDelegate(
          utxos = aliceUtxos,
          identityUtxo = identityUtxo,
          delegatePkh = Bob.addrKeyHash,
          validFrom = now,
          validUntil = oneHourLater,
          delegateType = utf8"auth",
          changeAddress = Alice.address,
          signer = Alice.signer
        )
        provider.submit(delegateTx).await()

        val delegationUtxo = findDelegationUtxo(delegateTx, txCreator)

        // Revoke delegate
        val aliceUtxos2 = provider.findUtxos(Alice.address).await().toOption.get
        val revokeTx = txCreator.revokeDelegate(
          utxos = aliceUtxos2,
          identityUtxo = identityUtxo,
          delegationUtxo = delegationUtxo,
          changeAddress = Alice.address,
          signer = Alice.signer
        )
        val revokeResult = provider.submit(revokeTx).await()
        assert(revokeResult.isRight, s"Revoke delegate should succeed: $revokeResult")
    }

    test("Revoke attribute: owner burns attribute token") {
        val provider = createProvider()
        val utxos = provider.findUtxos(Alice.address).await().toOption.get
        val seedUtxo = Utxo(utxos.head)
        val txCreator = createTxCreator(
          seedUtxo,
          PlutusScriptEvaluator(env, EvaluatorMode.EvaluateAndComputeCost)
        )

        // Create identity
        val createTx = txCreator.createIdentity(
          utxos = utxos,
          ownerPkh = Alice.addrKeyHash,
          changeAddress = Alice.address,
          signer = Alice.signer
        )
        provider.submit(createTx).await()
        val identityUtxo = Utxo(createTx.utxos.find(_._2.address == txCreator.scriptAddr).get)

        // Add delegate
        val aliceUtxos = provider.findUtxos(Alice.address).await().toOption.get
        val delegateTx = txCreator.addDelegate(
          utxos = aliceUtxos,
          identityUtxo = identityUtxo,
          delegatePkh = Bob.addrKeyHash,
          validFrom = now,
          validUntil = oneHourLater,
          delegateType = utf8"auth",
          changeAddress = Alice.address,
          signer = Alice.signer
        )
        provider.submit(delegateTx).await()
        val delegationUtxo = findDelegationUtxo(delegateTx, txCreator)

        // Publish attribute as delegate (Bob)
        val bobUtxos = provider.findUtxos(Bob.address).await().toOption.get
        val publishTx = txCreator.publishAttribute(
          utxos = bobUtxos,
          delegationUtxo = delegationUtxo,
          key = utf8"email",
          value = utf8"alice@example.com",
          validFrom = now,
          validUntil = oneHourLater,
          changeAddress = Bob.address,
          signer = Bob.signer
        )
        provider.submit(publishTx).await()
        val attributeUtxo = findAttributeUtxo(publishTx, txCreator)

        // Revoke attribute (by owner)
        val aliceUtxos2 = provider.findUtxos(Alice.address).await().toOption.get
        val revokeTx = txCreator.revokeAttribute(
          utxos = aliceUtxos2,
          identityUtxo = identityUtxo,
          attributeUtxo = attributeUtxo,
          changeAddress = Alice.address,
          signer = Alice.signer
        )
        val revokeResult = provider.submit(revokeTx).await()
        assert(revokeResult.isRight, s"Revoke attribute should succeed: $revokeResult")
    }

    test(
      "Full lifecycle: create -> delegate -> publish -> revoke attribute -> revoke delegate -> transfer"
    ) {
        val provider = createProvider()
        val utxos = provider.findUtxos(Alice.address).await().toOption.get
        val seedUtxo = Utxo(utxos.head)
        val txCreator = createTxCreator(
          seedUtxo,
          PlutusScriptEvaluator(env, EvaluatorMode.EvaluateAndComputeCost)
        )

        // 1. Create identity
        val createTx = txCreator.createIdentity(
          utxos = utxos,
          ownerPkh = Alice.addrKeyHash,
          changeAddress = Alice.address,
          signer = Alice.signer
        )
        val createResult = provider.submit(createTx).await()
        assert(createResult.isRight, s"Create identity should succeed: $createResult")
        val identityUtxo = Utxo(createTx.utxos.find(_._2.address == txCreator.scriptAddr).get)

        // 2. Add delegate (Bob)
        val aliceUtxos1 = provider.findUtxos(Alice.address).await().toOption.get
        val delegateTx = txCreator.addDelegate(
          utxos = aliceUtxos1,
          identityUtxo = identityUtxo,
          delegatePkh = Bob.addrKeyHash,
          validFrom = now,
          validUntil = oneHourLater,
          delegateType = utf8"auth",
          changeAddress = Alice.address,
          signer = Alice.signer
        )
        val delegateResult = provider.submit(delegateTx).await()
        assert(delegateResult.isRight, s"Add delegate should succeed: $delegateResult")
        val delegationUtxo = findDelegationUtxo(delegateTx, txCreator)

        // 3. Publish attribute (by Bob as delegate)
        val bobUtxos = provider.findUtxos(Bob.address).await().toOption.get
        val publishTx = txCreator.publishAttribute(
          utxos = bobUtxos,
          delegationUtxo = delegationUtxo,
          key = utf8"email",
          value = utf8"alice@example.com",
          validFrom = now,
          validUntil = oneHourLater,
          changeAddress = Bob.address,
          signer = Bob.signer
        )
        val publishResult = provider.submit(publishTx).await()
        assert(publishResult.isRight, s"Publish attribute should succeed: $publishResult")
        val attributeUtxo = findAttributeUtxo(publishTx, txCreator)

        // 4. Revoke attribute (by owner)
        val aliceUtxos2 = provider.findUtxos(Alice.address).await().toOption.get
        val revokeAttrTx = txCreator.revokeAttribute(
          utxos = aliceUtxos2,
          identityUtxo = identityUtxo,
          attributeUtxo = attributeUtxo,
          changeAddress = Alice.address,
          signer = Alice.signer
        )
        val revokeAttrResult = provider.submit(revokeAttrTx).await()
        assert(revokeAttrResult.isRight, s"Revoke attribute should succeed: $revokeAttrResult")

        // 5. Revoke delegate
        val aliceUtxos3 = provider.findUtxos(Alice.address).await().toOption.get
        val revokeDelegTx = txCreator.revokeDelegate(
          utxos = aliceUtxos3,
          identityUtxo = identityUtxo,
          delegationUtxo = delegationUtxo,
          changeAddress = Alice.address,
          signer = Alice.signer
        )
        val revokeDelegResult = provider.submit(revokeDelegTx).await()
        assert(revokeDelegResult.isRight, s"Revoke delegate should succeed: $revokeDelegResult")

        // 6. Transfer ownership to Bob
        val aliceUtxos4 = provider.findUtxos(Alice.address).await().toOption.get
        val transferTx = txCreator.transferOwnership(
          utxos = aliceUtxos4,
          identityUtxo = identityUtxo,
          newOwnerPkh = Bob.addrKeyHash,
          changeAddress = Alice.address,
          ownerSigner = Alice.signer,
          newOwnerSigner = Bob.signer
        )
        val transferResult = provider.submit(transferTx).await()
        assert(transferResult.isRight, s"Transfer should succeed: $transferResult")

        // Verify final state
        val finalOutput = transferTx.utxos
            .find(_._2.address == txCreator.scriptAddr)
            .getOrElse(fail("Identity NFT missing after transfer"))
        val finalDatum = finalOutput._2.inlineDatum.get.to[IdentityDatum]
        assert(
          finalDatum.ownerPkh.hash == (Bob.addrKeyHash: ByteString),
          "Final owner should be Bob"
        )
    }

    test("submitWithRetry: create identity with retry") {
        val provider = createProvider()
        val utxos = provider.findUtxos(Alice.address).await().toOption.get
        val seedUtxo = Utxo(utxos.head)
        val txCreator = createTxCreator(seedUtxo)

        val result = DecentralizedIdentityTransactions
            .submitWithRetry(provider, Alice.address) { utxos =>
                txCreator.createIdentity(utxos, Alice.addrKeyHash, Alice.address, Alice.signer)
            }
            .await()
        assert(result.isRight, s"submitWithRetry should succeed: $result")
    }

    // ===== Helper methods =====

    /** Find the delegation UTXO from a transaction (not the identity UTXO) */
    private def findDelegationUtxo(
        tx: Transaction,
        txCreator: DecentralizedIdentityTransactions
    ): Utxo = {
        val delegationEntry = tx.utxos
            .filter { case (_, out) =>
                out.address == txCreator.scriptAddr &&
                out.value.assets.assets.exists { case (cs, tokens) =>
                    cs == txCreator.policyId && tokens.keys.exists { assetName =>
                        assetName.bytes.take(1) == ByteString.fromString("d")
                    }
                }
            }
            .headOption
            .getOrElse(fail("Delegation UTXO not found"))
        Utxo(delegationEntry)
    }

    /** Find the attribute UTXO from a transaction */
    private def findAttributeUtxo(
        tx: Transaction,
        txCreator: DecentralizedIdentityTransactions
    ): Utxo = {
        val attributeEntry = tx.utxos
            .filter { case (_, out) =>
                out.address == txCreator.scriptAddr &&
                out.value.assets.assets.exists { case (cs, tokens) =>
                    cs == txCreator.policyId && tokens.keys.exists { assetName =>
                        assetName.bytes.take(1) == ByteString.fromString("a")
                    }
                }
            }
            .headOption
            .getOrElse(fail("Attribute UTXO not found"))
        Utxo(attributeEntry)
    }
}

object DecentralizedIdentityTest extends ScalusTest {
    private given env: CardanoInfo = testEnvironment
    private val compiledContract = DecentralizedIdentityContract.withErrorTraces

    // Use slot-derived times so they fall within valid slots for the emulator
    val now: Instant = env.slotConfig.slotToInstant(0)
    val oneHourLater: Instant = now.plusSeconds(3600)

    def createTxCreator(
        seedUtxo: Utxo,
        evaluator: PlutusScriptEvaluator = PlutusScriptEvaluator.constMaxBudget(env)
    ): DecentralizedIdentityTransactions =
        DecentralizedIdentityTransactions(
          env = env,
          evaluator = evaluator,
          contract = compiledContract,
          seed = seedUtxo
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
          )
        )

        Emulator(
          initialUtxos = initialUtxos,
          initialContext = Context.testMainnet()
        )
    }
}
