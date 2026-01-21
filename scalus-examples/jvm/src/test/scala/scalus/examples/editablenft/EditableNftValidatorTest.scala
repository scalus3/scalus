package scalus.examples.editablenft

import org.scalatest.funsuite.AnyFunSuite
import scalus.builtin.ByteString.utf8
import scalus.cardano.ledger.*
import scalus.cardano.ledger.rules.Context
import scalus.cardano.node.Emulator
import scalus.cardano.txbuilder.TxBuilder
import scalus.testing.kit.Party.{Alice, Bob}
import scalus.testing.kit.{Party, ScalusTest}
import scalus.testing.kit.TestUtil.{genesisHash, testEnvironment}
import scalus.utils.await

import scala.concurrent.ExecutionContext.Implicits.global

class EditableNftValidatorTest extends AnyFunSuite, ScalusTest {
    import EditableNftValidatorTest.{*, given}

    test(s"EditableNft validator size is ${EditableNftContract.script.script.size} bytes") {
        info(s"Validator size: ${EditableNftContract.script.script.size} bytes")
    }

    test("Mint: successful minting creates paired reference and user NFTs") {
        val provider = createProvider()
        val utxos = provider.findUtxos(Alice.address).await().toOption.get

        val mintTx = txCreator.mint(
          utxos = utxos,
          tokenId = tokenId,
          initialData = initialData,
          holderAddress = Alice.address,
          changeAddress = Alice.address,
          signer = Alice.signer
        )

        provider.submit(mintTx).await()

        // Verify reference NFT at script address
        val refNftOutput = mintTx.utxos.find(_._2.address == scriptAddress)
        val refDatum = mintTx.utxos
            .find(_._2.address == scriptAddress)
            .collect { case (_, o) =>
                o.inlineDatum.get.to[ReferenceNftDatum]
            }
            .getOrElse(fail("Reference NFT missing from the mint output"))

        // Verify reference NFT has correct datum
        assert(refDatum.tokenId == tokenId, "Datum tokenId should match")
        assert(refDatum.data == initialData, "Datum data should match initial data")
        assert(!refDatum.isSealed, "New NFT should not be sealed")

        // Verify user NFT at holder address
        val userNftOutput = mintTx.utxos.find { case (_, out) =>
            out.address == Alice.address &&
            out.value.assets.assets.exists { case (cs, tokens) =>
                cs == policyId && tokens.get(AssetName(userTokenName)).exists(_ > 0)
            }
        }
        assert(userNftOutput.nonEmpty, "User NFT should be at holder address")
    }

    test("Spend: holder can edit data when not sealed") {
        val provider = createProvider()
        val refNftUtxo = mintNft(provider, Alice)

        val utxos = provider.findUtxos(Alice.address).await().toOption.get
        val editTx = txCreator.edit(
          utxos = utxos,
          refNftUtxo = refNftUtxo,
          newData = updatedData,
          changeAddress = Alice.address,
          signer = Alice.signer
        )

        val result = provider.submit(editTx).await()
        assert(result.isRight, s"Edit should succeed: $result")

        // Verify data was updated
        val newDatum = editTx.utxos
            .find(_._2.address == scriptAddress)
            .map(_._2.inlineDatum.get.to[ReferenceNftDatum])
            .get
        assert(newDatum.data == updatedData, "Data should be updated")
        assert(newDatum.tokenId == tokenId, "Token ID should remain unchanged")
        assert(!newDatum.isSealed, "Should still be unsealed")
    }

    test("Spend: holder can seal NFT") {
        val provider = createProvider()
        val refNftUtxo = mintNft(provider, Alice)

        val utxos = provider.findUtxos(Alice.address).await().toOption.get
        val sealTx = txCreator.seal(
          utxos = utxos,
          refNftUtxo = refNftUtxo,
          changeAddress = Alice.address,
          signer = Alice.signer
        )

        val result = provider.submit(sealTx).await()
        assert(result.isRight, s"Seal should succeed: $result")

        // Verify NFT is sealed
        val newDatum = sealTx.utxos
            .find(_._2.address == scriptAddress)
            .map(_._2.inlineDatum.get.to[ReferenceNftDatum])
            .get
        assert(newDatum.isSealed, "NFT should be sealed")
        assert(newDatum.data == initialData, "Data should remain unchanged")
    }

    test("Spend: cannot edit data when sealed") {
        val provider = createProvider()
        val refNftUtxo = mintNft(provider, Alice)

        // First seal the NFT
        val utxos1 = provider.findUtxos(Alice.address).await().toOption.get
        val sealTx = txCreator.seal(
          utxos = utxos1,
          refNftUtxo = refNftUtxo,
          changeAddress = Alice.address,
          signer = Alice.signer
        )
        provider.submit(sealTx).await()

        val sealedRefNft = Utxo(sealTx.utxos.find(_._2.address == scriptAddress).get)

        // Now try to edit - should fail
        val utxos2 = provider.findUtxos(Alice.address).await().toOption.get
        val editTx = txCreator.edit(
          utxos = utxos2,
          refNftUtxo = sealedRefNft,
          newData = updatedData,
          changeAddress = Alice.address,
          signer = Alice.signer
        )

        val result = provider.submit(editTx).await()
        assert(result.isLeft, "Edit should fail when NFT is sealed")
    }

    test("Spend: non-holder cannot edit (missing user token)") {
        val provider = createProvider()
        val refNftUtxo = mintNft(provider, Alice)

        // Bob tries to edit without having the user token
        val bobUtxos = provider.findUtxos(Bob.address).await().toOption.get

        val exception = intercept[Exception] {
            txCreator.edit(
              utxos = bobUtxos,
              refNftUtxo = refNftUtxo,
              newData = updatedData,
              changeAddress = Bob.address,
              signer = Bob.signer
            )
        }
        assert(
          exception.getMessage.contains("User NFT not found"),
          s"Should fail with user NFT not found: ${exception.getMessage}"
        )
    }

    test("Spend: holder can edit multiple times") {
        val provider = createProvider()
        var refNftUtxo = mintNft(provider, Alice)

        // First edit
        val utxos1 = provider.findUtxos(Alice.address).await().toOption.get
        val editTx1 = txCreator.edit(
          utxos = utxos1,
          refNftUtxo = refNftUtxo,
          newData = updatedData,
          changeAddress = Alice.address,
          signer = Alice.signer
        )
        val result1 = provider.submit(editTx1).await()
        assert(result1.isRight, s"First edit should succeed: $result1")
        refNftUtxo = Utxo(editTx1.utxos.find(_._2.address == scriptAddress).get)

        // Second edit
        val utxos2 = provider.findUtxos(Alice.address).await().toOption.get
        val editTx2 = txCreator.edit(
          utxos = utxos2,
          refNftUtxo = refNftUtxo,
          newData = utf8"Third value",
          changeAddress = Alice.address,
          signer = Alice.signer
        )
        val result2 = provider.submit(editTx2).await()
        assert(result2.isRight, s"Second edit should succeed: $result2")
        refNftUtxo = Utxo(editTx2.utxos.find(_._2.address == scriptAddress).get)

        // Third edit
        val utxos3 = provider.findUtxos(Alice.address).await().toOption.get
        val editTx3 = txCreator.edit(
          utxos = utxos3,
          refNftUtxo = refNftUtxo,
          newData = utf8"Fourth value",
          changeAddress = Alice.address,
          signer = Alice.signer
        )
        val result3 = provider.submit(editTx3).await()
        assert(result3.isRight, s"Third edit should succeed: $result3")

        // Verify final data
        val finalDatum = editTx3.utxos
            .find(_._2.address == scriptAddress)
            .map(_._2.inlineDatum.get.to[ReferenceNftDatum])
            .get
        assert(finalDatum.data == utf8"Fourth value", "Data should be updated to final value")
    }

    test("Spend: new holder can edit after transfer") {
        val provider = createProvider()
        val refNftUtxo = mintNft(provider, Alice)

        // Alice transfers user token to Bob
        val aliceUtxos = provider.findUtxos(Alice.address).await().toOption.get
        val userNftUtxo = aliceUtxos
            .find { case (_, out) =>
                out.value.assets.assets.exists { case (cs, tokens) =>
                    cs == policyId && tokens.get(AssetName(userTokenName)).exists(_ > 0)
                }
            }
            .map(Utxo.apply)
            .get

        val transferTx = TxBuilder(env, PlutusScriptEvaluator.constMaxBudget(env))
            .spend(userNftUtxo)
            .payTo(Bob.address, Value.asset(policyId, AssetName(userTokenName), 1))
            .complete(availableUtxos = aliceUtxos, Alice.address)
            .sign(Alice.signer)
            .transaction

        provider.submit(transferTx).await()

        // Now Bob can edit
        val bobUtxos = provider.findUtxos(Bob.address).await().toOption.get
        val editTx = txCreator.edit(
          utxos = bobUtxos,
          refNftUtxo = refNftUtxo,
          newData = updatedData,
          changeAddress = Bob.address,
          signer = Bob.signer
        )

        val result = provider.submit(editTx).await()
        assert(result.isRight, s"New holder should be able to edit: $result")
    }

    private def mintNft(provider: Emulator, holder: Party): Utxo = {
        val utxos = provider.findUtxos(holder.address).await().toOption.get
        val mintTx = txCreator.mint(
          utxos = utxos,
          tokenId = tokenId,
          initialData = initialData,
          holderAddress = holder.address,
          changeAddress = holder.address,
          signer = holder.signer
        )
        provider.submit(mintTx).await()
        Utxo(mintTx.utxos.find(_._2.address == scriptAddress).get)
    }
}

object EditableNftValidatorTest extends ScalusTest {
    private given env: CardanoInfo = testEnvironment
    private val compiledContract = EditableNftContract.withErrorTraces
    val scriptAddress: scalus.cardano.address.Address = compiledContract.address(env.network)
    val policyId: scalus.cardano.ledger.PolicyId = compiledContract.script.scriptHash

    val txCreator = EditableNftTransactions(
      env = env,
      evaluator = PlutusScriptEvaluator.constMaxBudget(env),
      contract = compiledContract
    )

    // Test data
    val tokenId: scalus.builtin.ByteString = utf8"myNFT"
    val refTokenName: scalus.builtin.ByteString =
        EditableNftValidator.REFERENCE_NFT_LABEL ++ tokenId
    val userTokenName: scalus.builtin.ByteString = EditableNftValidator.USER_NFT_LABEL ++ tokenId

    val initialData: scalus.builtin.ByteString = utf8"Hello"
    val updatedData: scalus.builtin.ByteString = utf8"World"

    private def createProvider(): Emulator = {
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
          )
        )

        Emulator(
          initialUtxos = initialUtxos,
          initialContext = Context.testMainnet()
        )
    }
}
