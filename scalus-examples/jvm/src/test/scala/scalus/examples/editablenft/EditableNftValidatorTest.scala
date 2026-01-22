package scalus.examples.editablenft

import org.scalatest.funsuite.AnyFunSuite
import scalus.builtin.ByteString.utf8
import scalus.cardano.ledger.*
import scalus.cardano.ledger.rules.Context
import scalus.cardano.node.Emulator
import scalus.cardano.txbuilder.TxBuilder
import scalus.testing.kit.Party.{Alice, Bob}
import scalus.testing.kit.ScalusTest
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
        val seedUtxo = Utxo(utxos.head)
        val txCreator = createTxCreator(seedUtxo)

        val mintTx = txCreator.mint(
          utxos = utxos,
          tokenId = tokenId,
          initialData = initialData,
          holderAddress = Alice.address,
          changeAddress = Alice.address,
          signer = Alice.signer
        )

        val result = provider.submit(mintTx).await()
        assert(result.isRight, s"Minting should succeed: $result")

        // Verify reference NFT at script address
        val refNftOutput = mintTx.utxos
            .find(_._2.address == txCreator.scriptAddr)
            .getOrElse(fail("Reference NFT missing from mint output"))

        val refDatum = refNftOutput._2.inlineDatum.get.to[ReferenceNftDatum]
        assert(refDatum.tokenId == tokenId, "Datum tokenId should match")
        assert(refDatum.data == initialData, "Datum data should match initial data")
        assert(!refDatum.isSealed, "New NFT should not be sealed")

        // Verify user NFT at holder address
        val userTokenName = refDatum.userNftName
        val userNftOutput = mintTx.utxos.find { case (_, out) =>
            out.address == Alice.address &&
            out.value.assets.assets.exists { case (cs, tokens) =>
                cs == txCreator.policyId && tokens.get(AssetName(userTokenName)).exists(_ > 0)
            }
        }
        assert(userNftOutput.nonEmpty, "User NFT should be at holder address")
    }

    test("Lifecycle: mint -> edit -> edit -> seal success") {
        val provider = createProvider()
        val utxos = provider.findUtxos(Alice.address).await().toOption.get
        val seedUtxo = Utxo(utxos.head)
        val txCreator = createTxCreator(seedUtxo)

        // Mint
        val mintTx = txCreator.mint(
          utxos = utxos,
          tokenId = tokenId,
          initialData = initialData,
          holderAddress = Alice.address,
          changeAddress = Alice.address,
          signer = Alice.signer
        )
        val mintResult = provider.submit(mintTx).await()
        assert(mintResult.isRight, s"Mint should succeed: $mintResult")
        var refNftUtxo = Utxo(mintTx.utxos.find(_._2.address == txCreator.scriptAddr).get)

        // First edit
        val utxos1 = provider.findUtxos(Alice.address).await().toOption.get
        val editTx1 = txCreator.edit(
          utxos = utxos1,
          refNftUtxo = refNftUtxo,
          newData = utf8"First edit",
          changeAddress = Alice.address,
          signer = Alice.signer
        )
        val editResult1 = provider.submit(editTx1).await()
        assert(editResult1.isRight, s"First edit should succeed: $editResult1")
        refNftUtxo = Utxo(editTx1.utxos.find(_._2.address == txCreator.scriptAddr).get)

        // Second edit
        val utxos2 = provider.findUtxos(Alice.address).await().toOption.get
        val editTx2 = txCreator.edit(
          utxos = utxos2,
          refNftUtxo = refNftUtxo,
          newData = utf8"Second edit",
          changeAddress = Alice.address,
          signer = Alice.signer
        )
        val editResult2 = provider.submit(editTx2).await()
        assert(editResult2.isRight, s"Second edit should succeed: $editResult2")
        refNftUtxo = Utxo(editTx2.utxos.find(_._2.address == txCreator.scriptAddr).get)

        // Seal
        val utxos3 = provider.findUtxos(Alice.address).await().toOption.get
        val sealTx = txCreator.seal(
          utxos = utxos3,
          refNftUtxo = refNftUtxo,
          changeAddress = Alice.address,
          signer = Alice.signer
        )
        val sealResult = provider.submit(sealTx).await()
        assert(sealResult.isRight, s"Seal should succeed: $sealResult")

        // Verify final state
        val finalDatum = sealTx.utxos
            .find(_._2.address == txCreator.scriptAddr)
            .map(_._2.inlineDatum.get.to[ReferenceNftDatum])
            .get
        assert(finalDatum.isSealed, "NFT should be sealed")
        assert(finalDatum.data == utf8"Second edit", "Data should reflect last edit")
    }

    test("Lifecycle: mint -> edit -> edit -> seal -> edit failure") {
        val provider = createProvider()
        val utxos = provider.findUtxos(Alice.address).await().toOption.get
        val seedUtxo = Utxo(utxos.head)
        val txCreator = createTxCreator(seedUtxo)

        // Mint
        val mintTx = txCreator.mint(
          utxos = utxos,
          tokenId = tokenId,
          initialData = initialData,
          holderAddress = Alice.address,
          changeAddress = Alice.address,
          signer = Alice.signer
        )
        provider.submit(mintTx).await()
        var refNftUtxo = Utxo(mintTx.utxos.find(_._2.address == txCreator.scriptAddr).get)

        // First edit
        val utxos1 = provider.findUtxos(Alice.address).await().toOption.get
        val editTx1 = txCreator.edit(
          utxos = utxos1,
          refNftUtxo = refNftUtxo,
          newData = utf8"First edit",
          changeAddress = Alice.address,
          signer = Alice.signer
        )
        provider.submit(editTx1).await()
        refNftUtxo = Utxo(editTx1.utxos.find(_._2.address == txCreator.scriptAddr).get)

        // Second edit
        val utxos2 = provider.findUtxos(Alice.address).await().toOption.get
        val editTx2 = txCreator.edit(
          utxos = utxos2,
          refNftUtxo = refNftUtxo,
          newData = utf8"Second edit",
          changeAddress = Alice.address,
          signer = Alice.signer
        )
        provider.submit(editTx2).await()
        refNftUtxo = Utxo(editTx2.utxos.find(_._2.address == txCreator.scriptAddr).get)

        // Seal
        val utxos3 = provider.findUtxos(Alice.address).await().toOption.get
        val sealTx = txCreator.seal(
          utxos = utxos3,
          refNftUtxo = refNftUtxo,
          changeAddress = Alice.address,
          signer = Alice.signer
        )
        provider.submit(sealTx).await()
        refNftUtxo = Utxo(sealTx.utxos.find(_._2.address == txCreator.scriptAddr).get)

        // Try to edit after seal - should fail
        val utxos4 = provider.findUtxos(Alice.address).await().toOption.get
        val editTx3 = txCreator.edit(
          utxos = utxos4,
          refNftUtxo = refNftUtxo,
          newData = utf8"Should fail",
          changeAddress = Alice.address,
          signer = Alice.signer
        )
        val editResult3 = provider.submit(editTx3).await()
        assert(editResult3.isLeft, "Edit after seal should fail")
    }

    test("Lifecycle: mint -> edit -> transfer -> edit (by Bob) -> seal success") {
        val provider = createProvider()
        val utxos = provider.findUtxos(Alice.address).await().toOption.get
        val seedUtxo = Utxo(utxos.head)
        val txCreator = createTxCreator(seedUtxo)

        // Mint (Alice)
        val mintTx = txCreator.mint(
          utxos = utxos,
          tokenId = tokenId,
          initialData = initialData,
          holderAddress = Alice.address,
          changeAddress = Alice.address,
          signer = Alice.signer
        )
        provider.submit(mintTx).await()
        var refNftUtxo = Utxo(mintTx.utxos.find(_._2.address == txCreator.scriptAddr).get)

        // Edit (Alice)
        val utxos1 = provider.findUtxos(Alice.address).await().toOption.get
        val editTx1 = txCreator.edit(
          utxos = utxos1,
          refNftUtxo = refNftUtxo,
          newData = utf8"Alice edit",
          changeAddress = Alice.address,
          signer = Alice.signer
        )
        provider.submit(editTx1).await()
        refNftUtxo = Utxo(editTx1.utxos.find(_._2.address == txCreator.scriptAddr).get)

        // Transfer user token from Alice to Bob
        val aliceUtxos = provider.findUtxos(Alice.address).await().toOption.get
        val userTokenName = EditableNftValidator.userNftName(tokenId)
        val userNftUtxo = aliceUtxos
            .find { case (_, out) =>
                out.value.assets.assets.exists { case (cs, tokens) =>
                    cs == txCreator.policyId && tokens.get(AssetName(userTokenName)).exists(_ > 0)
                }
            }
            .map(Utxo.apply)
            .get

        val transferTx = TxBuilder(env, PlutusScriptEvaluator.constMaxBudget(env))
            .spend(userNftUtxo)
            .payTo(Bob.address, Value.asset(txCreator.policyId, AssetName(userTokenName), 1))
            .complete(availableUtxos = aliceUtxos, Alice.address)
            .sign(Alice.signer)
            .transaction

        provider.submit(transferTx).await()

        // Edit (Bob - new owner)
        val bobUtxos = provider.findUtxos(Bob.address).await().toOption.get
        val editTx2 = txCreator.edit(
          utxos = bobUtxos,
          refNftUtxo = refNftUtxo,
          newData = utf8"Bob edit",
          changeAddress = Bob.address,
          signer = Bob.signer
        )
        val editResult2 = provider.submit(editTx2).await()
        assert(editResult2.isRight, s"Bob should be able to edit after transfer: $editResult2")
        refNftUtxo = Utxo(editTx2.utxos.find(_._2.address == txCreator.scriptAddr).get)

        // Seal (Bob)
        val bobUtxos2 = provider.findUtxos(Bob.address).await().toOption.get
        val sealTx = txCreator.seal(
          utxos = bobUtxos2,
          refNftUtxo = refNftUtxo,
          changeAddress = Bob.address,
          signer = Bob.signer
        )
        val sealResult = provider.submit(sealTx).await()
        assert(sealResult.isRight, s"Bob should be able to seal: $sealResult")

        // Verify final state
        val finalDatum = sealTx.utxos
            .find(_._2.address == txCreator.scriptAddr)
            .map(_._2.inlineDatum.get.to[ReferenceNftDatum])
            .get
        assert(finalDatum.isSealed, "NFT should be sealed")
        assert(finalDatum.data == utf8"Bob edit", "Data should reflect Bob's edit")
    }

    test("Burn: successful burn removes both tokens") {
        val provider = createProvider()
        val utxos = provider.findUtxos(Alice.address).await().toOption.get
        val seedUtxo = Utxo(utxos.head)
        // Burn uses both spend + mint scripts, so we need proper cost evaluation
        // (constMaxBudget assigns max budget per script, which exceeds tx limit with 2 scripts)
        val txCreator = createTxCreator(
          seedUtxo,
          PlutusScriptEvaluator(env, EvaluatorMode.EvaluateAndComputeCost)
        )

        // Mint
        val mintTx = txCreator.mint(
          utxos = utxos,
          tokenId = tokenId,
          initialData = initialData,
          holderAddress = Alice.address,
          changeAddress = Alice.address,
          signer = Alice.signer
        )
        provider.submit(mintTx).await()
        val refNftUtxo = Utxo(mintTx.utxos.find(_._2.address == txCreator.scriptAddr).get)

        // Burn
        val utxos1 = provider.findUtxos(Alice.address).await().toOption.get
        val burnTx = txCreator.burn(
          utxos = utxos1,
          refNftUtxo = refNftUtxo,
          changeAddress = Alice.address,
          signer = Alice.signer
        )
        val burnResult = provider.submit(burnTx).await()
        assert(burnResult.isRight, s"Burn should succeed: $burnResult")

        // Verify tokens are burned (no longer exist)
        val refTokenName = EditableNftValidator.refNftName(tokenId)
        val userTokenName = EditableNftValidator.userNftName(tokenId)

        val noRefNft = burnTx.utxos.forall { case (_, out) =>
            !out.value.assets.assets.exists { case (cs, tokens) =>
                cs == txCreator.policyId && tokens.get(AssetName(refTokenName)).exists(_ > 0)
            }
        }
        val noUserNft = burnTx.utxos.forall { case (_, out) =>
            !out.value.assets.assets.exists { case (cs, tokens) =>
                cs == txCreator.policyId && tokens.get(AssetName(userTokenName)).exists(_ > 0)
            }
        }

        assert(noRefNft, "Reference NFT should be burned")
        assert(noUserNft, "User NFT should be burned")
    }
}

object EditableNftValidatorTest extends ScalusTest {
    private given env: CardanoInfo = testEnvironment
    private val compiledContract = EditableNftContract.withErrorTraces

    def createTxCreator(
        seedUtxo: Utxo,
        evaluator: PlutusScriptEvaluator = PlutusScriptEvaluator.constMaxBudget(env)
    ): EditableNftTransactions =
        EditableNftTransactions(
          env = env,
          evaluator = evaluator,
          contract = compiledContract,
          seed = seedUtxo
        )

    // Test data
    val tokenId: scalus.builtin.ByteString = utf8"myNFT"
    val initialData: scalus.builtin.ByteString = utf8"Hello"

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
          )
        )

        Emulator(
          initialUtxos = initialUtxos,
          initialContext = Context.testMainnet()
        )
    }
}
