package scalus.examples.editablenft

import org.scalatest.funsuite.AnyFunSuite
import scalus.uplc.builtin.ByteString.utf8
import scalus.uplc.builtin.Data
import scalus.uplc.builtin.Data.toData
import scalus.cardano.ledger.*
import scalus.cardano.ledger.rules.Context
import scalus.cardano.node.Emulator
import scalus.cardano.onchain.plutus.v3.{TxId, TxOutRef}
import scalus.cardano.txbuilder.TxBuilder
import scalus.testing.kit.Party.{Alice, Bob}
import scalus.testing.kit.ScalusTest
import scalus.testing.kit.TestUtil.{genesisHash, testEnvironment}
import scalus.utils.await

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

    test("CIP-68 token names use canonical 4-byte labels (100=ref, 222=user)") {
        val ref = EditableNftValidator.refNftName(tokenId).toHex
        val user = EditableNftValidator.userNftName(tokenId).toHex
        // CIP-67/68: reference token label 100 = 0x000643b0, user token label 222 = 0x000de140.
        assert(ref.startsWith("000643b0"), s"reference NFT must use CIP-68 label 100, got $ref")
        assert(user.startsWith("000de140"), s"user NFT must use CIP-68 label 222, got $user")
    }

    test("Mint: cannot mint under the same policy without spending the seed") {
        val provider = createProvider()
        val utxos = provider.findUtxos(Alice.address).await().toOption.get
        val seedUtxo = Utxo(utxos.head)
        val txCreator = createTxCreator(seedUtxo)

        // Legitimate first mint consumes the seed.
        val mintTx = txCreator.mint(
          utxos = utxos,
          tokenId = tokenId,
          initialData = initialData,
          holderAddress = Alice.address,
          changeAddress = Alice.address,
          signer = Alice.signer
        )
        assert(provider.submit(mintTx).await().isRight, "first mint should succeed")

        // Attacker mints again under the SAME policy (same seed parameter, same policyId) without
        // spending the seed, pointing seedIndex at an arbitrary owned input. The one-shot guarantee
        // must reject this.
        val aliceUtxos = provider.findUtxos(Alice.address).await().toOption.get
        val decoy = Utxo(aliceUtxos.head)
        val parameterizedScript = EditableNftContract.withErrorTraces.apply(
          TxOutRef(TxId(seedUtxo.input.transactionId), BigInt(seedUtxo.input.index)).toData
        )
        val tokenId2 = utf8"forgery"
        val refAsset = EditableNftValidator.refNftName(tokenId2)
        val userAsset = EditableNftValidator.userNftName(tokenId2)
        val refDatum = ReferenceNftDatum(tokenId2, initialData, isSealed = false)

        def buildRedeemer(tx: Transaction): Data = {
            val seedIdx = tx.body.value.inputs.toSeq.indexWhere(i =>
                i.transactionId == decoy.input.transactionId && i.index == decoy.input.index
            )
            val refIdx =
                tx.body.value.outputs.toSeq.indexWhere(_.value.address == txCreator.scriptAddr)
            MintRedeemer.Mint(BigInt(seedIdx), BigInt(refIdx)).toData
        }

        val attackTx = TxBuilder(env, PlutusScriptEvaluator.constMaxBudget(env))
            .spend(decoy)
            .mint(
              parameterizedScript,
              Map(AssetName(refAsset) -> 1L, AssetName(userAsset) -> 1L),
              buildRedeemer
            )
            .payTo(
              txCreator.scriptAddr,
              Value.asset(txCreator.policyId, AssetName(refAsset), 1),
              refDatum
            )
            .payTo(Alice.address, Value.asset(txCreator.policyId, AssetName(userAsset), 1))
            .complete(availableUtxos = aliceUtxos, Alice.address)
            .sign(Alice.signer)
            .transaction

        val result = provider.submit(attackTx).await()
        assert(result.isLeft, s"minting without spending the seed must fail, got: $result")
    }

    test("Mint: cannot mint a pair via the Burn redeemer (empty burn branch side door)") {
        val provider = createProvider()
        val utxos = provider.findUtxos(Alice.address).await().toOption.get
        val seedUtxo = Utxo(utxos.head)
        val txCreator = createTxCreator(seedUtxo)

        val parameterizedScript = EditableNftContract.withErrorTraces.apply(
          TxOutRef(TxId(seedUtxo.input.transactionId), BigInt(seedUtxo.input.index)).toData
        )
        val tokenId2 = utf8"forged-via-burn"
        val refAsset = EditableNftValidator.refNftName(tokenId2)
        val userAsset = EditableNftValidator.userNftName(tokenId2)

        // Attacker mints a fresh ref/user pair using the Burn redeemer. The seed is never spent and
        // no script UTxO is involved, so only the minting policy governs — and its Burn branch must
        // refuse to mint (positive quantities), otherwise the one-shot seed check is bypassed.
        val attackTx = TxBuilder(env, PlutusScriptEvaluator.constMaxBudget(env))
            .mint(
              parameterizedScript,
              Map(AssetName(refAsset) -> 1L, AssetName(userAsset) -> 1L),
              _ => MintRedeemer.Burn.toData
            )
            .payTo(Alice.address, Value.asset(txCreator.policyId, AssetName(refAsset), 1))
            .payTo(Alice.address, Value.asset(txCreator.policyId, AssetName(userAsset), 1))
            .complete(availableUtxos = utxos, Alice.address)
            .sign(Alice.signer)
            .transaction

        val result = provider.submit(attackTx).await()
        assert(result.isLeft, s"minting via the Burn redeemer must fail, got: $result")
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
        assertResult(
          ExUnits(memory = 193165L, steps = 56517151L)
        ):
            burnTx.witnessSet.redeemers.get.value.totalExUnits
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
    val tokenId: scalus.uplc.builtin.ByteString = utf8"myNFT"
    val initialData: scalus.uplc.builtin.ByteString = utf8"Hello"

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
