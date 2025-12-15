package scalus.cardano.txbuilder

import monocle.syntax.all.*
import org.scalatest.funsuite.AnyFunSuite
import scalus.builtin.ByteString.{hex, utf8}
import scalus.builtin.{ByteString, Data}
import scalus.builtin.Data.toData
import scalus.cardano.address.Address
import scalus.cardano.address.Network.Mainnet
import scalus.cardano.ledger.*
import scalus.cardano.ledger.DatumOption.Inline
import scalus.cardano.ledger.TransactionOutput.Babbage
import scalus.cardano.node.Emulator
import scalus.cardano.txbuilder.TestPeer.{Alice, Bob}
import scalus.prelude.List as PList
import scalus.utils.await
import scalus.{plutusV3, toUplc, Compiler}

import scala.collection.immutable.SortedMap
import scala.concurrent.ExecutionContext.Implicits.global

class TxBuilderTest extends AnyFunSuite {

    val testEnv: CardanoInfo = CardanoInfo.mainnet

    // Deterministic test mnemonic for cross-platform signature comparison
    private val testMnemonic =
        "test walk nut penalty hip pave soap entry language right filter choice"
    private val testDerivationPath = "m/1852'/1815'/0'/0/0"

    // Test scripts for validation - using PlutusV3
    val script1: Script.PlutusV3 = {
        val alwaysOk = Compiler.compile((sc: Data) => ())
        val alwaysOkCborBytes = alwaysOk.toUplc().plutusV3.cborByteString
        Script.PlutusV3(alwaysOkCborBytes)
    }

    val mintingPolicy: Script.PlutusV3 = script1

    def createScriptLockedUtxo(script: PlutusScript): Utxo = {
        val utxo = genAdaOnlyPubKeyUtxo(Alice).sample.get
        Utxo(
          utxo._1,
          utxo._2
              .focus(_.address)
              .replace(Address(Mainnet, Credential.ScriptHash(script.scriptHash)))
              .focus(_.datumOption)
              .replace(Some(Inline(42.toData)))
        )
    }

    test("TxBuilder spend without attaching script should fail when built") {
        val scriptUtxo = createScriptLockedUtxo(script1)
        val redeemer = Data.List(PList.Nil)

        val builder = TxBuilder(testEnv)
            .spend(scriptUtxo, redeemer)

        val exception = intercept[TxBuilderException.BuildStepException] {
            builder.build(changeTo = Alice.address)
        }

        assert(exception.getMessage.contains("No witness or ref/spent output is found for script"))
        assert(exception.getMessage.contains(scriptUtxo.output.address.scriptHashOption.get.toHex))
    }

    test("TxBuilder mint without attaching script should fail when built") {
        val policyId: PolicyId = mintingPolicy.scriptHash
        val redeemer = Data.List(PList.Nil)
        val assets = Map(AssetName(hex"deadbeef") -> 100L)
        val utxo = genAdaOnlyPubKeyUtxo(Alice).sample.get

        val builder = TxBuilder(testEnv)
            .spend(Utxo(utxo))
            .mint(redeemer, policyId, assets)

        val exception = intercept[TxBuilderException.BuildStepException] {
            builder.build(changeTo = Alice.address)
        }

        assert(exception.getMessage.contains("No witness or ref/spent output is found for script"))
        assert(exception.getMessage.contains(policyId.toHex))
    }

    test("TxBuilder spend with script should include script in witness set") {
        val scriptUtxo = createScriptLockedUtxo(script1)
        val redeemer = Data.List(PList.Nil)
        val paymentUtxo = genAdaOnlyPubKeyUtxo(Alice, min = 50_000_000).sample.get
        val collateralUtxo = genAdaOnlyPubKeyUtxo(Alice, min = 5_000_000).sample.get

        val validFrom = java.time.Instant.now()
        val validTo = validFrom.plusSeconds(3600)
        val inlineDatum = 123.toData

        val tx = TxBuilder(testEnv)
            .spend(Utxo(paymentUtxo))
            .collaterals(Utxo(collateralUtxo))
            .spend(scriptUtxo, redeemer, script1)
            .payTo(Alice.address, Value.ada(1))
            .payTo(Bob.address, Value.ada(2), inlineDatum)
            .validFrom(validFrom)
            .validTo(validTo)
            .build(changeTo = Alice.address)
            .transaction

        val plutusV3Scripts = tx.witnessSet.plutusV3Scripts.toMap.values
        assert(plutusV3Scripts.exists(_.scriptHash == script1.scriptHash))

        assert(tx.body.value.inputs.toSeq.contains(scriptUtxo.input))

        assert(tx.witnessSet.redeemers.isDefined)
        val redeemers = tx.witnessSet.redeemers.get.value.toSeq
        assert(redeemers.toSeq.size == 1)
        assert(redeemers.head.tag == RedeemerTag.Spend)
        assert(
          redeemers.head.index == tx.body.value.inputs.toSeq.indexWhere(_ == scriptUtxo.input)
        )

        assert(tx.body.value.collateralInputs.toSeq.contains(collateralUtxo._1))

        val bobOutput = tx.body.value.outputs.find(_.value.address == Bob.address)
        assert(bobOutput.isDefined)
        assert(bobOutput.get.value.datumOption.isDefined)
        bobOutput.get.value.datumOption.get match {
            case Inline(data) => assert(data == inlineDatum)
            case _            => fail("Expected inline datum")
        }

        assert(tx.body.value.validityStartSlot.isDefined)
        assert(
          tx.body.value.validityStartSlot.get == testEnv.slotConfig.timeToSlot(
            validFrom.toEpochMilli
          )
        )

        assert(tx.body.value.ttl.isDefined)
        assert(tx.body.value.ttl.get == testEnv.slotConfig.timeToSlot(validTo.toEpochMilli))
    }

    test("TxBuilder mint with script and payTo sends minted tokens to specified output") {
        val redeemer = Data.List(PList.Nil)
        val assetName1 = AssetName(hex"deadbeef")
        val assetName2 = AssetName(hex"cafebabe")
        val assets = Map(
          assetName1 -> 100L,
          assetName2 -> 50L
        )
        val utxo = genAdaOnlyPubKeyUtxo(Alice, min = 20_000_000).sample.get

        val paymentValue = Value(
          coin = Coin.ada(2),
          assets = MultiAsset(
            SortedMap(
              mintingPolicy.scriptHash -> SortedMap(
                assetName1 -> 100L,
                assetName2 -> 50L
              )
            )
          )
        )

        val metadata = AuxiliaryData.Metadata(
          Map(
            Word64(674L) -> Metadatum.Text("Test metadata"),
            Word64(1234L) -> Metadatum.Int(42L)
          )
        )

        val tx = TxBuilder(testEnv)
            .spend(Utxo(utxo))
            .mintAndAttach(redeemer, assets, mintingPolicy)
            .payTo(Bob.address, paymentValue)
            .metadata(metadata)
            .build(changeTo = Alice.address)
            .transaction

        // Check that mintingPolicy is in the plutus V3 scripts witness set
        val plutusV3Scripts = tx.witnessSet.plutusV3Scripts.toMap.values
        assert(plutusV3Scripts.exists(_.scriptHash == mintingPolicy.scriptHash))

        // Check that mint field is present
        assert(tx.body.value.mint.isDefined)
        val mint = tx.body.value.mint.get
        assert(mint.assets.contains(mintingPolicy.scriptHash))
        val mintedAssets = mint.assets(mintingPolicy.scriptHash)
        assert(mintedAssets.get(assetName1).contains(100L))
        assert(mintedAssets.get(assetName2).contains(50L))

        // Check that redeemers are present for minting
        assert(tx.witnessSet.redeemers.isDefined)
        val redeemers = tx.witnessSet.redeemers.get.value.toSeq
        assert(redeemers.toSeq.size == 1)
        assert(redeemers.head.tag == RedeemerTag.Mint)
        assert(redeemers.head.index == 0)

        // Check that tokens appear in Bob's output
        val bobOutputs = tx.body.value.outputs.filter(_.value.address == Bob.address)
        assert(bobOutputs.nonEmpty, "Should have at least one output to Bob")

        val bobOutputWithTokens = bobOutputs.find { output =>
            output.value.value.assets.assets.get(mintingPolicy.scriptHash).exists { assetsMap =>
                assetsMap.get(assetName1).contains(100L) && assetsMap.get(assetName2).contains(50L)
            }
        }
        assert(bobOutputWithTokens.isDefined, "Minted tokens should be in Bob's payment output")

        assert(bobOutputWithTokens.get.value.value.coin.value >= 2_000_000L)

        assert(tx.auxiliaryData.isDefined)
        val auxData = tx.auxiliaryData.get.value
        val md = auxData.getMetadata
        assert(md.contains(Word64(674L)))
        assert(md(Word64(674L)) == Metadatum.Text("Test metadata"))
        assert(md.contains(Word64(1234L)))
        assert(md(Word64(1234L)) == Metadatum.Int(42L))
    }

    test("TxBuilder sends specified TransactionOutput") {
        val utxo = genAdaOnlyPubKeyUtxo(Alice, min = 10_000_000).sample.get

        val customOutput = TransactionOutput(
          address = Bob.address,
          value = Value.ada(3),
          datumOption = Some(Inline(hex"deadbeef".toData)),
          scriptRef = None
        )

        val tx = TxBuilder(testEnv)
            .spend(Utxo(utxo))
            .output(customOutput)
            .build(changeTo = Alice.address)
            .transaction

        val bobOutputs = tx.body.value.outputs.filter(_.value.address == Bob.address)
        assert(bobOutputs.size == 1)

        val bobOutput = bobOutputs.head
        assert(bobOutput.value.value.coin.value == 3_000_000L)
        assert(bobOutput.value.datumOption.isDefined)
        bobOutput.value.datumOption.get match {
            case Inline(data) => assert(data == hex"deadbeef".toData)
            case _            => fail("Expected inline datum")
        }
    }

    test("TxBuilder payTo with datum hash includes datum in witness set") {
        val utxo = genAdaOnlyPubKeyUtxo(Alice, min = 10_000_000).sample.get

        val datum = Data.Constr(0, PList(100.toData, hex"abcd".toData))
        val datumHash = DataHash.fromByteString(
          scalus.builtin.Builtins.blake2b_256(scalus.builtin.Builtins.serialiseData(datum))
        )

        val tx = TxBuilder(testEnv)
            .spend(Utxo(utxo))
            .attach(datum)
            .payTo(Bob.address, Value.ada(2), datumHash)
            .build(changeTo = Alice.address)
            .transaction

        val bobOutput = tx.body.value.outputs.find(_.value.address == Bob.address)
        assert(bobOutput.isDefined)
        assert(bobOutput.get.value.datumOption.isDefined)
        bobOutput.get.value.datumOption.get match {
            case DatumOption.Hash(hash) => assert(hash == datumHash)
            case _                      => fail("Expected datum hash")
        }

        val plutusData = tx.witnessSet.plutusData.value.toMap.values.map(_.value)
        assert(plutusData.exists(_ == datum))
    }

    test("TxBuilder attach preserves multiple data in witness set") {
        val utxo = genAdaOnlyPubKeyUtxo(Alice, min = 10_000_000).sample.get

        val datum1 = 111.toData
        val datum2 = hex"aabbcc".toData
        val datum3 = Data.Constr(1, PList(222.toData))

        val tx = TxBuilder(testEnv)
            .spend(Utxo(utxo))
            .attach(datum1)
            .attach(datum2)
            .attach(datum3)
            .payTo(Bob.address, Value.ada(1))
            .build(changeTo = Alice.address)
            .transaction

        val plutusData = tx.witnessSet.plutusData.value.toMap.values.map(_.value).toSeq
        assert(plutusData.contains(datum1))
        assert(plutusData.contains(datum2))
        assert(plutusData.contains(datum3))
    }

    test("TxBuilder spend script UTXO without script or redeemer should fail") {
        val scriptUtxo = createScriptLockedUtxo(script1)

        val builder = TxBuilder(testEnv)
            .spend(scriptUtxo)
            .payTo(Bob.address, Value.ada(20))

        val exception = intercept[TxBuilderException.BuildStepException] {
            builder.build(changeTo = Alice.address)
        }
        succeed
    }

    test("TxBuilder should return tokens to change when spending token UTXO") {
        val co2 = AssetName(utf8"co2")
        val policyId = mintingPolicy.scriptHash
        val tokenAmount = 1000L

        // has tokens
        val inputValue = Value.asset(policyId, co2, tokenAmount, Coin.ada(10))
        val tokenUtxo =
            (genTransactionInput.sample.get, TransactionOutput(Alice.address, inputValue))

        // Only send ADA, no tokens
        val tx = TxBuilder(testEnv)
            .spend(Utxo(tokenUtxo))
            .payTo(Bob.address, Value.ada(5))
            .build(changeTo = Alice.address)
            .transaction

        // Bob should receive only ADA
        val bobOutputs = outputsOf(Bob, tx)
        assert(bobOutputs.nonEmpty, "Should have output to Bob")
        assert(bobOutputs.head.value.value.assets.isEmpty, "Bob should not receive tokens")

        // Alice should receive all tokens back as change
        val aliceOutputs = outputsOf(Alice, tx)
        assert(aliceOutputs.nonEmpty, "Should have change output to Alice")

        val aliceTokens = aliceOutputs.head.value.value.assets.assets(policyId)(co2)

        assert(
          aliceTokens == tokenAmount,
          s"Alice should receive all $tokenAmount tokens as change"
        )

    }

    test("TxBuilder should handle partial token sends with correct change") {
        val co2 = AssetName(utf8"co2")
        val policyId = mintingPolicy.scriptHash

        val tokenAmount = 1000L
        val sendAmount = 300L

        val inputValue = Value.asset(policyId, co2, tokenAmount, Coin.ada(10))
        val tokenUtxo =
            (genTransactionInput.sample.get, TransactionOutput(Alice.address, inputValue))

        val paymentValue = Value.asset(policyId, co2, sendAmount, Coin.ada(3))

        val tx = TxBuilder(testEnv)
            .spend(Utxo(tokenUtxo))
            .payTo(Bob.address, paymentValue)
            .build(changeTo = Alice.address)
            .transaction

        // Bob should receive 300 tokens
        val bobOutputs = outputsOf(Bob, tx)
        assert(bobOutputs.nonEmpty, "Should have output to Bob")
        val bobTokens = bobOutputs.head.value.value.assets.assets(policyId)(co2)
        assert(bobTokens == sendAmount, s"Bob should receive $sendAmount tokens")

        // Alice should receive remaining 700 tokens as change
        val aliceOutputs = outputsOf(Alice, tx)
        assert(aliceOutputs.nonEmpty, "Should have change output to Alice")
        val aliceTokens = aliceOutputs.head.value.value.assets.assets(policyId)(co2)
        val expectedChange = tokenAmount - sendAmount
        assert(
          aliceTokens == expectedChange,
          s"Alice should receive $expectedChange tokens as change"
        )
    }

    test("TxBuilder should handle multiple token types in change") {
        val co2 = AssetName(utf8"co2")
        val h2so4 = AssetName(utf8"h2so4")
        val policyId = mintingPolicy.scriptHash

        val inputValue = Value(
          Coin.ada(10),
          MultiAsset(
            SortedMap(
              policyId -> SortedMap(
                co2 -> 500L,
                h2so4 -> 300L
              )
            )
          )
        )
        val utxo = genAdaOnlyPubKeyUtxo(Alice, min = 10_000_000).sample.get
        val tokenUtxo =
            (genTransactionInput.sample.get, TransactionOutput(Alice.address, inputValue))

        // Send only co2, no h2so4
        val paymentValue = Value.asset(policyId, co2, 200, Coin.ada(3))

        val tx = TxBuilder(testEnv)
            .spend(Utxo(tokenUtxo))
            .payTo(Bob.address, paymentValue)
            .build(changeTo = Alice.address)
            .transaction

        // Bob should receive 200 co2
        val bobOutputs = outputsOf(Bob, tx)
        val bobAssets = bobOutputs.head.value.value.assets.assets(policyId)
        assert(bobAssets(co2) == 200, "Bob should receive 200 co2")
        assert(!bobAssets.contains(h2so4), "Bob should not receive h2so4")

        // Alice should receive remaining co2 + all h2so4 as change
        val aliceOutputs = outputsOf(Alice, tx)
        val aliceAssets = aliceOutputs.head.value.value.assets.assets(policyId)
        assert(aliceAssets(co2) == 300L, "Alice should receive 300 co2 as change")
        assert(aliceAssets(h2so4) == 300L, "Alice should receive all 300 h2so4 as change")
    }

    private def outputsOf(peer: TestPeer, tx: Transaction) =
        tx.body.value.outputs.toSeq.filter(_.value.address == peer.address)

    // ============================================================================
    // Transaction Chaining Tests
    // ============================================================================

    test("Transaction.utxos returns outputs with correct transaction inputs") {
        val utxo = genAdaOnlyPubKeyUtxo(Alice, min = 10_000_000).sample.get

        val tx = TxBuilder(testEnv)
            .spend(Utxo(utxo))
            .payTo(Bob.address, Value.ada(2))
            .payTo(Alice.address, Value.ada(3))
            .build(changeTo = Alice.address)
            .transaction

        val txUtxos = tx.utxos

        // Should have outputs for Bob, Alice explicit, and change
        assert(txUtxos.nonEmpty, "Transaction should create UTXOs")

        // Check that all UTXOs reference this transaction's ID
        txUtxos.foreach { case (input, _) =>
            assert(input.transactionId == tx.id, "UTXO input should reference transaction ID")
        }

        // Check that indices are sequential starting from 0
        val indices = txUtxos.keys.map(_.index).toSeq.sorted
        assert(
          indices == (0 until tx.body.value.outputs.size).toSeq,
          "Indices should be sequential"
        )

        // Check that outputs match transaction body outputs
        txUtxos.foreach { case (input, output) =>
            val bodyOutput = tx.body.value.outputs(input.index.toInt)
            assert(output == bodyOutput.value, "UTXO output should match transaction body output")
        }
    }

    test("transaction chaining - use outputs from tx1 as inputs in tx2") {
        val genesisHash = TransactionHash.fromByteString(ByteString.fromHex("0" * 64))

        // Use Emulator to build transactions - disable validators for simplicity
        // (signature verification is tested elsewhere in TransactionSignerTest)
        val provider = Emulator(
          initialUtxos = Map(
            TransactionInput(genesisHash, 0) -> TransactionOutput(Alice.address, Value.ada(100))
          ),
          validators = Set.empty,
          mutators = Emulator.defaultMutators
        )

        // Build tx1: send ADA to Bob
        val tx1 = TxBuilder(testEnv)
            .payTo(Bob.address, Value.ada(10))
            .complete(provider, Alice.address)
            .await()
            .transaction

        // Get UTXOs from tx1 directly using Transaction.utxos
        val bobUtxoFromTx1 = Utxo(tx1.utxos.find(_._2.address == Bob.address).get)
        assert(
          bobUtxoFromTx1.output.value.coin == Coin.ada(10),
          "Bob's UTXO should have 10 ADA"
        )

        // Verify the UTXO references tx1's hash
        assert(
          bobUtxoFromTx1.input.transactionId == tx1.id,
          "UTXO should reference tx1's transaction ID"
        )

        // Submit tx1 to provider - now Bob's UTXO exists in the chain
        val tx1Result = provider.submit(tx1).await()
        assert(tx1Result.isRight, s"tx1 should be submitted successfully: $tx1Result")

        // Now use the UTXO from tx1 to build tx2
        // Alice uses tx1.utxos to find her change output
        val aliceChangeFromTx1 = Utxo(tx1.utxos.find(_._2.address == Alice.address).get)

        // Build tx2 using UTXOs derived from tx1.utxos
        val tx2 = TxBuilder(testEnv)
            .spend(bobUtxoFromTx1) // Spend the UTXO from tx1 that we got via Transaction.utxos
            .spend(aliceChangeFromTx1) // Alice's change for fees
            .payTo(Alice.address, Value.ada(10))
            .build(changeTo = Alice.address)
            .transaction

        // The key assertion: Transaction.utxos produces valid references that can be used in tx2
        val tx2Inputs = tx2.body.value.inputs.toSeq
        assert(
          tx2Inputs.contains(bobUtxoFromTx1.input),
          "tx2 should contain Bob's UTXO from tx1 as input"
        )
        assert(
          tx2Inputs.contains(aliceChangeFromTx1.input),
          "tx2 should contain Alice's change from tx1 as input"
        )

        // Submit tx2 to provider
        val tx2Result = provider.submit(tx2).await()
        assert(tx2Result.isRight, s"tx2 should be submitted successfully: $tx2Result")

        // Verify Alice received the ADA back
        val aliceUtxo =
            provider.findUtxo(address = Alice.address, transactionId = Some(tx2.id)).await()
        assert(aliceUtxo.isRight, "Alice should have received the UTXO from tx2")
    }
}
