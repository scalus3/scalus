package scalus.cardano.txbuilder

import org.scalacheck.Arbitrary
import org.scalatest.funsuite.AnyFunSuite
import scalus.uplc.builtin.ByteString.{hex, utf8}
import scalus.uplc.builtin.Data.toData
import scalus.uplc.builtin.{ByteString, Data}
import scalus.cardano.address.Address
import scalus.cardano.address.Network.Mainnet
import scalus.cardano.ledger.*
import scalus.cardano.ledger.DatumOption.Inline
import scalus.cardano.node.Emulator
import scalus.cardano.txbuilder.txBuilder
import scalus.compiler.compile
import scalus.cardano.onchain.plutus.prelude.List as PList
import scalus.testing.kit.Party
import scalus.testing.kit.Party.{Alice, Bob}
import scalus.testing.kit.TestUtil.genAdaOnlyPubKeyUtxo
import scalus.toUplc
import scalus.utils.await

import java.time.Instant
import scala.collection.immutable.SortedMap

class TxBuilderTest extends AnyFunSuite, scalus.cardano.ledger.ArbitraryInstances {

    given testEnv: CardanoInfo = CardanoInfo.mainnet

    val genesisHash: TransactionHash =
        TransactionHash.fromByteString(ByteString.fromHex("0" * 64))

    // Test scripts for validation - using PlutusV3
    val script1: Script.PlutusV3 = {
        val alwaysOk = compile((sc: Data) => ())
        val alwaysOkCborBytes = alwaysOk.toUplc().plutusV3.cborByteString
        Script.PlutusV3(alwaysOkCborBytes)
    }

    val mintingPolicy: Script.PlutusV3 = script1

    def createScriptLockedUtxo(script: PlutusScript): Utxo = {
        val utxo = genAdaOnlyPubKeyUtxo(Alice).sample.get
        val newOutput = Output(
          address = Address(Mainnet, Credential.ScriptHash(script.scriptHash)),
          value = utxo.output.value,
          inlineDatum = 42.toData,
        )
        utxo.copy(output = newOutput)
    }

    test("TxBuilder spend without attaching script should fail when built") {
        val scriptUtxo = createScriptLockedUtxo(script1)
        val redeemer = Data.List(PList.Nil)

        val builder = txBuilder
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

        val builder = txBuilder
            .spend(utxo)
            .mint(policyId, assets, redeemer)

        val exception = intercept[TxBuilderException.BuildStepException] {
            builder.build(changeTo = Alice.address)
        }

        assert(exception.getMessage.contains("No witness or ref/spent output is found for script"))
        assert(exception.getMessage.contains(policyId.toHex))
    }

    test("TxBuilder spend with script should include script in witness set") {
        val scriptUtxo = createScriptLockedUtxo(script1)
        val redeemer = Data.List(PList.Nil)
        val paymentUtxo = genAdaOnlyPubKeyUtxo(Alice, min = Coin.ada(50)).sample.get
        val collateralUtxo = genAdaOnlyPubKeyUtxo(Alice, min = Coin.ada(5)).sample.get

        val validFrom = Instant.now()
        val validTo = validFrom.plusSeconds(3600)
        val inlineDatum = 123.toData

        val tx = txBuilder
            .spend(paymentUtxo)
            .collaterals(collateralUtxo)
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
          tx.body.value.validityStartSlot.get == testEnv.slotConfig.instantToSlot(validFrom)
        )

        assert(tx.body.value.ttl.isDefined)
        assert(tx.body.value.ttl.get == testEnv.slotConfig.instantToSlot(validTo))
    }

    test("TxBuilder mint with script and payTo sends minted tokens to specified output") {
        val redeemer = Data.List(PList.Nil)
        val assetName1 = AssetName(hex"deadbeef")
        val assetName2 = AssetName(hex"cafebabe")
        val assets = Map(
          assetName1 -> 100L,
          assetName2 -> 50L
        )
        val utxo = genAdaOnlyPubKeyUtxo(Alice, min = Coin.ada(20)).sample.get

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

        val tx = txBuilder
            .spend(utxo)
            .mint(mintingPolicy, assets, redeemer)
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
        val utxo = genAdaOnlyPubKeyUtxo(Alice, min = Coin.ada(10)).sample.get

        val customOutput = Output(
          address = Bob.address,
          value = Value.ada(3),
          datumOption = Some(Inline(hex"deadbeef".toData)),
          scriptRef = None
        )

        val tx = txBuilder
            .spend(utxo)
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
        val utxo = genAdaOnlyPubKeyUtxo(Alice, min = Coin.ada(10)).sample.get

        val datum = Data.Constr(0, PList(100.toData, hex"abcd".toData))
        val datumHash = DataHash.fromByteString(
          scalus.uplc.builtin.Builtins
              .blake2b_256(scalus.uplc.builtin.Builtins.serialiseData(datum))
        )

        val tx = txBuilder
            .spend(utxo)
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
        val utxo = genAdaOnlyPubKeyUtxo(Alice, min = Coin.ada(10)).sample.get

        val datum1 = 111.toData
        val datum2 = hex"aabbcc".toData
        val datum3 = Data.Constr(1, PList(222.toData))

        val tx = txBuilder
            .spend(utxo)
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

        val builder = txBuilder
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
            (
              Arbitrary.arbitrary[TransactionInput].sample.get,
              Output(Alice.address, inputValue)
            )

        // Only send ADA, no tokens
        val tx = txBuilder
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
            (
              Arbitrary.arbitrary[TransactionInput].sample.get,
              Output(Alice.address, inputValue)
            )

        val paymentValue = Value.asset(policyId, co2, sendAmount, Coin.ada(3))

        val tx = txBuilder
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
        val utxo = genAdaOnlyPubKeyUtxo(Alice, min = Coin.ada(10)).sample.get
        val tokenUtxo =
            (
              Arbitrary.arbitrary[TransactionInput].sample.get,
              Output(Alice.address, inputValue)
            )

        // Send only co2, no h2so4
        val paymentValue = Value.asset(policyId, co2, 200, Coin.ada(3))

        val tx = txBuilder
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

    test("build should not merge change with existing output to same address") {
        // When payTo(changeAddress, amount) is used, change should go to a SEPARATE output,
        // not merge with the payment output. Users expect explicitly stated amounts.
        val utxo = genAdaOnlyPubKeyUtxo(Alice, min = Coin.ada(100)).sample.get

        val tx = txBuilder
            .spend(utxo)
            .payTo(Alice.address, Value.ada(10)) // Explicit payment to Alice
            .build(changeTo = Alice.address) // Alice is also change address
            .transaction

        // Should have TWO separate outputs to Alice: payment + change
        val aliceOutputs = outputsOf(Alice, tx)
        assert(
          aliceOutputs.size == 2,
          s"Should have 2 outputs to Alice (payment + change), got ${aliceOutputs.size}"
        )

        // One output should be around 10 ADA (the payment)
        val hasPaymentOutput = aliceOutputs.exists(out =>
            out.value.value.coin >= Coin.ada(10) && out.value.value.coin < Coin.ada(15)
        )
        assert(hasPaymentOutput, "Should have the ~10 ADA payment output")

        // Other output should be the change (bulk of remaining funds)
        val hasChangeOutput =
            aliceOutputs.exists(out => out.value.value.coin.value > Coin.ada(80).value)
        assert(hasChangeOutput, "Should have the change output with 80+ ADA")
    }

    private def outputsOf(party: Party, tx: Transaction) =
        tx.body.value.outputs.toSeq.filter(_.value.address == party.address)

    // ============================================================================
    // Transaction Chaining Tests
    // ============================================================================

    test("Transaction.utxos returns outputs with correct transaction inputs") {
        val utxo = genAdaOnlyPubKeyUtxo(Alice, min = Coin.ada(10)).sample.get

        val tx = txBuilder
            .spend(utxo)
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
        // Use Emulator to build transactions - disable validators for simplicity
        // (signature verification is tested elsewhere in TransactionSignerTest)
        val provider = Emulator(
          initialUtxos = Map(
            Input(genesisHash, 0) -> Output(Alice.address, Value.ada(100))
          ),
          validators = Set.empty,
          mutators = Emulator.defaultMutators
        )

        // Build tx1: send ADA to Bob
        val tx1 = txBuilder
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
        val tx2 = txBuilder
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
        val aliceUtxo = provider
            .queryUtxos { u =>
                u.output.address == Alice.address && u.input.transactionId == tx2.id
            }
            .execute()
            .await()
            .map(utxos => Utxo(utxos.head))
        assert(aliceUtxo.isRight, "Alice should have received the UTXO from tx2")
    }

    // ============================================================================
    // Script Witness Factory Methods Tests
    // ============================================================================

    test("TxBuilder mint with unified ScriptWitness API (attached)") {
        import TwoArgumentPlutusScriptWitness.*

        val policyId: PolicyId = mintingPolicy.scriptHash
        val redeemer = Data.List(PList.Nil)
        val assets = Map(AssetName(hex"deadbeef") -> 100L)
        val utxo = genAdaOnlyPubKeyUtxo(Alice, min = Coin.ada(20)).sample.get
        val collateralUtxo = genAdaOnlyPubKeyUtxo(Alice, min = Coin.ada(5)).sample.get

        val validFrom = Instant.now()
        val validTo = validFrom.plusSeconds(3600)

        val tx = txBuilder
            .spend(utxo)
            .collaterals(collateralUtxo)
            .mint(policyId, assets, attached(mintingPolicy, redeemer))
            .validFrom(validFrom)
            .validTo(validTo)
            .build(changeTo = Alice.address)
            .transaction

        // Assert script is in witness set
        val plutusV3Scripts = tx.witnessSet.plutusV3Scripts.toMap.values
        assert(plutusV3Scripts.exists(_.scriptHash == mintingPolicy.scriptHash))
        // Assert mint is present
        assert(tx.body.value.mint.isDefined)
    }

    test("TxBuilder mint with unified ScriptWitness API (reference)") {
        import TwoArgumentPlutusScriptWitness.*

        val policyId: PolicyId = mintingPolicy.scriptHash
        val redeemer = Data.List(PList.Nil)
        val assets = Map(AssetName(hex"deadbeef") -> 100L)
        val utxo = genAdaOnlyPubKeyUtxo(Alice, min = Coin.ada(20)).sample.get
        val collateralUtxo = genAdaOnlyPubKeyUtxo(Alice, min = Coin.ada(5)).sample.get

        // Create reference script UTxO
        val refScriptUtxo = (
          Arbitrary.arbitrary[TransactionInput].sample.get,
          Output(Alice.address, Value.ada(5), None, Some(ScriptRef(mintingPolicy)))
        )

        val validFrom = Instant.now()
        val validTo = validFrom.plusSeconds(3600)

        val tx = txBuilder
            .spend(utxo)
            .collaterals(collateralUtxo)
            .references(Utxo(refScriptUtxo))
            .mint(policyId, assets, reference(redeemer))
            .validFrom(validFrom)
            .validTo(validTo)
            .build(changeTo = Alice.address)
            .transaction

        // Assert mint is present
        assert(tx.body.value.mint.isDefined)
        // Assert reference input is present
        assert(tx.body.value.referenceInputs.toSeq.nonEmpty)
    }

    test("TxBuilder withdrawRewards with script witness builds correctly") {
        import TwoArgumentPlutusScriptWitness.*
        import scalus.cardano.address.{StakeAddress, StakePayload}

        val scriptStakeAddress = StakeAddress(
          Mainnet,
          StakePayload.Script(script1.scriptHash)
        )
        val redeemer = Data.unit
        val utxo = genAdaOnlyPubKeyUtxo(Alice, min = Coin.ada(50)).sample.get
        val collateralUtxo = genAdaOnlyPubKeyUtxo(Alice, min = Coin.ada(5)).sample.get

        val validFrom = Instant.now()
        val validTo = validFrom.plusSeconds(3600)

        val tx = txBuilder
            .spend(utxo)
            .collaterals(collateralUtxo)
            .withdrawRewards(scriptStakeAddress, Coin(1_000_000), attached(script1, redeemer))
            .validFrom(validFrom)
            .validTo(validTo)
            .build(changeTo = Alice.address)
            .transaction

        // Assert script is in witness set
        val plutusV3Scripts = tx.witnessSet.plutusV3Scripts.toMap.values
        assert(plutusV3Scripts.exists(_.scriptHash == script1.scriptHash))
        // Assert withdrawal is present
        assert(tx.body.value.withdrawals.isDefined)
        // Assert redeemer has Rewarding tag
        assert(tx.witnessSet.redeemers.isDefined)
        val redeemers = tx.witnessSet.redeemers.get.value.toSeq
        assert(redeemers.exists(_.tag == RedeemerTag.Reward))
    }

    test("TxBuilder delegateTo with script witness builds correctly") {
        import TwoArgumentPlutusScriptWitness.*
        import scalus.cardano.address.{StakeAddress, StakePayload}

        val scriptStakeAddress = StakeAddress(
          Mainnet,
          StakePayload.Script(script1.scriptHash)
        )
        val poolId = PoolKeyHash.fromHex("0" * 56)
        val redeemer = Data.unit
        val utxo = genAdaOnlyPubKeyUtxo(Alice, min = Coin.ada(50)).sample.get
        val collateralUtxo = genAdaOnlyPubKeyUtxo(Alice, min = Coin.ada(5)).sample.get

        val validFrom = Instant.now()
        val validTo = validFrom.plusSeconds(3600)

        val tx = txBuilder
            .spend(utxo)
            .collaterals(collateralUtxo)
            .delegateTo(scriptStakeAddress, poolId, attached(script1, redeemer))
            .validFrom(validFrom)
            .validTo(validTo)
            .build(changeTo = Alice.address)
            .transaction

        // Assert script is in witness set
        val plutusV3Scripts = tx.witnessSet.plutusV3Scripts.toMap.values
        assert(plutusV3Scripts.exists(_.scriptHash == script1.scriptHash))
        // Assert certificate is present
        val certs = tx.body.value.certificates.toSeq
        assert(certs.exists {
            case Certificate.StakeDelegation(cred, pid) =>
                cred == scriptStakeAddress.credential && pid == poolId
            case _ => false
        })
    }

    test("TxBuilder registerStake with script witness builds correctly") {
        import TwoArgumentPlutusScriptWitness.*
        import scalus.cardano.address.{StakeAddress, StakePayload}

        val scriptStakeAddress = StakeAddress(
          Mainnet,
          StakePayload.Script(script1.scriptHash)
        )
        val redeemer = Data.unit
        val utxo = genAdaOnlyPubKeyUtxo(Alice, min = Coin.ada(50)).sample.get
        val collateralUtxo = genAdaOnlyPubKeyUtxo(Alice, min = Coin.ada(5)).sample.get

        val validFrom = Instant.now()
        val validTo = validFrom.plusSeconds(3600)

        val tx = txBuilder
            .spend(utxo)
            .collaterals(collateralUtxo)
            .registerStake(scriptStakeAddress, attached(script1, redeemer))
            .validFrom(validFrom)
            .validTo(validTo)
            .build(changeTo = Alice.address)
            .transaction

        // Assert script is in witness set
        val plutusV3Scripts = tx.witnessSet.plutusV3Scripts.toMap.values
        assert(plutusV3Scripts.exists(_.scriptHash == script1.scriptHash))
        // Assert certificate is present with Conway-style (explicit deposit)
        val certs = tx.body.value.certificates.toSeq
        val regCert = certs.collectFirst {
            case cert @ Certificate.RegCert(cred, coinOpt)
                if cred == scriptStakeAddress.credential =>
                (cert, coinOpt)
        }
        assert(regCert.isDefined, s"RegCert should be present. Certs: $certs")
        // Conway-style certificate must have explicit deposit for script execution
        assert(
          regCert.get._2.isDefined,
          "RegCert should have explicit deposit (Conway-style) for script execution"
        )
        // Assert certificate redeemer is present
        val redeemers = tx.witnessSet.redeemers.map(_.value.toSeq).getOrElse(Seq.empty)
        val certRedeemer = redeemers.find(_.tag == RedeemerTag.Cert)
        assert(certRedeemer.isDefined, "Certificate redeemer should be present")
        assert(certRedeemer.get.index == 0, "Certificate redeemer index should be 0")
    }

    test("TxBuilder registerStake with reference script witness includes certificate redeemer") {
        import TwoArgumentPlutusScriptWitness.*
        import scalus.cardano.address.{StakeAddress, StakePayload}

        val scriptStakeAddress = StakeAddress(Mainnet, StakePayload.Script(script1.scriptHash))
        val utxo = genAdaOnlyPubKeyUtxo(Alice, min = Coin.ada(50)).sample.get
        val collateralUtxo = genAdaOnlyPubKeyUtxo(Alice, min = Coin.ada(5)).sample.get

        // Create a reference UTxO with the script
        val refInput = Arbitrary.arbitrary[TransactionInput].sample.get
        val refOutput = Output(
          address = Alice.address,
          value = Value.ada(10),
          datumOption = None,
          scriptRef = Some(ScriptRef(script1))
        )
        val refUtxo = Utxo(refInput, refOutput)

        val validFrom = Instant.now()
        val validTo = validFrom.plusSeconds(3600)

        val tx = txBuilder
            .spend(utxo)
            .collaterals(collateralUtxo)
            .references(refUtxo)
            .registerStake(scriptStakeAddress, reference(Data.unit, Set.empty))
            .validFrom(validFrom)
            .validTo(validTo)
            .build(changeTo = Alice.address)
            .transaction

        // Assert certificate is present
        val certs = tx.body.value.certificates.toSeq
        assert(certs.exists {
            case Certificate.RegCert(cred, _) => cred == scriptStakeAddress.credential
            case _                            => false
        })
        // Assert certificate redeemer is present
        val redeemers = tx.witnessSet.redeemers.map(_.value.toSeq).getOrElse(Seq.empty)
        val certRedeemer = redeemers.find(_.tag == RedeemerTag.Cert)
        assert(
          certRedeemer.isDefined,
          "Certificate redeemer should be present when using reference script witness"
        )
        assert(certRedeemer.get.index == 0, "Certificate redeemer index should be 0")
    }

    test("TxBuilder mint + registerStake includes both redeemers") {
        import TwoArgumentPlutusScriptWitness.*
        import scalus.cardano.address.{StakeAddress, StakePayload}

        val scriptStakeAddress = StakeAddress(Mainnet, StakePayload.Script(script1.scriptHash))
        val utxo = genAdaOnlyPubKeyUtxo(Alice, min = Coin.ada(50)).sample.get
        val collateralUtxo = genAdaOnlyPubKeyUtxo(Alice, min = Coin.ada(5)).sample.get

        val validFrom = Instant.now()
        val validTo = validFrom.plusSeconds(3600)

        val tx = txBuilder
            .spend(utxo)
            .collaterals(collateralUtxo)
            .mint(
              policyId = script1.scriptHash,
              assets = Map(AssetName.empty -> 1L),
              witness = attached(script1, Data.unit)
            )
            .registerStake(scriptStakeAddress, attached(script1, Data.unit))
            .validFrom(validFrom)
            .validTo(validTo)
            .build(changeTo = Alice.address)
            .transaction

        // Assert both redeemers are present
        val redeemers = tx.witnessSet.redeemers.map(_.value.toSeq).getOrElse(Seq.empty)
        val mintRedeemerOpt = redeemers.find(_.tag == RedeemerTag.Mint)
        val certRedeemerOpt = redeemers.find(_.tag == RedeemerTag.Cert)

        assert(mintRedeemerOpt.isDefined, s"Mint redeemer should be present. Redeemers: $redeemers")
        assert(
          certRedeemerOpt.isDefined,
          s"Certificate redeemer should be present. Redeemers: $redeemers"
        )
    }

    test("TxBuilder registerDRep with script witness builds correctly") {
        import TwoArgumentPlutusScriptWitness.*

        val scriptCredential = Credential.ScriptHash(script1.scriptHash)
        val utxo = genAdaOnlyPubKeyUtxo(Alice, min = Coin.ada(550)).sample.get
        val collateralUtxo = genAdaOnlyPubKeyUtxo(Alice, min = Coin.ada(5)).sample.get

        val validFrom = Instant.now()
        val validTo = validFrom.plusSeconds(3600)

        val tx = txBuilder
            .spend(utxo)
            .collaterals(collateralUtxo)
            .registerDRep(scriptCredential, None, attached(script1, Data.unit))
            .validFrom(validFrom)
            .validTo(validTo)
            .build(changeTo = Alice.address)
            .transaction

        // Assert script is in witness set
        val plutusV3Scripts = tx.witnessSet.plutusV3Scripts.toMap.values
        assert(plutusV3Scripts.exists(_.scriptHash == script1.scriptHash))
        // Assert certificate is present
        val certs = tx.body.value.certificates.toSeq
        assert(certs.exists {
            case Certificate.RegDRepCert(cred, _, _) =>
                cred == scriptCredential
            case _ => false
        })
    }

    test("TxBuilder delegateTo with script witness includes certificate redeemer") {
        import TwoArgumentPlutusScriptWitness.*
        import scalus.cardano.address.{StakeAddress, StakePayload}

        val scriptStakeAddress = StakeAddress(Mainnet, StakePayload.Script(script1.scriptHash))
        val poolId = PoolKeyHash.fromHex("0" * 56)
        val utxo = genAdaOnlyPubKeyUtxo(Alice, min = Coin.ada(50)).sample.get
        val collateralUtxo = genAdaOnlyPubKeyUtxo(Alice, min = Coin.ada(5)).sample.get

        val validFrom = Instant.now()
        val validTo = validFrom.plusSeconds(3600)

        val tx = txBuilder
            .spend(utxo)
            .collaterals(collateralUtxo)
            .delegateTo(scriptStakeAddress, poolId, attached(script1, Data.unit))
            .validFrom(validFrom)
            .validTo(validTo)
            .build(changeTo = Alice.address)
            .transaction

        val redeemers = tx.witnessSet.redeemers.map(_.value.toSeq).getOrElse(Seq.empty)
        val certRedeemerOpt = redeemers.find(_.tag == RedeemerTag.Cert)

        assert(
          certRedeemerOpt.isDefined,
          s"Certificate redeemer should be present. Redeemers: $redeemers"
        )
    }

    test("TxBuilder deregisterStake with script witness includes certificate redeemer") {
        import TwoArgumentPlutusScriptWitness.*
        import scalus.cardano.address.{StakeAddress, StakePayload}

        val scriptStakeAddress = StakeAddress(Mainnet, StakePayload.Script(script1.scriptHash))
        val utxo = genAdaOnlyPubKeyUtxo(Alice, min = Coin.ada(50)).sample.get
        val collateralUtxo = genAdaOnlyPubKeyUtxo(Alice, min = Coin.ada(5)).sample.get

        val validFrom = Instant.now()
        val validTo = validFrom.plusSeconds(3600)

        val tx = txBuilder
            .spend(utxo)
            .collaterals(collateralUtxo)
            .deregisterStake(scriptStakeAddress, attached(script1, Data.unit))
            .validFrom(validFrom)
            .validTo(validTo)
            .build(changeTo = Alice.address)
            .transaction

        val redeemers = tx.witnessSet.redeemers.map(_.value.toSeq).getOrElse(Seq.empty)
        val certRedeemerOpt = redeemers.find(_.tag == RedeemerTag.Cert)

        assert(
          certRedeemerOpt.isDefined,
          s"Certificate redeemer should be present. Redeemers: $redeemers"
        )
    }

    test("TxBuilder can be created with context CardanoInfo") {
        // txBuilder uses context parameter
        val builder = txBuilder
        assert(builder.env == testEnv)
    }

    test("Certificate-only transaction must have at least one input") {
        import scalus.cardano.address.{StakeAddress, StakePayload}
        import scalus.cardano.ledger.rules.{Context, UtxoEnv}

        // Bug: When a transaction only contains certificate operations with refunds
        // (like deregisterStake), the TxBuilder can produce a transaction with zero inputs.
        // This happens because certificate refunds are added to the "consumed" side of
        // the balance equation. If the refund exceeds the fee, the balance is satisfied
        // without needing any input UTxOs. However, Cardano requires every transaction
        // to have at least one input anyway.

        // This only reproduces when a provider is involved, since txbuilder cannot access the account-based
        // stake funds otherwise.

        val alice = Alice.address(Mainnet)
        val stakeAddress =
            StakeAddress(Mainnet, StakePayload.Stake(Alice.account.stakeKeyHash))
        val keyDeposit = Coin(testEnv.protocolParams.stakeAddressDeposit)

        val initialUtxos = Map(
          Input(genesisHash, 0) -> Output(alice, Value.ada(100))
        )

        val context = Context
            .testMainnet()
            .copy(
              env = UtxoEnv
                  .testMainnet()
                  .copy(
                    certState = CertState(
                      dstate = scalus.cardano.ledger.DelegationState(
                        deposits = Map(
                          stakeAddress.credential -> keyDeposit
                        )
                      )
                    )
                  )
            )

        // Emulator starts off with some ADA deposit, enough to cover a fee.
        val emulator = Emulator(
          initialUtxos,
          context
        )

        // deregister, returning the deposit
        val tx = TxBuilder(testEnv)
            .deregisterStake(stakeAddress, keyDeposit)
            .complete(emulator, alice)
            .await()
            .sign(Alice.signer)
            .transaction

        // every transaction MUST have at least one input
        assert(
          tx.body.value.inputs.toSeq.nonEmpty,
          s"Transaction must have at least one input, but has none. " +
              s"Outputs: ${tx.body.value.outputs.size}, Fee: ${tx.body.value.fee}"
        )
    }
}
