package scalus.cardano.txbuilder

import monocle.syntax.all.*
import org.scalatest.funsuite.AnyFunSuite
import scalus.builtin.{ByteString, Data}
import scalus.cardano.address.Network.Mainnet
import scalus.cardano.address.{ShelleyAddress, ShelleyDelegationPart, ShelleyPaymentPart}
import scalus.cardano.ledger.*
import scalus.cardano.ledger.DatumOption.Inline
import scalus.cardano.ledger.TransactionOutput.Babbage
import scalus.cardano.txbuilder.TestPeer.{Alice, Bob}
import scalus.{plutusV2, plutusV3, toUplc, Compiler}

import scala.collection.immutable.SortedMap

class TxBuilderTest extends AnyFunSuite {

    val testEnv: CardanoInfo = CardanoInfo.mainnet

    // Test scripts for validation - using PlutusV3
    val script1: Script.PlutusV3 = {
        val alwaysOk = Compiler.compileInline((sc: Data) => ())
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
              .replace(
                ShelleyAddress(
                  network = Mainnet,
                  payment = ShelleyPaymentPart.Script(script.scriptHash),
                  delegation = ShelleyDelegationPart.Null
                )
              )
              .focus(_.datumOption)
              .replace(Some(Inline(Data.I(42))))
        )
    }

    test("TxBuilder spend without attaching script should fail when built") {
        val scriptUtxo = createScriptLockedUtxo(script1)
        val redeemer = Data.List(List.empty)

        val builder = TxBuilder(testEnv)
            .spend(scriptUtxo, redeemer)
            .changeTo(Alice.address)

        val exception = intercept[RuntimeException] {
            builder.build()
        }

        assert(exception.getMessage.contains("No witness or ref/spent output is found for script"))
        assert(exception.getMessage.contains(scriptUtxo.output.address.scriptHashOption.get.toHex))
    }

    test("TxBuilder mint without attaching script should fail when built") {
        val policyId: PolicyId = mintingPolicy.scriptHash
        val redeemer = Data.List(List.empty)
        val assets = Map(
          AssetName(ByteString.fromHex("deadbeef")) -> 100L
        )
        val utxo = genAdaOnlyPubKeyUtxo(Alice).sample.get

        val builder = TxBuilder(testEnv)
            .spend(Utxo(utxo))
            .mint(redeemer, policyId, assets)
            .changeTo(Alice.address)

        val exception = intercept[RuntimeException] {
            builder.build()
        }

        assert(exception.getMessage.contains("No witness or ref/spent output is found for script"))
        assert(exception.getMessage.contains(policyId.toHex))
    }

    test("TxBuilder spend with script should include script in witness set") {
        val scriptUtxo = createScriptLockedUtxo(script1)
        val redeemer = Data.List(List.empty)
        val paymentUtxo = genAdaOnlyPubKeyUtxo(Alice, min = 50_000_000).sample.get
        val collateralUtxo = genAdaOnlyPubKeyUtxo(Alice, min = 5_000_000).sample.get

        val validFrom = java.time.Instant.now()
        val validTo = validFrom.plusSeconds(3600)
        val inlineDatum = Data.I(123)

        val builtTx = TxBuilder(testEnv)
            .spend(Utxo(paymentUtxo))
            .collaterals(Utxo(collateralUtxo))
            .spend(scriptUtxo, redeemer, script1)
            .payTo(Alice.address, Value.ada(1))
            .payTo(Bob.address, Value.ada(2), inlineDatum)
            .validFrom(validFrom)
            .validTo(validTo)
            .changeTo(Alice.address)
            .build()
            .transaction

        val plutusV3Scripts = builtTx.witnessSet.plutusV3Scripts.toMap.values
        assert(plutusV3Scripts.exists(_.scriptHash == script1.scriptHash))

        assert(builtTx.body.value.inputs.toSeq.contains(scriptUtxo.input))

        assert(builtTx.witnessSet.redeemers.isDefined)
        val redeemers = builtTx.witnessSet.redeemers.get.value.toSeq
        assert(redeemers.toSeq.size == 1)
        assert(redeemers.head.tag == RedeemerTag.Spend)
        assert(
          redeemers.head.index == builtTx.body.value.inputs.toSeq.indexWhere(_ == scriptUtxo.input)
        )

        assert(builtTx.body.value.collateralInputs.toSeq.contains(collateralUtxo._1))

        val bobOutput = builtTx.body.value.outputs.find(_.value.address == Bob.address)
        assert(bobOutput.isDefined)
        assert(bobOutput.get.value.datumOption.isDefined)
        bobOutput.get.value.datumOption.get match {
            case Inline(data) => assert(data == inlineDatum)
            case _            => fail("Expected inline datum")
        }

        assert(builtTx.body.value.validityStartSlot.isDefined)
        assert(
          builtTx.body.value.validityStartSlot.get == testEnv.slotConfig.timeToSlot(
            validFrom.toEpochMilli
          )
        )

        assert(builtTx.body.value.ttl.isDefined)
        assert(builtTx.body.value.ttl.get == testEnv.slotConfig.timeToSlot(validTo.toEpochMilli))
    }

    test("TxBuilder mint with script and payTo sends minted tokens to specified output") {
        val redeemer = Data.List(List.empty)
        val assetName1 = AssetName(ByteString.fromHex("deadbeef"))
        val assetName2 = AssetName(ByteString.fromHex("cafebabe"))
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

        val builtTx = TxBuilder(testEnv)
            .spend(Utxo(utxo))
            .mint(redeemer, assets, mintingPolicy)
            .payTo(Bob.address, paymentValue)
            .metadata(metadata)
            .changeTo(Alice.address)
            .build()
            .transaction

        // Check that mintingPolicy is in the plutus V3 scripts witness set
        val plutusV3Scripts = builtTx.witnessSet.plutusV3Scripts.toMap.values
        assert(plutusV3Scripts.exists(_.scriptHash == mintingPolicy.scriptHash))

        // Check that mint field is present
        assert(builtTx.body.value.mint.isDefined)
        val mint = builtTx.body.value.mint.get
        assert(mint.assets.contains(mintingPolicy.scriptHash))
        val mintedAssets = mint.assets(mintingPolicy.scriptHash)
        assert(mintedAssets.get(assetName1).contains(100L))
        assert(mintedAssets.get(assetName2).contains(50L))

        // Check that redeemers are present for minting
        assert(builtTx.witnessSet.redeemers.isDefined)
        val redeemers = builtTx.witnessSet.redeemers.get.value.toSeq
        assert(redeemers.toSeq.size == 1)
        assert(redeemers.head.tag == RedeemerTag.Mint)
        assert(redeemers.head.index == 0)

        // Check that tokens appear in Bob's output
        val bobOutputs = builtTx.body.value.outputs.filter(_.value.address == Bob.address)
        assert(bobOutputs.nonEmpty, "Should have at least one output to Bob")

        val bobOutputWithTokens = bobOutputs.find { output =>
            output.value.value.assets.assets.get(mintingPolicy.scriptHash).exists { assetsMap =>
                assetsMap.get(assetName1).contains(100L) && assetsMap.get(assetName2).contains(50L)
            }
        }
        assert(bobOutputWithTokens.isDefined, "Minted tokens should be in Bob's payment output")

        assert(bobOutputWithTokens.get.value.value.coin.value >= 2_000_000L)

        assert(builtTx.auxiliaryData.isDefined)
        val auxData = builtTx.auxiliaryData.get.value
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
          datumOption = Some(Inline(Data.B(ByteString.fromHex("deadbeef")))),
          scriptRef = None
        )

        val builtTx = TxBuilder(testEnv)
            .spend(Utxo(utxo))
            .output(customOutput)
            .changeTo(Alice.address)
            .build()
            .transaction

        val bobOutputs = builtTx.body.value.outputs.filter(_.value.address == Bob.address)
        assert(bobOutputs.size == 1)

        val bobOutput = bobOutputs.head
        assert(bobOutput.value.value.coin.value == 3_000_000L)
        assert(bobOutput.value.datumOption.isDefined)
        bobOutput.value.datumOption.get match {
            case Inline(data) => assert(data == Data.B(ByteString.fromHex("deadbeef")))
            case _            => fail("Expected inline datum")
        }
    }

    test("TxBuilder payTo with datum hash includes datum in witness set") {
        val utxo = genAdaOnlyPubKeyUtxo(Alice, min = 10_000_000).sample.get

        val datum = Data.Constr(0, List(Data.I(100), Data.B(ByteString.fromHex("abcd"))))
        val datumHash = DataHash.fromByteString(
          scalus.builtin.Builtins.blake2b_256(scalus.builtin.Builtins.serialiseData(datum))
        )

        val builtTx = TxBuilder(testEnv)
            .spend(Utxo(utxo))
            .attach(datum)
            .payTo(Bob.address, Value.ada(2), datumHash)
            .changeTo(Alice.address)
            .build()
            .transaction

        val bobOutput = builtTx.body.value.outputs.find(_.value.address == Bob.address)
        assert(bobOutput.isDefined)
        assert(bobOutput.get.value.datumOption.isDefined)
        bobOutput.get.value.datumOption.get match {
            case DatumOption.Hash(hash) => assert(hash == datumHash)
            case _                      => fail("Expected datum hash")
        }

        val plutusData = builtTx.witnessSet.plutusData.value.toMap.values.map(_.value)
        assert(plutusData.exists(_ == datum))
    }

    test("TxBuilder attach preserves multiple scripts and data in witness set") {
        val utxo = genAdaOnlyPubKeyUtxo(Alice, min = 10_000_000).sample.get

        val script2 = Script.PlutusV3(
          Compiler.compileInline((sc: Data) => ()).toUplc().plutusV3.cborByteString
        )
        val script3 = Script.PlutusV2(
          Compiler.compileInline((sc: Data) => ()).toUplc().plutusV2.cborByteString
        )

        val datum1 = Data.I(111)
        val datum2 = Data.B(ByteString.fromHex("aabbcc"))
        val datum3 = Data.Constr(1, List(Data.I(222)))

        val builtTx = TxBuilder(testEnv)
            .spend(Utxo(utxo))
            .attach(script1)
            .attach(script2)
            .attach(script3)
            .attach(datum1)
            .attach(datum2)
            .attach(datum3)
            .payTo(Bob.address, Value.ada(1))
            .changeTo(Alice.address)
            .build()
            .transaction

        val v3Scripts = builtTx.witnessSet.plutusV3Scripts.toMap.values.toSeq
        assert(v3Scripts.exists(_.scriptHash == script1.scriptHash))
        assert(v3Scripts.exists(_.scriptHash == script2.scriptHash))

        val v2Scripts = builtTx.witnessSet.plutusV2Scripts.toMap.values.toSeq
        assert(v2Scripts.exists(_.scriptHash == script3.scriptHash))

        val plutusData = builtTx.witnessSet.plutusData.value.toMap.values.map(_.value).toSeq
        assert(plutusData.contains(datum1))
        assert(plutusData.contains(datum2))
        assert(plutusData.contains(datum3))
    }

    test("TxBuilder spend script UTXO without script or redeemer should fail") {
        val scriptUtxo = createScriptLockedUtxo(script1)

        val builder = TxBuilder(testEnv)
            .spend(scriptUtxo)
            .payTo(Bob.address, Value.ada(20))
            .changeTo(Alice.address)

        val exception = intercept[RuntimeException] {
            builder.build()
        }
        succeed
    }
}
