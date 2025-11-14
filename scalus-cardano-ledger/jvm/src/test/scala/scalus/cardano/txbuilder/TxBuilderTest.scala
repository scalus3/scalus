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
import scalus.{plutusV3, toUplc, Compiler}

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
        val paymentUtxo = genAdaOnlyPubKeyUtxo(Alice).sample.get

        val builtTx = TxBuilder(testEnv)
            .spend(Utxo(paymentUtxo))
            .spend(scriptUtxo, redeemer, script1)
            .payTo(Alice.address, Value(Coin(1_000_000L)))
            .changeTo(Alice.address)
            .build()
            .transaction

        // Check that script1 is in the plutus V3 scripts witness set
        val plutusV3Scripts = builtTx.witnessSet.plutusV3Scripts.toMap.values
        assert(plutusV3Scripts.exists(_.scriptHash == script1.scriptHash))

        // Check that the script UTxO is in the inputs
        assert(builtTx.body.value.inputs.toSeq.contains(scriptUtxo.input))

        // Check that redeemers are present
        assert(builtTx.witnessSet.redeemers.isDefined)
        val redeemers = builtTx.witnessSet.redeemers.get.value.toSeq
        assert(redeemers.toSeq.size == 1)
        assert(redeemers.head.tag == RedeemerTag.Spend)
        assert(
          redeemers.head.index == builtTx.body.value.inputs.toSeq.indexWhere(_ == scriptUtxo.input)
        )
    }

    ignore("TxBuilder mint with script sends minted tokens to change output") {
        val redeemer = Data.List(List.empty)
        val assetName1 = AssetName(ByteString.fromHex("deadbeef"))
        val assetName2 = AssetName(ByteString.fromHex("cafebabe"))
        val assets = Map(
          assetName1 -> 100L,
          assetName2 -> 50L
        )
        val utxo = genAdaOnlyPubKeyUtxo(Alice, min = 1_000_000).sample.get

        val builtTx = TxBuilder(testEnv)
            .spend(Utxo(utxo))
            .mint(redeemer, assets, mintingPolicy)
            .changeTo(Alice.address)
            .build()
            .transaction

        // Check that mintingPolicy is in the plutus V3 scripts witness set
        val plutusV3Scripts = builtTx.witnessSet.plutusV3Scripts.toMap.values
        assert(plutusV3Scripts.exists(_.scriptHash == mintingPolicy.scriptHash))

        // Check that mint field is present in transaction body
        assert(builtTx.body.value.mint.isDefined)
        val mint = builtTx.body.value.mint.get

        // Check that the policy ID is in the mint map
        assert(mint.assets.contains(mintingPolicy.scriptHash))
        val mintedAssets = mint.assets(mintingPolicy.scriptHash)

        // Check that both assets are minted with correct amounts
        assert(mintedAssets.get(assetName1).contains(100L))
        assert(mintedAssets.get(assetName2).contains(50L))

        // Check that redeemers are present for minting
        assert(builtTx.witnessSet.redeemers.isDefined)
        val redeemers = builtTx.witnessSet.redeemers.get.value.toSeq
        assert(redeemers.toSeq.size == 1)
        assert(redeemers.head.tag == RedeemerTag.Mint)
        // The index should be 0 since there's only one policy ID in the mint
        assert(redeemers.head.index == 0)

        // Check that tokens appear in the change output (to Alice's address)
        val aliceOutputs = builtTx.body.value.outputs.filter(_.value.address == Alice.address)
        assert(aliceOutputs.nonEmpty, "Should have at least one output to Alice")

        // Find output with the minted tokens
        val outputWithTokens = aliceOutputs.find { output =>
            output.value.value.assets.assets.get(mintingPolicy.scriptHash).exists { assetsMap =>
                assetsMap.get(assetName1).contains(100L) && assetsMap.get(assetName2).contains(50L)
            }
        }
        assert(outputWithTokens.isDefined, "Minted tokens should be in Alice's change output")
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

        // Build payment value with minted tokens
        val paymentValue = Value(
          coin = Coin(2_000_000L),
          assets = MultiAsset(
            SortedMap(
              mintingPolicy.scriptHash -> SortedMap(
                assetName1 -> 100L,
                assetName2 -> 50L
              )
            )
          )
        )

        val builtTx = TxBuilder(testEnv)
            .spend(Utxo(utxo))
            .mint(redeemer, assets, mintingPolicy)
            .payTo(Bob.address, paymentValue)
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

        // Verify Bob's output has the specified ADA amount
        assert(bobOutputWithTokens.get.value.value.coin.value >= 2_000_000L)
    }
}
