package scalus.cardano.ledger

import org.scalatest.funsuite.AnyFunSuite
import scalus.cardano.address.Network

import scala.scalajs.js

class JsProtocolParamsTest extends AnyFunSuite {

    test("preview info carries preview slot config and testnet network") {
        val info = JsCardanoInfo.preview()
        assert(info.network == "testnet")
        assert(info.slotConfig.zeroSlot == 0.0)
        assert(info.protocolParams.maxTxSize > 0.0)
    }

    test("mainnet parameters round-trip through Blockfrost JSON") {
        val params = JsCardanoInfo.mainnet().protocolParams
        val back = JsProtocolParams.fromBlockfrostJson(params.toBlockfrostJson())
        assert(back.txFeePerByte == params.txFeePerByte)
        assert(back.costModels.PlutusV3.length == params.costModels.PlutusV3.length)
    }

    test("custom info accepts a Yaci-style slot config") {
        val slotConfig = new JsSlotConfig(1_700_000_000_000d, 0d, 1000d, 500d, 0d)
        val info = JsCardanoInfo.custom(
          "testnet",
          slotConfig,
          JsCardanoInfo.preview().protocolParams
        )
        assert(info.slotConfig.epochLength == 500.0)
        assert(info.network == "testnet")
    }

    test("preprod info carries preprod slot config and testnet network") {
        val info = JsCardanoInfo.preprod()
        assert(info.network == "testnet")
        assert(info.slotConfig.zeroEpoch == 4.0)
    }

    test("mainnet info renders network as mainnet") {
        assert(JsCardanoInfo.mainnet().network == "mainnet")
    }

    test("custom rejects a network string that is neither mainnet nor testnet") {
        assertThrows[IllegalArgumentException] {
            JsCardanoInfo.custom(
              "devnet",
              JsSlotConfig.preview,
              JsCardanoInfo.preview().protocolParams
            )
        }
    }

    test(
      "network throws for a CardanoInfo holding Network.Other, rather than inventing a third string"
    ) {
        val other = CardanoInfo(
          JsCardanoInfo.preview().protocolParams.underlying,
          Network.Other(7),
          SlotConfig.preview
        )
        val handle = JsCardanoInfo.wrap(other)
        assertThrows[IllegalStateException](handle.network)
    }

    test(
      "withProtocolParams returns a new handle with only the params replaced, leaving the original untouched"
    ) {
        val info = JsCardanoInfo.preview()
        val originalFee = info.protocolParams.txFeePerByte
        val mainnetParams = JsCardanoInfo.mainnet().protocolParams
        val updated = info.withProtocolParams(mainnetParams)
        assert(updated.protocolParams.txFeePerByte == mainnetParams.txFeePerByte)
        assert(updated.network == info.network)
        assert(updated.slotConfig.zeroSlot == info.slotConfig.zeroSlot)
        assert(info.protocolParams.txFeePerByte == originalFee)
    }

    test(
      "the Double/BigInt split matches spec 4.4: deposits and unit costs are bigint, sizes and rates are double"
    ) {
        val params = JsCardanoInfo.mainnet().protocolParams
        assert(js.typeOf(params.txFeePerByte) == "number")
        assert(js.typeOf(params.txFeeFixed) == "number")
        assert(js.typeOf(params.maxTxSize) == "number")
        assert(js.typeOf(params.maxValueSize) == "number")
        assert(js.typeOf(params.collateralPercentage) == "number")
        assert(js.typeOf(params.maxCollateralInputs) == "number")
        assert(js.typeOf(params.minFeeRefScriptCostPerByte) == "number")
        assert(js.typeOf(params.protocolMajorVersion) == "number")
        assert(js.typeOf(params.priceMemory) == "number")
        assert(js.typeOf(params.priceSteps) == "number")
        assert(js.typeOf(params.stakeAddressDeposit) == "bigint")
        assert(js.typeOf(params.stakePoolDeposit) == "bigint")
        assert(js.typeOf(params.dRepDeposit) == "bigint")
        assert(js.typeOf(params.govActionDeposit) == "bigint")
        assert(js.typeOf(params.utxoCostPerByte) == "bigint")
        assert(js.typeOf(params.maxTxExecutionMemory) == "bigint")
        assert(js.typeOf(params.maxTxExecutionSteps) == "bigint")
    }

    test("a ProtocolParams handle exposes no own enumerable properties, but toObject does") {
        val params = JsCardanoInfo.mainnet().protocolParams
        assert(js.Object.keys(params).length == 0, "a handle exposes nothing to spread or toEqual")
        val plain = params.toObject()
        assert(
          js.Object.keys(plain).toSet == Set(
            "txFeePerByte",
            "txFeeFixed",
            "maxTxSize",
            "maxValueSize",
            "stakeAddressDeposit",
            "stakePoolDeposit",
            "dRepDeposit",
            "govActionDeposit",
            "utxoCostPerByte",
            "priceMemory",
            "priceSteps",
            "maxTxExecutionMemory",
            "maxTxExecutionSteps",
            "collateralPercentage",
            "maxCollateralInputs",
            "minFeeRefScriptCostPerByte",
            "protocolMajorVersion",
            "costModels"
          )
        )
        assert(js.Object.keys(plain.costModels).toSet == Set("PlutusV1", "PlutusV2", "PlutusV3"))
        assert(plain.txFeePerByte == params.txFeePerByte)
    }

    test("a CardanoInfo handle exposes no own enumerable properties, but toObject does") {
        val info = JsCardanoInfo.preview()
        assert(js.Object.keys(info).length == 0, "a handle exposes nothing to spread or toEqual")
        val plain = info.toObject()
        assert(js.Object.keys(plain).toSet == Set("network", "slotConfig", "protocolParams"))
        assert(plain.network == "testnet")
        assert(plain.protocolParams.txFeePerByte == info.protocolParams.txFeePerByte)
    }
}
