package scalus.testing.conformance

import scalus.cardano.ledger.ProtocolParams

/** Conformance tests for protocol parameters
  *
  * Validates that protocol parameters from different sources can be correctly loaded
  * and used with Scalus ledger validation.
  */
class ProtocolParamsConformanceTest extends ConformanceTestBase:

    test("load protocol parameters from Blockfrost format") {
        val paramsPath = "/blockfrost-params-epoch-543.json"
        val inputStream = getClass.getResourceAsStream(paramsPath)
        inputStream should not be null

        val params = ProtocolParams.fromBlockfrostJson(inputStream)

        // Verify key parameters are loaded
        params.txFeePerByte should be > 0L
        params.txFeeFixed should be > 0L
        params.maxBlockBodySize should be > 0L
        params.maxTxSize should be > 0L

        println(s"Loaded protocol parameters:")
        println(s"  txFeePerByte: ${params.txFeePerByte}")
        println(s"  txFeeFixed: ${params.txFeeFixed}")
        println(s"  maxBlockBodySize: ${params.maxBlockBodySize}")
        println(s"  maxTxSize: ${params.maxTxSize}")
        println(s"  minPoolCost: ${params.minPoolCost}")
    }

    test("load protocol parameters from multiple epochs") {
        val epochs = List(543, 544)

        epochs.foreach { epoch =>
            val paramsPath = s"/blockfrost-params-epoch-$epoch.json"
            val inputStream = getClass.getResourceAsStream(paramsPath)
            if inputStream != null then
                val params = ProtocolParams.fromBlockfrostJson(inputStream)
                println(s"Epoch $epoch parameters loaded successfully")
                params should not be null
            else
                println(s"Skipping epoch $epoch - no data file")
        }
    }

    test("compare protocol parameters across epochs") {
        val params543Stream = getClass.getResourceAsStream("/blockfrost-params-epoch-543.json")
        val params544Stream = getClass.getResourceAsStream("/blockfrost-params-epoch-544.json")

        if params543Stream != null && params544Stream != null then
            val params543 = ProtocolParams.fromBlockfrostJson(params543Stream)
            val params544 = ProtocolParams.fromBlockfrostJson(params544Stream)

            println(s"Comparing parameters between epochs 543 and 544:")
            println(s"  txFeePerByte: ${params543.txFeePerByte} -> ${params544.txFeePerByte}")
            println(s"  txFeeFixed: ${params543.txFeeFixed} -> ${params544.txFeeFixed}")
            println(s"  minPoolCost: ${params543.minPoolCost} -> ${params544.minPoolCost}")

            // Both should be valid
            params543 should not be null
            params544 should not be null
    }

    test("verify protocol parameters have required fields for ledger rules") {
        val paramsPath = "/blockfrost-params-epoch-544.json"
        val inputStream = getClass.getResourceAsStream(paramsPath)
        val params = ProtocolParams.fromBlockfrostJson(inputStream)

        // Verify all fields needed for transaction validation are present
        params.txFeePerByte should be > 0L
        params.txFeeFixed should be > 0L
        params.maxBlockBodySize should be > 0L
        params.maxTxSize should be > 0L
        params.maxBlockHeaderSize should be > 0L
        params.stakeAddressDeposit should be >= 0L
        params.stakePoolDeposit should be >= 0L
        params.minPoolCost should be >= 0L
        params.poolRetireMaxEpoch should be > 0L
        params.stakePoolTargetNum should be > 0L
        params.collateralPercentage should be > 0L
        params.maxCollateralInputs should be > 0L
        params.utxoCostPerByte should be > 0L

        println(s"All required protocol parameter fields are present and valid")
    }
