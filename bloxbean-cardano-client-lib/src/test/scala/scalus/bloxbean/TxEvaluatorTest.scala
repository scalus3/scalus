package scalus.bloxbean

import com.bloxbean.cardano.client.account.Account
import com.bloxbean.cardano.client.address.AddressProvider
import com.bloxbean.cardano.client.api.util.CostModelUtil
import com.bloxbean.cardano.client.common.ADAConversionUtil
import com.bloxbean.cardano.client.common.model.Networks
import com.bloxbean.cardano.client.plutus.spec.*
import com.bloxbean.cardano.client.transaction.spec.*
import org.scalatest.funsuite.AnyFunSuite
import scalus.*
import scalus.builtin.ByteString.*
import scalus.cardano.ledger.SlotConfig
import scalus.compiler.compile
import scalus.examples.PubKeyValidator
import scalus.uplc.eval.ExBudget
import scalus.utils.Hex.hexToBytes

import java.math.BigInteger
import java.util

class TxEvaluatorTest extends AnyFunSuite:
    val senderMnemonic: String =
        "drive useless envelope shine range ability time copper alarm museum near flee wrist live type device meadow allow churn purity wisdom praise drop code"
    val sender1 = Account.createFromMnemonic(Networks.testnet(), senderMnemonic)
    val sender1Addr: String = sender1.baseAddress()

    test("TxEvaluator PlutusV2") {
        val costMdls = CostMdls()
        costMdls.add(CostModelUtil.PlutusV1CostModel)
        costMdls.add(CostModelUtil.PlutusV2CostModel)
        val evaluator = TxEvaluator(
          SlotConfig.mainnet,
          initialBudget = ExBudget.fromCpuAndMemory(10_000000000L, 10_000000L),
          protocolMajorVersion = 8,
          costMdls = costMdls
        )
        val pubKeyValidator = compile(PubKeyValidator.validatorV2(hex"deadbeef")).toUplc().plutusV2
        val s: PlutusV2Script =
            PlutusV2Script
                .builder()
                .cborHex(pubKeyValidator.doubleCborHex)
                .build()
                .asInstanceOf[PlutusV2Script]
        val pubKeyScriptAddress = AddressProvider.getEntAddress(s, Networks.testnet())
        val input = TransactionInput.builder().transactionId("deadbeef").index(0).build()
        val inputs = util.List.of(input)

        val utxo = Map(
          input -> TransactionOutput
              .builder()
              .value(Value.builder().coin(BigInteger.valueOf(20)).build())
              .address(pubKeyScriptAddress.getAddress)
              .datumHash(PlutusData.unit().getDatumHash.hexToBytes)
              .build()
        )
        val redeemer = Redeemer
            .builder()
            .tag(RedeemerTag.Spend)
            .data(PlutusData.unit())
            .index(0)
            .exUnits(
              ExUnits
                  .builder()
                  .mem(BigInteger.valueOf(10_000_000L))
                  .steps(BigInteger.valueOf(10_000_000_000L))
                  .build()
            )
            .build()
        val tx = Transaction
            .builder()
            .body(
              TransactionBody
                  .builder()
                  .fee(ADAConversionUtil.adaToLovelace(0.2))
                  .ttl(1000)
                  .inputs(inputs)
                  .requiredSigners(util.List.of(hex"deadbeef".bytes))
                  .build()
            )
            .witnessSet(
              TransactionWitnessSet
                  .builder()
                  .plutusV2Scripts(util.List.of(s))
                  .redeemers(util.List.of(redeemer))
                  .plutusDataList(util.List.of(PlutusData.unit()))
                  .build()
            )
            .build()
        val redeemers = evaluator.evaluateTx(tx, utxo)
        assert(redeemers.size == 1)
        val redeemerResult = redeemers.head
        assert(redeemerResult.getExUnits.getMem.longValue == 13375L)
        assert(redeemerResult.getExUnits.getSteps.longValue == 3732650L)
    }

    test("TxEvaluator PlutusV3") {
        val costMdls = CostMdls()
        costMdls.add(CostModelUtil.PlutusV1CostModel)
        costMdls.add(CostModelUtil.PlutusV2CostModel)
        costMdls.add(CostModelUtil.PlutusV3CostModel)
        val evaluator = TxEvaluator(
          SlotConfig.mainnet,
          initialBudget = ExBudget.fromCpuAndMemory(10_000000000L, 10_000000L),
          protocolMajorVersion = 9,
          costMdls = costMdls
        )
        val pubKeyValidator =
            compile(PubKeyValidator.validatorV3(hex"deadbeef")).toUplc().plutusV3
        val s: PlutusV3Script =
            PlutusV3Script
                .builder()
                .cborHex(pubKeyValidator.doubleCborHex)
                .build()
                .asInstanceOf[PlutusV3Script]
        val pubKeyScriptAddress = AddressProvider.getEntAddress(s, Networks.testnet())
        val input = TransactionInput.builder().transactionId("deadbeef").index(0).build()
        val inputs = util.List.of(input)

        val utxo = Map(
          input -> TransactionOutput
              .builder()
              .value(Value.builder().coin(BigInteger.valueOf(20)).build())
              .address(pubKeyScriptAddress.getAddress)
              .datumHash(PlutusData.unit().getDatumHash.hexToBytes)
              .build()
        )
        val redeemer = Redeemer
            .builder()
            .tag(RedeemerTag.Spend)
            .data(PlutusData.unit())
            .index(0)
            .exUnits(
              ExUnits
                  .builder()
                  .mem(BigInteger.valueOf(10_000_000L))
                  .steps(BigInteger.valueOf(10_000_000_000L))
                  .build()
            )
            .build()
        val tx = Transaction
            .builder()
            .body(
              TransactionBody
                  .builder()
                  .fee(ADAConversionUtil.adaToLovelace(0.2))
                  .ttl(1000)
                  .inputs(inputs)
                  .requiredSigners(util.List.of(hex"deadbeef".bytes))
                  .build()
            )
            .witnessSet(
              TransactionWitnessSet
                  .builder()
                  .plutusV3Scripts(util.List.of(s))
                  .redeemers(util.List.of(redeemer))
                  .plutusDataList(util.List.of(PlutusData.unit()))
                  .build()
            )
            .build()
        val redeemers = evaluator.evaluateTx(tx, utxo)
        assert(redeemers.size == 1)
        val redeemerResult = redeemers.head
        assert(redeemerResult.getExUnits.getMem.longValue == 12775L)
        assert(redeemerResult.getExUnits.getSteps.longValue == 3636650L)
    }
