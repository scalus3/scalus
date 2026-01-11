package scalus.cardano.txbuilder

import org.scalatest.funsuite.AnyFunSuite
import scalus.builtin.Data.toData
import scalus.builtin.{ByteString, Data}
import scalus.cardano.address.{Address, ShelleyAddress, ShelleyDelegationPart, ShelleyPaymentPart}
import scalus.cardano.ledger.*
import scalus.cardano.ledger.DatumOption.Inline
import scalus.cardano.ledger.rules.ValidatorRulesTestKit
import scalus.cardano.ledger.utils.MinTransactionFee
import scalus.cardano.node.Emulator
import scalus.compiler.compileInline
import scalus.prelude.List as PList
import scalus.testing.kit.Party
import scalus.testing.kit.Party.{Alice, Bob}
import scalus.utils.await
import scalus.toUplc

import java.time.Instant
import scala.collection.immutable.SortedMap
import scala.concurrent.ExecutionContext.Implicits.global

class TxBuilderCompleteTest extends AnyFunSuite, ValidatorRulesTestKit {

    given testEnv: CardanoInfo = CardanoInfo.mainnet

    // Common test values
    val genesisHash: TransactionHash =
        TransactionHash.fromByteString(ByteString.fromHex("0" * 64))

    val alwaysOkScript: Script.PlutusV3 = {
        val alwaysOk = compileInline((sc: Data) => ())
        val alwaysOkCborBytes = alwaysOk.toUplc().plutusV3.cborByteString
        Script.PlutusV3(alwaysOkCborBytes)
    }

    val policyId: ScriptHash = alwaysOkScript.scriptHash
    val policyId2: ScriptHash = ScriptHash.fromByteString(ByteString.fromHex("1" * 56))
    val policyId3: ScriptHash = ScriptHash.fromByteString(ByteString.fromHex("2" * 56))

    val co2: AssetName = AssetName.fromString("co2")
    val h2so4: AssetName = AssetName.fromString("h2so4")
    val c11h15no2: AssetName = AssetName.fromString("c11h15no2")
    val token: AssetName = AssetName.fromString("token")

    val emptyRedeemer: Data = Data.List(PList.Nil)
    val inlineDatum42: DatumOption = Inline(Data.I(42))

    val scriptAddress: ShelleyAddress = ShelleyAddress(
      network = testEnv.network,
      payment = ShelleyPaymentPart.Script(alwaysOkScript.scriptHash),
      delegation = ShelleyDelegationPart.Null
    )

    // Helper methods for creating UTXOs
    def input(index: Int): TransactionInput = Input(genesisHash, index)

    def adaOutput(address: Address, ada: Int): TransactionOutput =
        TransactionOutput(address, Value.ada(ada))

    def tokenOutput(
        address: Address,
        ada: Int,
        tokens: (ScriptHash, Map[AssetName, Long])*
    ): TransactionOutput =
        TransactionOutput(address, Value.assets(tokens.toMap, Coin.ada(ada)))

    def scriptOutput(ada: Int, datum: DatumOption = inlineDatum42): TransactionOutput =
        TransactionOutput(scriptAddress, Value.ada(ada), Some(datum))

    def scriptUtxo(index: Int, ada: Int, datum: DatumOption = inlineDatum42): Utxo =
        Utxo(input(index), scriptOutput(ada, datum))

    private def outputsOf(party: Party, tx: Transaction) =
        tx.body.value.outputs.toSeq.filter(_.value.address == party.address)

    // ============================================================================
    // Basic ADA Transactions
    // ============================================================================

    test("complete should automatically add inputs for simple ADA payment") {
        val provider = Emulator(
          Map(
            input(0) -> adaOutput(Alice.address, 100),
            input(1) -> adaOutput(Alice.address, 50)
          )
        )

        val tx = TxBuilder(testEnv)
            .payTo(Bob.address, Value.ada(10))
            .complete(provider, Alice.address)
            .await()
            .transaction

        assert(tx.body.value.inputs.toSeq.size == 1, "Transaction must have exactly 1 input")
        assert(outputsOf(Bob, tx).size == 1, "Should have output to Bob")
        assert(
          outputsOf(Bob, tx).head.value.value.coin >= Coin.ada(10),
          "Bob should receive at least 10 ADA"
        )
        assert(outputsOf(Alice, tx).nonEmpty, "Should have change output to Alice")
    }

    test("complete inputs should be signable with sign()") {
        val provider = Emulator(
          Map(input(0) -> adaOutput(Alice.address, 100))
        )

        val signedTx = TxBuilder(testEnv)
            .payTo(Bob.address, Value.ada(10))
            .complete(provider, Alice.address)
            .await()
            .sign(Alice.signer)
            .transaction

        assert(
          signedTx.witnessSet.vkeyWitnesses.toSeq.nonEmpty,
          "Transaction should have vkey witnesses after signing"
        )
    }

    test("complete followed by sign() should produce a valid transaction") {
        val provider = Emulator(
          Map(
            input(0) -> adaOutput(Alice.address, 50),
            input(1) -> adaOutput(Alice.address, 50)
          )
        )

        val signedTx = TxBuilder(testEnv)
            .payTo(Bob.address, Value.ada(20))
            .complete(provider, Alice.address)
            .await()
            .sign(Alice.signer)
            .transaction

        assert(signedTx.witnessSet.vkeyWitnesses.toSeq.nonEmpty, "Should have vkey witnesses")

        val aliceInputs = signedTx.body.value.inputs.toSeq.filter { i =>
            provider.findUtxo(i).await().toOption.exists(_.output.address == Alice.address)
        }
        assert(aliceInputs.nonEmpty, "Should have inputs from Alice that were added by complete")
    }

    // ============================================================================
    // Multi-Asset Transactions
    // ============================================================================

    test("complete should handle multi-asset transactions") {
        val provider = Emulator(
          Map(
            input(0) -> tokenOutput(Alice.address, 100, policyId -> Map(co2 -> 200L)),
            input(1) -> adaOutput(Alice.address, 50)
          )
        )

        val tx = TxBuilder(testEnv)
            .payTo(Bob.address, Value.fromPolicy(policyId, Map(co2 -> 100L), Coin.ada(5)))
            .complete(provider, Alice.address)
            .await()
            .transaction

        val bobTokens = outputsOf(Bob, tx).head.value.value.assets.assets
            .get(policyId)
            .flatMap(_.get(co2))
        assert(bobTokens.contains(100L), "Bob should receive 100 tokens")

        val aliceTokens = outputsOf(Alice, tx).flatMap(
          _.value.value.assets.assets.get(policyId).flatMap(_.get(co2))
        )
        assert(aliceTokens.sum == 100L, "Alice should receive 100 tokens as change")
    }

    test("complete should handle multiple different tokens from separate UTXOs") {
        val provider = Emulator(
          Map(
            input(0) -> tokenOutput(Alice.address, 50, policyId -> Map(co2 -> 100L)),
            input(1) -> tokenOutput(Alice.address, 50, policyId2 -> Map(h2so4 -> 200L)),
            input(2) -> adaOutput(Alice.address, 10)
          )
        )

        val paymentValue = Value.assets(
          Map(policyId -> Map(co2 -> 30L), policyId2 -> Map(h2so4 -> 50L)),
          Coin.ada(5)
        )

        val tx = TxBuilder(testEnv)
            .payTo(Bob.address, paymentValue)
            .complete(provider, Alice.address)
            .await()
            .transaction

        assert(tx.body.value.inputs.toSeq.size >= 2, "Should select at least 2 inputs")

        val bobValue = outputsOf(Bob, tx).head.value.value
        assert(bobValue.assets.assets.get(policyId).flatMap(_.get(co2)).contains(30L))
        assert(bobValue.assets.assets.get(policyId2).flatMap(_.get(h2so4)).contains(50L))

        val aliceTokens = outputsOf(Alice, tx).flatMap(_.value.value.assets.assets.toSeq).toMap
        assert(aliceTokens.get(policyId).flatMap(_.get(co2)).contains(70L))
        assert(aliceTokens.get(policyId2).flatMap(_.get(h2so4)).contains(150L))
    }

    test("complete should add ADA-only UTXO when token UTXO has insufficient ADA for fees") {
        val provider = Emulator(
          Map(
            input(0) -> tokenOutput(Alice.address, 1, policyId -> Map(co2 -> 100L)),
            input(1) -> adaOutput(Alice.address, 50)
          )
        )

        val tx = TxBuilder(testEnv)
            .payTo(Bob.address, Value.fromPolicy(policyId, Map(co2 -> 50L)))
            .complete(provider, Alice.address)
            .await()
            .transaction

        assert(tx.body.value.inputs.toSeq.size == 2, "Should select both UTXOs")

        val bobOut = outputsOf(Bob, tx).head.value.value
        assert(
          bobOut.assets.assets.get(policyId).flatMap(_.get(co2)).contains(50L),
          "Bob should receive 50 co2"
        )
        assert(!bobOut.coin.isZero, "Bob must get at least min ADA coins")

        val aliceTokens = outputsOf(Alice, tx).flatMap(
          _.value.value.assets.assets.get(policyId).flatMap(_.get(co2))
        )
        assert(aliceTokens.sum == 50L, "Alice should receive 50 tokens as change")
    }

    test("complete should handle multiple token types in single UTXO efficiently") {
        val provider = Emulator(
          Map(
            input(0) -> tokenOutput(
              Alice.address,
              100,
              policyId -> Map(co2 -> 100L, h2so4 -> 200L, c11h15no2 -> 50L)
            ),
            input(1) -> adaOutput(Alice.address, 10)
          )
        )

        val tx = TxBuilder(testEnv)
            .payTo(
              Bob.address,
              Value.fromPolicy(policyId, Map(co2 -> 30L, h2so4 -> 100L), Coin.ada(5))
            )
            .complete(provider, Alice.address)
            .await()
            .transaction

        val bobAssets = outputsOf(Bob, tx).head.value.value.assets.assets.head._2
        assert(bobAssets.get(co2).contains(30L))
        assert(bobAssets.get(h2so4).contains(100L))
        assert(!bobAssets.contains(c11h15no2))

        val aliceAssets = outputsOf(Alice, tx)
            .flatMap(_.value.value.assets.assets.get(policyId))
            .foldLeft(SortedMap.empty[AssetName, Long]) { case (acc, assets) =>
                assets.foldLeft(acc) { case (a, (name, amount)) =>
                    a + (name -> (a.getOrElse(name, 0L) + amount))
                }
            }
        assert(aliceAssets.get(co2).contains(70L))
        assert(aliceAssets.get(h2so4).contains(100L))
        assert(aliceAssets.get(c11h15no2).contains(50L))
    }

    test("complete should send tokens acquired from additional input querying back as change") {
        val explicitUtxo = genAdaOnlyPubKeyUtxo(Alice, min = 5_000_000).sample.get

        val provider = Emulator(
          Map(
            input(0) -> tokenOutput(Alice.address, 100, policyId -> Map(co2 -> 200L)),
            explicitUtxo._1 -> explicitUtxo._2
          )
        )

        val tx = TxBuilder(testEnv)
            .spend(Utxo(explicitUtxo))
            .payTo(Bob.address, explicitUtxo._2.value)
            .complete(provider, Alice.address)
            .await()
            .transaction

        val bobOutValue = outputsOf(Bob, tx).head.value
        assert(bobOutValue.value.coin >= Coin.ada(5))
        assert(bobOutValue.value.assets.isEmpty, "Bob must not receive any tokens")

        val aliceChangeValue = outputsOf(Alice, tx).head.value.value
        assert(aliceChangeValue.coin > Coin.ada(99) && aliceChangeValue.coin < Coin.ada(100))

        val aliceChangeTokens = aliceChangeValue.assets.assets.head._2
        assert(aliceChangeTokens(co2) == 200L, "Alice must receive all co2 back as change")
    }

    // ============================================================================
    // Script Spending & Collateral
    // ============================================================================

    test("complete should auto-detect and add collateral for script spending") {
        val sUtxo = scriptUtxo(2, 20)

        val provider = Emulator(
          Map(
            input(0) -> adaOutput(Alice.address, 100),
            input(1) -> adaOutput(Alice.address, 10),
            sUtxo.input -> sUtxo.output
          )
        )

        val tx = TxBuilder(testEnv)
            .spend(sUtxo, emptyRedeemer, alwaysOkScript)
            .payTo(Bob.address, Value.ada(5))
            .complete(provider, Alice.address)
            .await()
            .transaction

        assert(tx.body.value.collateralInputs.toSeq.nonEmpty, "Should have collateral inputs")
        assert(outputsOf(Bob, tx).nonEmpty, "Should have output to Bob")
    }

    test("complete should auto-detect and add collateral for minting") {
        val assets = Map(co2 -> 100L)

        val provider = Emulator(
          Map(
            input(0) -> adaOutput(Alice.address, 100),
            input(1) -> adaOutput(Alice.address, 10)
          )
        )

        val tx = TxBuilder(testEnv)
            .mint(alwaysOkScript, assets, emptyRedeemer)
            .payTo(Bob.address, Value.fromPolicy(policyId, assets, Coin.ada(5)))
            .complete(provider, Alice.address)
            .await()
            .transaction

        assert(tx.body.value.collateralInputs.toSeq.nonEmpty, "Should have collateral inputs")
        assert(tx.body.value.mint.isDefined, "Transaction should have mint field")

        val bobTokens = outputsOf(Bob, tx).head.value.value.assets.assets.head._2
        assert(bobTokens.head._1 == co2 && bobTokens.head._2 == 100L)
    }

    test("complete should not add collateral for pubkey-only transactions") {
        val provider = Emulator(
          Map(input(0) -> adaOutput(Alice.address, 100))
        )

        val tx = TxBuilder(testEnv)
            .payTo(Bob.address, Value.ada(10))
            .complete(provider, Alice.address)
            .await()
            .transaction

        assert(tx.body.value.collateralInputs.toSeq.isEmpty, "Should not have collateral inputs")
        assert(tx.body.value.collateralReturnOutput.isEmpty, "Should not have collateral return")
        assert(tx.body.value.totalCollateral.isEmpty, "Should not have totalCollateral")
    }

    test("complete should set collateral return output for script transactions") {
        val sUtxo = scriptUtxo(2, 20)

        val provider = Emulator(
          Map(
            input(0) -> adaOutput(Alice.address, 100),
            sUtxo.input -> sUtxo.output
          )
        )

        val tx = TxBuilder(testEnv)
            .spend(sUtxo, emptyRedeemer, alwaysOkScript)
            .payTo(Bob.address, Value.ada(5))
            .complete(provider, Alice.address)
            .await()
            .transaction

        assert(
          tx.body.value.collateralReturnOutput.isDefined,
          "Script transaction should have collateral return output"
        )
        assert(
          tx.body.value.collateralReturnOutput.get.value.address == Alice.address,
          "Collateral return should go to sponsor address"
        )
    }

    test("complete should set totalCollateral field for script transactions") {
        val sUtxo = scriptUtxo(2, 20)

        val provider = Emulator(
          Map(
            input(0) -> adaOutput(Alice.address, 100),
            sUtxo.input -> sUtxo.output
          )
        )

        val tx = TxBuilder(testEnv)
            .spend(sUtxo, emptyRedeemer, alwaysOkScript)
            .payTo(Bob.address, Value.ada(5))
            .complete(provider, Alice.address)
            .await()
            .transaction

        assert(tx.body.value.totalCollateral.isDefined, "Should have totalCollateral set")

        val totalCollateral = tx.body.value.totalCollateral.get
        val fee = tx.body.value.fee
        val requiredCollateral = (fee.value * testEnv.protocolParams.collateralPercentage) / 100

        assert(
          totalCollateral.value >= requiredCollateral,
          s"totalCollateral ($totalCollateral) should be >= required ($requiredCollateral)"
        )
    }

    test("complete should calculate collateral based on fee estimate") {
        val sUtxo = scriptUtxo(2, 20)

        val provider = Emulator(
          Map(
            input(0) -> adaOutput(Alice.address, 100),
            sUtxo.input -> sUtxo.output
          )
        )

        val tx = TxBuilder(testEnv)
            .spend(sUtxo, emptyRedeemer, alwaysOkScript)
            .payTo(Bob.address, Value.ada(5))
            .complete(provider, Alice.address)
            .await()
            .transaction

        val fee = tx.body.value.fee
        val minRequiredCollateral = (fee.value * testEnv.protocolParams.collateralPercentage) / 100

        val collateralValue = tx.body.value.collateralInputs.toSeq
            .flatMap(i => provider.findUtxo(i).await().toOption.map(_.output.value.coin.value))
            .sum

        assert(
          collateralValue >= minRequiredCollateral,
          s"Collateral ($collateralValue) should be >= required ($minRequiredCollateral)"
        )
    }

    test("complete should handle mixed script and pubkey inputs") {
        val sUtxo = scriptUtxo(2, 10)

        val provider = Emulator(
          Map(
            input(0) -> adaOutput(Alice.address, 100),
            input(1) -> adaOutput(Alice.address, 50),
            sUtxo.input -> sUtxo.output
          )
        )

        val tx = TxBuilder(testEnv)
            .spend(sUtxo, emptyRedeemer, alwaysOkScript)
            .payTo(Bob.address, Value.ada(5))
            .complete(provider, Alice.address)
            .await()
            .transaction

        assert(tx.body.value.inputs.toSeq.contains(sUtxo.input))
        assert(tx.body.value.collateralInputs.toSeq.nonEmpty)
    }

    // ============================================================================
    // Minting Scenarios
    // ============================================================================

    test("complete should select inputs for fees when only minting (no explicit inputs)") {
        val provider = Emulator(
          Map(
            input(0) -> adaOutput(Alice.address, 100),
            input(1) -> adaOutput(Alice.address, 50)
          )
        )

        val tx = TxBuilder(testEnv)
            .mint(alwaysOkScript, Map(token -> 100L), emptyRedeemer)
            .payTo(Bob.address, Value.fromPolicy(policyId, Map(token -> 100L), Coin.ada(5)))
            .complete(provider, Alice.address)
            .await()
            .transaction

        assert(tx.body.value.inputs.toSeq.nonEmpty, "Should select inputs for fees")
        assert(tx.body.value.mint.isDefined)
        assert(tx.body.value.mint.get.assets.get(policyId).flatMap(_.get(token)).contains(100L))
    }

    // ============================================================================
    // Input Selection Edge Cases
    // ============================================================================

    test("complete should exclude already-spent inputs from selection") {
        val explicitInput = input(0)

        val provider = Emulator(
          Map(
            explicitInput -> adaOutput(Alice.address, 10),
            input(1) -> adaOutput(Alice.address, 100)
          )
        )

        val tx = TxBuilder(testEnv)
            .spend(Utxo(explicitInput, adaOutput(Alice.address, 10)))
            .payTo(Bob.address, Value.ada(50))
            .complete(provider, Alice.address)
            .await()
            .transaction

        assert(tx.body.value.inputs.toSeq.contains(explicitInput))
        assert(tx.body.value.inputs.toSeq.contains(input(1)))
    }

    test("complete with explicit spend should only add necessary additional inputs") {
        val explicitUtxo = genAdaOnlyPubKeyUtxo(Alice, min = 3_000_000).sample.get

        val provider = Emulator(
          Map(
            input(0) -> adaOutput(Alice.address, 100),
            explicitUtxo._1 -> explicitUtxo._2
          )
        )

        val tx = TxBuilder(testEnv)
            .spend(Utxo(explicitUtxo))
            .payTo(Bob.address, explicitUtxo._2.value)
            .complete(provider, Alice.address)
            .await()
            .transaction

        assert(tx.body.value.inputs.toSeq.contains(explicitUtxo._1))
        assert(tx.body.value.inputs.toSeq.size == 2)
        assert(outputsOf(Bob, tx).head.value.value.coin.value >= 3_000_000L)

        val aliceAda = outputsOf(Alice, tx).head.value.value.coin
        assert(aliceAda < Coin.ada(100) && aliceAda > Coin.ada(99))
    }

    // ============================================================================
    // Token Handling Edge Cases
    // ============================================================================

    test("complete should handle sending all tokens from a UTXO") {
        val provider = Emulator(
          Map(
            input(0) -> tokenOutput(Alice.address, 50, policyId -> Map(co2 -> 100L)),
            input(1) -> adaOutput(Alice.address, 50)
          )
        )

        val tx = TxBuilder(testEnv)
            .payTo(Bob.address, Value.fromPolicy(policyId, Map(co2 -> 100L), Coin.ada(5)))
            .complete(provider, Alice.address)
            .await()
            .transaction

        val bobTokens = outputsOf(Bob, tx).head.value.value.assets.assets
            .get(policyId)
            .flatMap(_.get(co2))
        assert(bobTokens.contains(100L), "Bob should receive all 100 tokens")

        val aliceTokens = outputsOf(Alice, tx).flatMap(
          _.value.value.assets.assets.get(policyId).flatMap(_.get(co2))
        )
        assert(aliceTokens.isEmpty, "Alice should not receive any tokens back")
    }

    test("complete should fail when required tokens are not available") {
        val nonexistent = AssetName.fromString("nonexistent")
        val provider = Emulator(
          Map(input(0) -> adaOutput(Alice.address, 100))
        )

        val exception = intercept[TxBuilderException.InsufficientTokensException] {
            TxBuilder(testEnv)
                .payTo(
                  Bob.address,
                  Value.fromPolicy(policyId, Map(nonexistent -> 100L), Coin.ada(5))
                )
                .complete(provider, Alice.address)
                .await()
        }

        assert(
          exception.policyId == policyId,
          s"Should indicate insufficient tokens for policyId $policyId"
        )
        assert(
          exception.assetName == nonexistent,
          s"Should indicate insufficient tokens for asset $nonexistent"
        )
    }

    test("complete should handle tokens from same UTXO under different policies") {
        val provider = Emulator(
          Map(
            input(0) -> tokenOutput(
              Alice.address,
              100,
              policyId -> Map(token -> 100L),
              policyId3 -> Map(token -> 200L)
            )
          )
        )

        val tx = TxBuilder(testEnv)
            .payTo(Bob.address, Value.fromPolicy(policyId, Map(token -> 50L), Coin.ada(5)))
            .complete(provider, Alice.address)
            .await()
            .transaction

        val bobPolicy1Tokens = outputsOf(Bob, tx).head.value.value.assets.assets
            .get(policyId)
            .flatMap(_.get(token))
        assert(bobPolicy1Tokens.contains(50L))

        val alicePolicy1 = outputsOf(Alice, tx)
            .flatMap(_.value.value.assets.assets.get(policyId).flatMap(_.get(token)))
        val alicePolicy2 = outputsOf(Alice, tx)
            .flatMap(_.value.value.assets.assets.get(policyId3).flatMap(_.get(token)))

        assert(alicePolicy1.sum == 50L, "Alice should receive 50 policy1 tokens as change")
        assert(alicePolicy2.sum == 200L, "Alice should receive all 200 policy2 tokens as change")
    }

    // ============================================================================
    // Change Output Handling
    // ============================================================================

    test("complete should ensure change output meets minimum ADA") {
        val provider = Emulator(
          Map(input(0) -> adaOutput(Alice.address, 100))
        )

        val tx = TxBuilder(testEnv)
            .payTo(Bob.address, Value.ada(98))
            .complete(provider, Alice.address)
            .await()
            .transaction

        val aliceOutputs = outputsOf(Alice, tx)
        if aliceOutputs.nonEmpty then {
            val changeAda = aliceOutputs.head.value.value.coin.value
            val minAda = testEnv.protocolParams.utxoCostPerByte * 160 // rough estimate
            assert(changeAda >= minAda, s"Change output ($changeAda) should meet min ADA ($minAda)")
        }
    }

    test("complete should correctly place change output at sponsor address") {
        val provider = Emulator(
          Map(input(0) -> adaOutput(Alice.address, 100))
        )

        val tx = TxBuilder(testEnv)
            .payTo(Bob.address, Value.ada(10))
            .complete(provider, Alice.address)
            .await()
            .transaction

        val nonBobOutputs = tx.body.value.outputs.toSeq.filterNot(_.value.address == Bob.address)
        assert(nonBobOutputs.nonEmpty, "Should have change output")
        assert(
          nonBobOutputs.forall(_.value.address == Alice.address),
          "All change should go to Alice"
        )
    }

    // ============================================================================
    // Validity Interval Tests
    // ============================================================================

    test("complete should preserve validity interval settings") {
        val provider = Emulator(
          Map(input(0) -> adaOutput(Alice.address, 100))
        )

        val tx = TxBuilder(testEnv)
            .validFrom(Instant.ofEpochSecond(1000))
            .validTo(Instant.ofEpochSecond(2000))
            .payTo(Bob.address, Value.ada(10))
            .complete(provider, Alice.address)
            .await()
            .transaction

        assert(tx.body.value.validityStartSlot.isDefined, "Should preserve validFrom")
        assert(tx.body.value.ttl.isDefined, "Should preserve validTo (TTL)")
    }

    // ============================================================================
    // Error Handling
    // ============================================================================

    test("complete should fail with insufficient funds") {
        val provider = Emulator(
          Map(input(0) -> adaOutput(Alice.address, 5))
        )

        val exception = intercept[TxBuilderException.InsufficientAdaException] {
            TxBuilder(testEnv)
                .payTo(Bob.address, Value.ada(100))
                .complete(provider, Alice.address)
                .await()
        }

        assert(
          exception.required.value > exception.available.value,
          s"Required ${exception.required.value} should be greater than available ${exception.available.value}"
        )
    }

    test("complete should handle empty UTXO set from provider") {
        val provider = Emulator(Map.empty)

        val exception = intercept[TxBuilderException.InsufficientAdaException] {
            TxBuilder(testEnv)
                .payTo(Bob.address, Value.ada(10))
                .complete(provider, Alice.address)
                .await()
        }

        assert(
          exception.available.value == 0,
          s"Should indicate no UTXOs available (0 ADA), got: ${exception.available.value}"
        )
    }

    test("complete should fail when outputs exceed possible input value") {
        val provider = Emulator(
          Map(input(0) -> adaOutput(Alice.address, 10))
        )

        val exception = intercept[TxBuilderException.InsufficientAdaException] {
            TxBuilder(testEnv)
                .payTo(Bob.address, Value.ada(1000))
                .complete(provider, Alice.address)
                .await()
        }

        assert(
          exception.required.value > exception.available.value,
          s"Should indicate insufficient funds, got: ${exception.getMessage}"
        )
    }

    // ============================================================================
    // Integration Tests
    // ============================================================================

    test("complete should produce deterministic results") {
        val provider = Emulator(
          Map(
            input(0) -> adaOutput(Alice.address, 100),
            input(1) -> adaOutput(Alice.address, 50)
          )
        )

        val tx1 = TxBuilder(testEnv)
            .payTo(Bob.address, Value.ada(10))
            .complete(provider, Alice.address)
            .await()
            .transaction

        val tx2 = TxBuilder(testEnv)
            .payTo(Bob.address, Value.ada(10))
            .complete(provider, Alice.address)
            .await()
            .transaction

        assert(tx1.body.value.inputs == tx2.body.value.inputs, "Inputs should be deterministic")
        assert(tx1.body.value.outputs == tx2.body.value.outputs, "Outputs should be deterministic")
        assert(tx1.body.value.fee == tx2.body.value.fee, "Fee should be deterministic")
    }

    // ============================================================================
    // Fee Calculation Bug Test
    // ============================================================================

    test("complete should account for signature size in fee calculation") {
        // BUG: TxBuilder.complete doesn't add dummy signatures during fee calculation,
        // unlike finalizeContext which uses addDummySignatures(this.expectedSigners.size, this.transaction)
        // before balancing. This means the fee is calculated without accounting for the
        // size of signatures that will be added when signing.

        val provider = Emulator(
          Map(input(0) -> adaOutput(Alice.address, 100))
        )

        val completedBuilder = TxBuilder(testEnv)
            .payTo(Bob.address, Value.ada(10))
            .complete(provider, Alice.address)
            .await()

        val unsignedTx = completedBuilder.transaction
        val signedTx = completedBuilder.sign(Alice.signer).transaction

        // Calculate the minimum fee required for the signed transaction
        // For simple pubkey transactions without reference scripts, pass the input UTXOs
        val resolvedUtxos: Utxos = signedTx.body.value.inputs.toSeq.flatMap { input =>
            provider.findUtxo(input).await().toOption.map(u => input -> u.output)
        }.toMap
        val signedTxMinFee = MinTransactionFee
            .computeMinFee(signedTx, resolvedUtxos, testEnv.protocolParams)
            .getOrElse(Coin.zero)

        val feeInTx = signedTx.body.value.fee

        // The fee calculated during complete should be >= the minimum required fee for signed tx
        // This test will FAIL because complete doesn't account for signature size
        assert(
          feeInTx >= signedTxMinFee,
          s"Fee in transaction ($feeInTx) should be >= min fee for signed tx ($signedTxMinFee). " +
              s"Difference: ${signedTxMinFee.value - feeInTx.value} lovelace"
        )
    }

    // ============================================================================
    // Delayed Redeemer Bug Test
    // ============================================================================

    test("complete should recompute delayed redeemers after adding inputs") {
        // BUG: complete adds inputs dynamically but doesn't recompute delayed redeemers
        // The redeemerBuilder is called BEFORE additional inputs are added, so redeemer
        // data computed from transaction (like input count) will be wrong.

        // Create script address using the alwaysOkScript already defined in the test
        val scriptAddr = Address(testEnv.network, Credential.ScriptHash(alwaysOkScript.scriptHash))

        // Script UTXO with minimal ADA - NOT enough for payment + fees + change
        // Contains the script as a reference script in the UTXO itself
        val scriptUtxo = Utxo(
          input(0),
          TransactionOutput(
            scriptAddr,
            Value.ada(3), // Only 3 ADA - not enough for 1 ADA payment + fees + change
            Some(DatumOption.Inline(0.toData)),
            scriptRef = Some(ScriptRef(alwaysOkScript))
          )
        )

        // Alice has ADA for fees - this will be selected by complete
        val provider = Emulator(
          Map(
            input(0) -> scriptUtxo.output,
            input(1) -> adaOutput(Alice.address, 100),
            input(2) -> adaOutput(Alice.address, 50) // Extra UTXO for collateral
          )
        )

        // RedeemerBuilder that captures input count at build time
        val redeemerBuilder: Transaction => Data = tx => {
            tx.body.value.inputs.toSeq.size.toData // Number of inputs
        }

        val completedBuilder = TxBuilder(testEnv)
            .spend(scriptUtxo, redeemerBuilder) // Uses ScriptSource.PlutusScriptAttached
            .payTo(Bob.address, Value.ada(2)) // 2 ADA - leaves only 1 ADA for fees/change
            .complete(provider, Alice.address)
            .await()

        val tx = completedBuilder.transaction

        // complete added Alice's input for fees, so now there are 2 inputs
        // But the redeemer was computed when there was only 1 input (the script UTXO)
        val actualInputCount = tx.body.value.inputs.toSeq.size
        val redeemerData = tx.witnessSet.redeemers.get.value.toSeq.head.data

        // This assertion FAILS - demonstrating the bug
        // The redeemer says 1 input, but there are actually 2
        assert(
          redeemerData == actualInputCount.toData,
          s"Redeemer should have input count $actualInputCount but has $redeemerData"
        )
    }

    // ============================================================================
    // Synchronous complete(availableUtxos, sponsor) Tests
    // ============================================================================

    test("complete with UTXOs should automatically add inputs for simple ADA payment") {
        val availableUtxos: Utxos = Map(
          input(0) -> adaOutput(Alice.address, 100),
          input(1) -> adaOutput(Alice.address, 50)
        )

        val tx = TxBuilder(testEnv)
            .payTo(Bob.address, Value.ada(10))
            .complete(availableUtxos, Alice.address)
            .transaction

        assert(tx.body.value.inputs.toSeq.size == 1, "Transaction must have exactly 1 input")
        assert(outputsOf(Bob, tx).size == 1, "Should have output to Bob")
        assert(
          outputsOf(Bob, tx).head.value.value.coin >= Coin.ada(10),
          "Bob should receive at least 10 ADA"
        )
        assert(outputsOf(Alice, tx).nonEmpty, "Should have change output to Alice")
    }

    test("complete with UTXOs should handle multi-asset transactions") {
        val availableUtxos: Utxos = Map(
          input(0) -> tokenOutput(Alice.address, 100, policyId -> Map(co2 -> 200L)),
          input(1) -> adaOutput(Alice.address, 50)
        )

        val tx = TxBuilder(testEnv)
            .payTo(Bob.address, Value.fromPolicy(policyId, Map(co2 -> 100L), Coin.ada(5)))
            .complete(availableUtxos, Alice.address)
            .transaction

        val bobTokens = outputsOf(Bob, tx).head.value.value.assets.assets
            .get(policyId)
            .flatMap(_.get(co2))
        assert(bobTokens.contains(100L), "Bob should receive 100 tokens")

        val aliceTokens = outputsOf(Alice, tx).flatMap(
          _.value.value.assets.assets.get(policyId).flatMap(_.get(co2))
        )
        assert(aliceTokens.sum == 100L, "Alice should receive 100 tokens as change")
    }

    test("complete with UTXOs should auto-detect and add collateral for script spending") {
        val sUtxo = scriptUtxo(2, 20)

        val availableUtxos: Utxos = Map(
          input(0) -> adaOutput(Alice.address, 100),
          input(1) -> adaOutput(Alice.address, 10),
          sUtxo.input -> sUtxo.output
        )

        val tx = TxBuilder(testEnv)
            .spend(sUtxo, emptyRedeemer, alwaysOkScript)
            .payTo(Bob.address, Value.ada(5))
            .complete(availableUtxos, Alice.address)
            .transaction

        assert(tx.body.value.collateralInputs.toSeq.nonEmpty, "Should have collateral inputs")
        assert(outputsOf(Bob, tx).nonEmpty, "Should have output to Bob")
    }

    test("complete with UTXOs should fail with insufficient funds") {
        val availableUtxos: Utxos = Map(
          input(0) -> adaOutput(Alice.address, 5)
        )

        val exception = intercept[TxBuilderException.InsufficientAdaException] {
            TxBuilder(testEnv)
                .payTo(Bob.address, Value.ada(100))
                .complete(availableUtxos, Alice.address)
        }

        assert(
          exception.required.value > exception.available.value,
          s"Required ${exception.required.value} should be greater than available ${exception.available.value}"
        )
    }

    test("complete with UTXOs should handle empty UTXO set") {
        val availableUtxos: Utxos = Map.empty

        val exception = intercept[TxBuilderException.InsufficientAdaException] {
            TxBuilder(testEnv)
                .payTo(Bob.address, Value.ada(10))
                .complete(availableUtxos, Alice.address)
        }

        assert(
          exception.available.value == 0,
          s"Should indicate no UTXOs available (0 ADA), got: ${exception.available.value}"
        )
    }

    test("complete with UTXOs should fail when required tokens are not available") {
        val nonexistent = AssetName.fromString("nonexistent")
        val availableUtxos: Utxos = Map(
          input(0) -> adaOutput(Alice.address, 100)
        )

        val exception = intercept[TxBuilderException.InsufficientTokensException] {
            TxBuilder(testEnv)
                .payTo(
                  Bob.address,
                  Value.fromPolicy(policyId, Map(nonexistent -> 100L), Coin.ada(5))
                )
                .complete(availableUtxos, Alice.address)
        }

        assert(
          exception.policyId == policyId,
          s"Should indicate insufficient tokens for policyId $policyId"
        )
        assert(
          exception.assetName == nonexistent,
          s"Should indicate insufficient tokens for asset $nonexistent"
        )
    }

    test("complete with UTXOs should not add collateral for pubkey-only transactions") {
        val availableUtxos: Utxos = Map(
          input(0) -> adaOutput(Alice.address, 100)
        )

        val tx = TxBuilder(testEnv)
            .payTo(Bob.address, Value.ada(10))
            .complete(availableUtxos, Alice.address)
            .transaction

        assert(tx.body.value.collateralInputs.toSeq.isEmpty, "Should not have collateral inputs")
        assert(tx.body.value.collateralReturnOutput.isEmpty, "Should not have collateral return")
        assert(tx.body.value.totalCollateral.isEmpty, "Should not have totalCollateral")
    }

    test("complete with UTXOs should be signable with sign()") {
        val availableUtxos: Utxos = Map(
          input(0) -> adaOutput(Alice.address, 100)
        )

        val signedTx = TxBuilder(testEnv)
            .payTo(Bob.address, Value.ada(10))
            .complete(availableUtxos, Alice.address)
            .sign(Alice.signer)
            .transaction

        assert(
          signedTx.witnessSet.vkeyWitnesses.toSeq.nonEmpty,
          "Transaction should have vkey witnesses after signing"
        )
    }

    // ============================================================================
    // Redeemer Index Bug Test
    // ============================================================================

    test("complete should recalculate redeemer indexes after adding inputs") {
        // This test verifies that redeemer indexes are correctly recalculated when
        // complete() adds inputs during balancing that sort before existing script inputs.
        //
        // Scenario:
        // 1. Script UTXO has txhash starting with "b" (sorts second)
        // 2. Sponsor UTXO has txhash starting with "0" (sorts first)
        // 3. Initially script input is at index 0 with redeemer index 0
        // 4. After balancing adds sponsor input, script moves to index 1
        // 5. Redeemer index should be updated to 1

        // Script UTXO with transaction hash starting with "b" (sorts AFTER sponsor)
        val scriptTxHash = TransactionHash.fromByteString(ByteString.fromHex("b" + "0" * 63))
        val scriptUtxoInput = Input(scriptTxHash, 0)

        // Sponsor UTXOs with transaction hashes starting with "0" (sorts BEFORE script "b...")
        // Need multiple UTXOs: one for collateral, one for fees
        val sponsorTxHash1 = TransactionHash.fromByteString(ByteString.fromHex("0" * 64))
        val sponsorInput1 = Input(sponsorTxHash1, 0)
        val sponsorInput2 = Input(sponsorTxHash1, 1) // Same txhash, different index

        // TxBuilder requires a datum for script outputs
        val sUtxo = Utxo(
          scriptUtxoInput,
          TransactionOutput(
            scriptAddress,
            Value.ada(3), // Minimal ADA, forces balancing to add sponsor input
            Some(inlineDatum42)
          )
        )

        val provider = Emulator(
          Map(
            sUtxo.input -> sUtxo.output,
            sponsorInput1 -> adaOutput(Alice.address, 100), // For fees
            sponsorInput2 -> adaOutput(Alice.address, 50) // For collateral
          )
        )

        // After the fix, this should succeed because redeemer indexes are re-calculated
        val tx = TxBuilder(testEnv)
            .spend(sUtxo, emptyRedeemer, alwaysOkScript)
            .payTo(Bob.address, Value.ada(2))
            .complete(provider, Alice.address)
            .await()
            .transaction

        // Verify the script input's position changed (sponsor input added before it)
        val sortedInputs = tx.body.value.inputs.toSeq.sorted
        assert(
          sortedInputs.indexOf(sUtxo.input) == 1,
          s"Script input should be at index 1 after sponsor input added, got: ${sortedInputs.indexOf(sUtxo.input)}"
        )

        // Verify redeemer has correct index (should be 1, not 0)
        val redeemer = tx.witnessSet.redeemers.get.value.toSeq
            .find(_.tag == RedeemerTag.Spend)
            .get
        assert(
          redeemer.index == 1,
          s"Redeemer index should be 1 but was ${redeemer.index}"
        )
        // This test verifies that delayed redeemers have correct ExUnits computed.
        // The bug: after replaceDelayedRedeemers, fromEditableTransactionSafe creates
        // redeemers with ExUnits.zero, discarding the ExUnits computed during balancing.
        assert(
          redeemer.exUnits.memory > 0 && redeemer.exUnits.steps > 0,
          s"ExUnits should be non-zero, got: ${redeemer.exUnits}"
        )
    }

}
