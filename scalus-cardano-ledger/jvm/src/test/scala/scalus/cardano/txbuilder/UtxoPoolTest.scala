package scalus.cardano.txbuilder

import org.scalatest.funsuite.AnyFunSuite
import scalus.builtin.ByteString
import scalus.cardano.ledger.*
import scalus.cardano.ledger.utils.MinCoinSizedTransactionOutput
import scalus.cardano.txbuilder.TestPeer.Alice

class UtxoPoolTest extends AnyFunSuite {

    // Use mainnet protocol params like other tests
    val testProtocolParams: ProtocolParams = CardanoInfo.mainnet.protocolParams

    // Computed minAda for a simple ADA-only output (using Alice's address as reference)
    val minAdaForSimpleOutput: Long = {
        val minimalOutput = TransactionOutput(Alice.address, Value.zero)
        MinCoinSizedTransactionOutput.ensureMinAda(Sized(minimalOutput), testProtocolParams).value
    }

    // Common test values
    val genesisHash: TransactionHash =
        TransactionHash.fromByteString(ByteString.fromHex("0" * 64))

    val policyId: ScriptHash = ScriptHash.fromByteString(ByteString.fromHex("1" * 56))
    val tokenName: AssetName = AssetName.fromString("token")

    // Helper methods
    def input(index: Int): TransactionInput = TransactionInput(genesisHash, index)

    def adaOutput(ada: Int): TransactionOutput =
        TransactionOutput(Alice.address, Value.ada(ada))

    def adaOutputLovelace(lovelace: Long): TransactionOutput =
        TransactionOutput(Alice.address, Value.lovelace(lovelace))

    def tokenOutput(ada: Int, tokenAmount: Long): TransactionOutput =
        TransactionOutput(
          Alice.address,
          Value.assets(Map(policyId -> Map(tokenName -> tokenAmount)), Coin.ada(ada))
        )

    // ============================================================================
    // selectForValue tests
    // ============================================================================

    test("selectForValue: selects single UTxO when one is sufficient") {
        val utxos = Map(
          input(0) -> adaOutput(10),
          input(1) -> adaOutput(5),
          input(2) -> adaOutput(3)
        )
        val pool = UtxoPool(utxos)
        val selected = pool.selectForValue(Value.ada(4))

        assert(selected.size == 1, "Should select exactly one UTxO")
        assert(
          selected.head._2.value.coin == Coin.ada(5),
          "Should select smallest sufficient UTxO (5 ADA)"
        )
    }

    test("selectForValue: selects smallest sufficient UTxO (not largest)") {
        val utxos = Map(
          input(0) -> adaOutput(100),
          input(1) -> adaOutput(50),
          input(2) -> adaOutput(20),
          input(3) -> adaOutput(10)
        )
        val pool = UtxoPool(utxos)
        val selected = pool.selectForValue(Value.ada(15))

        assert(selected.size == 1, "Should select exactly one UTxO")
        assert(
          selected.head._2.value.coin == Coin.ada(20),
          "Should select smallest sufficient UTxO (20 ADA), not the largest"
        )
    }

    test(
      "selectForValue: selects multiple UTxOs (largest first) when no single UTxO covers requirement"
    ) {
        val utxos = Map(
          input(0) -> adaOutput(5),
          input(1) -> adaOutput(4),
          input(2) -> adaOutput(3),
          input(3) -> adaOutput(2)
        )
        val pool = UtxoPool(utxos)
        val selected = pool.selectForValue(Value.ada(8))

        assert(selected.size == 2, "Should select exactly two UTxOs")
        val selectedAmounts = selected.values.map(_.value.coin).toSet
        assert(
          selectedAmounts == Set(Coin.ada(5), Coin.ada(4)),
          "Should select largest UTxOs first (5 ADA + 4 ADA = 9 ADA)"
        )
    }

    test("selectForValue: selects UTxOs with required tokens first") {
        val utxos = Map(
          input(0) -> adaOutput(10),
          input(1) -> tokenOutput(5, 100),
          input(2) -> adaOutput(3)
        )
        val pool = UtxoPool(utxos)

        // Need tokens and ADA
        val required = Value.assets(Map(policyId -> Map(tokenName -> 50L)), Coin.ada(1))
        val selected = pool.selectForValue(required)

        assert(selected.size == 1, "Should select exactly one UTxO (the one with tokens)")
        assert(
          selected.contains(input(1)),
          "Should select the UTxO containing the required tokens"
        )
    }

    test(
      "selectForValue: selects additional ADA-only UTxOs when token UTxOs don't have enough ADA"
    ) {
        val utxos = Map(
          input(0) -> adaOutput(10),
          input(1) -> tokenOutput(2, 100),
          input(2) -> adaOutput(5)
        )
        val pool = UtxoPool(utxos)

        // Need tokens and more ADA than the token UTxO has
        val required = Value.assets(Map(policyId -> Map(tokenName -> 50L)), Coin.ada(8))
        val selected = pool.selectForValue(required)

        assert(selected.size == 2, "Should select two UTxOs")
        assert(selected.contains(input(1)), "Should include the token UTxO")
        // The remaining ADA needed is 8 - 2 = 6 ADA, smallest sufficient is 10 ADA
        assert(selected.contains(input(0)), "Should include the smallest sufficient ADA-only UTxO")
    }

    test("selectForValue: returns empty map when requirement is zero") {
        val utxos = Map(
          input(0) -> adaOutput(10),
          input(1) -> adaOutput(5)
        )
        val pool = UtxoPool(utxos)
        val selected = pool.selectForValue(Value.zero)

        assert(selected.isEmpty, "Should return empty map when no value is required")
    }

    test("selectForValue: does not select already-selected inputs") {
        val utxos = Map(
          input(0) -> adaOutput(10),
          input(1) -> adaOutput(5),
          input(2) -> adaOutput(3)
        )
        val pool = UtxoPool(utxos)

        // First selection
        val firstSelection = pool.selectForValue(Value.ada(4))
        assert(firstSelection.size == 1)

        // Add to pool and select more
        val updatedPool = pool.withInputs(firstSelection)
        val secondSelection = updatedPool.selectForValue(Value.ada(4))

        // Should not select the same UTxO again
        assert(
          (firstSelection.keySet & secondSelection.keySet).isEmpty,
          "Second selection should not contain any UTxOs from first selection"
        )
    }

    // ============================================================================
    // selectForCollateral tests
    // ============================================================================

    test("selectForCollateral: prefers ADA-only UTxOs over token UTxOs") {
        val utxos = Map(
          input(0) -> tokenOutput(10, 100),
          input(1) -> adaOutput(5),
          input(2) -> tokenOutput(8, 50)
        )
        val pool = UtxoPool(utxos)
        val selected = pool.selectForCollateral(Coin.ada(3), testProtocolParams)

        assert(selected.size == 1, "Should select exactly one UTxO")
        assert(
          selected.contains(input(1)),
          "Should select the ADA-only UTxO even though token UTxOs have more ADA"
        )
    }

    test("selectForCollateral: selects optimal UTxO (covers requirement, excess < minAda)") {
        // minAda for simple output is approximately 0.95 ADA
        val requiredCollateral = Coin.ada(2)

        val utxos = Map(
          input(0) -> adaOutput(10), // 10 ADA - excess 8 ADA > minAda
          input(1) -> adaOutputLovelace(2_500_000), // 2.5 ADA - excess 0.5 ADA < minAda (optimal!)
          input(2) -> adaOutput(5) // 5 ADA - excess 3 ADA > minAda
        )
        val pool = UtxoPool(utxos)
        val selected = pool.selectForCollateral(requiredCollateral, testProtocolParams)

        assert(selected.size == 1, "Should select exactly one UTxO")
        val selectedOutput = selected.head._2
        val excess = selectedOutput.value.coin.value - requiredCollateral.value

        assert(
          excess < minAdaForSimpleOutput,
          s"Should select UTxO with excess ($excess) < minAda ($minAdaForSimpleOutput)"
        )
        assert(
          selected.contains(input(1)),
          "Should select the 2.5 ADA UTxO as optimal"
        )
    }

    test("selectForCollateral: selects smallest sufficient UTxO when no optimal exists") {
        val requiredCollateral = Coin.ada(2)
        // All UTxOs have excess >= minAda, so no optimal exists

        val utxos = Map(
          input(0) -> adaOutput(10),
          input(1) -> adaOutput(5),
          input(2) -> adaOutput(3) // smallest sufficient
        )
        val pool = UtxoPool(utxos)
        val selected = pool.selectForCollateral(requiredCollateral, testProtocolParams)

        assert(selected.size == 1, "Should select exactly one UTxO")
        assert(
          selected.contains(input(2)),
          "Should select smallest sufficient UTxO (3 ADA)"
        )
    }

    test(
      "selectForCollateral: selects multiple UTxOs (largest first) when no single UTxO covers requirement"
    ) {
        val utxos = Map(
          input(0) -> adaOutput(3),
          input(1) -> adaOutput(2),
          input(2) -> adaOutputLovelace(1_500_000), // 1.5 ADA
          input(3) -> adaOutput(1)
        )
        val pool = UtxoPool(utxos)
        val selected = pool.selectForCollateral(Coin.ada(5), testProtocolParams)

        assert(selected.size == 2, "Should select exactly two UTxOs")
        val selectedAmounts = selected.values.map(_.value.coin).toSet
        assert(
          selectedAmounts == Set(Coin.ada(3), Coin.ada(2)),
          "Should select largest UTxOs first (3 ADA + 2 ADA = 5 ADA)"
        )
    }

    test("selectForCollateral: can reuse UTxOs already selected for inputs") {
        // Only one UTxO available - must be reused for both input and collateral
        val utxos = Map(
          input(0) -> adaOutput(10)
        )
        val pool = UtxoPool(utxos)

        // Select for inputs first
        val inputSelection = pool.selectForValue(Value.ada(4))
        assert(inputSelection.contains(input(0)), "Should select the only available UTxO")

        val poolWithInputs = pool.withInputs(inputSelection)

        // Select for collateral - must reuse the same UTxO since it's the only one
        val collateralSelection =
            poolWithInputs.selectForCollateral(Coin.ada(3), testProtocolParams)

        // Verify the same UTxO is used for both input and collateral
        assert(
          collateralSelection.contains(input(0)),
          "Should reuse the same UTxO for collateral (collateral only consumed if scripts fail)"
        )
        assert(
          inputSelection.keySet == collateralSelection.keySet,
          "Input and collateral should be the same UTxO"
        )
    }

    test("selectForCollateral: uses token UTxOs when ADA-only UTxOs are insufficient") {
        val utxos = Map(
          input(0) -> adaOutput(1),
          input(1) -> tokenOutput(10, 100)
        )
        val pool = UtxoPool(utxos)
        val selected = pool.selectForCollateral(Coin.ada(5), testProtocolParams)

        assert(selected.nonEmpty, "Should select some UTxOs")
        assert(
          selected.contains(input(1)),
          "Should use token UTxO when ADA-only is insufficient"
        )
    }

    test("selectForCollateral: returns empty map when no UTxOs available") {
        val pool = UtxoPool(Map.empty)
        val selected = pool.selectForCollateral(Coin.ada(5), testProtocolParams)

        assert(selected.isEmpty, "Should return empty map when no UTxOs available")
    }

    test("selectForCollateral: documents no collateralReturn when optimal UTxO selected") {
        // This test documents the expected behavior:
        // When we select an optimal collateral UTxO (excess < minAda),
        // the TransactionBuilder will NOT create a collateralReturn output.

        val requiredCollateral = Coin.ada(3)

        // Create a UTxO with just enough excess to not trigger collateral return
        // excess = 3.5 ADA - 3 ADA = 0.5 ADA < minAda (~0.95 ADA)
        val optimalUtxo = adaOutputLovelace(3_500_000)

        val utxos = Map(
          input(0) -> adaOutput(10), // Would create collateral return
          input(1) -> optimalUtxo // Optimal - no collateral return needed
        )
        val pool = UtxoPool(utxos)
        val selected = pool.selectForCollateral(requiredCollateral, testProtocolParams)

        assert(selected.size == 1)
        val selectedOutput = selected.head._2
        val excess = selectedOutput.value.coin.value - requiredCollateral.value

        // Verify the selected UTxO is optimal (excess < minAda)
        assert(
          excess < minAdaForSimpleOutput,
          s"Optimal UTxO should have excess ($excess) < minAda ($minAdaForSimpleOutput), " +
              "which means no collateralReturn output will be created"
        )
    }

    // ============================================================================
    // withInputs / withCollateral tests
    // ============================================================================

    test("withInputs: correctly updates selected sets") {
        val utxos = Map(
          input(0) -> adaOutput(10),
          input(1) -> adaOutput(5)
        )
        val pool = UtxoPool(utxos)
        val newInputs = Map(input(0) -> adaOutput(10))
        val updatedPool = pool.withInputs(newInputs)

        assert(updatedPool.inputs == newInputs, "inputs should contain the added UTxOs")
        assert(pool.inputs.isEmpty, "Original pool should be unchanged")
    }

    test("withCollateral: correctly updates selected sets") {
        val utxos = Map(
          input(0) -> adaOutput(10),
          input(1) -> adaOutput(5)
        )
        val pool = UtxoPool(utxos)
        val newCollateral = Map(input(0) -> adaOutput(10))
        val updatedPool = pool.withCollateral(newCollateral)

        assert(updatedPool.collateral == newCollateral, "collateral should contain the added UTxOs")
        assert(pool.collateral.isEmpty, "Original pool should be unchanged")
    }

    test("remainingForInputs: excludes newly selected inputs") {
        val utxos = Map(
          input(0) -> adaOutput(10),
          input(1) -> adaOutput(5),
          input(2) -> adaOutput(3)
        )
        val pool = UtxoPool(utxos)

        assert(pool.remainingForInputs.size == 3, "All UTxOs should be initially remaining")

        val selection = Map(input(0) -> adaOutput(10))
        val updatedPool = pool.withInputs(selection)

        assert(
          updatedPool.remainingForInputs.size == 2,
          "remainingForInputs should exclude selected input"
        )
        assert(
          !updatedPool.remainingForInputs.contains(input(0)),
          "Selected input should not be in remainingForInputs"
        )
    }

    test("immutability: original pool unchanged after modifications") {
        val utxos = Map(
          input(0) -> adaOutput(10),
          input(1) -> adaOutput(5)
        )
        val pool = UtxoPool(utxos)

        // Make various modifications
        val pool1 = pool.withInputs(Map(input(0) -> adaOutput(10)))
        val pool2 = pool.withCollateral(Map(input(1) -> adaOutput(5)))

        // Verify original is unchanged
        assert(pool.inputs.isEmpty, "Original pool inputs should be empty")
        assert(pool.collateral.isEmpty, "Original pool collateral should be empty")
        assert(pool.remainingForInputs.size == 2, "Original pool should have all UTxOs remaining")

        // Verify modifications are independent
        assert(pool1.inputs.size == 1)
        assert(pool2.collateral.size == 1)
        assert(pool1.collateral.isEmpty)
        assert(pool2.inputs.isEmpty)
    }

    // ============================================================================
    // Integration-style tests
    // ============================================================================

    test("integration: full flow - select inputs, then select collateral reusing inputs") {
        val utxos = Map(
          input(0) -> adaOutput(10),
          input(1) -> adaOutput(5),
          input(2) -> adaOutput(3)
        )
        val pool = UtxoPool(utxos)

        // Step 1: Select for inputs
        val inputSelection = pool.selectForValue(Value.ada(4))
        assert(inputSelection.size == 1, "Should select one input")

        val poolWithInputs = pool.withInputs(inputSelection)

        // Step 2: Select for collateral (can reuse the same UTxO)
        val collateralSelection =
            poolWithInputs.selectForCollateral(Coin.ada(3), testProtocolParams)
        assert(collateralSelection.nonEmpty, "Should select collateral")

        // Verify the same UTxO can be used for both
        // (collateral is only consumed if script fails)
        val inputKeys = inputSelection.keySet
        val collateralKeys = collateralSelection.keySet

        // It's valid for collateral to reuse input UTxOs
        // This test just verifies the flow works
        assert(
          poolWithInputs.inputs == inputSelection,
          "Pool should track inputs correctly"
        )
    }

    test("integration: edge case - all UTxOs have tokens") {
        val utxos = Map(
          input(0) -> tokenOutput(10, 100),
          input(1) -> tokenOutput(5, 50),
          input(2) -> tokenOutput(3, 25)
        )
        val pool = UtxoPool(utxos)

        // Select for collateral when all UTxOs have tokens
        val selected = pool.selectForCollateral(Coin.ada(4), testProtocolParams)

        assert(selected.nonEmpty, "Should select token UTxOs when no ADA-only available")

        // Should select smallest sufficient
        if selected.size == 1 then {
            assert(
              selected.contains(input(1)),
              "Should select smallest sufficient token UTxO (5 ADA)"
            )
        }
    }

    test("totalAvailableAda: returns correct total") {
        val utxos = Map(
          input(0) -> adaOutput(10),
          input(1) -> adaOutput(5),
          input(2) -> tokenOutput(3, 100)
        )
        val pool = UtxoPool(utxos)

        assert(
          pool.totalAvailableAda == Coin.ada(18),
          "Should return total ADA across all UTxOs"
        )
    }

    test("totalAvailableTokens: returns correct token total") {
        val utxos = Map(
          input(0) -> tokenOutput(10, 100),
          input(1) -> tokenOutput(5, 50),
          input(2) -> adaOutput(3)
        )
        val pool = UtxoPool(utxos)

        assert(
          pool.totalAvailableTokens(policyId, tokenName) == 150,
          "Should return total tokens across all UTxOs"
        )
    }
}
