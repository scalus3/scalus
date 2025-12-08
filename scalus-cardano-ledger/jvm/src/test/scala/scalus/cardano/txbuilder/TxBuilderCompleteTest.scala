package scalus.cardano.txbuilder

import org.scalatest.funsuite.AnyFunSuite
import scalus.builtin.{ByteString, Data}
import scalus.cardano.address.{Address, ShelleyAddress, ShelleyDelegationPart, ShelleyPaymentPart}
import scalus.cardano.ledger.*
import scalus.cardano.ledger.DatumOption.Inline
import scalus.cardano.ledger.rules.ValidatorRulesTestKit
import scalus.cardano.node.{Provider, SubmitError}
import scalus.cardano.txbuilder.TestPeer.{Alice, Bob}
import scalus.{plutusV3, toUplc, Compiler}

import java.time.Instant
import scala.collection.immutable.SortedMap

// TODO: can't depend `testkit`, since it'd introduce circular dependency. /
class SimpleMockProvider(initialUtxos: Utxos) extends Provider {
    override def submit(transaction: Transaction): Either[SubmitError, TransactionHash] =
        Right(transaction.id)

    override def findUtxo(input: TransactionInput): Either[RuntimeException, Utxo] =
        initialUtxos
            .get(input)
            .map(output => Utxo(input, output))
            .toRight(new RuntimeException(s"UTXO not found: $input"))

    override def findUtxos(inputs: Set[TransactionInput]): Either[RuntimeException, Utxos] = {
        val found = inputs.flatMap(input => initialUtxos.get(input).map(input -> _)).toMap
        if found.size == inputs.size then Right(found)
        else Left(new RuntimeException(s"Some UTXOs not found"))
    }

    override def findUtxo(
        address: Address,
        transactionId: Option[TransactionHash],
        datum: Option[DatumOption],
        minAmount: Option[Coin]
    ): Either[RuntimeException, Utxo] = {
        initialUtxos
            .find { case (input, output) =>
                output.address == address &&
                transactionId.forall(_ == input.transactionId) &&
                minAmount.forall(min => output.value.coin.value >= min.value)
            }
            .map { case (input, output) => Utxo(input, output) }
            .toRight(new RuntimeException(s"No UTXO found for address $address"))
    }

    override def findUtxos(
        address: Address,
        transactionId: Option[TransactionHash],
        datum: Option[DatumOption],
        minAmount: Option[Coin],
        minRequiredTotalAmount: Option[Coin]
    ): Either[RuntimeException, Utxos] = {
        val filtered = initialUtxos.filter { case (input, output) =>
            output.address == address &&
            transactionId.forall(_ == input.transactionId) &&
            minAmount.forall(min => output.value.coin.value >= min.value)
        }

        minRequiredTotalAmount match {
            case Some(required) =>
                val (selected, total) = filtered.foldLeft(
                  (Map.empty[TransactionInput, TransactionOutput], 0L)
                ) { case ((acc, sum), (input, output)) =>
                    if sum < required.value then
                        (acc + (input -> output), sum + output.value.coin.value)
                    else (acc, sum)
                }
                if total >= required.value then Right(selected)
                else
                    Left(
                      new RuntimeException(
                        s"Insufficient funds: need ${required.value}, found $total"
                      )
                    )
            case None =>
                Right(filtered)
        }
    }
}

class TxBuilderCompleteTest extends AnyFunSuite, ValidatorRulesTestKit {

    val testEnv: CardanoInfo = CardanoInfo.mainnet

    // Common test values
    val genesisHash: TransactionHash =
        TransactionHash.fromByteString(ByteString.fromHex("0" * 64))

    val alwaysOkScript: Script.PlutusV3 = {
        val alwaysOk = Compiler.compileInline((sc: Data) => ())
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

    val emptyRedeemer: Data = Data.List(List.empty)
    val inlineDatum42: DatumOption = Inline(Data.I(42))

    val scriptAddress: ShelleyAddress = ShelleyAddress(
      network = testEnv.network,
      payment = ShelleyPaymentPart.Script(alwaysOkScript.scriptHash),
      delegation = ShelleyDelegationPart.Null
    )

    // Helper methods for creating UTXOs
    def input(index: Int): TransactionInput = TransactionInput(genesisHash, index)

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

    // Create TransactionSigner from TestPeer's keys
    def peerSigner(peer: TestPeer): TransactionSigner = {
        val account = peer.account
        // Ed25519 private key is 32 bytes, but bloxbean returns 64 (32 private + 32 public)
        val privateKeyData = account.hdKeyPair().getPrivateKey.getKeyData
        val privateKey = ByteString.fromArray(privateKeyData.take(32))
        val publicKey = ByteString.fromArray(account.hdKeyPair().getPublicKey.getKeyData)
        TransactionSigner(Set((privateKey, publicKey)))
    }

    lazy val aliceSigner: TransactionSigner = peerSigner(Alice)

    private def outputsOf(peer: TestPeer, tx: Transaction) =
        tx.body.value.outputs.toSeq.filter(_.value.address == peer.address)

    // ============================================================================
    // Basic ADA Transactions
    // ============================================================================

    test("complete should automatically add inputs for simple ADA payment") {
        val provider = SimpleMockProvider(
          Map(
            input(0) -> adaOutput(Alice.address, 100),
            input(1) -> adaOutput(Alice.address, 50)
          )
        )

        val tx = TxBuilder(testEnv)
            .payTo(Bob.address, Value.ada(10))
            .complete(provider, Alice.address)
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
        val provider = SimpleMockProvider(
          Map(input(0) -> adaOutput(Alice.address, 100))
        )

        val signedTx = TxBuilder(testEnv)
            .payTo(Bob.address, Value.ada(10))
            .complete(provider, Alice.address)
            .sign(aliceSigner)
            .transaction

        assert(
          signedTx.witnessSet.vkeyWitnesses.toSeq.nonEmpty,
          "Transaction should have vkey witnesses after signing"
        )
    }

    test("complete followed by sign() should produce a valid transaction") {
        val provider = SimpleMockProvider(
          Map(
            input(0) -> adaOutput(Alice.address, 50),
            input(1) -> adaOutput(Alice.address, 50)
          )
        )

        val signedTx = TxBuilder(testEnv)
            .payTo(Bob.address, Value.ada(20))
            .complete(provider, Alice.address)
            .sign(aliceSigner)
            .transaction

        assert(signedTx.witnessSet.vkeyWitnesses.toSeq.nonEmpty, "Should have vkey witnesses")

        val aliceInputs = signedTx.body.value.inputs.toSeq.filter { i =>
            provider.findUtxo(i).toOption.exists(_.output.address == Alice.address)
        }
        assert(aliceInputs.nonEmpty, "Should have inputs from Alice that were added by complete")
    }

    // ============================================================================
    // Multi-Asset Transactions
    // ============================================================================

    test("complete should handle multi-asset transactions") {
        val provider = SimpleMockProvider(
          Map(
            input(0) -> tokenOutput(Alice.address, 100, policyId -> Map(co2 -> 200L)),
            input(1) -> adaOutput(Alice.address, 50)
          )
        )

        val tx = TxBuilder(testEnv)
            .payTo(Bob.address, Value.fromPolicy(policyId, Map(co2 -> 100L), Coin.ada(5)))
            .complete(provider, Alice.address)
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
        val provider = SimpleMockProvider(
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
        val provider = SimpleMockProvider(
          Map(
            input(0) -> tokenOutput(Alice.address, 1, policyId -> Map(co2 -> 100L)),
            input(1) -> adaOutput(Alice.address, 50)
          )
        )

        val tx = TxBuilder(testEnv)
            .payTo(Bob.address, Value.fromPolicy(policyId, Map(co2 -> 50L)))
            .complete(provider, Alice.address)
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
        val provider = SimpleMockProvider(
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

        val provider = SimpleMockProvider(
          Map(
            input(0) -> tokenOutput(Alice.address, 100, policyId -> Map(co2 -> 200L)),
            explicitUtxo._1 -> explicitUtxo._2
          )
        )

        val tx = TxBuilder(testEnv)
            .spend(Utxo(explicitUtxo))
            .payTo(Bob.address, explicitUtxo._2.value)
            .complete(provider, Alice.address)
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

        val provider = SimpleMockProvider(
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
            .transaction

        assert(tx.body.value.collateralInputs.toSeq.nonEmpty, "Should have collateral inputs")
        assert(outputsOf(Bob, tx).nonEmpty, "Should have output to Bob")
    }

    test("complete should auto-detect and add collateral for minting") {
        val assets = Map(co2 -> 100L)

        val provider = SimpleMockProvider(
          Map(
            input(0) -> adaOutput(Alice.address, 100),
            input(1) -> adaOutput(Alice.address, 10)
          )
        )

        val tx = TxBuilder(testEnv)
            .mintAndAttach(emptyRedeemer, assets, alwaysOkScript)
            .payTo(Bob.address, Value.fromPolicy(policyId, assets, Coin.ada(5)))
            .complete(provider, Alice.address)
            .transaction

        assert(tx.body.value.collateralInputs.toSeq.nonEmpty, "Should have collateral inputs")
        assert(tx.body.value.mint.isDefined, "Transaction should have mint field")

        val bobTokens = outputsOf(Bob, tx).head.value.value.assets.assets.head._2
        assert(bobTokens.head._1 == co2 && bobTokens.head._2 == 100L)
    }

    test("complete should not add collateral for pubkey-only transactions") {
        val provider = SimpleMockProvider(
          Map(input(0) -> adaOutput(Alice.address, 100))
        )

        val tx = TxBuilder(testEnv)
            .payTo(Bob.address, Value.ada(10))
            .complete(provider, Alice.address)
            .transaction

        assert(tx.body.value.collateralInputs.toSeq.isEmpty, "Should not have collateral inputs")
        assert(tx.body.value.collateralReturnOutput.isEmpty, "Should not have collateral return")
        assert(tx.body.value.totalCollateral.isEmpty, "Should not have totalCollateral")
    }

    test("complete should set collateral return output for script transactions") {
        val sUtxo = scriptUtxo(2, 20)

        val provider = SimpleMockProvider(
          Map(
            input(0) -> adaOutput(Alice.address, 100),
            sUtxo.input -> sUtxo.output
          )
        )

        val tx = TxBuilder(testEnv)
            .spend(sUtxo, emptyRedeemer, alwaysOkScript)
            .payTo(Bob.address, Value.ada(5))
            .complete(provider, Alice.address)
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

        val provider = SimpleMockProvider(
          Map(
            input(0) -> adaOutput(Alice.address, 100),
            sUtxo.input -> sUtxo.output
          )
        )

        val tx = TxBuilder(testEnv)
            .spend(sUtxo, emptyRedeemer, alwaysOkScript)
            .payTo(Bob.address, Value.ada(5))
            .complete(provider, Alice.address)
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

        val provider = SimpleMockProvider(
          Map(
            input(0) -> adaOutput(Alice.address, 100),
            sUtxo.input -> sUtxo.output
          )
        )

        val tx = TxBuilder(testEnv)
            .spend(sUtxo, emptyRedeemer, alwaysOkScript)
            .payTo(Bob.address, Value.ada(5))
            .complete(provider, Alice.address)
            .transaction

        val fee = tx.body.value.fee
        val minRequiredCollateral = (fee.value * testEnv.protocolParams.collateralPercentage) / 100

        val collateralValue = tx.body.value.collateralInputs.toSeq
            .flatMap(i => provider.findUtxo(i).toOption.map(_.output.value.coin.value))
            .sum

        assert(
          collateralValue >= minRequiredCollateral,
          s"Collateral ($collateralValue) should be >= required ($minRequiredCollateral)"
        )
    }

    test("complete should handle mixed script and pubkey inputs") {
        val sUtxo = scriptUtxo(2, 10)

        val provider = SimpleMockProvider(
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
            .transaction

        assert(tx.body.value.inputs.toSeq.contains(sUtxo.input))
        assert(tx.body.value.collateralInputs.toSeq.nonEmpty)
    }

    // ============================================================================
    // Minting Scenarios
    // ============================================================================

    test("complete should select inputs for fees when only minting (no explicit inputs)") {
        val provider = SimpleMockProvider(
          Map(
            input(0) -> adaOutput(Alice.address, 100),
            input(1) -> adaOutput(Alice.address, 50)
          )
        )

        val tx = TxBuilder(testEnv)
            .mintAndAttach(emptyRedeemer, Map(token -> 100L), alwaysOkScript)
            .payTo(Bob.address, Value.fromPolicy(policyId, Map(token -> 100L), Coin.ada(5)))
            .complete(provider, Alice.address)
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

        val provider = SimpleMockProvider(
          Map(
            explicitInput -> adaOutput(Alice.address, 10),
            input(1) -> adaOutput(Alice.address, 100)
          )
        )

        val tx = TxBuilder(testEnv)
            .spend(Utxo(explicitInput, adaOutput(Alice.address, 10)))
            .payTo(Bob.address, Value.ada(50))
            .complete(provider, Alice.address)
            .transaction

        assert(tx.body.value.inputs.toSeq.contains(explicitInput))
        assert(tx.body.value.inputs.toSeq.contains(input(1)))
    }

    test("complete with explicit spend should only add necessary additional inputs") {
        val explicitUtxo = genAdaOnlyPubKeyUtxo(Alice, min = 3_000_000).sample.get

        val provider = SimpleMockProvider(
          Map(
            input(0) -> adaOutput(Alice.address, 100),
            explicitUtxo._1 -> explicitUtxo._2
          )
        )

        val tx = TxBuilder(testEnv)
            .spend(Utxo(explicitUtxo))
            .payTo(Bob.address, explicitUtxo._2.value)
            .complete(provider, Alice.address)
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
        val provider = SimpleMockProvider(
          Map(
            input(0) -> tokenOutput(Alice.address, 50, policyId -> Map(co2 -> 100L)),
            input(1) -> adaOutput(Alice.address, 50)
          )
        )

        val tx = TxBuilder(testEnv)
            .payTo(Bob.address, Value.fromPolicy(policyId, Map(co2 -> 100L), Coin.ada(5)))
            .complete(provider, Alice.address)
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
        val provider = SimpleMockProvider(
          Map(input(0) -> adaOutput(Alice.address, 100))
        )

        val exception = intercept[RuntimeException] {
            TxBuilder(testEnv)
                .payTo(
                  Bob.address,
                  Value.fromPolicy(policyId, Map(nonexistent -> 100L), Coin.ada(5))
                )
                .complete(provider, Alice.address)
        }

        assert(
          exception.getMessage.contains("Insufficient tokens") ||
              exception.getMessage.contains("not found"),
          s"Should indicate insufficient tokens, got: ${exception.getMessage}"
        )
    }

    test("complete should handle tokens from same UTXO under different policies") {
        val provider = SimpleMockProvider(
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
        val provider = SimpleMockProvider(
          Map(input(0) -> adaOutput(Alice.address, 100))
        )

        val tx = TxBuilder(testEnv)
            .payTo(Bob.address, Value.ada(98))
            .complete(provider, Alice.address)
            .transaction

        val aliceOutputs = outputsOf(Alice, tx)
        if aliceOutputs.nonEmpty then {
            val changeAda = aliceOutputs.head.value.value.coin.value
            val minAda = testEnv.protocolParams.utxoCostPerByte * 160 // rough estimate
            assert(changeAda >= minAda, s"Change output ($changeAda) should meet min ADA ($minAda)")
        }
    }

    test("complete should correctly place change output at sponsor address") {
        val provider = SimpleMockProvider(
          Map(input(0) -> adaOutput(Alice.address, 100))
        )

        val tx = TxBuilder(testEnv)
            .payTo(Bob.address, Value.ada(10))
            .complete(provider, Alice.address)
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
        val provider = SimpleMockProvider(
          Map(input(0) -> adaOutput(Alice.address, 100))
        )

        val tx = TxBuilder(testEnv)
            .validFrom(Instant.ofEpochSecond(1000))
            .validTo(Instant.ofEpochSecond(2000))
            .payTo(Bob.address, Value.ada(10))
            .complete(provider, Alice.address)
            .transaction

        assert(tx.body.value.validityStartSlot.isDefined, "Should preserve validFrom")
        assert(tx.body.value.ttl.isDefined, "Should preserve validTo (TTL)")
    }

    // ============================================================================
    // Error Handling
    // ============================================================================

    test("complete should fail with insufficient funds") {
        val provider = SimpleMockProvider(
          Map(input(0) -> adaOutput(Alice.address, 5))
        )

        val exception = intercept[RuntimeException] {
            TxBuilder(testEnv)
                .payTo(Bob.address, Value.ada(100))
                .complete(provider, Alice.address)
        }

        assert(
          exception.getMessage.contains("Utxos not found") ||
              exception.getMessage.contains("Insufficient"),
          s"Should indicate insufficient funds, got: ${exception.getMessage}"
        )
    }

    test("complete should handle empty UTXO set from provider") {
        val provider = SimpleMockProvider(Map.empty)

        val exception = intercept[RuntimeException] {
            TxBuilder(testEnv)
                .payTo(Bob.address, Value.ada(10))
                .complete(provider, Alice.address)
        }

        assert(
          exception.getMessage.contains("Insufficient") ||
              exception.getMessage.contains("not found") ||
              exception.getMessage.contains("Utxos not found"),
          s"Should indicate no UTXOs available, got: ${exception.getMessage}"
        )
    }

    test("complete should fail when outputs exceed possible input value") {
        val provider = SimpleMockProvider(
          Map(input(0) -> adaOutput(Alice.address, 10))
        )

        val exception = intercept[RuntimeException] {
            TxBuilder(testEnv)
                .payTo(Bob.address, Value.ada(1000))
                .complete(provider, Alice.address)
        }

        assert(
          exception.getMessage.contains("Insufficient") ||
              exception.getMessage.contains("not found"),
          s"Should indicate insufficient funds, got: ${exception.getMessage}"
        )
    }

    // ============================================================================
    // Integration Tests
    // ============================================================================

    test("complete should produce deterministic results") {
        val provider = SimpleMockProvider(
          Map(
            input(0) -> adaOutput(Alice.address, 100),
            input(1) -> adaOutput(Alice.address, 50)
          )
        )

        val tx1 = TxBuilder(testEnv)
            .payTo(Bob.address, Value.ada(10))
            .complete(provider, Alice.address)
            .transaction

        val tx2 = TxBuilder(testEnv)
            .payTo(Bob.address, Value.ada(10))
            .complete(provider, Alice.address)
            .transaction

        assert(tx1.body.value.inputs == tx2.body.value.inputs, "Inputs should be deterministic")
        assert(tx1.body.value.outputs == tx2.body.value.outputs, "Outputs should be deterministic")
        assert(tx1.body.value.fee == tx2.body.value.fee, "Fee should be deterministic")
    }

}
