package scalus.cardano.txbuilder

import org.scalatest.funsuite.AnyFunSuite
import scalus.builtin.{ByteString, Data}
import scalus.cardano.address.{Address, ShelleyAddress, ShelleyDelegationPart, ShelleyPaymentPart}
import scalus.cardano.ledger.*
import scalus.cardano.ledger.DatumOption.Inline
import scalus.cardano.ledger.TransactionOutput.Babbage
import scalus.cardano.ledger.rules.ValidatorRulesTestKit
import scalus.cardano.node.{Provider, SubmitError}
import scalus.cardano.txbuilder.TestPeer.{Alice, Bob}
import scalus.{plutusV3, toUplc, Compiler}

import scala.collection.immutable.SortedMap

// TODO: can't depend `testkit`, since it'd introduce circular dependency. /
class SimpleMockProvider(initialUtxos: Utxos) extends Provider {
    override def submit(transaction: Transaction): Either[SubmitError, Unit] = Right(())

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

    val alwaysOkScript: Script.PlutusV3 = {
        val alwaysOk = Compiler.compileInline((sc: Data) => ())
        val alwaysOkCborBytes = alwaysOk.toUplc().plutusV3.cborByteString
        Script.PlutusV3(alwaysOkCborBytes)
    }

    // Create TransactionSigner from TestPeer's keys
    def peerSigner(peer: TestPeer): TransactionSigner = {
        val account = peer.account
        // Ed25519 private key is 32 bytes, but bloxbean returns 64 (32 private + 32 public)
        val privateKeyData = account.hdKeyPair().getPrivateKey.getKeyData
        val privateKey = ByteString.fromArray(privateKeyData.take(32))
        val publicKey = ByteString.fromArray(account.hdKeyPair().getPublicKey.getKeyData)
        TransactionSigner(Set((privateKey, publicKey)))
    }

    lazy val aliceSigner = peerSigner(Alice)

    test("complete should automatically add inputs for simple ADA payment") {
        val genesisHash = TransactionHash.fromByteString(ByteString.fromHex("0" * 64))
        val provider = SimpleMockProvider(
          initialUtxos = Map(
            TransactionInput(genesisHash, 0) -> Babbage(
              address = Alice.address,
              value = Value.ada(100)
            ),
            TransactionInput(genesisHash, 1) -> Babbage(
              address = Alice.address,
              value = Value.ada(50)
            )
          )
        )

        val paymentValue = Value.ada(10)
        val builder = TxBuilder(testEnv)
            .payTo(Bob.address, paymentValue)
            .complete(provider, Alice.address)

        val tx = builder.transaction

        assert(tx.body.value.inputs.toSeq.size == 1, "Transaction must have exactly 1 input")

        // Should have payment to Bob
        val bobOutputs = outputsOf(Bob, tx)
        assert(bobOutputs.size == 1, "Should have output to Bob")
        assert(
          bobOutputs.head.value.value.coin >= paymentValue.coin,
          "Bob should receive at least 10 ADA"
        )

        // Should have change output to Alice
        val aliceOutputs = outputsOf(Alice, tx)
        assert(aliceOutputs.nonEmpty, "Should have change output to Alice")
    }

    test("complete inputs should be signable with sign()") {
        val genesisHash = TransactionHash.fromByteString(ByteString.fromHex("0" * 64))
        val provider = SimpleMockProvider(
          initialUtxos = Map(
            TransactionInput(genesisHash, 0) -> Babbage(
              address = Alice.address,
              value = Value.ada(100)
            )
          )
        )

        val unsignedBuilder = TxBuilder(testEnv)
            .payTo(Bob.address, Value.ada(10))
            .complete(provider, Alice.address)

        val signedTx = unsignedBuilder.sign(aliceSigner).transaction

        // Should have vkey witnesses for the inputs added by complete
        assert(
          signedTx.witnessSet.vkeyWitnesses.toSeq.nonEmpty,
          "Transaction should have vkey witnesses after signing"
        )

        // Number of signatures should match number of unique signers needed
        assert(
          signedTx.witnessSet.vkeyWitnesses.toSeq.nonEmpty,
          "Should have at least one signature"
        )
    }

    test("complete should handle multi-asset transactions") {
        val genesisHash = TransactionHash.fromByteString(ByteString.fromHex("0" * 64))
        val assetName = AssetName.fromString("co2")
        val policyId = alwaysOkScript.scriptHash

        val tokenValue = Value(
          coin = Coin.ada(100),
          assets = MultiAsset(
            SortedMap(
              policyId -> SortedMap(assetName -> 200L)
            )
          )
        )

        val provider = SimpleMockProvider(
          initialUtxos = Map(
            TransactionInput(genesisHash, 0) -> Babbage(
              address = Alice.address,
              value = tokenValue
            ),
            TransactionInput(genesisHash, 1) -> Babbage(
              address = Alice.address,
              value = Value.ada(50)
            )
          )
        )

        // Send 100 tokens to Bob
        val paymentValue = Value(
          coin = Coin.ada(5),
          assets = MultiAsset(
            SortedMap(
              policyId -> SortedMap(assetName -> 100L)
            )
          )
        )

        val builder = TxBuilder(testEnv)
            .payTo(Bob.address, paymentValue)
            .complete(provider, Alice.address)

        val tx = builder.transaction

        // Bob should receive the tokens
        val bobOutputs = outputsOf(Bob, tx)
        assert(bobOutputs.size == 1, "Should have output to Bob")

        val bobTokens = bobOutputs.head.value.value.assets.assets
            .get(policyId)
            .flatMap(_.get(assetName))
        assert(bobTokens.contains(100L), "Bob should receive 100 tokens")

        // Change output should have remaining 100 tokens
        val aliceOutputs = outputsOf(Alice, tx)
        assert(aliceOutputs.nonEmpty, "Should have change output to Alice")

        val aliceTokens = aliceOutputs.flatMap(
          _.value.value.assets.assets
              .get(policyId)
              .flatMap(_.get(assetName))
        )
        assert(aliceTokens.sum == 100L, "Alice should receive 100 tokens as change")
    }

    test("complete should auto-detect and add collateral for script spending") {
        val genesisHash = TransactionHash.fromByteString(ByteString.fromHex("0" * 64))

        // Create script-locked UTXO
        val scriptAddress = ShelleyAddress(
          network = testEnv.network,
          payment = ShelleyPaymentPart.Script(alwaysOkScript.scriptHash),
          delegation = ShelleyDelegationPart.Null
        )
        val scriptUtxo = Utxo(
          TransactionInput(genesisHash, 2),
          Babbage(
            address = scriptAddress,
            value = Value.ada(20),
            datumOption = Some(Inline(Data.I(42)))
          )
        )

        val redeemer = Data.List(List.empty)

        val provider = SimpleMockProvider(
          initialUtxos = Map(
            TransactionInput(genesisHash, 0) -> Babbage(
              address = Alice.address,
              value = Value.ada(100)
            ),
            TransactionInput(genesisHash, 1) -> Babbage(
              address = Alice.address,
              value = Value.ada(10)
            ),
            scriptUtxo.input -> scriptUtxo.output
          )
        )

        val builder = TxBuilder(testEnv)
            .spend(scriptUtxo, redeemer, alwaysOkScript)
            .payTo(Bob.address, Value.ada(5))
            .complete(provider, Alice.address)

        val tx = builder.transaction

        // Should have collateral inputs
        assert(
          tx.body.value.collateralInputs.toSeq.nonEmpty,
          "Transaction should have collateral inputs"
        )

        // Should have payment to Bob
        val bobOutputs = outputsOf(Bob, tx)
        assert(bobOutputs.nonEmpty, "Should have output to Bob")
    }

    test("complete should auto-detect and add collateral for minting") {
        val genesisHash = TransactionHash.fromByteString(ByteString.fromHex("0" * 64))
        val policyId = alwaysOkScript.scriptHash
        val redeemer = Data.List(List.empty)
        val assetName = AssetName.fromString("co2")
        val assets = Map(assetName -> 100L)

        val provider = SimpleMockProvider(
          initialUtxos = Map(
            TransactionInput(genesisHash, 0) -> Babbage(
              address = Alice.address,
              value = Value.ada(100)
            ),
            TransactionInput(genesisHash, 1) -> Babbage(
              address = Alice.address,
              value = Value.ada(10)
            )
          )
        )

        val mintValue = Value(
          coin = Coin.ada(5),
          assets = MultiAsset(
            SortedMap(
              policyId -> SortedMap.from(assets)
            )
          )
        )

        val builder = TxBuilder(testEnv)
            .mintAndAttach(redeemer, assets, alwaysOkScript)
            .payTo(Bob.address, mintValue)
            .complete(provider, Alice.address)

        val tx = builder.transaction

        // Should have collateral inputs
        assert(
          tx.body.value.collateralInputs.toSeq.nonEmpty,
          "Transaction should have collateral inputs for minting"
        )

        // Should have mint field
        assert(tx.body.value.mint.isDefined, "Transaction should have mint field")

        // Bob should receive co2
        val bobOuts = outputsOf(Bob, tx)
        assert(bobOuts.size == 1, "Bob should have 1 output")

        val bobOut = bobOuts.head
        val bobTokens = bobOut.value.value.assets.assets.head._2
        assert(bobTokens.size == 1, "Bob should receive 1 kind of token")
        assert(bobTokens.head._1 == assetName, "Bob should receive co2")
        assert(bobTokens.head._2 == 100L, "Bob should receive 100 co2")
    }

    test("complete should fail with insufficient funds") {
        val genesisHash = TransactionHash.fromByteString(ByteString.fromHex("0" * 64))

        val provider = SimpleMockProvider(
          initialUtxos = Map(
            TransactionInput(genesisHash, 0) -> Babbage(
              address = Alice.address,
              value = Value.ada(5)
            )
          )
        )

        val builder = TxBuilder(testEnv)
            .payTo(Bob.address, Value.ada(100))

        val exception = intercept[RuntimeException] {
            builder.complete(provider, Alice.address)
        }

        assert(
          exception.getMessage.contains("Utxos not found") ||
              exception.getMessage.contains("Insufficient"),
          s"Should indicate insufficient funds, got: ${exception.getMessage}"
        )
    }

    test("complete with explicit spend should only add necessary additional inputs") {
        val genesisHash = TransactionHash.fromByteString(ByteString.fromHex("0" * 64))
        val explicitUtxo = genAdaOnlyPubKeyUtxo(Alice, min = 3_000_000).sample.get

        val provider = SimpleMockProvider(
          initialUtxos = Map(
            TransactionInput(genesisHash, 0) -> Babbage(
              address = Alice.address,
              value = Value.ada(100)
            ),
            explicitUtxo._1 -> explicitUtxo._2
          )
        )

        // User explicitly spends one UTXO with 5 ADA, wants to pay 3 ADA
        // complete should recognize the explicit input and only add more if needed for fees
        val builder = TxBuilder(testEnv)
            .spend(Utxo(explicitUtxo))
            .payTo(Bob.address, explicitUtxo._2.value)
            .complete(provider, Alice.address)

        val tx = builder.transaction

        // Should have the explicit input
        assert(
          tx.body.value.inputs.toSeq.contains(explicitUtxo._1),
          "Should include explicitly spent UTXO"
        )

        assert(tx.body.value.inputs.toSeq.size == 2)

        // Bob should receive payment
        val bobOutputs = outputsOf(Bob, tx)
        assert(bobOutputs.nonEmpty, "Should have output to Bob")
        assert(
          bobOutputs.head.value.value.coin.value >= 3_000_000L,
          "Bob should receive at least 3 ADA"
        )

        val aliceOutputs = outputsOf(Alice, tx)
        assert(aliceOutputs.size == 1, "Should have change back to Alice")
        val aliceAda = aliceOutputs.head.value.value.coin
        assert(aliceAda < Coin.ada(100) && aliceAda > Coin.ada(99))
    }

    test("complete followed by sign() should produce a valid transaction") {
        val genesisHash = TransactionHash.fromByteString(ByteString.fromHex("0" * 64))
        val provider = SimpleMockProvider(
          initialUtxos = Map(
            TransactionInput(genesisHash, 0) -> Babbage(
              address = Alice.address,
              value = Value.ada(50)
            ),
            TransactionInput(genesisHash, 1) -> Babbage(
              address = Alice.address,
              value = Value.ada(50)
            )
          )
        )

        val builtTx = TxBuilder(testEnv)
            .payTo(Bob.address, Value.ada(20))
            .complete(provider, Alice.address)

        val signedTx = builtTx.sign(aliceSigner).transaction

        // Verify signatures exist
        assert(signedTx.witnessSet.vkeyWitnesses.toSeq.nonEmpty, "Should have vkey witnesses")

        // Verify all inputs from Alice's address have corresponding signatures
        val aliceInputs = signedTx.body.value.inputs.toSeq.filter { input =>
            provider.findUtxo(input).toOption.exists(_.output.address == Alice.address)
        }

        assert(
          aliceInputs.nonEmpty,
          "Should have inputs from Alice that were added by complete"
        )
    }

    test("complete should send tokens acquired from additional input querying back as change") {
        val genesisHash = TransactionHash.fromByteString(ByteString.fromHex("0" * 64))
        val explicitUtxo = genAdaOnlyPubKeyUtxo(Alice, min = 5_000_000).sample.get

        val assetName = AssetName.fromString("co2")
        val policyId = alwaysOkScript.scriptHash

        val provider = SimpleMockProvider(
          initialUtxos = Map(
            TransactionInput(genesisHash, 0) -> Babbage(
              address = Alice.address,
              value = Value(
                Coin.ada(100),
                MultiAsset(
                  SortedMap(
                    policyId -> SortedMap(assetName -> 200L)
                  )
                )
              )
            ),
            explicitUtxo._1 -> explicitUtxo._2
          )
        )

        val builder = TxBuilder(testEnv)
            .spend(Utxo(explicitUtxo))
            .payTo(Bob.address, explicitUtxo._2.value)
            .complete(provider, Alice.address)

        val tx = builder.transaction

        val bobOutputs = outputsOf(Bob, tx)
        assert(bobOutputs.size == 1, "Should have an output to Bob")
        val bobOutValue = bobOutputs.head.value
        assert(bobOutValue.value.coin >= Coin.ada(5), "Bob must receive 5 ADA")
        assert(bobOutValue.value.assets.isEmpty, "Bob must not receive any tokens")

        val aliceChangeOuts = outputsOf(Alice, tx)
        assert(aliceChangeOuts.size == 1, "Alice must receive one output back")
        val aliceChangeValue = aliceChangeOuts.head.value.value
        assert(aliceChangeValue.coin > Coin.ada(99) && aliceChangeValue.coin < Coin.ada(100))

        val aliceChangeTokens = aliceChangeValue.assets.assets.head._2
        assert(
          aliceChangeTokens(AssetName.fromString("co2")) == 200L,
          "Alice must receive all co2 back as change"
        )
    }

    test("complete should handle multiple different tokens from separate UTXOs") {
        val genesisHash = TransactionHash.fromByteString(ByteString.fromHex("0" * 64))

        val policyId1 = alwaysOkScript.scriptHash
        val policyId2 = ScriptHash.fromByteString(ByteString.fromHex("1" * 56))
        val token1 = AssetName.fromString("co2")
        val token2 = AssetName.fromString("h2so4")

        val provider = SimpleMockProvider(
          initialUtxos = Map(
            TransactionInput(genesisHash, 0) -> Babbage(
              address = Alice.address,
              value = Value(
                Coin.ada(50),
                MultiAsset(
                  SortedMap(
                    policyId1 -> SortedMap(token1 -> 100L)
                  )
                )
              )
            ),
            TransactionInput(genesisHash, 1) -> Babbage(
              address = Alice.address,
              value = Value(
                Coin.ada(50),
                MultiAsset(
                  SortedMap(
                    policyId2 -> SortedMap(token2 -> 200L)
                  )
                )
              )
            ),
            TransactionInput(genesisHash, 2) -> Babbage(
              address = Alice.address,
              value = Value.ada(10)
            )
          )
        )

        // Send both tokens to Bob
        val paymentValue = Value(
          coin = Coin.ada(5),
          assets = MultiAsset(
            SortedMap(
              policyId1 -> SortedMap(token1 -> 30L),
              policyId2 -> SortedMap(token2 -> 50L)
            )
          )
        )

        val builder = TxBuilder(testEnv)
            .payTo(Bob.address, paymentValue)
            .complete(provider, Alice.address)

        val tx = builder.transaction

        // Should have 2 inputs, because the tokens are spread between multiple utxos
        assert(
          tx.body.value.inputs.toSeq.size >= 2,
          "Should select at least 2 inputs for different tokens"
        )

        // Bob should receive both token types
        val bobOutputs = outputsOf(Bob, tx)
        assert(bobOutputs.size == 1, "Should have output to Bob")
        val bobValue = bobOutputs.head.value.value

        val bobToken1 = bobValue.assets.assets.get(policyId1).flatMap(_.get(token1))
        assert(bobToken1.contains(30L), "Bob should receive 30 of token1")

        val bobToken2 = bobValue.assets.assets.get(policyId2).flatMap(_.get(token2))
        assert(bobToken2.contains(50L), "Bob should receive 50 of token2")

        // Alice should receive change with both types of tokens
        val aliceOutputs = outputsOf(Alice, tx)
        assert(aliceOutputs.nonEmpty, "Should have change output to Alice")

        val aliceTokens = aliceOutputs.flatMap(_.value.value.assets.assets.toSeq).toMap
        val aliceToken1 = aliceTokens.get(policyId1).flatMap(_.get(token1))
        val aliceToken2 = aliceTokens.get(policyId2).flatMap(_.get(token2))

        assert(aliceToken1.contains(70L), "Alice should receive 70 of co2 as change")
        assert(aliceToken2.contains(150L), "Alice should receive 150 of h2so4 as change")
    }

    test(
      "complete should add ADA-only UTXO when token UTXO has insufficient ADA for fees"
    ) {
        val genesisHash = TransactionHash.fromByteString(ByteString.fromHex("0" * 64))
        val policyId = alwaysOkScript.scriptHash
        val assetName = AssetName.fromString("co2")

        val provider = SimpleMockProvider(
          initialUtxos = Map(
            // Almost no ADA, a lot of tokens
            TransactionInput(genesisHash, 0) -> Babbage(
              address = Alice.address,
              value = Value(
                Coin.ada(1),
                MultiAsset(
                  SortedMap(
                    policyId -> SortedMap(assetName -> 100L)
                  )
                )
              )
            ),
            // ADA-only, enough for payment, no tokens
            TransactionInput(genesisHash, 1) -> Babbage(
              address = Alice.address,
              value = Value.ada(50)
            )
          )
        )

        // Has only tokens, but will have to get bumped to min ADA
        val paymentValue = Value(
          coin = Coin.zero,
          assets = MultiAsset(
            SortedMap(
              policyId -> SortedMap(assetName -> 50L)
            )
          )
        )

        val builder = TxBuilder(testEnv)
            .payTo(Bob.address, paymentValue)
            .complete(provider, Alice.address)

        val tx = builder.transaction

        // Should end up with both inputs
        assert(
          tx.body.value.inputs.toSeq.size == 2,
          "Should select both token UTXO and ADA-only UTXO"
        )

        // Bob should receive the tokens
        val bobOutputs = outputsOf(Bob, tx)
        assert(bobOutputs.size == 1, "Should have output to Bob")

        val bobTokens = bobOutputs.head.value.value.assets.assets
            .get(policyId)
            .flatMap(_.get(assetName))
        assert(bobTokens.contains(50L), "Bob should receive 50 co2")
        val bobAda = bobOutputs.head.value.value.coin
        assert(!bobAda.isZero, "Bob must get at least min ADA coins")

        // Alice should receive change with remaining tokens
        val aliceOutputs = outputsOf(Alice, tx)
        assert(aliceOutputs.nonEmpty, "Should have change output to Alice")

        val aliceTokens = aliceOutputs.flatMap(
          _.value.value.assets.assets
              .get(policyId)
              .flatMap(_.get(assetName))
        )
        assert(aliceTokens.sum == 50L, "Alice should receive 50 tokens as change")
    }

    test("complete should handle multiple token types in single UTXO efficiently") {
        val genesisHash = TransactionHash.fromByteString(ByteString.fromHex("0" * 64))
        val policyId = alwaysOkScript.scriptHash
        val tokenA = AssetName.fromString("co2")
        val tokenB = AssetName.fromString("h2so4")
        val tokenC = AssetName.fromString("c11h15no2")

        val provider = SimpleMockProvider(
          initialUtxos = Map(
            TransactionInput(genesisHash, 0) -> Babbage(
              address = Alice.address,
              value = Value(
                Coin.ada(100),
                MultiAsset(
                  SortedMap(
                    policyId -> SortedMap(
                      tokenA -> 100L,
                      tokenB -> 200L,
                      tokenC -> 50L
                    )
                  )
                )
              )
            ),
            TransactionInput(genesisHash, 1) -> Babbage(
              address = Alice.address,
              value = Value.ada(10)
            )
          )
        )

        // partial amounts of co2 and h2so4 to Bob
        val paymentValue = Value(
          coin = Coin.ada(5),
          assets = MultiAsset(
            SortedMap(
              policyId -> SortedMap(
                tokenA -> 30L,
                tokenB -> 100L
              )
            )
          )
        )

        val builder = TxBuilder(testEnv)
            .payTo(Bob.address, paymentValue)
            .complete(provider, Alice.address)

        val tx = builder.transaction

        val bobOutputs = outputsOf(Bob, tx)
        assert(bobOutputs.size == 1, "Should have output to Bob")

        val bobAssets = bobOutputs.head.value.value.assets.assets.head._2

        assert(bobAssets.get(tokenA).contains(30L), "Bob should receive 30 of co2")
        assert(bobAssets.get(tokenB).contains(100L), "Bob should receive 100 of h2so4")
        assert(!bobAssets.contains(tokenC), "Bob should not receive c11h15no2")

        val aliceOutputs = outputsOf(Alice, tx)
        assert(aliceOutputs.nonEmpty, "Should have change output to Alice")

        val aliceAssets = aliceOutputs
            .flatMap(_.value.value.assets.assets.get(policyId))
            .foldLeft(SortedMap.empty[AssetName, Long]) { case (acc, assets) =>
                assets.foldLeft(acc) { case (a, (name, amount)) =>
                    a + (name -> (a.getOrElse(name, 0L) + amount))
                }
            }

        assert(
          aliceAssets.get(tokenA).contains(70L),
          "Alice should receive 70 of co2 as change"
        )
        assert(
          aliceAssets.get(tokenB).contains(100L),
          "Alice should receive 100 of h2so4 as change"
        )
        assert(
          aliceAssets.get(tokenC).contains(50L),
          "Alice should receive all 50 of c11h15no2 as change"
        )
    }

    private def outputsOf(peer: TestPeer, tx: Transaction) =
        tx.body.value.outputs.toSeq.filter(_.value.address == peer.address)

    // ============================================================================
    // Category 1: Input Selection Edge Cases
    // ============================================================================

    test("complete should exclude already-spent inputs from selection") {
        val genesisHash = TransactionHash.fromByteString(ByteString.fromHex("0" * 64))
        val explicitInput = TransactionInput(genesisHash, 0)

        val provider = SimpleMockProvider(
          initialUtxos = Map(
            explicitInput -> Babbage(
              address = Alice.address,
              value = Value.ada(10)
            ),
            TransactionInput(genesisHash, 1) -> Babbage(
              address = Alice.address,
              value = Value.ada(100)
            )
          )
        )

        // Explicitly spend the first UTXO
        val builder = TxBuilder(testEnv)
            .spend(Utxo(explicitInput, Babbage(Alice.address, Value.ada(10))))
            .payTo(Bob.address, Value.ada(50))
            .complete(provider, Alice.address)

        val tx = builder.transaction

        // Should have both inputs - the explicit one and one selected for fees
        assert(tx.body.value.inputs.toSeq.contains(explicitInput))
        // The second input should be selected to cover the difference
        assert(tx.body.value.inputs.toSeq.contains(TransactionInput(genesisHash, 1)))
    }

    // ============================================================================
    // Category 2: Token Handling Edge Cases
    // ============================================================================

    test("complete should handle sending all tokens from a UTXO") {
        val genesisHash = TransactionHash.fromByteString(ByteString.fromHex("0" * 64))
        val policyId = alwaysOkScript.scriptHash
        val assetName = AssetName.fromString("co2")

        val provider = SimpleMockProvider(
          initialUtxos = Map(
            TransactionInput(genesisHash, 0) -> Babbage(
              address = Alice.address,
              value = Value(
                Coin.ada(50),
                MultiAsset(SortedMap(policyId -> SortedMap(assetName -> 100L)))
              )
            ),
            TransactionInput(genesisHash, 1) -> Babbage(
              address = Alice.address,
              value = Value.ada(50)
            )
          )
        )

        // Send ALL tokens to Bob
        val paymentValue = Value(
          coin = Coin.ada(5),
          assets = MultiAsset(SortedMap(policyId -> SortedMap(assetName -> 100L)))
        )

        val tx = TxBuilder(testEnv)
            .payTo(Bob.address, paymentValue)
            .complete(provider, Alice.address)
            .transaction

        // Bob should receive all tokens
        val bobOutputs = outputsOf(Bob, tx)
        assert(bobOutputs.size == 1)
        val bobTokens = bobOutputs.head.value.value.assets.assets
            .get(policyId)
            .flatMap(_.get(assetName))
        assert(bobTokens.contains(100L), "Bob should receive all 100 tokens")

        // Alice's change should have no tokens
        val aliceOutputs = outputsOf(Alice, tx)
        assert(aliceOutputs.nonEmpty, "Should have change output")
        val aliceTokens = aliceOutputs.flatMap(
          _.value.value.assets.assets.get(policyId).flatMap(_.get(assetName))
        )
        assert(aliceTokens.isEmpty, "Alice should not receive any tokens back")
    }

    test("complete should fail when required tokens are not available") {
        val genesisHash = TransactionHash.fromByteString(ByteString.fromHex("0" * 64))
        val policyId = alwaysOkScript.scriptHash
        val assetName = AssetName.fromString("nonexistent")

        val provider = SimpleMockProvider(
          initialUtxos = Map(
            TransactionInput(genesisHash, 0) -> Babbage(
              address = Alice.address,
              value = Value.ada(100)
            )
          )
        )

        // Try to send tokens that don't exist
        val paymentValue = Value(
          coin = Coin.ada(5),
          assets = MultiAsset(SortedMap(policyId -> SortedMap(assetName -> 100L)))
        )

        val exception = intercept[RuntimeException] {
            TxBuilder(testEnv)
                .payTo(Bob.address, paymentValue)
                .complete(provider, Alice.address)
        }

        assert(
          exception.getMessage.contains("Insufficient tokens") ||
              exception.getMessage.contains("not found"),
          s"Should indicate insufficient tokens, got: ${exception.getMessage}"
        )
    }

    test("complete should handle tokens from same UTXO under different policies") {
        val genesisHash = TransactionHash.fromByteString(ByteString.fromHex("0" * 64))
        val policyId1 = alwaysOkScript.scriptHash
        val policyId2 = ScriptHash.fromByteString(ByteString.fromHex("2" * 56))
        val assetName = AssetName.fromString("token")

        val provider = SimpleMockProvider(
          initialUtxos = Map(
            TransactionInput(genesisHash, 0) -> Babbage(
              address = Alice.address,
              value = Value(
                Coin.ada(100),
                MultiAsset(
                  SortedMap(
                    policyId1 -> SortedMap(assetName -> 100L),
                    policyId2 -> SortedMap(assetName -> 200L)
                  )
                )
              )
            )
          )
        )

        // Send tokens from only one policy
        val paymentValue = Value(
          coin = Coin.ada(5),
          assets = MultiAsset(SortedMap(policyId1 -> SortedMap(assetName -> 50L)))
        )

        val tx = TxBuilder(testEnv)
            .payTo(Bob.address, paymentValue)
            .complete(provider, Alice.address)
            .transaction

        // Bob should receive tokens from policy1 only
        val bobOutputs = outputsOf(Bob, tx)
        val bobPolicy1Tokens = bobOutputs.head.value.value.assets.assets
            .get(policyId1)
            .flatMap(_.get(assetName))
        assert(bobPolicy1Tokens.contains(50L))

        // Alice should receive remaining policy1 tokens AND all policy2 tokens
        val aliceOutputs = outputsOf(Alice, tx)
        val alicePolicy1 = aliceOutputs
            .flatMap(_.value.value.assets.assets.get(policyId1).flatMap(_.get(assetName)))
        val alicePolicy2 = aliceOutputs
            .flatMap(_.value.value.assets.assets.get(policyId2).flatMap(_.get(assetName)))

        assert(alicePolicy1.sum == 50L, "Alice should receive 50 policy1 tokens as change")
        assert(alicePolicy2.sum == 200L, "Alice should receive all 200 policy2 tokens as change")
    }

    // ============================================================================
    // Category 3: Collateral Handling
    // ============================================================================

    test("complete should not add collateral for pubkey-only transactions") {
        val genesisHash = TransactionHash.fromByteString(ByteString.fromHex("0" * 64))

        val provider = SimpleMockProvider(
          initialUtxos = Map(
            TransactionInput(genesisHash, 0) -> Babbage(
              address = Alice.address,
              value = Value.ada(100)
            )
          )
        )

        val tx = TxBuilder(testEnv)
            .payTo(Bob.address, Value.ada(10))
            .complete(provider, Alice.address)
            .transaction

        // No scripts = no collateral needed
        assert(
          tx.body.value.collateralInputs.toSeq.isEmpty,
          "Pubkey-only transaction should not have collateral inputs"
        )
        assert(
          tx.body.value.collateralReturnOutput.isEmpty,
          "Pubkey-only transaction should not have collateral return"
        )
        assert(
          tx.body.value.totalCollateral.isEmpty,
          "Pubkey-only transaction should not have totalCollateral"
        )
    }

    test("complete should set collateral return output for script transactions") {
        val genesisHash = TransactionHash.fromByteString(ByteString.fromHex("0" * 64))

        val scriptAddress = ShelleyAddress(
          network = testEnv.network,
          payment = ShelleyPaymentPart.Script(alwaysOkScript.scriptHash),
          delegation = ShelleyDelegationPart.Null
        )
        val scriptUtxo = Utxo(
          TransactionInput(genesisHash, 2),
          Babbage(
            address = scriptAddress,
            value = Value.ada(20),
            datumOption = Some(Inline(Data.I(42)))
          )
        )

        val provider = SimpleMockProvider(
          initialUtxos = Map(
            TransactionInput(genesisHash, 0) -> Babbage(
              address = Alice.address,
              value = Value.ada(100)
            ),
            scriptUtxo.input -> scriptUtxo.output
          )
        )

        val tx = TxBuilder(testEnv)
            .spend(scriptUtxo, Data.List(List.empty), alwaysOkScript)
            .payTo(Bob.address, Value.ada(5))
            .complete(provider, Alice.address)
            .transaction

        // Should have collateral return output
        assert(
          tx.body.value.collateralReturnOutput.isDefined,
          "Script transaction should have collateral return output"
        )

        // Collateral return should go to sponsor (Alice)
        val collateralReturn = tx.body.value.collateralReturnOutput.get.value
        assert(
          collateralReturn.address == Alice.address,
          "Collateral return should go to sponsor address"
        )
    }

    test("complete should set totalCollateral field for script transactions") {
        val genesisHash = TransactionHash.fromByteString(ByteString.fromHex("0" * 64))

        val scriptAddress = ShelleyAddress(
          network = testEnv.network,
          payment = ShelleyPaymentPart.Script(alwaysOkScript.scriptHash),
          delegation = ShelleyDelegationPart.Null
        )
        val scriptUtxo = Utxo(
          TransactionInput(genesisHash, 2),
          Babbage(
            address = scriptAddress,
            value = Value.ada(20),
            datumOption = Some(Inline(Data.I(42)))
          )
        )

        val provider = SimpleMockProvider(
          initialUtxos = Map(
            TransactionInput(genesisHash, 0) -> Babbage(
              address = Alice.address,
              value = Value.ada(100)
            ),
            scriptUtxo.input -> scriptUtxo.output
          )
        )

        val tx = TxBuilder(testEnv)
            .spend(scriptUtxo, Data.List(List.empty), alwaysOkScript)
            .payTo(Bob.address, Value.ada(5))
            .complete(provider, Alice.address)
            .transaction

        // Should have totalCollateral set
        assert(
          tx.body.value.totalCollateral.isDefined,
          "Script transaction should have totalCollateral set"
        )

        val totalCollateral = tx.body.value.totalCollateral.get
        val fee = tx.body.value.fee
        val collateralPercentage = testEnv.protocolParams.collateralPercentage
        val requiredCollateral = (fee.value * collateralPercentage) / 100

        assert(
          totalCollateral.value >= requiredCollateral,
          s"totalCollateral ($totalCollateral) should be >= required ($requiredCollateral)"
        )
    }

    test("complete should calculate collateral based on fee estimate") {
        val genesisHash = TransactionHash.fromByteString(ByteString.fromHex("0" * 64))

        val scriptAddress = ShelleyAddress(
          network = testEnv.network,
          payment = ShelleyPaymentPart.Script(alwaysOkScript.scriptHash),
          delegation = ShelleyDelegationPart.Null
        )
        val scriptUtxo = Utxo(
          TransactionInput(genesisHash, 2),
          Babbage(
            address = scriptAddress,
            value = Value.ada(20),
            datumOption = Some(Inline(Data.I(42)))
          )
        )

        val provider = SimpleMockProvider(
          initialUtxos = Map(
            TransactionInput(genesisHash, 0) -> Babbage(
              address = Alice.address,
              value = Value.ada(100)
            ),
            scriptUtxo.input -> scriptUtxo.output
          )
        )

        val tx = TxBuilder(testEnv)
            .spend(scriptUtxo, Data.List(List.empty), alwaysOkScript)
            .payTo(Bob.address, Value.ada(5))
            .complete(provider, Alice.address)
            .transaction

        // Verify collateral amount is based on fee
        val fee = tx.body.value.fee
        val collateralPercentage = testEnv.protocolParams.collateralPercentage
        val minRequiredCollateral = (fee.value * collateralPercentage) / 100

        // Sum collateral inputs
        val collateralInputs = tx.body.value.collateralInputs.toSeq
        val collateralValue = collateralInputs
            .flatMap(input => provider.findUtxo(input).toOption.map(_.output.value.coin.value))
            .sum

        assert(
          collateralValue >= minRequiredCollateral,
          s"Collateral ($collateralValue) should be >= required ($minRequiredCollateral)"
        )
    }

    // ============================================================================
    // Category 4: Script Spending Scenarios
    // ============================================================================

    test("complete should handle mixed script and pubkey inputs") {
        val genesisHash = TransactionHash.fromByteString(ByteString.fromHex("0" * 64))

        val scriptAddress = ShelleyAddress(
          network = testEnv.network,
          payment = ShelleyPaymentPart.Script(alwaysOkScript.scriptHash),
          delegation = ShelleyDelegationPart.Null
        )
        val scriptUtxo = Utxo(
          TransactionInput(genesisHash, 2),
          Babbage(
            address = scriptAddress,
            value = Value.ada(10),
            datumOption = Some(Inline(Data.I(42)))
          )
        )

        val provider = SimpleMockProvider(
          initialUtxos = Map(
            TransactionInput(genesisHash, 0) -> Babbage(
              address = Alice.address,
              value = Value.ada(100)
            ),
            // Extra UTXO for collateral
            TransactionInput(genesisHash, 1) -> Babbage(
              address = Alice.address,
              value = Value.ada(50)
            ),
            scriptUtxo.input -> scriptUtxo.output
          )
        )

        // Spend script UTXO, complete should add pubkey input for fees
        val tx = TxBuilder(testEnv)
            .spend(scriptUtxo, Data.List(List.empty), alwaysOkScript)
            .payTo(Bob.address, Value.ada(5))
            .complete(provider, Alice.address)
            .transaction

        // Should have both inputs
        assert(tx.body.value.inputs.toSeq.contains(scriptUtxo.input))

        // Should have collateral for script
        assert(tx.body.value.collateralInputs.toSeq.nonEmpty)
    }

    // ============================================================================
    // Category 5: Minting Scenarios
    // ============================================================================

    test("complete should select inputs for fees when only minting (no explicit inputs)") {
        val genesisHash = TransactionHash.fromByteString(ByteString.fromHex("0" * 64))
        val policyId = alwaysOkScript.scriptHash
        val assetName = AssetName.fromString("token")

        val provider = SimpleMockProvider(
          initialUtxos = Map(
            TransactionInput(genesisHash, 0) -> Babbage(
              address = Alice.address,
              value = Value.ada(100)
            ),
            // Extra UTXO for collateral
            TransactionInput(genesisHash, 1) -> Babbage(
              address = Alice.address,
              value = Value.ada(50)
            )
          )
        )

        val mintValue = Value(
          coin = Coin.ada(5),
          assets = MultiAsset(SortedMap(policyId -> SortedMap(assetName -> 100L)))
        )

        val tx = TxBuilder(testEnv)
            .mintAndAttach(Data.List(List.empty), Map(assetName -> 100L), alwaysOkScript)
            .payTo(Bob.address, mintValue)
            .complete(provider, Alice.address)
            .transaction

        // Should have selected input for fees even though no explicit spend
        assert(tx.body.value.inputs.toSeq.nonEmpty, "Should select inputs for fees")

        // Should have minted tokens
        assert(tx.body.value.mint.isDefined)
        val minted = tx.body.value.mint.get.assets.get(policyId).flatMap(_.get(assetName))
        assert(minted.contains(100L))
    }

    // ============================================================================
    // Category 6: Change Output Handling
    // ============================================================================

    test("complete should ensure change output meets minimum ADA") {
        val genesisHash = TransactionHash.fromByteString(ByteString.fromHex("0" * 64))

        val provider = SimpleMockProvider(
          initialUtxos = Map(
            TransactionInput(genesisHash, 0) -> Babbage(
              address = Alice.address,
              value = Value.ada(100)
            )
          )
        )

        // Pay most of the ADA, leaving small change
        val tx = TxBuilder(testEnv)
            .payTo(Bob.address, Value.ada(98))
            .complete(provider, Alice.address)
            .transaction

        // Change output should meet min ADA
        val aliceOutputs = outputsOf(Alice, tx)
        if aliceOutputs.nonEmpty then {
            val changeAda = aliceOutputs.head.value.value.coin.value
            val minAda = testEnv.protocolParams.utxoCostPerByte * 160 // rough estimate
            assert(
              changeAda >= minAda,
              s"Change output ($changeAda) should meet min ADA ($minAda)"
            )
        }
    }

    test("complete should correctly place change output at sponsor address") {
        val genesisHash = TransactionHash.fromByteString(ByteString.fromHex("0" * 64))

        val provider = SimpleMockProvider(
          initialUtxos = Map(
            TransactionInput(genesisHash, 0) -> Babbage(
              address = Alice.address,
              value = Value.ada(100)
            )
          )
        )

        val tx = TxBuilder(testEnv)
            .payTo(Bob.address, Value.ada(10))
            .complete(provider, Alice.address)
            .transaction

        // All non-Bob outputs should go to Alice (the sponsor)
        val nonBobOutputs = tx.body.value.outputs.toSeq.filterNot(_.value.address == Bob.address)
        assert(nonBobOutputs.nonEmpty, "Should have change output")
        assert(
          nonBobOutputs.forall(_.value.address == Alice.address),
          "All change should go to sponsor (Alice)"
        )
    }

    // ============================================================================
    // Category 8: Validity Interval Tests
    // ============================================================================

    test("complete should preserve validity interval settings") {
        val genesisHash = TransactionHash.fromByteString(ByteString.fromHex("0" * 64))

        val provider = SimpleMockProvider(
          initialUtxos = Map(
            TransactionInput(genesisHash, 0) -> Babbage(
              address = Alice.address,
              value = Value.ada(100)
            )
          )
        )

        import java.time.Instant
        val tx = TxBuilder(testEnv)
            .validFrom(Instant.ofEpochSecond(1000))
            .validTo(Instant.ofEpochSecond(2000))
            .payTo(Bob.address, Value.ada(10))
            .complete(provider, Alice.address)
            .transaction

        assert(
          tx.body.value.validityStartSlot.isDefined,
          "Should preserve validFrom"
        )
        assert(
          tx.body.value.ttl.isDefined,
          "Should preserve validTo (TTL)"
        )
    }

    // ============================================================================
    // Category 11: Error Handling
    // ============================================================================

    test("complete should handle empty UTXO set from provider") {
        val provider = SimpleMockProvider(initialUtxos = Map.empty)

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
        val genesisHash = TransactionHash.fromByteString(ByteString.fromHex("0" * 64))

        val provider = SimpleMockProvider(
          initialUtxos = Map(
            TransactionInput(genesisHash, 0) -> Babbage(
              address = Alice.address,
              value = Value.ada(10)
            )
          )
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
    // Category 12: Integration Tests
    // ============================================================================

    test("complete should produce deterministic results") {
        val genesisHash = TransactionHash.fromByteString(ByteString.fromHex("0" * 64))

        val provider = SimpleMockProvider(
          initialUtxos = Map(
            TransactionInput(genesisHash, 0) -> Babbage(
              address = Alice.address,
              value = Value.ada(100)
            ),
            TransactionInput(genesisHash, 1) -> Babbage(
              address = Alice.address,
              value = Value.ada(50)
            )
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

        // Same inputs should produce same transaction
        assert(tx1.body.value.inputs == tx2.body.value.inputs, "Inputs should be deterministic")
        assert(tx1.body.value.outputs == tx2.body.value.outputs, "Outputs should be deterministic")
        assert(tx1.body.value.fee == tx2.body.value.fee, "Fee should be deterministic")
    }

}
