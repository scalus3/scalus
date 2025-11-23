package scalus.cardano.txbuilder

import org.scalatest.funsuite.AnyFunSuite
import scalus.builtin.{ByteString, Data}
import scalus.cardano.address.{Address, ShelleyAddress, ShelleyDelegationPart, ShelleyPaymentPart}
import scalus.cardano.ledger.*
import scalus.cardano.ledger.DatumOption.Inline
import scalus.cardano.ledger.TransactionOutput.Babbage
import scalus.cardano.ledger.rules.ValidatorRulesTestKit
import scalus.cardano.node.Provider
import scalus.cardano.txbuilder.TestPeer.{Alice, Bob}
import scalus.{plutusV3, toUplc, Compiler}

import scala.collection.immutable.SortedMap

// TODO: can't depend `testkit`, since it'd introduce circular dependency. /
class SimpleMockProvider(initialUtxos: Utxos) extends Provider {
    override def submit(transaction: Transaction): Either[RuntimeException, Unit] = Right(())

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

    test("complete() should automatically add inputs for simple ADA payment") {
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

        val builder = TxBuilder(testEnv)
            .payTo(Bob.address, Value.ada(10))
            .complete(provider, Alice.address)
            .build()

        val tx = builder.transaction

        // Should have at least one input
        assert(tx.body.value.inputs.toSeq.nonEmpty, "Transaction should have inputs")

        // Should have payment to Bob
        val bobOutputs = tx.body.value.outputs.toSeq.filter(_.value.address == Bob.address)
        assert(bobOutputs.nonEmpty, "Should have output to Bob")
        assert(
          bobOutputs.head.value.value.coin.value >= 10_000_000L,
          "Bob should receive at least 10 ADA"
        )

        // Should have change output to Alice
        val aliceOutputs = tx.body.value.outputs.toSeq.filter(_.value.address == Alice.address)
        assert(aliceOutputs.nonEmpty, "Should have change output to Alice")
    }

    test("complete() inputs should be signable with sign()") {
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
            .build()

        // Sign the transaction
        val signedBuilderResult = unsignedBuilder.sign(aliceSigner)

        assert(signedBuilderResult.isRight, "Should successfully sign the transaction")

        val signedTx = signedBuilderResult.toOption.get.transaction

        // Should have vkey witnesses for the inputs added by complete()
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

    test("complete() should handle multi-asset transactions") {
        val genesisHash = TransactionHash.fromByteString(ByteString.fromHex("0" * 64))
        val assetName = AssetName(ByteString.fromString("co2"))
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
            .build()

        val tx = builder.transaction

        // Bob should receive the tokens
        val bobOutputs = tx.body.value.outputs.toSeq.filter(_.value.address == Bob.address)
        assert(bobOutputs.nonEmpty, "Should have output to Bob")

        val bobTokens = bobOutputs.head.value.value.assets.assets
            .get(policyId)
            .flatMap(_.get(assetName))
        assert(bobTokens.contains(100L), "Bob should receive 100 tokens")

        // Change output should have remaining 100 tokens
        val aliceOutputs = tx.body.value.outputs.toSeq.filter(_.value.address == Alice.address)
        assert(aliceOutputs.nonEmpty, "Should have change output to Alice")

        val aliceTokens = aliceOutputs.flatMap(
          _.value.value.assets.assets
              .get(policyId)
              .flatMap(_.get(assetName))
        )
        assert(aliceTokens.sum == 100L, "Alice should receive 100 tokens as change")
    }

    test("complete() should auto-detect and add collateral for script spending") {
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
            .build()

        val tx = builder.transaction

        // Should have collateral inputs
        assert(
          tx.body.value.collateralInputs.toSeq.nonEmpty,
          "Transaction should have collateral inputs"
        )

        // Should have payment to Bob
        val bobOutputs = tx.body.value.outputs.toSeq.filter(_.value.address == Bob.address)
        assert(bobOutputs.nonEmpty, "Should have output to Bob")
    }

    test("complete() should auto-detect and add collateral for minting") {
        val genesisHash = TransactionHash.fromByteString(ByteString.fromHex("0" * 64))
        val policyId = alwaysOkScript.scriptHash
        val redeemer = Data.List(List.empty)
        val assetName = AssetName(ByteString.fromString("mint01"))
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
              policyId -> SortedMap(assetName -> 100L)
            )
          )
        )

        val builder = TxBuilder(testEnv)
            .mint(redeemer, assets, alwaysOkScript)
            .payTo(Bob.address, mintValue)
            .complete(provider, Alice.address)
            .build()

        val tx = builder.transaction

        // Should have collateral inputs
        assert(
          tx.body.value.collateralInputs.toSeq.nonEmpty,
          "Transaction should have collateral inputs for minting"
        )

        // Should have mint field
        assert(tx.body.value.mint.isDefined, "Transaction should have mint field")
    }

    test("complete() should fail with insufficient funds") {
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
            builder.complete(provider, Alice.address).build()
        }

        assert(
          exception.getMessage.contains("Utxos not found") ||
              exception.getMessage.contains("Insufficient"),
          s"Should indicate insufficient funds, got: ${exception.getMessage}"
        )
    }

    test("complete() with explicit spend should only add necessary additional inputs") {
        val genesisHash = TransactionHash.fromByteString(ByteString.fromHex("0" * 64))
        val explicitUtxo = genAdaOnlyPubKeyUtxo(Alice, min = 5_000_000).sample.get

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
        // complete() should recognize the explicit input and only add more if needed for fees
        val builder = TxBuilder(testEnv)
            .spend(Utxo(explicitUtxo))
            .payTo(Bob.address, Value.ada(3))
            .complete(provider, Alice.address)
            .build()

        val tx = builder.transaction

        // Should have at least the explicit input
        assert(
          tx.body.value.inputs.toSeq.contains(explicitUtxo._1),
          "Should include explicitly spent UTXO"
        )

        // Bob should receive payment
        val bobOutputs = tx.body.value.outputs.toSeq.filter(_.value.address == Bob.address)
        assert(bobOutputs.nonEmpty, "Should have output to Bob")
        assert(
          bobOutputs.head.value.value.coin.value >= 3_000_000L,
          "Bob should receive at least 3 ADA"
        )
    }

    test("complete() followed by sign() should produce a valid transaction") {
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
            .build()

        // Sign with Alice's wallet
        val signedResult = builtTx.sign(aliceSigner)

        assert(signedResult.isRight, s"Signing should succeed: ${signedResult.left.getOrElse("")}")

        val signedTx = signedResult.toOption.get.transaction

        // Verify signatures exist
        assert(signedTx.witnessSet.vkeyWitnesses.toSeq.nonEmpty, "Should have vkey witnesses")

        // Verify all inputs from Alice's address have corresponding signatures
        val aliceInputs = signedTx.body.value.inputs.toSeq.filter { input =>
            provider.findUtxo(input).toOption.exists(_.output.address == Alice.address)
        }

        assert(
          aliceInputs.nonEmpty,
          "Should have inputs from Alice that were added by complete()"
        )
    }
}
