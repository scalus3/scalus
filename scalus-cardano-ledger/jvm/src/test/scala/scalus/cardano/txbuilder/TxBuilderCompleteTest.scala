package scalus.cardano.txbuilder

import org.scalatest.funsuite.AnyFunSuite
import scalus.builtin.ByteString.utf8
import scalus.builtin.{ByteString, Data}
import scalus.cardano.address.{Address, ShelleyAddress, ShelleyDelegationPart, ShelleyPaymentPart}
import scalus.cardano.ledger.*
import scalus.cardano.ledger.DatumOption.Inline
import scalus.cardano.ledger.rules.ValidatorRulesTestKit
import scalus.cardano.node.{Provider, SubmitError}
import scalus.cardano.txbuilder.TestPeer.{Alice, Bob}
import scalus.{plutusV3, toUplc, Compiler}

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

    // Common test constants and helpers
    private val genesisHash =
        TransactionHash.fromByteString(ByteString.fromArray(new Array[Byte](32)))

    private def mkUtxo(
        address: Address,
        value: Value,
        index: Int = 0,
        datumOption: Option[DatumOption] = None
    ): (TransactionInput, TransactionOutput) =
        TransactionInput(genesisHash, index) -> TransactionOutput(address, value, datumOption)

    private def mkProvider(utxos: (TransactionInput, TransactionOutput)*): SimpleMockProvider =
        SimpleMockProvider(initialUtxos = utxos.toMap)

    private def getTokenAmount(
        output: Sized[TransactionOutput],
        policyId: PolicyId,
        assetName: AssetName
    ): Option[Long] =
        output.value.value.assets.assets.get(policyId).flatMap(_.get(assetName))

    private def sumTokens(
        outputs: Seq[Sized[TransactionOutput]],
        policyId: PolicyId
    ): Map[AssetName, Long] =
        outputs
            .flatMap(_.value.value.assets.assets.get(policyId))
            .foldLeft(Map.empty[AssetName, Long]) { (acc, assets) =>
                assets.foldLeft(acc) { case (a, (name, amount)) =>
                    a + (name -> (a.getOrElse(name, 0L) + amount))
                }
            }

    test("complete should automatically add inputs for simple ADA payment") {
        val provider = mkProvider(
          mkUtxo(Alice.address, Value.ada(100), 0),
          mkUtxo(Alice.address, Value.ada(50), 1)
        )

        val paymentValue = Value.ada(10)
        val tx = TxBuilder(testEnv)
            .payTo(Bob.address, paymentValue)
            .complete(provider, Alice.address)
            .build()
            .transaction

        assert(tx.body.value.inputs.toSeq.size == 1, "Transaction must have exactly 1 input")

        val bobOutputs = outputsOf(Bob, tx)
        assert(bobOutputs.size == 1, "Should have output to Bob")
        assert(
          bobOutputs.head.value.value.coin >= paymentValue.coin,
          "Bob should receive at least 10 ADA"
        )

        val aliceOutputs = outputsOf(Alice, tx)
        assert(aliceOutputs.nonEmpty, "Should have change output to Alice")
    }

    test("complete inputs should be signable with sign()") {
        val provider = mkProvider(mkUtxo(Alice.address, Value.ada(100), 0))

        val signedTx = TxBuilder(testEnv)
            .payTo(Bob.address, Value.ada(10))
            .complete(provider, Alice.address)
            .build()
            .sign(aliceSigner)
            .transaction

        assert(
          signedTx.witnessSet.vkeyWitnesses.toSeq.nonEmpty,
          "Should have vkey witnesses after signing"
        )
    }

    test("complete should handle multi-asset transactions") {
        val assetName = AssetName(utf8"co2")
        val policyId = alwaysOkScript.scriptHash

        val provider = mkProvider(
          mkUtxo(Alice.address, Value.asset(policyId, assetName, 200L, Coin.ada(100)), 0),
          mkUtxo(Alice.address, Value.ada(50), 1)
        )

        val paymentValue = Value.asset(policyId, assetName, 100L, Coin.ada(5))

        val tx = TxBuilder(testEnv)
            .payTo(Bob.address, paymentValue)
            .complete(provider, Alice.address)
            .build()
            .transaction

        val bobOutputs = outputsOf(Bob, tx)
        assert(bobOutputs.size == 1, "Should have output to Bob")
        assert(
          getTokenAmount(bobOutputs.head, policyId, assetName).contains(100L),
          "Bob should receive 100 tokens"
        )

        val aliceOutputs = outputsOf(Alice, tx)
        assert(aliceOutputs.nonEmpty, "Should have change output to Alice")
        assert(
          sumTokens(aliceOutputs, policyId).get(assetName).contains(100L),
          "Alice should receive 100 tokens as change"
        )
    }

    test("complete should auto-detect and add collateral for script spending") {
        val scriptAddress = ShelleyAddress(
          network = testEnv.network,
          payment = ShelleyPaymentPart.Script(alwaysOkScript.scriptHash),
          delegation = ShelleyDelegationPart.Null
        )
        val scriptUtxoTuple = mkUtxo(scriptAddress, Value.ada(20), 2, Some(Inline(Data.I(42))))
        val scriptUtxo = Utxo(scriptUtxoTuple)

        val provider = mkProvider(
          mkUtxo(Alice.address, Value.ada(100), 0),
          mkUtxo(Alice.address, Value.ada(10), 1),
          scriptUtxoTuple
        )

        val tx = TxBuilder(testEnv)
            .spend(scriptUtxo, Data.List(List.empty), alwaysOkScript)
            .payTo(Bob.address, Value.ada(5))
            .complete(provider, Alice.address)
            .build()
            .transaction

        assert(
          tx.body.value.collateralInputs.toSeq.nonEmpty,
          "Transaction should have collateral inputs"
        )
        assert(outputsOf(Bob, tx).nonEmpty, "Should have output to Bob")
    }

    test("complete should auto-detect and add collateral for minting") {
        val policyId = alwaysOkScript.scriptHash
        val assetName = AssetName(ByteString.fromString("mint01"))

        val provider = mkProvider(
          mkUtxo(Alice.address, Value.ada(100), 0),
          mkUtxo(Alice.address, Value.ada(10), 1)
        )

        val tx = TxBuilder(testEnv)
            .mintAndAttach(Data.List(List.empty), Map(assetName -> 100L), alwaysOkScript)
            .payTo(Bob.address, Value.asset(policyId, assetName, 100L, Coin.ada(5)))
            .complete(provider, Alice.address)
            .build()
            .transaction

        assert(
          tx.body.value.collateralInputs.toSeq.nonEmpty,
          "Transaction should have collateral inputs for minting"
        )
        assert(tx.body.value.mint.isDefined, "Transaction should have mint field")
    }

    test("complete should fail with insufficient funds") {
        val provider = mkProvider(mkUtxo(Alice.address, Value.ada(5), 0))

        val exception = intercept[RuntimeException] {
            TxBuilder(testEnv)
                .payTo(Bob.address, Value.ada(100))
                .complete(provider, Alice.address)
                .build()
        }

        assert(
          exception.getMessage.contains("Utxos not found") ||
              exception.getMessage.contains("Insufficient"),
          s"Should indicate insufficient funds, got: ${exception.getMessage}"
        )
    }

    test("complete with explicit spend should only add necessary additional inputs") {
        val explicitUtxo = genAdaOnlyPubKeyUtxo(Alice, min = 3_000_000).sample.get

        val provider = mkProvider(
          mkUtxo(Alice.address, Value.ada(100), 0),
          explicitUtxo
        )

        val tx = TxBuilder(testEnv)
            .spend(Utxo(explicitUtxo))
            .payTo(Bob.address, explicitUtxo._2.value)
            .complete(provider, Alice.address)
            .build()
            .transaction

        assert(
          tx.body.value.inputs.toSeq.contains(explicitUtxo._1),
          "Should include explicitly spent UTXO"
        )
        assert(tx.body.value.inputs.toSeq.size == 2)

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
        val provider = mkProvider(
          mkUtxo(Alice.address, Value.ada(50), 0),
          mkUtxo(Alice.address, Value.ada(50), 1)
        )

        val signedTx = TxBuilder(testEnv)
            .payTo(Bob.address, Value.ada(20))
            .complete(provider, Alice.address)
            .build()
            .sign(aliceSigner)
            .transaction

        assert(signedTx.witnessSet.vkeyWitnesses.toSeq.nonEmpty, "Should have vkey witnesses")

        val aliceInputs = signedTx.body.value.inputs.toSeq.filter { input =>
            provider.findUtxo(input).toOption.exists(_.output.address == Alice.address)
        }
        assert(aliceInputs.nonEmpty, "Should have inputs from Alice that were added by complete")
    }

    test("complete should send tokens acquired from additional input querying back as change") {
        val explicitUtxo = genAdaOnlyPubKeyUtxo(Alice, min = 5_000_000).sample.get
        val assetName = AssetName(utf8"co2")
        val policyId = alwaysOkScript.scriptHash

        val provider = mkProvider(
          mkUtxo(Alice.address, Value.asset(policyId, assetName, 200L, Coin.ada(100)), 0),
          explicitUtxo
        )

        val tx = TxBuilder(testEnv)
            .spend(Utxo(explicitUtxo))
            .payTo(Bob.address, explicitUtxo._2.value)
            .complete(provider, Alice.address)
            .build()
            .transaction

        val bobOutputs = outputsOf(Bob, tx)
        assert(bobOutputs.size == 1, "Should have an output to Bob")
        assert(bobOutputs.head.value.value.coin >= Coin.ada(5), "Bob must receive 5 ADA")
        assert(bobOutputs.head.value.value.assets.isEmpty, "Bob must not receive any tokens")

        val aliceChangeOuts = outputsOf(Alice, tx)
        assert(aliceChangeOuts.size == 1, "Alice must receive one output back")
        val aliceChangeValue = aliceChangeOuts.head.value.value
        assert(aliceChangeValue.coin > Coin.ada(99) && aliceChangeValue.coin < Coin.ada(100))
        assert(
          aliceChangeValue.assets.assets.head._2(assetName) == 200L,
          "Alice must receive all co2 back as change"
        )
    }

    test("complete should handle multiple different tokens from separate UTXOs") {
        val policyId1 = alwaysOkScript.scriptHash
        val policyId2 = ScriptHash.fromByteString(ByteString.fromHex("1" * 56))
        val token1 = AssetName(utf8"co2")
        val token2 = AssetName(utf8"h2so4")

        val provider = mkProvider(
          mkUtxo(Alice.address, Value.asset(policyId1, token1, 100L, Coin.ada(50)), 0),
          mkUtxo(Alice.address, Value.asset(policyId2, token2, 200L, Coin.ada(50)), 1),
          mkUtxo(Alice.address, Value.ada(10), 2)
        )

        val paymentValue = Value.assets(
          Map(policyId1 -> Map(token1 -> 30L), policyId2 -> Map(token2 -> 50L)),
          Coin.ada(5)
        )

        val tx = TxBuilder(testEnv)
            .payTo(Bob.address, paymentValue)
            .complete(provider, Alice.address)
            .build()
            .transaction

        assert(
          tx.body.value.inputs.toSeq.size >= 2,
          "Should select at least 2 inputs for different tokens"
        )

        val bobOutputs = outputsOf(Bob, tx)
        assert(bobOutputs.size == 1, "Should have output to Bob")
        assert(
          getTokenAmount(bobOutputs.head, policyId1, token1).contains(30L),
          "Bob should receive 30 of token1"
        )
        assert(
          getTokenAmount(bobOutputs.head, policyId2, token2).contains(50L),
          "Bob should receive 50 of token2"
        )

        val aliceOutputs = outputsOf(Alice, tx)
        assert(aliceOutputs.nonEmpty, "Should have change output to Alice")
        assert(
          sumTokens(aliceOutputs, policyId1).get(token1).contains(70L),
          "Alice should receive 70 of co2 as change"
        )
        assert(
          sumTokens(aliceOutputs, policyId2).get(token2).contains(150L),
          "Alice should receive 150 of h2so4 as change"
        )
    }

    test("complete should add ADA-only UTXO when token UTXO has insufficient ADA for fees") {
        val policyId = alwaysOkScript.scriptHash
        val assetName = AssetName(utf8"co2")

        val provider = mkProvider(
          mkUtxo(Alice.address, Value.asset(policyId, assetName, 100L, Coin.ada(1)), 0),
          mkUtxo(Alice.address, Value.ada(50), 1)
        )

        val paymentValue = Value.asset(policyId, assetName, 50L, Coin.zero)

        val tx = TxBuilder(testEnv)
            .payTo(Bob.address, paymentValue)
            .complete(provider, Alice.address)
            .build()
            .transaction

        assert(
          tx.body.value.inputs.toSeq.size == 2,
          "Should select both token UTXO and ADA-only UTXO"
        )

        val bobOutputs = outputsOf(Bob, tx)
        assert(bobOutputs.size == 1, "Should have output to Bob")
        assert(
          getTokenAmount(bobOutputs.head, policyId, assetName).contains(50L),
          "Bob should receive 50 co2"
        )
        assert(!bobOutputs.head.value.value.coin.isZero, "Bob must get at least min ADA coins")

        val aliceOutputs = outputsOf(Alice, tx)
        assert(aliceOutputs.nonEmpty, "Should have change output to Alice")
        assert(
          sumTokens(aliceOutputs, policyId).get(assetName).sum == 50L,
          "Alice should receive 50 tokens as change"
        )
    }

    test("complete should handle multiple token types in single UTXO efficiently") {
        val policyId = alwaysOkScript.scriptHash
        val tokenA = AssetName(utf8"co2")
        val tokenB = AssetName(utf8"h2so4")
        val tokenC = AssetName(utf8"c11h15no2")

        val provider = mkProvider(
          mkUtxo(
            Alice.address,
            Value.fromPolicy(
              policyId,
              Map(tokenA -> 100L, tokenB -> 200L, tokenC -> 50L),
              Coin.ada(100)
            ),
            0
          ),
          mkUtxo(Alice.address, Value.ada(10), 1)
        )

        val paymentValue =
            Value.fromPolicy(policyId, Map(tokenA -> 30L, tokenB -> 100L), Coin.ada(5))

        val tx = TxBuilder(testEnv)
            .payTo(Bob.address, paymentValue)
            .complete(provider, Alice.address)
            .build()
            .transaction

        val bobOutputs = outputsOf(Bob, tx)
        assert(bobOutputs.size == 1, "Should have output to Bob")
        assert(
          getTokenAmount(bobOutputs.head, policyId, tokenA).contains(30L),
          "Bob should receive 30 of co2"
        )
        assert(
          getTokenAmount(bobOutputs.head, policyId, tokenB).contains(100L),
          "Bob should receive 100 of h2so4"
        )
        assert(
          getTokenAmount(bobOutputs.head, policyId, tokenC).isEmpty,
          "Bob should not receive c11h15no2"
        )

        val aliceOutputs = outputsOf(Alice, tx)
        assert(aliceOutputs.nonEmpty, "Should have change output to Alice")
        val aliceTokens = sumTokens(aliceOutputs, policyId)
        assert(aliceTokens.get(tokenA).contains(70L), "Alice should receive 70 of co2 as change")
        assert(
          aliceTokens.get(tokenB).contains(100L),
          "Alice should receive 100 of h2so4 as change"
        )
        assert(
          aliceTokens.get(tokenC).contains(50L),
          "Alice should receive all 50 of c11h15no2 as change"
        )
    }

    private def outputsOf(peer: TestPeer, tx: Transaction) =
        tx.body.value.outputs.toSeq.filter(_.value.address == peer.address)

}
