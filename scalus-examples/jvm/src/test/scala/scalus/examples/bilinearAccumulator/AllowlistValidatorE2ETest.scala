package scalus.examples.bilinearAccumulator

import org.scalatest.funsuite.AnyFunSuite
import scalus.cardano.address.*
import scalus.cardano.ledger.*
import scalus.cardano.node.Emulator
import scalus.cardano.txbuilder.TxBuilder
import scalus.testing.kit.{Party, ScalusTest, TestUtil}
import scalus.cardano.onchain.plutus.prelude.List as PList
import scalus.uplc.builtin.Data
import scalus.uplc.builtin.Builtins.bls12_381_G2_compress
import scalus.utils.await

class AllowlistValidatorE2ETest extends AnyFunSuite with ScalusTest {

    given CardanoInfo = CardanoInfo.mainnet

    private val tau = BigInt("12345678901234567890")
    private val genesisHash = TestUtil.genesisHash

    private def scriptAddr(scriptHash: ScriptHash) = ShelleyAddress(
      summon[CardanoInfo].network,
      ShelleyPaymentPart.Script(scriptHash),
      ShelleyDelegationPart.Null
    )

    test("valid membership proof allows spending") {
        val fullSet = Vector(BigInt(10), BigInt(20), BigInt(30))
        val memberElement = BigInt(20)
        val setup = BilinearAccumulatorProver.trustedSetup(tau, fullSet.size)
        val acc = BilinearAccumulatorProver.accumulate(setup, fullSet)
        val proof = BilinearAccumulatorProver.membershipProof(setup, fullSet, Vector(memberElement))

        // Apply compressed accumulator as ByteString parameter
        val compressedAcc = bls12_381_G2_compress(acc)
        val applied = AllowlistContract.withErrorTraces(compressedAcc)
        val script = Script.PlutusV3(applied.program.cborByteString)
        val scriptAddress = scriptAddr(script.scriptHash)

        // Build redeemer: (element, compressed proof)
        val compressedProof = bls12_381_G2_compress(proof)
        val redeemer = Data.Constr(0, PList(Data.I(memberElement), Data.B(compressedProof)))

        // Create initial UTXOs
        val scriptUtxoEntry = TransactionInput(genesisHash, 0) ->
            TransactionOutput(scriptAddress, Value.lovelace(10_000_000L), Data.unit)
        val feePayerUtxo = TransactionInput(genesisHash, 1) ->
            TransactionOutput(Party.Alice.address, Value.lovelace(100_000_000L))
        val collateralUtxo = TransactionInput(genesisHash, 2) ->
            TransactionOutput(Party.Alice.address, Value.lovelace(50_000_000L))

        val emulator = Emulator(Map(scriptUtxoEntry, feePayerUtxo, collateralUtxo))

        // Build, sign, and submit transaction
        val tx = TxBuilder(summon[CardanoInfo])
            .spend(Utxo(scriptUtxoEntry), redeemer, script)
            .spend(Utxo(feePayerUtxo))
            .payTo(Party.Bob.address, Value.lovelace(10_000_000L))
            .changeTo(TransactionOutput(Party.Alice.address, Value.lovelace(0L)))
            .complete(emulator, Party.Alice.address)
            .await()
            .sign(Party.Alice.signer)
            .transaction

        val result = emulator.submit(tx).await()
        assert(result.isRight, s"Transaction should succeed: ${result.left.toOption.getOrElse("")}")
    }

    test("invalid proof rejects spending") {
        val fullSet = Vector(BigInt(10), BigInt(20), BigInt(30))
        val nonMemberElement = BigInt(99)
        val setup = BilinearAccumulatorProver.trustedSetup(tau, fullSet.size)
        val acc = BilinearAccumulatorProver.accumulate(setup, fullSet)
        // Proof is for element 20 (valid), but redeemer claims element 99
        val proof = BilinearAccumulatorProver.membershipProof(setup, fullSet, Vector(BigInt(20)))

        val compressedAcc = bls12_381_G2_compress(acc)
        val applied = AllowlistContract.withErrorTraces(compressedAcc)
        val script = Script.PlutusV3(applied.program.cborByteString)
        val scriptAddress = scriptAddr(script.scriptHash)

        val compressedProof = bls12_381_G2_compress(proof)
        val redeemer = Data.Constr(0, PList(Data.I(nonMemberElement), Data.B(compressedProof)))

        val scriptUtxoEntry = TransactionInput(genesisHash, 0) ->
            TransactionOutput(scriptAddress, Value.lovelace(10_000_000L), Data.unit)
        val feePayerUtxo = TransactionInput(genesisHash, 1) ->
            TransactionOutput(Party.Alice.address, Value.lovelace(100_000_000L))
        val collateralUtxo = TransactionInput(genesisHash, 2) ->
            TransactionOutput(Party.Alice.address, Value.lovelace(50_000_000L))

        val emulator = Emulator(Map(scriptUtxoEntry, feePayerUtxo, collateralUtxo))

        assertThrows[scalus.cardano.txbuilder.TxBuilderException.BalancingException] {
            TxBuilder(summon[CardanoInfo])
                .spend(Utxo(scriptUtxoEntry), redeemer, script)
                .spend(Utxo(feePayerUtxo))
                .payTo(Party.Bob.address, Value.lovelace(10_000_000L))
                .changeTo(TransactionOutput(Party.Alice.address, Value.lovelace(0L)))
                .complete(emulator, Party.Alice.address)
                .await()
        }
    }
}
