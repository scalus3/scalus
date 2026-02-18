package scalus.examples.bilinearAccumulator

import org.scalatest.funsuite.AnyFunSuite
import scalus.cardano.address.*
import scalus.cardano.ledger.*
import scalus.cardano.node.Emulator
import scalus.cardano.txbuilder.TxBuilder
import scalus.testing.kit.{Party, ScalusTest, TestUtil}
import scalus.uplc.builtin.{Builtins, Data}
import scalus.uplc.builtin.Builtins.bls12_381_G2_compress
import scalus.utils.await

class AllowlistValidatorE2ETest extends AnyFunSuite with ScalusTest {

    given CardanoInfo = CardanoInfo.mainnet

    // Test-only tau. In production, tau must be random and destroyed after setup.
    private val tau = BigInt("12345678901234567890")
    private val genesisHash = TestUtil.genesisHash

    /** Convert a PubKeyHash (28-byte ByteString) to a BigInt element for the accumulator. */
    private def pkhToElement(pkh: AddrKeyHash): BigInt =
        Builtins.byteStringToInteger(true, pkh)

    private def scriptAddr(scriptHash: ScriptHash) = ShelleyAddress(
      summon[CardanoInfo].network,
      ShelleyPaymentPart.Script(scriptHash),
      ShelleyDelegationPart.Null
    )

    test("valid membership proof allows spending") {
        // Accumulated elements are PubKeyHash integers
        val alicePkh = Party.Alice.addrKeyHash
        val bobPkh = Party.Bob.addrKeyHash
        val charlesPkh = Party.Charles.addrKeyHash
        val fullSet = Vector(pkhToElement(alicePkh), pkhToElement(bobPkh), pkhToElement(charlesPkh))

        val setup = BilinearAccumulatorProver.trustedSetup(tau, fullSet.size)
        val acc = BilinearAccumulatorProver.accumulate(setup, fullSet)
        val proof = BilinearAccumulatorProver.membershipProof(
          setup,
          fullSet,
          Vector(pkhToElement(alicePkh))
        )

        // Apply compressed accumulator as ByteString parameter
        val compressedAcc = bls12_381_G2_compress(acc)
        val applied = AllowlistContract.withErrorTraces(compressedAcc)
        val script = Script.PlutusV3(applied.program.cborByteString)
        val scriptAddress = scriptAddr(script.scriptHash)

        // Redeemer is just the compressed proof; member identity comes from tx.signatories
        val compressedProof = bls12_381_G2_compress(proof)
        val redeemer = Data.B(compressedProof)

        // Create initial UTXOs
        val scriptUtxoEntry = TransactionInput(genesisHash, 0) ->
            TransactionOutput(scriptAddress, Value.lovelace(10_000_000L), Data.unit)
        val feePayerUtxo = TransactionInput(genesisHash, 1) ->
            TransactionOutput(Party.Alice.address, Value.lovelace(100_000_000L))
        val collateralUtxo = TransactionInput(genesisHash, 2) ->
            TransactionOutput(Party.Alice.address, Value.lovelace(50_000_000L))

        val emulator = Emulator(Map(scriptUtxoEntry, feePayerUtxo, collateralUtxo))

        // Alice must be a required signer so her pkh appears in tx.signatories
        val tx = TxBuilder(summon[CardanoInfo])
            .spend(Utxo(scriptUtxoEntry), redeemer, script, Set(alicePkh))
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

    test("non-member cannot spend with a copied proof") {
        val alicePkh = Party.Alice.addrKeyHash
        val bobPkh = Party.Bob.addrKeyHash
        val charlesPkh = Party.Charles.addrKeyHash
        val fullSet = Vector(pkhToElement(alicePkh), pkhToElement(bobPkh), pkhToElement(charlesPkh))

        val setup = BilinearAccumulatorProver.trustedSetup(tau, fullSet.size)
        val acc = BilinearAccumulatorProver.accumulate(setup, fullSet)
        // Proof is for Alice's pkh
        val proof = BilinearAccumulatorProver.membershipProof(
          setup,
          fullSet,
          Vector(pkhToElement(alicePkh))
        )

        val compressedAcc = bls12_381_G2_compress(acc)
        val applied = AllowlistContract.withErrorTraces(compressedAcc)
        val script = Script.PlutusV3(applied.program.cborByteString)
        val scriptAddress = scriptAddr(script.scriptHash)

        // Dave (non-member) copies Alice's proof from the mempool
        // tx.signatories.head = Dave's pkh, which doesn't match Alice's proof element
        val compressedProof = bls12_381_G2_compress(proof)
        val redeemer = Data.B(compressedProof)
        val davePkh = Party.Dave.addrKeyHash

        val scriptUtxoEntry = TransactionInput(genesisHash, 0) ->
            TransactionOutput(scriptAddress, Value.lovelace(10_000_000L), Data.unit)
        val feePayerUtxo = TransactionInput(genesisHash, 1) ->
            TransactionOutput(Party.Dave.address, Value.lovelace(100_000_000L))
        val collateralUtxo = TransactionInput(genesisHash, 2) ->
            TransactionOutput(Party.Dave.address, Value.lovelace(50_000_000L))

        val emulator = Emulator(Map(scriptUtxoEntry, feePayerUtxo, collateralUtxo))

        assertThrows[scalus.cardano.txbuilder.TxBuilderException.BalancingException] {
            TxBuilder(summon[CardanoInfo])
                .spend(Utxo(scriptUtxoEntry), redeemer, script, Set(davePkh))
                .spend(Utxo(feePayerUtxo))
                .payTo(Party.Alice.address, Value.lovelace(10_000_000L))
                .changeTo(TransactionOutput(Party.Dave.address, Value.lovelace(0L)))
                .complete(emulator, Party.Dave.address)
                .await()
        }
    }
}
