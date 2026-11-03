package scalus.cardano.txbuilder

import org.scalatest.funsuite.AnyFunSuite
import scalus.compiler.{Compile, Options}
import scalus.uplc.builtin.{ByteString, Data}
import scalus.uplc.{PlutusV2, PlutusV3}
import scalus.cardano.address.{ShelleyAddress, ShelleyDelegationPart, ShelleyPaymentPart}
import scalus.cardano.ledger.*
import scalus.cardano.ledger.rules.Context
import scalus.cardano.node.Emulator
import scalus.testing.kit.Party.Alice
import scalus.utils.await

/** A spending validator that accepts only if the UTxO it guards is also a reference input. */
@Compile
object SpentAndReferencedValidator extends scalus.cardano.onchain.plutus.v3.Validator {
    import scalus.cardano.onchain.plutus.v3.{TxInfo, TxOutRef}
    import scalus.cardano.onchain.plutus.prelude.*

    inline override def spend(
        datum: Option[Data],
        redeemer: Data,
        tx: TxInfo,
        ownRef: TxOutRef
    ): Unit =
        require(tx.referenceInputs.exists(_.outRef === ownRef), "own input is not referenced")
}

/** From protocol version 11 the UTXO rule lets a transaction spend a UTxO and reference it too, so
  * an SDK may read from and collect from one UTxO in one transaction. A PlutusV3 script cannot run
  * in such a transaction: its context fails with `ReferenceInputsNotDisjointFromInputs`
  * (C/TxInfo.hs:488-493), so the ledger rejects the transaction, spec [TX-1]. A PlutusV2 script has
  * no such check. The ledger resolves the reference script of a spent input without a reference, so
  * a spend needs no reference for that.
  */
class SpendAndReferenceSameUtxoTest extends AnyFunSuite {

    given testEnv: CardanoInfo = CardanoInfo.mainnet

    private val genesisHash: TransactionHash =
        TransactionHash.fromByteString(ByteString.fromHex("0" * 64))

    private val script: Script.PlutusV3 = {
        // the PV 10 test runs it too, so no PV 11 lowering
        given Options = Options.plomin
        PlutusV3.compile(SpentAndReferencedValidator.validate).script
    }

    private val scriptAddress: ShelleyAddress = ShelleyAddress(
      network = testEnv.network,
      payment = ShelleyPaymentPart.Script(script.scriptHash),
      delegation = ShelleyDelegationPart.Null
    )

    private val lockedUtxo = Utxo(
      Input(genesisHash, 1),
      Output(
        scriptAddress,
        Value.ada(20),
        Some(DatumOption.Inline(Data.I(42))),
        Some(ScriptRef(script))
      )
    )

    private val v2Script: Script.PlutusV2 = PlutusV2.alwaysOk.script

    private val v2LockedUtxo = Utxo(
      Input(genesisHash, 2),
      Output(
        ShelleyAddress(
          network = testEnv.network,
          payment = ShelleyPaymentPart.Script(v2Script.scriptHash),
          delegation = ShelleyDelegationPart.Null
        ),
        Value.ada(20),
        Some(DatumOption.Inline(Data.I(42))),
        Some(ScriptRef(v2Script))
      )
    )

    private def emulator(): Emulator = Emulator(
      Map(
        Input(genesisHash, 0) -> Output(Alice.address, Value.ada(100)),
        lockedUtxo.input -> lockedUtxo.output,
        v2LockedUtxo.input -> v2LockedUtxo.output
      )
    )

    private val pv10: CardanoInfo = testEnv.copy(protocolParams =
        testEnv.protocolParams.copy(protocolVersion = ProtocolVersion(10, 0))
    )

    test("spend and reference the same UTxO with a PlutusV2 script at PV 11 is accepted") {
        assert(testEnv.protocolParams.protocolVersion.major == 11)
        val provider = emulator()
        val tx = TxBuilder(testEnv)
            .spend(v2LockedUtxo, Data.I(0))
            .references(v2LockedUtxo)
            .complete(provider, Alice.address)
            .await()
            .sign(Alice.signer)
            .transaction
        val body = tx.body.value
        assert(body.inputs.toSet.contains(v2LockedUtxo.input))
        assert(body.referenceInputs.toSet == Set(v2LockedUtxo.input))
        val result = provider.submit(tx).await()
        assert(result.isRight, s"Submission failed: ${result.left.getOrElse("")}")
        assert(!provider.utxos.contains(v2LockedUtxo.input))
    }

    private def assertNamesOverlap(message: String): Unit = {
        assert(message.contains("ReferenceInputsNotDisjointFromInputs"), message)
        assert(message.contains(lockedUtxo.input.toString), message)
    }

    test("TxBuilder refuses a PlutusV3 spend of a UTxO it also references at PV 11, spec [TX-1]") {
        for builder <- Seq(
              TxBuilder(testEnv).spend(lockedUtxo, Data.I(0)).references(lockedUtxo),
              TxBuilder(testEnv).references(lockedUtxo).spend(lockedUtxo, Data.I(0))
            )
        do
            val error = intercept[TxBuilderException.BalancingException] {
                builder.complete(emulator(), Alice.address).await()
            }
            assert(!error.isScriptFailure, error)
            assertNamesOverlap(error.getMessage)
    }

    test(
      "the Emulator rejects a PlutusV3 spend of a UTxO it also references at PV 11, spec [TX-1]"
    ) {
        // Built at PV 10, where the PlutusV3 context has no overlap check, then submitted at PV 11.
        val provider = emulator()
        val tx = TxBuilder(pv10)
            .spend(lockedUtxo, Data.I(0))
            .references(lockedUtxo)
            .complete(provider, Alice.address)
            .await()
            .sign(Alice.signer)
            .transaction
        val result = provider.submit(tx).await()
        assert(result.isLeft, result)
        result.left.foreach(error => assertNamesOverlap(error.message))
        assert(provider.utxos.contains(lockedUtxo.input))
    }

    test("the validator rejects a spend that does not reference the spent UTxO") {
        val error = intercept[TxBuilderException.BalancingException] {
            TxBuilder(testEnv)
                .spend(lockedUtxo, Data.I(0))
                .complete(emulator(), Alice.address)
                .await()
        }
        assert(error.isScriptFailure, error)
    }

    test("the Emulator rejects spending and referencing the same UTxO at PV 10") {
        val context = {
            val c = Context.testMainnet()
            c.copy(env = c.env.copy(params = pv10.protocolParams))
        }
        val provider = Emulator(
          Map(
            Input(genesisHash, 0) -> Output(Alice.address, Value.ada(100)),
            lockedUtxo.input -> lockedUtxo.output
          ),
          context
        )
        val tx = TxBuilder(pv10)
            .spend(lockedUtxo, Data.I(0))
            .references(lockedUtxo)
            .complete(provider, Alice.address)
            .await()
            .sign(Alice.signer)
            .transaction
        val result = provider.submit(tx).await()
        assert(result.left.map(_.rule) == Left("NonDisjointInputsAndReferenceInputs"), result)
    }
}
