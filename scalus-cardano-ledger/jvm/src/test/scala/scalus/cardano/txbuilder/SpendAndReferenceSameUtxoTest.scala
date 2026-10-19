package scalus.cardano.txbuilder

import org.scalatest.funsuite.AnyFunSuite
import scalus.compiler.{Compile, Options}
import scalus.uplc.builtin.{ByteString, Data}
import scalus.uplc.PlutusV3
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

/** From protocol version 11 a transaction may spend a UTxO and reference it too. A validator may
  * then find the UTxO it guards in `txInfoReferenceInputs`, and an SDK may read from and collect
  * from one UTxO in one transaction. The ledger resolves the reference script of a spent input
  * without a reference, so a spend needs no reference for that.
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

    private def emulator(): Emulator = Emulator(
      Map(
        Input(genesisHash, 0) -> Output(Alice.address, Value.ada(100)),
        lockedUtxo.input -> lockedUtxo.output
      )
    )

    private def assertAccepted(builder: TxBuilder): Unit = {
        assert(testEnv.protocolParams.protocolVersion.major == 11)
        val provider = emulator()
        val tx = builder
            .complete(provider, Alice.address)
            .await()
            .sign(Alice.signer)
            .transaction
        val body = tx.body.value
        assert(body.inputs.toSet.contains(lockedUtxo.input))
        assert(body.referenceInputs.toSet == Set(lockedUtxo.input))
        val result = provider.submit(tx).await()
        assert(result.isRight, s"Submission failed: ${result.left.getOrElse("")}")
        assert(!provider.utxos.contains(lockedUtxo.input))
    }

    test("spend then reference the same UTxO at PV 11 is accepted by the Emulator") {
        assertAccepted(TxBuilder(testEnv).spend(lockedUtxo, Data.I(0)).references(lockedUtxo))
    }

    test("reference then spend the same UTxO at PV 11 is accepted by the Emulator") {
        assertAccepted(TxBuilder(testEnv).references(lockedUtxo).spend(lockedUtxo, Data.I(0)))
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
        val pv10 = testEnv.copy(protocolParams =
            testEnv.protocolParams.copy(protocolVersion = ProtocolVersion(10, 0))
        )
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
