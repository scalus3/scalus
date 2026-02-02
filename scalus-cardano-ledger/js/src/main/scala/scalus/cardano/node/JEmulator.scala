package scalus.cardano.node

import io.bullet.borer.Cbor
import scalus.cardano.address.Address
import scalus.cardano.ledger.rules.{Context, UtxoEnv}
import scalus.cardano.ledger.*

import scala.scalajs.js
import scala.scalajs.js.annotation.{JSExportStatic, JSExportTopLevel}
import scala.scalajs.js.typedarray.{byteArray2Int8Array, Uint8Array}

/** JavaScript wrapper for the Emulator.
  *
  * Provides a JavaScript-friendly API for the Cardano emulator.
  */
@JSExportTopLevel("Emulator")
class JEmulator(
    initialUtxosCbor: Uint8Array,
    slotConfig: SlotConfig
) extends js.Object {

    private val emulator: Emulator = {
        val utxos: Utxos = Cbor.decode(initialUtxosCbor.toArray.map(_.toByte)).to[Utxos].value
        val env =
            if slotConfig == SlotConfig.mainnet then UtxoEnv.testMainnet()
            else UtxoEnv.default
        val context = new Context(env = env, slotConfig = slotConfig)
        new Emulator(
          initialUtxos = utxos,
          initialContext = context,
          validators = Emulator.defaultValidators,
          mutators = Emulator.defaultMutators
        )
    }

    /** Submit a transaction to the emulator.
      *
      * @return
      *   Object with isSuccess, txHash (on success), or error and logs (on failure)
      */
    def submitTx(txCborBytes: Uint8Array): js.Dynamic = {
        val tx = Transaction.fromCbor(txCborBytes.toArray.map(_.toByte))
        emulator.submitSync(tx) match {
            case Right(txHash) =>
                js.Dynamic.literal(isSuccess = true, txHash = txHash.toHex)
            case Left(submitError) =>
                submitError match {
                    case NodeSubmitError.ScriptFailure(msg, _, logs) if logs.nonEmpty =>
                        js.Dynamic.literal(
                          isSuccess = false,
                          error = msg,
                          logs = js.Array(logs*)
                        )
                    case _ =>
                        js.Dynamic.literal(isSuccess = false, error = submitError.message)
                }
        }
    }

    /** Get all UTxOs as CBOR. */
    def getUtxosCbor(): Uint8Array = {
        val bytes = Cbor.encode(emulator.utxos).toByteArray
        new Uint8Array(byteArray2Int8Array(bytes).buffer)
    }

    /** Get UTxOs for a specific address.
      *
      * @return
      *   Array of CBOR-encoded UTxO entries (each entry is a Map[Input, Output])
      */
    def getUtxosForAddress(addressBech32: String): js.Array[Uint8Array] = {
        val address = Address.fromString(addressBech32)
        val result = js.Array[Uint8Array]()
        emulator.utxos.foreach { case (input, output) =>
            if output.address == address then
                val utxo: Map[Input, Output] = Map(input -> output)
                val bytes = Cbor.encode(utxo).toByteArray
                result.push(new Uint8Array(byteArray2Int8Array(bytes).buffer))
        }
        result
    }

    /** Get all UTxOs as CBOR-encoded entries.
      *
      * @return
      *   Array of CBOR-encoded UTxO entries (each entry is a Map[Input, Output])
      */
    def getAllUtxos(): js.Array[Uint8Array] = {
        val result = js.Array[Uint8Array]()
        emulator.utxos.foreach { case (input, output) =>
            val utxo: Map[Input, Output] = Map(input -> output)
            val bytes = Cbor.encode(utxo).toByteArray
            result.push(new Uint8Array(byteArray2Int8Array(bytes).buffer))
        }
        result
    }

    def setSlot(slot: Double): Unit = {
        emulator.setSlot(slot.toLong)
    }

    def snapshot(): JEmulator = {
        val snapshotEmulator = emulator.snapshot()
        val cbor = Cbor.encode(snapshotEmulator.utxos).toByteArray
        new JEmulator(
          new Uint8Array(byteArray2Int8Array(cbor).buffer),
          emulator.currentContext.slotConfig
        )
    }
}

object JEmulator {

    /** Create an emulator with funded addresses.
      *
      * @param addressesBech32
      *   Array of addresses in bech32 format
      * @param slotConfig
      *   Slot configuration
      * @param lovelacePerAddress
      *   Amount of lovelace per address (default: 10,000 ADA)
      * @return
      *   A new JEmulator with funded addresses
      */
    @JSExportStatic
    def withAddresses(
        addressesBech32: js.Array[String],
        slotConfig: SlotConfig,
        lovelacePerAddress: js.BigInt = js.BigInt(10_000L)
    ): JEmulator = {
        val addresses = addressesBech32.toSeq.map(Address.fromString)
        val value = scalus.cardano.ledger.Value(
          scalus.cardano.ledger.Coin(lovelacePerAddress.toString().toLong)
        )
        val utxos = EmulatorBase.createInitialUtxos(addresses, value)
        val cbor = Cbor.encode(utxos).toByteArray
        new JEmulator(
          new Uint8Array(byteArray2Int8Array(cbor).buffer),
          slotConfig
        )
    }
}
