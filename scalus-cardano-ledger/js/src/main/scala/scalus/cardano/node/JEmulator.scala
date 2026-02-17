package scalus.cardano.node

import io.bullet.borer.Cbor
import scalus.uplc.DebugScript
import scalus.uplc.builtin.ByteString
import scalus.cardano.address.Address
import scalus.cardano.ledger.rules.{Context, UtxoEnv}
import scalus.cardano.ledger.*
import scalus.cardano.ledger.utils.AllResolvedScripts

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
        formatSubmitResult(emulator.submitSync(tx))
    }

    /** Submit a transaction with debug scripts for diagnostic replay.
      *
      * Debug scripts are provided as a dictionary mapping script hash hex to double-CBOR hex of the
      * debug-compiled script. The language version is resolved from the release script in the
      * transaction.
      *
      * @param txCborBytes
      *   CBOR-encoded transaction bytes
      * @param debugScripts
      *   dictionary mapping scriptHashHex to doubleCborHex of debug script
      * @return
      *   Object with isSuccess, txHash (on success), or error and logs (on failure)
      */
    def submitTx(txCborBytes: Uint8Array, debugScripts: js.Dictionary[String]): js.Dynamic = {
        val tx = Transaction.fromCbor(txCborBytes.toArray.map(_.toByte))

        // Resolve scripts from the transaction to determine language versions
        val resolvedScripts = AllResolvedScripts.allResolvedScriptsMap(tx, emulator.utxos) match
            case Right(map) => map
            case Left(error) =>
                js.Dynamic.global.console.error(
                  s"Emulator.submitTx(debugScripts): failed to resolve scripts: $error"
                )
                Map.empty[ScriptHash, Script]

        // Parse debug scripts dictionary
        val debugScriptsMap: Map[ScriptHash, DebugScript] = debugScripts.flatMap {
            case (hashHex, doubleCborHex) =>
                val hash = ScriptHash.fromHex(hashHex)
                val doubleCbor = ByteString.fromHex(doubleCborHex)
                // Determine language from the release script in the transaction
                val languageOpt = resolvedScripts.get(hash).collect { case ps: PlutusScript =>
                    ps.language
                }
                if languageOpt.isEmpty then
                    js.Dynamic.global.console.warn(
                      s"Debug script for hash $hashHex was provided but no matching Plutus script was found in the transaction."
                    )
                languageOpt.map { language =>
                    val plutusScript: PlutusScript = language match
                        case Language.PlutusV1 => Script.PlutusV1(doubleCbor)
                        case Language.PlutusV2 => Script.PlutusV2(doubleCbor)
                        case Language.PlutusV3 => Script.PlutusV3(doubleCbor)
                        case _                 => Script.PlutusV3(doubleCbor)
                    hash -> DebugScript(plutusScript)
                }
        }.toMap

        formatSubmitResult(emulator.submitSync(tx, debugScriptsMap))
    }

    private def formatSubmitResult(result: Either[SubmitError, TransactionHash]): js.Dynamic =
        result match {
            case Right(txHash) =>
                js.Dynamic.literal(isSuccess = true, txHash = txHash.toHex)
            case Left(submitError) =>
                submitError match {
                    case NodeSubmitError.ScriptFailure(msg, logs, _) if logs.nonEmpty =>
                        js.Dynamic.literal(
                          isSuccess = false,
                          error = msg,
                          logs = js.Array(logs*)
                        )
                    case _ =>
                        js.Dynamic.literal(isSuccess = false, error = submitError.message)
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
