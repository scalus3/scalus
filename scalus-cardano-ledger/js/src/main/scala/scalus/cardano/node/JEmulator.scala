package scalus.cardano.node

import io.bullet.borer.Cbor
import scalus.uplc.DebugScript
import scalus.uplc.builtin.{ByteString, Data}
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
  *
  * @param initialUtxosCbor
  *   CBOR-encoded initial UTxO set
  * @param slotConfig
  *   Slot configuration
  * @param initialStakeRewards
  *   Optional map from script hash hex to lovelace amount (as a numeric string), pre-registering
  *   stake credentials with the given reward balances. Use `"0"` for the zero-withdrawal trick.
  */
@JSExportTopLevel("Emulator")
class JEmulator(
    initialUtxosCbor: Uint8Array,
    slotConfig: SlotConfig,
    initialStakeRewards: js.Dictionary[String] = js.Dictionary()
) extends js.Object {

    private var emulator: Emulator = {
        val utxos: Utxos = Cbor.decode(initialUtxosCbor.toArray.map(_.toByte)).to[Utxos].value
        val env =
            if slotConfig == SlotConfig.mainnet then UtxoEnv.testMainnet()
            else UtxoEnv.default
        val context = new Context(env = env, slotConfig = slotConfig)
        val rewardsMap: Map[Credential, Coin] = initialStakeRewards.toMap.map {
            case (hashHex, lovelace) =>
                Credential.ScriptHash(ScriptHash.fromHex(hashHex)) -> Coin(lovelace.toLong)
        }
        if rewardsMap.isEmpty then
            new Emulator(
              initialUtxos = utxos,
              initialContext = context,
              validators = Emulator.defaultValidators,
              mutators = Emulator.defaultMutators
            )
        else
            Emulator.withRegisteredStakeCredentials(
              initialUtxos = utxos,
              initialStakeRewards = rewardsMap,
              initialContext = context
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
                    case NodeSubmitError.ScriptFailure(msg, logs, _, _) if logs.nonEmpty =>
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

    /** Get the reward balance for a script-based stake credential.
      *
      * @param scriptHashHex
      *   Hex-encoded script hash
      * @return
      *   Reward amount in lovelace as BigInt, or null if not registered
      */
    def getStakeReward(scriptHashHex: String): js.BigInt | Null = {
        val cred = Credential.ScriptHash(ScriptHash.fromHex(scriptHashHex))
        emulator.certState.dstate.rewards.get(cred) match
            case Some(Coin(amount)) => js.BigInt(amount.toString)
            case None               => null
    }

    def setSlot(slot: Double): Unit = {
        emulator.setSlot(slot.toLong)
    }

    /** Advance the current slot by `n` slots. */
    def tick(n: Double): Unit = emulator.tick(n.toLong)

    /** True once the tx with the given hash has been accepted. */
    def hasTx(txHashBytes: Uint8Array): Boolean = {
        val hash = TransactionHash.fromByteString(
          ByteString.unsafeFromArray(txHashBytes.toArray.map(_.toByte))
        )
        emulator.hasTx(hash)
    }

    /** Delegation + reward balance for a stake credential (CBOR-encoded). */
    def getDelegation(stakeCredentialCbor: Uint8Array): js.Dynamic = {
        val bytes = stakeCredentialCbor.toArray.map(_.toByte)
        val cred = Cbor.decode(bytes).to[Credential].value
        val info = emulator.getDelegation(cred)
        val pool = info.poolId match
            case Some(pk) =>
                new Uint8Array(byteArray2Int8Array(pk.bytes).buffer): Uint8Array | Null
            case None => null
        js.Dynamic.literal(
          poolId = pool,
          rewards = js.BigInt(info.rewards.value.toString)
        )
    }

    /** Look up a datum by hash. Returns `null` if unknown, else CBOR bytes. */
    def getDatum(datumHashBytes: Uint8Array): Uint8Array | Null = {
        val hash = DataHash.fromByteString(
          ByteString.unsafeFromArray(datumHashBytes.toArray.map(_.toByte))
        )
        emulator.datums.get(hash) match
            case Some(data) =>
                val bytes = Cbor.encode(data).toByteArray
                new Uint8Array(byteArray2Int8Array(bytes).buffer)
            case None => null
    }

    def snapshot(): JEmulator = {
        val snapshotEmulator = emulator.snapshot()
        val emptyUtxosCbor = Cbor.encode(Map.empty: Utxos).toByteArray
        val wrapper = new JEmulator(
          new Uint8Array(byteArray2Int8Array(emptyUtxosCbor).buffer),
          emulator.currentContext.slotConfig,
          initialStakeRewards
        )
        JEmulator.replaceEmulator(wrapper, snapshotEmulator)
        wrapper
    }
}

/** JS-shape of [[EmulatorInitialState]]. */
trait JEmulatorInitialState extends js.Object {
    val utxos: Uint8Array
    val stakeRegistrations: js.UndefOr[js.Array[JStakeRegistration]] = js.undefined
    val poolRegistrations: js.UndefOr[js.Array[JPoolRegistration]] = js.undefined
    val drepRegistrations: js.UndefOr[js.Array[JDRepRegistration]] = js.undefined
    val datums: js.UndefOr[js.Array[JDatumEntry]] = js.undefined
}

trait JStakeRegistration extends js.Object {

    /** "key" or "script" */
    val credentialType: String

    /** Hex-encoded 28-byte credential hash */
    val credentialHash: String
    val rewards: js.BigInt

    /** Hex-encoded 28-byte pool key hash (optional) */
    val delegatedTo: js.UndefOr[String] = js.undefined
}

trait JPoolRegistration extends js.Object {

    /** CBOR-encoded PoolRegistration certificate */
    val params: Uint8Array
}

trait JDRepRegistration extends js.Object {

    /** "key" or "script" */
    val credentialType: String

    /** Hex-encoded 28-byte credential hash */
    val credentialHash: String
    val deposit: js.BigInt

    /** CBOR-encoded anchor (optional) */
    val anchor: js.UndefOr[Uint8Array] = js.undefined
}

trait JDatumEntry extends js.Object {

    /** Hex-encoded 32-byte datum hash */
    val hash: String

    /** Hex-encoded CBOR-encoded datum */
    val datum: String
}

object JEmulator {

    private def decodeCbor[T: io.bullet.borer.Decoder](bytes: Uint8Array): T =
        Cbor.decode(bytes.toArray.map(_.toByte)).to[T].value

    private def replaceEmulator(wrapper: JEmulator, e: Emulator): Unit =
        wrapper.emulator = e

    private def parseCredential(credType: String, credHash: String): Credential =
        credType match
            case "key"    => Credential.KeyHash(AddrKeyHash.fromHex(credHash))
            case "script" => Credential.ScriptHash(ScriptHash.fromHex(credHash))
            case other =>
                throw new IllegalArgumentException(
                  s"credentialType must be \"key\" or \"script\", got: \"$other\""
                )

    /** Seeded constructor: accepts initial ledger state with JSON-friendly fields. */
    @JSExportStatic
    def withState(state: JEmulatorInitialState, slotConfig: SlotConfig): JEmulator = {
        val utxos = decodeCbor[Utxos](state.utxos)
        val stakeRegs = state.stakeRegistrations.toOption.toSeq.flatten.map { s =>
            EmulatorStakeRegistration(
              credential = parseCredential(s.credentialType, s.credentialHash),
              rewards = Coin(s.rewards.toString.toLong),
              delegatedTo = s.delegatedTo.toOption.map { hex =>
                  PoolKeyHash.fromHex(hex)
              }
            )
        }
        val poolRegs = state.poolRegistrations.toOption.toSeq.flatten.map { p =>
            val params = decodeCbor[Certificate](p.params) match
                case pr: Certificate.PoolRegistration => pr
                case other =>
                    throw new IllegalArgumentException(
                      s"Expected PoolRegistration certificate, got: $other"
                    )
            EmulatorPoolRegistration(params)
        }
        val drepRegs = state.drepRegistrations.toOption.toSeq.flatten.map { d =>
            EmulatorDRepRegistration(
              credential = parseCredential(d.credentialType, d.credentialHash),
              deposit = Coin(d.deposit.toString.toLong),
              anchor = d.anchor.toOption.map(decodeCbor[Anchor])
            )
        }
        val datumEntries = state.datums.toOption.toSeq.flatten.map { e =>
            DataHash.fromHex(e.hash) -> Data.fromCbor(ByteString.fromHex(e.datum).bytes)
        }.toMap
        val initState = EmulatorInitialState(
          utxos = utxos,
          stakeRegistrations = stakeRegs,
          poolRegistrations = poolRegs,
          drepRegistrations = drepRegs,
          datums = datumEntries
        )
        val env =
            if slotConfig == SlotConfig.mainnet then UtxoEnv.testMainnet()
            else UtxoEnv.default
        val context = new Context(env = env, slotConfig = slotConfig)
        val scalaEmulator = Emulator.withState(initState, context)

        val emptyUtxosCbor = Cbor.encode(Map.empty: Utxos).toByteArray
        val wrapper = new JEmulator(
          new Uint8Array(byteArray2Int8Array(emptyUtxosCbor).buffer),
          slotConfig
        )
        replaceEmulator(wrapper, scalaEmulator)
        wrapper
    }

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
