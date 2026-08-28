package scalus.cardano.node

import io.bullet.borer.Cbor
import scalus.interop.{TsName, TsType}
import scalus.uplc.DebugScript
import scalus.uplc.builtin.{ByteString, Data}
import scalus.cardano.address.Address
import scalus.cardano.ledger.rules.{Context, UtxoEnv}
import scalus.cardano.ledger.*
import scalus.cardano.ledger.utils.AllResolvedScripts

import scala.scalajs.js
import scala.scalajs.js.annotation.{JSExportStatic, JSExportTopLevel}
import scala.scalajs.js.typedarray.{byteArray2Int8Array, Uint8Array}

/** An in-memory Cardano ledger for tests and local development.
  *
  * A submitted transaction goes through the same phase-1 checks a node performs (fees, value
  * conservation, signatures, validity interval, min-ada, script and datum witnesses, certificates)
  * and then phase-2 Plutus script execution, over most of the node's UTxO ledger rules. Everything
  * lives in this object: no node, no network, no disk. The ledger starts with what you seed it
  * with, and disappears with the process.
  *
  * @param initialUtxosCbor
  *   The UTxO set to start from, as CBOR: a map whose keys are transaction inputs (a
  *   `[transactionHash, outputIndex]` pair) and whose values are transaction outputs, as in the
  *   Cardano ledger CDDL. This is the same shape `getUtxosCbor` gives back.
  * @param slotConfig
  *   Slot arithmetic for the emulated network, for example `SlotConfig.preview`.
  * @param initialStakeRewards
  *   Stake credentials to register before the first transaction: a map from a hex-encoded 28-byte
  *   script hash to a reward balance in lovelace, written as a decimal string. Only script
  *   credentials can be seeded here; use `Emulator.withState` to seed key credentials. A balance of
  *   `"0"` is the usual case: a reward (staking) validator only runs when a transaction withdraws
  *   from its address, and a withdrawal of zero lovelace is enough, but the address must already be
  *   registered for the transaction to be valid.
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

    /** Validates a transaction and, if it passes, applies it to the ledger state.
      *
      * @param txCborBytes
      *   CBOR bytes of the signed transaction, as it would be sent to a node.
      * @return
      *   `{ isSuccess: true, txHash }` when the transaction was accepted, otherwise
      *   `{ isSuccess: false, error }` naming the rule it broke, plus `logs` when a Plutus script
      *   failed and produced trace output. A rejected transaction leaves the ledger unchanged.
      */
    def submitTx(txCborBytes: Uint8Array): JSubmitResult = {
        val tx = Transaction.fromCbor(txCborBytes.toArray.map(_.toByte))
        formatSubmitResult(emulator.submitSync(tx))
    }

    /** Submits a transaction the same way, but keeps a debug build of each script at hand. A
      * release script that fails with no trace output is replayed with its debug build, so the
      * failure message carries real diagnostics instead of nothing.
      *
      * @param txCborBytes
      *   CBOR bytes of the signed transaction.
      * @param debugScripts
      *   Map from the hex-encoded script hash of a script in the transaction, to the double-CBOR
      *   hex of the debug build of that same script. Its Plutus version is taken from the script in
      *   the transaction; an entry that matches no script there is ignored, with a console warning.
      * @return
      *   The same shape as the one-argument `submitTx`.
      */
    def submitTx(txCborBytes: Uint8Array, debugScripts: js.Dictionary[String]): JSubmitResult = {
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

    private def formatSubmitResult(result: Either[SubmitError, TransactionHash]): JSubmitResult =
        result match {
            case Right(txHash) =>
                js.Dynamic
                    .literal(isSuccess = true, txHash = txHash.toHex)
                    .asInstanceOf[JSubmitResult]
            case Left(submitError) =>
                submitError match {
                    case NodeSubmitError.ScriptFailure(msg, logs, _, _) if logs.nonEmpty =>
                        js.Dynamic
                            .literal(
                              isSuccess = false,
                              error = msg,
                              logs = js.Array(logs*)
                            )
                            .asInstanceOf[JSubmitResult]
                    case _ =>
                        js.Dynamic
                            .literal(isSuccess = false, error = submitError.message)
                            .asInstanceOf[JSubmitResult]
                }
        }

    /** The whole UTxO set in a single CBOR map: keys are transaction inputs (a
      * `[transactionHash, outputIndex]` pair), values are transaction outputs, as in the Cardano
      * ledger CDDL. `getAllUtxos` returns the same data instead as one small map per UTxO.
      */
    def getUtxosCbor(): Uint8Array = {
        val bytes = Cbor.encode(emulator.utxos).toByteArray
        new Uint8Array(byteArray2Int8Array(bytes).buffer)
    }

    /** The UTxOs that sit at one address.
      *
      * @param addressBech32
      *   The address in bech32 form, for example `addr_test1...`.
      * @return
      *   One CBOR map per UTxO, each holding exactly one input-to-output entry, so a single UTxO
      *   can be decoded and passed around on its own.
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

    /** Every UTxO in the ledger.
      *
      * @return
      *   One CBOR map per UTxO, each holding exactly one input-to-output entry. Use `getUtxosCbor`
      *   for the whole set in one map instead.
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

    /** The reward balance of a script stake credential. Key (pub-key) credentials are not supported
      * here; read those with `getDelegation`, which takes any credential.
      *
      * @param scriptHashHex
      *   Hex-encoded 28-byte script hash.
      * @return
      *   The balance in lovelace, or `null` if that credential is not registered.
      */
    def getStakeReward(scriptHashHex: String): js.BigInt | Null = {
        val cred = Credential.ScriptHash(ScriptHash.fromHex(scriptHashHex))
        emulator.certState.dstate.rewards.get(cred) match
            case Some(Coin(amount)) => js.BigInt(amount.toString)
            case None               => null
    }

    /** Moves the clock to an absolute slot, forwards or backwards. Only validity intervals and
      * time-aware scripts see the difference; no blocks are produced in between and no rewards are
      * paid out. A fractional value is truncated.
      */
    def setSlot(slot: Double): Unit = {
        emulator.setSlot(slot.toLong)
    }

    /** The current slot number of the emulator. */
    def getSlot(): Double = emulator.currentContext.env.slot.toDouble

    /** Advance the current slot by `n` slots. */
    def tick(n: Double): Unit = emulator.tick(n.toLong)

    /** Whether a transaction with this hash was accepted by this emulator.
      *
      * @param txHashBytes
      *   The raw 32 bytes of the transaction hash, not a hex string. (`getStakeReward` takes hex;
      *   this one does not.)
      */
    def hasTx(txHashBytes: Uint8Array): Boolean = {
        val hash = TransactionHash.fromByteString(
          ByteString.unsafeFromArray(txHashBytes.toArray.map(_.toByte))
        )
        emulator.hasTx(hash)
    }

    /** The pool a stake credential delegates to, and its reward balance.
      *
      * @param stakeCredentialCbor
      *   The credential itself, CBOR-encoded: `[0, keyHash]` for a key credential or
      *   `[1, scriptHash]` for a script credential, the `stake_credential` of the Cardano ledger
      *   CDDL. The result is a plain object, not CBOR.
      * @return
      *   `poolId` is `null` when the credential delegates to no pool. `rewards` is `0n` both for a
      *   registered credential with no rewards and for one that was never registered, so use
      *   `getStakeReward` when you need to tell the two apart.
      */
    def getDelegation(stakeCredentialCbor: Uint8Array): JDelegationInfo = {
        val bytes = stakeCredentialCbor.toArray.map(_.toByte)
        val cred = Cbor.decode(bytes).to[Credential].value
        val info = emulator.getDelegation(cred)
        val pool = info.poolId match
            case Some(pk) =>
                new Uint8Array(byteArray2Int8Array(pk.bytes).buffer): Uint8Array | Null
            case None => null
        js.Dynamic
            .literal(
              poolId = pool,
              rewards = js.BigInt(info.rewards.value.toString)
            )
            .asInstanceOf[JDelegationInfo]
    }

    /** Looks a datum up by its hash, across the datums seeded into this emulator and the ones
      * witnessed by accepted transactions.
      *
      * @param datumHashBytes
      *   The raw 32 bytes of the datum hash, not a hex string.
      * @return
      *   The datum as CBOR, or `null` if this emulator has never seen it.
      */
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

    /** An independent copy of the whole ledger state: UTxOs, registrations and rewards, the datum
      * store, the accepted transactions, and the current slot. The copy and the original evolve
      * separately from then on, so one expensive setup can branch into several test scenarios
      * without being rebuilt.
      */
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

/** Result of `Emulator.submitTx`. Read `isSuccess` first: the other three fields are each present
  * for one outcome only.
  */
@TsName("SubmitResult")
trait JSubmitResult extends js.Object {
    val isSuccess: Boolean

    /** Transaction hash hex; present on success. */
    val txHash: js.UndefOr[String]

    /** Error message; present on failure. */
    val error: js.UndefOr[String]

    /** Script trace logs; present on script failure. */
    val logs: js.UndefOr[js.Array[String]]
}

/** Delegation info returned by `Emulator.getDelegation`. */
@TsName("DelegationInfo")
trait JDelegationInfo extends js.Object {

    /** Pool key hash bytes, or null if not delegated. */
    val poolId: Uint8Array | Null

    /** Reward balance in lovelace. */
    val rewards: js.BigInt
}

/** Ledger state to seed an emulator with, passed to `Emulator.withState`. Only `utxos` is required.
  */
@TsName("EmulatorInitialState")
trait JEmulatorInitialState extends js.Object {

    /** The starting UTxO set as CBOR: a map from transaction input (a
      * `[transactionHash, outputIndex]` pair) to transaction output, as in the Cardano ledger CDDL.
      */
    val utxos: Uint8Array

    /** Stake credentials that count as already registered, with their reward balances and, if you
      * want, the pool each delegates to.
      */
    val stakeRegistrations: js.UndefOr[js.Array[JStakeRegistration]] = js.undefined

    /** Stake pools that count as already registered, so transactions may delegate to them. */
    val poolRegistrations: js.UndefOr[js.Array[JPoolRegistration]] = js.undefined

    /** DReps that count as already registered, so transactions may delegate votes to them. */
    val drepRegistrations: js.UndefOr[js.Array[JDRepRegistration]] = js.undefined

    /** Datums to put in the emulator's datum store, where `getDatum` looks them up by hash. The
      * store is only a lookup table: every accepted transaction adds the datums it witnesses to it,
      * and validation still requires a transaction to carry the datums it needs.
      */
    val datums: js.UndefOr[js.Array[JDatumEntry]] = js.undefined
}

/** Stake registration entry for `EmulatorInitialState`. */
@TsName("StakeRegistration")
trait JStakeRegistration extends js.Object {

    /** Credential type: "key" for pub key hash, "script" for script hash. */
    @TsType("\"key\" | \"script\"")
    val credentialType: String

    /** Hex-encoded 28-byte credential hash */
    val credentialHash: String

    /** Starting reward balance in lovelace. */
    val rewards: js.BigInt

    /** Hex-encoded 28-byte key hash of the pool this credential delegates to. Leave it out for a
      * credential that is registered but delegates to nothing.
      */
    val delegatedTo: js.UndefOr[String] = js.undefined
}

/** Pool registration entry for `EmulatorInitialState`. */
@TsName("PoolRegistration")
trait JPoolRegistration extends js.Object {

    /** The pool's parameters, as one CBOR-encoded Cardano certificate: the whole 10-element
      * `pool_registration` array, tag `3` first, then operator key hash, VRF key hash, pledge,
      * cost, margin, reward account, owners, relays and metadata. The bare parameter list without
      * the certificate tag is rejected.
      */
    val params: Uint8Array
}

/** DRep registration entry for `EmulatorInitialState`. */
@TsName("DRepRegistration")
trait JDRepRegistration extends js.Object {

    /** Credential type: "key" for pub key hash, "script" for script hash. */
    @TsType("\"key\" | \"script\"")
    val credentialType: String

    /** Hex-encoded 28-byte credential hash */
    val credentialHash: String

    /** Deposit held for this DRep registration, in lovelace. */
    val deposit: js.BigInt

    /** The DRep's metadata anchor as CBOR: a `[url, dataHash]` pair, where `dataHash` is the
      * 32-byte hash of the document the URL points at.
      */
    val anchor: js.UndefOr[Uint8Array] = js.undefined
}

/** Datum entry for `EmulatorInitialState`. */
@TsName("DatumEntry")
trait JDatumEntry extends js.Object {

    /** Hex-encoded 32-byte datum hash */
    val hash: String

    /** The datum itself as CBOR, hex-encoded. Its hash must be `hash`; nothing checks that. */
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

    /** Creates an emulator seeded with a full starting ledger state: UTxOs, and optionally stake
      * credentials, stake pools, DReps and datums. Unlike the constructor, this takes both key and
      * script stake credentials, and takes hashes as hex rather than CBOR.
      *
      * @param state
      *   The starting state. Every field except `utxos` may be left out.
      * @param slotConfig
      *   Slot arithmetic for the emulated network.
      */
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

    /** Creates an emulator whose ledger holds one output per address, each carrying only ada. This
      * is the quickest way to get a funded wallet or two for a test.
      *
      * @param addressesBech32
      *   The addresses to fund, in bech32 form.
      * @param slotConfig
      *   Slot arithmetic for the emulated network.
      * @param lovelacePerAddress
      *   Lovelace in each output. Defaults to `10_000_000_000n`, that is 10 000 ada.
      */
    @JSExportStatic
    def withAddresses(
        addressesBech32: js.Array[String],
        slotConfig: SlotConfig,
        lovelacePerAddress: js.BigInt = js.BigInt(10_000_000_000L)
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
