package scalus.cardano.node

import io.bullet.borer.Cbor
import scalus.interop.{TsName, TsType}
import scalus.uplc.DebugScript
import scalus.uplc.builtin.{ByteString, Data}
import scalus.uplc.eval.JScalus
import scalus.cardano.address.{Address, StakeAddress}
import scalus.cardano.ledger.rules.{Context, UtxoEnv}
import scalus.cardano.ledger.*
import scalus.cardano.ledger.utils.AllResolvedScripts

import scala.annotation.nowarn
import scala.util.control.NonFatal
import scala.scalajs.js
import scala.scalajs.js.JSConverters.*
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
  * Build one with `Emulator.create`. The constructor below is deprecated: it takes protocol
  * parameters from the slot configuration alone, so a `SlotConfig.preview` emulator validated
  * transactions against mainnet's parameters. `Emulator.create` takes a `CardanoInfo`, in which the
  * two cannot disagree.
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
class JEmulator @deprecated("use Emulator.create", "1.2.0") (
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

    /** Network, slot configuration and protocol parameters this emulator validates transactions
      * against, as one coherent triple. For an emulator built with `Emulator.create`, this is
      * exactly the `CardanoInfo` it was created from; for the deprecated constructors, protocol
      * parameters come from `slotConfig` alone (mainnet parameters for `SlotConfig.mainnet`,
      * `UtxoEnv.default`'s parameters otherwise), which is the mismatch `Emulator.create` fixes.
      */
    def getCardanoInfo(): JsCardanoInfo = JsCardanoInfo.wrap(emulator.cardanoInfo)

    /** The protocol parameters this emulator validates transactions against. Equivalent to
      * `getCardanoInfo().protocolParams`.
      */
    def getProtocolParameters(): JsProtocolParams = getCardanoInfo().protocolParams

    /** Runs every Plutus script the transaction triggers and reports what each one costs, resolving
      * inputs against this emulator's UTxO set, slot config, cost models and protocol version.
      *
      * The standalone `evalPlutusScripts` needs all of that passed in, and getting any of it wrong
      * produces plausible, wrong budgets. Here there is nothing to get wrong.
      *
      * Unlike `submitTx`, this throws rather than returning a result object: it answers a question
      * about a transaction, and has no shape in which to say "there was no transaction". Bytes from
      * an untrusted source therefore need a `try`.
      *
      * @throws PlutusScriptEvaluationError
      *   if a script fails; it carries the failure message and that script's trace logs.
      * @throws Error
      *   if `txCborBytes` does not decode as a transaction.
      */
    def evaluateTx(txCborBytes: Uint8Array): js.Array[JScalus.Redeemer] =
        evaluateTxWith(txCborBytes, Map.empty)

    /** As above, plus UTxOs the emulator does not hold - outputs of a transaction not yet
      * submitted, typically.
      */
    def evaluateTx(
        txCborBytes: Uint8Array,
        additionalUtxos: js.Array[JsUtxo]
    ): js.Array[JScalus.Redeemer] =
        evaluateTxWith(
          txCborBytes,
          additionalUtxos.toSeq.map(u => u.input -> u.output).toMap
        )

    private def evaluateTxWith(
        txCborBytes: Uint8Array,
        extra: Utxos
    ): js.Array[JScalus.Redeemer] = {
        val tx = Transaction.fromCbor(txCborBytes.toArray.map(_.toByte))
        val info = emulator.cardanoInfo
        val evaluator = PlutusScriptEvaluator(
          slotConfig = info.slotConfig,
          initialBudget = ExUnits(Long.MaxValue, Long.MaxValue),
          protocolMajorVersion = info.majorProtocolVersion,
          costModels = info.protocolParams.costModels,
          mode = EvaluatorMode.EvaluateAndComputeCost
        )
        try
            evaluator
                .evalPlutusScripts(tx, emulator.utxos ++ extra)
                .map { r =>
                    new JScalus.Redeemer(
                      tag = r.tag.toString,
                      index = r.index,
                      budget = JScalus.JSExUnits(
                        steps = js.BigInt(r.exUnits.steps.toString),
                        memory = js.BigInt(r.exUnits.memory.toString)
                      )
                    )
                }
                .toJSArray
        catch
            case e: PlutusScriptEvaluationException =>
                throw js.JavaScriptException(
                  JScalus.JSPlutusScriptEvaluationError(e.getMessage, js.Array(e.logs*))
                )
    }

    /** Validates a transaction and, if it passes, applies it to the ledger state.
      *
      * Never throws: bytes that are not a transaction at all come back as a failure result with
      * `errorRule: "InvalidTransaction"`, the same shape as a rejection, so a caller handling
      * untrusted bytes needs one branch rather than a branch and a `try`.
      *
      * @param txCborBytes
      *   CBOR bytes of the signed transaction, as it would be sent to a node.
      * @return
      *   `{ isSuccess: true, txHash }` when the transaction was accepted, otherwise
      *   `{ isSuccess: false, error, errorRule }`, where `errorRule` is the condition that rejected
      *   it and `logs` carries the script's trace output (empty unless a Plutus script failed and
      *   produced any). A rejected transaction leaves the ledger unchanged.
      */
    def submitTx(txCborBytes: Uint8Array): JSubmitResult =
        decodeTx(txCborBytes) match
            case Right(tx)      => formatSubmitResult(emulator.submitSync(tx))
            case Left(rejected) => rejected

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
        val tx = decodeTx(txCborBytes) match
            case Right(tx)      => tx
            case Left(rejected) => return rejected

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

    /** The transaction in `txCborBytes`, or the failure result to hand back instead.
      *
      * `submitTx` promises a result object for every input, and a caller feeding it bytes off the
      * wire is the case an emulator-backed harness is for. A decode failure is a rejection like any
      * other: a node would refuse these bytes too, just earlier.
      */
    private def decodeTx(txCborBytes: Uint8Array): Either[JSubmitResult, Transaction] =
        try Right(Transaction.fromCbor(txCborBytes.toArray.map(_.toByte)))
        catch
            case NonFatal(e) =>
                Left(
                  formatSubmitResult(
                    Left(
                      NodeSubmitError.ValidationError(
                        s"the bytes are not a transaction: ${e.getMessage}",
                        Some("InvalidTransaction")
                      )
                    )
                  )
                )

    private def formatSubmitResult(result: Either[SubmitError, TransactionHash]): JSubmitResult =
        result match {
            case Right(txHash) =>
                js.Dynamic
                    .literal(isSuccess = true, txHash = txHash.toHex, logs = js.Array[String]())
                    .asInstanceOf[JSubmitResult]
            case Left(submitError) =>
                val logs = submitError match
                    case NodeSubmitError.ScriptFailure(_, l, _, _) => js.Array(l*)
                    case _                                         => js.Array[String]()
                js.Dynamic
                    .literal(
                      isSuccess = false,
                      error = submitError.message,
                      errorRule = submitError.rule,
                      logs = logs
                    )
                    .asInstanceOf[JSubmitResult]
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
    @deprecated("use getUtxos(filter) with an address filter", "1.2.0")
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
    @deprecated("use getUtxos()", "1.2.0")
    def getAllUtxos(): js.Array[Uint8Array] = {
        val result = js.Array[Uint8Array]()
        emulator.utxos.foreach { case (input, output) =>
            val utxo: Map[Input, Output] = Map(input -> output)
            val bytes = Cbor.encode(utxo).toByteArray
            result.push(new Uint8Array(byteArray2Int8Array(bytes).buffer))
        }
        result
    }

    /** Every UTxO in the ledger, as `Utxo` handles. */
    def getUtxos(): js.Array[JsUtxo] = wrapAll(emulator.utxos)

    /** The UTxOs matching `filter`. Filtering happens in the ledger, so only matches cross into
      * JavaScript - no `Utxo` handle is ever built for a row the filter would drop.
      *
      * Every field given is ANDed together; an empty filter matches everything. `outRefs` is the
      * one field that is itself a disjunction: it matches any of the given refs, not all of them.
      *
      * @throws Error
      *   if the filter carries a field `UtxoFilter` does not declare. An unknown field cannot be
      *   honoured, and ignoring it would silently widen the query - a misspelt `{ adress: ... }`
      *   would hand back every UTxO in the ledger rather than one address's.
      */
    def getUtxos(filter: JsUtxoFilter): js.Array[JsUtxo] =
        wrapAll(emulator.findUtxosSync(toQuery(filter)))

    private def wrapAll(utxos: Utxos): js.Array[JsUtxo] = {
        val out = js.Array[JsUtxo]()
        utxos.foreach { case (input, output) => out.push(JsUtxo.wrap(input, output)) }
        out
    }

    /** Translates a `JsUtxoFilter` into the `UtxoQuery` algebra, so the filtering itself runs in
      * Scala over `emulator.findUtxosSync`.
      *
      * A field this does not recognise is rejected, not ignored. Ignoring one only ever widens the
      * query, and a filter with no recognised field at all matches the entire ledger - so a
      * JavaScript caller who typed `{ adress: alice }` would be handed everyone's UTxOs as though
      * they were the wallet's, with nothing to signal the mistake. A wrong answer is worse here
      * than an exception. TypeScript callers already get this from `UtxoFilter`'s excess-property
      * check; the throw is what gives untyped callers the same guarantee.
      *
      * `paymentCredential` has no key/script discriminator on the JS side: it is a bare hex hash.
      * It is translated as "matches as a key-hash payment part, or matches as a script-hash payment
      * part", combined with `||`. That is the only translation covering the field's own contract
      * ("matches every address with this payment part") without knowing in advance which kind of
      * credential the hash names.
      *
      * @throws IllegalArgumentException
      *   if `filter` carries any field other than the ones `JsUtxoFilter` declares.
      */
    private def toQuery(filter: JsUtxoFilter): UtxoQuery = {
        val unknown = js.Object.keys(filter).toSeq.filterNot(JEmulator.utxoFilterFields.contains)
        if unknown.nonEmpty then
            throw new IllegalArgumentException(
              s"unknown UtxoFilter field(s): ${unknown.mkString(", ")}; " +
                  s"known fields are ${JEmulator.utxoFilterFields.toSeq.sorted.mkString(", ")}"
            )

        val sources: List[UtxoSource] = List(
          filter.address.toOption.map(a => UtxoSource.FromAddress(Address.fromString(a))),
          filter.paymentCredential.toOption.map { hex =>
              UtxoSource.FromPaymentCredential(Credential.KeyHash(AddrKeyHash.fromHex(hex))) ||
              UtxoSource.FromPaymentCredential(Credential.ScriptHash(ScriptHash.fromHex(hex)))
          },
          filter.txHash.toOption.map(hex =>
              UtxoSource.FromTransaction(TransactionHash.fromHex(hex))
          ),
          filter.outRefs.toOption.map(refs => UtxoSource.FromInputs(refs.toSeq.map(toInput).toSet)),
          // "lovelace" is not a source in the algebra - every UTxO already carries lovelace - so
          // it contributes nothing, which is the same as matching everything.
          filter.unit.toOption.filter(_ != "lovelace").map { unit =>
              val policyId = ScriptHash.fromHex(unit.take(56))
              val assetName = AssetName.fromHex(unit.drop(56))
              UtxoSource.FromAsset(policyId, assetName)
          }
        ).flatten

        val source: UtxoSource = sources match
            case Nil          => UtxoSource.FromInputs(emulator.utxos.keySet)
            case head :: tail => tail.foldLeft(head)(_ && _)

        val withSource: UtxoQuery = UtxoQuery(source)
        val withMinLovelace: UtxoQuery = filter.minLovelace.toOption
            .map(min => withSource && UtxoFilter.MinLovelace(Coin(BigInt(min.toString).toLong)))
            .getOrElse(withSource)

        // Applied last, after every source and filter above - not before.
        filter.limit.toOption.map(n => withMinLovelace.limit(n.toInt)).getOrElse(withMinLovelace)
    }

    private def toInput(ref: JsOutRef): TransactionInput =
        TransactionInput(TransactionHash.fromHex(ref.txHash), ref.outputIndex.toInt)

    /** The reward balance of a stake credential, key or script.
      *
      * @param rewardAddressBech32
      *   The reward (stake) address in bech32 form, for example `stake1...` or `stake_test1...`.
      *   The address carries which kind of credential it names, so this works for a key credential
      *   too - unlike the deprecated `Uint8Array` form this replaces, which only ever built a
      *   script credential.
      * @return
      *   The balance in lovelace, or `undefined` if that credential is not registered.
      */
    def getStakeReward(rewardAddressBech32: String): js.UndefOr[js.BigInt] = {
        val cred = rewardAddressCredential(rewardAddressBech32)
        emulator.certState.dstate.rewards
            .get(cred)
            .map(c => js.BigInt(c.value.toString))
            .orUndefined
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

    /** POSIX time in milliseconds at which the emulator's current slot starts. Equivalent to
      * `getCardanoInfo().slotConfig.slotToTime(getSlot())`.
      */
    def getTime(): Double = emulator.currentContext.slotConfig.slotToTime(getSlot())

    /** Moves the clock to the slot containing this POSIX time - the inverse of `getTime`. A
      * fractional slot is truncated, the same as `setSlot`.
      */
    def setTime(posixMillis: Double): Unit =
        setSlot(emulator.currentContext.slotConfig.timeToSlot(posixMillis))

    /** Whether a transaction with this hash was accepted by this emulator.
      *
      * @param txHashHex
      *   Hex-encoded 32-byte transaction hash.
      */
    def hasTx(txHashHex: String): Boolean =
        emulator.hasTx(TransactionHash.fromHex(txHashHex))

    /** Whether a transaction with this hash was accepted by this emulator, as a status string
      * rather than a boolean. Always one of the two values below: with no mempool and immediate
      * application, there is no pending state to report.
      *
      * @param txHashHex
      *   Hex-encoded 32-byte transaction hash.
      */
    @TsType("\"Confirmed\" | \"NotFound\"")
    def getTransactionStatus(txHashHex: String): String =
        if hasTx(txHashHex) then "Confirmed" else "NotFound"

    /** Looks up a previously applied transaction by hash.
      *
      * @param txHashHex
      *   Hex-encoded 32-byte transaction hash.
      * @return
      *   The transaction as CBOR, or `undefined` if this emulator never applied it.
      */
    def getTransaction(txHashHex: String): js.UndefOr[Uint8Array] =
        emulator
            .getTransaction(TransactionHash.fromHex(txHashHex))
            .map(tx => new Uint8Array(byteArray2Int8Array(tx.toCbor).buffer))
            .orUndefined

    /** Every transaction this emulator has applied, oldest first. */
    def getAppliedTxs(): js.Array[JAppliedTxInfo] = {
        val out = js.Array[JAppliedTxInfo]()
        emulator.appliedTxLog.foreach { applied =>
            out.push(
              js.Dynamic
                  .literal(txHash = applied.txHash.toHex, slot = applied.slot.toDouble)
                  .asInstanceOf[JAppliedTxInfo]
            )
        }
        out
    }

    /** The pool a stake credential delegates to, and its reward balance.
      *
      * @param rewardAddressBech32
      *   The reward (stake) address in bech32 form, for example `stake1...` or `stake_test1...`.
      * @return
      *   `poolId` is `undefined` when the credential delegates to no pool. `rewards` is `0n` both
      *   for a registered credential with no rewards and for one that was never registered, so use
      *   `getStakeReward` when you need to tell the two apart.
      */
    def getDelegation(rewardAddressBech32: String): JDelegationInfo = {
        val info = emulator.getDelegation(rewardAddressCredential(rewardAddressBech32))
        js.Dynamic
            .literal(
              poolId = info.poolId.map(_.toHex).orUndefined,
              rewards = js.BigInt(info.rewards.value.toString)
            )
            .asInstanceOf[JDelegationInfo]
    }

    /** Parses a bech32 reward (stake) address into its credential - the shared parsing
      * `getDelegation` and `getStakeReward` need.
      *
      * @throws IllegalArgumentException
      *   if `rewardAddressBech32` is not bech32 (or Base58) at all, or if it decodes cleanly but to
      *   something other than a stake address, such as a payment address.
      */
    private def rewardAddressCredential(rewardAddressBech32: String): Credential = {
        // `Address.fromString` tries bech32 and then Byron Base58, and reports whichever of the two
        // decoders got furthest - `Expected array of 2` or `Invalid Base58 character`, neither of
        // which tells a JavaScript caller that a `stake1...` string was what was wanted here.
        val address =
            try Address.fromString(rewardAddressBech32)
            catch
                case NonFatal(e) =>
                    throw new IllegalArgumentException(
                      s"expected a bech32 reward address (stake1.../stake_test1...), got " +
                          s"`$rewardAddressBech32`: ${e.getMessage}"
                    )
        address match
            case stakeAddress: StakeAddress => stakeAddress.credential
            case other =>
                throw new IllegalArgumentException(
                  s"expected a bech32 reward address (stake1.../stake_test1...), got: ${other.getClass.getSimpleName}"
                )
    }

    /** Live stake per registered stake credential: the lovelace sitting at addresses delegating to
      * it, plus its reward balance, and the pool it delegates to. Not a reward calculation -
      * nothing is paid out.
      */
    def getStakeDistribution(): js.Array[JsStakeDistributionEntry] = {
        val out = js.Array[JsStakeDistributionEntry]()
        emulator.stakeDistribution.foreach { entry =>
            out.push(
              js.Dynamic
                  .literal(
                    credential = credentialHex(entry.credential),
                    pool = entry.pool.map(_.toHex).orUndefined,
                    stake = js.BigInt(entry.stake.value.toString),
                    rewards = js.BigInt(entry.rewards.value.toString)
                  )
                  .asInstanceOf[JsStakeDistributionEntry]
            )
        }
        out
    }

    private def credentialHex(credential: Credential): String = credential match
        case Credential.KeyHash(hash)    => hash.toHex
        case Credential.ScriptHash(hash) => hash.toHex

    /** Looks a datum up by its hash, across the datums seeded into this emulator and the ones
      * witnessed by accepted transactions.
      *
      * @param datumHashHex
      *   Hex-encoded 32-byte datum hash.
      * @return
      *   The datum as CBOR, or `undefined` if this emulator has never seen it.
      */
    def getDatum(datumHashHex: String): js.UndefOr[Uint8Array] = {
        val hash = DataHash.fromHex(datumHashHex)
        emulator.datums
            .get(hash)
            .map { data =>
                val bytes = Cbor.encode(data).toByteArray
                new Uint8Array(byteArray2Int8Array(bytes).buffer)
            }
            .orUndefined
    }

    /** Adds a UTxO to the ledger directly, bypassing transaction validation. Useful for seeding a
      * UTxO a later transaction will spend, without the ceremony of a genesis transaction for it.
      * Overwrites any UTxO already sitting at the same input.
      *
      * @throws Error
      *   if `utxo` is not a `Utxo`. A plain `{ txHash, outputIndex, address, value }` object is the
      *   natural thing for an untyped caller to build, and it carries none of the parsed ledger
      *   pair this reads, so it would otherwise fail a frame later with an unreadable
      *   `undefined (of class java.lang.Void)`.
      */
    def addUtxo(utxo: JsUtxo): Unit = {
        if !utxo.isInstanceOf[JsUtxo] then
            throw new IllegalArgumentException(
              "addUtxo expects a Utxo, not a plain object: build one with " +
                  "`new Utxo(txHash, outputIndex, address, value)`, or take one from " +
                  "`getUtxos()` or `Utxo.fromCbor(bytes)`"
            )
        emulator.addUtxo(JsUtxo.input(utxo), JsUtxo.output(utxo))
    }

    /** Removes a UTxO from the ledger directly, bypassing transaction validation. A no-op if no
      * UTxO sits at that input.
      */
    def removeUtxo(outRef: JsOutRef): Unit =
        emulator.removeUtxo(toInput(outRef))

    /** An independent copy of the whole ledger state: UTxOs, registrations and rewards, the datum
      * store, the accepted transactions, and the current slot. The copy and the original evolve
      * separately from then on, so one expensive setup can branch into several test scenarios
      * without being rebuilt.
      */
    @nowarn("cat=deprecation") // constructing this class's own handle to write a snapshot into
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

/** Result of `Emulator.submitTx`. Read `isSuccess` first: `txHash` is present only on success,
  * `error`/`errorRule` only on failure. `logs` is the exception - always an array, empty when there
  * is nothing to report.
  */
@TsName("SubmitResult")
trait JSubmitResult extends js.Object {
    val isSuccess: Boolean

    /** Transaction hash hex; present on success. */
    val txHash: js.UndefOr[String]

    /** Error message; present on failure. */
    val error: js.UndefOr[String]

    /** The condition that rejected the transaction; present on failure.
      *
      * Four conditions have a name of their own, because a caller branches on them rather than on
      * the rule: `UtxoNotAvailable` (an input this transaction spends is not in the ledger - the
      * ledger rules `BadInputsUTxO` and `BadAllInputsUTxO` are deliberately folded into this one
      * name, since they are one condition), `TransactionExpired`, `ValueNotConserved` and
      * `ScriptFailure` (a native or a Plutus script did not validate). Bytes that are not a
      * transaction at all give `InvalidTransaction`.
      *
      * Every other rejection reports the ledger rule's own name: `FeesOk`, `MissingKeyHashes`,
      * `BadCollateralInputsUTxO`, `BadReferenceInputsUTxO`, `InvalidScriptDataHash`, `Datums`,
      * `ExUnitsExceedMax`, `WithdrawalsNotInRewards` and the rest.
      *
      * Stable enough to assert on in a test, unlike [[error]]; a name here does not minify away or
      * otherwise change under refactoring, unlike `getClass.getSimpleName`.
      */
    val errorRule: js.UndefOr[String]

    /** Script trace logs, oldest first. Always an array; empty when the script produced none. */
    val logs: js.Array[String]
}

/** Delegation info returned by `Emulator.getDelegation`. */
@TsName("DelegationInfo")
trait JDelegationInfo extends js.Object {

    /** Hex-encoded 28-byte pool key hash, or `undefined` if not delegated. */
    val poolId: js.UndefOr[String]

    /** Reward balance in lovelace. */
    val rewards: js.BigInt
}

/** One row of `Emulator.getAppliedTxs`. */
@TsName("AppliedTxInfo")
trait JAppliedTxInfo extends js.Object {

    /** Hex-encoded 32-byte transaction hash. */
    val txHash: String

    /** The emulator slot at the time this transaction was applied. */
    val slot: Double
}

/** One row of `Emulator.getStakeDistribution`. */
@TsName("StakeDistributionEntry")
trait JsStakeDistributionEntry extends js.Object {

    /** Hex-encoded 28-byte credential hash - a key hash or a script hash, with no discriminator,
      * the same convention `UtxoFilter.paymentCredential` uses.
      */
    val credential: String

    /** Hex-encoded 28-byte pool key hash, or `undefined` if this credential delegates to no pool.
      */
    val pool: js.UndefOr[String]

    /** Live stake: the lovelace sitting at addresses delegating to this credential. */
    val stake: js.BigInt

    /** This credential's reward balance. */
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

/** Everything an emulator may start with beyond its chain parameters, passed to `Emulator.create`.
  * Every field is optional.
  */
@TsName("EmulatorOptions")
trait JsEmulatorOptions extends js.Object {

    /** UTxOs to start from. */
    val utxos: js.UndefOr[js.Array[JsUtxo]] = js.undefined

    /** Starting slot. Defaults to the slot containing `Date.now()`. */
    val slot: js.UndefOr[Double] = js.undefined

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

/** Identifies one transaction output: the pair a `TransactionInput` is made of. */
@TsName("OutRef")
trait JsOutRef extends js.Object {
    val txHash: String
    val outputIndex: Double
}

/** Narrows `getUtxos`. Every field is optional; several may be combined, and they are ANDed. */
@TsName("UtxoFilter")
trait JsUtxoFilter extends js.Object {
    val address: js.UndefOr[String] = js.undefined

    /** Hex payment-credential hash: matches every address with this payment part, whatever its
      * stake part. This is the query a wallet wants.
      */
    val paymentCredential: js.UndefOr[String] = js.undefined

    /** `"lovelace"`, or a policy id and asset name concatenated as hex. */
    val unit: js.UndefOr[String] = js.undefined

    /** Matches a UTxO whose input is any of these - a disjunction over this array, unlike the other
      * fields, which AND together.
      */
    val outRefs: js.UndefOr[js.Array[JsOutRef]] = js.undefined
    val txHash: js.UndefOr[String] = js.undefined
    val minLovelace: js.UndefOr[js.BigInt] = js.undefined

    /** Applied last, after every other field above has been matched. */
    val limit: js.UndefOr[Double] = js.undefined
}

object JEmulator {

    /** Every field name [[JsUtxoFilter]] declares - the whitelist `toQuery` rejects a filter
      * against. Keep it in step with the trait: a field added there and forgotten here would be
      * rejected rather than honoured.
      */
    private val utxoFilterFields: Set[String] =
        Set(
          "address",
          "paymentCredential",
          "unit",
          "outRefs",
          "txHash",
          "minLovelace",
          "limit"
        )

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

    private def parseStakeRegistrations(
        regs: js.UndefOr[js.Array[JStakeRegistration]]
    ): Seq[EmulatorStakeRegistration] =
        regs.toOption.toSeq.flatten.map { s =>
            EmulatorStakeRegistration(
              credential = parseCredential(s.credentialType, s.credentialHash),
              rewards = Coin(s.rewards.toString.toLong),
              delegatedTo = s.delegatedTo.toOption.map { hex =>
                  PoolKeyHash.fromHex(hex)
              }
            )
        }

    private def parsePoolRegistrations(
        regs: js.UndefOr[js.Array[JPoolRegistration]]
    ): Seq[EmulatorPoolRegistration] =
        regs.toOption.toSeq.flatten.map { p =>
            val params = decodeCbor[Certificate](p.params) match
                case pr: Certificate.PoolRegistration => pr
                case other =>
                    throw new IllegalArgumentException(
                      s"Expected PoolRegistration certificate, got: $other"
                    )
            EmulatorPoolRegistration(params)
        }

    private def parseDrepRegistrations(
        regs: js.UndefOr[js.Array[JDRepRegistration]]
    ): Seq[EmulatorDRepRegistration] =
        regs.toOption.toSeq.flatten.map { d =>
            EmulatorDRepRegistration(
              credential = parseCredential(d.credentialType, d.credentialHash),
              deposit = Coin(d.deposit.toString.toLong),
              anchor = d.anchor.toOption.map(decodeCbor[Anchor])
            )
        }

    private def parseDatums(entries: js.UndefOr[js.Array[JDatumEntry]]): Map[DataHash, Data] =
        entries.toOption.toSeq.flatten.map { e =>
            DataHash.fromHex(e.hash) -> Data.fromCbor(ByteString.fromHex(e.datum).bytes)
        }.toMap

    // The deprecated constructor is still the only way to obtain a `JEmulator` instance to write
    // the real, already-built `Emulator` into - see `JsUtxo.wrap`'s doc for the same placeholder
    // pattern. Internal use is intentional, hence the suppression.
    @nowarn("cat=deprecation")
    private def wrapScalaEmulator(scalaEmulator: Emulator, slotConfig: SlotConfig): JEmulator = {
        val emptyUtxosCbor = Cbor.encode(Map.empty: Utxos).toByteArray
        val wrapper = new JEmulator(
          new Uint8Array(byteArray2Int8Array(emptyUtxosCbor).buffer),
          slotConfig
        )
        replaceEmulator(wrapper, scalaEmulator)
        wrapper
    }

    /** An emulator for a network, with an empty ledger.
      *
      * @param info
      *   Network, slot configuration and protocol parameters, as one coherent triple - see the
      *   class doc for why this replaces a bare `slotConfig` parameter.
      */
    @JSExportStatic
    def create(info: JsCardanoInfo): JEmulator =
        create(info, js.Object().asInstanceOf[JsEmulatorOptions])

    /** An emulator for a network, seeded with UTxOs and registrations.
      *
      * Unlike the older constructors, protocol parameters, network id and slot config come from one
      * `CardanoInfo`, so they cannot disagree - fixing the bug where a `SlotConfig.preview`
      * emulator would validate transactions against mainnet's protocol parameters.
      *
      * @param info
      *   Network, slot configuration and protocol parameters, as one coherent triple.
      * @param options
      *   UTxOs and registrations to start from. Every field is optional.
      */
    @JSExportStatic
    def create(info: JsCardanoInfo, options: JsEmulatorOptions): JEmulator = {
        val cardanoInfo = info.underlying
        val utxos: Utxos = options.utxos.toOption.toSeq.flatten.map { u =>
            JsUtxo.input(u) -> JsUtxo.output(u)
        }.toMap
        val slot: SlotNo = options.slot.toOption
            .map(_.toLong)
            .getOrElse {
                val nowMillis: Double = System.currentTimeMillis().toDouble
                math.floor(cardanoInfo.slotConfig.timeToSlot(nowMillis)).toLong
            }
        val initState = EmulatorInitialState(
          utxos = utxos,
          stakeRegistrations = parseStakeRegistrations(options.stakeRegistrations),
          poolRegistrations = parsePoolRegistrations(options.poolRegistrations),
          drepRegistrations = parseDrepRegistrations(options.drepRegistrations),
          datums = parseDatums(options.datums)
        )
        val context = Context(
          env = UtxoEnv(slot, cardanoInfo.protocolParams, CertState.empty, cardanoInfo.network),
          slotConfig = cardanoInfo.slotConfig
        )
        wrapScalaEmulator(Emulator.withState(initState, context), cardanoInfo.slotConfig)
    }

    /** Creates an emulator seeded with a full starting ledger state: UTxOs, and optionally stake
      * credentials, stake pools, DReps and datums. Unlike the constructor, this takes both key and
      * script stake credentials, and takes hashes as hex rather than CBOR.
      *
      * @param state
      *   The starting state. Every field except `utxos` may be left out.
      * @param slotConfig
      *   Slot arithmetic for the emulated network.
      */
    @deprecated("use Emulator.create", "1.2.0")
    @JSExportStatic
    def withState(state: JEmulatorInitialState, slotConfig: SlotConfig): JEmulator = {
        val utxos = decodeCbor[Utxos](state.utxos)
        val initState = EmulatorInitialState(
          utxos = utxos,
          stakeRegistrations = parseStakeRegistrations(state.stakeRegistrations),
          poolRegistrations = parsePoolRegistrations(state.poolRegistrations),
          drepRegistrations = parseDrepRegistrations(state.drepRegistrations),
          datums = parseDatums(state.datums)
        )
        val env =
            if slotConfig == SlotConfig.mainnet then UtxoEnv.testMainnet()
            else UtxoEnv.default
        val context = new Context(env = env, slotConfig = slotConfig)
        wrapScalaEmulator(Emulator.withState(initState, context), slotConfig)
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
    @deprecated("use Emulator.create", "1.2.0")
    @JSExportStatic
    @nowarn("cat=deprecation") // constructing the deprecated constructor's own JEmulator handle
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
