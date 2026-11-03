package scalus.cardano.node

import scalus.interop.{TsName, TsType}
import scalus.uplc.DebugScript
import scalus.uplc.builtin.{ByteString, Data}
import scalus.uplc.eval.{JBalancer, JEvaluator, JRedeemerBudget}
import scalus.utils.scalajs.internal.*
import scalus.cardano.address.{Address, StakeAddress, StakePayload}
import scalus.cardano.ledger.rules.{Context, UtxoEnv}
import scalus.cardano.ledger.*
import scalus.cardano.ledger.epoch.{AccountState, PoolPerformance, StakeSnapshot}
import scalus.cardano.ledger.utils.AllResolvedScripts
import scalus.cardano.node.emulator.{AwaitTxPolicy, BlockIssuer, BlockProduction, ChainConfig, ChainView, ChainViewError, DropReason, ForgeError, OrphanTxPolicy, RollbackError, TxStatus}
import scalus.cardano.node.stream.{ChainPoint, ChainTip}

import scala.annotation.nowarn
import scala.util.Try
import scala.util.control.NonFatal
import scala.scalajs.js
import scala.scalajs.js.JSConverters.*
import scala.scalajs.js.annotation.{JSExportStatic, JSExportTopLevel}
import scala.scalajs.js.typedarray.Uint8Array

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
  *   Cardano ledger CDDL. This is the same shape `getUtxosCbor` gives back. An array of
  *   `[input, output]` pairs, as `Utxo.toCbor` writes each, is read too.
  * @param slotConfig
  *   Slot arithmetic for the emulated network, for example `SlotConfig.preview`.
  * @param initialStakeRewards
  *   Stake credentials to register before the first transaction: a map from a hex-encoded 28-byte
  *   script hash to a reward balance in lovelace, written as a decimal string. Only script
  *   credentials can be seeded here; use `Emulator.create` with `stakeRegistrations` to seed key
  *   credentials. A balance of `"0"` is the usual case: a reward (staking) validator only runs when
  *   a transaction withdraws from its address, and a withdrawal of zero lovelace is enough, but the
  *   address must already be registered for the transaction to be valid.
  */
@JSExportTopLevel("Emulator")
class JEmulator @deprecated("use Emulator.create", "1.2.0") (
    initialUtxosCbor: Uint8Array,
    slotConfig: JsSlotConfig,
    initialStakeRewards: js.Dictionary[String] = js.Dictionary()
) extends js.Object {

    private var emulator: Emulator = {
        val utxos = JsCbor.decode(initialUtxosCbor)(using Utxos.mapOrPairsDecoder)
        val env =
            if slotConfig.underlying == SlotConfig.mainnet then UtxoEnv.testMainnet()
            else UtxoEnv.default
        val context = new Context(env = env, slotConfig = slotConfig.underlying)
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

    /** The wall clock's time source in POSIX milliseconds; `None` for a manual clock. */
    private var wallClock: Option[() => Double] = None

    /** On a wall clock, moves to the slot that contains now, never backwards. */
    private def syncClock(): Unit =
        wallClock.foreach { now =>
            val slot = emulator.currentContext.slotConfig.timeToSlot(now().toLong)
            if slot > emulator.currentContext.env.slot then emulator.setSlot(slot)
        }

    /** Network, slot configuration and protocol parameters this emulator validates transactions
      * against, as one coherent triple. For an emulator built with `Emulator.create`, this is the
      * `CardanoInfo` it was created from, with the pparams that later epoch boundaries enacted
      * (ledger-rules spec [ES-15]); for the deprecated constructors, protocol parameters come from
      * `slotConfig` alone (mainnet parameters for `SlotConfig.mainnet`, `UtxoEnv.default`'s
      * parameters otherwise), which is the mismatch `Emulator.create` fixes.
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
      *   if a script fails; it names the redeemer, classifies the failure, carries the script's
      *   traces and the arguments the script saw.
      * @throws Error
      *   for any other failure: `txCborBytes` does not decode as a transaction, an input no UTxO
      *   resolves, a script the transaction does not carry.
      */
    def evaluateTx(txCborBytes: Uint8Array): js.Array[JRedeemerBudget] =
        evaluateTxWith(txCborBytes, Map.empty)

    /** As above, plus UTxOs the emulator does not hold - outputs of a transaction not yet
      * submitted, typically. Each is a `Utxo` or an `[input, output]` pair as hex or bytes, as
      * `evaluator.evaluateTx` takes them.
      */
    def evaluateTx(
        txCborBytes: Uint8Array,
        @TsType("readonly (string | Uint8Array | Utxo)[]") additionalUtxos: js.Any
    ): js.Array[JRedeemerBudget] =
        evaluateTxWith(txCborBytes, JEvaluator.utxoMapOf(additionalUtxos, "additionalUtxos"))

    private def evaluateTxWith(
        txCborBytes: Uint8Array,
        extra: => Utxos
    ): js.Array[JRedeemerBudget] =
        surfacingErrors {
            syncClock()
            val tx = Transaction.fromCbor(txCborBytes.toByteArray)
            val info = emulator.cardanoInfo
            JEvaluator
                .evaluate(
                  tx,
                  emulator.utxos ++ extra,
                  info.slotConfig,
                  info.protocolParams.costModels,
                  info.majorProtocolVersion.version
                )
                .toJSArray
        }

    /** The cardano-ledger names of every ledger failure of a transaction, in the ledger's order,
      * for example `["BadInputsUTxO", "OutsideValidityIntervalUTxO"]`; empty when the ledger rules
      * accept it. A dry run: the ledger state does not change. Ledger-rules spec [FL-8].
      *
      * `submitTx` reports one failure; this lists them all, as a Haskell ledger test asserts them.
      *
      * @throws Error
      *   if `txCborBytes` does not decode as a transaction
      */
    def ledgerFailures(txCborBytes: Uint8Array): js.Array[String] =
        surfacingErrors {
            syncClock()
            val tx = Transaction.fromCbor(txCborBytes.toByteArray)
            emulator.ledgerFailures(tx).flatMap(TransactionException.ledgerNames).toJSArray
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
        syncClock()
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
        syncClock()
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
        try Right(Transaction.fromCbor(txCborBytes.toByteArray))
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
      * ledger CDDL. `getUtxos` returns the same UTxOs as `Utxo` handles.
      */
    def getUtxosCbor(): Uint8Array = {
        JsCbor.encode(emulator.utxos)
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

    /** The UTxOs matching `filter`, in the ledger state `view` names: `"tip"`, the chain's last
      * block, as `getUtxos(filter)` reads; `"mempool"`, after the pending transactions too; or `{
      * stable: n }`, after the block of depth `n`, where the tip has depth 1 and `height + 1` is
      * genesis. Pass `{}` as the filter for every UTxO.
      *
      * @throws TypeError
      *   if `view` is none of these shapes.
      * @throws RangeError
      *   if `stable` is below 1 or above the tip height plus 1, or its block is deeper than
      *   `securityParam + 1`, whose ledger state the emulator no longer keeps.
      */
    def getUtxos(
        filter: JsUtxoFilter,
        @TsType("\"tip\" | \"mempool\" | { stable: number }") view: js.Any
    ): js.Array[JsUtxo] =
        emulator.findUtxosSync(toQuery(filter), JEmulator.viewOf(view)) match
            case Right(utxos) => wrapAll(utxos)
            case Left(error) =>
                throw js.JavaScriptException(new js.RangeError(JEmulator.message(error)))

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
      * `paymentCredentialType` narrows a hash to key or script credentials. Without a type, the
      * hash matches either kind. A type without a hash is rejected.
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

        val credentialType = filter.paymentCredentialType.toOption
        credentialType.foreach { _ =>
            require(
              filter.paymentCredential.isDefined,
              "paymentCredentialType requires paymentCredential"
            )
        }

        val sources: List[UtxoSource] = List(
          filter.address.toOption.map(a => UtxoSource.FromAddress(Address.fromString(a))),
          filter.paymentCredential.toOption.map { hex =>
              credentialType match
                  case Some(kind) =>
                      UtxoSource.FromPaymentCredential(JEmulator.parseCredential(kind, hex))
                  case None =>
                      UtxoSource.FromPaymentCredential(
                        Credential.KeyHash(AddrKeyHash.fromHex(hex))
                      ) ||
                      UtxoSource.FromPaymentCredential(
                        Credential.ScriptHash(ScriptHash.fromHex(hex))
                      )
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
            .map(min => withSource && UtxoFilter.MinLovelace(Coin(longOf(min, "minLovelace"))))
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
        emulator.certState.dstate.accounts
            .get(cred)
            .map(_.balance.value.toJsBigInt)
            .orUndefined
    }

    /** Moves the clock to an absolute slot, forwards or backwards. Validity intervals and
      * time-aware scripts see the difference; no blocks are produced in between. Crossing an epoch
      * boundary forwards runs the epoch rules: pool retirement, proposal expiry and refunds,
      * rewards and enactment. Each boundary crossed costs one epoch transition, so a move across
      * millions of epochs is slow. A fractional value is truncated.
      */
    def setSlot(slot: Double): Unit = {
        emulator.setSlot(slot.toLong)
    }

    /** The treasury in lovelace at the start of the current epoch: the value a transaction must
      * state as its `currentTreasuryValue`. An epoch boundary changes it as the epoch rules do:
      * donations, the treasury cut of the rewards and enacted withdrawals.
      */
    def getTreasury(): js.BigInt = emulator.treasury.value.toJsBigInt

    /** The reserves in lovelace. They start as the `reserves` option. */
    def getReserves(): js.BigInt = emulator.reserves.value.toJsBigInt

    /** The gov state: the proposals in the order they were submitted, with their votes, the
      * committee and the constitution. A committee member's hot key is not here: a tx authorizes it
      * with a certificate, and the vote it casts shows in `votes`.
      */
    def getGovState(): JGovState = JEmulator.govStateOf(emulator.govState)

    /** The current slot number of the emulator. With clock `"wall"`, reading it does not move the
      * clock: only `submitTx`, `evaluateTx`, `ledgerFailures`, `produceBlock` and `awaitTx` do.
      */
    def getSlot(): Double = emulator.currentContext.env.slot.toDouble

    /** The epoch of the current slot. */
    def getEpoch(): Double = emulator.currentEpoch.toDouble

    /** Advance the current slot by `n` slots. */
    def tick(n: Double): Unit = emulator.tick(n.toLong)

    /** POSIX time in milliseconds at which the emulator's current slot starts. Equivalent to
      * `getCardanoInfo().slotConfig.slotToTime(getSlot())`. With clock `"wall"`, reading it does
      * not move the clock: only `submitTx`, `evaluateTx`, `ledgerFailures`, `produceBlock` and
      * `awaitTx` do.
      */
    def getTime(): Double =
        emulator.currentContext.slotConfig.slotToTime(emulator.currentContext.env.slot).toDouble

    /** Moves the clock to the slot containing this POSIX time - the inverse of `getTime`. A
      * fractional slot is truncated, the same as `setSlot`.
      */
    def setTime(posixMillis: Double): Unit =
        setSlot(emulator.currentContext.slotConfig.timeToSlot(posixMillis.toLong).toDouble)

    /** Whether the transaction is confirmed: in a block on the selected chain. A pending
      * transaction, with `blockProduction: "manual"`, reads `false`; `getTxStatus` tells pending
      * from unknown.
      *
      * @param txHashHex
      *   Hex-encoded 32-byte transaction hash.
      */
    def hasTx(txHashHex: String): Boolean =
        emulator.hasTx(TransactionHash.fromHex(txHashHex))

    /** Where a transaction stands, as one word: `"Pending"` in the mempool, `"Confirmed"` in a
      * block of the chain, `"Dropped"` when it left the mempool without a block, `"RolledBack"`
      * when its block left the chain under `orphanTxs: "drop"`, `"NotFound"` if this emulator never
      * saw it. `getTxStatus` gives the details. With the default `blockProduction: "instant"`, an
      * accepted transaction is `"Confirmed"` at once.
      *
      * @param txHashHex
      *   Hex-encoded 32-byte transaction hash.
      */
    @TsType("\"Pending\" | \"Confirmed\" | \"Dropped\" | \"RolledBack\" | \"NotFound\"")
    def getTransactionStatus(txHashHex: String): String = getTxStatus(txHashHex).status

    /** Where a transaction stands, with the details: the depth and the block of a confirmed
      * transaction (the tip has depth 1), the reason of a dropped one.
      *
      * @param txHashHex
      *   Hex-encoded 32-byte transaction hash.
      */
    def getTxStatus(txHashHex: String): JTxStatusInfo =
        JEmulator.txStatusInfo(emulator.txStatus(TransactionHash.fromHex(txHashHex)))

    /** Forges a block from the mempool and returns its point. With `blockProduction: "manual"` this
      * is the only way a block appears: at slot `max(getSlot(), tip slot + 1)`, with the oldest
      * pending transactions that fit the block limits, after the clock moves to that slot and the
      * mempool is checked again. An empty mempool forges an empty block.
      */
    def produceBlock(): JsChainPoint = {
        syncClock()
        JEmulator.pointOf(emulator.produceBlock())
    }

    /** Forges a block of `txs`, as CBOR, on the block `on`, at slot `on.slot + 1`: a block from
      * another pool, so the mempool is not read. A branch longer than the chain becomes the chain,
      * and the transactions of the blocks it replaces follow `orphanTxs`. `on` is a block of this
      * emulator, or the genesis point `{ slot: 0, blockHash: "00".repeat(32) }`.
      *
      * @return
      *   `{ isSuccess: true, point }`, or `{ isSuccess: false, error, errorKind }`, and then
      *   nothing changed.
      * @throws TypeError
      *   if `on` is not a point, or an element of `txs` is not a transaction.
      */
    def produceBlockOn(on: JsChainPoint, txs: js.Array[Uint8Array]): JChainResult = {
        val parent = JEmulator.chainPointOf(on)
        JEmulator.chainResult(emulator.produceBlock(parent, JEmulator.txsOf(txs)))
    }

    /** As above, at `slot`, which must be greater than `on.slot`. */
    def produceBlockOn(on: JsChainPoint, txs: js.Array[Uint8Array], slot: Double): JChainResult = {
        val parent = JEmulator.chainPointOf(on)
        val at = longOf(slot, "slot")
        JEmulator.chainResult(emulator.produceBlock(parent, at, JEmulator.txsOf(txs)))
    }

    /** Removes the last `n` blocks of the chain, and returns the new tip. Their transactions follow
      * `orphanTxs`, and the clock stays. `n` can be at most `securityParam`.
      *
      * @return
      *   `{ isSuccess: true, point }`, or `{ isSuccess: false, error, errorKind }`, and then
      *   nothing changed.
      */
    def rollback(n: Double): JChainResult =
        JEmulator.chainResult(emulator.rollback(intOf(n, "n")))

    /** The hashes of the pending transactions, oldest first. */
    def getMempool(): js.Array[String] = js.Array(emulator.mempoolTxs.map(_.id.toHex)*)

    /** The last block of the chain, with its height. Before the first block it is genesis:
      * `{ slot: 0, blockHash: "00".repeat(32), height: 0 }`.
      */
    def getTip(): JsChainTip = JEmulator.tipOf(emulator.tip)

    /** Waits for a pending transaction, as a provider's `awaitTx` does, and says if it is
      * confirmed. With `awaitTx: "produceBlock"`, the default, it forges blocks while the
      * transaction is pending; with `"waitOnly"` it forges nothing and reports the status as it is.
      *
      * @param txHashHex
      *   Hex-encoded 32-byte transaction hash.
      */
    def awaitTx(txHashHex: String): Boolean = {
        syncClock()
        val hash = TransactionHash.fromHex(txHashHex)
        // Each forge takes at least the oldest pending tx (spec [FG-4]), so a pending tx is forged
        // or dropped within as many blocks as the mempool holds. `pollForConfirmation` without its
        // `Future`, which would link `scala.concurrent` into the bundle.
        emulator.forgeWhilePending(hash, emulator.mempoolTxs.size)
        emulator.transactionStatus(hash) == TransactionStatus.Confirmed
    }

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
            .map(_.toCbor.toUint8Array)
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
              rewards = info.rewards.value.toJsBigInt
            )
            .asInstanceOf[JDelegationInfo]
    }

    /** Pays `lovelace` into a registered reward account, as an epoch boundary pays rewards. A later
      * withdrawal of the balance needs no registration first.
      *
      * @param rewardAddressBech32
      *   The reward (stake) address in bech32 form, for example `stake1...` or `stake_test1...`.
      * @throws Error
      *   if the account is not registered, or `lovelace` is negative.
      */
    def addRewards(rewardAddressBech32: String, lovelace: js.BigInt): Unit =
        emulator.addRewards(
          rewardAddressCredential(rewardAddressBech32),
          Coin(longOf(lovelace, "lovelace"))
        )

    /** The reward account of a reward address.
      *
      * @param rewardAddressBech32
      *   The reward (stake) address in bech32 form, for example `stake1...` or `stake_test1...`.
      * @return
      *   The account, or `undefined` if it is not registered.
      */
    def getAccount(rewardAddressBech32: String): js.UndefOr[JAccountInfo] =
        emulator.certState.dstate.accounts
            .get(rewardAddressCredential(rewardAddressBech32))
            .map { account =>
                js.Dynamic
                    .literal(
                      balance = account.balance.value.toJsBigInt,
                      deposit = account.deposit.value.toJsBigInt,
                      poolId = account.stakePoolDelegation.map(_.toHex).orUndefined,
                      drep = account.dRepDelegation.map(JEmulator.drepOf).orUndefined
                    )
                    .asInstanceOf[JAccountInfo]
            }
            .orUndefined

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
                    rewardAddress = rewardAddressOf(entry.credential),
                    pool = entry.pool.map(_.toHex).orUndefined,
                    stake = entry.stake.value.toJsBigInt,
                    rewards = entry.rewards.value.toJsBigInt
                  )
                  .asInstanceOf[JsStakeDistributionEntry]
            )
        }
        out
    }

    /** The bech32 reward address of `credential` on this emulator's network. */
    private def rewardAddressOf(credential: Credential): String = {
        val payload = credential match
            case Credential.KeyHash(hash) => StakePayload.Stake(StakeKeyHash.fromByteString(hash))
            case Credential.ScriptHash(hash) => StakePayload.Script(hash)
        val address = StakeAddress(emulator.currentContext.env.network, payload)
        address.toBech32.getOrElse(
          throw new IllegalStateException(
            s"Reward address ${address.toHex} cannot be encoded to bech32: no human-readable prefix is defined for its network"
          )
        )
    }

    /** The stake snapshots of the last epoch boundary: `mark`, `set` and `go`, each with one row
      * per credential that has active stake, and the fee snapshot. The genesis fills all three from
      * the initial state; each boundary moves `set` to `go`, `mark` to `set`, and takes a new
      * `mark`.
      */
    // ledger-rules spec [ES-9a], D82
    def getStakeSnapshots(): JsStakeSnapshots = {
        val snapshots = emulator.snapshots
        js.Dynamic
            .literal(
              mark = stakeRows(snapshots.mark),
              set = stakeRows(snapshots.set),
              go = stakeRows(snapshots.go),
              fee = snapshots.fee.value.toJsBigInt
            )
            .asInstanceOf[JsStakeSnapshots]
    }

    private def stakeRows(snapshot: StakeSnapshot): js.Array[JsStakeSnapshotEntry] =
        snapshot.activeStake.toSeq.map { (credential, stake) =>
            js.Dynamic
                .literal(
                  credential = credentialHex(credential),
                  rewardAddress = rewardAddressOf(credential),
                  pool = snapshot.delegations(credential).toHex,
                  stake = stake.value.toJsBigInt
                )
                .asInstanceOf[JsStakeSnapshotEntry]
        }.toJSArray

    /** The reward update the last epoch boundary computed, which the next boundary pays, or
      * `undefined` before the first boundary.
      */
    // ledger-rules spec [ES-9a], D82
    def getPendingRewards(): js.UndefOr[JsRewardUpdate] =
        emulator.pendingRewards.map { update =>
            val rewards = update.rewards.toSeq.map { (credential, amount) =>
                js.Dynamic
                    .literal(
                      credential = credentialHex(credential),
                      rewardAddress = rewardAddressOf(credential),
                      amount = amount.value.toJsBigInt
                    )
                    .asInstanceOf[JsRewardEntry]
            }.toJSArray
            js.Dynamic
                .literal(
                  deltaT = update.deltaT.value.toJsBigInt,
                  deltaR = update.deltaR.value.toJsBigInt,
                  deltaF = update.deltaF.value.toJsBigInt,
                  rewards = rewards
                )
                .asInstanceOf[JsRewardUpdate]
        }.orUndefined

    /** The ratify result the last epoch boundary computed, which the next boundary enacts: the ids
      * of the actions it enacts, in order, the ids of the actions that expired, and whether an
      * enacted action delayed the rest. At the start it enacts nothing. What a boundary enacted
      * shows after it in `getGovState`, `getProtocolParameters` and the balances.
      */
    // ledger-rules spec [ES-9b], D94
    def getPendingRatify(): JsPendingRatify = {
        val ratified = emulator.pendingRatify
        def ref(id: GovActionId): JsGovActionRef =
            js.Dynamic
                .literal(txHash = id.transactionId.toHex, index = id.govActionIndex.toDouble)
                .asInstanceOf[JsGovActionRef]
        val expired =
            ratified.expired.toSeq.sortBy(id => (id.transactionId.toHex, id.govActionIndex))
        js.Dynamic
            .literal(
              enacted = ratified.enacted.map(ref).toJSArray,
              expired = expired.map(ref).toJSArray,
              delayed = ratified.delayed
            )
            .asInstanceOf[JsPendingRatify]
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
        // the bytes the datum arrived in, so their hash is `datumHashHex`, spec [SC-13k]
        emulator.binaryDatums.get(hash).map(_.raw.toUint8Array).orUndefined
    }

    /** Adds a datum to the datum store `getDatum` reads. Use it for a UTxO added with `addUtxo`
      * that holds only the hash of its datum, so `getDatum` finds the datum, as a node does once a
      * transaction has carried it. The store keeps the bytes as given, and adding a datum it
      * already holds changes nothing.
      *
      * @param datumCbor
      *   The datum as CBOR, hex-encoded or as bytes.
      * @return
      *   The hex-encoded hash of exactly those bytes: the hash `getDatum` finds the datum under.
      * @throws TypeError
      *   if `datumCbor` is not the CBOR of one datum.
      */
    def addDatum(@TsType("string | Uint8Array") datumCbor: js.Any): String = {
        val datum =
            decodeOf(datumCbor, "datumCbor")(bytes => KeepRaw.unsafe(Data.fromCbor(bytes), bytes))
        emulator.addDatum(datum).toHex
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
        val wrapper = new JEmulator(
          JsCbor.encode(Map.empty: Utxos),
          JsSlotConfig.wrap(emulator.currentContext.slotConfig),
          initialStakeRewards
        )
        JEmulator.replaceEmulator(wrapper, snapshotEmulator)
        wrapper.wallClock = wallClock
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

/** A registered reward account, returned by `Emulator.getAccount`. */
@TsName("AccountInfo")
trait JAccountInfo extends js.Object {

    /** Reward balance in lovelace. */
    val balance: js.BigInt

    /** The deposit paid to register the account, in lovelace. */
    val deposit: js.BigInt

    /** Hex-encoded 28-byte key hash of the pool the account delegates to, or `undefined`. */
    val poolId: js.UndefOr[String]

    /** The DRep the account delegates its vote to, or `undefined`: a key or script credential with
      * its hex-encoded 28-byte hash, or one of the two predefined DReps by name.
      */
    @TsType(
      "{ readonly type: \"key\" | \"script\"; readonly hash: string } | \"AlwaysAbstain\" | \"AlwaysNoConfidence\""
    )
    val drep: js.UndefOr[js.Any]
}

/** One row of `Emulator.getAppliedTxs`. */
@TsName("AppliedTxInfo")
trait JAppliedTxInfo extends js.Object {

    /** Hex-encoded 32-byte transaction hash. */
    val txHash: String

    /** The emulator slot at the time this transaction was applied. */
    val slot: Double
}

/** The result of `Emulator.getStakeSnapshots`. */
@TsName("StakeSnapshots")
trait JsStakeSnapshots extends js.Object {

    /** The stake taken at the last epoch boundary. */
    val mark: js.Array[JsStakeSnapshotEntry]

    /** The stake taken at the boundary before. */
    val set: js.Array[JsStakeSnapshotEntry]

    /** The stake taken two boundaries ago, which the reward update of the last boundary read. */
    val go: js.Array[JsStakeSnapshotEntry]

    /** The fees of the epoch before the last boundary, in lovelace. */
    val fee: js.BigInt
}

/** One row of a stake snapshot of `Emulator.getStakeSnapshots`: a registered credential that
  * delegates to a pool and has stake above 0.
  */
@TsName("StakeSnapshotEntry")
trait JsStakeSnapshotEntry extends js.Object {

    /** Hex-encoded 28-byte credential hash, a key hash or a script hash, with no discriminator. */
    val credential: String

    /** The reward (stake) address of this credential in bech32, on the emulator's network. */
    val rewardAddress: String

    /** Hex-encoded 28-byte key hash of the pool this credential delegated to. */
    val pool: String

    /** The stake in lovelace: the coin of its base-address outputs plus its reward balance. */
    val stake: js.BigInt
}

/** The result of `Emulator.getPendingRewards`. The four parts sum to 0. */
@TsName("RewardUpdate")
trait JsRewardUpdate extends js.Object {

    /** The change to the treasury, in lovelace. */
    val deltaT: js.BigInt

    /** The change to the reserves, in lovelace, with its sign. */
    val deltaR: js.BigInt

    /** The change to the fees, in lovelace, with its sign. */
    val deltaF: js.BigInt

    /** The reward of each credential. The next boundary pays it to the credential's balance if it
      * is registered then, and to the treasury if not.
      */
    val rewards: js.Array[JsRewardEntry]
}

/** The result of `Emulator.getPendingRatify`. */
@TsName("PendingRatify")
trait JsPendingRatify extends js.Object {

    /** The actions the next boundary enacts, in the order they were ratified. */
    val enacted: js.Array[JsGovActionRef]

    /** The actions that expired unenacted; the next boundary removes them and refunds their
      * deposits.
      */
    val expired: js.Array[JsGovActionRef]

    /** Whether an enacted action delayed every other action to a later boundary. */
    val delayed: Boolean
}

/** The id of a gov action: the proposing transaction and the index of the proposal in it, as in
  * `GovActionState`.
  */
@TsName("GovActionRef")
trait JsGovActionRef extends js.Object {

    /** Hex-encoded 32-byte hash of the transaction that proposed the action. */
    val txHash: String

    /** The index of the proposal among the proposals of that transaction. */
    val index: Double
}

/** One reward of `RewardUpdate.rewards`. */
@TsName("RewardEntry")
trait JsRewardEntry extends js.Object {

    /** Hex-encoded 28-byte credential hash, a key hash or a script hash, with no discriminator. */
    val credential: String

    /** The reward (stake) address of this credential in bech32, on the emulator's network. */
    val rewardAddress: String

    /** The reward in lovelace: the leader and member rewards of this credential, summed. */
    val amount: js.BigInt
}

/** One row of `Emulator.getStakeDistribution`. */
@TsName("StakeDistributionEntry")
trait JsStakeDistributionEntry extends js.Object {

    /** Hex-encoded 28-byte credential hash - a key hash or a script hash, with no discriminator,
      * the same convention `UtxoFilter.paymentCredential` uses.
      */
    val credential: String

    /** The reward (stake) address of this credential in bech32, on the emulator's network. */
    val rewardAddress: String

    /** Hex-encoded 28-byte pool key hash, or `undefined` if this credential delegates to no pool.
      */
    val pool: js.UndefOr[String]

    /** Live stake: the lovelace sitting at addresses delegating to this credential. */
    val stake: js.BigInt

    /** This credential's reward balance. */
    val rewards: js.BigInt
}

/** Stake registration entry for `EmulatorOptions`. */
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

/** Pool registration entry for `EmulatorOptions`. Give exactly one of `params` and `poolId`.
  */
@TsName("PoolRegistration")
trait JPoolRegistration extends js.Object {

    /** The pool's parameters, as one CBOR-encoded Cardano certificate: the whole 10-element
      * `pool_registration` array, tag `3` first, then operator key hash, VRF key hash, pledge,
      * cost, margin, reward account, owners, relays and metadata. The bare parameter list without
      * the certificate tag is rejected.
      */
    val params: js.UndefOr[Uint8Array] = js.undefined

    /** Hex-encoded 28-byte pool key hash, for a pool whose parameters do not matter: its operator
      * is its only owner and takes its rewards, with no pledge, the minimum cost, no margin and no
      * relays.
      */
    val poolId: js.UndefOr[String] = js.undefined
}

/** DRep registration entry for `EmulatorOptions`. */
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

/** Datum entry for `EmulatorOptions`. */
@TsName("DatumEntry")
trait JDatumEntry extends js.Object {

    /** Hex-encoded 32-byte datum hash */
    val hash: String

    /** The datum itself as CBOR, hex-encoded. Its hash must be `hash`; nothing checks that. */
    val datum: String
}

/** A fraction: `numerator / denominator`. */
@TsName("Fraction")
trait JFraction extends js.Object {
    val numerator: Double
    val denominator: Double
}

/** One member of a constitutional committee. */
@TsName("CommitteeMember")
trait JCommitteeMember extends js.Object {

    /** Credential type of the member's cold credential: "key" or "script". */
    @TsType("\"key\" | \"script\"")
    val credentialType: String

    /** Hex-encoded 28-byte hash of the member's cold credential. */
    val credentialHash: String

    /** The last epoch of the member's term. */
    val expiresAfter: Double
}

/** A constitutional committee: its members and the fraction of them that must vote yes. */
@TsName("Committee")
trait JCommittee extends js.Object {
    val members: js.Array[JCommitteeMember]
    val threshold: JFraction
}

/** A hot key a committee member authorized, for `EmulatorOptions.committeeHotKeys`. */
@TsName("CommitteeHotKey")
trait JCommitteeHotKey extends js.Object {

    /** Credential type of the member's cold credential: "key" or "script". */
    @TsType("\"key\" | \"script\"")
    val coldCredentialType: String

    /** Hex-encoded 28-byte hash of the member's cold credential. */
    val coldCredentialHash: String

    /** Credential type of the hot credential: "key" or "script". */
    @TsType("\"key\" | \"script\"")
    val hotCredentialType: String

    /** Hex-encoded 28-byte hash of the hot credential. */
    val hotCredentialHash: String
}

/** The constitution: its anchor and, if any, the hash of its guardrails script. */
@TsName("Constitution")
trait JConstitution extends js.Object {

    /** The URL of the constitution text. */
    val anchorUrl: String

    /** Hex-encoded 32-byte hash of the document at `anchorUrl`. */
    val anchorDataHash: String

    /** Hex-encoded 28-byte hash of the guardrails script, or `undefined` for none. */
    val guardrailsScriptHash: js.UndefOr[String] = js.undefined
}

/** One vote on a proposal, in `GovActionState.votes`. */
@TsName("GovVote")
trait JGovVote extends js.Object {

    /** Who votes: a committee hot credential, a DRep, or a stake pool. */
    @TsType("\"committee\" | \"drep\" | \"pool\"")
    val voterRole: String

    /** Credential type of the voter: "key" or "script". A pool is always "key". */
    @TsType("\"key\" | \"script\"")
    val credentialType: String

    /** Hex-encoded 28-byte hash of the voter: the hot credential, the DRep credential, or the pool
      * key hash.
      */
    val credentialHash: String

    @TsType("\"Yes\" | \"No\" | \"Abstain\"")
    val vote: String
}

/** One proposal in `GovState.proposals`. */
@TsName("GovActionState")
trait JGovActionState extends js.Object {

    /** Hex-encoded 32-byte hash of the transaction that proposed the action. */
    val txHash: String

    /** The index of the proposal among the proposals of that transaction. */
    val index: Double

    /** The kind of gov action. */
    @TsType(
      "\"ParameterChange\" | \"HardForkInitiation\" | \"TreasuryWithdrawals\" | \"NoConfidence\" | \"UpdateCommittee\" | \"NewConstitution\" | \"InfoAction\""
    )
    val actionType: String

    /** The epoch the action was proposed in. */
    val proposedIn: Double

    /** The last epoch in which the action can be ratified. */
    val expiresAfter: Double

    /** The votes cast on the action, the last vote of each voter only. */
    val votes: js.Array[JGovVote]
}

/** The gov state, returned by `Emulator.getGovState`. */
@TsName("GovState")
trait JGovState extends js.Object {

    /** The proposals, in the order they were submitted. */
    val proposals: js.Array[JGovActionState]

    /** The committee, or `undefined` when there is none. */
    val committee: js.UndefOr[JCommittee]

    val constitution: JConstitution
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

    /** Stake pools that count as already registered, so transactions may delegate to them. Each
      * gives exactly one of `params` and `poolId`.
      */
    @TsType(
      "readonly (PoolRegistration & ({ params: Uint8Array; poolId?: undefined } | { poolId: string; params?: undefined }))[]"
    )
    val poolRegistrations: js.UndefOr[js.Array[JPoolRegistration]] = js.undefined

    /** DReps that count as already registered, so transactions may delegate votes to them. */
    val drepRegistrations: js.UndefOr[js.Array[JDRepRegistration]] = js.undefined

    /** Datums to put in the emulator's datum store, where `getDatum` looks them up by hash. The
      * store is only a lookup table: every accepted transaction adds the datums it witnesses to it,
      * and validation still requires a transaction to carry the datums it needs. Each `hash` must
      * be the blake2b-256 hash of its `datum` bytes.
      */
    val datums: js.UndefOr[js.Array[JDatumEntry]] = js.undefined

    /** The constitutional committee at the start. Leave it out for none: then only `NoConfidence`
      * and `UpdateCommittee` actions can be ratified.
      */
    val committee: js.UndefOr[JCommittee] = js.undefined

    /** The hot key each committee member has authorized at the start. Every cold credential must be
      * a member of `committee`, else `create` throws a `TypeError`.
      */
    val committeeHotKeys: js.UndefOr[js.Array[JCommitteeHotKey]] = js.undefined

    /** The constitution at the start. Leave it out for one with an empty anchor and no guardrails
      * script.
      */
    val constitution: js.UndefOr[JConstitution] = js.undefined

    /** The treasury in lovelace at the start of the current epoch. Defaults to the mainnet treasury
      * at epoch 665, 1 392 896 975 262 771 lovelace. A transaction that states a
      * `currentTreasuryValue` must state this value until the next epoch boundary, which changes it
      * as the epoch rules do. A negative value throws a `TypeError`.
      */
    val treasury: js.UndefOr[js.BigInt] = js.undefined

    /** The reserves in lovelace at the start. Defaults to the mainnet reserves at epoch 665, 6 029
      * 414 056 167 760 lovelace. No UTxO holds them. A negative value throws a `TypeError`.
      */
    val reserves: js.UndefOr[js.BigInt] = js.undefined

    /** Protocol parameters that replace those of the `CardanoInfo`. Only the fields given change,
      * and in `costModels` only the languages given: every other value stays as the `CardanoInfo`
      * has it. `getProtocolParameters` returns the result.
      */
    @TsType("Partial<ProtocolParamsLike>")
    val protocolParams: js.UndefOr[js.Any] = js.undefined

    /** `"manual"`, the default: the slot moves only when you move it. `"wall"`: before it validates
      * or evaluates a transaction, the emulator moves to the slot that contains `Date.now()`, never
      * backwards, so a validity interval built from the wall clock holds. `getSlot` and `getTime`
      * do not move the clock: only `submitTx`, `evaluateTx`, `ledgerFailures`, `produceBlock` and
      * `awaitTx` do.
      */
    @TsType("\"manual\" | \"wall\"")
    val clock: js.UndefOr[String] = js.undefined

    /** `"instant"`, the default: every accepted transaction is in a block at once. `"manual"`: an
      * accepted transaction waits in the mempool, and only `produceBlock` or `awaitTx` forges a
      * block, so `getUtxos` shows its outputs only then, as a real provider does.
      */
    @TsType("\"instant\" | \"manual\"")
    val blockProduction: js.UndefOr[String] = js.undefined

    /** What `awaitTx` does while the transaction is pending: `"produceBlock"`, the default, forges
      * blocks; `"waitOnly"` forges nothing.
      */
    @TsType("\"produceBlock\" | \"waitOnly\"")
    val awaitTx: js.UndefOr[String] = js.undefined

    /** What happens to the transactions of a block that leaves the chain: `"returnToMempool"`, the
      * default, puts them back in the mempool; `"drop"` drops them, and their status is
      * `"RolledBack"`.
      */
    @TsType("\"returnToMempool\" | \"drop\"")
    val orphanTxs: js.UndefOr[String] = js.undefined

    /** The deepest rollback the emulator allows, at least 1. Defaults to 2160. */
    val securityParam: js.UndefOr[Double] = js.undefined

    /** How the reward calculation rates the pools: `"ideal"`, the default, pays every pool with
      * stake as a perfect pool; `"measured"` pays by the blocks each pool made in the epoch before,
      * as the ledger does, so a pool with no block earns nothing.
      */
    @TsType("\"ideal\" | \"measured\"")
    val poolPerformance: js.UndefOr[String] = js.undefined

    /** `f`, the share of slots with a block, above 0 and at most 1: it sets the blocks an epoch is
      * expected to have under `"measured"`. Defaults to 1/20.
      */
    val activeSlotsCoeff: js.UndefOr[JFraction] = js.undefined

    /** The hex id of the pool, 28 bytes, that each forged block counts for, registered or not.
      * Leave it out for blocks that count for no pool, the default.
      */
    val blockIssuer: js.UndefOr[String] = js.undefined
}

/** A block of the emulator's chain: its slot and its block id, the hex hash of its header. The
  * genesis point is `{ slot: 0, blockHash: "00".repeat(32) }`.
  */
@TsName("ChainPoint")
trait JsChainPoint extends js.Object {
    val slot: Double
    val blockHash: String
}

/** The last block of the chain, with its height, the number of blocks from genesis. A tip is also a
  * `ChainPoint`.
  */
@TsName("ChainTip")
trait JsChainTip extends JsChainPoint {
    val height: Double
}

/** Result of `produceBlockOn` and `rollback`. Read `isSuccess` first: `point` is present only on
  * success, `error` and `errorKind` only on failure.
  */
@TsName("ChainResult")
trait JChainResult extends js.Object {
    val isSuccess: Boolean

    /** The new block, or the tip after a rollback; present on success. */
    val point: js.UndefOr[JsChainPoint]

    /** What went wrong, in prose; present on failure. */
    val error: js.UndefOr[String]

    /** Why nothing changed; present on failure. Stable enough to assert on, unlike `error`. */
    @TsType(
      "\"SlotNotAfterParent\" | \"UnknownPoint\" | \"TooDeep\" | \"InvalidTx\" | \"BlockTooBig\" | \"NotPositive\" | \"PastGenesis\""
    )
    val errorKind: js.UndefOr[String]
}

/** Result of `getTxStatus`. */
@TsName("TxStatusInfo")
trait JTxStatusInfo extends js.Object {
    @TsType("\"Pending\" | \"Confirmed\" | \"Dropped\" | \"RolledBack\" | \"NotFound\"")
    val status: String

    /** The depth of the block that holds the transaction, the tip being 1; present when
      * `"Confirmed"`.
      */
    val depth: js.UndefOr[Double]

    /** The block that holds the transaction; present when `"Confirmed"`. */
    val point: js.UndefOr[JsChainPoint]

    /** Why the transaction left the mempool; present when `"Dropped"`. `"Expired"`: its validity
      * interval ended. `"InputsSpent"`: an input is gone. `"Invalid"`: another rule failed.
      */
    @TsType("\"Expired\" | \"InputsSpent\" | \"Invalid\"")
    val reason: js.UndefOr[String]

    /** The message of the rule that failed; present when `reason` is `"Invalid"`. */
    val error: js.UndefOr[String]
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

    /** Restricts paymentCredential to key or script credentials. Requires paymentCredential;
      * omitted means either kind. Other filters still AND together.
      */
    @TsType("\"key\" | \"script\"")
    val paymentCredentialType: js.UndefOr[String] = js.undefined

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
          "paymentCredentialType",
          "unit",
          "outRefs",
          "txHash",
          "minLovelace",
          "limit"
        )

    /** A DRep as `getAccount` reports it. */
    private def drepOf(drep: DRep): js.Any = drep match
        case DRep.KeyHash(hash)      => js.Dynamic.literal(`type` = "key", hash = hash.toHex)
        case DRep.ScriptHash(hash)   => js.Dynamic.literal(`type` = "script", hash = hash.toHex)
        case DRep.AlwaysAbstain      => "AlwaysAbstain"
        case DRep.AlwaysNoConfidence => "AlwaysNoConfidence"

    private def committeeOf(c: JCommittee): Committee = {
        val name = "committee.threshold"
        val n = longOf(c.threshold.numerator, s"$name.numerator")
        val d = longOf(c.threshold.denominator, s"$name.denominator")
        if d <= 0 || n < 0 || n > d then typeError(s"$name must be a fraction in [0, 1]")
        Committee(
          c.members.toSeq.map { m =>
              parseCredential(m.credentialType, m.credentialHash) ->
                  longOf(m.expiresAfter, "committee.members.expiresAfter")
          }.toMap,
          UnitInterval(n, d)
        )
    }

    private def constitutionOf(c: JConstitution): Constitution =
        Constitution(
          Anchor(c.anchorUrl, DataHash.fromHex(c.anchorDataHash)),
          c.guardrailsScriptHash.toOption.map(ScriptHash.fromHex)
        )

    /** `(credentialType, credentialHash)` of `credential`, as the options take it. */
    private def credentialFields(credential: Credential): (String, String) = credential match
        case Credential.KeyHash(hash)    => ("key", hash.toHex)
        case Credential.ScriptHash(hash) => ("script", hash.toHex)

    private def govStateOf(gov: GovState): JGovState = {
        def vote(role: String, credential: Credential, v: Vote): JGovVote = {
            val (credentialType, credentialHash) = credentialFields(credential)
            js.Dynamic
                .literal(
                  voterRole = role,
                  credentialType = credentialType,
                  credentialHash = credentialHash,
                  vote = v.toString
                )
                .asInstanceOf[JGovVote]
        }
        val proposals = gov.proposals.map { p =>
            val votes = p.committeeVotes.map((c, v) => vote("committee", c, v)) ++
                p.drepVotes.map((c, v) => vote("drep", c, v)) ++
                p.spoVotes.map((pool, v) =>
                    vote("pool", Credential.KeyHash(AddrKeyHash.fromByteString(pool)), v)
                )
            js.Dynamic
                .literal(
                  txHash = p.id.transactionId.toHex,
                  index = p.id.govActionIndex.toDouble,
                  actionType = actionTypeOf(p.action),
                  proposedIn = p.proposedIn.toDouble,
                  expiresAfter = p.expiresAfter.toDouble,
                  votes = votes.toJSArray
                )
                .asInstanceOf[JGovActionState]
        }
        val committee = gov.committee.map { c =>
            val members = c.members.map { (credential, expiresAfter) =>
                val (credentialType, credentialHash) = credentialFields(credential)
                js.Dynamic
                    .literal(
                      credentialType = credentialType,
                      credentialHash = credentialHash,
                      expiresAfter = expiresAfter.toDouble
                    )
                    .asInstanceOf[JCommitteeMember]
            }
            js.Dynamic
                .literal(
                  members = members.toJSArray,
                  threshold = js.Dynamic.literal(
                    numerator = c.threshold.numerator.toDouble,
                    denominator = c.threshold.denominator.toDouble
                  )
                )
                .asInstanceOf[JCommittee]
        }
        val constitution = js.Dynamic
            .literal(
              anchorUrl = gov.constitution.anchor.url,
              anchorDataHash = gov.constitution.anchor.dataHash.toHex,
              guardrailsScriptHash = gov.constitution.scriptHash.map(_.toHex).orUndefined
            )
            .asInstanceOf[JConstitution]
        js.Dynamic
            .literal(
              proposals = proposals.toJSArray,
              committee = committee.orUndefined,
              constitution = constitution
            )
            .asInstanceOf[JGovState]
    }

    private def actionTypeOf(action: GovAction): String = action match
        case _: GovAction.ParameterChange     => "ParameterChange"
        case _: GovAction.HardForkInitiation  => "HardForkInitiation"
        case _: GovAction.TreasuryWithdrawals => "TreasuryWithdrawals"
        case _: GovAction.NoConfidence        => "NoConfidence"
        case _: GovAction.UpdateCommittee     => "UpdateCommittee"
        case _: GovAction.NewConstitution     => "NewConstitution"
        case GovAction.InfoAction             => "InfoAction"

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
              rewards = Coin(longOf(s.rewards, "stakeRegistrations.rewards")),
              delegatedTo = s.delegatedTo.toOption.map { hex =>
                  PoolKeyHash.fromHex(hex)
              }
            )
        }

    private def parsePoolRegistrations(
        regs: js.UndefOr[js.Array[JPoolRegistration]],
        env: UtxoEnv
    ): Seq[EmulatorPoolRegistration] =
        regs.toOption.toSeq.flatten.map { p =>
            val params = (p.params.toOption, p.poolId.toOption) match
                case (Some(cbor), None) =>
                    JsCbor.decode[Certificate](cbor) match
                        case pr: Certificate.PoolRegistration => pr
                        case other =>
                            typeError(s"Expected PoolRegistration certificate, got: $other")
                case (None, Some(hex)) => minimalPool(PoolKeyHash.fromHex(hex), env)
                case _ => typeError("a pool registration needs exactly one of params and poolId")
            EmulatorPoolRegistration(params)
        }

    /** A pool `poolId` operates alone: owner and reward account are its own key, with no pledge,
      * the minimum cost, no margin and no relays.
      */
    private def minimalPool(poolId: PoolKeyHash, env: UtxoEnv): Certificate.PoolRegistration = {
        val operator = AddrKeyHash.fromByteString(poolId)
        Certificate.PoolRegistration(
          operator = operator,
          vrfKeyHash = VrfKeyHash.fromByteString(ByteString.fromArray(new Array[Byte](32))),
          pledge = Coin.zero,
          cost = Coin(env.params.minPoolCost),
          margin = UnitInterval.zero,
          rewardAccount = RewardAccount(
            StakeAddress(env.network, StakePayload.Stake(StakeKeyHash.fromByteString(poolId)))
          ),
          poolOwners = Set(operator),
          relays = IndexedSeq.empty,
          poolMetadata = None
        )
    }

    private def parseDrepRegistrations(
        regs: js.UndefOr[js.Array[JDRepRegistration]]
    ): Seq[EmulatorDRepRegistration] =
        regs.toOption.toSeq.flatten.map { d =>
            EmulatorDRepRegistration(
              credential = parseCredential(d.credentialType, d.credentialHash),
              deposit = Coin(longOf(d.deposit, "drepRegistrations.deposit")),
              anchor = d.anchor.toOption.map(JsCbor.decode[Anchor])
            )
        }

    /** The datums of `entries`, keeping the bytes each was given in, spec [SC-13k]. An entry whose
      * `hash` is not the hash of its `datum` bytes throws a `TypeError`.
      */
    private def parseDatums(
        entries: js.UndefOr[js.Array[JDatumEntry]]
    ): Map[DataHash, KeepRaw[Data]] =
        entries.toOption.toSeq.flatten.map { e =>
            val bytes = ByteString.fromHex(e.datum).bytes
            val datum = KeepRaw.unsafe(Data.fromCbor(bytes), bytes)
            val hash = DataHash.fromHex(e.hash)
            if datum.dataHash != hash then
                typeError(
                  s"datum hash ${e.hash} is not the hash of its bytes, ${datum.dataHash.toHex}"
                )
            hash -> datum
        }.toMap

    /** The chain options of `options`, spec [SF-6]. An unknown value throws a `TypeError`. */
    private def chainConfigOf(options: JsEmulatorOptions): ChainConfig = {
        def choice[A](value: js.UndefOr[String], name: String, default: A, cases: (String, A)*): A =
            value.toOption.fold(default) { value =>
                cases.toMap.getOrElse(
                  value,
                  typeError(
                    s"$name must be ${cases.map(c => s"\"${c._1}\"").mkString(" or ")}, got: \"$value\""
                  )
                )
            }
        val defaults = ChainConfig.instant
        defaults
            .withBlockProduction(
              choice(
                options.blockProduction,
                "blockProduction",
                defaults.blockProduction,
                "instant" -> BlockProduction.Instant,
                "manual" -> BlockProduction.Manual
              )
            )
            .withAwaitTx(
              choice(
                options.awaitTx,
                "awaitTx",
                defaults.awaitTx,
                "produceBlock" -> AwaitTxPolicy.ProduceBlock,
                "waitOnly" -> AwaitTxPolicy.WaitOnly
              )
            )
            .withOrphanTxs(
              choice(
                options.orphanTxs,
                "orphanTxs",
                defaults.orphanTxs,
                "returnToMempool" -> OrphanTxPolicy.ReturnToMempool,
                "drop" -> OrphanTxPolicy.Drop
              )
            )
            .withSecurityParam(
              options.securityParam.toOption.fold(defaults.securityParam) { value =>
                  val k = longOf(value, "securityParam")
                  if k < 1 then typeError(s"securityParam must be at least 1, got: $k")
                  k
              }
            )
            .withPoolPerformance(
              // ledger-rules spec [ES-10a]
              choice(
                options.poolPerformance,
                "poolPerformance",
                defaults.poolPerformance,
                "ideal" -> PoolPerformance.Ideal,
                "measured" -> PoolPerformance.Measured
              )
            )
            .withActiveSlotsCoeff(
              // ledger-rules spec [ES-2a], [ES-10a]: checked here, not by the require of
              // ChainConfig, so the TypeError names the option
              options.activeSlotsCoeff.toOption.fold(defaults.activeSlotsCoeff) { f =>
                  def fail: Nothing = typeError(
                    s"activeSlotsCoeff must be a fraction in (0, 1], got: ${js.JSON.stringify(f)}"
                  )
                  val n = Try(longOf(f.numerator, "activeSlotsCoeff")).getOrElse(fail)
                  val d = Try(longOf(f.denominator, "activeSlotsCoeff")).getOrElse(fail)
                  if n <= 0 || d <= 0 || n > d then fail
                  UnitInterval(n, d)
              }
            )
            .withBlockIssuer(
              // ledger-rules spec [ES-10a]: absent is Unattributed
              options.blockIssuer.toOption.fold(defaults.blockIssuer) { hex =>
                  Try(PoolKeyHash.fromHex(hex)).toOption match
                      case Some(pool) => BlockIssuer.pool(pool)
                      case None =>
                          typeError(s"blockIssuer must be the hex of 28 bytes, got: \"$hex\"")
              }
            )
    }

    private def pointOf(point: ChainPoint): JsChainPoint =
        js.Dynamic
            .literal(slot = point.slot.toDouble, blockHash = point.blockHash.toHex)
            .asInstanceOf[JsChainPoint]

    private def tipOf(tip: ChainTip): JsChainTip =
        js.Dynamic
            .literal(
              slot = tip.slot.toDouble,
              blockHash = tip.blockHash.toHex,
              height = tip.blockNo.toDouble
            )
            .asInstanceOf[JsChainTip]

    /** The point `point` names. Throws a `TypeError` for anything else. */
    private def chainPointOf(point: JsChainPoint): ChainPoint = {
        val hash =
            try BlockHash.fromHex(point.blockHash)
            catch
                case NonFatal(e) => typeError(s"on.blockHash is not a block hash: ${e.getMessage}")
        ChainPoint(longOf(point.slot, "on.slot"), hash)
    }

    /** The transactions in `txs`. Throws a `TypeError` for an element that is not one. */
    private def txsOf(txs: js.Array[Uint8Array]): Seq[Transaction] =
        txs.toSeq.zipWithIndex.map { (bytes, index) =>
            try Transaction.fromCbor(bytes.toByteArray)
            catch
                case NonFatal(e) => typeError(s"txs[$index] is not a transaction: ${e.getMessage}")
        }

    /** A forge or rollback result. `errorKind` is the name of the error case. */
    private def chainResult(result: Either[ForgeError | RollbackError, ChainPoint]): JChainResult =
        result match
            case Right(point) =>
                js.Dynamic
                    .literal(isSuccess = true, point = pointOf(point))
                    .asInstanceOf[JChainResult]
            case Left(error) =>
                val message = error match
                    case e: ForgeError    => e.message
                    case e: RollbackError => e.message
                js.Dynamic
                    .literal(isSuccess = false, error = message, errorKind = error.productPrefix)
                    .asInstanceOf[JChainResult]

    private def txStatusInfo(status: TxStatus): JTxStatusInfo = {
        val info = status match
            case TxStatus.Pending => js.Dynamic.literal(status = "Pending")
            case TxStatus.Confirmed(depth, point) =>
                js.Dynamic.literal(
                  status = "Confirmed",
                  depth = depth.toDouble,
                  point = pointOf(point)
                )
            case TxStatus.Dropped(DropReason.Invalid(error)) =>
                js.Dynamic.literal(status = "Dropped", reason = "Invalid", error = error.message)
            case TxStatus.Dropped(DropReason.Expired) =>
                js.Dynamic.literal(status = "Dropped", reason = "Expired")
            case TxStatus.Dropped(DropReason.InputsSpent) =>
                js.Dynamic.literal(status = "Dropped", reason = "InputsSpent")
            case TxStatus.RolledBack => js.Dynamic.literal(status = "RolledBack")
            case TxStatus.NotFound   => js.Dynamic.literal(status = "NotFound")
        info.asInstanceOf[JTxStatusInfo]
    }

    /** The view `view` names, spec [SF-9]. Throws a `TypeError` for any other shape. */
    private def viewOf(view: js.Any): ChainView = {
        def invalid: Nothing =
            typeError("view must be \"tip\", \"mempool\" or { stable: n }")
        js.typeOf(view) match
            case "string" =>
                view.asInstanceOf[String] match
                    case "tip"     => ChainView.Tip
                    case "mempool" => ChainView.Mempool
                    case _         => invalid
            case "object"
                if view != null && js.Object.keys(view.asInstanceOf[js.Object]).toSeq == Seq(
                  "stable"
                ) =>
                ChainView.Stable(intOf(view.asInstanceOf[js.Dynamic].stable, "view.stable"))
            case _ => invalid
    }

    private def message(error: ChainViewError): String = error match
        case ChainViewError.DepthOutOfRange(depth, tipHeight) =>
            s"view { stable: $depth } is outside 1 .. ${tipHeight + 1}"
        case ChainViewError.SnapshotDropped(depth) =>
            s"the ledger state at depth $depth is deeper than securityParam + 1, and is not kept"

    // The deprecated constructor is still the only way to obtain a `JEmulator` instance to write
    // the real, already-built `Emulator` into - see `JsUtxo.wrap`'s doc for the same placeholder
    // pattern. Internal use is intentional, hence the suppression.
    @nowarn("cat=deprecation")
    private def wrapScalaEmulator(scalaEmulator: Emulator, slotConfig: JsSlotConfig): JEmulator = {
        val wrapper = new JEmulator(
          JsCbor.encode(Map.empty: Utxos),
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
      * @throws TypeError
      *   if an option is invalid.
      */
    @JSExportStatic
    def create(info: JsCardanoInfo, options: JsEmulatorOptions): JEmulator =
        create(info, options, () => js.Date.now())

    /** `create`, with `now` as the time source of the wall clock and of the default slot. Every
      * invalid option throws a `TypeError`, as the other input checks of the facade do.
      */
    private[node] def create(
        info: JsCardanoInfo,
        options: JsEmulatorOptions,
        now: () => Double
    ): JEmulator =
        try createUnchecked(info, options, now)
        catch
            case e: js.JavaScriptException => throw e
            case NonFatal(e)               => typeError(Option(e.getMessage).getOrElse(e.toString))

    private def createUnchecked(
        info: JsCardanoInfo,
        options: JsEmulatorOptions,
        now: () => Double
    ): JEmulator = {
        val wall = options.clock.toOption match
            case None | Some("manual") => false
            case Some("wall")          => true
            case Some(other) => typeError(s"clock must be \"manual\" or \"wall\", got: \"$other\"")
        val cardanoInfo = options.protocolParams.toOption.fold(info.underlying) { params =>
            val base = info.underlying
            base.copy(protocolParams =
                JBalancer.paramsOf(params, "protocolParams", base.protocolParams)
            )
        }
        val utxos: Utxos = options.utxos.toOption.toSeq.flatten.map { u =>
            JsUtxo.input(u) -> JsUtxo.output(u)
        }.toMap
        val slot: SlotNo = options.slot.toOption
            .map(longOf(_, "slot"))
            .getOrElse(cardanoInfo.slotConfig.timeToSlot(now().toLong))
        // ledger-rules spec [ES-7a]: the mainnet treasury when not given
        val treasury = options.treasury.toOption
            .fold(AccountState.mainnet.treasury)(t => Coin(longOf(t, "treasury")))
        if treasury.value < 0 then typeError("treasury must not be negative")
        val reserves = options.reserves.toOption
            .fold(AccountState.mainnet.reserves)(r => Coin(longOf(r, "reserves")))
        if reserves.value < 0 then typeError("reserves must not be negative")
        val env = UtxoEnv(
          slot,
          cardanoInfo.protocolParams,
          CertState.empty,
          cardanoInfo.network,
          treasury
        )
        val initState = EmulatorInitialState(
          utxos = utxos,
          stakeRegistrations = parseStakeRegistrations(options.stakeRegistrations),
          poolRegistrations = parsePoolRegistrations(options.poolRegistrations, env),
          drepRegistrations = parseDrepRegistrations(options.drepRegistrations),
          datums = Map.empty,
          committee = options.committee.toOption.map(committeeOf),
          committeeHotKeys = options.committeeHotKeys.toOption.toSeq.flatten.map { k =>
              parseCredential(k.coldCredentialType, k.coldCredentialHash) ->
                  parseCredential(k.hotCredentialType, k.hotCredentialHash)
          }.toMap,
          constitution = options.constitution.toOption.map(constitutionOf),
          reserves = reserves
        )
        val context = Context(env = env, slotConfig = cardanoInfo.slotConfig)
        val scalaEmulator = Emulator.withState(initState, context, chainConfigOf(options))
        scalaEmulator.seedDatums(parseDatums(options.datums))
        val wrapper = wrapScalaEmulator(scalaEmulator, JsSlotConfig.wrap(cardanoInfo.slotConfig))
        if wall then wrapper.wallClock = Some(now)
        wrapper
    }
}
