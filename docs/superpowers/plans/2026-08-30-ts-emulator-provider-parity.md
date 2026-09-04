# TypeScript Emulator Provider Parity Implementation Plan

> **For agentic workers:** REQUIRED SUB-SKILL: Use superpowers:subagent-driven-development (recommended) or superpowers:executing-plans to implement this plan task-by-task. Steps use checkbox (`- [ ]`) syntax for tracking.

**Goal:** Make the `scalus` npm Emulator a self-sufficient provider backend, so lucid-evolution, MeshJS and the Evolution SDK stop hand-rolling UTxO codecs, protocol parameters and evaluation plumbing around it.

**Architecture:** Tasks 1-4 add the Scala capabilities the facade promises. Tasks 5-7 add JS handle classes that wrap real Scala ledger values, so a `Utxo` handed out by a query can be handed straight back with no re-encoding. Tasks 8-12 rebuild the `Emulator` facade on top of them. Task 13 regenerates the declarations and proves the result through the two SDKs. Task 14 removes the duplicated JVM/JS emulator implementations.

**Tech Stack:** Scala 3.3.8, Scala.js 1.x (ESModule output), ScalaTest, `scalus-ts-exporter` (TASTy → `.d.ts`), vitest, `@meshsdk/core`, `@lucid-evolution/lucid`.

**Spec:** `docs/superpowers/specs/2026-08-30-ts-emulator-provider-parity-design.md` (§4, §5, §7.2)

## Global Constraints

- Run sbt inside the nix devshell: `nix develop .#ci --accept-flake-config --command bash -c "sbt --batch -Dsbt.supershell=false -Dsbt.log.noformat=true '<task>'"`. Never run two sbt commands concurrently.
- In a git worktree, `plutus-conformance` must be symlinked to the primary checkout's nix-store target.
- **`scalus.d.ts` is generated.** Never hand-edit it. Run `scalusCardanoLedgerJS/generateDts` and commit the result; `checkDtsUpToDate` gates drift in `ci-js`.
- **No default parameters on public entry points** — explicit overloads instead (interop guide, Tier 0).
- **Absence is `undefined`, never `null`.** Use `js.UndefOr[T]`, not `T | Null`.
- **Quantities are `js.BigInt`; slots, indices, sizes and POSIX milliseconds are `Double`.** Lovelace exceeds `Number.MAX_SAFE_INTEGER`.
- **Hashes, policy IDs, asset names and credentials are lowercase hex `String`.** Transaction, datum and script payloads are `Uint8Array`.
- **TS names carry no `J`/`Js` prefix.** The Scala class may be `JsValue`; `@JSExportTopLevel("Value")` is what ships.
- **One accessor convention on handles: every accessor is a `def`, never a `val`.** A `val` becomes an own enumerable property and a `def` does not, so mixing them would make some handles spreadable and `toEqual`-comparable while their neighbours are not.
- **Every handle carries `toObject()`**, returning a plain structural object, and every example asserts through it. Handle accessors are prototype members, so `expect(a).toEqual(b)` passes for two *different* handles — a test that fails open.
- **Errors follow spec §4.1:** a rejected transaction is a result, a failing script and malformed input throw, and every thrown class extends `js.Error`.
- **MiMa gates the JS facade.** Every changed public signature needs a reviewed `mimaBinaryIssueFilters` entry in `build.sbt` with a comment saying why. Verify with `scalusCardanoLedgerJS/mimaReportBinaryIssues`.
- Scala 3 style: braces for top-level and multi-line bodies, indentation syntax for `if`/`match`, `then` in `if`.
- Run `scalafmtAll` before every commit; `ci-jvm` fails on a single unformatted file.

## File Structure

| File | Responsibility |
|---|---|
| `scalus-cardano-ledger/shared/.../node/BlockchainProvider.scala` | `SubmitError.rule` (Task 1) |
| `scalus-cardano-ledger/shared/.../node/UtxoQuery.scala` | `UtxoSource.FromPaymentCredential` (Task 2) |
| `scalus-cardano-ledger/shared/.../node/EmulatorBase.scala` | `findUtxosSync`, `stakeDistribution` (Tasks 2, 4) |
| `scalus-core/shared/.../ledger/ProtocolParams.scala` | `toBlockfrostJson` (Task 3) |
| `scalus-core/js/.../ledger/JsValue.scala` | `Asset` and `Value` handles (Task 5) |
| `scalus-core/js/.../ledger/JsUtxo.scala` | `Utxo` handle (Task 6) |
| `scalus-core/js/.../ledger/JsProtocolParams.scala` | `ProtocolParams` and `CardanoInfo` handles (Task 7) |
| `scalus-cardano-ledger/js/.../node/JEmulator.scala` | the `Emulator` facade (Tasks 8-12) |
| `scalus-cardano-ledger/js/src/main/npm/__tests__/` | integration proof through mesh and lucid (Task 13) |

Handles live in `scalus-core/js` because the types they wrap (`Value`, `MultiAsset`, `TransactionInput`, `TransactionOutput`, `ProtocolParams`, `CardanoInfo`) are all in `scalus-core`. The emulator facade stays in `scalus-cardano-ledger/js`. `generateDts` already passes both as TASTy roots, so no build change is needed.

---

### Task 1: `SubmitError.rule`

Today a rejected transaction gives adapters a message string. Tests then match on prose. A rule name is what lets a test assert *which* rule fired.

**Files:**
- Modify: `scalus-cardano-ledger/shared/src/main/scala/scalus/cardano/node/BlockchainProvider.scala:339-341,525-551`
- Test: `scalus-cardano-ledger/jvm/src/test/scala/scalus/cardano/node/EmulatorTest.scala`

**Interfaces:**
- Produces: `SubmitError.rule: String` — e.g. `"ValueNotConservedUTxO"`, `"OutsideValidityInterval"`. Consumed by Task 11.

- [ ] **Step 1: Write the failing test**

Append to `EmulatorTest.scala`, reusing the double-spend fixture the suite already has (the
`"Property: invalid transaction (double spend) is rejected"` test at line 278) but with the default
validators, so a real ledger rule rejects it:

```scala
    test("a rejected transaction names the rule that rejected it") {
        val initialUtxos = Map(
          Input(genesisHash, 0) -> Output(Alice.address, Value.ada(1000))
        )
        val emulator = Emulator(initialUtxos = initialUtxos)

        val tx = TxBuilder(testEnv)
            .payTo(Bob.address, Value.ada(10))
            .complete(emulator, Alice.address)
            .await()
            .transaction

        assert(emulator.submitSync(tx).isRight, "the first submission should succeed")

        // The same transaction again: its input is gone from the UTxO set.
        emulator.submitSync(tx) match
            case Left(error) =>
                assert(error.rule.nonEmpty, "a rejection must name the rule that produced it")
                assert(error.rule == "BadAllInputsUTxO", error.rule)
            case Right(_) => fail("expected the double spend to be rejected")
    }
```

The expected name comes from `AllInputsMustBeInUtxoValidator` raising
`BadAllInputsUTxOException`, which step 3 renders as `"BadAllInputsUTxO"`. If the run reports a
different name, **the run is right** — update the literal. What this test pins is that a rejection
carries a stable rule name at all, not which validator fires first.

- [ ] **Step 2: Run it to verify it fails**

```bash
nix develop .#ci --accept-flake-config --command bash -c \
  "sbt --batch -Dsbt.supershell=false -Dsbt.log.noformat=true \
   'scalusCardanoLedgerJVM/testOnly scalus.cardano.node.EmulatorTest -- -z rule'"
```

Expected: FAIL to compile — `value rule is not a member of SubmitError`.

- [ ] **Step 3: Add the member**

In `BlockchainProvider.scala`, change the trait:

```scala
sealed trait SubmitError {
    def message: String

    /** Name of the ledger rule or provider condition that produced this error, e.g.
      * `"ValueNotConservedUTxO"`. Stable enough to assert on in a test, unlike [[message]].
      */
    def rule: String
}
```

Give every case a `rule`, derived from its own name where there is nothing better. Add to each case class in `NetworkSubmitError` and `NodeSubmitError`:

```scala
    def rule: String = productPrefix
```

`productPrefix` is a case-class member, so it needs no reflection and is safe on Scala.js.

Then, in `fromException`, carry the ledger's own name through instead of the `SubmitError` case name. Add a `rule` field to the four `NodeSubmitError` cases that `fromException` produces:

```scala
    case class UtxoNotAvailable(
        message: String,
        unavailableInputs: Set[TransactionInput] = Set.empty,
        override val rule: String = "UtxoNotAvailable"
    ) extends NodeSubmitError

    case class TransactionExpired(message: String, override val rule: String = "TransactionExpired")
        extends NodeSubmitError

    case class ValueNotConserved(message: String, override val rule: String = "ValueNotConserved")
        extends NodeSubmitError

    case class ScriptFailure(
        message: String,
        logs: Seq[String] = Seq.empty,
        scriptHash: Option[ScriptHash] = None,
        spentBudget: ExUnits = ExUnits.zero,
        override val rule: String = "ScriptFailure"
    ) extends NodeSubmitError

    case class ValidationError(
        message: String,
        errorCode: Option[String] = None,
        override val rule: String = "ValidationError"
    ) extends NodeSubmitError
```

and populate it in `fromException` with a **literal per branch**. Not `getSimpleName`: `errorRule` is
sold to users as stable enough to assert on, and deriving it by reflection puts that promise on top
of `withMinify(true)`, which is precisely the thing that can rename a class. The match already
enumerates every case, so a literal costs one argument each and removes the risk entirely.

```scala
    def fromException(ex: TransactionException): SubmitError = {
        ex match
            case e: TransactionException.BadAllInputsUTxOException =>
                val inputs = e.missingInputs ++ e.missingCollateralInputs ++ e.missingReferenceInputs
                UtxoNotAvailable(e.explain, inputs, "BadAllInputsUTxO")
            case e: TransactionException.BadInputsUTxOException =>
                UtxoNotAvailable(e.explain, Set.empty, "BadInputsUTxO")
            case e: TransactionException.BadCollateralInputsUTxOException =>
                UtxoNotAvailable(e.explain, Set.empty, "BadCollateralInputsUTxO")
            case e: TransactionException.BadReferenceInputsUTxOException =>
                UtxoNotAvailable(e.explain, Set.empty, "BadReferenceInputsUTxO")
            case e: TransactionException.OutsideValidityIntervalException =>
                TransactionExpired(e.explain, "OutsideValidityInterval")
            case e: TransactionException.ValueNotConservedUTxOException =>
                ValueNotConserved(e.explain, "ValueNotConservedUTxO")
            case e: TransactionException.NativeScriptsException =>
                ScriptFailure(e.explain, Seq.empty, None, ExUnits.zero, "NativeScripts")
            case e: TransactionException.PlutusScriptValidationException =>
                ScriptFailure(
                  e.explain,
                  e.logs,
                  e.scriptHash,
                  e.spentBudget,
                  "PlutusScriptValidation"
                )
            case e =>
                ValidationError(e.explain, None, "ValidationError")
    }
```

That set of nine names is the documented contract (spec §4.7). Adding a branch means adding a name
to both.

- [ ] **Step 4: Run the test**

Same command as step 2. Expected: PASS. Then run the whole suite — `scalusCardanoLedgerJVM/test` — because the added case-class parameters change `unapply` arity for any pattern match that binds all fields.

- [ ] **Step 5: Add the MiMa filters**

```bash
nix develop .#ci --accept-flake-config --command bash -c \
  "sbt --batch -Dsbt.supershell=false -Dsbt.log.noformat=true \
   'scalusCardanoLedgerJVM/mimaReportBinaryIssues'"
```

Add one `ProblemFilters.exclude[...]` per reported problem to `build.sbt`, each with a comment naming this task and why the break is intended. Re-run until clean.

- [ ] **Step 6: Commit**

```bash
scalafmtAll
git add scalus-cardano-ledger build.sbt
git commit -m "feat: name the rule that rejected a transaction in SubmitError"
```

---

### Task 2: Synchronous UTxO queries and a payment-credential source

The facade's `getUtxos(filter)` must filter in Scala. Two gaps: `findUtxos` returns a `Future` the emulator never needed, and the query algebra has no payment-credential source.

**Files:**
- Modify: `scalus-cardano-ledger/shared/src/main/scala/scalus/cardano/node/UtxoQuery.scala:22-45,218-250`
- Modify: `scalus-cardano-ledger/shared/src/main/scala/scalus/cardano/node/EmulatorBase.scala`
- Test: `scalus-cardano-ledger/jvm/src/test/scala/scalus/cardano/node/EmulatorTest.scala`

**Interfaces:**
- Produces: `UtxoSource.FromPaymentCredential(credential: Credential)`.
- Produces: `EmulatorBase.findUtxosSync(query: UtxoQuery): Utxos`. Consumed by Task 10.

- [ ] **Step 1: Write the failing test**

```scala
    test("findUtxosSync filters by payment credential") {
        val alice = Alice.address(Network.Mainnet)
        val emulator = Emulator.withAddresses(Seq(alice))
        val credential = alice.keyHash
            .map(Credential.KeyHash(_))
            .getOrElse(fail("test address has no payment key hash"))
        val found = emulator.findUtxosSync(UtxoQuery(UtxoSource.FromPaymentCredential(credential)))
        assert(found.size == 1)
        assert(found.values.forall(_.address == alice))
    }
```

Use whatever accessor `Address` exposes for the payment credential; if the name differs, adapt the two lines that build `credential`, not the assertion.

- [ ] **Step 2: Run it to verify it fails**

```bash
nix develop .#ci --accept-flake-config --command bash -c \
  "sbt --batch -Dsbt.supershell=false -Dsbt.log.noformat=true \
   'scalusCardanoLedgerJVM/testOnly scalus.cardano.node.EmulatorTest -- -z payment'"
```

Expected: FAIL to compile — neither `FromPaymentCredential` nor `findUtxosSync` exists.

- [ ] **Step 3: Add the source case**

In `UtxoQuery.scala`, inside `object UtxoSource`:

```scala
    /** Every UTxO whose address carries this payment credential, whatever its stake part.
      *
      * Providers index by address; this is the query a wallet actually wants, because one payment
      * credential appears at many addresses once staking is involved.
      */
    case class FromPaymentCredential(credential: Credential) extends UtxoSource
```

In `EmulatorBase.evalQuery`'s `evalSource`, add the arm:

```scala
            case UtxoSource.FromPaymentCredential(credential) =>
                utxos.filter { case (_, output) =>
                    output.address.keyHash.map(Credential.KeyHash(_)).contains(credential) ||
                    output.address.scriptHash.map(Credential.ScriptHash(_)).contains(credential)
                }
```

Adapt the accessor names to whatever `Address` exposes; the arm must match a key-hash payment part and a script payment part, and must not match on the stake part.

Every other `BlockchainProvider` implementation must also answer the new case. Compile the whole module and fix each non-exhaustive match the compiler reports — `BlockfrostProvider` will need either a real implementation or an explicit `UtxoQueryError.NotSupported`.

- [ ] **Step 4: Add the synchronous query**

In `EmulatorBase.scala`, next to `findUtxos`:

```scala
    /** [[findUtxos]] without the `Future`. The emulator's state is in memory and its query
      * evaluation is pure, so the effect wrapper is an interface formality — JavaScript and Java
      * callers both want the value.
      */
    def findUtxosSync(query: UtxoQuery): Utxos = EmulatorBase.evalQuery(utxos, query)

    override def findUtxos(query: UtxoQuery): Future[Either[UtxoQueryError, Utxos]] =
        Future.successful(Right(findUtxosSync(query)))
```

- [ ] **Step 5: Run the test**

Same command as step 2. Expected: PASS. Then `scalusCardanoLedgerJVM/test` for the exhaustivity fixes.

- [ ] **Step 6: Commit**

```bash
scalafmtAll
git add scalus-cardano-ledger
git commit -m "feat: query UTxOs by payment credential, synchronously"
```

---

### Task 3: `ProtocolParams.toBlockfrostJson`

Blockfrost JSON is how parameters reach mesh and lucid: both already map it into their own shapes. We can read it and cannot write it.

**Files:**
- Modify: `scalus-core/shared/src/main/scala/scalus/cardano/ledger/ProtocolParams.scala`
- Test: `scalus-core/jvm/src/test/scala/scalus/cardano/ledger/ProtocolParamsTest.scala` (create if absent)

**Interfaces:**
- Produces: `ProtocolParams.toBlockfrostJson(params: ProtocolParams): String`, and `params.toBlockfrostJson` if an extension reads better. Consumed by Task 7.

- [ ] **Step 1: Write the failing round-trip test**

```scala
class ProtocolParamsTest extends AnyFunSuite {

    test("Blockfrost JSON round-trips through ProtocolParams") {
        for params <- Seq(
              CardanoInfo.mainnet.protocolParams,
              CardanoInfo.preprod.protocolParams,
              CardanoInfo.preview.protocolParams
            )
        do
            val json = ProtocolParams.toBlockfrostJson(params)
            val back = ProtocolParams.fromBlockfrostJson(json)
            assert(back == params, ProtocolParams.diff(params, back).mkString("\n"))
    }
}
```

`ProtocolParams.diff` already exists and reports field-level differences, which is what makes a failure here readable.

- [ ] **Step 2: Run it to verify it fails**

```bash
nix develop .#ci --accept-flake-config --command bash -c \
  "sbt --batch -Dsbt.supershell=false -Dsbt.log.noformat=true \
   'scalusJVM/testOnly scalus.cardano.ledger.ProtocolParamsTest'"
```

Expected: FAIL to compile — `toBlockfrostJson` is not a member.

- [ ] **Step 3: Implement the writer**

Read `fromBlockfrostJson` first and mirror it field for field — it is the specification for this function, including which fields Blockfrost renders as decimal strings rather than numbers. Write with `ujson`, which the reader already uses:

```scala
    /** Render as Blockfrost's `/epochs/{n}/parameters` JSON.
      *
      * The inverse of [[fromBlockfrostJson]], and tested as such. This shape exists because it is
      * what the JavaScript SDKs already know how to consume: both MeshJS and the Evolution SDK
      * ship a Blockfrost parameter mapping, so handing them this costs them no new code.
      */
    def toBlockfrostJson(params: ProtocolParams): String = {
        val obj = ujson.Obj(
          "min_fee_a" -> params.txFeePerByte,
          "min_fee_b" -> params.txFeeFixed,
          "max_tx_size" -> params.maxTxSize,
          "max_val_size" -> params.maxValueSize.toString,
          "key_deposit" -> params.stakeAddressDeposit.toString,
          "pool_deposit" -> params.stakePoolDeposit.toString,
          "coins_per_utxo_size" -> params.utxoCostPerByte.toString,
          "collateral_percent" -> params.collateralPercentage,
          "max_collateral_inputs" -> params.maxCollateralInputs
          // ... every remaining field that fromBlockfrostJson reads
        )
        ujson.write(obj)
    }
```

Two rules the reader will tell you: a field the reader parses with the `asLong` extension may arrive as a string, so write it as a string; and cost models go under `cost_models` keyed `"PlutusV1"`/`"PlutusV2"`/`"PlutusV3"`, not positionally.

- [ ] **Step 4: Run the test**

Same command as step 2. Expected: PASS for all three networks. A failure prints the differing fields via `diff`; fix the writer, not the test.

- [ ] **Step 5: Commit**

```bash
scalafmtAll
git add scalus-core
git commit -m "feat: render ProtocolParams as Blockfrost JSON"
```

---

### Task 4: Stake distribution

**Files:**
- Modify: `scalus-cardano-ledger/shared/src/main/scala/scalus/cardano/node/EmulatorBase.scala`
- Test: `scalus-cardano-ledger/jvm/src/test/scala/scalus/cardano/node/EmulatorTest.scala`

**Interfaces:**
- Produces: `case class StakeDistributionEntry(credential: Credential, pool: Option[PoolKeyHash], stake: Coin, rewards: Coin)` and `EmulatorBase.stakeDistribution: Seq[StakeDistributionEntry]`. Consumed by Task 12.

- [ ] **Step 1: Write the failing test**

```scala
    test("stakeDistribution sums UTxO value and rewards per stake credential") {
        val stakeKeyHash = StakeKeyHash.fromHex("c" * 56)
        val stakeCredential = Credential.KeyHash(AddrKeyHash.fromByteString(stakeKeyHash))
        // A base address: Alice's payment part, the stake part above.
        val delegated = ShelleyAddress(
          network = Network.Mainnet,
          payment = Alice.address.asInstanceOf[ShelleyAddress].payment,
          delegation = ShelleyDelegationPart.Key(stakeKeyHash)
        )
        val emulator = Emulator.withRegisteredStakeCredentials(
          initialUtxos = Map(Input(genesisHash, 0) -> Output(delegated, Value.ada(500))),
          initialStakeRewards = Map(stakeCredential -> Coin(1_000_000L))
        )

        val entry = emulator.stakeDistribution
            .find(_.credential == stakeCredential)
            .getOrElse(fail("credential missing from the distribution"))
        assert(entry.rewards == Coin(1_000_000L))
        assert(entry.stake == Coin.ada(500), "UTxO value at the delegated address counts as stake")
        assert(entry.pool.isEmpty, "registered but delegating to no pool")
    }
```

`ShelleyAddress` / `ShelleyDelegationPart` are the constructors in `scalus.cardano.address`; if the
delegation-part case is spelled differently there, adapt those two lines. `genesisHash`, `Alice`
and `testEnv` already exist in this suite.

- [ ] **Step 2: Run it to verify it fails**

```bash
nix develop .#ci --accept-flake-config --command bash -c \
  "sbt --batch -Dsbt.supershell=false -Dsbt.log.noformat=true \
   'scalusCardanoLedgerJVM/testOnly scalus.cardano.node.EmulatorTest -- -z stakeDistribution'"
```

Expected: FAIL to compile.

- [ ] **Step 3: Implement it**

In `EmulatorBase.scala`:

```scala
    /** Live stake per registered stake credential: the lovelace sitting at addresses delegating to
      * it, plus its reward balance, and the pool it delegates to.
      *
      * This is the emulator's answer to "who controls how much stake", the query a governance or
      * delegation test needs. It is not a reward calculation: nothing is paid out.
      *
      * Pointer addresses are ignored — they are deprecated and carry no stake here.
      */
    def stakeDistribution: Seq[StakeDistributionEntry] = {
        val dstate = certState.dstate
        val byCredential = utxos.values
            .flatMap { output =>
                output.address.stakeCredential.map(_ -> output.value.coin.value)
            }
            .groupMapReduce(_._1)(_._2)(_ + _)
        val credentials = dstate.rewards.keySet ++ byCredential.keySet
        credentials.toSeq.map { credential =>
            StakeDistributionEntry(
              credential = credential,
              pool = dstate.stakePools.get(credential),
              stake = Coin(byCredential.getOrElse(credential, 0L)),
              rewards = dstate.rewards.getOrElse(credential, Coin.zero)
            )
        }
    }
```

and next to `DelegationInfo`:

```scala
/** One row of [[EmulatorBase.stakeDistribution]]. */
case class StakeDistributionEntry(
    credential: Credential,
    pool: Option[PoolKeyHash],
    stake: Coin,
    rewards: Coin
)
```

`output.address.stakeCredential` may not exist under that name — use whatever `Address` exposes for the delegation part, returning `None` for enterprise and pointer addresses.

- [ ] **Step 4: Run the test**

Same command as step 2. Expected: PASS.

- [ ] **Step 5: Commit**

```bash
scalafmtAll
git add scalus-cardano-ledger
git commit -m "feat: expose the emulator's stake distribution"
```

---

### Task 5: Secondary constructors in the exporter, then the `Asset` and `Value` handles

The generator change comes first because the handle pattern does not compile into a usable `.d.ts`
without it. `ExportCollector.classMembers` (line 486) reads `sym.primaryConstructor` and nothing
else, and skips it when `Flags.Private` — so a handle with a private primary constructor taking the
Scala value and a public secondary taking JS values emits **no** constructor at all, and
`new Value(…)` becomes a TypeScript error.

Then the first two handles. Get the pattern right here; Tasks 6 and 7 copy it.

**Files:**
- Modify: `scalus-ts-exporter/src/main/scala/scalus/tsexport/ExportCollector.scala:486-495`
- Create: `scalus-ts-exporter/fixtures/src/main/scala/tsfixtures/Ctors.scala`
- Modify: `scalus-ts-exporter/src/test/resources/golden/fixtures.d.ts` (regenerated, not hand-edited)
- Create: `scalus-core/js/src/main/scala/scalus/cardano/ledger/JsValue.scala`
- Test: `scalus-core/js/src/test/scala/scalus/cardano/ledger/JsValueTest.scala`

**Interfaces:**
- Produces: `ExportCollector` emits every public constructor — primary and secondary — as overloads of one `TsMember.Ctor`.
- Produces: `JsAsset` exported as `"Asset"`, with `policyId: String`, `assetName: String`, `quantity: js.BigInt`, `unit: String`, `toObject()`.
- Produces: `JsValue` exported as `"Value"`, with `coin: js.BigInt`, `assets: js.Array[JsAsset]`, `plus(JsValue): JsValue`, `toObject()`, statics `ada`, `of`.
- Produces: `JsValue.wrap(v: scalus.cardano.ledger.Value): JsValue` and `JsValue.underlying` — the internal bridge Tasks 6, 8 and 9 use. Not exported to TypeScript.

- [ ] **Step 0a: Write the failing exporter fixture and test**

Create `scalus-ts-exporter/fixtures/src/main/scala/tsfixtures/Ctors.scala`:

```scala
package tsfixtures

import scala.scalajs.js
import scala.scalajs.js.annotation.JSExportTopLevel

/** A handle: the private primary takes the wrapped value, the public secondary takes JS values. */
@JSExportTopLevel("Ctors")
class Ctors private (private val wrapped: List[String]) extends js.Object {
    def this(head: String) = this(List(head))
    def this(head: String, tail: String) = this(List(head, tail))
    def size: Double = wrapped.size.toDouble
}
```

Add to `ExportCollectorTest.scala`:

```scala
    test("public secondary constructors are emitted as overloads; a private primary is not") {
        val c = decl("Ctors").asInstanceOf[TsDecl.Cls]
        val ctors = c.members.collect { case ctor: TsMember.Ctor => ctor }
        assert(ctors.sizeIs == 1, "all constructors belong to one Ctor member")
        val paramLists = ctors.head.overloads
        assert(paramLists.sizeIs == 2, s"expected two overloads, got ${paramLists.size}")
        assert(paramLists.map(_.map(_.name)) == List(List("head"), List("head", "tail")))
    }
```

Adapt `ctors.head.overloads` to whatever `TsMember.Ctor` names its list of parameter lists.

- [ ] **Step 0b: Run it to verify it fails**

```bash
nix develop .#ci --accept-flake-config --command bash -c \
  "sbt --batch -Dsbt.supershell=false -Dsbt.log.noformat=true \
   'scalusTsExporter/testOnly scalus.tsexport.ExportCollectorTest -- -z secondary'"
```

Expected: FAIL — no `Ctor` member at all, because the primary is private and secondaries are ignored.

- [ ] **Step 0c: Collect secondary constructors**

In `ExportCollector.classMembers`, replace the primary-only block:

```scala
                def classMembers(sym: Symbol, ctorDoc: Option[TsDoc]): List[TsMember] = {
                    // Every public constructor, primary and secondary, becomes one overload.
                    // A handle class keeps its primary private — it takes the wrapped Scala value —
                    // and exposes a JS-friendly secondary, so collecting only the primary would
                    // emit no constructor at all and make `new Utxo(…)` a type error.
                    val ctorSymbols =
                        (sym.primaryConstructor +: sym.declaredMethods.filter(_.isClassConstructor))
                            .distinct
                            .filter(c => c.exists && !c.flags.is(Flags.Private))
                    val ctor = ctorSymbols.flatMap(c => methodOverload(sym, c)) match
                        case Nil => Nil
                        case overloads =>
                            val doc = ctorSymbols.head.docstring
                                .flatMap(DocConverter.convert)
                                .orElse(ctorDoc)
                            List(TsMember.Ctor(overloads.map(_.params), doc))
```

`declaredMethods.filter(_.isClassConstructor)` is the quoted-reflection way to reach secondaries;
if that predicate is named differently in this Scala version, find the equivalent — do not fall back
to name matching on `"<init>"`.

- [ ] **Step 0d: Run the exporter tests**

```bash
nix develop .#ci --accept-flake-config --command bash -c \
  "sbt --batch -Dsbt.supershell=false -Dsbt.log.noformat=true 'scalusTsExporter/test'"
```

Expected: the new test passes. `GoldenTest` will fail because the fixture added a declaration —
regenerate the golden file the way that suite documents, read the diff, and confirm the only change
is the new `Ctors` class with two constructor overloads.

- [ ] **Step 0e: Commit the generator change separately**

```bash
scalafmtAll
git add scalus-ts-exporter
git commit -m "feat(tsexport): emit public secondary constructors as overloads"
```

- [ ] **Step 1: Write the failing test**

```scala
package scalus.cardano.ledger

import org.scalatest.funsuite.AnyFunSuite
import scala.scalajs.js

class JsValueTest extends AnyFunSuite {

    test("Value.ada exposes lovelace as a BigInt and round-trips to the Scala value") {
        val v = JsValue.ada(js.BigInt("10"))
        assert(v.coin.toString == "10000000")
        assert(v.assets.length == 0)
        assert(JsValue.wrap(v.underlying).coin.toString == "10000000")
    }

    test("assets surface policy id, asset name and quantity as hex and BigInt") {
        val policy = "0" * 56
        val name = "abcd"
        val v = JsValue.of(js.BigInt("0"), js.Array(new JsAsset(policy, name, js.BigInt("5"))))
        assert(v.assets.length == 1)
        val a = v.assets(0)
        assert(a.policyId == policy)
        assert(a.assetName == name)
        assert(a.quantity.toString == "5")
        assert(a.unit == policy + name)
    }

    test("plus adds coin and merges assets") {
        val sum = JsValue.ada(js.BigInt("2")).plus(JsValue.ada(js.BigInt("3")))
        assert(sum.coin.toString == "5000000")
    }

    test("toObject yields own enumerable properties, which the handle does not") {
        val v = JsValue.ada(js.BigInt("1"))
        assert(js.Object.keys(v).length == 0, "a handle exposes nothing to spread or toEqual")
        val plain = v.toObject()
        assert(js.Object.keys(plain).toSet == Set("coin", "assets"))
        assert(plain.coin.toString == "1000000")
    }

    test("the public constructor takes lovelace") {
        assert(new JsValue(js.BigInt("250")).coin.toString == "250")
    }
}
```

- [ ] **Step 2: Run it to verify it fails**

```bash
nix develop .#ci --accept-flake-config --command bash -c \
  "sbt --batch -Dsbt.supershell=false -Dsbt.log.noformat=true \
   'scalusJS/testOnly scalus.cardano.ledger.JsValueTest'"
```

Expected: FAIL to compile — `JsValue` does not exist.

- [ ] **Step 3: Implement the handles**

```scala
package scalus.cardano.ledger

import scalus.uplc.builtin.ByteString

import scala.scalajs.js
import scala.scalajs.js.JSConverters.*
import scala.scalajs.js.annotation.{JSExportStatic, JSExportTopLevel}

/** One native asset: a policy, a name under it, and how much of it.
  *
  * `policyId` and `assetName` are hex. `unit` is the two concatenated, which is what
  * lucid-evolution and MeshJS call a unit — provided so adapters need no string arithmetic.
  */
@JSExportTopLevel("Asset")
class JsAsset(
    private val policyIdHex: String,
    private val assetNameHex: String,
    private val amount: js.BigInt
) extends js.Object {
    // `def`, not `val`, on every handle accessor: see the plain-object rule below.
    def policyId: String = policyIdHex
    def assetName: String = assetNameHex
    def quantity: js.BigInt = amount
    def unit: String = policyIdHex + assetNameHex

    /** A plain object with the same fields. Assert on this, never on the handle. */
    def toObject(): JsPlainAsset = js.Dynamic
        .literal(policyId = policyId, assetName = assetName, quantity = quantity, unit = unit)
        .asInstanceOf[JsPlainAsset]
}

/** The structural form of [[JsAsset]]. */
@TsName("PlainAsset")
trait JsPlainAsset extends js.Object {
    val policyId: String
    val assetName: String
    val quantity: js.BigInt
    val unit: String
}

/** The structural form of [[JsValue]]. */
@TsName("PlainValue")
trait JsPlainValue extends js.Object {
    val coin: js.BigInt
    val assets: js.Array[JsPlainAsset]
}

/** An amount of ada together with any native assets beside it.
  *
  * This wraps the ledger's own `Value`: the object handed to you by a query holds the real thing,
  * so passing it back to the emulator costs no re-encoding.
  */
@JSExportTopLevel("Value")
class JsValue private (private[ledger] val underlying: Value) extends js.Object {

    /** Lovelace, with no native assets. `Value.of` adds those. */
    def this(coin: js.BigInt) = this(Value(Coin(BigInt(coin.toString).toLong)))

    /** Lovelace. A `bigint` because the ada supply exceeds `Number.MAX_SAFE_INTEGER`. */
    def coin: js.BigInt = js.BigInt(underlying.coin.value.toString)

    /** The native assets, in ledger order. Empty for pure ada. */
    def assets: js.Array[JsAsset] = {
        val out = js.Array[JsAsset]()
        underlying.assets.assets.foreach { case (policyId, byName) =>
            byName.foreach { case (name, quantity) =>
                out.push(
                  new JsAsset(policyId.toHex, name.bytes.toHex, js.BigInt(quantity.toString))
                )
            }
        }
        out
    }

    /** This value plus another. Neither operand is modified. */
    def plus(other: JsValue): JsValue = new JsValue(underlying + other.underlying)

    override def toString: String = underlying.toString

    /** A plain object with the same fields.
      *
      * Handle accessors live on the prototype, so `JSON.stringify`, spread and — the dangerous one
      * — vitest's `toEqual` all see an empty object, which makes `expect(a).toEqual(b)` pass for
      * two different values. Assert through this instead.
      */
    def toObject(): JsPlainValue = js.Dynamic
        .literal(coin = coin, assets = assets.map(_.toObject()))
        .asInstanceOf[JsPlainValue]
}

object JsValue {

    /** Internal bridge: wrap a ledger value without copying. Not exported. */
    private[scalus] def wrap(value: Value): JsValue = new JsValue(value)

    /** `n` ada, as lovelace. */
    @JSExportStatic
    def ada(ada: js.BigInt): JsValue =
        new JsValue(Value(Coin(BigInt(ada.toString).toLong * 1_000_000L)))

    /** Lovelace plus native assets. */
    @JSExportStatic
    def of(coin: js.BigInt, assets: js.Array[JsAsset]): JsValue = {
        val multiAsset = assets.toSeq.foldLeft(MultiAsset.empty) { (acc, a) =>
            acc + MultiAsset.asset(
              PolicyId.fromHex(a.policyId),
              AssetName(ByteString.fromHex(a.assetName)),
              BigInt(a.quantity.toString).toLong
            )
        }
        new JsValue(Value(Coin(BigInt(coin.toString).toLong), multiAsset))
    }
}
```

`MultiAsset` has a private constructor, so build it through whatever public combinator it offers — `MultiAsset.asset`, `MultiAsset.apply(Map)` or `+`. Read `MultiAsset`'s companion and use what is there; do not add a public constructor to the shared type.

- [ ] **Step 4: Run the test**

Same command as step 2. Expected: PASS.

- [ ] **Step 5: Commit**

```bash
scalafmtAll
git add scalus-core
git commit -m "feat: Value and Asset handles for JavaScript"
```

---

### Task 6: The `Utxo` handle

**Files:**
- Create: `scalus-core/js/src/main/scala/scalus/cardano/ledger/JsUtxo.scala`
- Test: `scalus-core/js/src/test/scala/scalus/cardano/ledger/JsUtxoTest.scala`

**Interfaces:**
- Consumes: `JsValue.wrap`, `JsValue.underlying` from Task 5.
- Produces: `JsUtxo` exported as `"Utxo"` with `txHash`, `outputIndex`, `address`, `value`, `datumHash?`, `inlineDatum?`, `scriptRef?`, `scriptLanguage?`, `toObject()`, `toCbor()`, statics `fromCbor`.
- **Constructor shape corrected 2026-08-31.** One PUBLIC constructor taking JS values, plus class-private `var`s that `wrap` overwrites. The private-primary/public-secondary shape this plan originally specified **does not compile** — Scala.js rejects it with "Private methods in non-native JS classes cannot be overloaded", and `private[pkg]` fails identically. Copy the shape Task 5 shipped in `JsValue.scala`; do not re-derive it.
- **Do not use `private[ledger]` for the wrapped fields.** A qualified-private member of a non-native JS class must be `final`, and it is still emitted into `scalus.d.ts` — `visibleMember` (`ExportCollector.scala:332`) filters only `Flags.Private`, which dotty does not set for qualified private. Emitting `TransactionInput` there is an `ExportError`. Use class-private, and expose anything the emulator facade needs through a companion extension method, as Task 5 did for `underlying`.
- Produces: `JsUtxo.wrap(input: TransactionInput, output: TransactionOutput): JsUtxo`, `JsUtxo.input`, `JsUtxo.output`. Consumed by Tasks 9, 10, 12.

- [ ] **Step 1: Write the failing test**

```scala
package scalus.cardano.ledger

import org.scalatest.funsuite.AnyFunSuite
import scalus.cardano.address.{Address, Network}
import scalus.uplc.builtin.ByteString
import scala.scalajs.js

class JsUtxoTest extends AnyFunSuite {

    private val hash = TransactionHash.fromByteString(ByteString.fromHex("0" * 64))
    private val address = Address.fromString("addr_test1vzpwq95z3xyum8vqndgdd9mdnmafh3djcxnc6jemlgdmswcve6tkw")

    test("a wrapped UTxO exposes hex ids and a Value handle") {
        val utxo = JsUtxo.wrap(TransactionInput(hash, 3), TransactionOutput(address, Value.ada(7)))
        assert(utxo.txHash == "0" * 64)
        assert(utxo.outputIndex == 3.0)
        assert(utxo.address == address.toBech32)
        assert(utxo.value.coin.toString == "7000000")
        assert(utxo.datumHash.isEmpty)
        assert(utxo.inlineDatum.isEmpty)
    }

    test("a UTxO built in JavaScript round-trips to the same ledger value") {
        val built = new JsUtxo("0" * 64, 1.0, address.toBech32, JsValue.ada(js.BigInt("4")))
        assert(built.output.value == Value.ada(4))
        assert(built.input.index == 1)
    }

    test("CBOR round-trip preserves the UTxO") {
        val utxo = JsUtxo.wrap(TransactionInput(hash, 0), TransactionOutput(address, Value.ada(1)))
        val back = JsUtxo.fromCbor(utxo.toCbor())
        assert(back.txHash == utxo.txHash)
        assert(back.value.coin.toString == utxo.value.coin.toString)
    }
}
```

If `Address.fromString` rejects that literal, substitute any valid testnet address; the assertions do not depend on which.

- [ ] **Step 2: Run it to verify it fails**

```bash
nix develop .#ci --accept-flake-config --command bash -c \
  "sbt --batch -Dsbt.supershell=false -Dsbt.log.noformat=true \
   'scalusJS/testOnly scalus.cardano.ledger.JsUtxoTest'"
```

Expected: FAIL to compile.

- [ ] **Step 3: Implement it**

```scala
package scalus.cardano.ledger

import io.bullet.borer.Cbor
import scalus.cardano.address.Address

import scala.scalajs.js
import scala.scalajs.js.annotation.{JSExportStatic, JSExportTopLevel}
import scala.scalajs.js.typedarray.{byteArray2Int8Array, Uint8Array}

/** One unspent output: where it is, whose it is, and what it holds.
  *
  * Holds the ledger's own input and output, so a `Utxo` a query hands you can be handed straight
  * back — to `evaluateTx` or `addUtxo` — with no encoding step in between.
  */
@JSExportTopLevel("Utxo")
class JsUtxo(txHash0: String, outputIndex0: Double, address0: String, value0: JsValue)
    extends js.Object {

    // One public constructor, holding the ledger pair in private vars that `wrap` overwrites.
    // A private primary constructor beside a public secondary DOES NOT COMPILE — Scala.js rejects
    // it with "Private methods in non-native JS classes cannot be overloaded", and `private[pkg]`
    // fails identically. See the spec's §4 preamble. `wrap` is the only writer, and it writes only
    // to a handle it has just allocated, so these are immutable in every reachable sense.
    //
    // These are class-private, NOT `private[ledger]`: a qualified-private member of a non-native JS
    // class must be `final`, and — worse — it is still emitted into scalus.d.ts, because
    // `visibleMember` (ExportCollector.scala:332) filters only `Flags.Private`, which dotty does
    // not set for qualified private. Emitting `TransactionInput` there is an ExportError.
    private var input: TransactionInput =
        TransactionInput(TransactionHash.fromHex(txHash0), outputIndex0.toInt)
    private var output: TransactionOutput =
        TransactionOutput(Address.fromString(address0), value0.underlying)

    def txHash: String = input.transactionId.toHex
    def outputIndex: Double = input.index.toDouble
    def address: String = output.address.toBech32
    def value: JsValue = JsValue.wrap(output.value)

    /** The datum hash, when the output references a datum rather than carrying one. */
    def datumHash: js.UndefOr[String] = output.datumOption match
        case Some(DatumOption.Hash(h)) => h.toHex
        case _                         => js.undefined

    /** The datum itself as CBOR, when the output carries it inline. */
    def inlineDatum: js.UndefOr[Uint8Array] = output.datumOption match
        case Some(DatumOption.Inline(d)) => toUint8Array(Cbor.encode(d).toByteArray)
        case _                           => js.undefined

    /** The reference script as CBOR, when the output carries one. */
    def scriptRef: js.UndefOr[Uint8Array] =
        output.scriptRef.map(r => toUint8Array(Cbor.encode(r).toByteArray)).orUndefined

    /** Which language `scriptRef` is written in. */
    def scriptLanguage: js.UndefOr[String] = output.scriptRef.map(_.script).map {
        case _: Script.Native   => "Native"
        case _: Script.PlutusV1 => "PlutusV1"
        case _: Script.PlutusV2 => "PlutusV2"
        case _: Script.PlutusV3 => "PlutusV3"
    }.orUndefined

    /** This UTxO as a one-entry CBOR map from input to output, the shape `getUtxosCbor` uses. */
    def toCbor(): Uint8Array =
        toUint8Array(Cbor.encode(Map(input -> output): Utxos).toByteArray)

    /** A plain object with the same fields. Assert on this, never on the handle — see Task 5. */
    def toObject(): JsPlainUtxo = js.Dynamic
        .literal(
          txHash = txHash,
          outputIndex = outputIndex,
          address = address,
          value = value.toObject(),
          datumHash = datumHash,
          inlineDatum = inlineDatum,
          scriptRef = scriptRef,
          scriptLanguage = scriptLanguage
        )
        .asInstanceOf[JsPlainUtxo]

    override def toString: String = s"Utxo($txHash#$outputIndex at $address)"

    private def toUint8Array(bytes: Array[Byte]): Uint8Array =
        new Uint8Array(byteArray2Int8Array(bytes).buffer)
}

object JsUtxo {

    /** Wrap a ledger pair with no re-encoding. Follow the shape `JsValue.wrap` already ships from
      * Task 5 — allocate, then overwrite the private vars — rather than inventing a new one.
      *
      * **The constructor body is on this path.** `getUtxos()` calls `wrap` once per UTxO, so
      * anything the constructor does eagerly is paid per row. Task 5's `JsValue` constructor is a
      * BigInt-to-Long conversion, which is trivial; `JsUtxo`'s parses hex *and* bech32, which is
      * not. Make the parse lazy, or give `wrap` a path that skips it. Decide deliberately, say
      * which you chose in your report, and do not let `getUtxos()` pay a bech32 decode per UTxO
      * only to overwrite the result.
      */
    private[scalus] def wrap(input: TransactionInput, output: TransactionOutput): JsUtxo = ???

    /** Read back what `toCbor` wrote: a CBOR map holding exactly one input-to-output entry. */
    @JSExportStatic
    def fromCbor(cbor: Uint8Array): JsUtxo = {
        val utxos = Cbor.decode(cbor.toArray.map(_.toByte)).to[Utxos].value
        val (input, output) = utxos.headOption.getOrElse(
          throw new IllegalArgumentException("expected a CBOR map holding one UTxO, got an empty map")
        )
        new JsUtxo(input, output)
    }
}
```

Add the `JsPlainUtxo` trait alongside, mirroring `JsPlainValue` from Task 5:

```scala
/** The structural form of [[JsUtxo]]. */
@TsName("PlainUtxo")
trait JsPlainUtxo extends js.Object {
    val txHash: String
    val outputIndex: Double
    val address: String
    val value: JsPlainValue
    val datumHash: js.UndefOr[String]
    val inlineDatum: js.UndefOr[Uint8Array]
    val scriptRef: js.UndefOr[Uint8Array]
    val scriptLanguage: js.UndefOr[String]
}
```

The `withDatumHash` / `withInlineDatum` / `withScriptRef` builders named in the spec belong here too — each constructs a new `TransactionOutput` with the datum or script set and returns a new `JsUtxo`. Add them with a test each, in this same task.

- [ ] **Step 4: Run the test**

Same command as step 2. Expected: PASS.

- [ ] **Step 5: Commit**

```bash
scalafmtAll
git add scalus-core
git commit -m "feat: a Utxo handle over the ledger's own input and output"
```

---

### Task 7: `ProtocolParams` and `CardanoInfo` handles

**Files:**
- Create: `scalus-core/js/src/main/scala/scalus/cardano/ledger/JsProtocolParams.scala`
- Test: `scalus-core/js/src/test/scala/scalus/cardano/ledger/JsProtocolParamsTest.scala`

**Interfaces:**
- Consumes: `ProtocolParams.toBlockfrostJson` from Task 3.
- Produces: `JsProtocolParams` exported as `"ProtocolParams"` with the accessors listed in spec §4.4, `costModels`, `toBlockfrostJson()`, statics `fromBlockfrostJson`, `fromCardanoCliJson`.
- Produces: `JsCardanoInfo` exported as `"CardanoInfo"` with statics `mainnet()`, `preprod()`, `preview()`, `custom(network, slotConfig, params: JsProtocolParams)`, and accessors `network`, `slotConfig`, `protocolParams`, `withProtocolParams(params: JsProtocolParams)`. Consumed by Task 8.
- Produces: `JsCardanoInfo.wrap` / `.underlying`, `JsProtocolParams.wrap` / `.underlying`.

- [ ] **Step 1: Write the failing test**

```scala
package scalus.cardano.ledger

import org.scalatest.funsuite.AnyFunSuite

class JsProtocolParamsTest extends AnyFunSuite {

    test("preview info carries preview slot config and testnet network") {
        val info = JsCardanoInfo.preview()
        assert(info.network == "testnet")
        assert(info.slotConfig.zeroSlot == 0.0)
        assert(info.protocolParams.maxTxSize > 0.0)
    }

    test("mainnet parameters round-trip through Blockfrost JSON") {
        val params = JsCardanoInfo.mainnet().protocolParams
        val back = JsProtocolParams.fromBlockfrostJson(params.toBlockfrostJson())
        assert(back.txFeePerByte == params.txFeePerByte)
        assert(back.costModels.PlutusV3.length == params.costModels.PlutusV3.length)
    }

    test("custom info accepts a Yaci-style slot config") {
        val slotConfig = new SlotConfig(1_700_000_000_000d, 0d, 1000d, 500d, 0d)
        val info = JsCardanoInfo.custom(
          "testnet",
          slotConfig,
          JsCardanoInfo.preview().protocolParams
        )
        assert(info.slotConfig.epochLength == 500.0)
        assert(info.network == "testnet")
    }
}
```

- [ ] **Step 2: Run it to verify it fails**

```bash
nix develop .#ci --accept-flake-config --command bash -c \
  "sbt --batch -Dsbt.supershell=false -Dsbt.log.noformat=true \
   'scalusJS/testOnly scalus.cardano.ledger.JsProtocolParamsTest'"
```

Expected: FAIL to compile.

- [ ] **Step 3: Implement the handles**

**Constructor shape, corrected 2026-08-31:** these handles take one PUBLIC constructor plus
class-private `var`s overwritten by a companion `wrap`, exactly as Task 5's `JsValue` ships. The
private-primary/public-secondary shape does not compile, and `private[pkg]` fields are still
emitted into `scalus.d.ts`. Copy Task 5's file; do not re-derive the pattern. Pin
`js.Object.keys(handle).length == 0` for every handle you add, as Task 5 does.

Write `JsProtocolParams` as a handle over `ProtocolParams` with one accessor per field named in spec §4.4, using the `Double`/`js.BigInt` split from the Global Constraints: `txFeePerByte`, `txFeeFixed`, `maxTxSize`, `maxValueSize` and the percentages are `Double`; every deposit and `utxoCostPerByte`, `maxTxExecutionMemory`, `maxTxExecutionSteps` are `js.BigInt`. `costModels` returns a `js.Object` trait with three `js.Array[Double]` fields:

```scala
/** The three Plutus cost models, by language rather than by position. */
@TsName("CostModels")
trait JsCostModels extends js.Object {
    val PlutusV1: js.Array[Double]
    val PlutusV2: js.Array[Double]
    val PlutusV3: js.Array[Double]
}
```

`costModels` is a `def` like every other accessor, but it appears in the `.d.ts` as a property, matching spec §4.4.

`JsCardanoInfo` wraps `CardanoInfo`. `custom` builds `CardanoInfo(params.underlying, network, slotConfig)` — it takes a `JsProtocolParams`, not JSON, because §4.4 already gives that type `fromBlockfrostJson` and `fromCardanoCliJson` statics.

`network` renders `Network.Mainnet` as `"mainnet"` and `Network.Testnet` as `"testnet"`. `Network.Other(id)` is **not** representable: the accessor's type is exactly what `custom` can construct, so there is no value a caller can read but not write. If an `Other` case is ever needed, widen both together. Annotate `network` with `@TsType("\"mainnet\" | \"testnet\"")` so the declaration is a literal union rather than `string`, the way `JStakeRegistration.credentialType` already does; if `CardanoInfo` is ever constructed with `Other`, `network` should throw rather than invent a third string.

- [ ] **Step 4: Run the test**

Same command as step 2. Expected: PASS.

- [ ] **Step 5: Commit**

```bash
scalafmtAll
git add scalus-core
git commit -m "feat: ProtocolParams and CardanoInfo handles for JavaScript"
```

---

### Task 8: `Emulator.create` and the parameter accessors

**Files:**
- Modify: `scalus-cardano-ledger/js/src/main/scala/scalus/cardano/node/JEmulator.scala`
- Test: `scalus-cardano-ledger/js/src/test/scala/scalus/cardano/node/EmulatorJsTest.scala`

**Interfaces:**
- Consumes: `JsCardanoInfo`, `JsProtocolParams`, `JsUtxo` from Tasks 6-7.
- Produces: `JEmulator.create(info)`, `create(info, options)`, `getCardanoInfo()`, `getProtocolParameters()`. `JsEmulatorOptions` is a chased `js.Object` trait exported as `"EmulatorOptions"`.

- [ ] **Step 1: Write the failing test**

```scala
    test("Emulator.create uses the network's own protocol parameters, not mainnet's") {
        val info = JsCardanoInfo.preview()
        val emulator = JEmulator.create(info)
        assert(emulator.getCardanoInfo().network == "testnet")
        assert(
          emulator.getProtocolParameters().maxTxSize ==
              info.protocolParams.maxTxSize
        )
    }
```

This is the bug the spec names in §1.3: today a `SlotConfig.preview` emulator validates with mainnet parameters.

- [ ] **Step 2: Run it to verify it fails**

```bash
nix develop .#ci --accept-flake-config --command bash -c \
  "sbt --batch -Dsbt.supershell=false -Dsbt.log.noformat=true \
   'scalusCardanoLedgerJS/testOnly scalus.cardano.node.EmulatorJsTest -- -z create'"
```

Expected: FAIL to compile.

- [ ] **Step 3: Implement it**

Add the options trait and the two factories:

```scala
/** Everything an emulator may start with beyond its chain parameters. Every field is optional. */
@TsName("EmulatorOptions")
trait JsEmulatorOptions extends js.Object {

    /** UTxOs to start from. */
    val utxos: js.UndefOr[js.Array[JsUtxo]] = js.undefined

    /** Starting slot. Defaults to the slot containing `Date.now()`. */
    val slot: js.UndefOr[Double] = js.undefined

    val stakeRegistrations: js.UndefOr[js.Array[JStakeRegistration]] = js.undefined
    val poolRegistrations: js.UndefOr[js.Array[JPoolRegistration]] = js.undefined
    val drepRegistrations: js.UndefOr[js.Array[JDRepRegistration]] = js.undefined
    val datums: js.UndefOr[js.Array[JDatumEntry]] = js.undefined
}
```

and in `object JEmulator`:

```scala
    /** An emulator for a network, with an empty ledger. */
    @JSExportStatic
    def create(info: JsCardanoInfo): JEmulator = create(info, js.Object().asInstanceOf[JsEmulatorOptions])

    /** An emulator for a network, seeded with UTxOs and registrations.
      *
      * Unlike the older constructors, protocol parameters, network id and slot config come from
      * one `CardanoInfo`, so they cannot disagree.
      */
    @JSExportStatic
    def create(info: JsCardanoInfo, options: JsEmulatorOptions): JEmulator = { ... }
```

The body builds `Context(env = UtxoEnv(slot, info.underlying.protocolParams, certState, info.underlying.network), slotConfig = info.underlying.slotConfig)` and calls `Emulator.withState`. Default the slot to `info.slotConfig.timeToSlot(System.currentTimeMillis()).floor`, matching what the Evolution SDK adapter does by hand today.

Keep the existing constructor, `withAddresses` and `withState`, and mark each `@deprecated("use Emulator.create", "1.2.0")`.

- [ ] **Step 4: Run the test**

Same command as step 2. Expected: PASS.

- [ ] **Step 5: Commit**

```bash
scalafmtAll
git add scalus-cardano-ledger
git commit -m "feat: Emulator.create from a CardanoInfo"
```

---

### Task 9: `Emulator.evaluateTx`

**Files:**
- Modify: `scalus-cardano-ledger/js/src/main/scala/scalus/cardano/node/JEmulator.scala`
- Test: `scalus-cardano-ledger/js/src/test/scala/scalus/cardano/node/EmulatorJsTest.scala`

**Interfaces:**
- Consumes: `JsUtxo.input`, `JsUtxo.output` from Task 6; `JEmulator.create` from Task 8.
- Produces: `evaluateTx(txCbor)` and `evaluateTx(txCbor, additionalUtxos)` returning `js.Array[JScalus.Redeemer]` — the class already exported as `"RedeemerBudget"`.

- [ ] **Step 1: Write the failing test**

Build on `EmulatorJsTest`'s existing
`"Emulator.withRegisteredStakeCredentials allows zero-withdrawal without registration tx"` case: it
already constructs an always-succeeds Plutus V3 script, a stake address for it, a witness and a
funded emulator. Lift that setup into a private helper in the suite and use it from both tests, so
the fixture stays known-good.

```scala
    test("evaluateTx resolves inputs against the emulator's own UTxO set") {
        val (emulator, tx) = zeroWithdrawalFixture()   // the helper lifted from that test
        val budgets = emulator.evaluateTx(toUint8Array(tx.toCbor))
        assert(budgets.length == 1, s"expected one redeemer, got ${budgets.length}")
        assert(budgets(0).tag == "Reward")
        assert(BigInt(budgets(0).budget.steps.toString) > 0)
    }
```

- [ ] **Step 2: Run it to verify it fails**

```bash
nix develop .#ci --accept-flake-config --command bash -c \
  "sbt --batch -Dsbt.supershell=false -Dsbt.log.noformat=true \
   'scalusCardanoLedgerJS/testOnly scalus.cardano.node.EmulatorJsTest -- -z evaluateTx'"
```

Expected: FAIL to compile.

- [ ] **Step 3: Implement it**

```scala
    /** Runs every Plutus script the transaction triggers and reports what each one costs,
      * resolving inputs against this emulator's UTxO set, slot config, cost models and protocol
      * version.
      *
      * The standalone `evalPlutusScripts` needs all of that passed in, and getting any of it wrong
      * produces plausible, wrong budgets. Here there is nothing to get wrong.
      */
    def evaluateTx(txCborBytes: Uint8Array): js.Array[JScalus.Redeemer] =
        evaluateTxWith(txCborBytes, Map.empty)

    /** As above, plus UTxOs the emulator does not hold — outputs of a transaction not yet
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
    }
```

Mirror `JScalus.evalPlutusScripts`'s failure handling: a `PlutusScriptEvaluationException` must surface as the exported `PlutusScriptEvaluationError`, not as a raw Scala exception. Spec §4.1 fixes the error model, so do two things here:

- Make `JSPlutusScriptEvaluationError` extend `js.Error`, so `instanceof Error`, `.stack` and unhandled-rejection output all work. Its own doc comment currently warns that it does not — delete that warning with the change.
- Add a test that a failing script throws it and that `error instanceof Error` holds in JavaScript, not just that the message matches.

- [ ] **Step 4: Run the test**

Same command as step 2. Expected: PASS.

- [ ] **Step 5: Add a parity test**

The emulator-owned path and the standalone function must agree. Add:

```scala
    test("evaluateTx agrees with evalPlutusScripts given the same parameters") {
        val fromEmulator = emulator.evaluateTx(txCbor)
        val fromStandalone = JScalus.evalPlutusScripts(
          txCbor,
          emulator.getUtxosCbor(),
          emulator.getCardanoInfo().slotConfig,
          costModelsAsArrays,
          emulator.getCardanoInfo().protocolParams.protocolMajorVersion.toInt
        )
        assert(fromEmulator.length == fromStandalone.length)
        for i <- fromEmulator.indices do
            assert(fromEmulator(i).budget.steps.toString == fromStandalone(i).budget.steps.toString)
    }
```

- [ ] **Step 6: Commit**

```bash
scalafmtAll
git add scalus-cardano-ledger
git commit -m "feat: evaluate a transaction against the emulator's own state"
```

---

### Task 10: `Emulator.getUtxos(filter)`

**Files:**
- Modify: `scalus-cardano-ledger/js/src/main/scala/scalus/cardano/node/JEmulator.scala`
- Test: `scalus-cardano-ledger/js/src/test/scala/scalus/cardano/node/EmulatorJsTest.scala`

**Interfaces:**
- Consumes: `EmulatorBase.findUtxosSync`, `UtxoSource.FromPaymentCredential` from Task 2; `JsUtxo.wrap` from Task 6.
- Produces: `getUtxos()`, `getUtxos(filter: JsUtxoFilter)` returning `js.Array[JsUtxo]`; `JsUtxoFilter` exported as `"UtxoFilter"`.

- [ ] **Step 1: Write the failing test**

```scala
    test("getUtxos filters by address, by unit and by out-ref") {
        val alice = Alice.address(Network.Mainnet)
        val emulator = JEmulator.create(JsCardanoInfo.mainnet(), optionsWith(aliceUtxos))
        assert(emulator.getUtxos().length == aliceUtxos.length)

        val byAddress = emulator.getUtxos(filter(address = alice.toBech32))
        assert(byAddress.length == aliceUtxos.length)
        assert(byAddress.toSeq.forall(_.address == alice.toBech32))

        val byLovelace = emulator.getUtxos(filter(unit = "lovelace"))
        assert(byLovelace.length == aliceUtxos.length, "lovelace matches every UTxO")

        val one = emulator.getUtxos(filter(outRefs = js.Array(outRef(byAddress(0)))))
        assert(one.length == 1)
        assert(one(0).txHash == byAddress(0).txHash)
    }
```

Write the small `filter(...)` and `outRef(...)` helpers at the top of the suite; they build `JsUtxoFilter` and `JsOutRef` literals via `js.Dynamic.literal(...).asInstanceOf[...]`.

- [ ] **Step 2: Run it to verify it fails**

```bash
nix develop .#ci --accept-flake-config --command bash -c \
  "sbt --batch -Dsbt.supershell=false -Dsbt.log.noformat=true \
   'scalusCardanoLedgerJS/testOnly scalus.cardano.node.EmulatorJsTest -- -z getUtxos'"
```

Expected: FAIL to compile.

- [ ] **Step 3: Implement it**

Add the filter traits:

```scala
/** A transaction output's address on the chain. */
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
    val outRefs: js.UndefOr[js.Array[JsOutRef]] = js.undefined
    val txHash: js.UndefOr[String] = js.undefined
    val minLovelace: js.UndefOr[js.BigInt] = js.undefined
    val limit: js.UndefOr[Double] = js.undefined
}
```

and the methods, translating the filter into the Scala algebra so the work happens there:

```scala
    /** Every UTxO in the ledger. */
    def getUtxos(): js.Array[JsUtxo] = wrapAll(emulator.utxos)

    /** The UTxOs matching `filter`. Filtering happens in the ledger, so only matches cross into
      * JavaScript.
      */
    def getUtxos(filter: JsUtxoFilter): js.Array[JsUtxo] =
        wrapAll(emulator.findUtxosSync(toQuery(filter)))

    private def wrapAll(utxos: Utxos): js.Array[JsUtxo] = {
        val out = js.Array[JsUtxo]()
        utxos.foreach { case (input, output) => out.push(JsUtxo.wrap(input, output)) }
        out
    }
```

`toQuery` maps `address` to `UtxoSource.FromAddress`, `paymentCredential` to `FromPaymentCredential`, `txHash` to `FromTransaction`, `outRefs` to `FromInputs`, a non-`"lovelace"` `unit` to `FromAsset`, then applies `MinLovelace` and `limit`. With no source given, start from every UTxO. Combine multiple sources with `&&`.

Keep `getUtxosForAddress`, `getAllUtxos` and `getUtxosCbor`; deprecate the first two.

- [ ] **Step 4: Run the test**

Same command as step 2. Expected: PASS.

- [ ] **Step 5: Commit**

```bash
scalafmtAll
git add scalus-cardano-ledger
git commit -m "feat: filtered UTxO queries returning Utxo handles"
```

---

### Task 11: `SubmitResult.errorRule` and non-optional `logs`

**Files:**
- Modify: `scalus-cardano-ledger/js/src/main/scala/scalus/cardano/node/JEmulator.scala:135-156,309-324`
- Test: `scalus-cardano-ledger/js/src/test/scala/scalus/cardano/node/EmulatorJsTest.scala`

**Interfaces:**
- Consumes: `SubmitError.rule` from Task 1.
- Produces: `JSubmitResult` gains `errorRule: js.UndefOr[String]`; `logs` becomes `js.Array[String]`, always present.

- [ ] **Step 1: Write the failing test**

```scala
    test("a rejected submission reports the rule and an empty log array") {
        val result = emulator.submitTx(unbalancedTxCbor)
        assert(!result.isSuccess)
        assert(result.errorRule.contains("ValueNotConservedUTxO"), result.errorRule.toString)
        assert(result.logs.length == 0, "logs is always an array, empty when there are none")
    }
```

- [ ] **Step 2: Run it to verify it fails**

```bash
nix develop .#ci --accept-flake-config --command bash -c \
  "sbt --batch -Dsbt.supershell=false -Dsbt.log.noformat=true \
   'scalusCardanoLedgerJS/testOnly scalus.cardano.node.EmulatorJsTest -- -z rejected'"
```

Expected: FAIL — `errorRule` is not a member.

- [ ] **Step 3: Implement it**

Update the trait:

```scala
@TsName("SubmitResult")
trait JSubmitResult extends js.Object {
    val isSuccess: Boolean

    /** Transaction hash hex; present on success. */
    val txHash: js.UndefOr[String]

    /** Error message; present on failure. */
    val error: js.UndefOr[String]

    /** The ledger rule that rejected the transaction; present on failure. Stable enough to assert
      * on in a test, unlike the message.
      */
    val errorRule: js.UndefOr[String]

    /** Script trace logs, oldest first. Always an array; empty when the script produced none. */
    val logs: js.Array[String]
}
```

and collapse `formatSubmitResult` to one shape:

```scala
    private def formatSubmitResult(result: Either[SubmitError, TransactionHash]): JSubmitResult =
        result match {
            case Right(txHash) =>
                js.Dynamic
                    .literal(isSuccess = true, txHash = txHash.toHex, logs = js.Array[String]())
                    .asInstanceOf[JSubmitResult]
            case Left(submitError) =>
                val logs = submitError match
                    case NodeSubmitError.ScriptFailure(_, l, _, _, _) => js.Array(l*)
                    case _                                            => js.Array[String]()
                js.Dynamic
                    .literal(
                      isSuccess = false,
                      error = submitError.message,
                      errorRule = submitError.rule,
                      logs = logs
                    )
                    .asInstanceOf[JSubmitResult]
        }
```

The `ScriptFailure` pattern gains the `rule` field added in Task 1, hence the extra `_`.

- [ ] **Step 4: Run the test**

Same command as step 2. Expected: PASS. Then run the npm tests, which assert on today's shape:

```bash
cd scalus-cardano-ledger/js/src/main/npm && npx vitest run
```

Update any test that treats `logs` as optional.

- [ ] **Step 5: Commit**

```bash
scalafmtAll
git add scalus-cardano-ledger
git commit -m "feat: report the rejecting rule on SubmitResult"
```

---

### Task 12: The remaining accessors

Spec §4.8. Each is small; they share one test cycle because none is independently reviewable.

**Files:**
- Modify: `scalus-cardano-ledger/js/src/main/scala/scalus/cardano/node/JEmulator.scala`
- Test: `scalus-cardano-ledger/js/src/test/scala/scalus/cardano/node/EmulatorJsTest.scala`

**Interfaces:**
- Consumes: `EmulatorBase.stakeDistribution` from Task 4; `JsUtxo` from Task 6.
- Produces: `getTime`, `setTime`, `hasTx(hex)`, `getTransactionStatus`, `getTransaction`, `getAppliedTxs`, `getDatum(hex)`, `getDelegation(bech32)`, `getStakeReward(bech32)`, `getStakeDistribution`, `addUtxo`, `removeUtxo`.
- **Not** produced: `clearAppliedTxs`. The Scala method stays; spec §4.8 says why it is not exposed to JavaScript.

- [ ] **Step 1: Write the failing tests**

```scala
    test("identifiers are hex everywhere") {
        val hash = submittedTxHashHex
        assert(emulator.hasTx(hash))
        assert(emulator.getTransactionStatus(hash) == "Confirmed")
        assert(emulator.getTransaction(hash).isDefined)
        assert(emulator.getTransactionStatus("00" * 32) == "NotFound")
    }

    test("time and slot move together") {
        val info = emulator.getCardanoInfo()
        emulator.setSlot(1000)
        assert(emulator.getTime() == info.slotConfig.slotToTime(1000))
        emulator.setTime(info.slotConfig.slotToTime(2000))
        assert(emulator.getSlot() == 2000.0)
    }

    test("addUtxo and removeUtxo edit the ledger") {
        val utxo = new JsUtxo("11" * 32, 0, aliceBech32, JsValue.ada(js.BigInt("5")))
        val before = emulator.getUtxos().length
        emulator.addUtxo(utxo)
        assert(emulator.getUtxos().length == before + 1)
        emulator.removeUtxo(outRefOf(utxo))
        assert(emulator.getUtxos().length == before)
    }

    test("getDelegation and getStakeReward take a reward address") {
        val info = emulator.getDelegation(stakeAddressBech32)
        assert(info.rewards.toString == "1000000")
        assert(emulator.getStakeReward(stakeAddressBech32).contains(js.BigInt("1000000")))
    }

    test("getStakeDistribution reports stake and rewards per credential") {
        val entry = emulator.getStakeDistribution().toSeq
            .find(_.credential == stakeCredentialHex)
            .getOrElse(fail("credential missing"))
        assert(BigInt(entry.rewards.toString) == 1_000_000)
    }
```

- [ ] **Step 2: Run them to verify they fail**

```bash
nix develop .#ci --accept-flake-config --command bash -c \
  "sbt --batch -Dsbt.supershell=false -Dsbt.log.noformat=true \
   'scalusCardanoLedgerJS/testOnly scalus.cardano.node.EmulatorJsTest'"
```

Expected: FAIL to compile.

- [ ] **Step 3: Implement them**

Follow the signatures in spec §4.8. Three rules apply throughout:

- Identifiers are hex strings. `hasTx` and `getDatum` currently take raw `Uint8Array`; change them and deprecate nothing — nobody is on 1.x.
- Absence is `js.UndefOr`, never `null`. `getStakeReward` returns `js.UndefOr[js.BigInt]`, `getDatum` returns `js.UndefOr[Uint8Array]`, `JDelegationInfo.poolId` becomes `js.UndefOr[String]` (hex, not bytes).
- `getDelegation` and `getStakeReward` take a bech32 reward address and derive the credential with `RewardAccount`/`StakeAddress`, replacing the hand-built credential CBOR the Evolution SDK adapter writes today.

`getStakeDistribution` returns `js.Array[JsStakeDistributionEntry]`, a chased trait with `credential: String` (hex), `pool: js.UndefOr[String]` (hex), `stake: js.BigInt`, `rewards: js.BigInt`.

- [ ] **Step 4: Run the tests**

Same command as step 2. Expected: PASS.

- [ ] **Step 5: Commit**

```bash
scalafmtAll
git add scalus-cardano-ledger
git commit -m "feat: hex identifiers, time control, ledger edits and stake distribution"
```

---

### Task 13: Regenerate the declarations and prove it through both SDKs

**Files:**
- Modify: `scalus-cardano-ledger/js/src/main/npm/scalus.d.ts` (generated — never hand-edited)
- Create: `scalus-cardano-ledger/js/src/main/npm/__tests__/provider-mesh.test.ts`
- Create: `scalus-cardano-ledger/js/src/main/npm/__tests__/provider-lucid.test.ts`
- Create: `scalus-cardano-ledger/js/src/main/npm/__tests__/collision.test.ts`
- Modify: `scalus-cardano-ledger/js/src/main/npm/package.json`
- Modify: `scalus-site/content/testing/js-emulator.mdx`

**Interfaces:**
- Consumes: everything from Tasks 8-12.

- [ ] **Step 1: Regenerate and read the declarations**

```bash
nix develop .#ci --accept-flake-config --command bash -c \
  "sbt --batch -Dsbt.supershell=false -Dsbt.log.noformat=true \
   'scalusCardanoLedgerJS/generateDts'"
```

Then read `scalus.d.ts` end to end. It is the API a stranger sees. Check: no `J` prefix survived; no `| null`; every quantity is `bigint` and every slot is `number`; **every handle declares its public constructor** (if one is missing, Task 5's generator change regressed); `toObject()` is present on each handle and its `Plain*` interface is declared; and the doc comment on each public member says what it is for, not what it is called.

- [ ] **Step 2: Write the mesh integration test**

```ts
// __tests__/provider-mesh.test.ts
import { describe, test, expect } from "vitest";
import { Emulator, CardanoInfo, Utxo, Value } from "../scalus";

describe("Emulator as a MeshJS provider backend", () => {
  test("serves protocol parameters, UTxOs and evaluation without hand-rolled codecs", () => {
    const aliceAddress =
      "addr_test1vzpwq95z3xyum8vqndgdd9mdnmafh3djcxnc6jemlgdmswcve6tkw";
    const info = CardanoInfo.preview();
    const emulator = Emulator.create(info, {
      utxos: [new Utxo("00".repeat(32), 0, aliceAddress, Value.ada(1000n))],
    });

    // What an IFetcher needs, with no CBOR in sight.
    const params = emulator.getProtocolParameters();
    expect(params.txFeePerByte).toBeGreaterThan(0);
    expect(params.costModels.PlutusV3.length).toBeGreaterThan(0);

    const utxos = emulator.getUtxos({ address: aliceAddress });
    expect(utxos.length).toBe(1);
    expect(utxos[0].value.coin).toBe(1_000_000_000n);

    // The Blockfrost shape mesh's castProtocol already consumes.
    const json = JSON.parse(params.toBlockfrostJson());
    expect(json.min_fee_a).toBe(params.txFeePerByte);
  });
});
```

Then extend it to build and submit a real transaction with `MeshTxBuilder`, using the emulator as fetcher, submitter and evaluator — that is the claim the PR to MeshJS makes, so it must be tested here first.

- [ ] **Step 3: Write the lucid integration test**

Add `@lucid-evolution/lucid` as a devDependency and write the equivalent: build a transaction with lucid against the emulator's UTxOs and parameters, sign it with a lucid wallet, submit it to the emulator, and assert the resulting UTxO set.

- [ ] **Step 4: Write the collision test**

```ts
// __tests__/collision.test.ts
// Both packages export `UTxO`/`Utxo`, `Value`, `Asset`. Importing both must stay ergonomic.
import * as Scalus from "../scalus";
import { MeshTxBuilder } from "@meshsdk/core";

test("scalus and @meshsdk/core coexist in one module", () => {
  const v = Scalus.Value.ada(1n);
  expect(v.coin).toBe(1_000_000n);
  expect(typeof MeshTxBuilder).toBe("function");
});
```

- [ ] **Step 5: Run everything**

```bash
nix develop .#ci --accept-flake-config --command bash -c \
  "sbt --batch -Dsbt.supershell=false -Dsbt.log.noformat=true \
   'scalusCardanoLedgerJS/runNpmTests'"
```

Expected: PASS.

**`bundle-size.test.ts` does not measure what you just built.** It reads the *committed*
`scalus.js`, and `ci-js` never regenerates it — `JS_BUNDLE_SIZE.md` says the same thing: tightening
the number against a stale artifact proves nothing. The committed file currently predates the
2026-08-30 size work. So, in this task:

1. Measure the real number yourself:
   `ls -l scalus-cardano-ledger/js/src/main/npm/scalus.js` **after** `prepareNpmPackage` has run
   (`runNpmTests` depends on it), and compare against the 2,591,052 B baseline. If §4 added more
   than ~100 KB, find which task added the weight and say so before continuing.
2. Commit the regenerated `scalus.js`, so the committed artifact matches the code again.
3. Change `bundle-size.test.ts` to read the linker output that `prepareNpmPackage` just wrote,
   rather than the committed file, and only then tighten the limit to 2.75 MB. Measuring the build
   under test is the whole point of the guard.

- [ ] **Step 6: Update the documentation page**

Rewrite `scalus-site/content/testing/js-emulator.mdx` against the new API: `Emulator.create`, `getUtxos(filter)` returning objects, `evaluateTx`, `getProtocolParameters`. Delete the `cbor-x` decoding section — it exists only because the old API handed back CBOR.

- [ ] **Step 7: Commit**

```bash
scalafmtAll
git add scalus-cardano-ledger scalus-site
git commit -m "test: drive the emulator through MeshJS and lucid, and regenerate scalus.d.ts"
```

---

### Task 14: Unify the JVM and JS emulator implementations

`Emulator.scala` exists twice, near-identical. The claim that the JS emulator runs the same rules as the JVM one is currently maintained by hand. Do this last: it touches both platforms and is the largest MiMa surface.

**Files:**
- Modify: `scalus-cardano-ledger/shared/src/main/scala/scalus/cardano/node/EmulatorBase.scala`
- Modify: `scalus-cardano-ledger/jvm/src/main/scala/scalus/cardano/node/Emulator.scala`
- Modify: `scalus-cardano-ledger/js/src/main/scala/scalus/cardano/node/Emulator.scala`
- Test: `scalus-cardano-ledger/shared/src/test/scala/scalus/cardano/node/EmulatorParityTest.scala` (create)

**Interfaces:**
- Produces: a `protected` state cell in `EmulatorBase` that each platform implements — JVM with an atomic reference, JS with a plain `var`.

- [ ] **Step 1: Write the parity test**

Create a shared test that both platforms run, submitting the same sequence of transactions and asserting the same UTxO set, cert state, datum store and applied-tx log. Put it in `shared/src/test` so it compiles for JVM, JS and Native.

- [ ] **Step 2: Run it on both platforms**

```bash
nix develop .#ci --accept-flake-config --command bash -c \
  "sbt --batch -Dsbt.supershell=false -Dsbt.log.noformat=true \
   'scalusCardanoLedgerJVM/testOnly scalus.cardano.node.EmulatorParityTest'"
```

then the same for `scalusCardanoLedgerJS`. Expected: PASS on both — this test documents current behaviour before the refactor, so a failure here means the platforms have *already* drifted, which is the finding the task exists to prevent.

- [ ] **Step 3: Move the state machine into `EmulatorBase`**

Lift `submitSync`, `setSlot`, `clearAppliedTxs`, `recordApplied` and `snapshot` into `EmulatorBase`, expressed against an abstract state cell:

```scala
    /** The mutable cell holding this emulator's state. JVM guards it for concurrent submission;
      * JavaScript is single-threaded and uses a plain `var`.
      */
    protected def updateState(f: EmulatorState => EmulatorState): Unit
    protected def readState: EmulatorState
```

Each platform `Emulator` shrinks to the cell plus its constructor.

- [ ] **Step 4: Run the parity test and the full suites**

Both platforms, plus `scalusCardanoLedgerJVM/test` and `scalusCardanoLedgerJS/test`. Expected: PASS.

- [ ] **Step 5: Add MiMa filters and commit**

```bash
nix develop .#ci --accept-flake-config --command bash -c \
  "sbt --batch -Dsbt.supershell=false -Dsbt.log.noformat=true 'mima'"
scalafmtAll
git add scalus-cardano-ledger build.sbt
git commit -m "refactor: one emulator state machine for JVM and JavaScript"
```

---

## Self-Review Notes

- **Spec coverage:** §4 preamble (generator change) → Task 5 steps 0a-0e. §4.1 conventions → Global Constraints; §4.1 error model → Global Constraints, Task 9. §4.2 → Tasks 5, 6. §4.3 → Tasks 7, 8. §4.4 → Tasks 3, 7. §4.5 → Task 9. §4.6 → Tasks 2, 10. §4.7 → Task 11. §4.8 → Task 12. §5.1 → Task 14. §5.2 → Task 8. §5.3 → Task 2. §5.4 → Task 1. §5.5 → Task 3. §5.6 → Task 2. §5.7 → Task 4. §5.9 → Tasks 1, 14 and the Global Constraints. §5.10 → Task 5 steps 0a-0e. §5.11 → Task 9. §7.2 → Task 13.
- **Not covered here, by design:** §6 (everything deferred to M2) and §7 items 2-4 (upstream PRs, which live in other repos and follow a release). §7.1 is the separate `2026-08-30-js-conformance-evidence.md` plan.
- **Type consistency:** `JsValue.wrap`/`.underlying` (Task 5) are used by Tasks 6, 8, 9. `JsUtxo.wrap`/`.input`/`.output` (Task 6) are used by Tasks 9, 10, 12. `SubmitError.rule` (Task 1) is used by Task 11. `findUtxosSync` (Task 2) is used by Task 10. `stakeDistribution` (Task 4) is used by Task 12. `JsCardanoInfo` (Task 7) is used by Tasks 8, 9.
- **Fixtures named, not stubbed.** Tasks 1, 4, 9 and 12 build on tests that already exist — the double-spend case at `EmulatorTest.scala:278`, and `EmulatorJsTest`'s zero-withdrawal case. Each says which one and what to lift out of it. Where a constructor name might differ from what this plan assumes (`ShelleyDelegationPart`, `Address`'s payment-credential accessor), the step says so and says which lines to adapt — the assertions do not depend on it.
- **One assertion is deliberately provisional.** Task 1 pins `rule == "BadAllInputsUTxO"` and says in the same breath that if the run disagrees, the run wins. The behaviour under test is that a rejection carries a stable name, not which validator fires first.
