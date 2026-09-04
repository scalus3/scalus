package scalus.cardano.node

import io.bullet.borer.Cbor
import org.scalatest.funsuite.AnyFunSuite
import scalus.uplc.builtin.{ByteString, Data}
import scalus.cardano.address.{Network, ShelleyAddress, ShelleyDelegationPart, ShelleyPaymentPart, StakeAddress, StakePayload}
import scalus.cardano.ledger.*
import scalus.cardano.txbuilder.{ScriptSource, TwoArgumentPlutusScriptWitness, TxBuilder}
import scalus.compiler.Options
import scalus.testing.kit.Party.{Alice, Bob}
import scalus.uplc.PlutusV3
import scalus.uplc.eval.JScalus

import scala.scalajs.js
import scala.scalajs.js.typedarray.{byteArray2Int8Array, Uint8Array}

class EmulatorJsTest extends AnyFunSuite {

    given testEnv: CardanoInfo = CardanoInfo.mainnet
    val genesisHash: TransactionHash =
        TransactionHash.fromByteString(ByteString.fromHex("0" * 64))

    private def toUint8Array(bytes: Array[Byte]): Uint8Array =
        new Uint8Array(byteArray2Int8Array(bytes).buffer)

    /** ScalaTest's `assert` macro crashes the Scala.js backend when it decomposes a raw
      * `js.UndefOr` member chain (confirmed twice already - see `JsUtxoTest.scala` and the
      * `errorRule` check below), so every `js.UndefOr` assertion in this file converts to `Option`
      * first, outside the macro's view.
      */
    private def optionOf[A](u: js.UndefOr[A]): Option[A] = u.toOption

    /** Builds a `JsUtxoFilter` literal; every argument left out stays `js.undefined`, matching what
      * a caller who only sets one or two fields would produce.
      */
    private def filter(
        address: js.UndefOr[String] = js.undefined,
        paymentCredential: js.UndefOr[String] = js.undefined,
        unit: js.UndefOr[String] = js.undefined,
        outRefs: js.UndefOr[js.Array[JsOutRef]] = js.undefined,
        txHash: js.UndefOr[String] = js.undefined,
        minLovelace: js.UndefOr[js.BigInt] = js.undefined,
        limit: js.UndefOr[Double] = js.undefined
    ): JsUtxoFilter = js.Dynamic
        .literal(
          address = address,
          paymentCredential = paymentCredential,
          unit = unit,
          outRefs = outRefs,
          txHash = txHash,
          minLovelace = minLovelace,
          limit = limit
        )
        .asInstanceOf[JsUtxoFilter]

    private def outRef(u: JsUtxo): JsOutRef =
        js.Dynamic
            .literal(txHash = u.txHash, outputIndex = u.outputIndex)
            .asInstanceOf[JsOutRef]

    private def optionsWith(utxos: Seq[(Input, Output)]): JsEmulatorOptions =
        js.Dynamic
            .literal(utxos = js.Array(utxos.map { case (i, o) => JsUtxo.wrap(i, o) }*))
            .asInstanceOf[JsEmulatorOptions]

    /** A `JEmulator` seeded with one funded UTxO and the always-succeeds script's reward address,
      * pre-registered with a zero balance so a zero-withdrawal transaction can trigger it without a
      * registration transaction first, plus a transaction that does exactly that.
      *
      * `JEmulator.create` + `stakeRegistrations` here is exactly the pre-registered stake
      * credential `Emulator.withRegisteredStakeCredentials` builds - same deposit, same reward,
      * same `Context.testMainnet()`-equivalent slot/env - reached through the public JS API instead
      * of the Scala-only one, so the fixture can also hand back a `JEmulator` for `evaluateTx` to
      * run against.
      */
    private def zeroWithdrawalFixture(): (JEmulator, Transaction) = {
        val alwaysOkScript = PlutusV3.alwaysOk.script
        val scriptHash = alwaysOkScript.scriptHash
        val stakeAddress = StakeAddress(Network.Mainnet, StakePayload.Script(scriptHash))
        val witness = TwoArgumentPlutusScriptWitness(
          ScriptSource.PlutusScriptValue(alwaysOkScript),
          Data.unit
        )
        val alice = Alice.address(Network.Mainnet)
        val initialUtxos = Map(
          Input(genesisHash, 0) -> Output(alice, Value.ada(5000))
        )
        val stakeRegistration = js.Dynamic
            .literal(
              credentialType = "script",
              credentialHash = scriptHash.toHex,
              rewards = js.BigInt(0)
            )
            .asInstanceOf[JStakeRegistration]
        val options = js.Dynamic
            .literal(
              utxos = js.Array(initialUtxos.toSeq.map { case (i, o) => JsUtxo.wrap(i, o) }*),
              // Fixed, rather than the create() default of "now": keeps the fixture deterministic,
              // and matches the slot the old Emulator.withRegisteredStakeCredentials(...) default
              // (Context.testMainnet(), slot 0) used before this fixture was lifted out of it.
              slot = 0.0,
              stakeRegistrations = js.Array(stakeRegistration)
            )
            .asInstanceOf[JsEmulatorOptions]
        val emulator = JEmulator.create(JsCardanoInfo.mainnet(), options)
        val tx = TxBuilder(testEnv)
            .withdrawRewards(stakeAddress, Coin.zero, witness)
            .complete(initialUtxos, alice)
            .sign(Alice.signer)
            .transaction
        (emulator, tx)
    }

    test("Emulator.withRegisteredStakeCredentials allows zero-withdrawal without registration tx") {
        val (emulator, tx) = zeroWithdrawalFixture()
        val result = emulator.submitTx(toUint8Array(tx.toCbor))
        assert(
          result.isSuccess,
          s"Zero-withdrawal should succeed with pre-registered credential: ${result.error}"
        )
    }

    test("a rejected submission reports the rule and an empty log array") {
        val alice = Alice.address(Network.Mainnet)
        val bob = Bob.address(Network.Mainnet)
        val input = Input(genesisHash, 0)

        // Built and signed against a UTxO set where this input is worth far more than what the
        // emulator will actually hold at submission time - so ValueNotConservedUTxOValidator is
        // what rejects it, not input presence (same input key, just a different resolved value)
        // or a bad signature (nothing about the signed body changes).
        val fundedUtxos = Map(input -> Output(alice, Value.ada(5000)))
        val tx = TxBuilder(testEnv)
            .payTo(bob, Value.ada(10))
            .complete(fundedUtxos, alice)
            .sign(Alice.signer)
            .transaction

        val underfundedUtxos = Seq(input -> Output(alice, Value.ada(1)))
        val emulator = JEmulator.create(JsCardanoInfo.mainnet(), optionsWith(underfundedUtxos))

        val result = emulator.submitTx(toUint8Array(tx.toCbor))
        assert(!result.isSuccess)
        // Routed through .toOption in a val first, not `result.errorRule.contains(...)` inline in
        // the assert: ScalaTest's assert macro decomposing a raw js.UndefOr member chain crashes
        // the Scala.js backend at compile time (see optionOf in JsUtxoTest.scala for the same
        // workaround), so the UndefOr -> Option conversion has to happen outside the macro's view.
        val errorRule = result.errorRule.toOption
        assert(errorRule.contains("ValueNotConserved"), errorRule.toString)
        assert(result.logs.length == 0, "logs is always an array, empty when there are none")
    }

    test("submitTx answers with a result, not an exception, for bytes that are not a transaction") {
        val emulator = JEmulator.create(JsCardanoInfo.mainnet())
        val result = emulator.submitTx(toUint8Array(Array[Byte](0)))
        assert(!result.isSuccess, "undecodable bytes are a rejection, not an acceptance")
        // See the note above about js.UndefOr inside ScalaTest's assert macro.
        val errorRule = result.errorRule.toOption
        assert(errorRule.contains("InvalidTransaction"), errorRule.toString)
        val error = result.error.toOption.getOrElse("")
        assert(error.contains("not a transaction"), error)
        assert(result.logs.length == 0)
    }

    test("evaluateTx throws for bytes that are not a transaction, as its doc says") {
        val emulator = JEmulator.create(JsCardanoInfo.mainnet())
        intercept[Throwable] { emulator.evaluateTx(toUint8Array(Array[Byte](0))) }
    }

    test("evaluateTx resolves inputs against the emulator's own UTxO set") {
        val (emulator, tx) = zeroWithdrawalFixture()
        val budgets = emulator.evaluateTx(toUint8Array(tx.toCbor))
        assert(budgets.length == 1, s"expected one redeemer, got ${budgets.length}")
        assert(budgets(0).tag == "Reward")
        assert(BigInt(budgets(0).budget.steps.toString) > 0)
    }

    test("evaluateTx agrees with evalPlutusScripts given the same parameters") {
        val (emulator, tx) = zeroWithdrawalFixture()
        val txCbor = toUint8Array(tx.toCbor)
        val info = emulator.getCardanoInfo()
        val cm = info.protocolParams.costModels
        // Built explicitly by language, never by iterating a Map's `.values` - that relies on
        // key-insertion/hash order lining up with V1/V2/V3, which is exactly the "cost-model
        // reshuffle" this method exists to make unnecessary (see JEmulator.evaluateTx's doc).
        val costModelsAsArrays = js.Array(cm.PlutusV1, cm.PlutusV2, cm.PlutusV3)

        val fromEmulator = emulator.evaluateTx(txCbor)
        val fromStandalone = JScalus.evalPlutusScripts(
          txCbor,
          emulator.getUtxosCbor(),
          info.slotConfig,
          costModelsAsArrays,
          info.protocolParams.protocolMajorVersion.toInt
        )

        assert(fromEmulator.length == fromStandalone.length)
        assert(fromEmulator.length == 1, "fixture is expected to produce exactly one redeemer")
        for i <- fromEmulator.indices do
            // Compare the actual budget, not merely the count: a discriminating check has to be
            // able to fail. Steps and memory are read from a live PlutusScriptEvaluator run, not a
            // literal, so a real divergence between the two call paths would show up here.
            assert(
              fromEmulator(i).budget.steps.toString == fromStandalone(i).budget.steps.toString,
              s"steps differ at $i: ${fromEmulator(i).budget.steps} vs ${fromStandalone(i).budget.steps}"
            )
            assert(
              fromEmulator(i).budget.memory.toString == fromStandalone(i).budget.memory.toString,
              s"memory differs at $i: ${fromEmulator(i).budget.memory} vs ${fromStandalone(i).budget.memory}"
            )
            assert(BigInt(fromEmulator(i).budget.steps.toString) > 0)
    }

    test("evaluateTx throws a real PlutusScriptEvaluationError for a failing script") {
        given Options = Options.default
        val failingScript =
            PlutusV3.compile((_: Data) => throw new Exception("always fails")).script
        val stakeAddress =
            StakeAddress(Network.Mainnet, StakePayload.Script(failingScript.scriptHash))
        val witness = TwoArgumentPlutusScriptWitness(
          ScriptSource.PlutusScriptValue(failingScript),
          Data.unit
        )
        val alice = Alice.address(Network.Mainnet)
        val paymentInput = Input(genesisHash, 0)
        val paymentOutput = Output(alice, Value.ada(5000))

        // `.draft`, not `.complete`: completing a transaction evaluates its scripts to price them
        // for the fee, so a script that always throws would blow up right here, before
        // `evaluateTx` ever saw it. `.draft` is exactly TxBuilder's escape hatch for this - "Testing:
        // Creating transactions for ScriptContext derivation" - it assembles the input, the
        // withdrawal, its redeemer and its script witness with no fee/balance/evaluation pass in
        // between. (A real Cardano tx body needs at least one input regardless of balance, hence
        // `.spend` here even though nothing checks the resulting transaction's balance.)
        val tx = TxBuilder(testEnv)
            .spend(Map(paymentInput -> paymentOutput))
            .withdrawRewards(stakeAddress, Coin.zero, witness)
            .draft
        val emulator = JEmulator.create(JsCardanoInfo.mainnet())

        // Also exercises the two-argument overload: the emulator's own ledger is empty, so the
        // spent payment UTxO is resolved only because it is passed as `additionalUtxos`.
        val caught = intercept[js.JavaScriptException] {
            emulator.evaluateTx(
              toUint8Array(tx.toCbor),
              js.Array(JsUtxo.wrap(paymentInput, paymentOutput))
            )
        }
        caught.exception match
            case err: JScalus.JSPlutusScriptEvaluationError =>
                // The point of extending js.Error: a raw JS `instanceof Error` check has to pass,
                // not merely a Scala-side pattern match on the error's declared type. Widened to
                // `Any` first so the check is a live runtime `instanceof`, not something the
                // compiler could fold to `true` from `err`'s already-`js.Error` static type.
                assert(
                  (err: Any).isInstanceOf[js.Error],
                  "PlutusScriptEvaluationError must be a real JS Error (instanceof Error)"
                )
                assert(err.message.contains("always fails"))
                assert(err.logs.nonEmpty)
            case other =>
                fail(s"expected JSPlutusScriptEvaluationError, got: $other")
    }

    test("Emulator.create uses the network's own protocol parameters, not mainnet's") {
        val info = JsCardanoInfo.preview()
        val emulator = JEmulator.create(info)
        assert(emulator.getCardanoInfo().network == "testnet")
        // maxTxSize alone would not catch a regression to UtxoEnv.default: mainnet and preview
        // happen to share that value. Compare the whole wrapped ProtocolParams instead, and
        // require it to differ from mainnet's - the actual bug being fixed - not just equal
        // preview's.
        val params = JsProtocolParams.underlying(emulator.getProtocolParameters())
        assert(params == CardanoInfo.preview.protocolParams)
        assert(params != CardanoInfo.mainnet.protocolParams)
    }

    test("Emulator.create(info, options) seeds UTxOs and applies the given slot") {
        val info = JsCardanoInfo.preview()
        val aliceBech32 = Alice
            .address(Network.Testnet)
            .encode
            .getOrElse(fail("test address must encode to bech32"))
        val seededValue = JsValue.ada(js.BigInt(5000))
        val seededUtxo = new JsUtxo(genesisHash.toHex, 0.0, aliceBech32, seededValue)

        // Preview's zeroTime is late 2022 with one-second slots, so `timeToSlot(Date.now())` is
        // currently under 200 million. 999,999,999 is far enough above that this cannot pass by
        // coincidence with the Date.now()-derived default.
        val explicitSlot = 999_999_999.0

        val options = js.Dynamic
            .literal(utxos = js.Array(seededUtxo), slot = explicitSlot)
            .asInstanceOf[JsEmulatorOptions]
        val emulator = JEmulator.create(info, options)

        assert(emulator.getSlot() == explicitSlot)

        val allUtxos = emulator.getUtxos()
        assert(allUtxos.length == 1)
        val seeded = allUtxos(0)
        assert(seeded.address == aliceBech32)
        assert(seeded.value.coin.toString == seededValue.coin.toString)
    }

    test("Emulator.withRegisteredStakeCredentials pre-populates certState correctly") {
        val alwaysOkScript = PlutusV3.alwaysOk
        val scriptHash = alwaysOkScript.script.scriptHash
        val stakeCred = Credential.ScriptHash(scriptHash)
        val alice = Alice.address(Network.Mainnet)
        val initialUtxos = Map(
          Input(genesisHash, 0) -> Output(alice, Value.ada(5000))
        )
        val initialReward = Coin.ada(42L)
        val emulator = Emulator.withRegisteredStakeCredentials(
          initialUtxos = initialUtxos,
          initialStakeRewards = Map(stakeCred -> initialReward)
        )
        val cs = emulator.certState
        assert(
          cs.dstate.rewards.get(stakeCred).contains(initialReward),
          s"rewards should contain stake credential with expected amount: ${cs.dstate.rewards}"
        )
        val expectedDeposit = Coin(testEnv.protocolParams.stakeAddressDeposit)
        assert(
          cs.dstate.deposits.get(stakeCred).contains(expectedDeposit),
          s"deposits should contain stake credential with protocol deposit: ${cs.dstate.deposits}"
        )
    }

    test("getUtxos filters by address, by unit and by out-ref") {
        val alice = Alice.address(Network.Mainnet)
        val aliceBech32 = alice.encode.getOrElse(fail("test address must encode to bech32"))
        val bob = Bob.address(Network.Mainnet)
        val bobBech32 = bob.encode.getOrElse(fail("test address must encode to bech32"))

        val policyId = ScriptHash.fromHex("11" * 28)
        val goldAsset = AssetName.fromString("Gold")
        val unit = policyId.toHex + goldAsset.bytes.toHex

        // Two UTxOs at Alice's address - one plain-ada, one carrying the asset under test - plus
        // one at Bob's address that every assertion below must exclude. If a filter were ignored,
        // Bob's UTxO (or the wrong Alice UTxO) would leak into the result and the length checks
        // would catch it.
        val aliceUtxo0 = Input(genesisHash, 0) -> Output(alice, Value.ada(10))
        val aliceUtxo1 = Input(genesisHash, 1) ->
            Output(alice, Value(Coin.ada(2), MultiAsset.asset(policyId, goldAsset, 1L)))
        val bobUtxo = Input(genesisHash, 2) -> Output(bob, Value.ada(10))

        val aliceUtxos = Seq(aliceUtxo0, aliceUtxo1)
        val allSeeded = aliceUtxos :+ bobUtxo
        val emulator = JEmulator.create(JsCardanoInfo.mainnet(), optionsWith(allSeeded))

        assert(emulator.getUtxos().length == allSeeded.length)

        // An empty filter object exercises the "no source given" branch of the translation
        // (getUtxos() with no argument never runs it - it wraps emulator.utxos directly).
        val empty = emulator.getUtxos(filter())
        assert(empty.length == allSeeded.length, "an empty filter matches everything")

        val byAddress = emulator.getUtxos(filter(address = aliceBech32))
        assert(byAddress.length == aliceUtxos.length)
        assert(byAddress.toSeq.forall(_.address == aliceBech32))

        val byLovelace = emulator.getUtxos(filter(unit = "lovelace"))
        assert(
          byLovelace.length == allSeeded.length,
          "lovelace matches every UTxO, including Bob's"
        )

        val byUnit = emulator.getUtxos(filter(unit = unit))
        assert(byUnit.length == 1)
        // Not txHash: every seeded UTxO here shares genesisHash, so only outputIndex tells
        // aliceUtxo1 (the only one carrying this asset) apart from the other two.
        assert(
          byUnit(0).outputIndex == 1.0,
          "must be aliceUtxo1, the only UTxO carrying this asset"
        )

        // Two source-mapped fields together must intersect, not just pick one of the two: Bob's
        // address matches nothing that also carries this asset, so the combination must be empty
        // even though each field alone matches something (Bob's plain UTxO; Alice's asset UTxO).
        val addressAndUnit = emulator.getUtxos(filter(address = bobBech32, unit = unit))
        assert(
          addressAndUnit.length == 0,
          "address and unit must AND together - Bob never holds this asset"
        )

        val one = emulator.getUtxos(filter(outRefs = js.Array(outRef(byAddress(0)))))
        assert(one.length == 1)
        assert(one(0).txHash == byAddress(0).txHash)
        assert(one(0).outputIndex == byAddress(0).outputIndex)

        // outRefs is a disjunction over its own array, not conjoined with anything else: refs
        // spanning two different addresses must both come back.
        val byBob = emulator.getUtxos(filter(address = bobBech32))
        assert(byBob.length == 1)
        val union = emulator.getUtxos(
          filter(outRefs = js.Array(outRef(byAddress(0)), outRef(byBob(0))))
        )
        assert(union.length == 2, "outRefs must match any of the given refs, across addresses")
    }

    test(
      "getUtxos filters by payment credential (key or script), by tx hash, and ANDs min lovelace with limit"
    ) {
        // Same payment key, two different stake parts: proves paymentCredential looks only at the
        // payment part, unlike `address` which would tell these two apart.
        val aliceKeyHash = Alice.addrKeyHash
        val aliceNoStake =
            ShelleyAddress(
              Network.Mainnet,
              ShelleyPaymentPart.Key(aliceKeyHash),
              ShelleyDelegationPart.Null
            )
        val bobStakeKeyHash = StakeKeyHash.fromByteString(Bob.addrKeyHash)
        val aliceWithStake = ShelleyAddress(
          Network.Mainnet,
          ShelleyPaymentPart.Key(aliceKeyHash),
          ShelleyDelegationPart.Key(bobStakeKeyHash)
        )

        // A script-hash payment credential, seeded so a translation that only ever builds
        // Credential.KeyHash (or only ever Credential.ScriptHash) from the bare hex field fails
        // one of the two assertions below.
        val scriptHash = ScriptHash.fromHex("33" * 28)
        val scriptAddress =
            ShelleyAddress(
              Network.Mainnet,
              ShelleyPaymentPart.Script(scriptHash),
              ShelleyDelegationPart.Null
            )

        val txA = TransactionHash.fromByteString(ByteString.fromHex("11" * 32))
        val txB = TransactionHash.fromByteString(ByteString.fromHex("22" * 32))

        val u1 = Input(txA, 0) -> Output(aliceNoStake, Value.ada(3))
        val u2 = Input(txA, 1) -> Output(aliceWithStake, Value.ada(100))
        val u3 = Input(txB, 0) -> Output(scriptAddress, Value.ada(20))
        val u4 = Input(txB, 1) -> Output(Bob.address(Network.Mainnet), Value.ada(50))

        val emulator = JEmulator.create(JsCardanoInfo.mainnet(), optionsWith(Seq(u1, u2, u3, u4)))

        val byKeyCredential = emulator.getUtxos(filter(paymentCredential = aliceKeyHash.toHex))
        assert(byKeyCredential.length == 2, "must match both alice utxos regardless of stake part")

        val byScriptCredential = emulator.getUtxos(filter(paymentCredential = scriptHash.toHex))
        assert(byScriptCredential.length == 1)
        assert(byScriptCredential(0).txHash == txB.toHex)
        assert(byScriptCredential(0).outputIndex == 0.0)

        val byTxHash = emulator.getUtxos(filter(txHash = txA.toHex))
        assert(byTxHash.length == 2)
        assert(byTxHash.toSeq.forall(_.txHash == txA.toHex))

        // minLovelace is ANDed with the credential source: u1 (3 ada) is below the threshold and
        // must drop out, leaving only u2 (100 ada). Dropping either filter would leave 2 results.
        val byCredentialAndMin = emulator.getUtxos(
          filter(paymentCredential = aliceKeyHash.toHex, minLovelace = js.BigInt(50_000_000L))
        )
        assert(byCredentialAndMin.length == 1)
        assert(byCredentialAndMin(0).outputIndex == 1.0)

        // limit applies after filtering: txA alone has two matches, so a limit of 1 must cut it
        // down to exactly one, not zero and not two.
        val limited = emulator.getUtxos(filter(txHash = txA.toHex, limit = 1.0))
        assert(limited.length == 1)
    }

    test(
      "identifiers are hex everywhere: hasTx, getTransactionStatus, getTransaction, getAppliedTxs"
    ) {
        val (emulator, tx) = zeroWithdrawalFixture()
        val result = emulator.submitTx(toUint8Array(tx.toCbor))
        assert(result.isSuccess, s"submission must succeed: ${result.error}")
        val hash = tx.id.toHex

        assert(emulator.hasTx(hash))
        assert(emulator.getTransactionStatus(hash) == "Confirmed")
        assert(!emulator.hasTx("00" * 32))
        assert(emulator.getTransactionStatus("00" * 32) == "NotFound")

        // Not merely "isDefined": the returned CBOR must decode back to the exact submitted
        // transaction, so a stub that returns some other transaction's bytes would be caught.
        val fetched = optionOf(emulator.getTransaction(hash))
            .getOrElse(fail("submitted transaction must be found by hash"))
        assert(Transaction.fromCbor(fetched.toArray.map(_.toByte)) == tx)
        assert(optionOf(emulator.getTransaction("00" * 32)).isEmpty)

        // Checks both fields, not just length: a getAppliedTxs that dropped the slot or returned
        // the wrong hash would still pass a bare length check.
        val applied = emulator.getAppliedTxs()
        assert(applied.length == 1)
        assert(applied(0).txHash == hash)
        assert(applied(0).slot == 0.0, "zeroWithdrawalFixture pins slot 0")
    }

    test("getDatum looks datums up by hex hash, seeded or absent") {
        val datumHashHex = "ab" * 32
        // Encoded via the real Data CBOR encoder, not a hand-written literal - the assertion below
        // checks the returned bytes against this same value, so it must come from an independent
        // encoding of the datum, not be copied from what getDatum happens to return.
        val datumCborHex = ByteString.fromArray(Cbor.encode(Data.I(42): Data).toByteArray).toHex
        val datumEntry = js.Dynamic
            .literal(hash = datumHashHex, datum = datumCborHex)
            .asInstanceOf[JDatumEntry]
        val options = js.Dynamic
            .literal(datums = js.Array(datumEntry))
            .asInstanceOf[JsEmulatorOptions]
        val emulator = JEmulator.create(JsCardanoInfo.mainnet(), options)

        val found = optionOf(emulator.getDatum(datumHashHex))
            .getOrElse(fail("seeded datum must be found by hash"))
        assert(ByteString.fromArray(found.toArray.map(_.toByte)).toHex == datumCborHex)
        assert(optionOf(emulator.getDatum("00" * 32)).isEmpty)
    }

    test("time and slot move together") {
        val emulator = JEmulator.create(JsCardanoInfo.mainnet())
        val info = emulator.getCardanoInfo()
        emulator.setSlot(1000)
        assert(emulator.getTime() == info.slotConfig.slotToTime(1000))
        emulator.setTime(info.slotConfig.slotToTime(2000))
        assert(emulator.getSlot() == 2000.0)
    }

    test("addUtxo and removeUtxo edit the ledger") {
        val emulator = JEmulator.create(JsCardanoInfo.mainnet())
        val aliceBech32 =
            Alice
                .address(Network.Mainnet)
                .encode
                .getOrElse(fail("test address must encode to bech32"))
        val utxo = new JsUtxo("11" * 32, 0.0, aliceBech32, JsValue.ada(js.BigInt("5")))
        val before = emulator.getUtxos().length

        emulator.addUtxo(utxo)
        val afterAdd = emulator.getUtxos()
        assert(afterAdd.length == before + 1)
        // Not just the count: the specific UTxO added must be the one found by its own out-ref,
        // with the right address and value - a bug that added an empty/wrong UTxO would still
        // grow the count by one.
        val added = afterAdd.toSeq
            .find(u => u.txHash == utxo.txHash && u.outputIndex == utxo.outputIndex)
            .getOrElse(fail("added UTxO must be found by its out-ref"))
        assert(added.address == aliceBech32)
        assert(added.value.coin.toString == "5000000")

        emulator.removeUtxo(outRef(utxo))
        val afterRemove = emulator.getUtxos()
        assert(afterRemove.length == before)
        assert(
          afterRemove.toSeq.forall(u =>
              !(u.txHash == utxo.txHash && u.outputIndex == utxo.outputIndex)
          ),
          "the removed out-ref must actually be gone, not merely count-balanced by a different removal"
        )
    }

    test("getDelegation and getStakeReward take a reward address, for any credential kind") {
        val scriptHash = PlutusV3.alwaysOk.script.scriptHash
        val scriptStakeBech32 =
            StakeAddress(Network.Mainnet, StakePayload.Script(scriptHash)).toBech32
                .getOrElse(fail("test address must encode to bech32"))
        val poolKeyHash = PoolKeyHash.fromHex("33" * 28)

        val keyHash = StakeKeyHash.fromHex("55" * 28)
        val keyStakeBech32 = StakeAddress(Network.Mainnet, StakePayload.Stake(keyHash)).toBech32
            .getOrElse(fail("test address must encode to bech32"))

        val scriptRegistration = js.Dynamic
            .literal(
              credentialType = "script",
              credentialHash = scriptHash.toHex,
              rewards = js.BigInt(1_000_000),
              delegatedTo = poolKeyHash.toHex
            )
            .asInstanceOf[JStakeRegistration]
        // Key credential too: getStakeReward used to accept only a script hash. A bech32 reward
        // address carries its own credential kind, so this must work without special-casing.
        val keyRegistration = js.Dynamic
            .literal(
              credentialType = "key",
              credentialHash = keyHash.toHex,
              rewards = js.BigInt(2_000_000)
            )
            .asInstanceOf[JStakeRegistration]
        val options = js.Dynamic
            .literal(stakeRegistrations = js.Array(scriptRegistration, keyRegistration))
            .asInstanceOf[JsEmulatorOptions]
        val emulator = JEmulator.create(JsCardanoInfo.mainnet(), options)

        val info = emulator.getDelegation(scriptStakeBech32)
        assert(info.rewards.toString == "1000000")
        assert(optionOf(info.poolId).contains(poolKeyHash.toHex))
        assert(optionOf(emulator.getStakeReward(scriptStakeBech32)).contains(js.BigInt("1000000")))

        assert(optionOf(emulator.getStakeReward(keyStakeBech32)).contains(js.BigInt("2000000")))

        // A never-registered credential: rewards defaults to 0 (getDelegation), but
        // getStakeReward must come back undefined so the two are distinguishable.
        val neverRegisteredBech32 =
            StakeAddress(
              Network.Mainnet,
              StakePayload.Script(ScriptHash.fromHex("44" * 28))
            ).toBech32
                .getOrElse(fail("test address must encode to bech32"))
        assert(emulator.getDelegation(neverRegisteredBech32).rewards.toString == "0")
        assert(optionOf(emulator.getDelegation(neverRegisteredBech32).poolId).isEmpty)
        assert(optionOf(emulator.getStakeReward(neverRegisteredBech32)).isEmpty)
    }

    test("getDelegation and getStakeReward reject a string that is not a reward address") {
        val emulator = JEmulator.create(JsCardanoInfo.mainnet())

        // A valid bech32 address, but a payment address, not a reward (stake) address - the
        // explicit `case other =>` throw in `rewardAddressCredential`. If this silently derived
        // some credential instead of throwing, it would return plausible-looking data for the
        // wrong account, which is the failure mode worth guarding against here.
        val paymentAddressBech32 = Alice
            .address(Network.Mainnet)
            .encode
            .getOrElse(fail("test address must encode to bech32"))
        intercept[IllegalArgumentException] { emulator.getDelegation(paymentAddressBech32) }
        intercept[IllegalArgumentException] { emulator.getStakeReward(paymentAddressBech32) }

        // Not bech32 (or Base58) at all - fails inside `Address.fromString` itself, before
        // `rewardAddressCredential`'s own match ever runs. A different code path to the one
        // above, but documented as raising the same exception type.
        intercept[IllegalArgumentException] { emulator.getDelegation("not-a-bech32-address") }
        intercept[IllegalArgumentException] { emulator.getStakeReward("not-a-bech32-address") }
    }

    test("getStakeDistribution reports live UTxO stake and reward balance per credential") {
        val scriptHash = PlutusV3.alwaysOk.script.scriptHash
        val stakeCredentialHex = scriptHash.toHex
        val poolKeyHash = PoolKeyHash.fromHex("66" * 28)

        val delegatingAddress = ShelleyAddress(
          Network.Mainnet,
          ShelleyPaymentPart.Key(Alice.addrKeyHash),
          ShelleyDelegationPart.Script(scriptHash)
        )
        val seededUtxo = Input(genesisHash, 0) -> Output(delegatingAddress, Value.ada(250))

        val stakeRegistration = js.Dynamic
            .literal(
              credentialType = "script",
              credentialHash = stakeCredentialHex,
              rewards = js.BigInt(1_000_000),
              delegatedTo = poolKeyHash.toHex
            )
            .asInstanceOf[JStakeRegistration]
        val options = js.Dynamic
            .literal(
              utxos = js.Array(JsUtxo.wrap(seededUtxo._1, seededUtxo._2)),
              stakeRegistrations = js.Array(stakeRegistration)
            )
            .asInstanceOf[JsEmulatorOptions]
        val emulator = JEmulator.create(JsCardanoInfo.mainnet(), options)

        val entry = emulator
            .getStakeDistribution()
            .toSeq
            .find(_.credential == stakeCredentialHex)
            .getOrElse(fail("credential missing from stake distribution"))
        // Distinct values on purpose - stake (from the UTxO) and rewards (from certState) come
        // from two different sources, so pinning both distinguishes a mapping that swapped them.
        assert(BigInt(entry.stake.toString) == 250_000_000L)
        assert(BigInt(entry.rewards.toString) == 1_000_000L)
        assert(optionOf(entry.pool).contains(poolKeyHash.toHex))
    }
}
