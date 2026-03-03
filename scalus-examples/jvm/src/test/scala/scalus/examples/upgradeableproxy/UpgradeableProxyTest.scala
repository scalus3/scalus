package scalus.examples.upgradeableproxy

import org.scalatest.funsuite.AnyFunSuite
import scalus.*
import scalus.cardano.address.{StakeAddress, StakePayload}
import scalus.cardano.ledger.*
import scalus.cardano.ledger.rules.{Context, DefaultMutators}
import scalus.cardano.node.{Emulator, NodeSubmitError}
import scalus.cardano.txbuilder.{ScriptWitness, TwoArgumentPlutusScriptWitness, TxBuilder}
import scalus.compiler.Options
import scalus.testing.kit.Party.{Alice, Bob}
import scalus.testing.kit.{ScalusTest, TestUtil}
import scalus.uplc.PlutusV3
import scalus.uplc.builtin.Data
import scalus.utils.await
import scalus.cardano.onchain.plutus.v3.{ScriptContext, Value as OnchainValue}
import scalus.cardano.onchain.plutus.prelude.{!==, ===, require}

class UpgradeableProxyTest extends AnyFunSuite, ScalusTest {

    private given env: CardanoInfo = TestUtil.testEnvironment
    private given Options = Options.release

    private val contract = ProxyContract.withErrorTraces

    // Succeeds when the transaction mints nothing.
    private val mustNotMintLogic = PlutusV3
        .compile((scData: Data) => {
            val sc = scData.to[ScriptContext]
            require(sc.txInfo.mint === OnchainValue.zero, "Transaction must not mint")
        })
        .withErrorTraces

    // Succeeds when the transaction mints something.
    private val mustMintLogic = PlutusV3
        .compile((scData: Data) => {
            val sc = scData.to[ScriptContext]
            require(sc.txInfo.mint !== OnchainValue.zero, "Transaction must mint")
        })
        .withErrorTraces

    private val mustNotMintHash = mustNotMintLogic.script.scriptHash
    private val mustMintHash = mustMintLogic.script.scriptHash

    private val mustNotMintStakeAddress =
        StakeAddress(env.network, StakePayload.Script(mustNotMintHash))
    private val mustMintStakeAddress =
        StakeAddress(env.network, StakePayload.Script(mustMintHash))

    private val mustNotMintWitness =
        TwoArgumentPlutusScriptWitness.attached(mustNotMintLogic.script, Data.unit)
    private val mustMintWitness =
        TwoArgumentPlutusScriptWitness.attached(mustMintLogic.script, Data.unit)

    private val proxyValue = Value.ada(10)

    // Always-succeeds script to mint tokens, register script addresses, etc.
    private val alwaysSucceeds = PlutusV3.alwaysOk.withErrorTraces
    private val alwaysSucceedsWitness =
        TwoArgumentPlutusScriptWitness.attached(alwaysSucceeds.script, Data.unit)
    private val alwaysSucceedsStakeAddress =
        StakeAddress(env.network, StakePayload.Script(alwaysSucceeds.script.scriptHash))
    private val mintAssets: Map[AssetName, Long] = Map(AssetName.fromString("test") -> 1L)

    // One third to make sure that up to 3 scripts can fit.
    private val evaluator: PlutusScriptEvaluator = PlutusScriptEvaluator.const(
      ExUnits(
        env.protocolParams.maxTxExecutionUnits.memory / 3,
        env.protocolParams.maxTxExecutionUnits.steps / 3
      )
    )

    private val txCreator: ProxyTransactions =
        ProxyTransactions(env = env, evaluator = evaluator, contract = contract)

    private def createProvider(): Emulator =
        Emulator(
          initialUtxos = Map(
            Input(TestUtil.genesisHash, 0) -> Output(Alice.address, Value.ada(5000)),
            Input(TestUtil.genesisHash, 1) -> Output(Alice.address, Value.ada(5000)),
            Input(TestUtil.genesisHash, 2) -> Output(Bob.address, Value.ada(5000))
          ),
          initialContext = Context.testMainnet(),
          mutators = DefaultMutators.all
        )

    private def assertSubmitScriptFail(provider: Emulator, expectedError: String)(
        tx: Transaction
    ): Unit = {
        val result = provider.submit(tx).await()
        result match
            case Right(_) =>
                fail(s"Transaction submission should have failed but succeeded")
            case Left(sf: NodeSubmitError.ScriptFailure) =>
                if !sf.logs.exists(_.contains(expectedError)) then
                    fail(
                      s"Expected error containing '$expectedError' but got: ${sf.logs.mkString(", ")}"
                    )
            case Left(other) =>
                fail(s"Expected ScriptFailure but got: $other")
    }

    private def deployProxy(provider: Emulator, logicHash: ScriptHash): Utxo = {
        val utxos = provider.findUtxos(Alice.address).await().toOption.get
        val tx = txCreator.deploy(
          utxos = utxos,
          value = proxyValue,
          logicHash = logicHash,
          owner = Alice.addrKeyHash,
          sponsor = Alice.address,
          signer = Alice.signer
        )
        assert(provider.submit(tx).await().isRight, "deploy failed")
        Utxo(tx.utxos.find(_._2.address == txCreator.scriptAddress).get)
    }

    private def registerStake(
        provider: Emulator,
        stakeAddress: StakeAddress,
        witness: ScriptWitness
    ): Unit = {
        val utxos = provider.findUtxos(Alice.address).await().toOption.get
        val tx = TxBuilder(env, evaluator)
            .registerStake(stakeAddress, witness)
            .complete(availableUtxos = utxos, sponsor = Alice.address)
            .sign(Alice.signer)
            .transaction
        assert(provider.submit(tx).await().isRight, s"registerStake failed")
    }

    // mustMintLogic requires a mint in every tx it runs in, including registration.
    private def registerMustMintStake(provider: Emulator): Unit = {
        val utxos = provider.findUtxos(Alice.address).await().toOption.get
        val tx = TxBuilder(env, evaluator)
            .registerStake(mustMintStakeAddress, mustMintWitness)
            .mint(alwaysSucceeds, mintAssets, Data.unit)
            .complete(availableUtxos = utxos, sponsor = Alice.address)
            .sign(Alice.signer)
            .transaction
        assert(provider.submit(tx).await().isRight, s"registerMustMintStake failed")
    }

    test("success: Call with mustNotMint logic validator") {
        val provider = createProvider()
        val proxyUtxo = deployProxy(provider, mustNotMintHash)
        registerStake(provider, mustNotMintStakeAddress, mustNotMintWitness)

        val utxos = provider.findUtxos(Alice.address).await().toOption.get
        // no minting -- should pass
        val tx = txCreator.call(
          utxos = utxos,
          proxyUtxo = proxyUtxo,
          logicStakeAddress = mustNotMintStakeAddress,
          logicWitness = mustNotMintWitness,
          sponsor = Alice.address,
          signer = Alice.signer
        )
        assert(provider.submit(tx).await().isRight, "call with mustNotMint logic failed")

        val continuationUtxo = tx.utxos.find(_._2.address == txCreator.scriptAddress)
        assert(continuationUtxo.isDefined, "Continuation UTxO not found")
        val datum = continuationUtxo.get._2.requireInlineDatum.to[ProxyDatum]
        assert(datum.logicHash == mustNotMintHash, "Logic hash changed unexpectedly")
    }

    test("failure: Call without the logic validator withdrawal") {
        val provider = createProvider()
        val proxyUtxo = deployProxy(provider, mustNotMintHash)

        val utxos = provider.findUtxos(Alice.address).await().toOption.get
        val proxyDatum = proxyUtxo.output.requireInlineDatum
        // no withdrawal -- the call should fail
        val tx = TxBuilder(env, evaluator)
            .spend(proxyUtxo, ProxyRedeemer.Call, txCreator.script)
            .payTo(txCreator.scriptAddress, proxyUtxo.output.value, proxyDatum)
            .complete(availableUtxos = utxos, sponsor = Alice.address)
            .sign(Alice.signer)
            .transaction

        assertSubmitScriptFail(provider, ProxyValidator.LogicNotInvoked)(tx)
    }

    test("failure: Call with wrong logic validator withdrawn") {
        val provider = createProvider()
        val proxyUtxo = deployProxy(provider, mustNotMintHash)
        // register the malicious logic
        registerStake(provider, alwaysSucceedsStakeAddress, alwaysSucceedsWitness)

        // Withdraw alwaysSucceeds instead of mustNotMint -- should be rejected, as we've swapped out the logic maliciously.
        val utxos = provider.findUtxos(Alice.address).await().toOption.get
        val proxyDatum = proxyUtxo.output.requireInlineDatum
        val tx = TxBuilder(env, evaluator)
            .spend(proxyUtxo, ProxyRedeemer.Call, txCreator.script)
            .payTo(txCreator.scriptAddress, proxyUtxo.output.value, proxyDatum)
            .withdrawRewards(alwaysSucceedsStakeAddress, Coin.zero, alwaysSucceedsWitness)
            .complete(availableUtxos = utxos, sponsor = Alice.address)
            .sign(Alice.signer)
            .transaction

        assertSubmitScriptFail(provider, ProxyValidator.LogicNotInvoked)(tx)
    }

    test("failure: Call when logic validator itself fails") {
        val provider = createProvider()

        val proxyUtxo = deployProxy(provider, mustNotMintHash)
        registerStake(provider, mustNotMintStakeAddress, mustNotMintWitness)

        val utxos = provider.findUtxos(Alice.address).await().toOption.get
        val proxyDatum = proxyUtxo.output.requireInlineDatum

        // The logic forbids minting, but we're trying to mint anyway.
        val tx = TxBuilder(env, evaluator)
            .spend(proxyUtxo, ProxyRedeemer.Call, txCreator.script)
            .payTo(txCreator.scriptAddress, proxyUtxo.output.value, proxyDatum)
            .withdrawRewards(mustNotMintStakeAddress, Coin.zero, mustNotMintWitness)
            .mint(alwaysSucceeds, mintAssets, Data.unit)
            .complete(availableUtxos = utxos, sponsor = Alice.address)
            .sign(Alice.signer)
            .transaction

        assertSubmitScriptFail(provider, "Transaction must not mint")(tx)
    }

    test("failure: Call with logic hash changed in continuation datum") {
        val provider = createProvider()
        val proxyUtxo = deployProxy(provider, mustNotMintHash)
        registerStake(provider, mustNotMintStakeAddress, mustNotMintWitness)

        val utxos = provider.findUtxos(Alice.address).await().toOption.get
        val oldDatum = proxyUtxo.output.requireInlineDatum.to[ProxyDatum]
        val tamperedDatum = oldDatum.copy(logicHash = mustMintHash)
        val tx = TxBuilder(env, evaluator)
            .spend(proxyUtxo, ProxyRedeemer.Call, txCreator.script)
            .payTo(txCreator.scriptAddress, proxyUtxo.output.value, tamperedDatum)
            .withdrawRewards(mustNotMintStakeAddress, Coin.zero, mustNotMintWitness)
            .complete(availableUtxos = utxos, sponsor = Alice.address)
            .sign(Alice.signer)
            .transaction

        assertSubmitScriptFail(provider, ProxyValidator.LogicHashChanged)(tx)
    }

    test("success: Upgrade from mustNotMint to mustMint logic with owner signature") {
        val provider = createProvider()
        val proxyUtxo = deployProxy(provider, mustNotMintHash)

        val utxos = provider.findUtxos(Alice.address).await().toOption.get
        val tx = txCreator.upgrade(
          utxos = utxos,
          proxyUtxo = proxyUtxo,
          newLogicHash = mustMintHash,
          ownerPkh = Alice.addrKeyHash,
          sponsor = Alice.address,
          signer = Alice.signer
        )
        assert(provider.submit(tx).await().isRight, "upgrade failed")

        val newUtxo = tx.utxos.find(_._2.address == txCreator.scriptAddress)
        assert(newUtxo.isDefined, "Continuation UTxO not found after upgrade")
        val newDatum = newUtxo.get._2.requireInlineDatum.to[ProxyDatum]
        assert(newDatum.logicHash == mustMintHash, "Logic hash not updated to mustMint")
    }

    test("success: Call rejects old logic after upgrade") {
        val provider = createProvider()
        val proxyUtxo = deployProxy(provider, mustNotMintHash)
        registerStake(provider, mustNotMintStakeAddress, mustNotMintWitness)

        val upgradeUtxos = provider.findUtxos(Alice.address).await().toOption.get
        val upgradeTx = txCreator.upgrade(
          utxos = upgradeUtxos,
          proxyUtxo = proxyUtxo,
          newLogicHash = mustMintHash,
          ownerPkh = Alice.addrKeyHash,
          sponsor = Alice.address,
          signer = Alice.signer
        )
        assert(provider.submit(upgradeTx).await().isRight)
        val upgradedProxyUtxo =
            Utxo(upgradeTx.utxos.find(_._2.address == txCreator.scriptAddress).get)

        val callUtxos = provider.findUtxos(Alice.address).await().toOption.get
        val tx = TxBuilder(env, evaluator)
            .spend(upgradedProxyUtxo, ProxyRedeemer.Call, txCreator.script)
            .payTo(
              txCreator.scriptAddress,
              upgradedProxyUtxo.output.value,
              upgradedProxyUtxo.output.requireInlineDatum
            )
            .withdrawRewards(mustNotMintStakeAddress, Coin.zero, mustNotMintWitness) // old logic
            .complete(availableUtxos = callUtxos, sponsor = Alice.address)
            .sign(Alice.signer)
            .transaction

        assertSubmitScriptFail(provider, ProxyValidator.LogicNotInvoked)(tx)
    }

    test("failure: Upgrade without owner signature") {
        val provider = createProvider()
        val proxyUtxo = deployProxy(provider, mustNotMintHash)

        val utxos = provider.findUtxos(Bob.address).await().toOption.get
        val oldDatum = proxyUtxo.output.requireInlineDatum.to[ProxyDatum]
        val newDatum = oldDatum.copy(logicHash = mustMintHash)
        val tx = TxBuilder(env, evaluator)
            .spend(
              proxyUtxo,
              ProxyRedeemer.Upgrade(mustMintHash),
              txCreator.script,
              Set(Bob.addrKeyHash)
            )
            .payTo(txCreator.scriptAddress, proxyUtxo.output.value, newDatum)
            .complete(availableUtxos = utxos, sponsor = Bob.address)
            .sign(Bob.signer)
            .transaction

        assertSubmitScriptFail(provider, ProxyValidator.NotSignedByOwner)(tx)
    }

    test("failure: Upgrade with wrong logic hash in continuation datum") {
        val provider = createProvider()
        val proxyUtxo = deployProxy(provider, mustNotMintHash)

        val utxos = provider.findUtxos(Alice.address).await().toOption.get
        val oldDatum = proxyUtxo.output.requireInlineDatum

        val tx = TxBuilder(env, evaluator)
            .spend(
              proxyUtxo,
              ProxyRedeemer.Upgrade(mustMintHash),
              txCreator.script,
              Set(Alice.addrKeyHash)
            )
            .payTo(txCreator.scriptAddress, proxyUtxo.output.value, oldDatum)
            .complete(availableUtxos = utxos, sponsor = Alice.address)
            .sign(Alice.signer)
            .transaction

        assertSubmitScriptFail(provider, ProxyValidator.LogicHashMismatch)(tx)
    }

    test("failure: Upgrade with value drained from proxy") {
        val provider = createProvider()
        val proxyUtxo = deployProxy(provider, mustNotMintHash)

        val utxos = provider.findUtxos(Alice.address).await().toOption.get
        val oldDatum = proxyUtxo.output.requireInlineDatum.to[ProxyDatum]
        val newDatum = oldDatum.copy(logicHash = mustMintHash)
        val tx = TxBuilder(env, evaluator)
            .spend(
              proxyUtxo,
              ProxyRedeemer.Upgrade(mustMintHash),
              txCreator.script,
              Set(Alice.addrKeyHash)
            )
            .payTo(txCreator.scriptAddress, Value.ada(5), newDatum)
            .complete(availableUtxos = utxos, sponsor = Alice.address)
            .sign(Alice.signer)
            .transaction

        assertSubmitScriptFail(provider, ProxyValidator.ValueMustBePreserved)(tx)
    }

    test("success then failure: mustMint logic works, then fails after upgrade to mustNotMint") {
        val provider = createProvider()
        // Deploy with mustMint logic
        val proxyUtxo = deployProxy(provider, mustMintHash)
        registerMustMintStake(provider)
        registerStake(provider, alwaysSucceedsStakeAddress, alwaysSucceedsWitness)

        // Step 1: Call with mint -- mustMintLogic passes
        val callUtxos1 = provider.findUtxos(Alice.address).await().toOption.get
        val proxyDatum = proxyUtxo.output.requireInlineDatum
        val callTx1 = TxBuilder(env, evaluator)
            .spend(proxyUtxo, ProxyRedeemer.Call, txCreator.script)
            .payTo(txCreator.scriptAddress, proxyValue, proxyDatum)
            .withdrawRewards(mustMintStakeAddress, Coin.zero, mustMintWitness)
            .mint(alwaysSucceeds, mintAssets, Data.unit)
            .complete(availableUtxos = callUtxos1, sponsor = Alice.address)
            .sign(Alice.signer)
            .transaction
        assert(provider.submit(callTx1).await().isRight, "first call with mint should succeed")
        val proxyUtxo2 = Utxo(callTx1.utxos.find(_._2.address == txCreator.scriptAddress).get)

        // Step 2: Upgrade to mustNotMint logic
        val upgradeUtxos = provider.findUtxos(Alice.address).await().toOption.get
        val upgradeTx = txCreator.upgrade(
          utxos = upgradeUtxos,
          proxyUtxo = proxyUtxo2,
          newLogicHash = mustNotMintHash,
          ownerPkh = Alice.addrKeyHash,
          sponsor = Alice.address,
          signer = Alice.signer
        )
        assert(provider.submit(upgradeTx).await().isRight, "upgrade to mustNotMint should succeed")
        val proxyUtxo3 = Utxo(upgradeTx.utxos.find(_._2.address == txCreator.scriptAddress).get)
        registerStake(provider, mustNotMintStakeAddress, mustNotMintWitness)

        // Step 3: Call with mint again -- mustNotMintLogic now rejects it
        val callUtxos2 = provider.findUtxos(Alice.address).await().toOption.get
        val callTx2 = TxBuilder(env, evaluator)
            .spend(proxyUtxo3, ProxyRedeemer.Call, txCreator.script)
            .payTo(txCreator.scriptAddress, proxyValue, proxyUtxo3.output.requireInlineDatum)
            .withdrawRewards(mustNotMintStakeAddress, Coin.zero, mustNotMintWitness)
            .mint(alwaysSucceeds, mintAssets, Data.unit)
            .complete(availableUtxos = callUtxos2, sponsor = Alice.address)
            .sign(Alice.signer)
            .transaction
        assertSubmitScriptFail(provider, "Transaction must not mint")(callTx2)
    }
}
