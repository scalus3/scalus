package scalus.cardano.node

import org.scalacheck.Gen
import org.scalatest.funsuite.AnyFunSuite
import org.scalatestplus.scalacheck.ScalaCheckPropertyChecks
import scalus.uplc.builtin.{ByteString, Data}
import scalus.cardano.address.{Network, StakeAddress, StakePayload}
import scalus.cardano.ledger.*
import scalus.cardano.ledger.rules.Context
import scalus.cardano.txbuilder.{ScriptSource, TwoArgumentPlutusScriptWitness, TxBuilder}
import scalus.testing.kit.Party.{Alice, Bob}
import scalus.uplc.PlutusV3
import scalus.utils.await

class EmulatorTest extends AnyFunSuite with ScalaCheckPropertyChecks {

    given testEnv: CardanoInfo = CardanoInfo.mainnet
    val genesisHash: TransactionHash =
        TransactionHash.fromByteString(ByteString.fromHex("0" * 64))

    test("Emulator.utxos returns all UTXOs") {
        val initialUtxos = Map(
          Input(genesisHash, 0) -> Output(Alice.address, Value.ada(100)),
          Input(genesisHash, 1) -> Output(Bob.address, Value.ada(50))
        )

        val provider = Emulator(
          initialUtxos = initialUtxos,
          validators = Set.empty,
          mutators = Emulator.defaultMutators
        )

        // utxos should return all current UTXOs
        assert(provider.utxos == initialUtxos, "Emulator.utxos should return all UTXOs")

        // Build and submit tx1
        val tx1 = TxBuilder(testEnv)
            .payTo(Bob.address, Value.ada(10))
            .complete(provider, Alice.address)
            .await()
            .transaction

        val submitResult = provider.submit(tx1).await()
        assert(submitResult.isRight, s"Transaction should succeed: $submitResult")

        // After tx1, utxos should be updated
        val utxosAfterTx1 = provider.utxos
        assert(utxosAfterTx1.size >= 2, "Should have at least 2 UTXOs after tx1")
        assert(
          utxosAfterTx1.keys.exists(_.transactionId == tx1.id),
          "UTXOs should include outputs from tx1"
        )
    }

    test("Emulator records an applied-transaction log with slot and spent inputs") {
        val initialUtxos = Map(
          Input(genesisHash, 0) -> Output(Alice.address, Value.ada(100))
        )
        val provider =
            Emulator(
              initialUtxos = initialUtxos,
              validators = Set.empty,
              mutators = Emulator.defaultMutators
            )
        provider.setSlot(42L)

        assert(provider.appliedTxLog.isEmpty, "log should start empty")

        val tx = TxBuilder(testEnv)
            .payTo(Bob.address, Value.ada(10))
            .complete(provider, Alice.address)
            .await()
            .transaction

        val submitResult = provider.submit(tx).await()
        assert(submitResult.isRight, s"submit should succeed: $submitResult")

        val log = provider.appliedTxLog
        assert(log.size == 1, s"expected a single applied tx, got ${log.size}")
        val applied = log.head
        assert(applied.tx.id == tx.id)
        assert(applied.slot == 42L, "applied slot should be the emulator slot at submission time")
        // the transaction consumed Alice's genesis UTxO, which is gone from the live set
        assert(applied.spent == initialUtxos, "spent should be the resolved consumed inputs")
        assert(!provider.utxos.contains(Input(genesisHash, 0)), "spent input removed from live set")
        assert(provider.getTransaction(tx.id).contains(tx), "getTransaction finds the applied tx")
        assert(provider.getTransaction(genesisHash).isEmpty, "unknown hash resolves to None")
        // index by hash-id
        assert(provider.appliedTxIndex.keySet == Set(tx.id))
        assert(provider.getAppliedTx(tx.id).contains(applied), "getAppliedTx returns the record")
        assert(provider.hasTx(tx.id) && !provider.hasTx(genesisHash))
        assert(provider.appliedTxs == Set(tx.id), "appliedTxs derives from the index")
    }

    test("Emulator.clearAppliedTxs resets the log/index but keeps ledger state") {
        val initialUtxos = Map(
          Input(genesisHash, 0) -> Output(Alice.address, Value.ada(100))
        )
        val provider =
            Emulator(
              initialUtxos = initialUtxos,
              validators = Set.empty,
              mutators = Emulator.defaultMutators
            )
        val tx = TxBuilder(testEnv)
            .payTo(Bob.address, Value.ada(10), (_: Transaction) => Data.unit)
            .complete(provider, Alice.address)
            .await()
            .transaction
        assert(provider.submit(tx).await().isRight)

        val utxosBefore = provider.utxos
        val datumsBefore = provider.datums
        assert(provider.appliedTxLog.nonEmpty, "precondition: a tx was applied")
        assert(provider.datums.nonEmpty, "precondition: the tx carried an inline datum")

        provider.clearAppliedTxs()

        // bookkeeping cleared
        assert(provider.appliedTxLog.isEmpty)
        assert(provider.appliedTxIndex.isEmpty)
        assert(provider.appliedTxs.isEmpty)
        assert(!provider.hasTx(tx.id))
        assert(provider.getTransaction(tx.id).isEmpty)
        // ledger state untouched
        assert(provider.utxos == utxosBefore, "utxos must be preserved")
        assert(provider.datums == datumsBefore, "datum cache must be preserved")
    }

    test("Emulator.withRegisteredStakeCredentials allows zero-withdrawal without registration tx") {
        val alwaysOkScript = PlutusV3.alwaysOk
        val scriptHash = alwaysOkScript.script.scriptHash
        val stakeCred = Credential.ScriptHash(scriptHash)
        val stakeAddress =
            StakeAddress(Network.Mainnet, StakePayload.Script(scriptHash))
        val witness = TwoArgumentPlutusScriptWitness(
          ScriptSource.PlutusScriptValue(alwaysOkScript.script),
          Data.unit
        )
        val alice = Alice.address(Network.Mainnet)
        val initialUtxos = Map(
          Input(genesisHash, 0) -> Output(alice, Value.ada(5000))
        )
        val emulator = Emulator.withRegisteredStakeCredentials(
          initialUtxos = initialUtxos,
          initialStakeRewards = Map(stakeCred -> Coin.zero)
        )
        val tx = TxBuilder(testEnv)
            .withdrawRewards(stakeAddress, Coin.zero, witness)
            .complete(initialUtxos, alice)
            .sign(Alice.signer)
            .transaction
        val result = emulator.submitSync(tx)
        assert(
          result.isRight,
          s"Zero-withdrawal should succeed with pre-registered credential: $result"
        )
    }

    test("Emulator.withRegisteredStakeCredentials pre-populates certState correctly") {
        val alwaysOkScript = PlutusV3.alwaysOk
        val scriptHash = alwaysOkScript.script.scriptHash
        val stakeCred = Credential.ScriptHash(scriptHash)
        val alice = Alice.address(Network.Mainnet)
        val initialUtxos = Map(
          Input(genesisHash, 0) -> Output(alice, Value.ada(5000))
        )
        val initialReward = Coin(42_000_000L)
        val emulator = Emulator.withRegisteredStakeCredentials(
          initialUtxos = initialUtxos,
          initialStakeRewards = Map(stakeCred -> initialReward)
        )
        val cs = emulator.certState
        assert(
          cs.dstate.rewards.get(stakeCred).contains(initialReward),
          s"rewards should contain stake credential with expected amount: ${cs.dstate.rewards}"
        )
        val expectedDeposit = Coin(CardanoInfo.mainnet.protocolParams.stakeAddressDeposit)
        assert(
          cs.dstate.deposits.get(stakeCred).contains(expectedDeposit),
          s"deposits should contain stake credential with protocol deposit: ${cs.dstate.deposits}"
        )
    }

    test("Emulator doesn't allow stake registration and withdrawal in the same tx") {
        val alice = Alice.address(Network.Mainnet)
        val initialUtxos = Map(
          Input(genesisHash, 0) -> Output(alice, Value.ada(100)),
          Input(genesisHash, 1) -> Output(alice, Value.ada(200)),
        )
        val emulator = Emulator(initialUtxos)

        val (withdrawZeroStakeAddress, withdrawZeroScript, withdrawZeroScriptWitness) = {
            val alwaysOkScript = PlutusV3.alwaysOk
            val stakeAddress =
                StakeAddress(Network.Testnet, StakePayload.Script(alwaysOkScript.script.scriptHash))
            val witness = TwoArgumentPlutusScriptWitness(
              ScriptSource.PlutusScriptValue(alwaysOkScript.script),
              Data.unit
            )
            (stakeAddress, alwaysOkScript, witness)
        }
        val tx = TxBuilder(testEnv)
            .registerStake(
              withdrawZeroStakeAddress,
              TwoArgumentPlutusScriptWitness.attached(
                withdrawZeroScript.script,
                Data.unit
              )
            )
            .withdrawRewards(withdrawZeroStakeAddress, Coin.zero, withdrawZeroScriptWitness)
            .complete(initialUtxos, alice)
            .sign(Alice.signer)
            .transaction

        val result = emulator.submitSync(tx)
        assert(result.isLeft)
        assert(result.swap.getOrElse(fail()).message.contains("missing reward accounts"))
    }

    test("Property: submitted valid transaction UTXOs become available") {
        forAll(
          Gen.choose(100L, 1000L),
          Gen.choose(10L, 50L)
        ) { (initialAmount: Long, paymentAmount: Long) =>
            whenever(initialAmount > paymentAmount + 1) {
                val initialUtxos = Map(
                  Input(genesisHash, 0) -> Output(
                    Alice.address,
                    Value.ada(initialAmount)
                  )
                )

                val emulator = Emulator(
                  initialUtxos = initialUtxos,
                  validators = Set.empty,
                  mutators = Emulator.defaultMutators
                )

                val tx = TxBuilder(testEnv)
                    .payTo(Bob.address, Value.ada(paymentAmount))
                    .complete(emulator, Alice.address)
                    .await()
                    .transaction

                val submitResult = emulator.submit(tx).await()

                assert(
                  submitResult.isRight,
                  s"Transaction should be accepted but got: $submitResult"
                )

                // After submission, transaction outputs should be available
                val utxosAfter = emulator.utxos
                tx.body.value.outputs.zipWithIndex.foreach { case (output, idx) =>
                    val txInput = Input(tx.id, idx)
                    assert(
                      utxosAfter.contains(txInput),
                      s"Output $idx from transaction ${tx.id} should be available in UTXOs"
                    )
                    assert(
                      utxosAfter(txInput) == output.value,
                      s"Output $idx value should match"
                    )
                }

                // All inputs should be consumed (removed from UTXO set)
                tx.body.value.inputs.toSeq.foreach { input =>
                    assert(
                      !utxosAfter.contains(input),
                      s"Input $input should be consumed (removed from UTXOs)"
                    )
                }
            }
        }
    }

    test("Property: invalid transaction (double spend) is rejected") {
        forAll(Gen.choose(100L, 1000L)) { initialAmount =>
            val initialUtxos = Map(
              Input(genesisHash, 0) -> Output(
                Alice.address,
                Value.ada(initialAmount)
              )
            )

            val emulator = Emulator(
              initialUtxos = initialUtxos,
              validators = Set.empty,
              mutators = Emulator.defaultMutators
            )

            val tx1 = TxBuilder(testEnv)
                .payTo(Bob.address, Value.ada(10))
                .complete(emulator, Alice.address)
                .await()
                .transaction

            val submit1 = emulator.submit(tx1).await()
            assert(submit1.isRight, "First transaction should succeed")

            val submit2 = emulator.submit(tx1).await()
            assert(submit2.isLeft, "Double spend should be rejected")
        }
    }

    private val poolOperator = AddrKeyHash.fromHex("f" * 56)
    private val poolId = PoolKeyHash.fromByteString(poolOperator)

    private def mkPoolRegistration(params: ProtocolParams): Certificate.PoolRegistration =
        Certificate.PoolRegistration(
          operator = poolOperator,
          vrfKeyHash = VrfKeyHash.fromHex("a" * 64),
          pledge = Coin.ada(100),
          cost = Coin(params.minPoolCost),
          margin = UnitInterval.one,
          rewardAccount = RewardAccount(
            StakeAddress(Network.Mainnet, StakePayload.Stake(StakeKeyHash.fromHex("b" * 56)))
          ),
          poolOwners = Set(poolOperator),
          relays = IndexedSeq.empty,
          poolMetadata = None
        )

    private def certTx(cert: Certificate): Transaction = Transaction(
      TransactionBody(
        inputs = TaggedSortedSet.empty,
        outputs = IndexedSeq.empty,
        fee = Coin.zero,
        certificates = TaggedOrderedStrictSet.from(Seq(cert))
      )
    )

    test("Emulator applies pool registration and retirement certificates") {
        val params = CardanoInfo.mainnet.protocolParams
        val slotConfig = CardanoInfo.mainnet.slotConfig
        val emulator = Emulator(
          validators = Set.empty,
          mutators = Emulator.defaultMutators
        )

        val registration = mkPoolRegistration(params)
        val regResult = emulator.submitSync(certTx(registration))
        assert(regResult.isRight, s"Pool registration should succeed: $regResult")
        assert(emulator.certState.pstate.stakePools.contains(poolId))
        assert(
          emulator.certState.pstate.deposits.get(poolId).contains(Coin(params.stakePoolDeposit))
        )

        // At slot 0 the mainnet slot config clamps to zeroEpoch
        val currentEpoch = slotConfig.epochOf(0L)
        assert(currentEpoch == 208L)

        // Retirement of an unregistered pool is rejected
        val unknownPool = PoolKeyHash.fromHex("e" * 56)
        val unknownResult =
            emulator.submitSync(certTx(Certificate.PoolRetirement(unknownPool, currentEpoch + 1)))
        assert(unknownResult.isLeft, "Retirement of unregistered pool should fail")

        // Retirement with a non-future epoch is rejected
        val pastResult =
            emulator.submitSync(certTx(Certificate.PoolRetirement(poolId, currentEpoch)))
        assert(pastResult.isLeft, "Retirement with non-future epoch should fail")

        // Valid retirement is scheduled
        val retireResult =
            emulator.submitSync(certTx(Certificate.PoolRetirement(poolId, currentEpoch + 1)))
        assert(retireResult.isRight, s"Pool retirement should succeed: $retireResult")
        assert(emulator.certState.pstate.retiring.get(poolId).contains(currentEpoch + 1))

        // Re-registration cancels the pending retirement
        val reRegResult = emulator.submitSync(certTx(registration))
        assert(reRegResult.isRight, s"Pool re-registration should succeed: $reRegResult")
        assert(!emulator.certState.pstate.retiring.contains(poolId))
        assert(emulator.certState.pstate.futureStakePoolParams.contains(poolId))
    }

    test("Emulator applies DRep registration, update and deregistration certificates") {
        val params = CardanoInfo.mainnet.protocolParams
        val emulator = Emulator(
          validators = Set.empty,
          mutators = Emulator.defaultMutators
        )

        val drepCred = Credential.KeyHash(AddrKeyHash.fromHex("d" * 56))
        val deposit = Coin(params.dRepDeposit)
        val anchor = Anchor(
          url = "https://example.com/drep.json",
          dataHash = DataHash.fromHex("1" * 64)
        )

        // Deregistration before registration is rejected
        val premature = emulator.submitSync(certTx(Certificate.UnregDRepCert(drepCred, deposit)))
        assert(premature.isLeft, "Deregistration of unregistered DRep should fail")

        // Registration with a wrong deposit is rejected
        val wrongDeposit = emulator.submitSync(
          certTx(Certificate.RegDRepCert(drepCred, deposit + Coin.ada(1), None))
        )
        assert(wrongDeposit.isLeft, "Registration with wrong deposit should fail")

        // Registration lands in vstate with the deposit
        val regResult =
            emulator.submitSync(certTx(Certificate.RegDRepCert(drepCred, deposit, None)))
        assert(regResult.isRight, s"DRep registration should succeed: $regResult")
        assert(emulator.certState.vstate.dreps.get(drepCred).map(_.deposit).contains(deposit))

        // Double registration is rejected
        val doubleReg =
            emulator.submitSync(certTx(Certificate.RegDRepCert(drepCred, deposit, None)))
        assert(doubleReg.isLeft, "Double DRep registration should fail")

        // Update changes the anchor
        val updResult =
            emulator.submitSync(certTx(Certificate.UpdateDRepCert(drepCred, Some(anchor))))
        assert(updResult.isRight, s"DRep update should succeed: $updResult")
        assert(emulator.certState.vstate.dreps.get(drepCred).flatMap(_.anchor).contains(anchor))

        // Deregistration with a wrong refund is rejected
        val wrongRefund = emulator.submitSync(
          certTx(Certificate.UnregDRepCert(drepCred, deposit + Coin.ada(1)))
        )
        assert(wrongRefund.isLeft, "Deregistration with wrong refund should fail")

        // Deregistration removes the DRep
        val unregResult = emulator.submitSync(certTx(Certificate.UnregDRepCert(drepCred, deposit)))
        assert(unregResult.isRight, s"DRep deregistration should succeed: $unregResult")
        assert(!emulator.certState.vstate.dreps.contains(drepCred))
    }

    test("Emulator.setSlot preserves evaluatorMode and debugScripts (issue #314)") {
        val emulator = Emulator(
          initialContext =
              Context.testMainnet().copy(evaluatorMode = EvaluatorMode.EvaluateAndComputeCost)
        )
        assert(emulator.evaluatorMode == EvaluatorMode.EvaluateAndComputeCost)

        emulator.setSlot(100L)

        assert(
          emulator.evaluatorMode == EvaluatorMode.EvaluateAndComputeCost,
          "setSlot must not silently revert evaluatorMode to Validate"
        )
        assert(emulator.currentSlot.await() == 100L)
    }

    test("Emulator derives the current epoch from the slot for retirement validation") {
        val params = CardanoInfo.mainnet.protocolParams
        val slotConfig = CardanoInfo.mainnet.slotConfig
        val emulator = Emulator(
          validators = Set.empty,
          mutators = Emulator.defaultMutators
        )

        val regResult = emulator.submitSync(certTx(mkPoolRegistration(params)))
        assert(regResult.isRight, s"Pool registration should succeed: $regResult")

        val epoch = 250L
        emulator.setSlot(slotConfig.firstSlotOfEpoch(epoch))

        // Not in the future
        assert(emulator.submitSync(certTx(Certificate.PoolRetirement(poolId, epoch))).isLeft)
        // Beyond currentEpoch + poolRetireMaxEpoch
        val tooFar = epoch + params.poolRetireMaxEpoch + 1
        assert(emulator.submitSync(certTx(Certificate.PoolRetirement(poolId, tooFar))).isLeft)
        // Within bounds
        val ok = emulator.submitSync(certTx(Certificate.PoolRetirement(poolId, epoch + 1)))
        assert(ok.isRight, s"Retirement within bounds should succeed: $ok")
        assert(emulator.certState.pstate.retiring.get(poolId).contains(epoch + 1))
    }
}
