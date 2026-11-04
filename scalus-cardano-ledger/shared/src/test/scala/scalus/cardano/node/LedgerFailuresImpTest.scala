package scalus.cardano.node

import org.scalatest.funsuite.AnyFunSuite
import scalus.cardano.address.{Address, Network}
import scalus.cardano.ledger.*
import scalus.cardano.node.GovImp.*
import scalus.cardano.node.emulator.{ChainConfig, DropReason, TxStatus}
import scalus.cardano.txbuilder.TransactionBuilderStep.{SubmitProposal, SubmitVotingProcedure}
import scalus.cardano.txbuilder.{TransactionSigner, TwoArgumentPlutusScriptWitness, TxBuilder}
import scalus.testing.kit.Party
import scalus.testing.kit.Party.*
import scalus.uplc.builtin.{ByteString, Data}
import scalus.uplc.{PlutusV3, Term}

import java.time.Instant

/** The failure lists of the Imp tests of cardano-ledger, ported to `Emulator.ledgerFailures`,
  * ledger-rules spec section 9.3 and phase 6. Imp's `submitFailingTx` compares the whole list, in
  * order (eras/shelley/impl/testlib/Test/Cardano/Ledger/Shelley/ImpTest.hs:1306-1316), so each test
  * compares the ledger names of `ledgerFailures`, spec [FL-6], with the list of its Imp test.
  *
  * Imp fixes up a tx before it submits it: fee, balance, collateral, script integrity hash and
  * witnesses. The tests here build the tx with `TxBuilder`, which does the same, and redo the hash
  * and the witnesses after the change that the Imp test makes.
  */
class LedgerFailuresImpTest extends AnyFunSuite {

    private given env: CardanoInfo = CardanoInfo.mainnet

    private val genesis = TransactionHash.fromByteString(ByteString.fromHex("0" * 64))
    private val alice = Alice.address(Network.Mainnet)

    private val alwaysOk: Script.PlutusV3 = PlutusV3.alwaysOk.script
    private val alwaysFails: Script.PlutusV3 = Script.PlutusV3(Term.Error().plutusV3.cborByteString)

    private def scriptAddress(script: Script): Address =
        Address(Network.Mainnet, Credential.ScriptHash(script.scriptHash))

    private val scriptInput = Input(genesis, 10)

    /** An Emulator with Alice's UTxOs and one UTxO locked by `script`, `produceScript` of Imp. */
    private def emulator(
        script: Script,
        adjust: ProtocolParams => ProtocolParams = identity,
        config: ChainConfig = ChainConfig.instant,
        seed: EmulatorInitialState => EmulatorInitialState = identity
    ): Emulator = {
        val utxos: Utxos =
            (0 until 10).map(i => Input(genesis, i) -> Output(alice, Value.ada(1000))).toMap +
                (scriptInput -> Output(
                  scriptAddress(script),
                  Value.ada(10),
                  DatumOption.Inline(Data.unit)
                ))
        val context = rules.Context.testMainnet()
        Emulator.withState(
          seed(EmulatorInitialState.empty.withUtxos(utxos)),
          context.copy(env = context.env.copy(params = adjust(context.env.params))),
          config
        )
    }

    private def names(e: EmulatorBase, tx: Transaction): Seq[String] =
        e.ledgerFailures(tx).flatMap(TransactionException.ledgerNames)

    private def aliceUtxos(e: EmulatorBase): Utxos = e.utxos.filter(_._2.address == alice)

    /** A builder that does not run the scripts, so it can build a tx whose scripts fail. */
    private def unevaluated: TxBuilder =
        TxBuilder(env, PlutusScriptEvaluator.const(ExUnits(100_000, 50_000_000)))

    /** A tx that spends the script UTxO of [[emulator]] with redeemer `()`, signed by Alice. */
    private def spendScript(
        e: EmulatorBase,
        script: PlutusScript,
        builder: TxBuilder = TxBuilder(env)
    ): Transaction =
        builder
            .spend(Utxo(scriptInput, e.utxos(scriptInput)), Data.unit, script)
            .complete(aliceUtxos(e), alice)
            .sign(Alice.signer)
            .transaction

    /** `fixupPPHash` (Alonzo ImpTest): the script integrity hash under the pparams of `e`. */
    private def fixPPHash(e: EmulatorBase, tx: Transaction): Transaction = {
        val params = EmulatorBase.stateOf(e).context.env.params
        val hash = ScriptDataHashGenerator
            .computeScriptDataHash(tx, e.utxos, params)
            .getOrElse(fail("the hash needs the inputs"))
        tx.withBody(_.copy(scriptDataHash = hash))
    }

    /** `resetAddrWits` (Alonzo ImpTest): Alice's witness for the tx as it is now. */
    private def resign(tx: Transaction): Transaction =
        Alice.signer.sign(tx.withWitness(_.copy(vkeyWitnesses = TaggedSortedSet.empty)))

    private def withRedeemers(tx: Transaction, redeemers: Seq[Redeemer]): Transaction =
        tx.withWitness(
          _.copy(redeemers =
              if redeemers.isEmpty then None
              else Some(KeepRaw(Redeemers.Array(redeemers.toIndexedSeq)))
          )
        )

    private def redeemersOf(tx: Transaction): Seq[Redeemer] =
        tx.witnessSet.redeemers.toSeq.flatMap(_.value.toSeq)

    /** Takes `extra` lovelace from Alice's change output for the fee, for a tx that the test made
      * bigger after `TxBuilder` set its fee; Imp's fixup sets the fee after the change.
      */
    private def payMoreFee(tx: Transaction, extra: Long): Transaction =
        tx.withBody { body =>
            val change = body.outputs.lastIndexWhere(_.value.address == alice)
            val out = body.outputs(change).value
            body.copy(
              fee = body.fee + Coin(extra),
              outputs = body.outputs.updated(
                change,
                Sized(Output(out.address, out.value - Value.lovelace(extra), out.datumOption, None))
              )
            )
        }

    test("PV 11, as the Imp vectors of spec section 9.3") {
        assert(env.protocolParams.protocolVersion.major == 11)
        assert(
          EmulatorBase.stateOf(emulator(alwaysOk)).context.env.params.protocolVersion.major == 11
        )
    }

    // ------------------------------------------------------------------ the five Imp lists

    test("Alonzo UTXOW MissingRedeemers: MissingRedeemers, CollectErrors [NoRedeemer]") {
        // eras/alonzo/impl/testlib/Test/Cardano/Ledger/Alonzo/Imp/UtxowSpec/Invalid.hs:72-84
        val e = emulator(alwaysOk)
        val tx = resign(fixPPHash(e, withRedeemers(spendScript(e, alwaysOk), Nil)))
        assert(names(e, tx) == Seq("MissingRedeemers", "CollectErrors"))
        assert(
          e.ledgerFailures(tx)(1) == TransactionException.CollectErrorsException(
            tx.id,
            Seq(CollectError.NoRedeemer(RedeemerTag.Spend, 0))
          )
        )
    }

    test(
      "Alonzo UTXOW Redeemer with incorrect purpose: ExtraRedeemers, MissingRedeemers, CollectErrors"
    ) {
        // eras/alonzo/impl/testlib/Test/Cardano/Ledger/Alonzo/Imp/UtxowSpec/Invalid.hs:166-186
        val e = emulator(alwaysOk)
        val minting = Redeemer(RedeemerTag.Mint, 0, Data.I(32), ExUnits(0, 0))
        val tx = resign(
          fixPPHash(e, payMoreFee(withRedeemers(spendScript(e, alwaysOk), Seq(minting)), 100_000))
        )
        assert(names(e, tx) == Seq("ExtraRedeemers", "MissingRedeemers", "CollectErrors"))
    }

    test(
      "Babbage UTXOW ExtraRedeemers/RedeemerPointerPointsToNothing: ExtraRedeemers, CollectErrors [BadTranslation]"
    ) {
        // eras/babbage/impl/testlib/Test/Cardano/Ledger/Babbage/Imp/UtxowSpec/Invalid.hs:94-107
        val e = emulator(alwaysOk)
        val spend = spendScript(e, alwaysOk)
        val nothing = Redeemer(RedeemerTag.Mint, 2, Data.I(1), ExUnits(0, 0))
        val tx = resign(
          fixPPHash(e, payMoreFee(withRedeemers(spend, redeemersOf(spend) :+ nothing), 100_000))
        )
        assert(names(e, tx) == Seq("ExtraRedeemers", "CollectErrors"))
        // BadTranslation (RedeemerPointerPointsToNothing ptr), B/TxInfo.hs:201, spec [FL-10]
        e.ledgerFailures(tx)(1) match
            case TransactionException.CollectErrorsException(
                  _,
                  Seq(CollectError.BadTranslation(message))
                ) if message.startsWith("RedeemerPointerPointsToNothing: ") =>
            case other => fail(s"expected one RedeemerPointerPointsToNothing, got $other")
    }

    test(
      "Conway CERTS Withdrawing from an unregistered staking address: ConwayWithdrawalsMissingAccounts, ConwayWdrlNotDelegatedToDRep"
    ) {
        // eras/conway/impl/testlib/Test/Cardano/Ledger/Conway/Imp/CertsSpec.hs:33-90. Group (6) of
        // spec [FL-7] is reversed: validateWithdrawalsDelegated runs before
        // testIncompleteAndMissingWithdrawals (C/Rules/Ledger.hs:410, 416).
        val g = new GovImp(accounts = Seq(Sybil))
        val expected = Seq("ConwayWithdrawalsMissingAccounts", "ConwayWdrlNotDelegatedToDRep")
        def withdrawing(withdrawals: (Party, Coin)*): Transaction = {
            val builder = withdrawals.foldLeft(TxBuilder(g.env)) { case (b, (p, amount)) =>
                b.withdrawRewards(stakeAddress(p), amount)
            }
            builder
                .complete(aliceUtxos(g.e), alice)
                .sign(
                  new TransactionSigner(
                    Set(Alice.account.paymentKeyPair) ++ withdrawals.map(_._1.account.stakeKeyPair)
                  )
                )
                .transaction
        }
        assert(names(g.e, withdrawing(Peggy -> Coin(20))) == expected)
        // a registered account that delegates to AlwaysNoConfidence, and a zero withdrawal from
        // the unregistered one
        g.e.addRewards(stakeCredential(Sybil), Coin(1_000_000))
        g.submit(
          TxBuilder(g.env).delegateVoteToDRep(stakeAddress(Sybil), DRep.AlwaysNoConfidence),
          Sybil
        )
        assert(names(g.e, withdrawing(Peggy -> Coin.zero, Sybil -> Coin(1_000_000))) == expected)
    }

    test("Conway GOV VotersDoNotExist: UnelectedCommitteeVoters, VotersDoNotExist") {
        // eras/conway/impl/testlib/Test/Cardano/Ledger/Conway/Imp/GovSpec.hs:746-760
        val g = new GovImp()
        val id = g.propose(GovAction.HardForkInitiation(None, ProtocolVersion(11, 1)))
        val tx = TxBuilder(g.env)
            .addSteps(
              SubmitVotingProcedure(
                Voter.ConstitutionalCommitteeHotKey(Peggy.addrKeyHash),
                Map(id -> VotingProcedure(Vote.Yes, None))
              )
            )
            .complete(aliceUtxos(g.e), alice)
            .sign(
              new TransactionSigner(Set(Alice.account.paymentKeyPair, Peggy.account.paymentKeyPair))
            )
            .transaction
        assert(names(g.e, tx) == Seq("UnelectedCommitteeVoters", "VotersDoNotExist"))
    }

    // ------------------------------------------------------------------ single-failure lists

    test("Alonzo UTXOS Valid transaction marked as invalid: ValidationTagMismatch, spec [FL-9]") {
        // eras/alonzo/impl/testlib/Test/Cardano/Ledger/Alonzo/Imp/UtxosSpec.hs:174-176
        val e = emulator(alwaysOk)
        val payment = TxBuilder(env)
            .payTo(Bob.address(Network.Mainnet), Value.ada(5))
            .complete(aliceUtxos(e), alice)
            .sign(Alice.signer)
            .transaction
        val tx = payment.copy(isValid = false)
        assert(e.ledgerFailures(tx) == Seq(TransactionException.PassedUnexpectedlyException(tx.id)))
        assert(names(e, tx) == Seq("ValidationTagMismatch"))
        assert(e.submitSync(tx).left.map(_.rule) == Left("PassedUnexpectedly"))
    }

    test("Alonzo UTXOS No cost model: CollectErrors [NoCostModel PlutusV3], spec [FL-10]") {
        // eras/alonzo/impl/testlib/Test/Cardano/Ledger/Alonzo/Imp/UtxosSpec.hs:202-208
        val e = emulator(alwaysOk, _.copy(costModels = CostModels(Map.empty)))
        val tx = resign(fixPPHash(e, spendScript(e, alwaysOk, unevaluated)))
        assert(
          e.ledgerFailures(tx) == Seq(
            TransactionException.CollectErrorsException(
              tx.id,
              Seq(CollectError.NoCostModel(Language.PlutusV3))
            )
          )
        )
        assert(e.submitSync(tx).left.map(_.rule) == Left("CollectErrors"))
    }

    /** `testingCostModels [PlutusV1 .. PlutusV2]` of the Conway UtxosSpec: no PlutusV3 entry, so a
      * lookup with a wrong key finds a model.
      */
    private def onlyV1V2(params: ProtocolParams): ProtocolParams = params.copy(costModels =
        CostModels(
          params.costModels.models.filter((language, _) => language < Language.PlutusV3.ordinal)
        )
    )

    test("Conway UTXOS Minting token fails: CollectErrors [NoCostModel PlutusV3], spec [FL-10]") {
        // eras/conway/impl/testlib/Test/Cardano/Ledger/Conway/Imp/UtxosSpec.hs:536-548
        val e = emulator(alwaysOk, onlyV1V2)
        val minting = unevaluated
            .mint(alwaysOk, Map(AssetName.empty -> 1L), Data.unit)
            .complete(aliceUtxos(e), alice)
            .sign(Alice.signer)
            .transaction
        val tx = resign(fixPPHash(e, minting))
        assert(
          e.ledgerFailures(tx) == Seq(
            TransactionException.CollectErrorsException(
              tx.id,
              Seq(CollectError.NoCostModel(Language.PlutusV3))
            )
          )
        )
    }

    test(
      "Conway UTXOS ParameterChange with a PlutusV3 guardrails script: CollectErrors [NoCostModel PlutusV3], spec [FL-10]"
    ) {
        // eras/conway/impl/testlib/Test/Cardano/Ledger/Conway/Imp/UtxosSpec.hs:516-534: the
        // constitution names a PlutusV3 policy, so the proposal runs it with purpose Proposing
        val returnTo = Alice.account.stakeAddress(Network.Mainnet)
        // a deposit that Alice's UTxOs pay
        val deposit = 100_000_000L
        val e = emulator(
          alwaysOk,
          params => onlyV1V2(params).copy(govActionDeposit = deposit),
          seed = _.withStakeRegistrations(Seq(EmulatorStakeRegistration.of(returnTo.credential)))
              .withConstitution(Constitution(anchor, Some(alwaysOk.scriptHash)))
        )
        val v3 = env.protocolParams.costModels.models.filter(_._1 == Language.PlutusV3.ordinal)
        val proposal = ProposalProcedure(
          Coin(deposit),
          RewardAccount(returnTo),
          GovAction.ParameterChange(
            None,
            ProtocolParamUpdate(costModels = Some(CostModels(v3))),
            Some(alwaysOk.scriptHash)
          ),
          anchor
        )
        val proposing = unevaluated
            .addSteps(
              SubmitProposal(proposal, TwoArgumentPlutusScriptWitness.attached(alwaysOk, Data.unit))
            )
            .complete(aliceUtxos(e), alice)
            .sign(Alice.signer)
            .transaction
        val tx = resign(fixPPHash(e, proposing))
        assert(
          e.ledgerFailures(tx) == Seq(
            TransactionException.CollectErrorsException(
              tx.id,
              Seq(CollectError.NoCostModel(Language.PlutusV3))
            )
          )
        )
    }

    test(
      "Conway UTXOS ReferenceInputsNotDisjointFromInputs: CollectErrors [BadTranslation], spec [TX-1], [FL-10]"
    ) {
        // eras/conway/impl/testlib/Test/Cardano/Ledger/Conway/Imp/UtxosSpec.hs:110-129
        val e = emulator(alwaysOk)
        // TxBuilder refuses to build the overlap, so the reference input comes after it
        val spend = spendScript(e, alwaysOk)
        val tx = resign(
          payMoreFee(spend, 100_000).withBody(
            _.copy(referenceInputs = TaggedSortedSet(scriptInput))
          )
        )
        e.ledgerFailures(tx) match
            case Seq(
                  TransactionException.CollectErrorsException(
                    _,
                    Seq(CollectError.BadTranslation(m))
                  )
                ) =>
                assert(m.contains("ReferenceInputsNotDisjointFromInputs"), m)
                assert(m.contains(scriptInput.toString), m)
            case other => fail(s"expected one BadTranslation, got $other")
        assert(e.submitSync(tx).left.map(_.rule) == Left("CollectErrors"))
    }

    // ------------------------------------------------------------------ rules of section 9.3

    test("spec [FL-5]: the scripts are not evaluated after an earlier failure") {
        val e = emulator(alwaysFails)
        e.setSlot(1000)
        def failing(validTo: SlotNo) =
            unevaluated
                .spend(Utxo(scriptInput, e.utxos(scriptInput)), Data.unit, alwaysFails)
                .validTo(Instant.ofEpochMilli(env.slotConfig.slotToTime(validTo)))
                .complete(aliceUtxos(e), alice)
                .sign(Alice.signer)
                .transaction
        // the script fails when nothing else does
        assert(names(e, failing(2000)) == Seq("ValidationTagMismatch"))
        // after an expired validity interval the script is not run
        val expired = failing(500)
        assert(names(e, expired) == Seq("OutsideValidityIntervalUTxO"))
        // a collect error is reported all the same, outside whenFailureFree
        val noRedeemer = resign(fixPPHash(e, withRedeemers(expired, Nil)))
        assert(
          names(e, noRedeemer) ==
              Seq("MissingRedeemers", "CollectErrors", "OutsideValidityIntervalUTxO")
        )
    }

    test("spec [FL-5]: the scripts are not evaluated after a GOV failure") {
        // Conway LEDGER runs GOV before UTXOW (eras/conway/impl/src/Cardano/Ledger/Conway/Rules/
        // Ledger.hs:440, 459), and no validator checks the voters, so only GovMutator fails here
        val e = emulator(alwaysFails)
        val tx = unevaluated
            .spend(Utxo(scriptInput, e.utxos(scriptInput)), Data.unit, alwaysFails)
            .addSteps(
              SubmitVotingProcedure(
                Voter.DRepKey(Peggy.addrKeyHash),
                Map(GovActionId(genesis, 0) -> VotingProcedure(Vote.Yes, None))
              )
            )
            .complete(aliceUtxos(e), alice)
            .sign(
              new TransactionSigner(Set(Alice.account.paymentKeyPair, Peggy.account.paymentKeyPair))
            )
            .transaction
        // the votes of an unknown voter are dropped before the action check (Gov.hs:584-613), so
        // the unknown action gives no GovActionsDoNotExist
        assert(names(e, tx) == Seq("VotersDoNotExist"))
    }

    test("spec [FL-3]: submit and mempool revalidation keep their first failure") {
        val e = emulator(alwaysOk, config = ChainConfig.manual)
        e.setSlot(100)
        val payTo = Bob.address(Network.Mainnet)
        def paymentValidTo(slot: SlotNo, amount: Long) =
            TxBuilder(env)
                .payTo(payTo, Value.ada(amount))
                .validTo(Instant.ofEpochMilli(env.slotConfig.slotToTime(slot)))
                .complete(aliceUtxos(e).filter(_._1 == Input(genesis, 0)), alice)
                .sign(Alice.signer)
                .transaction
        val pending = paymentValidTo(150, 5)
        assert(e.submitSync(pending) == Right(pending.id))
        // a rival block at slot 200 spends the same input: both rules fail at revalidation
        val rival = paymentValidTo(300, 6)
        assert(e.produceBlock(e.tip.point, 200, Seq(rival)).isRight)
        assert(e.txStatus(pending.id) == TxStatus.Dropped(DropReason.InputsSpent))
        assert(e.submitSync(pending).left.map(_.rule) == Left("UtxoNotAvailable"))
        // the list has both, BadInputsUTxO first by [FL-7] (3)
        assert(names(e, pending) == Seq("BadInputsUTxO", "OutsideValidityIntervalUTxO"))
    }

    test("spec [FL-4], [FL-4a], [FL-11] (a): a missing input is one BadInputsUTxO") {
        // shelley test-suite Test/Cardano/Ledger/Shelley/UnitTests.hs:329-333 gives
        // [ValueNotConservedUTxO, BadInputsUTxO]: the ledger computes the balance on the inputs it
        // finds. Scalus skips the balance when an input is missing, known difference [FL-11] (a).
        val e = emulator(alwaysOk)
        val tx = TxBuilder(env)
            .payTo(Bob.address(Network.Mainnet), Value.ada(5))
            .complete(aliceUtxos(e), alice)
            .sign(Alice.signer)
            .transaction
        tx.body.value.inputs.toSeq.foreach(e.removeUtxo)
        assert(names(e, tx) == Seq("BadInputsUTxO"))
        // the default rules of CardanoMutator give the same list
        val state = EmulatorBase.stateOf(e)
        assert(
          rules.CardanoMutator.transitAll(state.context, state.ledger, tx) ==
              Left(e.ledgerFailures(tx))
        )
        // spec [FL-4a]: without AllInputsMustBeInUtxoValidator the dropped failure is kept
        val withoutInputsCheck =
            e.validators.filterNot(_ == rules.AllInputsMustBeInUtxoValidator)
        assert(
          state
              .ledgerFailures(tx, withoutInputsCheck, e.mutators)
              .map(TransactionException.ledgerNames) == Seq(Seq("BadInputsUTxO"))
        )
    }

    test("spec [FL-8]: ledgerFailures is empty for a valid tx and does not change the state") {
        val e = emulator(alwaysOk)
        val tx = spendScript(e, alwaysOk)
        val before = EmulatorBase.stateOf(e)
        assert(e.ledgerFailures(tx).isEmpty)
        assert(EmulatorBase.stateOf(e) eq before)
        assert(e.submitSync(tx) == Right(tx.id))
    }
}
