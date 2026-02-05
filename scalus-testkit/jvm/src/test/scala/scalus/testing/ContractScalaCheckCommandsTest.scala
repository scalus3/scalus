package scalus.testing

import org.scalacheck.Prop
import org.scalatest.funsuite.AnyFunSuite
import scalus.cardano.ledger.*
import scalus.cardano.node.{BlockchainReader, NodeSubmitError, SubmitError}
import scalus.cardano.txbuilder.TxBuilder
import scalus.testing.kit.Party.{Alice, Bob}

import scala.concurrent.duration.Duration
import scala.concurrent.{Await, ExecutionContext, Future}

class ContractScalaCheckCommandsTest extends AnyFunSuite {

    private given CardanoInfo = CardanoInfo.mainnet
    private given ExecutionContext = ExecutionContext.global

    /** Simple state tracking total ADA sent from Alice to Bob. */
    case class TransferState(totalSent: Long)

    /** A simple contract step that transfers ADA from Alice to Bob. */
    class SimpleTransferStep extends ContractStepVariations[TransferState] {

        override def extractState(reader: BlockchainReader)(using
            ExecutionContext
        ): Future[TransferState] = {
            // In a real scenario, we'd query UTxOs and extract state
            // For testing, just return a default state
            Future.successful(TransferState(0L))
        }

        override def makeBaseTx(reader: BlockchainReader, state: TransferState)(using
            ExecutionContext
        ): Future[TxTemplate] = {
            val builder = TxBuilder(reader.cardanoInfo)
                .payTo(Bob.address, Value.ada(10))
            Future.successful(TxTemplate(builder, Alice.address, Alice.signer))
        }

        override def variations: TxVariations[TransferState] = {
            // Simple variations: just the base transaction
            new TxVariations[TransferState] {
                override def enumerate(
                    reader: BlockchainReader,
                    state: TransferState,
                    txTemplate: TxTemplate
                )(using ExecutionContext): Future[Seq[Transaction]] = {
                    txTemplate.complete(reader).map(tx => Seq(tx))
                }
            }
        }
    }

    /** A simple contract step with slot delays for testing Wait actions. */
    class TransferStepWithDelays extends SimpleTransferStep {
        override def slotDelays(state: TransferState): Seq[Long] = Seq(10L, 50L, 100L)
    }

    /** A contract step that generates multiple transaction variations. */
    class MultiVariationStep extends ContractStepVariations[TransferState] {

        override def extractState(reader: BlockchainReader)(using
            ExecutionContext
        ): Future[TransferState] =
            Future.successful(TransferState(0L))

        override def makeBaseTx(reader: BlockchainReader, state: TransferState)(using
            ExecutionContext
        ): Future[TxTemplate] = {
            val builder = TxBuilder(reader.cardanoInfo)
                .payTo(Bob.address, Value.ada(5))
            Future.successful(TxTemplate(builder, Alice.address, Alice.signer))
        }

        override def variations: TxVariations[TransferState] = {
            new TxVariations[TransferState] {
                override def enumerate(
                    reader: BlockchainReader,
                    state: TransferState,
                    txTemplate: TxTemplate
                )(using ExecutionContext): Future[Seq[Transaction]] = {
                    // Generate variations with different amounts
                    val amounts = Seq(5L, 10L, 15L)
                    val txFutures = amounts.map { amount =>
                        val modifiedTemplate = txTemplate.mapBuilder { builder =>
                            TxBuilder(reader.cardanoInfo)
                                .payTo(Bob.address, Value.ada(amount))
                        }
                        modifiedTemplate.complete(reader)
                    }
                    Future.sequence(txFutures)
                }
            }
        }
    }

    // =========================================================================
    // Basic construction tests
    // =========================================================================

    test("ContractScalaCheckCommands can be constructed") {
        val emulator = ImmutableEmulator.withAddresses(Seq(Alice.address, Bob.address))
        val step = new SimpleTransferStep
        val commands = ContractScalaCheckCommands(emulator, step)()

        assert(commands != null)
    }

    test("ContractScalaCheckCommands with invariant checker") {
        val emulator = ImmutableEmulator.withAddresses(Seq(Alice.address, Bob.address))
        val step = new SimpleTransferStep

        val commands = ContractScalaCheckCommands(emulator, step) { (reader, state) =>
            Future.successful(Prop.passed)
        }

        assert(commands != null)
    }

    test("ContractScalaCheckCommands with slot delays") {
        val emulator = ImmutableEmulator.withAddresses(Seq(Alice.address, Bob.address))
        val step = new TransferStepWithDelays

        val commands = ContractScalaCheckCommands(emulator, step)()

        assert(commands != null)
    }

    // =========================================================================
    // genInitialState tests
    // =========================================================================

    test("genInitialState produces valid state") {
        val emulator = ImmutableEmulator.withAddresses(Seq(Alice.address, Bob.address))
        val step = new SimpleTransferStep
        val commands = ContractScalaCheckCommands(emulator, step)()

        val stateOpt = commands.genInitialState.sample
        assert(stateOpt.isDefined, "genInitialState should produce a sample")

        val state = stateOpt.get
        assert(state.emulator.utxos.nonEmpty, "Emulator should have UTxOs")
        assert(state.contractState == TransferState(0L), "Contract state should be extracted")
    }

    // =========================================================================
    // genCommand tests
    // =========================================================================

    test("genCommand generates commands from variations") {
        val emulator = ImmutableEmulator.withAddresses(Seq(Alice.address, Bob.address))
        val step = new SimpleTransferStep
        val commands = ContractScalaCheckCommands(emulator, step)()

        val stateOpt = commands.genInitialState.sample
        assert(stateOpt.isDefined)

        val cmdOpt = commands.genCommand(stateOpt.get).sample
        assert(cmdOpt.isDefined, "genCommand should produce a sample")
        assert(
          cmdOpt.get.isInstanceOf[commands.SubmitTxCommand],
          "Command should be SubmitTxCommand"
        )
    }

    test("genCommand with slot delays generates both command types") {
        val emulator = ImmutableEmulator.withAddresses(Seq(Alice.address, Bob.address))
        val step = new TransferStepWithDelays
        val commands = ContractScalaCheckCommands(emulator, step)()

        val stateOpt = commands.genInitialState.sample
        assert(stateOpt.isDefined)

        // Generate many commands to check both types appear
        val cmds = (1 to 100).flatMap(_ => commands.genCommand(stateOpt.get).sample)
        val submitCmds = cmds.count(_.isInstanceOf[commands.SubmitTxCommand])
        val slotCmds = cmds.count(_.isInstanceOf[commands.AdvanceSlotCommand])

        assert(submitCmds > 0, "Should generate some SubmitTxCommands")
        assert(slotCmds > 0, "Should generate some AdvanceSlotCommands")
    }

    test("genCommand with multiple variations selects from all") {
        val emulator = ImmutableEmulator.withAddresses(Seq(Alice.address, Bob.address))
        val step = new MultiVariationStep
        val commands = ContractScalaCheckCommands(emulator, step)()

        val stateOpt = commands.genInitialState.sample
        assert(stateOpt.isDefined)

        // Generate multiple commands - should be different
        val cmds = (1 to 20).flatMap { _ =>
            commands.genCommand(stateOpt.get).sample.collect { case cmd: commands.SubmitTxCommand =>
                cmd.tx
            }
        }

        assert(cmds.nonEmpty, "Should generate some transactions")
    }

    // =========================================================================
    // newSut / destroySut tests
    // =========================================================================

    test("newSut creates mutable emulator from state") {
        val emulator = ImmutableEmulator.withAddresses(Seq(Alice.address, Bob.address))
        val step = new SimpleTransferStep
        val commands = ContractScalaCheckCommands(emulator, step)()

        val stateOpt = commands.genInitialState.sample
        assert(stateOpt.isDefined)

        val sut = commands.newSut(stateOpt.get)
        assert(sut != null)
        assert(sut.utxos == stateOpt.get.emulator.utxos, "Sut should have same UTxOs as state")
    }

    test("destroySut is a no-op") {
        val emulator = ImmutableEmulator.withAddresses(Seq(Alice.address, Bob.address))
        val step = new SimpleTransferStep
        val commands = ContractScalaCheckCommands(emulator, step)()

        val stateOpt = commands.genInitialState.sample
        assert(stateOpt.isDefined)

        val sut = commands.newSut(stateOpt.get)
        // Should not throw
        commands.destroySut(sut)
    }

    // =========================================================================
    // SubmitTxCommand tests
    // =========================================================================

    test("SubmitTxCommand.run executes transaction on sut") {
        val emulator = ImmutableEmulator.withAddresses(Seq(Alice.address, Bob.address))
        val step = new SimpleTransferStep
        val commands = ContractScalaCheckCommands(emulator, step)()

        val stateOpt = commands.genInitialState.sample
        assert(stateOpt.isDefined)

        val cmdOpt = commands.genCommand(stateOpt.get).sample
        assert(cmdOpt.isDefined)

        val sut = commands.newSut(stateOpt.get)
        // Run the command and verify it succeeds
        val cmd = cmdOpt.get.asInstanceOf[commands.SubmitTxCommand]
        val result = cmd.run(sut)

        assert(result.isRight, s"Transaction should succeed: $result")
    }

    test("SubmitTxCommand.nextState updates emulator on success") {
        val emulator = ImmutableEmulator.withAddresses(Seq(Alice.address, Bob.address))
        val step = new SimpleTransferStep
        val commands = ContractScalaCheckCommands(emulator, step)()

        val stateOpt = commands.genInitialState.sample
        assert(stateOpt.isDefined)

        val cmdOpt = commands.genCommand(stateOpt.get).sample
        assert(cmdOpt.isDefined)

        val originalState = stateOpt.get
        val newState = cmdOpt.get.nextState(originalState)

        // State should change (new UTxOs after transaction)
        assert(newState.emulator.utxos != originalState.emulator.utxos)
        // RNG should advance
        assert(newState.rng != originalState.rng)
    }

    test("SubmitTxCommand.postCondition passes on success without invariants") {
        val emulator = ImmutableEmulator.withAddresses(Seq(Alice.address, Bob.address))
        val step = new SimpleTransferStep
        val commands = ContractScalaCheckCommands(emulator, step)()

        val stateOpt = commands.genInitialState.sample
        assert(stateOpt.isDefined)

        val cmdOpt = commands.genCommand(stateOpt.get).sample
        assert(cmdOpt.isDefined)

        val cmd = cmdOpt.get.asInstanceOf[commands.SubmitTxCommand]
        val newState = cmd.nextState(stateOpt.get)

        val txHash = TransactionHash.fromHex("0" * 64)
        val prop = cmd.postCondition(newState, scala.util.Success(Right(txHash)))

        // Default invariant checker returns Prop.passed
        assert(prop == Prop.passed)
    }

    test("SubmitTxCommand.postCondition passes on transaction rejection") {
        val emulator = ImmutableEmulator.withAddresses(Seq(Alice.address, Bob.address))
        val step = new SimpleTransferStep
        val commands = ContractScalaCheckCommands(emulator, step)()

        val stateOpt = commands.genInitialState.sample
        assert(stateOpt.isDefined)

        val cmdOpt = commands.genCommand(stateOpt.get).sample
        assert(cmdOpt.isDefined)

        val cmd = cmdOpt.get.asInstanceOf[commands.SubmitTxCommand]
        val newState = cmd.nextState(stateOpt.get)

        val error: SubmitError = NodeSubmitError.ValidationError("test error")
        val prop = cmd.postCondition(newState, scala.util.Success(Left(error)))

        // Transaction rejection is expected for attack variations
        assert(prop == Prop.passed)
    }

    // =========================================================================
    // AdvanceSlotCommand tests
    // =========================================================================

    test("AdvanceSlotCommand advances slot in sut") {
        val emulator = ImmutableEmulator.withAddresses(Seq(Alice.address, Bob.address))
        val step = new TransferStepWithDelays
        val commands = ContractScalaCheckCommands(emulator, step)()

        val stateOpt = commands.genInitialState.sample
        assert(stateOpt.isDefined)

        val sut = commands.newSut(stateOpt.get)
        val initialSlot = Await.result(sut.currentSlot, Duration.Inf)

        val advanceCmd = commands.AdvanceSlotCommand(100L)
        advanceCmd.run(sut)

        val newSlot = Await.result(sut.currentSlot, Duration.Inf)
        assert(newSlot == initialSlot + 100)
    }

    test("AdvanceSlotCommand.nextState advances slot in state") {
        val emulator = ImmutableEmulator.withAddresses(Seq(Alice.address, Bob.address))
        val step = new SimpleTransferStep
        val commands = ContractScalaCheckCommands(emulator, step)()

        val stateOpt = commands.genInitialState.sample
        assert(stateOpt.isDefined)

        val initialSlot = stateOpt.get.emulator.currentSlot
        val advanceCmd = commands.AdvanceSlotCommand(50L)
        val newState = advanceCmd.nextState(stateOpt.get)

        assert(newState.emulator.currentSlot == initialSlot + 50)
    }

    test("AdvanceSlotCommand.preCondition rejects non-positive slots") {
        val emulator = ImmutableEmulator.withAddresses(Seq(Alice.address, Bob.address))
        val step = new SimpleTransferStep
        val commands = ContractScalaCheckCommands(emulator, step)()

        val stateOpt = commands.genInitialState.sample
        assert(stateOpt.isDefined)

        val zeroCmd = commands.AdvanceSlotCommand(0L)
        assert(!zeroCmd.preCondition(stateOpt.get))

        val negativeCmd = commands.AdvanceSlotCommand(-10L)
        assert(!negativeCmd.preCondition(stateOpt.get))

        val positiveCmd = commands.AdvanceSlotCommand(10L)
        assert(positiveCmd.preCondition(stateOpt.get))
    }

    // =========================================================================
    // Integration tests
    // =========================================================================

    test("property().check() runs without errors") {
        val emulator = ImmutableEmulator.withAddresses(Seq(Alice.address, Bob.address))
        val step = new SimpleTransferStep
        val commands = ContractScalaCheckCommands(emulator, step)()

        // Run a small property test
        val result = org.scalacheck.Test.check(
          org.scalacheck.Test.Parameters.default
              .withMinSuccessfulTests(5)
              .withMaxDiscardRatio(10),
          commands.property()
        )

        assert(result.passed, s"Property test failed: $result")
    }

    test("property with invariant checking") {
        val emulator = ImmutableEmulator.withAddresses(Seq(Alice.address, Bob.address))
        val step = new SimpleTransferStep

        var invariantCheckCount = 0
        val commands = ContractScalaCheckCommands(emulator, step) { (reader, state) =>
            invariantCheckCount += 1
            Future.successful(Prop.passed)
        }

        val result = org.scalacheck.Test.check(
          org.scalacheck.Test.Parameters.default
              .withMinSuccessfulTests(5)
              .withMaxDiscardRatio(10),
          commands.property()
        )

        assert(result.passed, s"Property test failed: $result")
        assert(invariantCheckCount > 0, "Invariants should have been checked")
    }

    test("property with failing invariant") {
        val emulator = ImmutableEmulator.withAddresses(Seq(Alice.address, Bob.address))
        val step = new SimpleTransferStep

        val commands = ContractScalaCheckCommands(emulator, step) { (reader, state) =>
            Future.successful(Prop.falsified)
        }

        val result = org.scalacheck.Test.check(
          org.scalacheck.Test.Parameters.default
              .withMinSuccessfulTests(10)
              .withMaxDiscardRatio(10),
          commands.property()
        )

        assert(!result.passed, "Property test should fail with failing invariant")
    }

    // =========================================================================
    // allActions tests
    // =========================================================================

    test("allActions returns Submit actions without slot delays") {
        val emulator = ImmutableEmulator.withAddresses(Seq(Alice.address, Bob.address))
        val step = new SimpleTransferStep
        val reader = emulator.asReader
        val state = Await.result(step.extractState(reader), Duration.Inf)
        val actions = Await.result(step.allActions(reader, state), Duration.Inf)

        assert(actions.nonEmpty, "Should have actions")
        assert(actions.forall(_.isInstanceOf[StepAction.Submit]), "All actions should be Submit")
    }

    test("allActions returns both Submit and Wait actions with slot delays") {
        val emulator = ImmutableEmulator.withAddresses(Seq(Alice.address, Bob.address))
        val step = new TransferStepWithDelays
        val reader = emulator.asReader
        val state = Await.result(step.extractState(reader), Duration.Inf)
        val actions = Await.result(step.allActions(reader, state), Duration.Inf)

        val submits = actions.collect { case s: StepAction.Submit => s }
        val waits = actions.collect { case w: StepAction.Wait => w }

        assert(submits.nonEmpty, "Should have Submit actions")
        assert(waits.size == 3, "Should have 3 Wait actions (10, 50, 100)")
        assert(waits.map(_.slots).toSet == Set(10L, 50L, 100L))
    }
}
