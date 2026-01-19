package scalus.cardano.node

import scalus.builtin.ByteString
import scalus.cardano.address.Address
import scalus.cardano.ledger.rules.{CardanoMutator, Context, PlutusScriptsTransactionMutator, STS, State}
import scalus.cardano.ledger.*
import scalus.cardano.node.SubmitError.NodeError

import java.util.concurrent.atomic.AtomicReference
import scala.annotation.tailrec
import scala.concurrent.{ExecutionContext, Future}

/** An in-memory bare-bones node implementation.
  *
  * Allows submitting transaction and querying UTxO state. Runs [[validators]] and [[mutators]]
  * against all submitted transactions. The default validator and mutator lists reflect the Cardano
  * Node UTxO related ledger rules.
  *
  * @see
  *   [[scalus.cardano.ledger.rules]] for the ledger rules
  */
class Emulator(
    initialUtxos: Utxos = Map.empty,
    initialContext: Context = Context.testMainnet(),
    val validators: Iterable[STS.Validator] = Emulator.defaultValidators,
    val mutators: Iterable[STS.Mutator] = Emulator.defaultMutators
) extends Provider {
    private val stateRef = new AtomicReference[State](State(initialUtxos))
    private val contextRef = new AtomicReference[Context](initialContext)

    @tailrec
    private final def submitSync(transaction: Transaction): Either[SubmitError, TransactionHash] = {
        val currentState = stateRef.get()
        val currentContext = contextRef.get()

        processTransaction(currentContext, currentState, transaction) match {
            case Right(newState) =>
                if stateRef.compareAndSet(currentState, newState) then Right(transaction.id)
                else submitSync(transaction)
            case Left(t: TransactionException) =>
                Left(NodeError(s"Ledger rule violation: ${t.explain}", Some(t)))
        }
    }

    def fetchLatestParams(using ExecutionContext): Future[ProtocolParams] = {
        val params = contextRef.get().env.params
        Future.successful(params)
    }

    def submit(transaction: Transaction)(using
        ExecutionContext
    ): Future[Either[SubmitError, TransactionHash]] =
        Future.successful(submitSync(transaction))

    def findUtxo(input: TransactionInput)(using
        ExecutionContext
    ): Future[Either[RuntimeException, Utxo]] = {
        val result = utxos.get(input) match {
            case Some(output) => Right(Utxo(input, output))
            case None         => Left(new RuntimeException(s"Utxo not found for input: $input"))
        }
        Future.successful(result)
    }

    def findUtxos(inputs: Set[TransactionInput])(using
        ExecutionContext
    ): Future[Either[RuntimeException, Utxos]] = {
        val foundUtxos = inputs.view.flatMap { input =>
            utxos.get(input).map(output => input -> output)
        }.toMap

        val result =
            if foundUtxos.size == inputs.size then Right(foundUtxos)
            else
                val missingInputs = inputs.filterNot(foundUtxos.contains)
                Left(new RuntimeException(s"Utxos not found for inputs: $missingInputs"))

        Future.successful(result)
    }

    def findUtxo(
        address: Address,
        transactionId: Option[TransactionHash] = None,
        datum: Option[DatumOption] = None,
        minAmount: Option[Coin] = None
    )(using ExecutionContext): Future[Either[RuntimeException, Utxo]] = {
        val result = utxos.find { case (input, output) =>
            (address == output.address) &&
            transactionId.forall(_ == input.transactionId) &&
            (
              (datum, output.datumOption) match
                  case (Some(d1), Some(d2)) => d1.contentEquals(d2)
                  case (None, _)            => true
                  case _                    => false
            ) &&
            minAmount.forall(amount => output.value.coin >= amount)
        } match
            case Some(utxo) => Right(Utxo(utxo))
            case None =>
                Left(
                  new RuntimeException(
                    s"Utxo not found for address: $address, transactionId: $transactionId, datum: $datum, minAmount: $minAmount"
                  )
                )
        Future.successful(result)
    }

    def findUtxos(
        address: Address,
        transactionId: Option[TransactionHash] = None,
        datum: Option[DatumOption] = None,
        minAmount: Option[Coin] = None,
        minRequiredTotalAmount: Option[Coin] = None
    )(using ExecutionContext): Future[Either[RuntimeException, Utxos]] = {
        if minRequiredTotalAmount.exists(_ <= Coin(0)) then
            return Future.successful(Right(Map.empty))

        val (foundUtxos, totalAmount) = utxos.view
            .filter { case (input, output) =>
                (address == output.address) &&
                transactionId.forall(_ == input.transactionId) &&
                (
                  (datum, output.datumOption) match
                      case (Some(d1), Some(d2)) => d1.contentEquals(d2)
                      case (None, _)            => true
                      case _                    => false
                ) &&
                minAmount.forall(amount => output.value.coin >= amount)
            }
            .foldLeft((Map.empty[TransactionInput, TransactionOutput], Coin(0))) {
                case (acc @ (accUtxos, accAmount), (input, output)) =>
                    if minRequiredTotalAmount.exists(accAmount >= _) then acc
                    else
                        (
                          accUtxos + (input -> output),
                          Coin(accAmount.value + output.value.coin.value)
                        )
            }

        val result =
            if foundUtxos.nonEmpty && minRequiredTotalAmount.forall(totalAmount >= _) then
                Right(foundUtxos)
            else
                Left(
                  new RuntimeException(
                    s"Utxos not found for address: $address, transactionId: $transactionId, datum: $datum, minAmount: $minAmount, minRequiredTotalAmount: $minRequiredTotalAmount"
                  )
                )
        Future.successful(result)
    }

    @tailrec
    final def setSlot(slot: SlotNo): Unit = {
        val currentContext = contextRef.get()
        val newContext = Context(
          fee = currentContext.fee,
          env = currentContext.env.copy(slot = slot),
          slotConfig = currentContext.slotConfig
        )
        if !contextRef.compareAndSet(currentContext, newContext) then setSlot(slot)
    }

    def snapshot(): Emulator = Emulator(
      initialUtxos = this.utxos,
      initialContext = this.contextRef.get(),
      validators = this.validators,
      mutators = this.mutators
    )

    def utxos: Utxos = stateRef.get().utxos

    private def processTransaction(
        context: Context,
        state: State,
        transaction: Transaction
    ): Either[TransactionException, State] = {
        STS.Mutator.transit(validators, mutators, context, state, transaction)
    }
}

object Emulator {
    val defaultValidators: Set[STS.Validator] = CardanoMutator.allValidators.values.toSet
    val defaultMutators: Set[STS.Mutator] = CardanoMutator.allMutators.values.toSet

    /** Creates an Emulator with the specified addresses, each with the given initial value.
      *
      * @param addresses
      *   The addresses to initialize with funds
      * @param initialValue
      *   Initial value per address (default: 10,000 ADA like Yaci Devkit)
      * @return
      *   An Emulator instance with the addresses funded
      */
    def withAddresses(
        addresses: Seq[Address],
        initialValue: Value = Value.ada(10_000L)
    ): Emulator = {
        val genesisHash = TransactionHash.fromByteString(ByteString.fromHex("0" * 64))
        val initialUtxos = addresses.zipWithIndex.map { case (address, index) =>
            Input(genesisHash, index) -> Output(address, initialValue)
        }.toMap
        Emulator(
          initialUtxos = initialUtxos,
          initialContext = Context.testMainnet(),
          mutators = Set(PlutusScriptsTransactionMutator)
        )
    }
}
