package scalus.cardano.node

import scalus.cardano.address.Address
import scalus.cardano.ledger.rules.{CardanoMutator, Context, STS, State}
import scalus.cardano.ledger.*
import scalus.cardano.node.SubmitError.NodeError

import java.util.concurrent.atomic.AtomicReference
import scala.annotation.tailrec

/** An in-memory bare-bones node implementation.
  *
  * Allows submitting transaction and querying UTxO state. Runs [[validators]] and [[mutators]]
  * against all submitted transactions. The default validator and mutator lists reflect the Cardano
  * Node UTxO related ledger rules.
  *
  * @see
  *   [[scalus.cardano.ledger.rules]] for the ledger rules
  */
class NodeEmulator(
    initialUtxos: Utxos = Map.empty,
    initialContext: Context = Context.testMainnet(),
    val validators: Iterable[STS.Validator] = NodeEmulator.defaultValidators,
    val mutators: Iterable[STS.Mutator] = NodeEmulator.defaultMutators
) extends Provider {
    private val stateRef = new AtomicReference[State](State(initialUtxos))
    private val contextRef = new AtomicReference[Context](initialContext)

    @tailrec
    final def submit(transaction: Transaction): Either[SubmitError, TransactionHash] = {
        val currentState = stateRef.get()
        val currentContext = contextRef.get()

        processTransaction(currentContext, currentState, transaction) match {
            case Right(newState) =>
                if stateRef.compareAndSet(currentState, newState) then Right(transaction.id)
                else submit(transaction)
            case Left(t: TransactionException) =>
                Left(NodeError(s"Ledger rule violation: ${t.explain}", Some(t)))
        }
    }

    def findUtxo(input: TransactionInput): Either[RuntimeException, Utxo] = {
        utxos.get(input) match {
            case Some(output) => Right(Utxo(input, output))
            case None         => Left(new RuntimeException(s"Utxo not found for input: $input"))
        }
    }

    def findUtxos(inputs: Set[TransactionInput]): Either[RuntimeException, Utxos] = {
        val foundUtxos = inputs.view.flatMap { input =>
            utxos.get(input).map(output => input -> output)
        }.toMap

        if foundUtxos.size == inputs.size then Right(foundUtxos)
        else
            val missingInputs = inputs.filterNot(foundUtxos.contains)
            Left(new RuntimeException(s"Utxos not found for inputs: $missingInputs"))
    }

    def findUtxo(
        address: Address,
        transactionId: Option[TransactionHash] = None,
        datum: Option[DatumOption] = None,
        minAmount: Option[Coin] = None
    ): Either[RuntimeException, Utxo] = {
        utxos.find { case (input, output) =>
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
    }

    def findUtxos(
        address: Address,
        transactionId: Option[TransactionHash] = None,
        datum: Option[DatumOption] = None,
        minAmount: Option[Coin] = None,
        minRequiredTotalAmount: Option[Coin] = None
    ): Either[RuntimeException, Utxos] = {
        if minRequiredTotalAmount.exists(_ <= Coin(0)) then return Right(Map.empty)

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

        if foundUtxos.nonEmpty && minRequiredTotalAmount.forall(totalAmount >= _) then
            Right(foundUtxos)
        else
            Left(
              new RuntimeException(
                s"Utxos not found for address: $address, transactionId: $transactionId, datum: $datum, minAmount: $minAmount, minRequiredTotalAmount: $minRequiredTotalAmount"
              )
            )
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

    def snapshot(): NodeEmulator = NodeEmulator(
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

object NodeEmulator {
    val defaultValidators: Set[STS.Validator] = CardanoMutator.allValidators.values.toSet
    val defaultMutators: Set[STS.Mutator] = CardanoMutator.allMutators.values.toSet
}
