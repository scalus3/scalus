package scalus.cardano.node

import scalus.cardano.ledger.*
import scalus.cardano.ledger.rules.{CardanoMutator, Context, STS, State}
import scalus.cardano.address.Address

class LedgerProvider(
    initialUtxos: Utxos = Map.empty,
    private var context: Context = Context.testMainnet(),
    val validators: Iterable[STS.Validator] = LedgerProvider.defaultValidators,
    val mutators: Iterable[STS.Mutator] = LedgerProvider.defaultMutators
) extends Provider {
    def submit(transaction: Transaction): Either[RuntimeException, Unit] = {
        processTransaction(transaction) match {
            case Right(newState) =>
                state = newState
                Right(())
            case Left(exception) => Left(exception)
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
                  case (None, None)         => true
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
        minRequiredAmount: Option[Coin] = None
    ): Either[RuntimeException, Utxos] = {
        if minRequiredAmount.exists(_ <= Coin(0)) then return Right(Map.empty)

        val (foundUtxos, totalAmount) = utxos.view
            .filter { case (input, output) =>
                (address == output.address) &&
                transactionId.forall(_ == input.transactionId) &&
                (
                  (datum, output.datumOption) match
                      case (Some(d1), Some(d2)) => d1.contentEquals(d2)
                      case (None, None)         => true
                      case _                    => false
                ) &&
                minAmount.forall(amount => output.value.coin >= amount)
            }
            .foldLeft((Map.empty[TransactionInput, TransactionOutput], Coin(0))) {
                case (acc @ (accUtxos, accAmount), (input, output)) =>
                    if minRequiredAmount.exists(accAmount >= _) then acc
                    else
                        (
                          accUtxos + (input -> output),
                          Coin(accAmount.value + output.value.coin.value)
                        )
            }

        if foundUtxos.nonEmpty && minRequiredAmount.forall(totalAmount >= _) then Right(foundUtxos)
        else
            Left(
              new RuntimeException(
                s"Utxos not found for address: $address, transactionId: $transactionId, datum: $datum, minAmount: $minAmount, minRequiredAmount: $minRequiredAmount"
              )
            )
    }

    override def setSlot(slot: SlotNo): Unit = {
        context = Context(
          fee = context.fee,
          env = context.env.copy(slot = slot),
          slotConfig = context.slotConfig
        )
    }

    def snapshot(): LedgerProvider = LedgerProvider(
      initialUtxos = this.utxos,
      context = this.context,
      validators = this.validators,
      mutators = this.mutators
    )

    private var state: State = State(initialUtxos)

    private def utxos: Utxos = state.utxos

    private def processTransaction(
        transaction: Transaction
    ): Either[TransactionException, State] = {
        STS.Mutator.transit(validators, mutators, context, state, transaction)
    }
}

object LedgerProvider {
    val defaultValidators: Set[STS.Validator] = CardanoMutator.allValidators.values.toSet
    val defaultMutators: Set[STS.Mutator] = CardanoMutator.allMutators.values.toSet
}
