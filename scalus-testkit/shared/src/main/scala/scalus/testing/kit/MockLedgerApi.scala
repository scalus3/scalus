package scalus.testing.kit

import scalus.cardano.address.Address
import scalus.cardano.ledger.rules.{CardanoMutator, Context, STS, State}
import scalus.cardano.ledger.*
import scalus.cardano.node.Provider

class MockLedgerApi(
    initialUtxos: Utxos = Map.empty,
    private var context: Context = Context.testMainnet(),
    val validators: Iterable[STS.Validator] = MockLedgerApi.defaultValidators,
    val mutators: Iterable[STS.Mutator] = MockLedgerApi.defaultMutators
) extends Provider {
    def submit(transaction: Transaction): Either[SubmitError, Unit] = {
        processTransaction(transaction) match {
            case Right(newState) =>
                state = newState
                Right(())
            case Left(t: TransactionException) => Left(t.explain)
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

    def setSlot(slot: SlotNo): Unit = {
        context = Context(
          fee = context.fee,
          env = context.env.copy(slot = slot),
          slotConfig = context.slotConfig
        )
    }

    def snapshot(): MockLedgerApi = MockLedgerApi(
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

object MockLedgerApi {
    val defaultValidators: Set[STS.Validator] = CardanoMutator.allValidators.values.toSet
    val defaultMutators: Set[STS.Mutator] = CardanoMutator.allMutators.values.toSet
}
