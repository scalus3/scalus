package scalus.cardano.node

import scalus.builtin.ByteString
import scalus.cardano.address.Address
import scalus.cardano.ledger.rules.{CardanoMutator, Context, PlutusScriptsTransactionMutator, STS, State}
import scalus.cardano.ledger.*

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
                Left(SubmitError.fromException(t))
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
    ): Future[Either[UtxoQueryError, Utxo]] = {
        findUtxos(UtxoQuery(UtxoSource.FromInputs(Set(input)))).map { result =>
            result.flatMap { utxos =>
                utxos.headOption match
                    case Some((i, o)) => Right(Utxo(i, o))
                    case None => Left(UtxoQueryError.NotFound(UtxoSource.FromInputs(Set(input))))
            }
        }
    }

    def findUtxos(inputs: Set[TransactionInput])(using
        ExecutionContext
    ): Future[Either[UtxoQueryError, Utxos]] = {
        findUtxos(UtxoQuery(UtxoSource.FromInputs(inputs))).map { result =>
            result.flatMap { foundUtxos =>
                if foundUtxos.size == inputs.size then Right(foundUtxos)
                else Left(UtxoQueryError.NotFound(UtxoSource.FromInputs(inputs)))
            }
        }
    }

    def findUtxo(
        address: Address,
        transactionId: Option[TransactionHash] = None,
        datum: Option[DatumOption] = None,
        minAmount: Option[Coin] = None
    )(using ExecutionContext): Future[Either[UtxoQueryError, Utxo]] = {
        findUtxos(address, transactionId, datum, minAmount, None).map { result =>
            result.flatMap { utxos =>
                utxos.headOption match
                    case Some((i, o)) => Right(Utxo(i, o))
                    case None => Left(UtxoQueryError.NotFound(UtxoSource.FromAddress(address)))
            }
        }
    }

    def findUtxos(
        address: Address,
        transactionId: Option[TransactionHash] = None,
        datum: Option[DatumOption] = None,
        minAmount: Option[Coin] = None,
        minRequiredTotalAmount: Option[Coin] = None
    )(using ExecutionContext): Future[Either[UtxoQueryError, Utxos]] = {
        if minRequiredTotalAmount.exists(_ <= Coin(0)) then
            return Future.successful(Right(Map.empty))

        // Build source using And combinator when transactionId is provided
        val source: UtxoSource = transactionId match
            case Some(txId) => UtxoSource.FromAddress(address) && UtxoSource.FromTransaction(txId)
            case None       => UtxoSource.FromAddress(address)

        // Build the query
        var query: UtxoQuery = UtxoQuery(source)

        // Add minRequiredTotalAmount
        query = minRequiredTotalAmount.fold(query)(amt => query.minTotal(amt))

        // Add datum filter
        query = datum.fold(query)(d => query && UtxoFilter.HasDatum(d))

        // Add minAmount filter
        query = minAmount.fold(query)(amt => query && UtxoFilter.MinLovelace(amt))

        findUtxos(query)
    }

    def findUtxos(
        query: UtxoQuery
    )(using ExecutionContext): Future[Either[UtxoQueryError, Utxos]] = {
        // Evaluate source to get candidate UTxOs
        def evalSource(source: UtxoSource): Utxos = source match
            case UtxoSource.FromAddress(addr) =>
                utxos.filter { case (_, output) => output.address == addr }
            case UtxoSource.FromAsset(policyId, assetName) =>
                utxos.filter { case (_, output) =>
                    output.value.assets.assets
                        .get(policyId)
                        .exists(_.contains(assetName))
                }
            case UtxoSource.FromInputs(inputs) =>
                utxos.filter { case (input, _) => inputs.contains(input) }
            case UtxoSource.FromTransaction(txId) =>
                utxos.filter { case (input, _) => input.transactionId == txId }
            case UtxoSource.Or(left, right) =>
                evalSource(left) ++ evalSource(right)
            case UtxoSource.And(left, right) =>
                val leftResult = evalSource(left)
                val rightResult = evalSource(right)
                leftResult.filter { case (input, _) => rightResult.contains(input) }

        // Evaluate a simple query
        def evalSimple(q: UtxoQuery.Simple): Utxos = {
            val candidates = evalSource(q.source)
            val filtered = q.filter match
                case Some(f) => candidates.filter(UtxoQuery.evalFilter(f, _))
                case None    => candidates
            UtxoQuery.applyPagination(filtered, q.limit, q.offset, q.minRequiredTotalAmount)
        }

        // Evaluate query recursively
        def evalQuery(q: UtxoQuery): Utxos = q match
            case simple: UtxoQuery.Simple => evalSimple(simple)
            case UtxoQuery.Or(left, right, limit, offset, minTotal) =>
                val leftResult = evalQuery(UtxoQuery.propagate(left, limit, minTotal))
                val rightResult = evalQuery(UtxoQuery.propagate(right, limit, minTotal))
                val combined = leftResult ++ rightResult
                UtxoQuery.applyPagination(combined, limit, offset, minTotal)

        Future.successful(Right(evalQuery(query)))
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
