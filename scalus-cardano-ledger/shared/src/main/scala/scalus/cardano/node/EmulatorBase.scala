package scalus.cardano.node

import scalus.builtin.ByteString
import scalus.cardano.address.Address
import scalus.cardano.ledger.rules.{CardanoMutator, Context, PlutusScriptsTransactionMutator, STS, State}
import scalus.cardano.ledger.*

import scala.concurrent.{ExecutionContext, Future}

/** Base trait for Emulator implementations containing shared logic.
  *
  * Platform-specific implementations (JVM/JS) extend this trait and provide thread-safe or
  * single-threaded state management as appropriate.
  */
trait EmulatorBase extends Provider {
    def validators: Iterable[STS.Validator]
    def mutators: Iterable[STS.Mutator]

    // Abstract - platform-specific state access
    def utxos: Utxos
    protected def currentContext: Context

    // Abstract - platform-specific state modification
    protected def submitSync(transaction: Transaction): Either[SubmitError, TransactionHash]
    def setSlot(slot: SlotNo): Unit
    def snapshot(): Emulator

    def fetchLatestParams(using ExecutionContext): Future[ProtocolParams] = {
        val params = currentContext.env.params
        Future.successful(params)
    }

    def submit(transaction: Transaction)(using
        ExecutionContext
    ): Future[Either[SubmitError, TransactionHash]] =
        Future.successful(submitSync(transaction))

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

    protected def processTransaction(
        context: Context,
        state: State,
        transaction: Transaction
    ): Either[TransactionException, State] = {
        STS.Mutator.transit(validators, mutators, context, state, transaction)
    }
}

object EmulatorBase {
    val defaultValidators: Set[STS.Validator] = CardanoMutator.allValidators.values.toSet
    val defaultMutators: Set[STS.Mutator] = CardanoMutator.allMutators.values.toSet

    /** Creates initial UTxOs for the given addresses.
      *
      * @param addresses
      *   The addresses to initialize with funds
      * @param initialValue
      *   Initial value per address (default: 10,000 ADA like Yaci Devkit)
      * @return
      *   A map of transaction inputs to outputs
      */
    def createInitialUtxos(
        addresses: Seq[Address],
        initialValue: Value = Value.ada(10_000L)
    ): Utxos = {
        val genesisHash = TransactionHash.fromByteString(ByteString.fromHex("0" * 64))
        addresses.zipWithIndex.map { case (address, index) =>
            Input(genesisHash, index) -> Output(address, initialValue)
        }.toMap
    }
}
