package scalus.cardano.ledger
package utils

import scala.collection.View
import scala.util.boundary
import scala.util.boundary.break

object AllProvidedReferenceScripts {
    def allProvidedReferenceScriptsMap(
        transaction: Transaction,
        utxos: Utxos
    ): Either[
      TransactionException.BadInputsUTxOException |
          TransactionException.BadReferenceInputsUTxOException,
      Map[ScriptHash, Script]
    ] = {
        allProvidedReferenceScriptsView(transaction, utxos).map(
          _.map(script => script.scriptHash -> script).toMap
        )
    }

    /** Returns all reference scripts provided by transaction inputs and reference inputs as a
      * non-distinct sequence.
      *
      * Unlike `allProvidedReferenceScripts`, this method preserves duplicates. Each occurrence of a
      * script in the UTxO set is counted separately, which is important for fee calculation where
      * the same script referenced multiple times counts towards the minFee calculation for each
      * reference.
      *
      * @param transaction
      *   the transaction to analyze
      * @param utxos
      *   the UTxO set containing the referenced outputs
      * @return
      *   Either a `BadInputsUTxOException` or `BadReferenceInputsUTxOException` if any input is
      *   missing from the UTxO set, or a sequence containing all reference scripts (with duplicates
      *   preserved)
      */
    def allProvidedReferenceScriptsNonDistinctSeq(
        transaction: Transaction,
        utxos: Utxos
    ): Either[
      TransactionException.BadInputsUTxOException |
          TransactionException.BadReferenceInputsUTxOException,
      Seq[Script]
    ] = {
        for
            inputScripts <- providedReferenceScriptsSeq(
              transaction.id,
              transaction.body.value.inputs.toSeq,
              utxos,
              TransactionException.BadInputsUTxOException(_)
            )
            refInputScripts <- providedReferenceScriptsSeq(
              transaction.id,
              transaction.body.value.referenceInputs.toSeq,
              utxos,
              TransactionException.BadReferenceInputsUTxOException(_)
            )
        yield inputScripts ++ refInputScripts
    }

    def allProvidedReferenceScripts(
        transaction: Transaction,
        utxos: Utxos
    ): Either[
      TransactionException.BadInputsUTxOException |
          TransactionException.BadReferenceInputsUTxOException,
      Set[Script]
    ] = {
        allProvidedReferenceScriptsView(transaction, utxos).map(_.toSet)
    }

    def allProvidedReferenceScriptHashes(
        transaction: Transaction,
        utxos: Utxos
    ): Either[
      TransactionException.BadInputsUTxOException |
          TransactionException.BadReferenceInputsUTxOException,
      Set[ScriptHash]
    ] = {
        allProvidedReferenceScriptHashesView(transaction, utxos).map(_.toSet)
    }

    def allProvidedReferenceScriptHashesView(
        transaction: Transaction,
        utxos: Utxos
    ): Either[
      TransactionException.BadInputsUTxOException |
          TransactionException.BadReferenceInputsUTxOException,
      View[ScriptHash]
    ] = {
        allProvidedReferenceScriptsView(transaction, utxos).map(_.map(_.scriptHash))
    }

    def allProvidedReferencePlutusScriptsMap(
        transaction: Transaction,
        utxos: Utxos
    ): Either[
      TransactionException.BadInputsUTxOException |
          TransactionException.BadReferenceInputsUTxOException,
      Map[ScriptHash, PlutusScript]
    ] = {
        allProvidedReferencePlutusScriptsView(transaction, utxos).map(
          _.map(script => script.scriptHash -> script).toMap
        )
    }

    def allProvidedReferencePlutusScripts(
        transaction: Transaction,
        utxos: Utxos
    ): Either[
      TransactionException.BadInputsUTxOException |
          TransactionException.BadReferenceInputsUTxOException,
      Set[PlutusScript]
    ] = {
        allProvidedReferencePlutusScriptsView(transaction, utxos).map(_.toSet)
    }

    def allProvidedReferencePlutusScriptsView(
        transaction: Transaction,
        utxos: Utxos
    ): Either[
      TransactionException.BadInputsUTxOException |
          TransactionException.BadReferenceInputsUTxOException,
      View[PlutusScript]
    ] = {
        allProvidedReferenceScriptsView(transaction, utxos).map(_.flatMap {
            case plutusScript: PlutusScript => Some(plutusScript)
            case _                          => None
        })
    }

    def allProvidedReferencePlutusScriptHashes(
        transaction: Transaction,
        utxos: Utxos
    ): Either[
      TransactionException.BadInputsUTxOException |
          TransactionException.BadReferenceInputsUTxOException,
      Set[ScriptHash]
    ] = {
        allProvidedReferencePlutusScriptHashesView(transaction, utxos).map(_.toSet)
    }

    def allProvidedReferencePlutusScriptHashesView(
        transaction: Transaction,
        utxos: Utxos
    ): Either[
      TransactionException.BadInputsUTxOException |
          TransactionException.BadReferenceInputsUTxOException,
      View[ScriptHash]
    ] = {
        allProvidedReferencePlutusScriptsView(transaction, utxos).map(_.map(_.scriptHash))
    }

    def allProvidedReferenceNativeScriptsMap(
        transaction: Transaction,
        utxos: Utxos
    ): Either[
      TransactionException.BadInputsUTxOException |
          TransactionException.BadReferenceInputsUTxOException,
      Map[ScriptHash, Script.Native]
    ] = {
        allProvidedReferenceNativeScriptsView(transaction, utxos).map(
          _.map(script => script.scriptHash -> script).toMap
        )
    }

    def allProvidedReferenceNativeScripts(
        transaction: Transaction,
        utxos: Utxos
    ): Either[
      TransactionException.BadInputsUTxOException |
          TransactionException.BadReferenceInputsUTxOException,
      Set[Script.Native]
    ] = {
        allProvidedReferenceNativeScriptsView(transaction, utxos).map(_.toSet)
    }

    def allProvidedReferenceNativeScriptsView(
        transaction: Transaction,
        utxos: Utxos
    ): Either[
      TransactionException.BadInputsUTxOException |
          TransactionException.BadReferenceInputsUTxOException,
      View[Script.Native]
    ] = {
        allProvidedReferenceScriptsView(transaction, utxos).map(_.flatMap {
            case timelock: Script.Native => Some(timelock)
            case _                       => None
        })
    }

    def allProvidedReferenceNativeScriptHashes(
        transaction: Transaction,
        utxos: Utxos
    ): Either[
      TransactionException.BadInputsUTxOException |
          TransactionException.BadReferenceInputsUTxOException,
      Set[ScriptHash]
    ] = {
        allProvidedReferenceNativeScriptHashesView(transaction, utxos).map(_.toSet)
    }

    def allProvidedReferenceNativeScriptHashesView(
        transaction: Transaction,
        utxos: Utxos
    ): Either[
      TransactionException.BadInputsUTxOException |
          TransactionException.BadReferenceInputsUTxOException,
      View[ScriptHash]
    ] = {
        allProvidedReferenceNativeScriptsView(transaction, utxos).map(_.map(_.scriptHash))
    }

    def allProvidedReferenceScriptsView(
        transaction: Transaction,
        utxos: Utxos
    ): Either[
      TransactionException.BadInputsUTxOException |
          TransactionException.BadReferenceInputsUTxOException,
      View[Script]
    ] = {
        for
            allProvidedInputsReferenceScripts <- allProvidedInputsReferenceScripts(
              transaction,
              utxos
            )
            allProvidedReferenceInputsReferenceScripts <-
                allProvidedReferenceInputsReferenceScripts(
                  transaction,
                  utxos
                )
        yield allProvidedInputsReferenceScripts.view ++
            allProvidedReferenceInputsReferenceScripts.view
    }

    def allProvidedInputsReferenceScriptsMap(
        transaction: Transaction,
        utxos: Utxos
    ): Either[TransactionException.BadInputsUTxOException, Map[ScriptHash, Script]] = {
        allProvidedInputsReferenceScripts(transaction, utxos).map(
          _.map(script => script.scriptHash -> script).toMap
        )
    }

    def allProvidedInputsReferenceScripts(
        transaction: Transaction,
        utxos: Utxos
    ): Either[TransactionException.BadInputsUTxOException, Set[Script]] = {
        providedReferenceScripts(
          transaction.id,
          transaction.body.value.inputs.toSet,
          utxos,
          TransactionException.BadInputsUTxOException(_)
        )
    }

    def allProvidedInputsReferenceScriptHashes(
        transaction: Transaction,
        utxos: Utxos
    ): Either[TransactionException.BadInputsUTxOException, Set[ScriptHash]] = {
        allProvidedInputsReferenceScriptHashesView(transaction, utxos).map(_.toSet)
    }

    def allProvidedInputsReferenceScriptHashesView(
        transaction: Transaction,
        utxos: Utxos
    ): Either[TransactionException.BadInputsUTxOException, View[ScriptHash]] = {
        allProvidedInputsReferenceScripts(
          transaction,
          utxos
        ).map(_.view.map(_.scriptHash))
    }

    def allProvidedReferenceInputsReferenceScriptsMap(
        transaction: Transaction,
        utxos: Utxos
    ): Either[TransactionException.BadReferenceInputsUTxOException, Map[ScriptHash, Script]] = {
        allProvidedReferenceInputsReferenceScripts(transaction, utxos).map(
          _.map(script => script.scriptHash -> script).toMap
        )
    }

    def allProvidedReferenceInputsReferenceScripts(
        transaction: Transaction,
        utxos: Utxos
    ): Either[TransactionException.BadReferenceInputsUTxOException, Set[Script]] = {
        providedReferenceScripts(
          transaction.id,
          transaction.body.value.referenceInputs.toSet,
          utxos,
          TransactionException.BadReferenceInputsUTxOException(_)
        )
    }

    def allProvidedReferenceInputsReferenceScriptHashes(
        transaction: Transaction,
        utxos: Utxos
    ): Either[TransactionException.BadReferenceInputsUTxOException, Set[ScriptHash]] = {
        allProvidedReferenceInputsReferenceScriptHashesView(transaction, utxos).map(_.toSet)
    }

    def allProvidedReferenceInputsReferenceScriptHashesView(
        transaction: Transaction,
        utxos: Utxos
    ): Either[TransactionException.BadReferenceInputsUTxOException, View[ScriptHash]] = {
        allProvidedReferenceInputsReferenceScripts(
          transaction,
          utxos
        ).map(_.view.map(_.scriptHash))
    }

    private def providedReferenceScripts[
        ExceptionT <: TransactionException.BadInputsUTxOException |
            TransactionException.BadReferenceInputsUTxOException
    ](
        transactionId: TransactionHash,
        inputs: Set[TransactionInput],
        utxos: Utxos,
        missingUTxOException: TransactionHash => ExceptionT
    ): Either[ExceptionT, Set[Script]] = boundary {
        val result = for
            input <- inputs
            script <- utxos.get(input) match
                case Some(output) => output.scriptRef.map(_.script)
                // This check allows to be an order independent in the sequence of validation rules
                case None => break(Left(missingUTxOException(transactionId)))
        yield script
        Right(result)
    }

    private def providedReferenceScriptsSeq[
        ExceptionT <: TransactionException.BadInputsUTxOException |
            TransactionException.BadReferenceInputsUTxOException
    ](
        transactionId: TransactionHash,
        inputs: Seq[TransactionInput],
        utxos: Utxos,
        missingUTxOException: TransactionHash => ExceptionT
    ): Either[ExceptionT, Seq[Script]] = boundary {
        val result = for
            input <- inputs
            script <- utxos.get(input) match
                case Some(output) => output.scriptRef.map(_.script)
                case None         => break(Left(missingUTxOException(transactionId)))
        yield script
        Right(result)
    }
}
