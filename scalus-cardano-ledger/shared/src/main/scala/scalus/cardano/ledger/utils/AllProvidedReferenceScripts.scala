package scalus.cardano.ledger
package utils

import scala.collection.View
import scala.util.boundary
import scala.util.boundary.break

object AllProvidedReferenceScripts {
    def allProvidedReferenceScriptsMap(
        transaction: Transaction,
        utxo: Utxos
    ): Either[
      TransactionException.BadInputsUTxOException |
          TransactionException.BadReferenceInputsUTxOException,
      Map[ScriptHash, Script]
    ] = {
        allProvidedReferenceScriptsView(transaction, utxo).map(
          _.map(script => script.scriptHash -> script).toMap
        )
    }

    def allProvidedReferenceScripts(
        transaction: Transaction,
        utxo: Utxos
    ): Either[
      TransactionException.BadInputsUTxOException |
          TransactionException.BadReferenceInputsUTxOException,
      Set[Script]
    ] = {
        allProvidedReferenceScriptsView(transaction, utxo).map(_.toSet)
    }

    def allProvidedReferenceScriptHashes(
        transaction: Transaction,
        utxo: Utxos
    ): Either[
      TransactionException.BadInputsUTxOException |
          TransactionException.BadReferenceInputsUTxOException,
      Set[ScriptHash]
    ] = {
        allProvidedReferenceScriptHashesView(transaction, utxo).map(_.toSet)
    }

    def allProvidedReferenceScriptHashesView(
        transaction: Transaction,
        utxo: Utxos
    ): Either[
      TransactionException.BadInputsUTxOException |
          TransactionException.BadReferenceInputsUTxOException,
      View[ScriptHash]
    ] = {
        allProvidedReferenceScriptsView(transaction, utxo).map(_.map(_.scriptHash))
    }

    def allProvidedReferencePlutusScriptsMap(
        transaction: Transaction,
        utxo: Utxos
    ): Either[
      TransactionException.BadInputsUTxOException |
          TransactionException.BadReferenceInputsUTxOException,
      Map[ScriptHash, PlutusScript]
    ] = {
        allProvidedReferencePlutusScriptsView(transaction, utxo).map(
          _.map(script => script.scriptHash -> script).toMap
        )
    }

    def allProvidedReferencePlutusScripts(
        transaction: Transaction,
        utxo: Utxos
    ): Either[
      TransactionException.BadInputsUTxOException |
          TransactionException.BadReferenceInputsUTxOException,
      Set[PlutusScript]
    ] = {
        allProvidedReferencePlutusScriptsView(transaction, utxo).map(_.toSet)
    }

    def allProvidedReferencePlutusScriptsView(
        transaction: Transaction,
        utxo: Utxos
    ): Either[
      TransactionException.BadInputsUTxOException |
          TransactionException.BadReferenceInputsUTxOException,
      View[PlutusScript]
    ] = {
        allProvidedReferenceScriptsView(transaction, utxo).map(_.flatMap {
            case plutusScript: PlutusScript => Some(plutusScript)
            case _                          => None
        })
    }

    def allProvidedReferencePlutusScriptHashes(
        transaction: Transaction,
        utxo: Utxos
    ): Either[
      TransactionException.BadInputsUTxOException |
          TransactionException.BadReferenceInputsUTxOException,
      Set[ScriptHash]
    ] = {
        allProvidedReferencePlutusScriptHashesView(transaction, utxo).map(_.toSet)
    }

    def allProvidedReferencePlutusScriptHashesView(
        transaction: Transaction,
        utxo: Utxos
    ): Either[
      TransactionException.BadInputsUTxOException |
          TransactionException.BadReferenceInputsUTxOException,
      View[ScriptHash]
    ] = {
        allProvidedReferencePlutusScriptsView(transaction, utxo).map(_.map(_.scriptHash))
    }

    def allProvidedReferenceNativeScriptsMap(
        transaction: Transaction,
        utxo: Utxos
    ): Either[
      TransactionException.BadInputsUTxOException |
          TransactionException.BadReferenceInputsUTxOException,
      Map[ScriptHash, Script.Native]
    ] = {
        allProvidedReferenceNativeScriptsView(transaction, utxo).map(
          _.map(script => script.scriptHash -> script).toMap
        )
    }

    def allProvidedReferenceNativeScripts(
        transaction: Transaction,
        utxo: Utxos
    ): Either[
      TransactionException.BadInputsUTxOException |
          TransactionException.BadReferenceInputsUTxOException,
      Set[Script.Native]
    ] = {
        allProvidedReferenceNativeScriptsView(transaction, utxo).map(_.toSet)
    }

    def allProvidedReferenceNativeScriptsView(
        transaction: Transaction,
        utxo: Utxos
    ): Either[
      TransactionException.BadInputsUTxOException |
          TransactionException.BadReferenceInputsUTxOException,
      View[Script.Native]
    ] = {
        allProvidedReferenceScriptsView(transaction, utxo).map(_.flatMap {
            case timelock: Script.Native => Some(timelock)
            case _                       => None
        })
    }

    def allProvidedReferenceNativeScriptHashes(
        transaction: Transaction,
        utxo: Utxos
    ): Either[
      TransactionException.BadInputsUTxOException |
          TransactionException.BadReferenceInputsUTxOException,
      Set[ScriptHash]
    ] = {
        allProvidedReferenceNativeScriptHashesView(transaction, utxo).map(_.toSet)
    }

    def allProvidedReferenceNativeScriptHashesView(
        transaction: Transaction,
        utxo: Utxos
    ): Either[
      TransactionException.BadInputsUTxOException |
          TransactionException.BadReferenceInputsUTxOException,
      View[ScriptHash]
    ] = {
        allProvidedReferenceNativeScriptsView(transaction, utxo).map(_.map(_.scriptHash))
    }

    def allProvidedReferenceScriptsView(
        transaction: Transaction,
        utxo: Utxos
    ): Either[
      TransactionException.BadInputsUTxOException |
          TransactionException.BadReferenceInputsUTxOException,
      View[Script]
    ] = {
        for
            allProvidedInputsReferenceScripts <- allProvidedInputsReferenceScripts(
              transaction,
              utxo
            )
            allProvidedReferenceInputsReferenceScripts <-
                allProvidedReferenceInputsReferenceScripts(
                  transaction,
                  utxo
                )
        yield allProvidedInputsReferenceScripts.view ++
            allProvidedReferenceInputsReferenceScripts.view
    }

    def allProvidedInputsReferenceScriptsMap(
        transaction: Transaction,
        utxo: Utxos
    ): Either[TransactionException.BadInputsUTxOException, Map[ScriptHash, Script]] = {
        allProvidedInputsReferenceScripts(transaction, utxo).map(
          _.map(script => script.scriptHash -> script).toMap
        )
    }

    def allProvidedInputsReferenceScripts(
        transaction: Transaction,
        utxo: Utxos
    ): Either[TransactionException.BadInputsUTxOException, Set[Script]] = {
        providedReferenceScripts(
          transaction.id,
          transaction.body.value.inputs.toSet,
          utxo,
          TransactionException.BadInputsUTxOException(_)
        )
    }

    def allProvidedInputsReferenceScriptHashes(
        transaction: Transaction,
        utxo: Utxos
    ): Either[TransactionException.BadInputsUTxOException, Set[ScriptHash]] = {
        allProvidedInputsReferenceScriptHashesView(transaction, utxo).map(_.toSet)
    }

    def allProvidedInputsReferenceScriptHashesView(
        transaction: Transaction,
        utxo: Utxos
    ): Either[TransactionException.BadInputsUTxOException, View[ScriptHash]] = {
        allProvidedInputsReferenceScripts(
          transaction,
          utxo
        ).map(_.view.map(_.scriptHash))
    }

    def allProvidedReferenceInputsReferenceScriptsMap(
        transaction: Transaction,
        utxo: Utxos
    ): Either[TransactionException.BadReferenceInputsUTxOException, Map[ScriptHash, Script]] = {
        allProvidedReferenceInputsReferenceScripts(transaction, utxo).map(
          _.map(script => script.scriptHash -> script).toMap
        )
    }

    def allProvidedReferenceInputsReferenceScripts(
        transaction: Transaction,
        utxo: Utxos
    ): Either[TransactionException.BadReferenceInputsUTxOException, Set[Script]] = {
        providedReferenceScripts(
          transaction.id,
          transaction.body.value.referenceInputs.toSet,
          utxo,
          TransactionException.BadReferenceInputsUTxOException(_)
        )
    }

    def allProvidedReferenceInputsReferenceScriptHashes(
        transaction: Transaction,
        utxo: Utxos
    ): Either[TransactionException.BadReferenceInputsUTxOException, Set[ScriptHash]] = {
        allProvidedReferenceInputsReferenceScriptHashesView(transaction, utxo).map(_.toSet)
    }

    def allProvidedReferenceInputsReferenceScriptHashesView(
        transaction: Transaction,
        utxo: Utxos
    ): Either[TransactionException.BadReferenceInputsUTxOException, View[ScriptHash]] = {
        allProvidedReferenceInputsReferenceScripts(
          transaction,
          utxo
        ).map(_.view.map(_.scriptHash))
    }

    private def providedReferenceScripts[
        ExceptionT <: TransactionException.BadInputsUTxOException |
            TransactionException.BadReferenceInputsUTxOException
    ](
        transactionId: TransactionHash,
        inputs: Set[TransactionInput],
        utxo: Utxos,
        missingUTxOException: TransactionHash => ExceptionT
    ): Either[ExceptionT, Set[Script]] = boundary {
        val result = for
            input <- inputs
            script <- utxo.get(input) match
                case Some(output) => output.scriptRef.map(_.script)
                // This check allows to be an order independent in the sequence of validation rules
                case None => break(Left(missingUTxOException(transactionId)))
        yield script
        Right(result)
    }
}
