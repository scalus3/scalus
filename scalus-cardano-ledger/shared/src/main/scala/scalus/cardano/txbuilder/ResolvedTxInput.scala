package scalus.cardano.txbuilder

import scalus.uplc.builtin.Data
import scalus.cardano.ledger.{DatumOption, PlutusScript, TransactionInput, TransactionOutput}

enum ResolvedTxInput {
    case Pubkey(utxo: (TransactionInput, TransactionOutput), data: Option[DatumOption] = None)
    case Script(
        utxo: (TransactionInput, TransactionOutput),
        script: PlutusScript,
        redeemer: Data,
        data: Option[DatumOption] = None
    )
}
extension (r: ResolvedTxInput) {
    def utxo: (TransactionInput, TransactionOutput) = r match {
        case ResolvedTxInput.Pubkey(utxo, _)       => utxo
        case ResolvedTxInput.Script(utxo, _, _, _) => utxo
    }

    def input: TransactionInput = r match {
        case ResolvedTxInput.Pubkey(utxo, data)                   => utxo._1
        case ResolvedTxInput.Script(utxo, script, redeemer, data) => utxo._1
    }

    def output: TransactionOutput = r match {
        case ResolvedTxInput.Pubkey(utxo, data)                   => utxo._2
        case ResolvedTxInput.Script(utxo, script, redeemer, data) => utxo._2
    }
}
