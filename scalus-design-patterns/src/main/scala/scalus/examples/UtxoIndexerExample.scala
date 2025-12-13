package scalus.examples

import scalus.*
import scalus.prelude.Validator
import scalus.Compiler.compile
import scalus.builtin.Builtins.*
import scalus.builtin.Data
import scalus.builtin.Data.{FromData, ToData}
import scalus.ledger.api.v3.{TxInfo, TxOutRef}
import scalus.patterns.UtxoIndexer
import scalus.uplc.Program

/** Example validator using the UTxO Indexer pattern. */

case class IndexerRedeemer(inputIdx: BigInt, outputIdx: BigInt) derives FromData, ToData

@Compile
object IndexerValidator extends Validator:
    inline override def spend(
        datum: prelude.Option[Data],
        redeemer: Data,
        tx: TxInfo,
        ownRef: TxOutRef
    ): Unit = {
        val IndexerRedeemer(inputIdx, outputIdx) = redeemer.to[IndexerRedeemer]

        // Use the indexer pattern to validate the input-output relationship
        UtxoIndexer.oneToOne(
          ownRef,
          inputIdx,
          outputIdx,
          tx,
          // Custom validation logic goes here
          (input, output) => {
              trace("Validating input-output pair")(())
              // In a real validator, you'd check something meaningful like:
              // - Value is preserved: input.resolved.value.lovelace === output.value.lovelace
              // - Datum is correct
              // - Address matches, etc.
              true // Simplified for example
          }
        )
    }

object IndexerValidatorContract:
    val compiledValidator = compile(IndexerValidator.validate)
    val script: Program = compiledValidator.toUplc(generateErrorTraces = true).plutusV3

/** Off-chain code demonstrating how to use SpendWithDelayedRedeemer to compute the indices. */
object Offchain:
    import scalus.builtin.Data.toData
    import scalus.cardano.address.Address
    import scalus.cardano.ledger.{Transaction, Utxo}
    import scalus.cardano.ledger.Script.PlutusV3
    import scalus.cardano.txbuilder.{Environment, TxBuilder}

    def buildTransaction(
        env: Environment,
        scriptUtxo: Utxo,
        recipientAddress: Address
    ): TxBuilder = {
        val plutusScript = PlutusV3(IndexerValidatorContract.script.cborByteString)

        TxBuilder(env)
            .spend(
              scriptUtxo,
              redeemerBuilder = (tx: Transaction) => {
                  val inputIdx = tx.body.value.inputs.toSeq.indexOf(scriptUtxo.input)
                  val outputIdx = tx.body.value.outputs.indexWhere { sized =>
                      sized.value.address == recipientAddress
                  }

                  IndexerRedeemer(BigInt(inputIdx), BigInt(outputIdx)).toData
              },
              plutusScript
            )
            .payTo(recipientAddress, scriptUtxo.output.value)
        // Caller should call .build(changeTo = address)
    }
