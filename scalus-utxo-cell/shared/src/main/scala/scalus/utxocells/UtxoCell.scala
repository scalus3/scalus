package scalus.utxocells

import scalus.Compile
import scalus.uplc.builtin.ByteString
import scalus.uplc.builtin.Data.toData
import scalus.uplc.builtin.ToData
import scalus.cardano.onchain.plutus.v1.{PolicyId, Value}
import scalus.cardano.onchain.plutus.v2.OutputDatum
import scalus.cardano.onchain.plutus.v3.*
import scalus.cardano.onchain.plutus.prelude.*

/** On-chain helpers for UtxoCell state machine validators.
  *
  * Usage pattern:
  * {{{
  * @Compile
  * object MyCell extends Validator {
  *     def transition(state: MyState, action: MyAction): UtxoCellTransition[MyState] = ...
  *
  *     inline override def spend(datum: Option[Data], redeemer: Data, tx: TxInfo, ownRef: TxOutRef): Unit = {
  *         val state = datum.getOrFail("missing datum").to[MyState]
  *         val action = redeemer.to[MyAction]
  *         val result = transition(state, action)
  *         UtxoCellLib.verifySpendResult(result, tx, ownRef)
  *     }
  * }
  * }}}
  */
@Compile
object UtxoCellLib {

    /** Verify that a spend transition result is consistent with the on-chain transaction.
      *
      * If nextState is Some, verifies that exactly one continuing output exists at the same script
      * address with a matching inline datum. If nextState is None, the cell is terminating and no
      * continuing output is required. Also verifies all additional outputs in result.outputs.
      */
    def verifySpendResult[S: ToData](
        result: UtxoCellTransition[S],
        tx: TxInfo,
        ownRef: TxOutRef
    ): Unit = {
        result.nextState match
            case Option.Some(nextState) =>
                val ownInput = tx.findOwnInputOrFail(ownRef)
                val scriptHash = ownInput.resolved.address.credential match
                    case Credential.ScriptCredential(hash) => hash
                    case _ => fail("UtxoCell: input is not a script")
                val outputs = tx.findOwnScriptOutputs(scriptHash)
                require(outputs.length === BigInt(1), "UtxoCell: expected one continuing output")
                val continuingOutput = outputs.head
                continuingOutput.datum match
                    case OutputDatum.OutputDatum(inlineData) =>
                        require(
                          inlineData === nextState.toData,
                          "UtxoCell: continuing output datum mismatch"
                        )
                    case _ => fail("UtxoCell: expected inline datum on continuing output")
            case Option.None =>
                ()
        verifyOutputs(result.outputs, tx)
    }

    /** Verify a spend transition when using CellContext (outputs checked by ctx.addOutput).
      *
      * If nextState is Some, verifies that exactly one continuing output exists at the same script
      * address with a matching inline datum. If nextState is None, the cell is terminating.
      */
    def verifyContinuingOutput[S: ToData](
        nextState: Option[S],
        tx: TxInfo,
        ownRef: TxOutRef
    ): Unit = {
        nextState match
            case Option.Some(state) =>
                val ownInput = tx.findOwnInputOrFail(ownRef)
                val scriptHash = ownInput.resolved.address.credential match
                    case Credential.ScriptCredential(hash) => hash
                    case _ => fail("UtxoCell: input is not a script")
                val outputs = tx.findOwnScriptOutputs(scriptHash)
                require(outputs.length === BigInt(1), "UtxoCell: expected one continuing output")
                val continuingOutput = outputs.head
                continuingOutput.datum match
                    case OutputDatum.OutputDatum(inlineData) =>
                        require(
                          inlineData === state.toData,
                          "UtxoCell: continuing output datum mismatch"
                        )
                    case _ => fail("UtxoCell: expected inline datum on continuing output")
            case Option.None =>
                ()
    }

    /** Verify that all expected outputs exist in the transaction.
      *
      * For each expected output, checks that at least one transaction output has a matching address
      * and value >= the expected value.
      */
    def verifyOutputs(expectedOutputs: List[UtxoCellOutput], tx: TxInfo): Unit = {
        expectedOutputs.foreach { expected =>
            val found = tx.outputs.exists { out =>
                out.address === expected.address && valueGeq(out.value, expected.value)
            }
            require(found, "UtxoCell: expected output not found in transaction")
        }
    }

    /** Check that value `a` contains at least value `b` (a >= b for all tokens). */
    def valueGeq(a: Value, b: Value): Boolean = {
        val diff = Value.minus(a, b)
        diff.isZero || diff.toSortedMap.forall(_._2.forall(_._2 > BigInt(0)))
    }

    /** Verify that a mint (init) transaction is consistent with the on-chain transaction.
      *
      * Checks that the beacon token is minted exactly once, and that exactly one output exists at
      * the script address (identified by policyId) with the correct inline datum matching the
      * initial state.
      */
    def verifyMintResult[S: ToData](
        initialState: S,
        tokenName: ByteString,
        policyId: PolicyId,
        tx: TxInfo
    ): Unit = {
        require(
          tx.mint.quantityOf(policyId, tokenName) === BigInt(1),
          "UtxoCell: beacon token not minted"
        )
        val outputs = tx.findOwnScriptOutputs(policyId)
        require(outputs.length === BigInt(1), "UtxoCell: expected one output at script address")
        val output = outputs.head
        output.datum match
            case OutputDatum.OutputDatum(inlineData) =>
                require(
                  inlineData === initialState.toData,
                  "UtxoCell: initial state datum mismatch"
                )
            case _ => fail("UtxoCell: expected inline datum on initial output")
    }

    /** Verify that a beacon token is burned exactly once (for terminal transitions). */
    def verifyBurnBeacon(
        tokenName: ByteString,
        policyId: PolicyId,
        tx: TxInfo
    ): Unit = {
        require(
          tx.mint.quantityOf(policyId, tokenName) === BigInt(-1),
          "UtxoCell: beacon token not burned"
        )
    }
}
