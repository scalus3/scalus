package scalus.utxocells

import scalus.Compile
import scalus.uplc.builtin.{ByteString, Data}
import scalus.uplc.builtin.Data.toData
import scalus.uplc.builtin.ToData
import scalus.cardano.onchain.plutus.v1.{Address, PolicyId, Value}
import scalus.cardano.onchain.plutus.v2
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
      * address with a matching inline datum, and that the staking credential is preserved (V016
      * protection). If nextState is None, the cell is terminating and no continuing output is
      * required. Also verifies all additional outputs in result.outputs (V005-safe).
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
                verifyStakingCredential(ownInput.resolved.address, continuingOutput.address)
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
      * address with a matching inline datum, and that the staking credential is preserved (V016
      * protection). If nextState is None, the cell is terminating.
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
                verifyStakingCredential(ownInput.resolved.address, continuingOutput.address)
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

    /** Verify that all expected outputs exist in the transaction (V005-safe).
      *
      * Each expected output is matched to a distinct transaction output. Once a tx output is used
      * to satisfy one expected output, it cannot satisfy another. This prevents double satisfaction
      * attacks where a single output could satisfy multiple requirements.
      */
    def verifyOutputs(expectedOutputs: List[UtxoCellOutput], tx: TxInfo): Unit = {
        expectedOutputs.foldLeft(tx.outputs) { (remaining, expected) =>
            val rest = removeFirstMatching(remaining, expected.address, expected.value)
            require(
              rest.length < remaining.length,
              "UtxoCell: expected output not found in transaction"
            )
            rest
        }
    }

    /** Remove the first output matching the given address and minimum value. */
    def removeFirstMatching(
        list: List[v2.TxOut],
        address: Address,
        value: Value
    ): List[v2.TxOut] = {
        list match
            case List.Nil => List.Nil
            case List.Cons(head, tail) =>
                if head.address === address && valueGeq(head.value, value) then tail
                else List.Cons(head, removeFirstMatching(tail, address, value))
    }

    /** Verify that the continuing output preserves the staking credential from the input (V016).
      *
      * Without this check, an attacker could redirect staking rewards by changing the staking
      * credential on the continuing output.
      */
    def verifyStakingCredential(
        inputAddress: Address,
        outputAddress: Address
    ): Unit = {
        require(
          inputAddress.stakingCredential === outputAddress.stakingCredential,
          "UtxoCell: staking credential must be preserved on continuing output"
        )
    }

    /** Check that value `a` contains at least value `b` (a >= b for all tokens). */
    def valueGeq(a: Value, b: Value): Boolean = {
        val diff = Value.minus(a, b)
        diff.isZero || diff.toSortedMap.forall(_._2.forall(_._2 > BigInt(0)))
    }

    /** Verify that a mint (init) transaction is consistent with the on-chain transaction.
      *
      * Checks that the beacon token is minted exactly once, that no other tokens are minted under
      * this policy (V011 protection), and that exactly one output exists at the script address
      * (identified by policyId) with the correct inline datum matching the initial state.
      */
    def verifyMintResult[S: ToData](
        initialState: S,
        tokenName: ByteString,
        policyId: PolicyId,
        tx: TxInfo
    ): Unit = {
        val mintedTokens = tx.mint.tokens(policyId)
        require(
          mintedTokens.length === BigInt(1),
          "UtxoCell: only the beacon token may be minted under this policy"
        )
        require(
          mintedTokens.keys.head === tokenName,
          "UtxoCell: unexpected token name minted"
        )
        require(
          mintedTokens.values.head === BigInt(1),
          "UtxoCell: beacon token must be minted exactly once"
        )
        val outputs = tx.findOwnScriptOutputs(policyId)
        require(outputs.length === BigInt(1), "UtxoCell: expected one output at script address")
        val output = outputs.head
        require(
          output.value.quantityOf(policyId, tokenName) === BigInt(1),
          "UtxoCell: initial output must contain the beacon token"
        )
        output.datum match
            case OutputDatum.OutputDatum(inlineData) =>
                require(
                  inlineData === initialState.toData,
                  "UtxoCell: initial state datum mismatch"
                )
            case _ => fail("UtxoCell: expected inline datum on initial output")
    }

    /** Verify that a beacon token is burned exactly once (for terminal transitions).
      *
      * Also verifies that no other tokens are minted or burned under this policy (V011 protection).
      */
    def verifyBurnBeacon(
        tokenName: ByteString,
        policyId: PolicyId,
        tx: TxInfo
    ): Unit = {
        val mintedTokens = tx.mint.tokens(policyId)
        require(
          mintedTokens.length === BigInt(1),
          "UtxoCell: only the beacon token may be burned under this policy"
        )
        require(
          mintedTokens.keys.head === tokenName,
          "UtxoCell: unexpected token name in burn"
        )
        require(
          mintedTokens.values.head === BigInt(-1),
          "UtxoCell: beacon token must be burned exactly once"
        )
    }

    /** Verify a flow spend transition.
      *
      * If nextDatum is Some, verifies the continuing output (datum match + staking credential). If
      * nextDatum is None (terminal), verifies the beacon token is burned.
      */
    def verifyFlowSpend(
        nextDatum: Option[Data],
        beaconName: ByteString,
        tx: TxInfo,
        ownRef: TxOutRef
    ): Unit = {
        verifyContinuingOutput[Data](nextDatum, tx, ownRef)
        nextDatum match
            case Option.None =>
                val ownInput = tx.findOwnInputOrFail(ownRef)
                val policyId = ownInput.resolved.address.credential match
                    case Credential.ScriptCredential(hash) => hash
                    case _ => fail("UtxoFlow: input is not a script")
                verifyBurnBeacon(beaconName, policyId, tx)
            case _ => ()
    }
}
