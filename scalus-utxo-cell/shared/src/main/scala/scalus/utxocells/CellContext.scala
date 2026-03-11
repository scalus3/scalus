package scalus.utxocells

import scalus.Compile
import scalus.compiler.{Ignore, OnChainSubstitute}
import scalus.uplc.builtin.ByteString
import scalus.cardano.onchain.plutus.v1.{Address, PolicyId, PosixTime, PubKeyHash, Value}
import scalus.cardano.onchain.plutus.v3.ScriptContext

/** Dual-interpretation context for UtxoCell transitions.
  *
  * On-chain: `@OnChainSubstitute` redirects method calls to [[CellContextOps]]. The validator
  * passes `ScriptContext` directly as `CellContext` via `asInstanceOf` — the plugin's `typeProxy`
  * cast makes this zero-cost.
  *
  * Off-chain: [[OffChainCellContext]] implements this trait normally, accumulating builder
  * constraints and outputs.
  *
  * Usage in transition:
  * {{{
  * def transition(state: S, action: A, ctx: CellContext): Option[S] =
  *     action match
  *         case Action.Bid(bidder, amount) =>
  *             ctx.txInfo.requireSignedBy(bidder)
  *             ctx.txInfo.outputs.add(refundAddress, refundValue)
  *             Option.Some(newState)
  * }}}
  */
@Compile
@OnChainSubstitute(CellContextOps, selfAs = classOf[ScriptContext])
trait CellContext {

    /** Access transaction info. On-chain: returns TxInfo. Off-chain: returns builder wrapper. */
    @Ignore def txInfo: CellTxInfo

    /** The policy ID of this cell. On-chain: from scriptInfo. Off-chain: from cell definition. */
    @Ignore def ownPolicyId: PolicyId

    /** The value locked in the current input. On-chain: from txInfo.inputs. Off-chain: from UTxO.
      */
    @Ignore def ownInputValue: Value

    /** Mint tokens under the cell's own policy. On-chain: verifies mint quantity. Off-chain: adds
      * Mint step to builder.
      */
    @Ignore def mint(tokenName: ByteString, amount: BigInt): Unit

    /** Set the required value for the continuing output. On-chain: verifies the continuing output
      * has >= this value. Off-chain: records the value for the builder to use.
      */
    @Ignore def setContinuingValue(value: Value): Unit

    /** Require that the cell's own input contains at least the given quantity of a token (V024).
      *
      * Use this to verify authentication tokens (e.g., an external NFT that proves this is a
      * legitimate parameterized instance). On-chain: verifies own input value. Off-chain: verifies
      * the cell UTxO has the token with a clear error message.
      */
    @Ignore def requireInputToken(policyId: PolicyId, tokenName: ByteString, quantity: BigInt): Unit
}

/** Dual-interpretation transaction info for UtxoCell transitions.
  *
  * On-chain: `@OnChainSubstitute` redirects to [[CellTxInfoOps]]. Methods receive `TxInfo` as Data.
  * Off-chain: implements constraint accumulation on the builder.
  */
@Compile
@OnChainSubstitute(CellTxInfoOps)
trait CellTxInfo {

    /** Access outputs for verification/accumulation. */
    @Ignore def outputs: CellOutputs

    /** Require that the transaction is signed by the given public key hash. */
    @Ignore def requireSignedBy(pkh: PubKeyHash): Unit

    /** Require that the transaction validity range starts after the given time. */
    @Ignore def requireValidAfter(time: PosixTime): Unit

    /** Require that the transaction validity range ends before the given time. */
    @Ignore def requireValidBefore(time: PosixTime): Unit
}

/** Dual-interpretation transaction outputs for UtxoCell transitions.
  *
  * On-chain: `@OnChainSubstitute` redirects to [[CellOutputsOps]]. Verifies outputs exist in
  * `tx.outputs` with matching address and `>= value`. Off-chain: adds `payTo` steps to the
  * transaction builder.
  */
@Compile
@OnChainSubstitute(CellOutputsOps)
trait CellOutputs {

    /** Add a required output. On-chain: checks output exists. Off-chain: adds payTo to builder.
      *
      * WARNING (V005): Each call independently searches all tx outputs. If multiple `add` calls
      * target the same address with overlapping value, a single output can satisfy multiple checks.
      * For V005-safe matching, use `UtxoCellLib.verifyOutputs` with a collected list of outputs.
      */
    @Ignore def add(address: Address, value: Value): Unit
}
