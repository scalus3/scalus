package scalus.examples

import scalus.compiler.Compile

import scalus.*
import scalus.uplc.builtin.{Data, FromData, ToData}
import scalus.compiler.Options
import scalus.cardano.onchain.plutus.v1.{Credential, PubKeyHash}
import scalus.cardano.onchain.plutus.v3.*
import scalus.patterns.MerkelizedValidator
import scalus.cardano.onchain.plutus.prelude.*
import scalus.cardano.onchain.plutus.v3.{DataParameterizedValidator, Validator}
import scalus.uplc.PlutusV3

/** Multi-Pool DEX Batch Settlement - demonstrates '''multiple''' withdraw-zero scripts with the
  * merkelized validator pattern.
  *
  * '''Use Case:''' A DEX with multiple liquidity pools (e.g. ADA/TokenA, ADA/TokenB). Orders
  * targeting different pools are settled in a single batch transaction. Each pool has its own
  * staking validator that verifies swap invariants, and the order spending validator reads verified
  * swap data from the target pool.
  *
  * '''Why multiple withdraw-zero scripts:'''
  *   - Each pool is a separate parameterized instance of [[PoolSwapValidator]]
  *   - Different parameters produce different script hashes
  *   - A batch transaction includes withdrawals from multiple pool scripts
  *   - Each pool validates its constant product invariant ONCE (not per order)
  *   - Orders across different pools are settled simultaneously in one transaction
  *
  * '''Architecture:'''
  *   - '''[[PoolSwapValidator]]''' (staking, one instance per pool): Verifies the pool's constant
  *     product invariant and reports verified swap totals in its redeemer.
  *   - '''[[OrderValidator]]''' (spending): Reads the order's target pool hash from datum, uses
  *     [[scalus.patterns.MerkelizedValidator.verifyAndGetRedeemer]] to get the verified swap data
  *     from that pool's staking redeemer, and verifies the order is correctly filled.
  *
  * '''Transaction structure for a batch with 2 pools:'''
  *   - Inputs: order UTxOs (at [[OrderValidator]] address), fee payer UTxO
  *   - Outputs: ADA to order owners
  *   - Withdrawals: 0 ADA from pool A staking script + 0 ADA from pool B staking script
  *   - Redeemers: pool A's [[SwapRedeemer]] + pool B's [[SwapRedeemer]] + per-order
  *     [[OrderRedeemer]]
  *
  * '''Compared to other examples:'''
  *   - [[BatchAuctionExample]]: single withdraw-zero, merkelized (reads verified data)
  *   - [[StakeValidatorPaymentSplitterExample]]: single withdraw-zero, non-merkelized
  *   - '''This example''': multiple withdraw-zero scripts, merkelized - each pool is independent
  *
  * '''Production considerations:''' This example is simplified to focus on the multi-withdraw-zero
  * pattern. A production DEX would additionally need:
  *   - '''Pool state verification:''' The pool validator should verify reserves against an on-chain
  *     pool UTxO (e.g., an NFT-authenticated continuing output) rather than trusting the redeemer.
  *   - '''Flow verification:''' The pool validator should verify that `totalInputAmount` and
  *     `totalOutputAmount` match the actual sum of order inputs/outputs in the transaction.
  *   - '''Double satisfaction prevention:''' The order validator should link each order to a
  *     specific output index (passed in the redeemer) rather than summing all outputs to the owner.
  *   - '''Order expiry:''' Orders should include a deadline after which anyone can reclaim them.
  *
  * @see
  *   [[scalus.patterns.MerkelizedValidator]]
  * @see
  *   [[scalus.patterns.StakeValidator]]
  */

// -- Data types --

/** Redeemer for a pool staking validator containing verified swap totals.
  *
  * Off-chain code computes these; the pool validator verifies the invariant on-chain.
  *
  * @param totalInputAmount
  *   total tokens going into the pool across all orders in this batch
  * @param totalOutputAmount
  *   total ADA coming out of the pool across all orders in this batch
  * @param reserveABefore
  *   pool's token reserve before the batch
  * @param reserveBBefore
  *   pool's ADA reserve before the batch
  */
case class SwapRedeemer(
    totalInputAmount: BigInt,
    totalOutputAmount: BigInt,
    reserveABefore: BigInt,
    reserveBBefore: BigInt
) derives ToData,
      FromData

/** Datum for an order UTxO.
  *
  * @param owner
  *   who placed the order and receives output
  * @param poolScriptHash
  *   hash of the target pool's staking validator
  * @param minOutputLovelace
  *   minimum ADA the user accepts (slippage protection)
  * @param inputTokenAmount
  *   amount of tokens the user is selling
  */
case class OrderDatum(
    owner: PubKeyHash,
    poolScriptHash: ValidatorHash,
    minOutputLovelace: BigInt,
    inputTokenAmount: BigInt
) derives ToData,
      FromData

/** Redeemer for spending an order UTxO. */
enum OrderRedeemer derives ToData, FromData:
    case Execute
    case Cancel

// -- Pool Swap Validator (staking script, one instance per pool) --

/** Pool Swap Validator - verifies constant product invariant for a liquidity pool.
  *
  * Each pool instance is parameterized by a pool identifier. Different parameters produce different
  * script hashes, enabling multiple withdraw-zero scripts in a single transaction.
  *
  * The reward endpoint verifies that `reserveA * reserveB` is preserved (or increased due to fees)
  * after the batch swap.
  */
@Compile
object PoolSwapValidator extends DataParameterizedValidator {

    /** Reward endpoint - verifies constant product invariant.
      *
      * Runs ONCE per pool per transaction, regardless of how many orders target this pool.
      * `poolIdData` is not used in validation - it exists solely to parameterize the script,
      * producing a unique hash per pool.
      */
    inline override def reward(
        poolIdData: Data,
        redeemer: Data,
        stakingKey: Credential,
        tx: TxInfo
    ): Unit = {
        val swap = redeemer.to[SwapRedeemer]

        require(swap.reserveABefore > 0, "Reserve A must be positive")
        require(swap.reserveBBefore > 0, "Reserve B must be positive")
        require(swap.totalInputAmount > 0, "Input amount must be positive")
        require(swap.totalOutputAmount > 0, "Output amount must be positive")

        // PRODUCTION: verify reserves against an on-chain pool UTxO (e.g. NFT-authenticated)
        // rather than trusting the redeemer. Also verify that totalInputAmount/totalOutputAmount
        // match the actual sum of order token flows in the transaction.

        // Constant product: k_after >= k_before
        // After swap: reserveA grows by input, reserveB shrinks by output
        val kBefore = swap.reserveABefore * swap.reserveBBefore
        val reserveBAfter = swap.reserveBBefore - swap.totalOutputAmount

        require(reserveBAfter > 0, "Insufficient pool liquidity")

        val kAfter = (swap.reserveABefore + swap.totalInputAmount) * reserveBAfter
        require(kAfter >= kBefore, "Constant product invariant violated")
    }
}

// -- Order Validator (spending script for order UTxOs) --

/** Order Validator - spending validator for DEX orders.
  *
  * Each order's datum specifies which pool it targets (by pool script hash). The spending validator
  * uses [[scalus.patterns.MerkelizedValidator.verifyAndGetRedeemer]] to read the verified swap data
  * from that pool. This enables different orders in the same transaction to target different pools,
  * with each pool's validation running only once.
  */
@Compile
object OrderValidator extends Validator {

    inline override def spend(
        datum: Option[Data],
        redeemer: Data,
        tx: TxInfo,
        ownRef: TxOutRef
    ): Unit = {
        val order = datum.getOrFail("Order datum is required").to[OrderDatum]
        val orderRedeemer = redeemer.to[OrderRedeemer]

        orderRedeemer match
            case OrderRedeemer.Execute =>
                // Read verified swap data from the target pool's staking validator
                val poolRedeemer =
                    MerkelizedValidator.verifyAndGetRedeemer(order.poolScriptHash, tx)
                val swap = poolRedeemer.to[SwapRedeemer]

                // PRODUCTION: prevent "double satisfaction" by linking each order to a specific
                // output index (passed in the redeemer) instead of summing all outputs to the owner.
                // Without this, when the same owner has multiple orders in one transaction,
                // a malicious batcher could satisfy both validators with a single output.
                val ownerCredential = Credential.PubKeyCredential(order.owner)
                val ownerOutput = tx.outputs.foldLeft(BigInt(0)) { (sum, output) =>
                    if output.address.credential === ownerCredential then
                        sum + output.value.getLovelace
                    else sum
                }

                // Slippage protection
                require(
                  ownerOutput >= order.minOutputLovelace,
                  "Output below minimum (slippage exceeded)"
                )

                // Fair share: ownerOutput / totalOutput >= inputTokenAmount / totalInput
                // Cross-multiplied to avoid on-chain division:
                require(
                  ownerOutput * swap.totalInputAmount >= order.inputTokenAmount * swap.totalOutputAmount,
                  "Order not filled at fair rate"
                )

            case OrderRedeemer.Cancel =>
                // PRODUCTION: add a deadline field to OrderDatum and allow anyone to
                // reclaim the order after expiry, preventing stuck orders.
                require(
                  tx.signatories.exists(pkh => pkh === order.owner),
                  "Cancel requires owner signature"
                )
    }
}

// -- Compilation --

private object MultiPoolDexCompilation {
    private given options: Options = Options.release
    lazy val poolContract = PlutusV3.compile(PoolSwapValidator.validate)
    lazy val orderContract = PlutusV3.compile(OrderValidator.validate)
}

lazy val PoolSwapContract = MultiPoolDexCompilation.poolContract
lazy val OrderContract = MultiPoolDexCompilation.orderContract
