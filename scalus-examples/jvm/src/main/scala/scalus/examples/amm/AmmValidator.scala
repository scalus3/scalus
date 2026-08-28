package scalus.examples.amm

import scalus.compiler.Compile
import scalus.uplc.builtin.{Data, FromData, ToData}
import scalus.uplc.builtin.ByteString.utf8
import scalus.cardano.onchain.plutus.v1.{PolicyId, TokenName, Value}
import scalus.cardano.onchain.plutus.v2.OutputDatum
import scalus.cardano.onchain.plutus.v2
import scalus.cardano.onchain.plutus.v3.*
import scalus.cardano.onchain.plutus.prelude.*

type TradedToken = (PolicyId, TokenName)

/** Validator parameter: identifies the token pair, fee rate, and the one-shot seed UTxO.
  *
  * `seed` makes each pool's applied script hash - and therefore its LP `policyId` - globally
  * unique: two pools can only share a `policyId` by sharing a `seed`, and a UTxO is spendable once.
  * Without it, two pools with the same token pair and fee would mint the SAME LP asset, so an
  * attacker could mint LP cheaply from one pool and redeem it against another.
  */
case class AmmParams(
    t0: TradedToken,
    t1: TradedToken,
    feeNumerator: BigInt,
    feeDenominator: BigInt,
    seed: TxOutRef
) derives FromData,
      ToData

/** Minting-policy redeemer.
  *
  *   - `Init` creates an empty pool: it consumes the one-shot `seed` and mints the single pool NFT
  *     that authenticates the pool UTxO.
  *   - `ChangeLiquidity` mints/burns LP on deposit/redeem.
  *   - `Close` burns the pool NFT when an empty pool is torn down.
  */
enum AmmMintAction derives FromData, ToData:
    case Init
    case ChangeLiquidity
    case Close

case class AmmDatum(
    r0: BigInt,
    r1: BigInt,
    lpSupply: BigInt
) derives FromData,
      ToData

@Compile
object AmmDatum {
    given Eq[AmmDatum] = Eq.structural: (a: AmmDatum, b: AmmDatum) =>
        a.r0 === b.r0 && a.r1 === b.r1 && a.lpSupply === b.lpSupply
}

/** Pure pool-transition math, shared by the on-chain validator and the off-chain tx builder.
  *
  * Because Scalus on-chain code is ordinary Scala, [[AmmOffchain]] calls these same functions to
  * build the pool datum it puts in a transaction, while [[AmmValidator]] calls them to recompute
  * the expected datum and check it. The two can never disagree on the formula — the off-chain
  * builder and the on-chain check are literally the same code.
  */
@Compile
object AmmMath {

    /** Expected pool datum after depositing `(x0, x1)`. The minted LP amount is
      * `result.lpSupply - current.lpSupply`. Does not validate the deposit ratio — callers do.
      */
    def depositDatum(current: AmmDatum, x0: BigInt, x1: BigInt): AmmDatum = {
        val lpMinted =
            if current.lpSupply === BigInt(0) then Math.sqrt(x0 * x1)
            else Math.min(x0 * current.lpSupply / current.r0, x1 * current.lpSupply / current.r1)
        AmmDatum(current.r0 + x0, current.r1 + x1, current.lpSupply + lpMinted)
    }

    /** Expected pool datum after burning `lp` LP tokens. */
    def redeemDatum(current: AmmDatum, lp: BigInt): AmmDatum =
        AmmDatum(
          r0 = current.r0 - lp * current.r0 / current.lpSupply,
          r1 = current.r1 - lp * current.r1 / current.lpSupply,
          lpSupply = current.lpSupply - lp
        )

    /** `(amountOut, expected pool datum)` after swapping `amountIn` in the given direction. */
    def swapResult(
        current: AmmDatum,
        feeNumerator: BigInt,
        feeDenominator: BigInt,
        t0In: Boolean,
        amountIn: BigInt
    ): (BigInt, AmmDatum) = {
        val dxAdjusted = amountIn * feeNumerator
        if t0In then
            val out = current.r1 * dxAdjusted / (current.r0 * feeDenominator + dxAdjusted)
            (out, AmmDatum(current.r0 + amountIn, current.r1 - out, current.lpSupply))
        else
            val out = current.r0 * dxAdjusted / (current.r1 * feeDenominator + dxAdjusted)
            (out, AmmDatum(current.r0 - out, current.r1 + amountIn, current.lpSupply))
    }
}

/** Redeemer for the spending validator. */
enum AmmRedeemer derives FromData, ToData:
    case Deposit(x0: BigInt, x1: BigInt)
    case Redeem(lp: BigInt)
    case Swap(t0In: Boolean, amountIn: BigInt, minAmountOut: BigInt)
    case Close

/** Single-script AMM validator — acts as both pool spending validator and LP minting policy.
  *
  * The `policyId` of the LP token equals the `scriptHash` of this validator. The minting endpoint
  * only verifies that the minted/burned LP delta matches `lpSupply' - lpSupply` in the pool's
  * output datum. All invariant checks are performed by the spending endpoint.
  */
@Compile
object AmmValidator extends DataParameterizedValidator {

    /** Reads the [[AmmDatum]] from an output's inline datum; fails otherwise. */
    inline def readPoolDatum(out: TxOut): AmmDatum =
        out.datum.inlineOrFail[AmmDatum]("Pool output must have inline datum")

    /** The single NFT minted at `Init` that authenticates a pool UTxO. */
    val poolNftName: TokenName = utf8"POOL"

    /** Canonical LP token name. Pinning it is what keeps the LP token a single fungible asset:
      * without a name check the mint endpoint would accept LP minted under arbitrary names (the net
      * sum still balances, but wallets/price feeds that treat "the LP token" as one asset would
      * break, and redemption fragments across names).
      */
    val lpTokenName: TokenName = utf8"LP"

    inline def mint(param: Data, redeemer: Data, policyId: PolicyId, tx: TxInfo): Unit =
        redeemer.to[AmmMintAction] match
            case AmmMintAction.Init =>
                // Consume the one-shot seed so this policyId can only ever be initialized once.
                tx.findInputOrFail(param.to[AmmParams].seed, "Init: must spend the seed UTxO")
                // Mint exactly the pool NFT and nothing else, into a fresh empty pool that holds it.
                require(
                  tx.mint.hasOnly(policyId, poolNftName, 1),
                  "Init: must mint exactly one pool NFT"
                )
                // There is no pool input yet, so the fresh pool output is located by its payment
                // credential (`policyId == scriptHash`); the staking part is the creator's choice.
                val poolOut = tx.outputs.findUniqueOrFail(
                  _.address.credential === Credential.ScriptCredential(policyId),
                  "Init: expected exactly one pool output"
                )
                require(
                  poolOut.hasInlineDatum(AmmDatum(BigInt(0), BigInt(0), BigInt(0))),
                  "Init: pool must start empty"
                )
                require(
                  poolOut.value.hasNft(policyId, poolNftName),
                  "Init: empty pool must hold the pool NFT"
                )

            case AmmMintAction.ChangeLiquidity =>
                // Locate the pool input we're spending, and its continuation.
                val poolInput = tx.inputs.findUniqueOrFail(
                  _.resolved.address.credential === Credential.ScriptCredential(policyId),
                  "Mint: expected exactly one pool input"
                )
                val poolDatum = readPoolDatum(poolInput.resolved)
                val continuationDatum = readPoolDatum(
                  tx.findContinuingOutputOrFail(poolInput, "Mint: expected exactly one pool output")
                )

                // The tx must mint/burn exactly `lpDelta` of the LP token and NOTHING else under
                // this policy: `hasOnly` pins the token name and rejects any other name in one check
                // (works for negative `lpDelta`, i.e. burns, too), so the pool NFT can't be minted
                // or burned on a liquidity change either.
                val lpDelta = continuationDatum.lpSupply - poolDatum.lpSupply
                require(tx.mint.hasOnly(policyId, lpTokenName, lpDelta), "Mint: LP delta mismatch")

            case AmmMintAction.Close =>
                // Burn exactly the pool NFT and nothing else. The spend endpoint (which runs
                // because the NFT-holding pool UTxO is spent) enforces the pool is empty.
                require(
                  tx.mint.hasOnly(policyId, poolNftName, -1),
                  "Close: must burn exactly the pool NFT"
                )
    inline def spend(
        param: Data,
        d: Option[Data],
        redeemer: Data,
        tx: TxInfo,
        ownRef: TxOutRef
    ): Unit = {
        val params = param.to[AmmParams]
        val ownInput = tx.findInputOrFail(ownRef, "Own pool input not found")
        val poolPolicyId = ownInput.resolved.address.credential
            .scriptHashOrFail("Own pool input must be script-locked")
        val datum = d.getOrFail("Pool datum missing").to[AmmDatum]

        redeemer.to[AmmRedeemer] match {
            case AmmRedeemer.Close =>
                // Tear down an empty pool: the datum must show no liquidity, and the pool NFT must
                // be burned (the mint `Close` branch checks nothing else is burned). The leftover
                // min-ADA is free for the spender to reclaim.
                require(
                  datum === AmmDatum(BigInt(0), BigInt(0), BigInt(0)),
                  "Close: pool must be empty"
                )
                require(
                  tx.mint.quantityOf(poolPolicyId, poolNftName) === BigInt(-1),
                  "Close: pool NFT must be burned"
                )

            case action =>
                val poolOutput =
                    tx.findContinuingOutputOrFail(ownInput, "Expected exactly one pool output")
                // Each handler computes the datum the continuing output must carry and checks it
                // with `hasInlineDatum` (one data equality, no decoding); the value it returns is
                // that verified datum, so the reserve binding below can read its fields.
                val newDatum = action match {
                    case AmmRedeemer.Deposit(x0, x1) =>
                        require(x0 > 0 && x1 > 0, "Deposit: amounts must be positive")
                        if datum.lpSupply !== BigInt(0) then
                            require(x0 * datum.r1 === x1 * datum.r0, "Deposit: ratio mismatch")

                        val expectedDatum = AmmMath.depositDatum(datum, x0, x1)
                        require(
                          expectedDatum.lpSupply - datum.lpSupply > 0,
                          "Deposit: zero LP minted"
                        )
                        require(
                          poolOutput.hasInlineDatum(expectedDatum),
                          "Deposit: output datum mismatch"
                        )
                        expectedDatum
                    case AmmRedeemer.Redeem(lp) =>
                        // We don't check where the redeemed tokens go: the ledger already guarantees
                        // the tx balances, and the reserve binding below ties the new datum reserves
                        // to the continuing pool output's actual token quantities, so the pool cannot
                        // be under-funded. We only validate the datum transition here (same for Swap).
                        require(lp > 0, "Redeem: LP amount must be positive")
                        require(lp <= datum.lpSupply, "Redeem: LP amount exceeds supply")

                        val expectedDatum = AmmMath.redeemDatum(datum, lp)
                        require(
                          poolOutput.hasInlineDatum(expectedDatum),
                          "Redeem: output datum mismatch"
                        )
                        expectedDatum

                    case AmmRedeemer.Swap(t0In, amountIn, minAmountOut) =>
                        // As with Redeem, we validate only the datum transition; the reserve binding
                        // below ties the new reserves to the pool output's actual token quantities.
                        require(amountIn > 0, "Swap: amountIn must be positive")

                        val (amountOut, expectedDatum) =
                            AmmMath.swapResult(
                              datum,
                              params.feeNumerator,
                              params.feeDenominator,
                              t0In,
                              amountIn
                            )

                        require(amountOut >= minAmountOut, "Swap: slippage exceeded")
                        require(
                          expectedDatum.r0 * expectedDatum.r1 >= datum.r0 * datum.r1,
                          "Swap: invariant violated"
                        )
                        require(
                          poolOutput.hasInlineDatum(expectedDatum),
                          "Swap: output datum mismatch"
                        )
                        expectedDatum
                    case AmmRedeemer.Close => fail("unreachable")
                }

                // Bind the datum reserves to the tokens actually held by the continuing pool output.
                // The handlers above only check the datum arithmetic; without this an attacker can
                // write a valid-looking datum while sending the real reserve tokens elsewhere,
                // draining the pool.
                require(
                  poolOutput.value.quantityOf(params.t0._1, params.t0._2) === newDatum.r0,
                  ReserveT0Mismatch
                )
                require(
                  poolOutput.value.quantityOf(params.t1._1, params.t1._2) === newDatum.r1,
                  ReserveT1Mismatch
                )
                // The pool NFT must stay with the pool - it can only be burned via `Close`.
                require(
                  poolOutput.value.hasNft(poolPolicyId, poolNftName),
                  "Pool output must retain the pool NFT"
                )
        }
    }

    private inline val ReserveT0Mismatch = "Pool output must hold r0 of token0"
    private inline val ReserveT1Mismatch = "Pool output must hold r1 of token1"
}
