package scalus.examples.amm

import scalus.compiler.Compile
import scalus.uplc.builtin.{Data, FromData, ToData}
import scalus.cardano.onchain.plutus.v1.{PolicyId, TokenName, Value}
import scalus.cardano.onchain.plutus.v2.OutputDatum
import scalus.cardano.onchain.plutus.v2
import scalus.cardano.onchain.plutus.v3.*
import scalus.cardano.onchain.plutus.prelude.*

type TradedToken = (PolicyId, TokenName)

/** Validator parameter: identifies the token pair and fee rate. */
case class AmmParams(
    t0: TradedToken,
    t1: TradedToken,
    feeNumerator: BigInt,
    feeDenominator: BigInt
) derives FromData,
      ToData

case class AmmDatum(
    r0: BigInt,
    r1: BigInt,
    lpSupply: BigInt
) derives FromData,
      ToData

@Compile
object AmmDatum {
    given Eq[AmmDatum] = (a: AmmDatum, b: AmmDatum) =>
        a.r0 === b.r0 && a.r1 === b.r1 && a.lpSupply === b.lpSupply
}

/** Redeemer for the spending validator. */
enum AmmRedeemer derives FromData, ToData:
    case Deposit(x0: BigInt, x1: BigInt)
    case Redeem(lp: BigInt)
    case Swap(t0In: Boolean, amountIn: BigInt, minAmountOut: BigInt)

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
        out.datum match
            case OutputDatum.OutputDatum(d) => d.to[AmmDatum]
            case _                          => fail("Pool output must have inline datum")

    /** Finds the unique pool output at `addr`; fails if absent or ambiguous. */
    inline def findPoolOutput(outputs: List[TxOut], addr: Address): TxOut = {
        val matching = outputs.filter(_.address === addr)
        matching match
            case List.Cons(out, List.Nil) => out
            case List.Nil                 => fail("No pool output found")
            case _                        => fail("Multiple pool outputs found")
    }

    // mints LP tokens
    inline def mint(param: Data, redeemer: Data, policyId: PolicyId, tx: TxInfo): Unit = {
        // Locate the pool input that we're spending
        val poolInputOpt = tx.inputs.find { inp =>
            inp.resolved.address.credential match
                case Credential.ScriptCredential(sh) => sh === policyId
                case _                               => false
        }
        val poolInput = poolInputOpt.getOrFail("Mint: no pool input found")
        val poolDatum = readPoolDatum(poolInput.resolved)

        val poolAddress = poolInput.resolved.address
        val continuationOut = findPoolOutput(tx.outputs, poolAddress)
        val continuationDatum = readPoolDatum(continuationOut)

        // LP delta must match actual minted/burned amount for this policyId.
        val lpDelta = continuationDatum.lpSupply - poolDatum.lpSupply
        val actualDelta = tx.mint.tokens(policyId).toList.foldLeft(BigInt(0)) { (acc, pair) =>
            acc + pair._2
        }
        require(actualDelta === lpDelta, "Mint: LP delta mismatch")
    }
    inline def spend(
        param: Data,
        d: Option[Data],
        redeemer: Data,
        tx: TxInfo,
        ownRef: TxOutRef
    ): Unit = {
        val params = param.to[AmmParams]
        val action = redeemer.to[AmmRedeemer]

        val ownInput = tx.findOwnInputOrFail(ownRef, "Own pool input not found")
        val poolAddress = ownInput.resolved.address

        val datum = d.getOrFail("Pool datum missing").to[AmmDatum]
        val poolOutput = findPoolOutput(tx.outputs, poolAddress)
        val newDatum = readPoolDatum(poolOutput)

        action match {
            case AmmRedeemer.Deposit(x0, x1) =>
                handleDeposit(params, datum, newDatum, x0, x1)
            case AmmRedeemer.Redeem(lp) =>
                handleRedeem(datum, newDatum, lp)
            case AmmRedeemer.Swap(t0In, amountIn, minAmountOut) =>
                handleSwap(params, datum, newDatum, t0In, amountIn, minAmountOut)
        }
    }

    private inline def handleDeposit(
        params: AmmParams,
        datum: AmmDatum,
        newDatum: AmmDatum,
        x0: BigInt,
        x1: BigInt
    ): Unit = {
        require(x0 > BigInt(0) && x1 > BigInt(0), "Deposit: amounts must be positive")

        val lpMinted =
            if datum.lpSupply === BigInt(0) then Math.sqrt(x0 * x1)
            else {
                require(x0 * datum.r1 === x1 * datum.r0, "Deposit: ratio mismatch")
                val lp0 = x0 * datum.lpSupply / datum.r0
                val lp1 = x1 * datum.lpSupply / datum.r1
                Math.min(lp0, lp1)
            }

        require(lpMinted > BigInt(0), "Deposit: zero LP minted")

        val expectedDatum = AmmDatum(
          r0 = datum.r0 + x0,
          r1 = datum.r1 + x1,
          lpSupply = datum.lpSupply + lpMinted
        )
        require(newDatum === expectedDatum, "Deposit: output datum mismatch")
    }

    private inline def handleRedeem(
        datum: AmmDatum,
        newDatum: AmmDatum,
        lp: BigInt
    ): Unit = {
        // Notice that we intentionally don't check where the redeemed tokens are going.
        // As long as the transaction passed the phase-1 checks, we know that it's balanced, and don't have to
        // check anything other than the pool invariants. Similar thing occurs in `handleSwap`.

        require(lp > BigInt(0), "Redeem: LP amount must be positive")
        require(lp <= datum.lpSupply, "Redeem: LP amount exceeds supply")

        val out0 = lp * datum.r0 / datum.lpSupply
        val out1 = lp * datum.r1 / datum.lpSupply

        val expectedDatum = AmmDatum(
          r0 = datum.r0 - out0,
          r1 = datum.r1 - out1,
          lpSupply = datum.lpSupply - lp
        )
        require(newDatum === expectedDatum, "Redeem: output datum mismatch")
    }

    private inline def handleSwap(
        params: AmmParams,
        datum: AmmDatum,
        newDatum: AmmDatum,
        t0In: Boolean,
        amountIn: BigInt,
        minAmountOut: BigInt
    ): Unit = {
        // Notice that we intentionally don't check where the redeemed tokens are going.
        // As long as the transaction passed the phase-1 checks, we know that it's balanced, and don't have to
        // check anything other than the pool invariants. Similar thing occurs in `handleRedeem`.

        require(amountIn > BigInt(0), "Swap: amountIn must be positive")

        val dxAdjusted = amountIn * params.feeNumerator

        val (amountOut, newR0, newR1) =
            if t0In then
                val out = datum.r1 * dxAdjusted / (datum.r0 * params.feeDenominator + dxAdjusted)
                (out, datum.r0 + amountIn, datum.r1 - out)
            else
                val out = datum.r0 * dxAdjusted / (datum.r1 * params.feeDenominator + dxAdjusted)
                (out, datum.r0 - out, datum.r1 + amountIn)

        require(amountOut >= minAmountOut, "Swap: slippage exceeded")
        require(newR0 * newR1 >= datum.r0 * datum.r1, "Swap: invariant violated")

        val expectedDatum = AmmDatum(r0 = newR0, r1 = newR1, lpSupply = datum.lpSupply)
        require(newDatum === expectedDatum, "Swap: output datum mismatch")
    }
}
