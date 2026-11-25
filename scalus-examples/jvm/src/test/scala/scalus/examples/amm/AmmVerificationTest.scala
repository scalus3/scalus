package scalus.examples.amm

import org.scalatest.funsuite.AnyFunSuite
import scalus.*
import scalus.verify.*
import scalus.verify.Props.*
import scalus.verify.uplcblaster.{Budget, LeanManual, LeanProofs, LeanProofsSuite, UplcBlaster}

import java.nio.file.Path

/** Properties of the AMM's pool arithmetic, [[AmmMath]], proved about its compiled UPLC with the
  * `UplcBlaster` tactic (see `docs/design/verification-details/uplc-blaster.md`).
  *
  * Each function of [[AmmMath]] states its specification in its code, with `ensuring`. A statement
  * here is that clause, as the verifier reads it from the function's SIR, and a negative control
  * beside it: a claim close to the clause that is false, and is refuted.
  *
  * The arithmetic multiplies and divides unknown values, which the solver decides less readily than
  * the linear arithmetic of the other examples. What Lean does not finish here is what a proof
  * written by hand is for, in the example's workspace (phase 2a of
  * `docs/design/verification-overview.md`).
  *
  * The functions are compiled with the tactic's options (`UplcBlaster.options`), which differ from
  * those of [[AmmContract]]: the statements are about the same source, not about the published
  * script's bytes.
  *
  * Without Lean, that is without `lake` and a built workspace, the tests pass on the results kept
  * beside this file (see [[LeanProofs]]).
  */
class AmmVerificationTest extends AnyFunSuite with LeanProofsSuite {
    import proofs.*

    /** The example's own Lean workspace, `src/test/lean/Amm`. It requires Scalus's Lean library,
      * and has that library's packages where the library's workspace has them.
      */
    override protected def leanWorkspace: Path = AmmVerificationTest.workspace

    /** The results of the statements below are kept beside this file: a run without Lean passes on
      * them, and sees at once where the code is no longer the one they are of.
      */
    override protected def keptResultsFile: Option[Path] = Some(
      LeanProofs
          .inSources("scalus-examples", "jvm", "src", "test", "scala", "scalus", "examples", "amm")
          .resolve("AmmVerificationTest.proofs.json")
    )

    private val swapResult = FunctionDef(AmmMath.swapResult)
    private val redeemDatum = FunctionDef(AmmMath.redeemDatum)
    private val depositDatum = FunctionDef(AmmMath.depositDatum)

    verifier.addFunction(swapResult)
    verifier.addFunction(redeemDatum)
    verifier.addFunction(depositDatum)

    // A swap

    /** The contract `swapResult` states in its body. A statement of the contract, and not of its
      * `prop`, so the verifier knows that it checks the `ensuring` clause it was read from.
      */
    private val swapContractHolds = FunctionContract
        .inSource(swapResult)
        .map(stated => verifier.statement("amm.swap.stated_contract_holds", stated))

    /** The product of the reserves needs the fee's condition: where more than the amount in counts
      * (`feeNumerator > feeDenominator`), a swap takes more out than the product allows.
      */
    private val productKeptWithoutTheFeeBound = verifier.refute(
      "amm.swap.product_kept_without_the_fee_bound",
      contract(swapResult)(
        expects = (current, feeNumerator, feeDenominator, t0In, amountIn) =>
            current.r0 >= BigInt(0) && current.r1 >= BigInt(0) && amountIn >= BigInt(0)
                && feeNumerator >= BigInt(0),
        ensures = (current, feeNumerator, feeDenominator, t0In, amountIn) =>
            result => result._2.r0 * result._2.r1 >= current.r0 * current.r1
      ).prop
    )

    test("a swap keeps the product of the reserves, and does not drain the pool") {
        val contract = verify(swapContractHolds.getOrElse(fail("swapResult states no contract")))
        assert(contract.isProven, describe(contract))
        // negative control: without the fee's bound, the product falls.
        val withoutTheFeeBound = verify(productKeptWithoutTheFeeBound)
        assert(withoutTheFeeBound.isRefuted, describe(withoutTheFeeBound))
    }

    // A redemption

    /** The contract `redeemDatum` states in its body. */
    private val redeemContractHolds = FunctionContract
        .inSource(redeemDatum)
        .map(stated => verifier.statement("amm.redeem.stated_contract_holds", stated))

    /** The reserves need the supply's bound: a redemption of more than the supply takes more than
      * the pool has.
      */
    private val reservesKeptWithoutTheSupplyBound = verifier.refute(
      "amm.redeem.reserves_kept_without_the_supply_bound",
      contract(redeemDatum)(
        expects =
            (current, lp) => current.r0 >= BigInt(0) && current.r1 >= BigInt(0) && lp >= BigInt(0),
        ensures = (current, lp) => next => next.r0 >= BigInt(0) && next.r1 >= BigInt(0)
      ).prop
    )

    test("a redemption takes no more than its share") {
        val contract = verify(redeemContractHolds.getOrElse(fail("redeemDatum states no contract")))
        assert(contract.isProven, describe(contract))
        // negative control: a redemption of more than the supply.
        val withoutTheSupplyBound = verify(reservesKeptWithoutTheSupplyBound)
        assert(withoutTheSupplyBound.isRefuted, describe(withoutTheSupplyBound))
    }

    // A deposit

    /** The contract `depositDatum` states in its body. Lean does not check it: the first deposit
      * mints `Math.sqrt(x0 * x1)`, whose `log2` calls `integerToByteString`, and Blaster does not
      * translate the `Nat.log2` in PlutusCore's model of that builtin
      * (input-output-hk/Lean-blaster#273). The function's program has that branch, which the
      * clause's condition excludes but the solver is given all the same.
      *
      * Nor does any proof check it as written: its test computes the deposit before it reads the
      * condition, and the square root of a first deposit loops longer than any fixed number of
      * steps on large enough amounts. With the condition as a premise, it is
      * [[depositDilutesNoToken]], proved by hand.
      */
    private val depositContractHolds = FunctionContract
        .inSource(depositDatum)
        .map(stated => verifier.statement("amm.deposit.stated_contract_holds", stated))

    depositContractHolds.foreach(statement =>
        verifier.open(
          statement,
          "as written it holds at no budget (Math.sqrt loops on the first deposit, which the " +
              "condition excludes); with the condition as a premise it is " +
              "amm.deposit.dilutes_no_token, proved by hand"
        )
    )

    /** The deposit's clause with its condition as a premise: of a pool with liquidity, and amounts
      * that are not negative. The deposit then mints in proportion, and takes no square root, so
      * the statement holds at a budget that covers that run. Blaster cannot check it: the program
      * still has the square root, which it does not translate. It is proved by hand, in
      * `src/test/lean/Amm/Amm/Proofs/Deposit.lean`.
      */
    private val depositDilutesNoToken = verifier.statement(
      "amm.deposit.dilutes_no_token",
      contract(depositDatum)(
        expects = (current, x0, x1) =>
            current.r0 > BigInt(0) && current.r1 > BigInt(0) && current.lpSupply > BigInt(0)
                && x0 >= BigInt(0) && x1 >= BigInt(0),
        ensures = (current, x0, x1) =>
            next =>
                next.r0 * current.lpSupply >= current.r0 * next.lpSupply
                    && next.r1 * current.lpSupply >= current.r1 * next.lpSupply
      ).prop
    )

    /** The steps of the statement proved by hand: they cover a deposit into a pool with liquidity,
      * whose claim takes 473 steps of Lean's machine, whatever the numbers (`measureRun`). Its
      * module runs the square root of a first deposit symbolically for as many steps, which the
      * proof never reads: a larger budget only makes the module take longer to build.
      */
    private val depositByHandBudget = 500

    test("a deposit into a pool with liquidity dilutes no LP token: a proof by hand") {
        val result = verifyByHand(
          depositDilutesNoToken,
          "Amm.Proofs.Deposit",
          "Amm.Proofs.Deposit.deposit_dilutes_no_token",
          depositByHandBudget
        )
        result match
            case VerificationResult.Proven(proof) =>
                // Lean's kernel, of the runs as Blaster optimizes them: no solver, and no lemma.
                assert(proof.kinds == Set(ProofKind.LeanKernelOfOptimizedRuns))
                assert(proof.usedLemmas.isEmpty)
            case other => fail(s"expected a proof, got $other")
    }

    test("a theorem of another statement is no proof by hand of the deposit's") {
        // negative control, apart from the proof: a refused proof is no result that is kept, so it
        // needs Lean, and the proof passes on its kept result without Lean.
        val result = verifyWithoutKeeping(
          depositDilutesNoToken,
          LeanManual(
            "Amm.Proofs.Redeem",
            "Amm.Proofs.Redeem.redeem_contract",
            depositByHandBudget,
            lean
          )
        )
        assert(result.isFailed, describe(result))
        val reason = result.why.getOrElse("")
        assert(reason.toLowerCase.contains("type mismatch"), reason)
    }

    /** The steps of the check of the deposit: the run reaches `log2` within them. */
    private val depositBudget = 600

    test("Lean does not check yet that a deposit dilutes no LP token") {
        // Not tagged `Unfinished`: it fails at once, and it fails no longer where Blaster
        // translates `Nat.log2`, which says to prove the clause.
        val result = verifyWithoutKeeping(
          depositContractHolds.getOrElse(fail("depositDatum states no contract")),
          UplcBlaster(Budget.LeanSteps(depositBudget), lean)
        )
        assert(result.isFailed, describe(result))
        val reason = result.why.getOrElse("")
        assert(reason.contains("translateApp: unexpected application"), reason)
        assert(reason.contains("Init.Data.Nat.Log2"), reason)
    }
}

object AmmVerificationTest {

    /** The example's own Lean workspace, `src/test/lean/Amm`, in which the suite runs its checks.
      */
    def workspace: Path =
        LeanProofs.inSources("scalus-examples", "jvm", "src", "test", "lean", "Amm")
}
