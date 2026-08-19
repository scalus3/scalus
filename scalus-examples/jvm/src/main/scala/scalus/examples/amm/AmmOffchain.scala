package scalus.examples.amm

import scalus.cardano.address.Address
import scalus.cardano.ledger.*
import scalus.cardano.txbuilder.*
import scalus.uplc.PlutusV3
import scalus.uplc.builtin.Data.toData
import scalus.uplc.builtin.Data

/** Off-chain utilities for interacting with the AMM contract. */
case class AmmOffchain(
    env: CardanoInfo,
    evaluator: PlutusScriptEvaluator,
    contract: PlutusV3[Data => Data => Unit],
    params: AmmParams
) {
    private val appliedScript: PlutusV3[Data => Unit] = contract.apply(params.toData)

    val script: Script.PlutusV3 = appliedScript.script
    val policyId: PolicyId = appliedScript.script.scriptHash
    val scriptAddress: Address = appliedScript.address(env.network)

    // Single source of truth: the LP name the on-chain minting policy pins.
    val lpAssetName: AssetName = AssetName(AmmValidator.lpTokenName)

    def readPoolDatum(utxo: Utxo): AmmDatum =
        utxo.output.inlineDatum
            .getOrElse(throw new Exception(s"Pool UTxO has no inline datum: $utxo"))
            .to[AmmDatum]

    def findPool(utxos: Iterable[Utxo]): Utxo =
        utxos
            .find(_.output.address == scriptAddress)
            .getOrElse(throw new Exception("Pool UTxO not found"))

    /** Constructs the on-chain [[Value]] for the pool output from explicit reserves. */
    private def poolValue(r0: BigInt, r1: BigInt, lovelace: Long): Value = {
        val (p0, n0) = params.t0
        val (p1, n1) = params.t1
        Value.assets(
          Map(
            ScriptHash.fromArray(p0.bytes) -> Map(AssetName(n0) -> r0.toLong),
            ScriptHash.fromArray(p1.bytes) -> Map(AssetName(n1) -> r1.toLong)
          ),
          lovelace = Coin(lovelace)
        )
    }

    /** Initializes an AMM with an empty pool and lp = 0. */
    def createEmptyPool(
        utxos: Utxos,
        lovelace: Long,
        sponsor: Address,
        signer: TransactionSigner
    ): Transaction = {
        val emptyDatum = AmmDatum(r0 = BigInt(0), r1 = BigInt(0), lpSupply = BigInt(0))
        TxBuilder(env, evaluator)
            .payTo(scriptAddress, Value.lovelace(lovelace), emptyDatum)
            .complete(utxos, sponsor)
            .sign(signer)
            .transaction
    }

    /** Deposits `x0` of t0 and `x1` of t1 into the pool, minting LP tokens to the sender.
      *
      * If the pool is empty (lpSupply == 0), mints `sqrt(x0 * x1)` lp tokens. On subsequent
      * deposits, the ratio `x0 / x1` must match the current reserves and LP tokens are minted
      * proportionally.
      */
    def deposit(
        utxos: Utxos,
        poolUtxo: Utxo,
        x0: Long,
        x1: Long,
        sponsor: Address,
        signer: TransactionSigner
    ): Transaction = {
        val d = readPoolDatum(poolUtxo)
        // Same math the on-chain validator will re-check, so the datum can't drift.
        val newDatum = AmmMath.depositDatum(d, x0, x1)
        val lpMinted: Long = (newDatum.lpSupply - d.lpSupply).toLong
        val newValue = poolValue(newDatum.r0, newDatum.r1, poolUtxo.output.value.coin.value)
        val spendRedeemer = AmmRedeemer.Deposit(x0, x1).toData
        val mintRedeemer = ().toData

        TxBuilder(env, evaluator)
            .spend(poolUtxo, _ => spendRedeemer, script)
            .mint(script, Map(lpAssetName -> lpMinted), _ => mintRedeemer)
            .payTo(scriptAddress, newValue, newDatum)
            .complete(utxos, sponsor)
            .sign(signer)
            .transaction
    }

    /** Burns lp tokens and returns proportional amounts of t0 and t1 to the `sponsor`. */
    def redeem(
        utxos: Utxos,
        poolUtxo: Utxo,
        lp: Long,
        sponsor: Address,
        signer: TransactionSigner
    ): Transaction = {
        val d = readPoolDatum(poolUtxo)
        val newDatum = AmmMath.redeemDatum(d, lp)
        val newValue = poolValue(newDatum.r0, newDatum.r1, poolUtxo.output.value.coin.value)
        val spendRedeemer = AmmRedeemer.Redeem(lp).toData
        val mintRedeemer = ().toData

        TxBuilder(env, evaluator)
            .spend(poolUtxo, _ => spendRedeemer, script)
            .mint(script, Map(lpAssetName -> -lp), _ => mintRedeemer)
            .payTo(scriptAddress, newValue, newDatum)
            .complete(utxos, sponsor)
            .sign(signer)
            .transaction
    }

    /** Swaps `amountIn` of one token for the other, subject to `minAmountOut` slippage protection.
      */
    def swap(
        utxos: Utxos,
        poolUtxo: Utxo,
        t0In: Boolean,
        amountIn: Long,
        minAmountOut: Long,
        sponsor: Address,
        signer: TransactionSigner
    ): Transaction = {
        val d = readPoolDatum(poolUtxo)
        val (_, newDatum) =
            AmmMath.swapResult(
              d,
              params.feeNumerator,
              params.feeDenominator,
              t0In,
              amountIn
            )
        val newValue = poolValue(newDatum.r0, newDatum.r1, poolUtxo.output.value.coin.value)
        val spendRedeemer =
            AmmRedeemer.Swap(t0In, amountIn, minAmountOut).toData

        TxBuilder(env, evaluator)
            .spend(poolUtxo, _ => spendRedeemer, script)
            .payTo(scriptAddress, newValue, newDatum)
            .complete(utxos, sponsor)
            .sign(signer)
            .transaction
    }

    /** Returns `(amountOut, priceImpact)` for a hypothetical swap.
      *
      * `priceImpact` is in `[0, 1]`; multiply by 100 for a percentage.
      */
    def swapQuote(pool: Utxo, t0In: Boolean, amountIn: Long): (Long, BigDecimal) = {
        val d = readPoolDatum(pool)
        val (amountOut, _) =
            AmmMath.swapResult(
              d,
              params.feeNumerator,
              params.feeDenominator,
              t0In,
              amountIn
            )
        val (reserveIn, reserveOut) = if t0In then (d.r0, d.r1) else (d.r1, d.r0)
        val midPrice = BigDecimal(reserveOut) / BigDecimal(reserveIn)
        val executionPrice = BigDecimal(amountOut) / BigDecimal(amountIn)
        val priceImpact = (midPrice - executionPrice) / midPrice
        (amountOut.toLong, priceImpact)
    }
}
