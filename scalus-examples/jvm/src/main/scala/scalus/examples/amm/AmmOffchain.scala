package scalus.examples.amm

import scalus.cardano.address.Address
import scalus.cardano.ledger.*
import scalus.cardano.onchain.plutus.prelude.Math
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

    val lpAssetName: AssetName = AssetName.fromString("lp")

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
        val lpMinted: Long =
            if d.lpSupply == BigInt(0) then Math.sqrt(BigInt(x0) * BigInt(x1)).toLong
            else {
                val lp0 = (BigInt(x0) * d.lpSupply / d.r0).toLong
                val lp1 = (BigInt(x1) * d.lpSupply / d.r1).toLong
                lp0 min lp1
            }
        val newDatum = AmmDatum(
          r0 = d.r0 + BigInt(x0),
          r1 = d.r1 + BigInt(x1),
          lpSupply = d.lpSupply + BigInt(lpMinted)
        )
        val newValue = poolValue(newDatum.r0, newDatum.r1, poolUtxo.output.value.coin.value)
        val spendRedeemer = AmmRedeemer.Deposit(BigInt(x0), BigInt(x1)).toData
        val mintRedeemer = ().toData

        TxBuilder(env, evaluator)
            .spend(poolUtxo, _ => spendRedeemer, script, Set.empty)
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
        val out0 = (BigInt(lp) * d.r0 / d.lpSupply).toLong
        val out1 = (BigInt(lp) * d.r1 / d.lpSupply).toLong
        val newDatum = AmmDatum(
          r0 = d.r0 - BigInt(out0),
          r1 = d.r1 - BigInt(out1),
          lpSupply = d.lpSupply - BigInt(lp)
        )
        val newValue = poolValue(newDatum.r0, newDatum.r1, poolUtxo.output.value.coin.value)
        val spendRedeemer = AmmRedeemer.Redeem(BigInt(lp)).toData
        val mintRedeemer = ().toData

        TxBuilder(env, evaluator)
            .spend(poolUtxo, _ => spendRedeemer, script, Set.empty)
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
        val dxAdj = BigInt(amountIn) * params.feeNumerator
        val (_, newR0, newR1) =
            if t0In then
                val out = d.r1 * dxAdj / (d.r0 * params.feeDenominator + dxAdj)
                (out, d.r0 + BigInt(amountIn), d.r1 - out)
            else
                val out = d.r0 * dxAdj / (d.r1 * params.feeDenominator + dxAdj)
                (out, d.r0 - out, d.r1 + BigInt(amountIn))
        val newDatum = AmmDatum(r0 = newR0, r1 = newR1, lpSupply = d.lpSupply)
        val newValue = poolValue(newDatum.r0, newDatum.r1, poolUtxo.output.value.coin.value)
        val spendRedeemer =
            AmmRedeemer.Swap(t0In, BigInt(amountIn), BigInt(minAmountOut)).toData

        TxBuilder(env, evaluator)
            .spend(poolUtxo, _ => spendRedeemer, script, Set.empty)
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
        val dxAdj = BigInt(amountIn) * params.feeNumerator
        val amountOut =
            if t0In then d.r1 * dxAdj / (d.r0 * params.feeDenominator + dxAdj)
            else d.r0 * dxAdj / (d.r1 * params.feeDenominator + dxAdj)
        val (reserveIn, reserveOut) = if t0In then (d.r0, d.r1) else (d.r1, d.r0)
        val midPrice = BigDecimal(reserveOut) / BigDecimal(reserveIn)
        val executionPrice = BigDecimal(amountOut) / BigDecimal(amountIn)
        val priceImpact = (midPrice - executionPrice) / midPrice
        (amountOut.toLong, priceImpact)
    }
}
