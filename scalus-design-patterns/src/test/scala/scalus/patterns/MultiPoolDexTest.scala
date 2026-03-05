package scalus.patterns

import org.scalatest.funsuite.AnyFunSuite
import scalus.*
import scalus.uplc.builtin.Data.toData
import scalus.uplc.builtin.ByteString
import scalus.cardano.ledger.{ExUnits, Script}
import scalus.examples.{OrderContract, OrderDatum, OrderRedeemer, PoolSwapContract, SwapRedeemer}
import scalus.cardano.onchain.plutus.v1.Credential.{PubKeyCredential, ScriptCredential}
import scalus.cardano.onchain.plutus.v1.{Address, Credential, PubKeyHash, Value}
import scalus.cardano.onchain.plutus.v2.{OutputDatum, TxOut}
import scalus.cardano.onchain.plutus.v3.*
import scalus.cardano.onchain.plutus.v3.ScriptInfo.{RewardingScript, SpendingScript}
import scalus.cardano.onchain.plutus.prelude.{List, Option as POption, SortedMap}
import scalus.testing.kit.ScalusTest

/** Tests for the Multi-Pool DEX Example.
  *
  * Demonstrates multiple withdraw-zero scripts in a single transaction: each liquidity pool has its
  * own staking validator, and orders targeting different pools are settled simultaneously.
  */
class MultiPoolDexTest extends AnyFunSuite with ScalusTest {

    // Pool contracts parameterized by different pool IDs → different script hashes
    private val poolAId = ByteString.fromHex("aa")
    private val poolBId = ByteString.fromHex("bb")

    private val poolAContract = PoolSwapContract.withErrorTraces
    private val poolBContract = PoolSwapContract.withErrorTraces

    private val poolAApplied = poolAContract.program $ poolAId.toData
    private val poolBApplied = poolBContract.program $ poolBId.toData

    private val poolAHash = Script.PlutusV3(poolAApplied).scriptHash
    private val poolBHash = Script.PlutusV3(poolBApplied).scriptHash

    private val orderContract = OrderContract.withErrorTraces

    private val orderTxId = random[TxId]
    private val txId = random[TxId]

    // Test users
    private val alice = PubKeyHash(genByteStringOfN(28).sample.get)
    private val bob = PubKeyHash(genByteStringOfN(28).sample.get)
    private val carol = PubKeyHash(genByteStringOfN(28).sample.get)
    private val orderScriptHash = orderContract.script.scriptHash

    // -- Pool validator tests --

    test("pool: success - valid constant product swap") {
        val swap = SwapRedeemer(
          totalInputAmount = 100,
          totalOutputAmount = 90,
          reserveABefore = 1000,
          reserveBBefore = 1000
        )
        // k_before = 1000 * 1000 = 1_000_000
        // k_after = (1000 + 100) * (1000 - 90) = 1100 * 910 = 1_001_000
        val result = evalPool(poolAApplied, poolAHash, swap)
        checkResult(success, result)
    }

    test("pool: failure - constant product violated") {
        val swap = SwapRedeemer(
          totalInputAmount = 100,
          totalOutputAmount = 200, // too much output
          reserveABefore = 1000,
          reserveBBefore = 1000
        )
        // k_after = 1100 * 800 = 880_000 < 1_000_000
        val result = evalPool(poolAApplied, poolAHash, swap)
        checkResult(failure("Constant product invariant violated"), result)
    }

    test("pool: failure - insufficient liquidity") {
        val swap = SwapRedeemer(
          totalInputAmount = 100,
          totalOutputAmount = 1001, // more than reserve
          reserveABefore = 1000,
          reserveBBefore = 1000
        )
        val result = evalPool(poolAApplied, poolAHash, swap)
        checkResult(failure("Insufficient pool liquidity"), result)
    }

    // -- Order validator tests with single pool --

    test("order: success - single order filled at fair rate") {
        // Pool: 1000 token reserve, 1000 ADA reserve
        // User sells 100 tokens, pool gives 90 ADA
        // Fair share: 100/100 * 90 = 90 ADA (user is the only seller)
        OrderTestCase(
          orders = scala.List(
            Order(alice, poolAHash, inputTokens = 100, minOutput = 80, outputAda = 90)
          ),
          pools = scala.List(
            Pool(
              poolAHash,
              poolAApplied,
              totalInput = 100,
              totalOutput = 90,
              resA = 1000,
              resB = 1000
            )
          ),
          expectedResult = success
        ).run()
    }

    test("order: failure - slippage exceeded") {
        OrderTestCase(
          orders = scala.List(
            Order(alice, poolAHash, inputTokens = 100, minOutput = 95, outputAda = 90)
          ),
          pools = scala.List(
            Pool(
              poolAHash,
              poolAApplied,
              totalInput = 100,
              totalOutput = 90,
              resA = 1000,
              resB = 1000
            )
          ),
          expectedResult = failure("Output below minimum (slippage exceeded)")
        ).run()
    }

    test("order: failure - unfair rate") {
        // User provides 50 of 100 total input but receives only 30 of 90 total output
        // Fair share: 50/100 * 90 = 45, but user only gets 30
        OrderTestCase(
          orders = scala.List(
            Order(alice, poolAHash, inputTokens = 50, minOutput = 25, outputAda = 30)
          ),
          pools = scala.List(
            Pool(
              poolAHash,
              poolAApplied,
              totalInput = 100,
              totalOutput = 90,
              resA = 1000,
              resB = 1000
            )
          ),
          expectedResult = failure("Order not filled at fair rate")
        ).run()
    }

    test("order: success - cancel with owner signature") {
        val order = OrderDatum(
          owner = alice,
          poolScriptHash = poolAHash,
          minOutputLovelace = 80,
          inputTokenAmount = 100
        )
        val txInfo = TxInfo(
          inputs = List(
            TxInInfo(
              outRef = TxOutRef(orderTxId, 0),
              resolved = TxOut(
                address = Address(ScriptCredential(orderScriptHash), POption.None),
                value = Value.lovelace(100),
                datum = OutputDatum.OutputDatum(order.toData)
              )
            )
          ),
          fee = BigInt(200000),
          signatories = List(alice),
          id = txId
        )
        val ctx = ScriptContext(
          txInfo = txInfo,
          redeemer = OrderRedeemer.Cancel.toData,
          scriptInfo = SpendingScript(
            txOutRef = TxOutRef(orderTxId, 0),
            datum = POption.Some(order.toData)
          )
        )
        val result = (orderContract.program $ ctx.toData).evaluateDebug
        checkResult(success, result)
    }

    test("order: failure - cancel without owner signature") {
        val order = OrderDatum(
          owner = alice,
          poolScriptHash = poolAHash,
          minOutputLovelace = 80,
          inputTokenAmount = 100
        )
        val txInfo = TxInfo(
          inputs = List(
            TxInInfo(
              outRef = TxOutRef(orderTxId, 0),
              resolved = TxOut(
                address = Address(ScriptCredential(orderScriptHash), POption.None),
                value = Value.lovelace(100),
                datum = OutputDatum.OutputDatum(order.toData)
              )
            )
          ),
          fee = BigInt(200000),
          signatories = List(bob), // wrong signer
          id = txId
        )
        val ctx = ScriptContext(
          txInfo = txInfo,
          redeemer = OrderRedeemer.Cancel.toData,
          scriptInfo = SpendingScript(
            txOutRef = TxOutRef(orderTxId, 0),
            datum = POption.Some(order.toData)
          )
        )
        val result = (orderContract.program $ ctx.toData).evaluateDebug
        checkResult(failure("Cancel requires owner signature"), result)
    }

    // -- Multiple pool tests (the main demonstration) --

    test("multi-pool: orders across two pools settled in one transaction") {
        // Alice sells tokens in Pool A, Bob sells tokens in Pool B
        // Both pools validate independently via separate withdraw-zero scripts
        OrderTestCase(
          orders = scala.List(
            Order(alice, poolAHash, inputTokens = 100, minOutput = 80, outputAda = 90),
            Order(bob, poolBHash, inputTokens = 200, minOutput = 150, outputAda = 180)
          ),
          pools = scala.List(
            Pool(
              poolAHash,
              poolAApplied,
              totalInput = 100,
              totalOutput = 90,
              resA = 1000,
              resB = 1000
            ),
            Pool(
              poolBHash,
              poolBApplied,
              totalInput = 200,
              totalOutput = 180,
              resA = 2000,
              resB = 2000
            )
          ),
          expectedResult = success
        ).run()
    }

    test("multi-pool: multiple orders per pool across two pools") {
        // Alice and Carol sell in Pool A; Bob sells in Pool B
        // Pool A total: 150 tokens in, 130 ADA out
        // Pool B total: 200 tokens in, 180 ADA out
        OrderTestCase(
          orders = scala.List(
            Order(alice, poolAHash, inputTokens = 100, minOutput = 80, outputAda = 87),
            Order(carol, poolAHash, inputTokens = 50, minOutput = 40, outputAda = 44),
            Order(bob, poolBHash, inputTokens = 200, minOutput = 150, outputAda = 180)
          ),
          pools = scala.List(
            Pool(
              poolAHash,
              poolAApplied,
              totalInput = 150,
              totalOutput = 130,
              resA = 1000,
              resB = 1000
            ),
            Pool(
              poolBHash,
              poolBApplied,
              totalInput = 200,
              totalOutput = 180,
              resA = 2000,
              resB = 2000
            )
          ),
          expectedResult = success
        ).run()
    }

    test("multi-pool: failure when one pool's invariant is violated") {
        // Pool A is valid, Pool B violates constant product
        OrderTestCase(
          orders = scala.List(
            Order(alice, poolAHash, inputTokens = 100, minOutput = 80, outputAda = 90),
            Order(bob, poolBHash, inputTokens = 200, minOutput = 150, outputAda = 1900)
          ),
          pools = scala.List(
            Pool(
              poolAHash,
              poolAApplied,
              totalInput = 100,
              totalOutput = 90,
              resA = 1000,
              resB = 1000
            ),
            Pool(
              poolBHash,
              poolBApplied,
              totalInput = 200,
              totalOutput = 1900,
              resA = 2000,
              resB = 2000
            ) // kAfter = 2200 * 100 = 220000 < kBefore = 4000000
          ),
          expectedResult = failure("Constant product invariant violated")
        ).run()
    }

    test("budget: multi-pool batch shows each pool validates once") {
        val testCase = OrderTestCase(
          orders = scala.List(
            Order(alice, poolAHash, inputTokens = 100, minOutput = 80, outputAda = 90),
            Order(bob, poolBHash, inputTokens = 200, minOutput = 150, outputAda = 180),
            Order(carol, poolAHash, inputTokens = 50, minOutput = 40, outputAda = 43)
          ),
          pools = scala.List(
            Pool(
              poolAHash,
              poolAApplied,
              totalInput = 150,
              totalOutput = 133,
              resA = 1000,
              resB = 1000
            ),
            Pool(
              poolBHash,
              poolBApplied,
              totalInput = 200,
              totalOutput = 180,
              resA = 2000,
              resB = 2000
            )
          ),
          expectedResult = success
        )

        val (poolABudget, poolBBudget, orderBudget) = testCase.runWithBudget()

        println(s"\n=== Multi-Pool DEX Budget (3 orders, 2 pools) ===")
        println(
          s"Pool A reward (runs once):  mem=${poolABudget.memory}, cpu=${poolABudget.steps}"
        )
        println(
          s"Pool B reward (runs once):  mem=${poolBBudget.memory}, cpu=${poolBBudget.steps}"
        )
        println(
          s"Order spend (per order):    mem=${orderBudget.memory}, cpu=${orderBudget.steps}"
        )
        val totalMem = poolABudget.memory + poolBBudget.memory + 3 * orderBudget.memory
        val totalCpu = poolABudget.steps + poolBBudget.steps + 3 * orderBudget.steps
        println(s"Total (2 pools + 3 orders): mem=$totalMem, cpu=$totalCpu")
        println(
          s"Each pool validates once regardless of order count. Adding more orders only adds spend cost."
        )
        println()
    }

    // -- Test helpers --

    case class Order(
        owner: PubKeyHash,
        poolHash: ValidatorHash,
        inputTokens: Long,
        minOutput: Long,
        outputAda: Long
    )

    case class Pool(
        hash: ValidatorHash,
        applied: scalus.uplc.Program,
        totalInput: Long,
        totalOutput: Long,
        resA: Long,
        resB: Long
    )

    case class OrderTestCase(
        orders: scala.List[Order],
        pools: scala.List[Pool],
        expectedResult: (String | Unit, POption[ExUnits])
    ) {
        private def buildTxInfo(): TxInfo = {
            // Build order inputs
            val orderInputs = orders.zipWithIndex.map { case (order, idx) =>
                val datum = OrderDatum(
                  owner = order.owner,
                  poolScriptHash = order.poolHash,
                  minOutputLovelace = BigInt(order.minOutput),
                  inputTokenAmount = BigInt(order.inputTokens)
                )
                TxInInfo(
                  outRef = TxOutRef(orderTxId, idx),
                  resolved = TxOut(
                    address = Address(ScriptCredential(orderScriptHash), POption.None),
                    value = Value.lovelace(BigInt(order.inputTokens)),
                    datum = OutputDatum.OutputDatum(datum.toData)
                  )
                )
            }

            // Build outputs to order owners
            val txOutputs = orders.map { order =>
                TxOut(
                  address = Address(PubKeyCredential(order.owner), POption.None),
                  value = Value.lovelace(BigInt(order.outputAda))
                )
            }

            // Build withdrawals - one per pool (withdraw-zero trick)
            val withdrawalEntries = pools.map { pool =>
                (Credential.ScriptCredential(pool.hash), BigInt(0))
            }

            // Build redeemers - spending redeemers for orders + rewarding redeemers for pools
            val spendingRedeemers = orders.zipWithIndex.map { case (_, idx) =>
                val outRef = TxOutRef(orderTxId, idx)
                (ScriptPurpose.Spending(outRef), OrderRedeemer.Execute.toData)
            }

            val rewardingRedeemers = pools.map { pool =>
                val swap = SwapRedeemer(
                  totalInputAmount = BigInt(pool.totalInput),
                  totalOutputAmount = BigInt(pool.totalOutput),
                  reserveABefore = BigInt(pool.resA),
                  reserveBBefore = BigInt(pool.resB)
                )
                (ScriptPurpose.Rewarding(Credential.ScriptCredential(pool.hash)), swap.toData)
            }

            TxInfo(
              inputs = List.from(orderInputs),
              outputs = List.from(txOutputs),
              fee = BigInt(200000),
              withdrawals = SortedMap.fromList(List.from(withdrawalEntries)),
              redeemers = SortedMap.fromList(List.from(spendingRedeemers ++ rewardingRedeemers)),
              id = txId
            )
        }

        def run(): Unit = {
            val txInfo = buildTxInfo()

            // Test each pool's reward endpoint
            val poolFailure = pools.iterator
                .map { pool =>
                    val swap = SwapRedeemer(
                      totalInputAmount = BigInt(pool.totalInput),
                      totalOutputAmount = BigInt(pool.totalOutput),
                      reserveABefore = BigInt(pool.resA),
                      reserveBBefore = BigInt(pool.resB)
                    )
                    evalPool(pool.applied, pool.hash, swap, txInfo)
                }
                .find { poolResult =>
                    expectedResult._1 match
                        case msg: String =>
                            poolResult.isFailure && poolResult.logs.exists(_.contains(msg))
                        case () =>
                            checkResult(success, poolResult)
                            false
                }

            poolFailure match
                case Some(result) =>
                    // A pool failed as expected
                    checkResult(expectedResult, result)
                case None =>
                    // All pools passed; test each order's spend endpoint
                    for (order, idx) <- orders.zipWithIndex do {
                        val datum = OrderDatum(
                          owner = order.owner,
                          poolScriptHash = order.poolHash,
                          minOutputLovelace = BigInt(order.minOutput),
                          inputTokenAmount = BigInt(order.inputTokens)
                        )
                        val ctx = ScriptContext(
                          txInfo = txInfo,
                          redeemer = OrderRedeemer.Execute.toData,
                          scriptInfo = SpendingScript(
                            txOutRef = TxOutRef(orderTxId, idx),
                            datum = POption.Some(datum.toData)
                          )
                        )
                        val result = (orderContract.program $ ctx.toData).evaluateDebug
                        checkResult(expectedResult, result)
                    }
        }

        def runWithBudget(): (ExUnits, ExUnits, ExUnits) = {
            val txInfo = buildTxInfo()

            // Pool A budget
            val poolA = pools.head
            val swapA = SwapRedeemer(
              totalInputAmount = BigInt(poolA.totalInput),
              totalOutputAmount = BigInt(poolA.totalOutput),
              reserveABefore = BigInt(poolA.resA),
              reserveBBefore = BigInt(poolA.resB)
            )
            val poolABudget = evalPool(poolA.applied, poolA.hash, swapA, txInfo).budget

            // Pool B budget
            val poolB = pools(1)
            val swapB = SwapRedeemer(
              totalInputAmount = BigInt(poolB.totalInput),
              totalOutputAmount = BigInt(poolB.totalOutput),
              reserveABefore = BigInt(poolB.resA),
              reserveBBefore = BigInt(poolB.resB)
            )
            val poolBBudget = evalPool(poolB.applied, poolB.hash, swapB, txInfo).budget

            // Order spend budget (first order)
            val firstOrder = orders.head
            val datum = OrderDatum(
              owner = firstOrder.owner,
              poolScriptHash = firstOrder.poolHash,
              minOutputLovelace = BigInt(firstOrder.minOutput),
              inputTokenAmount = BigInt(firstOrder.inputTokens)
            )
            val ctx = ScriptContext(
              txInfo = txInfo,
              redeemer = OrderRedeemer.Execute.toData,
              scriptInfo = SpendingScript(
                txOutRef = TxOutRef(orderTxId, 0),
                datum = POption.Some(datum.toData)
              )
            )
            val orderBudget = (orderContract.program $ ctx.toData).evaluateDebug.budget

            (poolABudget, poolBBudget, orderBudget)
        }
    }

    private val emptyTxInfo: TxInfo = TxInfo(inputs = List.Nil, id = txId)

    private def evalPool(
        applied: scalus.uplc.Program,
        poolHash: ValidatorHash,
        swap: SwapRedeemer,
        txInfo: TxInfo = emptyTxInfo
    ): scalus.uplc.eval.Result = {
        val stakingCred = Credential.ScriptCredential(poolHash)
        val ctx = ScriptContext(
          txInfo = txInfo,
          redeemer = swap.toData,
          scriptInfo = RewardingScript(stakingCred)
        )
        (applied $ ctx.toData).evaluateDebug
    }
}
