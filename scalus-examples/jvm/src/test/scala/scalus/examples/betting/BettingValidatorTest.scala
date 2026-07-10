package scalus.examples.betting

import org.scalatest.funsuite.AnyFunSuite
import scalus.*
import scalus.uplc.builtin.ByteString
import scalus.uplc.builtin.ByteString.*
import scalus.uplc.builtin.Data.toData
import scalus.cardano.onchain.plutus.v1.Address
import scalus.cardano.onchain.plutus.v1.PubKeyHash
import scalus.cardano.onchain.plutus.v1.PubKeyHash.*
import scalus.cardano.onchain.plutus.v2.OutputDatum
import scalus.cardano.onchain.plutus.v3.*
import scalus.cardano.onchain.plutus.prelude.*
import scalus.cardano.onchain.plutus.prelude.Option.*
import scalus.cardano.ledger.ExUnits
import scalus.testing.kit.{ScalusTest, TestUtil}

import scala.language.implicitConversions

class BettingValidatorTest extends AnyFunSuite, ScalusTest:
    private val contract = BettingContract.compiled.withErrorTraces

    /*
    case class TestCase(
        expiration: PosixTime
    ):

        def check: Result =
            val testTransaction = TxInfo.placeholder
            val action = Nothing
            val result = BettingContract.compiled.runScript(
              ScriptContext(
                txInfo = testTransaction,
                redeemer = action.toData,
                scriptInfo = ScriptInfo.SpendingScript(
                  txOutRef = tx
                )
              )
            )
            println(result.logs)
            result
     */

    test("Verify that a bet can be properly initialized"):
        val player1 = TestUtil.mockPubKeyHash(1)
        // Create test betting config for a new bet
        val initialBettingConfig = Config(
          player1,
          // No second player yet
          player2 = pkh"",
          oracle = TestUtil.mockPubKeyHash(3),
          // 31th of July 2025
          expiration = 1753939940,
        )
        val policyId = TestUtil.mockScriptHash(1)
        // Create test transaction that mints a bet token
        val testTransaction = TxInfo.placeholder.copy(
          outputs = List(
            TxOut(
              address = Address.fromScriptHash(policyId),
              // 3 ADA initial bet
              value = Value.lovelace(3_000_000) + Value(
                cs = policyId,
                tn = utf8"lucky_number_slevin",
                v = 1
              ),
              datum = OutputDatum.OutputDatum(initialBettingConfig.toData)
            )
          ),
          // Include the minted token in tx.mint
          mint = Value(policyId, utf8"lucky_number_slevin", 1),
          signatories = List(player1),
          // 20th of July 2025 - for 5 minutes
          validRange = Interval.between(1752989540, 1752990020)
        )
        val result = contract.program.runWithDebug(
          ScriptContext(
            txInfo = testTransaction,
            scriptInfo = ScriptInfo.MintingScript(policyId = policyId)
          )
        )
        if result.isFailure then
            result.logs.foreach(println)
            println(result)
        assert(result.isSuccess, "Script execution should succeed for initial minting")
        assert(
          result.budget == (ExUnits(memory = 103532L, steps = 34535888L))
        )

    test("Verify that player2 can join an existing bet"):
        val player1 = TestUtil.mockPubKeyHash(1)
        val player2 = TestUtil.mockPubKeyHash(2)
        val oracle = TestUtil.mockPubKeyHash(3)
        // Initial state: bet created by player1
        val initialBettingConfig = Config(
          player1,
          // No second player yet
          player2 = PubKeyHash(ByteString.empty),
          oracle = oracle,
          // 31th of July 2025
          expiration = 1753939940,
        )
        // Updated state: player2 has joined
        val updatedBettingConfig = Config(
          player1,
          player2,
          oracle,
          // 31th of July 2025
          expiration = 1753939940,
        )
        val policyId = TestUtil.mockScriptHash(1)
        val tx = TestUtil.mockTxOutRef(1, 0)
        // Create test transaction where player2 joins
        val testTransaction = TxInfo.placeholder.copy(
          inputs = List(
            TxInInfo(
              outRef = tx,
              resolved = TxOut(
                address = Address.fromScriptHash(policyId),
                // Original 3 ADA bet
                value = Value.lovelace(3_000_000) + Value(
                  cs = policyId,
                  tn = utf8"lucky_number_slevin",
                  v = 1
                ),
                datum = OutputDatum.OutputDatum(initialBettingConfig.toData)
              )
            )
          ),
          outputs = List(
            TxOut(
              address = Address.fromScriptHash(policyId),
              // Doubled to 6 ADA
              value = Value.lovelace(6_000_000) + Value(
                cs = policyId,
                tn = utf8"lucky_number_slevin",
                v = 1
              ),
              datum = OutputDatum.OutputDatum(updatedBettingConfig.toData)
            )
          ),
          signatories = List(player2),
          // 22th of July 2025 - for 5 minutes
          validRange = Interval.between(1753162820, 1753163120)
        )
        val joinAction: Action = Action.Join
        val result = contract.program.runWithDebug(
          ScriptContext(
            txInfo = testTransaction,
            redeemer = joinAction.toData,
            scriptInfo = ScriptInfo.SpendingScript(
              txOutRef = tx,
              datum = Some(updatedBettingConfig.toData)
            )
          )
        )
        if result.isFailure then
            result.logs.foreach(println)
            println(result)
        assert(result.isSuccess, "Script execution should succeed for player2 joining spending")
        assert(
          result.budget == (ExUnits(memory = 261455L, steps = 89940840L))
        )

    test("Verify that the oracle can announce winner and trigger payout"):
        val player1 = TestUtil.mockPubKeyHash(1)
        val player2 = TestUtil.mockPubKeyHash(2)
        val oracle = TestUtil.mockPubKeyHash(3)
        // Final bet state with both players
        val finalBettingConfig = Config(
          player1,
          player2,
          oracle,
          // 31th of July 2025
          expiration = 1753939940,
        )
        val policyId = TestUtil.mockScriptHash(1)
        val tx = TestUtil.mockTxOutRef(1, 0)
        // Create test transaction where oracle announces player2 as winner
        val testTransaction = TxInfo.placeholder.copy(
          inputs = List(
            TxInInfo(
              outRef = tx,
              resolved = TxOut(
                address = Address.fromScriptHash(policyId),
                // Total pot: 6 ADA
                value = Value.lovelace(6_000_000) + Value(
                  cs = policyId,
                  tn = utf8"lucky_number_slevin",
                  v = 1
                ),
                datum = OutputDatum.OutputDatum(finalBettingConfig.toData)
              )
            )
          ),
          outputs = List(
            TxOut(
              // Payout goes to player2's address — the pot lovelace; the NFT is burned, not paid out.
              address = Address.fromPubKeyHash(player2),
              value = Value.lovelace(6_000_000)
            )
          ),
          // The bet NFT is burned on payout (one-shot).
          mint = Value(policyId, utf8"lucky_number_slevin", -1),
          // Oracle signs to announce the winner
          signatories = List(oracle),
          // 1st of August 2025 - for 5 minutes
          validRange = Interval.between(1754027120, 1754027420)
        )
        val announceWinnerAction: Action = Action.AnnounceWinner(player2, BigInt(0))
        val result = contract.program.runWithDebug(
          ScriptContext(
            txInfo = testTransaction,
            redeemer = announceWinnerAction.toData,
            scriptInfo = ScriptInfo.SpendingScript(
              txOutRef = tx
            )
          )
        )
        if result.isFailure then
            result.logs.foreach(println)
            println(result)
        assert(result.isSuccess, "Script execution should succeed for announce winner spending")
        assert(
          result.budget == (ExUnits(memory = 236568L, steps = 75355846L))
        )

    test("Verify that announcing the winner fails if the bet token is not burned"):
        val player1 = TestUtil.mockPubKeyHash(1)
        val player2 = TestUtil.mockPubKeyHash(2)
        val oracle = TestUtil.mockPubKeyHash(3)
        val config = Config(player1, player2, oracle, expiration = 1753939940)
        val policyId = TestUtil.mockScriptHash(1)
        val tx = TestUtil.mockTxOutRef(1, 0)
        // Same as the success case but the NFT is NOT burned (it rides along to the winner) — the
        // bet must be one-shot, so this is rejected.
        val testTransaction = TxInfo.placeholder.copy(
          inputs = List(
            TxInInfo(
              outRef = tx,
              resolved = TxOut(
                address = Address.fromScriptHash(policyId),
                value = Value.lovelace(6_000_000) + Value(policyId, utf8"lucky_number_slevin", 1),
                datum = OutputDatum.OutputDatum(config.toData)
              )
            )
          ),
          outputs = List(
            TxOut(
              address = Address.fromPubKeyHash(player2),
              value = Value.lovelace(6_000_000) + Value(policyId, utf8"lucky_number_slevin", 1)
            )
          ),
          // No mint: the token is not burned.
          signatories = List(oracle),
          validRange = Interval.between(1754027120, 1754027420)
        )
        val result = contract.program.runWithDebug(
          ScriptContext(
            txInfo = testTransaction,
            redeemer = (Action.AnnounceWinner(player2, BigInt(0)): Action).toData,
            scriptInfo = ScriptInfo.SpendingScript(txOutRef = tx)
          )
        )
        assert(result.isFailure, "Announcing without burning the bet token must fail")
        assert(
          result.logs.exists(_.contains("must be burned")),
          s"Expected burn error, got: ${result.logs.mkString(", ")}"
        )

    /** Build a joined-bet timeout transaction with the given outputs and validity range. */
    private def timeoutTx(
        policyId: PolicyId,
        tx: TxOutRef,
        config: Config,
        outputs: List[TxOut],
        signatory: PubKeyHash,
        validRange: Interval
    ): TxInfo =
        TxInfo.placeholder.copy(
          inputs = List(
            TxInInfo(
              outRef = tx,
              resolved = TxOut(
                address = Address.fromScriptHash(policyId),
                value = Value.lovelace(6_000_000) + Value(policyId, utf8"lucky_number_slevin", 1),
                datum = OutputDatum.OutputDatum(config.toData)
              )
            )
          ),
          outputs = outputs,
          // The bet NFT is burned on timeout (one-shot).
          mint = Value(policyId, utf8"lucky_number_slevin", -1),
          signatories = List(signatory),
          validRange = validRange
        )

    test("Verify that both players can reclaim the pot after a timeout"):
        val player1 = TestUtil.mockPubKeyHash(1)
        val player2 = TestUtil.mockPubKeyHash(2)
        val oracle = TestUtil.mockPubKeyHash(3)
        val config = Config(player1, player2, oracle, expiration = 1753939940)
        val policyId = TestUtil.mockScriptHash(1)
        val tx = TestUtil.mockTxOutRef(1, 0)

        // Each player is refunded their 3 ADA stake; the NFT is burned (not handed out).
        val outputs = List(
          TxOut(
            address = Address.fromPubKeyHash(player1),
            value = Value.lovelace(3_000_000)
          ),
          TxOut(
            address = Address.fromPubKeyHash(player2),
            value = Value.lovelace(3_000_000)
          )
        )
        // 1st of August 2025 — after expiration
        val result = contract.program.runWithDebug(
          ScriptContext(
            txInfo = timeoutTx(policyId, tx, config, outputs, player1, Interval.after(1754027120)),
            redeemer = (Action.Timeout: Action).toData,
            scriptInfo = ScriptInfo.SpendingScript(txOutRef = tx)
          )
        )
        if result.isFailure then result.logs.foreach(println)
        assert(result.isSuccess, "Reclaim after expiration should succeed")
        assert(
          result.budget == (ExUnits(memory = 309061L, steps = 98674117L))
        )

    test("Verify that reclaim before expiration fails"):
        val player1 = TestUtil.mockPubKeyHash(1)
        val player2 = TestUtil.mockPubKeyHash(2)
        val oracle = TestUtil.mockPubKeyHash(3)
        val config = Config(player1, player2, oracle, expiration = 1753939940)
        val policyId = TestUtil.mockScriptHash(1)
        val tx = TestUtil.mockTxOutRef(1, 0)
        val outputs = List(
          TxOut(
            address = Address.fromPubKeyHash(player1),
            value = Value.lovelace(3_000_000) + Value(policyId, utf8"lucky_number_slevin", 1)
          ),
          TxOut(address = Address.fromPubKeyHash(player2), value = Value.lovelace(3_000_000))
        )
        // Validity entirely before expiration
        val result = contract.program.runWithDebug(
          ScriptContext(
            txInfo = timeoutTx(
              policyId,
              tx,
              config,
              outputs,
              player1,
              Interval.between(1752989540, 1752990020)
            ),
            redeemer = (Action.Timeout: Action).toData,
            scriptInfo = ScriptInfo.SpendingScript(txOutRef = tx)
          )
        )
        assert(result.isFailure, "Reclaim before expiration must fail")

    test("Verify that reclaim fails if a player is not refunded"):
        val player1 = TestUtil.mockPubKeyHash(1)
        val player2 = TestUtil.mockPubKeyHash(2)
        val oracle = TestUtil.mockPubKeyHash(3)
        val config = Config(player1, player2, oracle, expiration = 1753939940)
        val policyId = TestUtil.mockScriptHash(1)
        val tx = TestUtil.mockTxOutRef(1, 0)
        // player1 grabs the whole pot; player2 gets nothing
        val outputs = List(
          TxOut(
            address = Address.fromPubKeyHash(player1),
            value = Value.lovelace(6_000_000) + Value(policyId, utf8"lucky_number_slevin", 1)
          )
        )
        val result = contract.program.runWithDebug(
          ScriptContext(
            txInfo = timeoutTx(policyId, tx, config, outputs, player1, Interval.after(1754027120)),
            redeemer = (Action.Timeout: Action).toData,
            scriptInfo = ScriptInfo.SpendingScript(txOutRef = tx)
          )
        )
        assert(result.isFailure, "Reclaim must refund both players")
