package scalus.examples.betting

import org.scalatest.funsuite.AnyFunSuite
import scalus.*
import scalus.builtin.ByteString
import scalus.builtin.ByteString.*
import scalus.builtin.Data.toData
import scalus.ledger.api.v1.Address
import scalus.ledger.api.v1.PubKeyHash
import scalus.ledger.api.v1.PubKeyHash.*
import scalus.ledger.api.v2.OutputDatum
import scalus.ledger.api.v3.*
import scalus.prelude.*
import scalus.prelude.Option.*
import scalus.testing.kit.{Mock, ScalusTest}

import scala.language.implicitConversions

class BettingTest extends AnyFunSuite, ScalusTest:
    private lazy val program = BettingContract.debugCompiledContract.program

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

    // TODO: test wrong tx fails

    test("Verify that a bet can be properly initialized"):
        val player1 = Mock.mockPubKeyHash(1)
        // Create test datum for a new bet
        val initialBetDatum = BetDatum(
          player1,
          // No second player yet
          player2 = pkh"",
          oracle = Mock.mockPubKeyHash(3),
          // 31th of July 2025
          expiration = 1753939940,
        )
        val policyId = Mock.mockScriptHash(1)
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
              datum = OutputDatum.OutputDatum(initialBetDatum.toData)
            )
          ),
          signatories = List(player1),
          // 20th of July 2025 - for 5 minutes
          validRange = Interval.between(1752989540, 1752990020)
        )
        val result = program.runWithDebug(
          ScriptContext(
            txInfo = testTransaction,
            scriptInfo = ScriptInfo.MintingScript(policyId = policyId)
          )
        )
        if result.isFailure then
            result.logs.foreach(println)
            println(result)
        assert(result.isSuccess, "Script execution should succeed for initial minting")

    test("Verify that player2 can join an existing bet"):
        val player1 = Mock.mockPubKeyHash(1)
        val player2 = Mock.mockPubKeyHash(2)
        val oracle = Mock.mockPubKeyHash(3)
        // Initial state: bet created by player1
        val initialBetDatum = BetDatum(
          player1,
          // No second player yet
          player2 = PubKeyHash(ByteString.empty),
          oracle = oracle,
          // 31th of July 2025
          expiration = 1753939940,
        )
        // Updated state: player2 has joined
        val updatedBetDatum = BetDatum(
          player1,
          player2,
          oracle,
          // 31th of July 2025
          expiration = 1753939940,
        )
        val policyId = Mock.mockScriptHash(1)
        val tx = Mock.mockTxOutRef(1, 0)
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
                datum = OutputDatum.OutputDatum(initialBetDatum.toData)
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
              datum = OutputDatum.OutputDatum(updatedBetDatum.toData)
            )
          ),
          signatories = List(player2),
          // 22th of July 2025 - for 5 minutes
          validRange = Interval.between(1753162820, 1753163120)
        )
        val joinAction: Action = Action.Join
        val result = program.runWithDebug(
          ScriptContext(
            txInfo = testTransaction,
            redeemer = joinAction.toData,
            scriptInfo = ScriptInfo.SpendingScript(
              txOutRef = tx,
              datum = Some(updatedBetDatum.toData)
            )
          )
        )
        if result.isFailure then
            result.logs.foreach(println)
            println(result)
        assert(result.isSuccess, "Script execution should succeed for player2 joining spending")

    test("Verify that the oracle can announce winner and trigger payout"):
        val player1 = Mock.mockPubKeyHash(1)
        val player2 = Mock.mockPubKeyHash(2)
        val oracle = Mock.mockPubKeyHash(3)
        // Final bet state with both players
        val finalBetDatum = BetDatum(
          player1,
          player2,
          oracle,
          // 31th of July 2025
          expiration = 1753939940,
        )
        val policyId = Mock.mockScriptHash(1)
        val tx = Mock.mockTxOutRef(1, 0)
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
                datum = OutputDatum.OutputDatum(finalBetDatum.toData)
              )
            )
          ),
          outputs = List(
            TxOut(
              // Payout goes to player2's address
              address = Address.fromPubKeyHash(player2),
              // Winner takes all
              value = Value.lovelace(6_000_000) + Value(
                cs = policyId,
                tn = utf8"lucky_number_slevin",
                v = 1
              )
            )
          ),
          // Oracle signs to announce the winner
          signatories = List(oracle),
          // 1st of August 2025 - for 5 minutes
          validRange = Interval.between(1754027120, 1754027420)
        )
        val announceWinnerAction: Action = Action.AnnounceWinner(player2)
        val result = program.runWithDebug(
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
