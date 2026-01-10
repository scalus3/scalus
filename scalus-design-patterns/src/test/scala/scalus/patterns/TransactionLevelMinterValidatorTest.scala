package scalus.patterns

import org.scalatest.funsuite.AnyFunSuite
import scalus.*
import scalus.builtin.ByteString
import scalus.builtin.Data.toData
import scalus.cardano.onchain.RequirementError
import scalus.ledger.api.v3.*
import scalus.prelude.*
import scalus.testing.kit.EvalTestKit

class TransactionLevelMinterValidatorTest
    extends AnyFunSuite
    with EvalTestKit
    with scalus.ledger.api.v3.ArbitraryInstances {

    // TODO: UPLC error
    ignore("success spend") {
        assertEvalSuccess {
            val minterScriptHash = ByteString.empty
            val minterRedeemerValidator = (redeemer: Redeemer) => true
            val minterTokensValidator = (tokens: SortedMap[TokenName, BigInt]) => true

            val txInfo = TxInfo(
              inputs = List.empty,
              mint = Value.zero,
              redeemers = SortedMap.singleton(ScriptPurpose.Minting(minterScriptHash), ().toData),
              id = TxId(ByteString.empty)
            )

            TransactionLevelMinterValidator.spend(
              minterScriptHash = minterScriptHash,
              minterRedeemerValidator = minterRedeemerValidator,
              minterTokensValidator = minterTokensValidator,
              txInfo = txInfo
            )
        }
    }

    // TODO: UPLC error
    ignore("failed spend with missing redeemer") {
        assertEvalFailsWithMessage[NoSuchElementException](
          TransactionLevelMinterValidator.MissingRedeemer
        ) {
            val minterScriptHash = ByteString.empty
            val minterRedeemerValidator = (redeemer: Redeemer) => true
            val minterTokensValidator = (tokens: SortedMap[TokenName, BigInt]) => true

            val txInfo = TxInfo(
              inputs = List.empty,
              mint = Value.zero,
              redeemers = SortedMap.empty,
              id = TxId(ByteString.empty)
            )

            TransactionLevelMinterValidator.spend(
              minterScriptHash = minterScriptHash,
              minterRedeemerValidator = minterRedeemerValidator,
              minterTokensValidator = minterTokensValidator,
              txInfo = txInfo
            )
        }
    }

    // TODO: UPLC error
    ignore("failed spend with minter redeemer validator failed") {
        assertEvalFailsWithMessage[RequirementError](
          TransactionLevelMinterValidator.MinterRedeemerValidatorFailed
        ) {
            val minterScriptHash = ByteString.empty
            val minterRedeemerValidator = (redeemer: Redeemer) => false
            val minterTokensValidator = (tokens: SortedMap[TokenName, BigInt]) => true

            val txInfo = TxInfo(
              inputs = List.empty,
              mint = Value.zero,
              redeemers = SortedMap.singleton(ScriptPurpose.Minting(minterScriptHash), ().toData),
              id = TxId(ByteString.empty)
            )

            TransactionLevelMinterValidator.spend(
              minterScriptHash = minterScriptHash,
              minterRedeemerValidator = minterRedeemerValidator,
              minterTokensValidator = minterTokensValidator,
              txInfo = txInfo
            )
        }
    }

    // TODO: UPLC error
    ignore("failed spend with minter tokens validator failed") {
        assertEvalFailsWithMessage[RequirementError](
          TransactionLevelMinterValidator.MinterTokensValidatorFailed
        ) {
            val minterScriptHash = ByteString.empty
            val minterRedeemerValidator = (redeemer: Redeemer) => true
            val minterTokensValidator = (tokens: SortedMap[TokenName, BigInt]) => false

            val txInfo = TxInfo(
              inputs = List.empty,
              mint = Value.zero,
              redeemers = SortedMap.singleton(ScriptPurpose.Minting(minterScriptHash), ().toData),
              id = TxId(ByteString.empty)
            )

            TransactionLevelMinterValidator.spend(
              minterScriptHash = minterScriptHash,
              minterRedeemerValidator = minterRedeemerValidator,
              minterTokensValidator = minterTokensValidator,
              txInfo = txInfo
            )
        }
    }

    test("success spendMinimal") {
        assertEvalSuccess {
            val minterScriptHash = ByteString.empty

            val txInfo = TxInfo(
              inputs = List.empty,
              mint = Value(minterScriptHash, ByteString.empty, BigInt(1)),
              redeemers = SortedMap.empty,
              id = TxId(ByteString.empty)
            )

            TransactionLevelMinterValidator.spendMinimal(
              minterScriptHash = minterScriptHash,
              txInfo = txInfo
            )
        }
    }

    test("failed spendMinimal with missing mint") {
        assertEvalFailsWithMessage[NoSuchElementException](
          TransactionLevelMinterValidator.MissingMint
        ) {
            val minterScriptHash = ByteString.empty

            val txInfo = TxInfo(
              inputs = List.empty,
              mint = Value.zero,
              redeemers = SortedMap.empty,
              id = TxId(ByteString.empty)
            )

            TransactionLevelMinterValidator.spendMinimal(
              minterScriptHash = minterScriptHash,
              txInfo = txInfo
            )
        }
    }
}
