package scalus.patterns

import org.scalatest.funsuite.AnyFunSuite
import scalus.cardano.onchain.plutus.prelude.fail as onchainFail
import scalus.cardano.onchain.OnchainError
import scalus.*
import scalus.uplc.builtin.ByteString
import scalus.uplc.builtin.Data.toData
import scalus.cardano.onchain.RequirementError
import scalus.cardano.onchain.plutus.v3.*
import scalus.cardano.onchain.plutus.prelude.*
import scalus.testing.kit.EvalTestKit

class TransactionLevelMinterValidatorTest
    extends AnyFunSuite
    with EvalTestKit
    with scalus.cardano.onchain.plutus.v3.ArbitraryInstances {

    // TODO: UPLC error
    ignore("success spend") {
        assertEvalSuccess {
            val minterScriptHash = ByteString.empty
            val minterRedeemerValidator = (redeemer: Redeemer) => ()
            val minterTokensValidator = (tokens: SortedMap[TokenName, BigInt]) => ()

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
            val minterRedeemerValidator = (redeemer: Redeemer) => ()
            val minterTokensValidator = (tokens: SortedMap[TokenName, BigInt]) => ()

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
        assertEvalFailsWithMessage[OnchainError]("minter redeemer validator failed") {
            val minterScriptHash = ByteString.empty
            val minterRedeemerValidator =
                (redeemer: Redeemer) => onchainFail("minter redeemer validator failed")
            val minterTokensValidator = (tokens: SortedMap[TokenName, BigInt]) => ()

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
        assertEvalFailsWithMessage[OnchainError]("minter tokens validator failed") {
            val minterScriptHash = ByteString.empty
            val minterRedeemerValidator = (redeemer: Redeemer) => ()
            val minterTokensValidator = (tokens: SortedMap[TokenName, BigInt]) =>
                onchainFail("minter tokens validator failed")

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
