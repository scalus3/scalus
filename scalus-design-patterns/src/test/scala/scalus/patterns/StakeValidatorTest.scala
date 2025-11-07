package scalus.patterns

import scalus.*
import scalus.builtin.ByteString
import scalus.builtin.Data.toData
import scalus.ledger.api.v3.*
import scalus.prelude.*
import scalus.cardano.onchain.RequirementError

class StakeValidatorTest extends StdlibTestKit with scalus.ledger.api.v3.ArbitraryInstances {
    // TODO: UPLC error
    ignore("success spend") {
        assertEvalSuccess {
            val scriptHash = ByteString.empty
            val credential = Credential.ScriptCredential(scriptHash)

            val txInfo = TxInfo(
              inputs = List.empty,
              withdrawals = SortedMap.singleton(credential, 0),
              redeemers = SortedMap.singleton(ScriptPurpose.Rewarding(credential), ().toData),
              id = TxId(ByteString.empty)
            )

            StakeValidator.spend(
              withdrawalScriptHash = scriptHash,
              withdrawalRedeemerValidator = (redeemer, lovelace) => true,
              txInfo = txInfo
            )
        }
    }

    // TODO: UPLC error
    ignore("failed spend with missing redeemer") {
        assertEvalFailsWithMessage[NoSuchElementException](StakeValidator.MissingRedeemer) {
            val scriptHash = ByteString.empty
            val credential = Credential.ScriptCredential(scriptHash)

            val txInfo = TxInfo(
              inputs = List.empty,
              withdrawals = SortedMap.singleton(credential, 0),
              redeemers = SortedMap.empty,
              id = TxId(ByteString.empty)
            )

            StakeValidator.spend(
              withdrawalScriptHash = scriptHash,
              withdrawalRedeemerValidator = (redeemer, lovelace) => true,
              txInfo = txInfo
            )
        }
    }

    // TODO: UPLC error
    ignore("failed spend with missing withdrawal") {
        assertEvalFailsWithMessage[NoSuchElementException](StakeValidator.MissingWithdrawal) {
            val scriptHash = ByteString.empty
            val credential = Credential.ScriptCredential(scriptHash)

            val txInfo = TxInfo(
              inputs = List.empty,
              withdrawals = SortedMap.empty,
              redeemers = SortedMap.singleton(ScriptPurpose.Rewarding(credential), ().toData),
              id = TxId(ByteString.empty)
            )

            StakeValidator.spend(
              withdrawalScriptHash = scriptHash,
              withdrawalRedeemerValidator = (redeemer, lovelace) => true,
              txInfo = txInfo
            )
        }
    }

    // TODO: UPLC error
    ignore("failed spend with withdrawal redeemer validator failed") {
        assertEvalFailsWithMessage[RequirementError](
          StakeValidator.WithdrawalRedeemerValidatorFailed
        ) {
            val scriptHash = ByteString.empty
            val credential = Credential.ScriptCredential(scriptHash)

            val txInfo = TxInfo(
              inputs = List.empty,
              withdrawals = SortedMap.singleton(credential, 0),
              redeemers = SortedMap.singleton(ScriptPurpose.Rewarding(credential), ().toData),
              id = TxId(ByteString.empty)
            )

            StakeValidator.spend(
              withdrawalScriptHash = scriptHash,
              withdrawalRedeemerValidator = (redeemer, lovelace) => false,
              txInfo = txInfo
            )
        }
    }

    test("success spendMinimal") {
        assertEvalSuccess {
            val scriptHash = ByteString.empty
            val credential = Credential.ScriptCredential(scriptHash)

            val txInfo = TxInfo(
              inputs = List.empty,
              withdrawals = SortedMap.singleton(credential, 0),
              id = TxId(ByteString.empty)
            )

            StakeValidator.spendMinimal(
              withdrawalScriptHash = scriptHash,
              txInfo = txInfo
            )
        }
    }

    test("failed spendMinimal with missing withdrawal") {
        assertEvalFailsWithMessage[NoSuchElementException](StakeValidator.MissingWithdrawal) {
            val scriptHash = ByteString.empty

            val txInfo = TxInfo(
              inputs = List.empty,
              withdrawals = SortedMap.empty,
              id = TxId(ByteString.empty)
            )

            StakeValidator.spendMinimal(
              withdrawalScriptHash = scriptHash,
              txInfo = txInfo
            )
        }
    }

    // TODO: UPLC error
    ignore("success withdraw") {
        assertEvalSuccess {
            val scriptHash = ByteString.empty
            val credential = Credential.ScriptCredential(scriptHash)

            val txInfo = TxInfo(
              inputs = List.empty,
              id = TxId(ByteString.empty)
            )

            StakeValidator.withdraw(
              withdrawalValidator = (redeemer, validatorHash, tx) => true,
              redeemer = ().toData,
              credential = credential,
              txInfo = txInfo
            )
        }
    }

    // TODO: UPLC error
//    test("failed withdraw with pub key credential not supported") {
//        assertEvalFailsWithMessage[OnchainError](StakeValidator.PubKeyCredentialNotSupported) {
//            val scriptHash = ByteString.empty
//            val credential = Credential.PubKeyCredential(PubKeyHash(scriptHash))
//
//            val txInfo = TxInfo(
//              inputs = List.empty,
//              id = TxId(ByteString.empty)
//            )
//
//            StakeValidator.withdraw(
//              withdrawalValidator = (redeemer, validatorHash, tx) => true,
//              redeemer = ().toData,
//              credential = credential,
//              txInfo = txInfo
//            )
//        }
//    }

    // TODO: UPLC error
//    test("failed withdraw with withdrawal validator failed") {
//        assertEvalFailsWithMessage[RequirementError](StakeValidator.WithdrawalValidatorFailed) {
//            val scriptHash = ByteString.empty
//            val credential = Credential.ScriptCredential(scriptHash)
//
//            val txInfo = TxInfo(
//              inputs = List.empty,
//              id = TxId(ByteString.empty)
//            )
//
//            StakeValidator.withdraw(
//              withdrawalValidator = (redeemer, validatorHash, tx) => false,
//              redeemer = ().toData,
//              credential = credential,
//              txInfo = txInfo
//            )
//        }
//    }
}
