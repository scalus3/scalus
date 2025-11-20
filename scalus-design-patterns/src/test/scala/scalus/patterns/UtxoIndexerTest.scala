package scalus.patterns

import scalus.*
import scalus.builtin.ByteString
import scalus.ledger.api.v1.{Address, Value}
import scalus.ledger.api.v2.TxOut
import scalus.ledger.api.v3.*
import scalus.prelude.*
import scalus.cardano.onchain.RequirementError

class UtxoIndexerTest extends StdlibTestKit with scalus.ledger.api.v3.ArbitraryInstances {

    test("success oneToOne") {
        assertEvalSuccess {
            val txId = TxId(ByteString.empty)
            val ownRef = TxOutRef(txId, 0)
            val address =
                Address(Credential.PubKeyCredential(PubKeyHash(ByteString.empty)), Option.None)
            val value = Value.zero

            val input = TxInInfo(ownRef, TxOut(address, value))
            val output = TxOut(address, value)

            val txInfo = TxInfo(
              inputs = List.single(input),
              outputs = List.single(output),
              id = txId
            )

            UtxoIndexer.oneToOne(
              ownRef = ownRef,
              inputIdx = 0,
              outputIdx = 0,
              tx = txInfo,
              validator = (_, _) => true
            )
        }
    }

    test("failed oneToOne with wrong input index") {
        assertEvalFailsWithMessage[RequirementError](UtxoIndexer.InputIndexMismatch) {
            val txId = TxId(ByteString.empty)
            val ownRef = TxOutRef(txId, 0)
            val address =
                Address(Credential.PubKeyCredential(PubKeyHash(ByteString.empty)), Option.None)
            val value = Value.zero

            val wrongRef = TxOutRef(txId, 1)
            val input = TxInInfo(wrongRef, TxOut(address, value))
            val output = TxOut(address, value)

            val txInfo = TxInfo(
              inputs = List.single(input),
              outputs = List.single(output),
              id = txId
            )

            UtxoIndexer.oneToOne(
              ownRef = ownRef,
              inputIdx = 0,
              outputIdx = 0,
              tx = txInfo,
              validator = (_, _) => true
            )
        }
    }

    test("failed oneToOne with validator failure") {
        assertEvalFailsWithMessage[RequirementError](UtxoIndexer.ValidatorFailed) {
            val txId = TxId(ByteString.empty)
            val ownRef = TxOutRef(txId, 0)
            val address =
                Address(Credential.PubKeyCredential(PubKeyHash(ByteString.empty)), Option.None)
            val value = Value.zero

            val input = TxInInfo(ownRef, TxOut(address, value))
            val output = TxOut(address, value)

            val txInfo = TxInfo(
              inputs = List.single(input),
              outputs = List.single(output),
              id = txId
            )

            UtxoIndexer.oneToOne(
              ownRef = ownRef,
              inputIdx = 0,
              outputIdx = 0,
              tx = txInfo,
              validator = (_, _) => false
            )
        }
    }

    test("success oneToOne with multiple inputs and outputs") {
        assertEvalSuccess {
            val txId = TxId(ByteString.empty)
            val ownRef = TxOutRef(txId, 1)
            val address =
                Address(Credential.PubKeyCredential(PubKeyHash(ByteString.empty)), Option.None)
            val value = Value.zero

            val input0 = TxInInfo(TxOutRef(txId, 0), TxOut(address, value))
            val input1 = TxInInfo(ownRef, TxOut(address, value))
            val input2 = TxInInfo(TxOutRef(txId, 2), TxOut(address, value))

            val output0 = TxOut(address, value)
            val output1 = TxOut(address, value)
            val output2 = TxOut(address, value)

            val txInfo = TxInfo(
              inputs = List(input0, input1, input2),
              outputs = List(output0, output1, output2),
              id = txId
            )

            UtxoIndexer.oneToOne(
              ownRef = ownRef,
              inputIdx = 1,
              outputIdx = 2,
              tx = txInfo,
              validator = (_, _) => true
            )
        }
    }
}
