package scalus.cardano.ledger

import org.scalatest.funsuite.AnyFunSuite
import scalus.cardano.address.*
import scalus.uplc.builtin.ByteString

/** The PlutusV3 TxInfo of a tx whose inputs and reference inputs overlap, spec [TX-1].
  *
  * From protocol version 11 the ledger accepts the overlap in the UTXO rule, but the PlutusV3
  * context fails with `ReferenceInputsNotDisjointFromInputs` (C/TxInfo.hs:488-493). PlutusV2 has no
  * such check.
  */
class ReferenceInputsDisjointTxInfoTest extends AnyFunSuite {

    private def input(n: Int): TransactionInput =
        TransactionInput(TransactionHash.fromByteString(ByteString.fromHex(f"$n%064x")), 0)

    private val address = ShelleyAddress(
      network = Network.Mainnet,
      payment = ShelleyPaymentPart.Key(AddrKeyHash.fromHex("11" * 28)),
      delegation = ShelleyDelegationPart.Null
    )

    private val (a, b, c, d) = (input(1), input(2), input(3), input(4))

    private val utxos: Map[TransactionInput, TransactionOutput] =
        Seq(a, b, c, d).map(_ -> TransactionOutput(address, Value.ada(10))).toMap

    private def tx(inputs: Set[TransactionInput], referenceInputs: Set[TransactionInput]) =
        Transaction(
          TransactionBody(
            inputs = TaggedSortedSet.from(inputs),
            outputs = IndexedSeq.empty,
            fee = Coin.zero,
            referenceInputs = TaggedSortedSet.from(referenceInputs)
          )
        )

    private def txInfoV3(t: Transaction, pv: MajorProtocolVersion) =
        LedgerToPlutusTranslation.getTxInfoV3(t, utxos, SlotConfig.mainnet, pv)

    test("PlutusV3 TxInfo fails at PV 11 and names every common input, spec [TX-1], [TX-1a]") {
        val overlapping = tx(Set(a, b, c), Set(b, c, d))
        val error = intercept[IllegalArgumentException] {
            txInfoV3(overlapping, MajorProtocolVersion.vanRossemPV)
        }
        assert(error.getMessage.contains("ReferenceInputsNotDisjointFromInputs"), error.getMessage)
        assert(
          error.getMessage.contains(s"$b") && error.getMessage.contains(s"$c"),
          error.getMessage
        )
        assert(
          !error.getMessage.contains(s"$a") && !error.getMessage.contains(s"$d"),
          error.getMessage
        )
    }

    test("PlutusV3 TxInfo fails at PV 12 too, spec [TX-1]") {
        intercept[IllegalArgumentException] {
            txInfoV3(tx(Set(a), Set(a)), MajorProtocolVersion.dijkstraPV)
        }
    }

    test("PlutusV3 TxInfo allows the overlap before PV 11, spec [TX-1]") {
        // The UTXO rule rejects it at PV 9 and 10 (BabbageNonDisjointRefInputs) instead.
        val info = txInfoV3(tx(Set(a, b), Set(b)), MajorProtocolVersion.plominPV)
        assert(info.inputs.length == BigInt(2) && info.referenceInputs.length == BigInt(1))
    }

    test("PlutusV3 TxInfo of disjoint inputs and reference inputs builds at PV 11") {
        val info = txInfoV3(tx(Set(a, b), Set(c)), MajorProtocolVersion.vanRossemPV)
        assert(info.inputs.length == BigInt(2) && info.referenceInputs.length == BigInt(1))
    }

    test("PlutusV2 TxInfo allows the overlap at PV 11, spec [TX-1]") {
        val info = LedgerToPlutusTranslation.getTxInfoV2(
          tx(Set(a, b), Set(b)),
          utxos,
          SlotConfig.mainnet,
          MajorProtocolVersion.vanRossemPV
        )
        assert(info.inputs.length == BigInt(2) && info.referenceInputs.length == BigInt(1))
    }
}
