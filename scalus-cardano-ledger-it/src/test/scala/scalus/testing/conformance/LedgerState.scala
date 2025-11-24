package scalus.testing.conformance

import io.bullet.borer.Dom.Element
import io.bullet.borer.derivation.ArrayBasedCodecs.*
import io.bullet.borer.*
import scalus.cardano.ledger.*

case class LedgerState(certs: LedgerState.CertState, utxos: UTxOState) derives Codec
object LedgerState {

    def fromCbor(cbor: Array[Byte]): LedgerState = {
        given OriginalCborByteArray = OriginalCborByteArray(cbor)
        Cbor.decode(cbor).to[LedgerState].value
    }

    extension (ledgerState: LedgerState)
        def ruleState = rules.State(
          utxos = ledgerState.utxos.utxo,
          deposited = ledgerState.utxos.deposited,
          fees = ledgerState.utxos.fees,
          stakeDistribution = ledgerState.utxos.stakeDistribution,
          donation = ledgerState.utxos.donation
        )

    case class CertState(
        vstate: /* VotingState */ Array[Element],
        pstate: /* PoolsState */ Array[Element],
        dstate: /*DelegationState*/ Array[Element]
    ) derives Codec

    given Decoder[TransactionInput] with
        def read(r: Reader): TransactionInput =
            if r.hasByteArray then MempackParser.parseTransactionInput(r.readByteArray())
            else r.read[TransactionInput]()

    given Decoder[TransactionOutput] with
        def read(r: Reader): TransactionOutput =
            if r.hasByteArray then MempackParser.parseTransactionOutput(r.readByteArray())
            else r.read[TransactionOutput]()

    given Codec[UTxOState] = Codec.derived

}
