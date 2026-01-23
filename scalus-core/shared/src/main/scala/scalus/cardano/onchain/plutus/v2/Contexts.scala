package scalus.cardano.onchain.plutus.v2

import scalus.Compile
import scalus.uplc.builtin.Data
import scalus.uplc.builtin.FromData
import scalus.uplc.builtin.ToData
import scalus.cardano.onchain.plutus.v2.OutputDatum.NoOutputDatum
import scalus.cardano.onchain.plutus.prelude.{<=>, ===, Eq, List, Option, Ord, Order, SortedMap}
import scalus.uplc.builtin.ByteString.*

enum OutputDatum:
    case NoOutputDatum
    case OutputDatumHash(datumHash: DatumHash)
    case OutputDatum(datum: Datum)

@Compile
object OutputDatum {
    given Eq[scalus.cardano.onchain.plutus.v2.OutputDatum] =
        (
            a: scalus.cardano.onchain.plutus.v2.OutputDatum,
            b: scalus.cardano.onchain.plutus.v2.OutputDatum
        ) =>
            a match
                case NoOutputDatum =>
                    b match
                        case NoOutputDatum => true
                        case _             => false
                case OutputDatumHash(datumHash) =>
                    b match
                        case OutputDatumHash(datumHash2) => datumHash == datumHash2
                        case _                           => false
                case OutputDatum(datum) =>
                    b match
                        case OutputDatum(datum2) => datum == datum2
                        case _                   => false

    given Ord[scalus.cardano.onchain.plutus.v2.OutputDatum] =
        (
            x: scalus.cardano.onchain.plutus.v2.OutputDatum,
            y: scalus.cardano.onchain.plutus.v2.OutputDatum
        ) =>
            x match
                case NoOutputDatum =>
                    y match
                        case NoOutputDatum => Order.Equal
                        case _             => Order.Less

                case OutputDatumHash(hash1) =>
                    y match
                        case NoOutputDatum          => Order.Greater
                        case OutputDatumHash(hash2) => hash1 <=> hash2
                        case _                      => Order.Less

                case OutputDatum(datum1) =>
                    y match
                        case OutputDatum(datum2) => datum1 <=> datum2
                        case _                   => Order.Greater

    given ToData[scalus.cardano.onchain.plutus.v2.OutputDatum] = ToData.derived
    given FromData[scalus.cardano.onchain.plutus.v2.OutputDatum] = FromData.derived
}

case class TxOut(
    address: Address,
    value: Value,
    datum: OutputDatum = NoOutputDatum,
    referenceScript: Option[ScriptHash] = Option.None
)

@Compile
object TxOut {

    given Eq[TxOut] = (a: TxOut, b: TxOut) =>
        a.address === b.address &&
            a.value === b.value &&
            a.datum === b.datum &&
            a.referenceScript === b.referenceScript

    given Ord[TxOut] = (x: TxOut, y: TxOut) =>
        given Ord[Value] = Value.valueOrd
        (x.address <=> y.address) ifEqualThen
            (x.value <=> y.value) ifEqualThen
            (x.datum <=> y.datum) ifEqualThen
            (x.referenceScript <=> y.referenceScript)

    given ToData[TxOut] = ToData.derived

    given FromData[TxOut] = FromData.derived

}

case class TxInInfo(outRef: TxOutRef, resolved: TxOut)

@Compile
object TxInInfo {
    given Eq[TxInInfo] = (a: TxInInfo, b: TxInInfo) =>
        a.outRef === b.outRef && a.resolved === b.resolved

    given Ord[TxInInfo] = (x: TxInInfo, y: TxInInfo) =>
        (x.outRef <=> y.outRef) ifEqualThen (x.resolved <=> y.resolved)

    given ToData[TxInInfo] = ToData.derived

    given FromData[TxInInfo] = FromData.derived

}

/** A pending transaction.
  */
case class TxInfo(
    inputs: List[TxInInfo],
    referenceInputs: List[TxInInfo],
    outputs: List[TxOut],
    fee: Value,
    mint: Value,
    dcert: List[DCert],
    withdrawals: SortedMap[StakingCredential, BigInt],
    validRange: PosixTimeRange,
    signatories: List[PubKeyHash],
    redeemers: SortedMap[ScriptPurpose, Redeemer],
    data: SortedMap[DatumHash, Datum],
    id: TxId
)

@Compile
object TxInfo {
    val placeholder: TxInfo = TxInfo(
      inputs = List.empty,
      referenceInputs = List.empty,
      outputs = List.empty,
      fee = Value.zero,
      mint = Value.zero,
      dcert = List.empty,
      withdrawals = SortedMap.empty,
      validRange = Interval.always,
      signatories = List.empty,
      redeemers = SortedMap.empty,
      data = SortedMap.empty,
      id = TxId(hex"0000000000000000000000000000000000000000000000000000000000000000")
    )

    given Eq[TxInfo] = (a: TxInfo, b: TxInfo) =>
        a.inputs === b.inputs &&
            a.referenceInputs === b.referenceInputs &&
            a.outputs === b.outputs &&
            a.fee === b.fee &&
            a.mint === b.mint &&
            a.dcert === b.dcert &&
            a.withdrawals === b.withdrawals &&
            a.validRange === b.validRange &&
            a.signatories === b.signatories &&
            a.redeemers === b.redeemers &&
            a.data === b.data &&
            a.id === b.id

    given Ord[TxInfo] = (x: TxInfo, y: TxInfo) =>
        given Ord[Value] = Value.valueOrd
        (x.inputs <=> y.inputs) ifEqualThen
            (x.referenceInputs <=> y.referenceInputs) ifEqualThen
            (x.outputs <=> y.outputs) ifEqualThen
            (x.fee <=> y.fee) ifEqualThen
            (x.mint <=> y.mint) ifEqualThen
            (x.dcert <=> y.dcert) ifEqualThen
            (x.withdrawals <=> y.withdrawals) ifEqualThen
            (x.validRange <=> y.validRange) ifEqualThen
            (x.signatories <=> y.signatories) ifEqualThen
            (x.redeemers <=> y.redeemers) ifEqualThen
            (x.data <=> y.data) ifEqualThen
            (x.id <=> y.id)

    given ToData[TxInfo] = ToData.derived

    given FromData[TxInfo] = FromData.derived

    extension (self: TxInfo) {
        def findOwnInput(outRef: TxOutRef): Option[TxInInfo] = {
            Utils.findInput(self.inputs, outRef)
        }

        def findOwnDatum(datumHash: DatumHash): Option[Datum] = {
            Utils.findDatum(self.outputs, self.data, datumHash)
        }

        def findOwnScriptOutputs(scriptHash: ValidatorHash): List[TxOut] = {
            Utils.findScriptOutputs(self.outputs, scriptHash)
        }
    }
}

/** The context that the currently-executing script can access.
  */
case class ScriptContext(
    txInfo: TxInfo,
    purpose: ScriptPurpose
)

@Compile
object ScriptContext {
    given Eq[ScriptContext] = (a: ScriptContext, b: ScriptContext) =>
        a.txInfo === b.txInfo && a.purpose === b.purpose

    given Ord[ScriptContext] = (x: ScriptContext, y: ScriptContext) =>
        (x.txInfo <=> y.txInfo) ifEqualThen (x.purpose <=> y.purpose)

    given ToData[ScriptContext] = ToData.derived
    given FromData[ScriptContext] = FromData.derived

}

@Compile
object Utils {

    /** Finds an input in the list of inputs by its out reference.
      *
      * @param inputs
      *   The list of inputs to search in.
      * @param outRef
      *   The output reference to find.
      * @return
      *   An `Option` containing the found input, or `None` if not found.
      */
    def findInput(inputs: List[TxInInfo], outRef: TxOutRef): Option[TxInInfo] = {
        inputs.find(_.outRef === outRef)
    }

    /** Finds a datum firstly in the datum map by its hash otherwise in the outputs.
      *
      * @param outputs
      *   The list of outputs to search in.
      * @param datum
      *   The map of datum hashes to datums.
      * @param datumHash
      *   The hash of the datum to find.
      * @return
      *   An `Option` containing the found datum, or `None` if not found.
      */
    def findDatum(
        outputs: List[TxOut],
        datum: SortedMap[DatumHash, Datum],
        datumHash: DatumHash
    ): Option[Datum] = {
        datum.get(datumHash) match
            case Option.Some(datum) => Option.Some(datum)
            case Option.None =>
                outputs.findMap { output =>
                    output.datum match
                        case OutputDatum.OutputDatum(data) =>
                            if data.dataHash === datumHash then Option.Some(data)
                            else Option.None
                        case _ => Option.None
                }
    }

    /** Finds all outputs that match a given script hash.
      *
      * @param outputs
      *   The list of outputs to search in.
      * @param scriptHash
      *   The script hash to match against the outputs' addresses.
      * @return
      *   A list of outputs that match the script hash.
      */
    def findScriptOutputs(outputs: List[TxOut], scriptHash: ValidatorHash): List[TxOut] = {
        outputs.filter { output =>
            output.address.credential match
                case Credential.ScriptCredential(hash) => hash === scriptHash
                case _                                 => false
        }
    }
}
