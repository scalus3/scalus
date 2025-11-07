package scalus.testing.regression.cosmex20251107.step4
import scalus.*
import scalus.builtin.Builtins.*
import scalus.builtin.Data.toData
import scalus.builtin.{ByteString, Data, FromData, ToData}
import scalus.ledger.api.v2
import scalus.ledger.api.v3.*
import scalus.prelude.*

type PubKey = ByteString
type Signature = ByteString

case class Snapshot(
    snapshotVersion: BigInt
)

case class SignedSnapshot(
    signedSnapshot: Snapshot,
    snapshotClientSignature: Signature,
    snapshotExchangeSignature: Signature
)

enum Action:
    case Close(signedSnapshot: SignedSnapshot)

case class OnChainState(
    clientPkh: PubKeyHash,
    clientPubKey: ByteString,
    clientTxOutRef: TxOutRef
)

@Compile
object CosmexToDataInstances {
    given Data.ToData[Snapshot] = ToData.derived
    given Data.ToData[SignedSnapshot] = ToData.derived
    given Data.ToData[Action] = ToData.derived
    given Data.ToData[OnChainState] = ToData.derived
}

@Compile
object CosmexFromDataInstances {
    given Data.FromData[Snapshot] = FromData.derived
    given Data.FromData[SignedSnapshot] = FromData.derived
    given Data.FromData[Action] = FromData.derived
    given Data.FromData[OnChainState] = FromData.derived
}

@Compile
object CosmexContract extends DataParameterizedValidator {

    import CosmexFromDataInstances.given
    import CosmexToDataInstances.given

    def validSignedSnapshot(
        signedSnapshot: SignedSnapshot
    ): Boolean = true

    inline def handleClose(
        state: OnChainState,
        newSignedSnapshot: SignedSnapshot,
        ownTxInResolvedTxOut: TxOut
    ) = {
        state match
            case OnChainState(_, _, _) =>
                newSignedSnapshot match
                    case SignedSnapshot(signedSnapshot, _, _) =>
                        ownTxInResolvedTxOut match
                            case TxOut(_, _, _, _) =>
                                validSignedSnapshot(newSignedSnapshot)
    }

    inline override def spend(
        param: Datum,
        datum: Option[Datum],
        redeemer: Datum,
        tx: TxInfo,
        ownRef: TxOutRef
    ): Unit = {
        val key = ByteString.fromHex("aabbccdd")
        val snap = Snapshot(1)
        val signed = SignedSnapshot(snap, key, key)
        val action = Action.Close(signed)
        val state = OnChainState(PubKeyHash(key), key, ownRef)
        val txOut = TxOut(Address(Credential.PubKeyCredential(PubKeyHash(key)), Option.None), Value.zero, v2.OutputDatum.NoOutputDatum, Option.None)
        
        import Action.*
        val result = action match
            case Close(signedSnapshot) =>
                handleClose(
                  state,
                  signedSnapshot,
                  txOut
                )
        require(result, "Validation failed")
    }
}
