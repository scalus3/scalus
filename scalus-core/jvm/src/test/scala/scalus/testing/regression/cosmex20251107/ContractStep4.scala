package scalus.testing.regression.cosmex20251107.step4
import scalus.*
import scalus.uplc.builtin.Builtins.*
import scalus.uplc.builtin.Data.toData
import scalus.uplc.builtin.{ByteString, Data, FromData, ToData}
import scalus.cardano.onchain.plutus.v2
import scalus.cardano.onchain.plutus.v3.*
import scalus.cardano.onchain.plutus.prelude.*
import scalus.cardano.onchain.plutus.v3.DataParameterizedValidator

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
    case Close(party: Party, signedSnapshot: SignedSnapshot)

enum Party:
    case Client

case class OnChainState(
    clientPkh: PubKeyHash,
    clientPubKey: ByteString,
    clientTxOutRef: TxOutRef
)

@Compile
object CosmexDataInstances {
    given Data.ToData[Snapshot] = ToData.derived
    given Data.ToData[SignedSnapshot] = ToData.derived
    given Data.ToData[Action] = ToData.derived
    given Data.ToData[Party] = ToData.derived
    given Data.ToData[OnChainState] = ToData.derived

    given Data.FromData[Snapshot] = FromData.derived
    given Data.FromData[SignedSnapshot] = FromData.derived
    given Data.FromData[Action] = FromData.derived
    given Data.FromData[Party] = FromData.derived
    given Data.FromData[OnChainState] = FromData.derived
}

@Compile
object CosmexContract extends DataParameterizedValidator {
    // Force recompilation - v7 symbol-based fix complete, debug removed
    val dummy: BigInt = BigInt(8)

    import CosmexDataInstances.given

    def validSignedSnapshot(
        signedSnapshot: SignedSnapshot,
        clientTxOutRef: TxOutRef,
        clientPubKey: PubKey,
        exchangePubKey: PubKey
    ): Boolean = {
        signedSnapshot match
            case SignedSnapshot(
                  signedSnapshot,
                  snapshotClientSignature,
                  snapshotExchangeSignature
                ) =>
                // This uses signedSnapshot (Snapshot) with toData which triggers type checking
                val signedInfo = (clientTxOutRef, signedSnapshot)
                val msg = serialiseData(signedInfo.toData)
                val validExchangeSig =
                    verifyEd25519Signature(exchangePubKey, msg, snapshotExchangeSignature)
                val validClientSig =
                    verifyEd25519Signature(clientPubKey, msg, snapshotClientSignature)
                validClientSig && validExchangeSig
    }

    inline def handleClose(
        state: OnChainState,
        newSignedSnapshot: SignedSnapshot,
        ownTxInResolvedTxOut: TxOut
    ) = {
        state match
            case OnChainState(clientPkh, clientPubKey, clientTxOutRef) =>
                newSignedSnapshot match
                    case SignedSnapshot(signedSnapshot, _, _) =>
                        ownTxInResolvedTxOut match
                            case TxOut(_, _, _, _) =>
                                // Here's the bug: due to shadowing, newSignedSnapshot might resolve incorrectly
                                validSignedSnapshot(
                                  newSignedSnapshot,
                                  clientTxOutRef,
                                  clientPubKey,
                                  clientPubKey
                                )
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
        val action = Action.Close(Party.Client, signed)
        val state = OnChainState(PubKeyHash(key), key, ownRef)
        val txOut = TxOut(
          Address(Credential.PubKeyCredential(PubKeyHash(key)), Option.None),
          Value.zero,
          v2.OutputDatum.NoOutputDatum,
          Option.None
        )

        import Action.*
        val result = action match
            case Close(party, signedSnapshot) =>
                handleClose(
                  state,
                  signedSnapshot,
                  txOut
                )
        require(result, "Validation failed")
    }
}
