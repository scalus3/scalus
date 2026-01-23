package scalus.testing.regression.cosmex20251107.minimized
import scalus.*
import scalus.uplc.builtin.{ByteString, Data, FromData, ToData}
import scalus.cardano.onchain.plutus.v3.*
import scalus.cardano.onchain.plutus.prelude.*

case class Snapshot(version: BigInt)

case class SignedSnapshot(
    signedSnapshot: Snapshot,
    sig1: ByteString,
    sig2: ByteString
)

enum Action:
    case Close(signedSnapshot: SignedSnapshot)

@Compile
object CosmexDataInstances {
    given Data.ToData[Snapshot] = ToData.derived
    given Data.ToData[SignedSnapshot] = ToData.derived
    given Data.ToData[Action] = ToData.derived
    given Data.FromData[Snapshot] = FromData.derived
    given Data.FromData[SignedSnapshot] = FromData.derived
    given Data.FromData[Action] = FromData.derived
}

@Compile
object CosmexContract extends DataParameterizedValidator {

    inline def handleClose(newSignedSnapshot: SignedSnapshot) = {
        newSignedSnapshot match
            case SignedSnapshot(_, _, _) =>
                true
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

        import Action.*
        val result = action match
            case Close(signedSnapshot) =>
                handleClose(signedSnapshot)
        require(result, "Validation failed")
    }
}
