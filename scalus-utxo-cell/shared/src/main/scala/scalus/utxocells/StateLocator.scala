package scalus.utxocells

import scalus.uplc.builtin.ByteString
import scalus.cardano.onchain.plutus.v1.PolicyId

/** Describes how to locate the UTxO state on-chain (find continuing output, detect beacon). */
sealed trait StateLocator

object StateLocator {

    /** Use a beacon token minted by the cell's own policy, with the given token name. */
    case class SelfBeacon(tokenName: ByteString) extends StateLocator

    /** Use an externally-minted beacon token. */
    case class BeaconToken(policyId: PolicyId, tokenName: ByteString) extends StateLocator

    /** Locate by the script's own address (no beacon). */
    case object ByScriptAddress extends StateLocator
}
