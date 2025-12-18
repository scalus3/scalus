package scalus.cardano.wallet

import scala.scalajs.js
import scala.scalajs.js.annotation.JSImport
import scala.scalajs.js.typedarray.Uint8Array

// LucidEvolution wrappers for wallet + signing logic
//

@JSImport("@lucid-evolution/wallet", "walletFromSeed")
@js.native
def walletFromSeed(seed: String, options: js.UndefOr[WalletOptions]): WalletCredentials = js.native

private[wallet] class WalletOptions(
    val password: js.UndefOr[String] = js.undefined,
    val addressType: js.UndefOr[String] = js.undefined, // "Base" | "Enterprise"
    val accountIndex: js.UndefOr[Int] = js.undefined,
    val network: js.UndefOr[String] = js.undefined // "Mainnet" | "Testnet"
) extends js.Object

@js.native
private[wallet] trait WalletCredentials extends js.Object {
    val address: String = js.native
    val rewardAddress: js.UndefOr[String] = js.native
    // Bech32-encoded private keys
    val paymentKey: String = js.native
    val stakeKey: js.UndefOr[String] = js.native
}

// Cardano Multiplatform Library wrappers for JS crypto primitives
//

@JSImport("@anastasia-labs/cardano-multiplatform-lib-nodejs", JSImport.Namespace)
@js.native
private[wallet] object CML extends js.Object {
    val PrivateKey: CMLPrivateKeyModule = js.native
}

@js.native
private[wallet] trait CMLPrivateKeyModule extends js.Object {
    def from_bech32(bech32: String): CMLPrivateKey = js.native
}

@js.native
trait CMLPrivateKey extends js.Object {
    def to_raw_bytes(): Uint8Array = js.native
    def to_public(): CMLPublicKey = js.native
    def sign(message: Uint8Array): CMLSignature = js.native
}

@js.native
private[wallet] trait CMLPublicKey extends js.Object {
    def to_raw_bytes(): Uint8Array = js.native
}

@js.native
trait CMLSignature extends js.Object {
    def to_raw_bytes(): Uint8Array = js.native
}
