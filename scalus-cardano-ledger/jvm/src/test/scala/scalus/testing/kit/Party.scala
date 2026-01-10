package scalus.testing.kit

import org.scalacheck.{Arbitrary, Gen}
import scalus.cardano.address.ShelleyDelegationPart.Null
import scalus.cardano.address.ShelleyPaymentPart.Key
import scalus.cardano.address.ShelleyAddress
import scalus.cardano.ledger.{AddrKeyHash, CardanoInfo}
import scalus.cardano.txbuilder.TransactionSigner
import scalus.cardano.wallet.hd.HdAccount
import scalus.crypto.ed25519.JvmEd25519Signer
import scalus.testing.kit.Party.{accountCache, mnemonic}

import scala.annotation.threadUnsafe
import scala.collection.mutable

// Use JVM Ed25519 signer for key derivation
private given scalus.crypto.ed25519.Ed25519Signer = JvmEd25519Signer

/** Test parties for smart contract testing.
  *
  * Each party has a deterministic wallet derived from a test mnemonic, providing:
  *   - `hdAccount`: HdAccount for CIP-1852 compatible key management
  *   - `address`: Shelley address for receiving funds
  *   - `signer`: TransactionSigner for signing transactions
  *
  * Many names reference classic cryptographic roles (Alice, Bob, Eve, Mallory, etc.) or notable
  * figures in cryptography and blockchain (Hal Finney, Nick Szabo).
  *
  * There are exactly 20 parties to match Yaci Devkit's default configuration, which creates 20
  * funded addresses from the same test mnemonic. This allows the same test code to work with both
  * the in-memory [[scalus.cardano.node.Emulator]] (for fast unit tests) and Yaci Devkit (for
  * integration tests).
  */
enum Party derives CanEqual {
    case Alice // First party
    case Bob // Second party
    case Charles // Third party
    case Dave // Fourth party
    case Eve // Eavesdropper
    case Faith // Trusted third party
    case Grace // Government representative
    case Hal // Hal Finney
    case Ivan // Issuer
    case Judy // Judge
    case Kevin
    case Laura
    case Mallory // Malicious attacker
    case Nick // Nick Szabo
    case Oracle // Blockchain oracle
    case Peggy // Prover
    case Sybil // Sybil attack
    case Trent // Trusted third party
    case Victor // Verifier
    case Wendy // Whistleblower

    /** HD wallet account for this party (CIP-1852 compatible). */
    def hdAccount: HdAccount = accountCache.getOrElseUpdate(
      this,
      HdAccount.fromMnemonic(mnemonic, "", this.ordinal)
    )

    @threadUnsafe
    lazy val addrKeyHash: AddrKeyHash = hdAccount.paymentKeyHash

    def address(using env: CardanoInfo): ShelleyAddress = {
        ShelleyAddress(env.network, Key(addrKeyHash), Null)
    }

    def signer: TransactionSigner = {
        new TransactionSigner(Set(hdAccount.paymentKeyPair))
    }
}

object Party {
    private val mnemonic: String =
        "test test test test " +
            "test test test test " +
            "test test test test " +
            "test test test test " +
            "test test test test " +
            "test test test sauce"

    private val accountCache: mutable.Map[Party, HdAccount] = mutable.Map.empty

    /////////////////////////////
    // Generators

    val genParty: Gen[Party] =
        Gen.choose(0, Party.values.length - 1).map(Party.fromOrdinal)

    /** Choose between 2 and 20 parties */
    val genParties: Gen[Seq[Party]] =
        for numParties <- Gen.choose(2, Party.values.length)
        yield Party.values.take(numParties).toSeq

    given Arbitrary[Party] = Arbitrary(genParty)
}
