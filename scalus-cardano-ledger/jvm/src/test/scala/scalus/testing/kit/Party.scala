package scalus.testing.kit

import com.bloxbean.cardano.client.account.Account
import com.bloxbean.cardano.client.common.model.Network as BBNetwork
import com.bloxbean.cardano.client.crypto.cip1852.DerivationPath.createExternalAddressDerivationPathForAccount
import org.scalacheck.{Arbitrary, Gen}
import scalus.builtin.ByteString
import scalus.cardano.address.ShelleyDelegationPart.Null
import scalus.cardano.address.ShelleyPaymentPart.Key
import scalus.cardano.address.{ShelleyAddress, ShelleyPaymentPart}
import scalus.cardano.ledger.{AddrKeyHash, CardanoInfo}
import scalus.cardano.txbuilder.TransactionSigner
import scalus.cardano.wallet.BloxbeanAccount
import scalus.testing.kit.Party.{accountCache, mnemonic}

import scala.annotation.threadUnsafe
import scala.collection.mutable

/** Test parties (A-Z) for smart contract testing.
  *
  * Each party has a deterministic wallet derived from a test mnemonic, providing:
  *   - `account`: Bloxbean Account for key management
  *   - `address`: Shelley address for receiving funds
  *   - `signer`: TransactionSigner for signing transactions
  *
  * Many names reference classic cryptographic roles (Alice, Bob, Eve, Mallory, etc.) or notable
  * figures in cryptography and blockchain (Hal Finney, Nick Szabo).
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
    case Quentin // Quentin Tarantino
    case Rachel
    case Sybil // Sybil attack
    case Trent // Trusted third party
    case Ursula
    case Victor // Verifier
    case Wendy // Whistleblower
    case Xavier // Xavier Leroy
    case Yve
    case Zulu

    def account: Account = accountCache.getOrElseUpdate(
      this,
      Account.createFromMnemonic(
        BBNetwork(0, 42),
        mnemonic,
        createExternalAddressDerivationPathForAccount(this.ordinal)
      )
    )

    @threadUnsafe
    lazy val addrKeyHash: AddrKeyHash = {
        val pkh = ByteString.unsafeFromArray(account.hdKeyPair().getPublicKey.getKeyHash)
        AddrKeyHash.fromByteString(pkh)
    }

    def address(using env: CardanoInfo): ShelleyAddress = {
        ShelleyAddress(env.network, Key(addrKeyHash), Null)
    }

    def signer: TransactionSigner = {
        new TransactionSigner(Set(new BloxbeanAccount(account).paymentKeyPair))
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

    private val accountCache: mutable.Map[Party, Account] = mutable.Map.empty

    /////////////////////////////
    // Generators

    val genParty: Gen[Party] =
        Gen.choose(0, Party.values.length - 1).map(Party.fromOrdinal)

    /** Choose between 2 and 26 peers */
    val genParties: Gen[Seq[Party]] =
        for numParties <- Gen.choose(2, Party.values.length)
        yield Party.values.take(numParties).toSeq

    given Arbitrary[Party] = Arbitrary(genParty)
}
