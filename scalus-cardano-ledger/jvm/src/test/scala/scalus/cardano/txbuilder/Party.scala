package scalus.cardano.txbuilder

import com.bloxbean.cardano.client.account.Account
import com.bloxbean.cardano.client.common.model.Network as BBNetwork
import com.bloxbean.cardano.client.crypto.cip1852.DerivationPath.createExternalAddressDerivationPathForAccount
import org.scalacheck.{Arbitrary, Gen}
import scalus.cardano.address.Network.Mainnet
import scalus.cardano.address.ShelleyDelegationPart.Null
import scalus.cardano.address.ShelleyPaymentPart.Key
import scalus.cardano.address.{Network, ShelleyAddress, ShelleyPaymentPart}
import scalus.cardano.ledger.AddrKeyHash
import scalus.cardano.wallet.BloxbeanAccount

import scala.collection.mutable

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

    def compareTo(another: Party): Int = this.toString.compareTo(another.toString)

    def account: Account = Party.account(this)

    def address: ShelleyAddress = Party.address(this)

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

    def account(party: Party): Account = accountCache.getOrElseUpdate(
      party,
      Account.createFromMnemonic(
        BBNetwork(0, 42),
        mnemonic,
        createExternalAddressDerivationPathForAccount(party.ordinal)
      )
    )

    def address(party: Party, network: Network = Mainnet): ShelleyAddress = {
        val pkh = account(party).hdKeyPair().getPublicKey.getKeyHash
        ShelleyAddress(network, Key(AddrKeyHash.fromArray(pkh)), Null)
    }

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
