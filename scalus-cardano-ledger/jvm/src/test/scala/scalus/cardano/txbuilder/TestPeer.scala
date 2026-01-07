package scalus.cardano.txbuilder

import com.bloxbean.cardano.client.account.Account
import com.bloxbean.cardano.client.common.model.Network as BBNetwork
import com.bloxbean.cardano.client.crypto.cip1852.DerivationPath.createExternalAddressDerivationPathForAccount
import org.scalacheck.Gen
import scalus.builtin.Builtins.blake2b_224
import scalus.builtin.ByteString
import scalus.cardano.address.Network.Mainnet
import scalus.cardano.address.ShelleyDelegationPart.Null
import scalus.cardano.address.ShelleyPaymentPart.Key
import scalus.cardano.address.{Network, ShelleyAddress, ShelleyDelegationPart, ShelleyPaymentPart}
import scalus.cardano.ledger.Hash
import scalus.cardano.wallet.BloxbeanAccount

import scala.collection.mutable

enum TestPeer derives CanEqual:
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

    def compareTo(another: TestPeer): Int = this.toString.compareTo(another.toString)

    def account: Account = TestPeer.account(this)

    def wallet: Wallet = TestPeer.mkWallet(this)

    def walletId: WalletId = TestPeer.mkWalletId(this)

    def address: ShelleyAddress = TestPeer.address(this)

    def signer: TransactionSigner = {
        new TransactionSigner(Set(new BloxbeanAccount(account).paymentKeyPair))
    }

object TestPeer:
    private val mnemonic: String =
        "test test test test " +
            "test test test test " +
            "test test test test " +
            "test test test test " +
            "test test test test " +
            "test test test sauce"

    private val accountCache: mutable.Map[TestPeer, Account] = mutable.Map.empty
        .withDefault(peer =>
            Account.createFromMnemonic(
              BBNetwork(0, 42),
              mnemonic,
              createExternalAddressDerivationPathForAccount(peer.ordinal)
            )
        )

    private val walletCache: mutable.Map[TestPeer, Wallet] = mutable.Map.empty
        .withDefault(peer =>
            Wallet(
              peer.toString,
              WalletModuleBloxbean,
              account(peer).hdKeyPair().getPublicKey,
              account(peer).hdKeyPair().getPrivateKey
            )
        )

    private val addressCache: mutable.Map[TestPeer, (ShelleyPaymentPart, ShelleyDelegationPart)] =
        mutable.Map.empty.withDefault(peer =>
            (
              Key(Hash(blake2b_224(ByteString.fromArray(account(peer).publicKeyBytes())))),
              Null
            )
        )

    def account(peer: TestPeer): Account = accountCache.cache(peer)

    def mkWallet(peer: TestPeer): Wallet = walletCache.cache(peer)

    def mkWalletId(peer: TestPeer): WalletId = WalletId(peer.toString)

    def address(peer: TestPeer, network: Network = Mainnet): ShelleyAddress = {
        val (payment, delegation) = addressCache.cache(peer)
        ShelleyAddress(network, payment, delegation)
    }

extension [K, V](map: mutable.Map[K, V])
    def cache(key: K): V = map.get(key) match {
        case None =>
            val missing = map.default(key)
            @annotation.unused
            val _ = map.put(key, missing)
            missing
        case Some(value) => value
    }

/////////////////////////////
// Generators

val genTestPeer: Gen[TestPeer] =
    Gen.choose(0, TestPeer.values.length - 1).map(TestPeer.fromOrdinal)

/** Choose between 2 and 26 peers */
val genTestPeers: Gen[Seq[TestPeer]] =
    for numPeers <- Gen.choose(2, TestPeer.values.length)
    yield TestPeer.values.take(numPeers).toSeq
