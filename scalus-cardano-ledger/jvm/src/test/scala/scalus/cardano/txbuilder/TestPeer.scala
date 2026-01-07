package scalus.cardano.txbuilder

import cats.data.NonEmptyList
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

enum TestPeer(@annotation.unused ix: Int) derives CanEqual:
    case Alice extends TestPeer(0) // First party
    case Bob extends TestPeer(1) // Second party
    case Charles extends TestPeer(2) // Third party
    case Dave extends TestPeer(3) // Fourth party
    case Eve extends TestPeer(4) // Eavesdropper
    case Faith extends TestPeer(5) // Trusted third party
    case Grace extends TestPeer(6) // Government representative
    case Heidi extends TestPeer(7)
    case Ivan extends TestPeer(8) // Issuer
    case Judy extends TestPeer(9) // Judge
    case Kevin extends TestPeer(10)
    case Laura extends TestPeer(11)
    case Mallory extends TestPeer(12) // Malicious attacker
    case Nathan extends TestPeer(13)
    case Oracle extends TestPeer(14) // Blockchain oracle
    case Peggy extends TestPeer(15) // Prover
    case Quentin extends TestPeer(16)
    case Rachel extends TestPeer(17)
    case Sybil extends TestPeer(18) // Sybil attack
    case Trent extends TestPeer(19) // Trusted third party
    case Ursula extends TestPeer(20)
    case Victor extends TestPeer(21) // Verifier
    case Wendy extends TestPeer(22) // Whistleblower
    case Xavier extends TestPeer(23)
    case Yve extends TestPeer(24)
    case Zulu extends TestPeer(25)

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
    Gen.oneOf(
      TestPeer.Alice,
      TestPeer.Bob,
      TestPeer.Charles,
      TestPeer.Dave,
      TestPeer.Eve,
      TestPeer.Faith,
      TestPeer.Grace,
      TestPeer.Heidi,
      TestPeer.Ivan,
      TestPeer.Judy,
      TestPeer.Kevin,
      TestPeer.Laura,
      TestPeer.Mallory,
      TestPeer.Nathan,
      TestPeer.Oracle,
      TestPeer.Peggy,
      TestPeer.Quentin,
      TestPeer.Rachel,
      TestPeer.Sybil,
      TestPeer.Trent,
      TestPeer.Ursula,
      TestPeer.Victor,
      TestPeer.Wendy,
      TestPeer.Xavier,
      TestPeer.Yve,
      TestPeer.Zulu
    )

/** Choose between 2 and 26 peers */
val genTestPeers: Gen[NonEmptyList[TestPeer]] =
    for {
        numPeers <- Gen.choose(2, 26)
        peersList = List(
          TestPeer.Alice,
          TestPeer.Bob,
          TestPeer.Charles,
          TestPeer.Dave,
          TestPeer.Eve,
          TestPeer.Faith,
          TestPeer.Grace,
          TestPeer.Heidi,
          TestPeer.Ivan,
          TestPeer.Judy,
          TestPeer.Kevin,
          TestPeer.Laura,
          TestPeer.Mallory,
          TestPeer.Nathan,
          TestPeer.Oracle,
          TestPeer.Peggy,
          TestPeer.Quentin,
          TestPeer.Rachel,
          TestPeer.Sybil,
          TestPeer.Trent,
          TestPeer.Ursula,
          TestPeer.Victor,
          TestPeer.Wendy,
          TestPeer.Xavier,
          TestPeer.Yve,
          TestPeer.Zulu
        )
    } yield NonEmptyList.fromList(peersList.take(numPeers)).get
