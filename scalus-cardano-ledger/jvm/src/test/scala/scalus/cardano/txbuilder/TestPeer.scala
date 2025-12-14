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
import scalus.cardano.ledger.{Hash, ProtocolVersion, Transaction as STransaction}

import scala.collection.mutable

enum TestPeer(@annotation.unused ix: Int) derives CanEqual:
    case Alice extends TestPeer(0)
    case Bob extends TestPeer(1)
    case Carol extends TestPeer(2)
    case Daniella extends TestPeer(3)
    case Erin extends TestPeer(4)
    case Frank extends TestPeer(5)
    case Gustavo extends TestPeer(6)
    case Hector extends TestPeer(7)
    case Isabel extends TestPeer(8)
    case Julia extends TestPeer(9)

    def compareTo(another: TestPeer): Int = this.toString.compareTo(another.toString)

    def account: Account = TestPeer.account(this)

    def wallet: Wallet = TestPeer.mkWallet(this)

    def walletId: WalletId = TestPeer.mkWalletId(this)

    def address: ShelleyAddress = TestPeer.address(this)

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
      TestPeer.Carol,
      TestPeer.Daniella,
      TestPeer.Erin,
      TestPeer.Frank,
      TestPeer.Gustavo,
      TestPeer.Hector,
      TestPeer.Isabel,
      TestPeer.Julia
    )

/** Choose betweeen 2 and 10 peers */
val genTestPeers: Gen[NonEmptyList[TestPeer]] =
    for {
        numPeers <- Gen.choose(2, 10)
        peersList = List(
          TestPeer.Alice,
          TestPeer.Bob,
          TestPeer.Carol,
          TestPeer.Daniella,
          TestPeer.Erin,
          TestPeer.Frank,
          TestPeer.Gustavo,
          TestPeer.Hector,
          TestPeer.Isabel,
          TestPeer.Julia
        )
    } yield NonEmptyList.fromList(peersList.take(numPeers)).get
