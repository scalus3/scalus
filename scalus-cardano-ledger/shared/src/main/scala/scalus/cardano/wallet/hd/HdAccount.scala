package scalus.cardano.wallet.hd

import scalus.uplc.builtin.platform
import scalus.cardano.address.*
import scalus.cardano.ledger.{AddrKeyHash, Hash, StakeKeyHash}
import scalus.cardano.wallet.{Account, KeyPair}
import scalus.crypto.ed25519.Ed25519Signer

/** HD wallet account implementing CIP-1852 derivation with BIP32-Ed25519.
  *
  * Provides payment, change, staking, and DRep keys derived from a BIP-39 mnemonic using
  * BIP32-Ed25519 key derivation (Icarus-style) and CIP-1852/CIP-105 path structure.
  *
  * This implementation is compatible with standard Cardano wallets (Daedalus, Yoroi, etc.) because
  * it uses BIP32-Ed25519 which supports both hardened and non-hardened derivation.
  *
  * Path structure (CIP-1852 / CIP-105):
  *   - Payment keys: m/1852'/1815'/account'/0/index (role and index are non-hardened)
  *   - Change keys: m/1852'/1815'/account'/1/index
  *   - Staking keys: m/1852'/1815'/account'/2/index
  *   - DRep keys: m/1852'/1815'/account'/3/index (CIP-105)
  *
  * @param accountIndex
  *   the account index (0, 1, 2, ...)
  * @param accountKey
  *   the account-level extended key at path m/1852'/1815'/account'
  * @param paymentIndex
  *   the payment address index (default 0)
  * @param changeIndex
  *   the change address index (default 0)
  * @param stakingIndex
  *   the staking key index (default 0)
  * @param drepIndex
  *   the DRep key index (default 0, CIP-105 recommends only index 0)
  */
class HdAccount(
    val accountIndex: Int,
    private val accountKey: HdKeyPair,
    val paymentIndex: Int = 0,
    val changeIndex: Int = 0,
    val stakingIndex: Int = 0,
    val drepIndex: Int = 0
) extends Account {

    /** The account-level extended key (m/1852'/1815'/account'). */
    def accountKeyPair: HdKeyPair = accountKey

    /** Payment (external) key for receiving funds. */
    override lazy val paymentKeyPair: KeyPair = derivePaymentKey(paymentIndex)

    /** Change (internal) key for transaction change outputs. */
    override lazy val changeKeyPair: KeyPair = deriveChangeKey(changeIndex)

    /** Staking key for delegation and rewards. */
    override lazy val stakeKeyPair: KeyPair = deriveStakingKey(stakingIndex)

    /** DRep (governance) key - for Cardano governance participation.
      *
      * This is a dedicated key derived at role 3 (m/1852'/1815'/account'/3/index) per CIP-105,
      * separate from the staking key at role 2. Using a distinct key for governance provides better
      * security isolation between staking delegation and governance voting.
      *
      * Note: CIP-105 recommends using only index 0 for DRep keys (one DRep per account).
      *
      * @see
      *   https://cips.cardano.org/cip/CIP-0105
      */
    override lazy val drepKeyPair: KeyPair = deriveDrepKey(drepIndex)

    /** Get the payment key hash (Blake2b-224 of verification key). */
    lazy val paymentKeyHash: AddrKeyHash = keyHash(paymentKeyPair)

    /** Get the stake key hash (Blake2b-224 of verification key). */
    lazy val stakeKeyHash: StakeKeyHash = Hash.stakeKeyHash(
      platform.blake2b_224(stakeKeyPair.verificationKey)
    )

    /** Get the base address for this account.
      *
      * Base address = payment key hash + stake key hash
      *
      * @param network
      *   the Cardano network (Mainnet or Testnet)
      * @return
      *   the base ShelleyAddress
      */
    def baseAddress(network: Network): ShelleyAddress =
        ShelleyAddress(
          network,
          ShelleyPaymentPart.Key(paymentKeyHash),
          ShelleyDelegationPart.Key(stakeKeyHash)
        )

    /** Get the enterprise address for this account.
      *
      * Enterprise address = payment key hash only (no staking)
      *
      * @param network
      *   the Cardano network (Mainnet or Testnet)
      * @return
      *   the enterprise ShelleyAddress
      */
    def enterpriseAddress(network: Network): ShelleyAddress =
        ShelleyAddress(
          network,
          ShelleyPaymentPart.Key(paymentKeyHash),
          ShelleyDelegationPart.Null
        )

    /** Get the stake address for this account.
      *
      * @param network
      *   the Cardano network (Mainnet or Testnet)
      * @return
      *   the StakeAddress
      */
    def stakeAddress(network: Network): StakeAddress =
        StakeAddress(network, StakePayload.Stake(stakeKeyHash))

    private def keyHash(keyPair: KeyPair): AddrKeyHash =
        AddrKeyHash(platform.blake2b_224(keyPair.verificationKey))

    /** Derive a payment key at the given index (non-hardened per CIP-1852). */
    def derivePaymentKey(index: Int): HdKeyPair = {
        require(index >= 0, s"Index must be non-negative, got $index")
        // m/1852'/1815'/account'/0/index (role and index are non-hardened per CIP-1852)
        accountKey.deriveNormal(Cip1852.RoleExternal).deriveNormal(index)
    }

    /** Derive a change key at the given index (non-hardened per CIP-1852). */
    def deriveChangeKey(index: Int): HdKeyPair = {
        require(index >= 0, s"Index must be non-negative, got $index")
        // m/1852'/1815'/account'/1/index (role and index are non-hardened per CIP-1852)
        accountKey.deriveNormal(Cip1852.RoleInternal).deriveNormal(index)
    }

    /** Derive a staking key at the given index (non-hardened per CIP-1852). */
    def deriveStakingKey(index: Int): HdKeyPair = {
        require(index >= 0, s"Index must be non-negative, got $index")
        // m/1852'/1815'/account'/2/index (role and index are non-hardened per CIP-1852)
        accountKey.deriveNormal(Cip1852.RoleStaking).deriveNormal(index)
    }

    /** Derive a DRep key at the given index (non-hardened per CIP-105). */
    def deriveDrepKey(index: Int): HdKeyPair = {
        require(index >= 0, s"Index must be non-negative, got $index")
        // m/1852'/1815'/account'/3/index (role and index are non-hardened per CIP-105)
        accountKey.deriveNormal(Cip1852.RoleDRep).deriveNormal(index)
    }

    /** Create a new HdAccount with a different payment index. */
    def withPaymentIndex(index: Int): HdAccount =
        new HdAccount(accountIndex, accountKey, index, changeIndex, stakingIndex, drepIndex)

    /** Create a new HdAccount with a different change index. */
    def withChangeIndex(index: Int): HdAccount =
        new HdAccount(accountIndex, accountKey, paymentIndex, index, stakingIndex, drepIndex)

    /** Create a new HdAccount with a different staking index. */
    def withStakingIndex(index: Int): HdAccount =
        new HdAccount(accountIndex, accountKey, paymentIndex, changeIndex, index, drepIndex)

    /** Create a new HdAccount with a different DRep index. */
    def withDrepIndex(index: Int): HdAccount =
        new HdAccount(accountIndex, accountKey, paymentIndex, changeIndex, stakingIndex, index)
}

object HdAccount {

    /** Create an HdAccount from a BIP-39 mnemonic.
      *
      * Uses BIP32-Ed25519 (Icarus-style) key derivation which is compatible with standard Cardano
      * wallets like Daedalus, Yoroi, and others.
      *
      * @param mnemonic
      *   the BIP-39 mnemonic sentence
      * @param passphrase
      *   optional passphrase (empty string if none)
      * @param accountIndex
      *   the account index (default 0)
      * @return
      *   the HD account
      */
    def fromMnemonic(mnemonic: String, passphrase: String = "", accountIndex: Int = 0)(using
        Ed25519Signer
    ): HdAccount = {
        require(accountIndex >= 0, s"Account index must be non-negative, got $accountIndex")
        val accountPath = Cip1852.accountPath(accountIndex)
        val accountKey = HdKeyPair.fromMnemonic(mnemonic, passphrase, accountPath)
        new HdAccount(accountIndex, accountKey)
    }
}
