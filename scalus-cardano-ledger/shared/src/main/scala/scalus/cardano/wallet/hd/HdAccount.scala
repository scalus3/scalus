package scalus.cardano.wallet.hd

import scalus.cardano.wallet.{Account, KeyPair}
import scalus.crypto.ed25519.Ed25519Signer

/** HD wallet account implementing CIP-1852 derivation with SLIP-0010.
  *
  * Provides payment, change, staking, and DRep keys derived from a BIP-39 seed using SLIP-0010 key
  * derivation and CIP-1852 path structure.
  *
  * IMPORTANT: SLIP-0010 only supports hardened derivation for Ed25519. This implementation uses
  * hardened derivation for all path components (including role and index), which differs from
  * standard CIP-1852. For full CIP-1852 compatibility with non-hardened derivation, use a
  * BIP32-Ed25519 implementation instead.
  *
  * Path structure (all hardened):
  *   - Payment keys: m/1852'/1815'/account'/0'/index'
  *   - Change keys: m/1852'/1815'/account'/1'/index'
  *   - Staking keys: m/1852'/1815'/account'/2'/index'
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
  */
class HdAccount(
    val accountIndex: Int,
    private val accountKey: HdKeyPair,
    val paymentIndex: Int = 0,
    val changeIndex: Int = 0,
    val stakingIndex: Int = 0
)(using Ed25519Signer)
    extends Account {

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
      * Note: CIP-1852 doesn't define a specific path for DRep keys. We use the staking path with
      * index 0 by convention, as governance delegation is conceptually similar to stake delegation.
      */
    override lazy val drepKeyPair: KeyPair = stakeKeyPair

    /** Derive a payment key at the given index (hardened). */
    def derivePaymentKey(index: Int): HdKeyPair = {
        require(index >= 0, s"Index must be non-negative, got $index")
        // m/1852'/1815'/account'/0'/index' (all hardened for SLIP-0010 Ed25519)
        accountKey.deriveHardened(Cip1852.RoleExternal).deriveHardened(index)
    }

    /** Derive a change key at the given index (hardened). */
    def deriveChangeKey(index: Int): HdKeyPair = {
        require(index >= 0, s"Index must be non-negative, got $index")
        // m/1852'/1815'/account'/1'/index' (all hardened for SLIP-0010 Ed25519)
        accountKey.deriveHardened(Cip1852.RoleInternal).deriveHardened(index)
    }

    /** Derive a staking key at the given index (hardened). */
    def deriveStakingKey(index: Int): HdKeyPair = {
        require(index >= 0, s"Index must be non-negative, got $index")
        // m/1852'/1815'/account'/2'/index' (all hardened for SLIP-0010 Ed25519)
        accountKey.deriveHardened(Cip1852.RoleStaking).deriveHardened(index)
    }

    /** Create a new HdAccount with a different payment index. */
    def withPaymentIndex(index: Int): HdAccount =
        new HdAccount(accountIndex, accountKey, index, changeIndex, stakingIndex)

    /** Create a new HdAccount with a different change index. */
    def withChangeIndex(index: Int): HdAccount =
        new HdAccount(accountIndex, accountKey, paymentIndex, index, stakingIndex)

    /** Create a new HdAccount with a different staking index. */
    def withStakingIndex(index: Int): HdAccount =
        new HdAccount(accountIndex, accountKey, paymentIndex, changeIndex, index)
}

object HdAccount {

    /** Create an HdAccount from a BIP-39 seed.
      *
      * @param seed
      *   the 64-byte BIP-39 seed
      * @param accountIndex
      *   the account index (default 0)
      * @return
      *   the HD account
      */
    def fromSeed(seed: Array[Byte], accountIndex: Int = 0)(using Ed25519Signer): HdAccount = {
        require(accountIndex >= 0, s"Account index must be non-negative, got $accountIndex")
        val accountPath = Cip1852.accountPath(accountIndex)
        val accountKey = HdKeyPair.fromSeed(seed, accountPath)
        new HdAccount(accountIndex, accountKey)
    }

    /** Create an HdAccount from a BIP-39 mnemonic.
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
        val seed = Bip39.mnemonicToSeed(mnemonic, passphrase)
        fromSeed(seed, accountIndex)
    }

    /** Create multiple HdAccounts from a single seed.
      *
      * @param seed
      *   the 64-byte BIP-39 seed
      * @param count
      *   number of accounts to create
      * @return
      *   sequence of HD accounts
      */
    def multipleFromSeed(seed: Array[Byte], count: Int)(using Ed25519Signer): Seq[HdAccount] = {
        require(count > 0, s"Count must be positive, got $count")
        (0 until count).map(i => fromSeed(seed, i))
    }
}
