package scalus.cardano.wallet.hd

/** CIP-1852 derivation paths for Cardano HD wallets.
  *
  * CIP-1852 defines the hierarchical deterministic wallet structure for Cardano Shelley-era
  * wallets. It follows the BIP-44 structure with Cardano-specific constants.
  *
  * Path structure: m / purpose' / coin_type' / account' / role / index
  *   - purpose: 1852' (Cardano Shelley-era)
  *   - coin_type: 1815' (Cardano)
  *   - account: 0', 1', 2', ... (hardened)
  *   - role: 0 (external), 1 (internal/change), 2 (staking), 3 (DRep), 4 (CC cold), 5 (CC hot)
  *   - index: 0, 1, 2, ... (non-hardened)
  *
  * @see
  *   https://cips.cardano.org/cip/CIP-1852
  * @see
  *   https://cips.cardano.org/cip/CIP-0105 (Conway era key chains)
  */
object Cip1852 {

    /** Purpose constant for Cardano Shelley-era wallets. */
    val Purpose: Int = 1852

    /** Coin type constant for Cardano (registered at SLIP-44). */
    val CoinType: Int = 1815

    /** Role for external (receiving) addresses. */
    val RoleExternal: Int = 0

    /** Role for internal (change) addresses. */
    val RoleInternal: Int = 1

    /** Role for staking keys. */
    val RoleStaking: Int = 2

    /** Role for DRep (governance) keys (CIP-105). */
    val RoleDRep: Int = 3

    /** Role for Constitutional Committee cold keys (CIP-105). */
    val RoleCCCold: Int = 4

    /** Role for Constitutional Committee hot keys (CIP-105). */
    val RoleCCHot: Int = 5

    /** Build the derivation path for an account root key.
      *
      * Path: m/1852'/1815'/account'
      *
      * @param account
      *   the account index (0, 1, 2, ...)
      * @return
      *   the path string
      */
    def accountPath(account: Int): String = {
        require(account >= 0, s"Account index must be non-negative, got $account")
        s"m/${Purpose}'/${CoinType}'/${account}'"
    }

    /** Build the derivation path for a payment (external) key.
      *
      * Path: m/1852'/1815'/account'/0/index
      *
      * @param account
      *   the account index
      * @param index
      *   the address index
      * @return
      *   the path string
      */
    def paymentPath(account: Int, index: Int): String = {
        require(account >= 0, s"Account index must be non-negative, got $account")
        require(index >= 0, s"Address index must be non-negative, got $index")
        s"m/${Purpose}'/${CoinType}'/${account}'/${RoleExternal}/${index}"
    }

    /** Build the derivation path for a change (internal) key.
      *
      * Path: m/1852'/1815'/account'/1/index
      *
      * @param account
      *   the account index
      * @param index
      *   the address index
      * @return
      *   the path string
      */
    def changePath(account: Int, index: Int): String = {
        require(account >= 0, s"Account index must be non-negative, got $account")
        require(index >= 0, s"Address index must be non-negative, got $index")
        s"m/${Purpose}'/${CoinType}'/${account}'/${RoleInternal}/${index}"
    }

    /** Build the derivation path for a staking key.
      *
      * Path: m/1852'/1815'/account'/2/index
      *
      * Note: Typically index is 0 for the primary staking key.
      *
      * @param account
      *   the account index
      * @param index
      *   the staking key index (usually 0)
      * @return
      *   the path string
      */
    def stakingPath(account: Int, index: Int = 0): String = {
        require(account >= 0, s"Account index must be non-negative, got $account")
        require(index >= 0, s"Staking index must be non-negative, got $index")
        s"m/${Purpose}'/${CoinType}'/${account}'/${RoleStaking}/${index}"
    }

    /** Build the derivation path for a DRep (governance) key (CIP-105).
      *
      * Path: m/1852'/1815'/account'/3/index
      *
      * Note: CIP-105 recommends using only index 0 for DRep keys.
      *
      * @param account
      *   the account index
      * @param index
      *   the DRep key index (usually 0)
      * @return
      *   the path string
      */
    def drepPath(account: Int, index: Int = 0): String = {
        require(account >= 0, s"Account index must be non-negative, got $account")
        require(index >= 0, s"DRep index must be non-negative, got $index")
        s"m/${Purpose}'/${CoinType}'/${account}'/${RoleDRep}/${index}"
    }

    /** Build a generic derivation path.
      *
      * @param account
      *   the account index
      * @param role
      *   the key role (0=external, 1=internal, 2=staking, 3=drep, 4=cc-cold, 5=cc-hot)
      * @param index
      *   the address/key index
      * @return
      *   the path string
      */
    def path(account: Int, role: Int, index: Int): String = {
        require(account >= 0, s"Account index must be non-negative, got $account")
        require(role >= 0, s"Role must be non-negative, got $role")
        require(index >= 0, s"Index must be non-negative, got $index")
        s"m/${Purpose}'/${CoinType}'/${account}'/${role}/${index}"
    }

    /** Parse a CIP-1852 path string into components.
      *
      * @param pathStr
      *   the path string (e.g., "m/1852'/1815'/0'/0/0")
      * @return
      *   Some((account, role, index)) if valid CIP-1852 path, None otherwise
      */
    def parsePath(pathStr: String): Option[(Int, Int, Int)] = {
        val trimmed = pathStr.trim
        val normalized = if trimmed.startsWith("m/") then trimmed.drop(2) else trimmed

        val parts = normalized.split("/")
        if parts.length != 5 then return None

        try {
            val purpose = parseHardenedIndex(parts(0))
            val coinType = parseHardenedIndex(parts(1))
            val account = parseHardenedIndex(parts(2))
            val role = parts(3).toInt
            val index = parts(4).toInt

            if purpose != Purpose || coinType != CoinType then None
            else Some((account, role, index))
        } catch {
            case _: NumberFormatException => None
        }
    }

    /** Parse an account-level path.
      *
      * @param pathStr
      *   the path string (e.g., "m/1852'/1815'/0'")
      * @return
      *   Some(account) if valid account path, None otherwise
      */
    def parseAccountPath(pathStr: String): Option[Int] = {
        val trimmed = pathStr.trim
        val normalized = if trimmed.startsWith("m/") then trimmed.drop(2) else trimmed

        val parts = normalized.split("/")
        if parts.length != 3 then return None

        try {
            val purpose = parseHardenedIndex(parts(0))
            val coinType = parseHardenedIndex(parts(1))
            val account = parseHardenedIndex(parts(2))

            if purpose != Purpose || coinType != CoinType then None
            else Some(account)
        } catch {
            case _: NumberFormatException => None
        }
    }

    private def parseHardenedIndex(s: String): Int = {
        require(
          s.endsWith("'") || s.endsWith("H"),
          s"Expected hardened index (ending with ' or H), got: $s"
        )
        s.dropRight(1).toInt
    }
}
