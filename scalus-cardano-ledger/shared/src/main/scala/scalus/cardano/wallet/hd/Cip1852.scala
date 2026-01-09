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
  *   - role: 0 (external), 1 (internal/change), 2 (staking)
  *   - index: 0, 1, 2, ... (non-hardened)
  *
  * @see
  *   https://cips.cardano.org/cip/CIP-1852
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

    /** Represents a key role in CIP-1852 derivation. */
    enum Role(val value: Int):
        case External extends Role(0)
        case Internal extends Role(1)
        case Staking extends Role(2)

    object Role:
        def fromInt(value: Int): Option[Role] = value match
            case 0 => Some(External)
            case 1 => Some(Internal)
            case 2 => Some(Staking)
            case _ => None

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

    /** Build a generic derivation path.
      *
      * @param account
      *   the account index
      * @param role
      *   the key role
      * @param index
      *   the address/key index
      * @return
      *   the path string
      */
    def path(account: Int, role: Role, index: Int): String = {
        require(account >= 0, s"Account index must be non-negative, got $account")
        require(index >= 0, s"Index must be non-negative, got $index")
        s"m/${Purpose}'/${CoinType}'/${account}'/${role.value}/${index}"
    }

    /** Parse a CIP-1852 path string into components.
      *
      * @param pathStr
      *   the path string (e.g., "m/1852'/1815'/0'/0/0")
      * @return
      *   Some((account, role, index)) if valid CIP-1852 path, None otherwise
      */
    def parsePath(pathStr: String): Option[(Int, Role, Int)] = {
        val trimmed = pathStr.trim
        val normalized = if trimmed.startsWith("m/") then trimmed.drop(2) else trimmed

        val parts = normalized.split("/")
        if parts.length != 5 then return None

        try {
            val purpose = parseHardenedIndex(parts(0))
            val coinType = parseHardenedIndex(parts(1))
            val account = parseHardenedIndex(parts(2))
            val roleValue = parts(3).toInt
            val index = parts(4).toInt

            if purpose != Purpose || coinType != CoinType then return None

            Role.fromInt(roleValue).map(role => (account, role, index))
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
