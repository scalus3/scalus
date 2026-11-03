package scalus.cardano.ledger.epoch

import scalus.cardano.ledger.Coin

/** The two pots of ADA outside the UTxO set and the accounts, the ledger's `ChainAccountState`: the
  * treasury and the reserves, ledger-rules spec [DM-9].
  *
  * The ledger reads the treasury of the last epoch boundary in LEDGER, so a tx's
  * `currentTreasuryValue` must equal it; the Emulator keeps `UtxoEnv.treasury` equal to it, spec
  * [DM-13]. The reserves fund the rewards of phase 4.
  *
  * @param treasury
  *   the treasury, `casTreasury`
  * @param reserves
  *   the reserves, `casReserves`
  */
case class AccountState(treasury: Coin, reserves: Coin)

object AccountState {

    /** The mainnet pots at epoch 665 (Koios `/totals`): what an Emulator starts with when its
      * caller gives no treasury or reserves, spec [ES-7], [ES-7a]. No UTxO holds the ADA outside
      * the Emulator, spec [ES-7b].
      */
    val mainnet: AccountState =
        AccountState(
          treasury = Coin(1_392_896_975_262_771L),
          reserves = Coin(6_029_414_056_167_760L)
        )
}
