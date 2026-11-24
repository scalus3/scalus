package scalus.cardano.ledger
package rules

import scalus.cardano.address.{Network, ShelleyAddress, ShelleyDelegationPart, ShelleyPaymentPart, StakeAddress, StakePayload}

/** Fixed values for the rule suites, distinct for each `n`, spec `[kit.values.fixed]`. No
  * generators, so both `ValidatorRulesTestKit` and `CertsTestKit` mix it in. A test that signs
  * takes its key from `Party` instead, spec `[kit.keys.party]`.
  */
trait RuleTestValues {

    /** A transaction input, distinct for each `n`, spec `[kit.values.input]`. */
    protected def input(n: Int): TransactionInput =
        TransactionInput(TransactionHash.fromHex(f"$n%064x"), 0)

    protected def addrKeyHash(n: Int): AddrKeyHash = AddrKeyHash.fromHex(f"$n%056x")

    protected def scriptHash(n: Int): ScriptHash = ScriptHash.fromHex(f"$n%056x")

    protected def poolKeyHash(n: Int): PoolKeyHash = PoolKeyHash.fromHex(f"$n%056x")

    protected def stakeKeyHash(n: Int): StakeKeyHash = StakeKeyHash.fromHex(f"$n%056x")

    protected def govActionId(n: Int): GovActionId =
        GovActionId(TransactionHash.fromHex(f"$n%064x"), 0)

    /** A key address with no stake part. `Network.Testnet` is the network of `Context()`. */
    protected def keyAddress(n: Int, network: Network = Network.Testnet): ShelleyAddress =
        ShelleyAddress(network, ShelleyPaymentPart.Key(addrKeyHash(n)), ShelleyDelegationPart.Null)

    /** A reward account of a stake key. */
    protected def rewardAccount(n: Int, network: Network = Network.Testnet): RewardAccount =
        RewardAccount(StakeAddress(network, StakePayload.Stake(stakeKeyHash(n))))

    /** A reward account of a script, on `Network.Testnet`. */
    protected def scriptRewardAccount(hash: ScriptHash): RewardAccount =
        RewardAccount(StakeAddress(Network.Testnet, StakePayload.Script(hash)))

    /** The address of `hash` with no stake part, on `Network.Testnet`, spec
      * `[kit.rules.script-utxo]`.
      */
    protected def scriptAddress(hash: ScriptHash): ShelleyAddress =
        ShelleyAddress(Network.Testnet, ShelleyPaymentPart.Script(hash), ShelleyDelegationPart.Null)

    /** A body with no inputs, no outputs and fee 0. A test sets the fields its rule reads with
      * `.copy(...)`, or `Transaction.withBody`.
      */
    protected val emptyBody: TransactionBody =
        TransactionBody(TaggedSortedSet.empty, IndexedSeq.empty, Coin.zero)

    protected val anchor: Anchor = Anchor("https://example.com", DataHash.fromHex("0" * 64))

    /** A proposal of `action` with a deposit of 100 ada, returned to `rewardAccount(0)` unless the
      * test gives another account.
      */
    protected def proposal(
        action: GovAction,
        account: RewardAccount = rewardAccount(0),
        deposit: Coin = Coin.ada(100)
    ): ProposalProcedure = ProposalProcedure(deposit, account, action, anchor)

    /** A mint of one token with the empty asset name under `policy`. */
    protected def mintOne(policy: ScriptHash): Mint =
        Mint(MultiAsset.asset(policy, AssetName.empty, 1))
}
