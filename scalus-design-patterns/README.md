# Table of Contents

<!-- vim-markdown-toc GFM -->

* [Scalus Library for Common Design Patterns in Cardano Smart Contracts](#scalus-library-for-common-design-patterns-in-cardano-smart-contracts)
    * [Optimization Patterns](#optimization-patterns)
        * [Stake Validator](#stake-validator)
        * [Transaction Level Validator Minting Policy](#transaction-level-validator-minting-policy)
        * [Merkelized Validator](#merkelized-validator)
    * [Validation Patterns](#validation-patterns)
        * [Parameter Validation](#parameter-validation)
    * [Indexing Patterns](#indexing-patterns)
        * [UTxO Indexers](#utxo-indexers)
    * [Advanced Data structures](#advanced-data-structures)
        * [Linked List](#linked-list)
    * [Utilities](#utilities)
        * [Validity Range Normalization](#validity-range-normalization)
    * [License](#license)

<!-- vim-markdown-toc -->

# Scalus Library for Common Design Patterns in Cardano Smart Contracts

To help facilitate faster development of Cardano smart contracts, we present a
collection of tried and tested modules and functions for implementing common
design patterns.

Based on our [`design-patterns`](https://github.com/Anastasia-Labs/design-patterns) repository.

## Optimization Patterns

### Stake Validator

This pattern delegates computation to a staking script using the "withdraw zero trick."
Instead of running expensive logic per-UTxO (O(N²)), the stake validator runs once (O(N)).

#### How It Works

1. **Spending validator** (runs per UTxO): Minimal logic - just checks stake validator ran
2. **Stake validator** (runs once): Heavy computation in the reward endpoint

#### API

| Function | Use Case |
|----------|----------|
| `spend` | Check stake validator ran + validate its redeemer and withdrawal amount |
| `spendMinimal` | Just check stake validator ran (most common) |
| `withdraw` | Helper for reward endpoint - extracts script hash from credential |

#### Example

```scala
import scalus.patterns.StakeValidator

// Spending endpoint - minimal, runs per UTxO
inline override def spend(datum: Option[Data], redeemer: Redeemer, tx: TxInfo, ownRef: TxOutRef): Unit = {
    val ownScriptHash = tx.findOwnInputOrFail(ownRef).resolved.address.credential
        .scriptOption.getOrFail("Own address must be Script")

    // Option 1: Just check stake validator ran (withdraw zero trick)
    StakeValidator.spendMinimal(ownScriptHash, tx)

    // Option 2: Also validate redeemer and withdrawal amount
    StakeValidator.spend(
      withdrawalScriptHash = ownScriptHash,
      withdrawalRedeemerValidator = (redeemer, lovelace) => lovelace === BigInt(0),
      txInfo = tx
    )
}

// Reward endpoint - heavy logic, runs once
inline override def reward(redeemer: Redeemer, stakingKey: Credential, tx: TxInfo): Unit = {
    StakeValidator.withdraw(
      withdrawalValidator = (redeemer, validatorHash, txInfo) => {
          // Your heavy validation logic here
          true
      },
      redeemer = redeemer,
      credential = stakingKey,
      txInfo = tx
    )
}
```

See `scalus.examples.StakeValidatorPaymentSplitterExample` for a complete example.

#### Real-World Cost Comparison

The `scalus-examples` module includes a side-by-side comparison:
- `NaivePaymentSplitterValidator` - Runs full validation per UTxO (O(N²))
- `OptimizedPaymentSplitterValidator` - Uses stake validator pattern (O(N))

Tests show **~71% reduction** in memory and CPU costs when spending multiple UTxOs.
See `PaymentSplitterTxBuilderTest` for the cost comparison test.

### Transaction Level Validator Minting Policy

Similar to the [Stake Validator](#stake-validator), this pattern delegates heavy computation
to a single execution point. Instead of using a stake validator, it couples spending
and minting endpoints of the same validator.

#### How It Works

1. **Spending validator** (runs per UTxO): Minimal - just checks minting endpoint executes
2. **Minting validator** (runs once): Heavy computation when minting/burning tokens

#### API

| Function | Use Case |
|----------|----------|
| `spend` | Check minting policy ran + validate its redeemer and minted tokens |
| `spendMinimal` | Just check at least one token is minted/burnt with the policy |

#### Example

```scala
import scalus.patterns.TransactionLevelMinterValidator

// Spending endpoint - minimal, runs per UTxO
inline override def spend(datum: Option[Data], redeemer: Redeemer, tx: TxInfo, ownRef: TxOutRef): Unit = {
    val ownScriptHash = tx.findOwnInputOrFail(ownRef).resolved.address.credential
        .scriptOption.getOrFail("Own address must be Script")

    // Option 1: Just check minting policy ran
    TransactionLevelMinterValidator.spendMinimal(ownScriptHash, tx)

    // Option 2: Also validate redeemer and minted tokens
    TransactionLevelMinterValidator.spend(
      minterScriptHash = ownScriptHash,
      minterRedeemerValidator = _.to[MintRedeemer].isValid,
      minterTokensValidator = tokens => {
          val (tokenName, qty) = tokens.toList.head
          tokenName === utf8"BEACON" && (qty === BigInt(1) || qty === BigInt(-1))
      },
      txInfo = tx
    )
}

// Minting endpoint - heavy logic, runs once
inline override def mint(redeemer: Redeemer, policyId: PolicyId, tx: TxInfo): Unit = {
    // Your heavy validation logic here - e.g., count script inputs
    val scriptInputsCount = tx.inputs.foldRight(BigInt(0)) { (input, acc) =>
        if input.resolved.address.credential === Credential.ScriptCredential(policyId)
        then acc + 1 else acc
    }
    require(scriptInputsCount === redeemer.to[MintRedeemer].expectedCount)
}
```

See `scalus.examples.TransactionLevelMinterValidatorExample` for a complete example.

### Merkelized Validator

This pattern allows spending validators to **read verified data** from a stake
validator's redeemer. It builds on the [Stake Validator](#stake-validator) pattern
but adds the ability for spending validators to access computation results that
were verified once by the stake validator.

#### When to Use Which Pattern

| Pattern | Use Case | Example |
|---------|----------|---------|
| **StakeValidator.spendMinimal** | Spending validator only needs to check stake validator ran | Payment splitter |
| **MerkelizedValidator.verifyAndGetRedeemer** | Spending validator needs to **read** verified data | Batch auction |

#### How It Works

1. Off-chain code computes expensive values (e.g., clearing price)
2. Values are included in the stake validator's redeemer
3. Stake validator verifies the values are correct (runs **once**)
4. Spending validator reads the verified values via `MerkelizedValidator` (runs per UTxO)

#### API

```scala
import scalus.patterns.MerkelizedValidator

// In your spending validator:
val stakeRedeemer = MerkelizedValidator.verifyAndGetRedeemer(ownScriptHash, txInfo)
val verifiedData = stakeRedeemer.to[YourRedeemerType]
// Now use verifiedData - it's been verified by the stake validator
```

**Functions:**

- `getStakeRedeemer(hash, txInfo)` - Retrieves the stake validator's redeemer
- `verifyAndGetRedeemer(hash, txInfo)` - Verifies withdrawal exists AND returns redeemer

#### Example: Batch Auction

See `scalus.examples.BatchAuctionValidator` for a complete example where:
- **Stake validator**: Verifies the clearing price calculation once
- **Spending validator**: Reads the verified clearing price to determine if each bid is filled or refunded

```scala
// Stake validator redeemer with verified data
case class AuctionSettlementRedeemer(
    clearingPrice: BigInt,
    totalUnitsAvailable: BigInt
) derives ToData, FromData

// In spending endpoint - read verified clearing price
val stakeRedeemer = MerkelizedValidator.verifyAndGetRedeemer(ownScriptHash, tx)
val settlement = stakeRedeemer.to[AuctionSettlementRedeemer]

// Use the verified clearing price
if bid.bidPrice >= settlement.clearingPrice then
    // Fill the bid
else
    // Refund the bid
```

#### Benefits

When spending N UTxOs with iteration-heavy logic:
- **Without pattern**: O(N²) - each spending validator iterates all inputs/outputs
- **With pattern**: O(N) - stake validator iterates once, spending validators just read

Run the `BatchAuctionTest` budget comparison test to see actual memory/CPU savings.

> [!NOTE]
> Total size of reference scripts is limited to 200KiB (204800 bytes), with
> exponential fee implications. See [cardano-ledger#3952](https://github.com/IntersectMBO/cardano-ledger/issues/3952).

## Validation Patterns

### Parameter Validation

This pattern enables verification that script instances are legitimate instantiations
of parameterized scripts with specific parameter values.

#### The Problem

When a minting policy needs to ensure tokens go only to a spending script parameterized
with a specific value (e.g., a royalty address), it cannot directly inspect the target
script's parameters. Each different parameter creates a different script hash, making
it impossible to distinguish between legitimate and arbitrary script destinations
without this pattern.

#### Use Cases

- **Minting Policy → Spending Script**: A minting policy verifies tokens only go to
  a spending script parameterized with the correct royalty address
- **Multi-Script Coordination**: A coordinator script verifies helper scripts have
  expected parameters
- **Factory Pattern**: A factory mints tokens referencing scripts with verified parameters

#### How It Works

Script hashing in Cardano:

```
script_hash = blake2b_224(language_tag ++ cbor_encoded_program)

Language tags:
  - PlutusV1: 0x01
  - PlutusV2: 0x02
  - PlutusV3: 0x03

When parameters are applied:
  parameterized_program = base_program $ param1 $ param2 ...
  parameterized_hash = blake2b_224(language_tag ++ cbor(parameterized_program))
```

The pattern:
1. **Off-chain**: Compile base script, apply parameters, compute expected hash
2. **Off-chain**: Pass expected hash to the dependent script (e.g., minting policy)
3. **On-chain**: Dependent script verifies outputs go to addresses matching expected hash

#### API

**Off-chain functions** (`ParameterValidation`):

| Function | Description |
|----------|-------------|
| `computeScriptHashV3` | Compute hash for PlutusV3 script with applied parameters |
| `computeScriptHashV2` | Compute hash for PlutusV2 script with applied parameters |
| `computeScriptHashV1` | Compute hash for PlutusV1 script with applied parameters |

**On-chain functions** (`ParameterValidationOnChain`):

| Function | Description |
|----------|-------------|
| `verifyScriptCredential` | Verify credential matches expected script hash |
| `verifyAddressScript` | Verify address has expected script credential |
| `findOutputsToScript` | Find outputs sent to a specific script hash |
| `isExpectedScript` | Check if credential matches expected hash (returns Boolean) |

#### Example: NFT with Verified Marketplace

A minting policy ensures NFTs can only be sent to a marketplace parameterized with
the correct creator royalty address.

**Off-chain setup:**

```scala
import scalus.patterns.ParameterValidation
import scalus.builtin.Data.toData

// 1. Compute marketplace hash for specific creator
def computeMarketplaceHash(creatorPkh: PubKeyHash): ValidatorHash = {
    ParameterValidation.computeScriptHashV3(
      MarketplaceBaseProgram.program.deBruijnedProgram,
      creatorPkh.toData
    )
}

// 2. Create parameterized scripts
val marketplaceHash = computeMarketplaceHash(creatorPkh)
val marketplace = MarketplaceBaseProgram.program.deBruijnedProgram $ creatorPkh.toData
val mintingPolicy = NFTMintingBaseProgram.program.deBruijnedProgram $
    NFTMintParams(marketplaceHash, tokenName).toData
```

**On-chain verification (minting policy):**

```scala
import scalus.patterns.ParameterValidationOnChain

@Compile
object NFTMintingPolicy {
    inline def validate(params: NFTMintParams)(scData: Data): Unit = {
        val sc = scData.to[ScriptContext]
        sc.scriptInfo match
            case ScriptInfo.MintingScript(policyId) =>
                val mintedAmount = sc.txInfo.mint.quantityOf(policyId, params.tokenName)
                require(mintedAmount === BigInt(1), "Must mint exactly 1 NFT")

                // Find output containing our NFT
                val nftOutput = sc.txInfo.outputs.find { output =>
                    output.value.quantityOf(policyId, params.tokenName) > 0
                }.getOrFail("NFT output not found")

                // Verify output goes to the expected marketplace script
                ParameterValidationOnChain.verifyAddressScript(
                  nftOutput.address,
                  params.expectedMarketplaceHash
                )
            case _ => fail("Unsupported script purpose")
    }
}
```

See `scalus.examples.ParameterValidationExample` for the complete implementation.

## Indexing Patterns

### UTxO Indexers

This pattern provides an optimized way to map input UTxOs to output UTxOs using
indices computed off-chain. Instead of searching through all inputs/outputs on-chain
(O(n) per lookup), validators receive pre-computed indices and just verify correctness (O(1)).

#### Available Functions

| Function | Use Case |
|----------|----------|
| `validateInput` | Validate a single input at a known index |
| `oneToOne` | Map one input to one output |
| `oneToMany` | Map one input to multiple outputs |
| `multiOneToOneNoRedeemer` | Map multiple script inputs to outputs (same redeemer) |
| `multiOneToOneWithRedeemer` | Map multiple script inputs with different redeemers |

#### Basic Usage: One-to-One

```scala
import scalus.patterns.UtxoIndexer

case class IndexerRedeemer(inputIdx: BigInt, outputIdx: BigInt) derives FromData, ToData

@Compile
object MyValidator extends Validator:
    inline override def spend(
        datum: Option[Data],
        redeemer: Data,
        tx: TxInfo,
        ownRef: TxOutRef
    ): Unit = {
        val IndexerRedeemer(inputIdx, outputIdx) = redeemer.to[IndexerRedeemer]

        UtxoIndexer.oneToOne(
          ownRef,
          inputIdx,
          outputIdx,
          tx,
          validator = (input, output) => {
              // Your validation logic: check values, datums, addresses, etc.
              input.resolved.value.getLovelace === output.value.getLovelace
          }
        )
    }
```

#### One-to-Many Example

```scala
UtxoIndexer.oneToMany(
  ownRef,
  inputIdx,
  outputIndices = List(0, 2, 4),  // Non-contiguous indices supported
  tx,
  perOutputValidator = (input, idx, output) => {
      // Validate each output individually
      output.value.getLovelace >= minAmount
  },
  collectiveValidator = (input, outputs) => {
      // Validate all outputs together
      outputs.foldLeft(BigInt(0))(_ + _.value.getLovelace) === input.resolved.value.getLovelace
  }
)
```

#### Multiple One-to-One Pairs

For processing multiple UTxOs from the same script in a single transaction:

**Without individual redeemers** (`multiOneToOneNoRedeemer`):
Use when all script inputs share the same validation logic.

```scala
// Process multiple script UTxOs with the same redeemer
UtxoIndexer.multiOneToOneNoRedeemer(
  indexPairs = List((0, 0), (2, 1), (3, 2)),  // (inputIdx, outputIdx) pairs
  scriptHash = ownScriptHash,
  tx = txInfo,
  validator = (inIdx, input, outIdx, output) => {
      // Validate each input-output pair
      input.resolved.value.getLovelace === output.value.getLovelace
  }
)
```

**With individual redeemers** (`multiOneToOneWithRedeemer`):
Use with the withdraw-zero trick when each input needs different redeemer data.
Requires a staking script as coupling mechanism.

```scala
// Each spend embeds stake credential; coercer extracts redeemer and credential
UtxoIndexer.multiOneToOneWithRedeemer[MyRedeemer](
  indexPairs = List((0, 0), (1, 1)),
  spendingScriptHash = spendScriptHash,
  stakeScriptHash = stakeScriptHash,
  tx = txInfo,
  redeemerCoercerAndStakeExtractor = (data: Data) => {
      val r = data.to[MySpendRedeemer]
      (r.payload, r.stakeCredential)
  },
  validator = (inIdx, input, redeemer, outIdx, output) => {
      // Validate with per-input redeemer data
      true
  }
)
```

#### Off-Chain Index Computation

Use `TxBuilder` with a redeemer builder function to compute indices after the
transaction is assembled:

```scala
import scalus.cardano.txbuilder.TxBuilder

TxBuilder(env)
    .spend(
      scriptUtxo,
      redeemerBuilder = (tx: Transaction) => {
          val inputIdx = tx.body.value.inputs.toSeq.indexOf(scriptUtxo.input)
          val outputIdx = tx.body.value.outputs.indexWhere(_.address == recipientAddress)
          IndexerRedeemer(BigInt(inputIdx), BigInt(outputIdx)).toData
      },
      script
    )
    .payTo(recipientAddress, value)
```

> [!NOTE]
> The singular UTxO indexer patterns (`oneToOne`, `oneToMany`) do not provide
> protection against the [double satisfaction](https://github.com/Plutonomicon/plutonomicon/blob/b6906173c3f98fb5d7b40fd206f9d6fe14d0b03b/vulnerabilities.md#double-satisfaction)
> vulnerability. Implement your own protection based on your contract's needs.

## Advanced Data structures

### Linked List

An on-chain linked list implementation using NFTs and datums. Each node is a UTxO
containing a unique NFT token and a datum with the node's key, reference to next node,
and user data.

```
  ╭──────╮  ╭───────╮  ╭────────╮  ╭────────╮  ╭───────╮
  │•Head•├─>│ Apple ├─>│ Banana ├─>│ Orange ├─>│ Peach │
  ╰──────╯  ╰───────╯  ╰────────╯  ╰────────╯  ╰───────╯
```

#### Two Variants

| Variant | Module | Description |
|---------|--------|-------------|
| **OrderedLinkedList** | `scalus.patterns.OrderedLinkedList` | Keys must be sorted (key < ref). Supports `insert` at any position. |
| **UnorderedLinkedList** | `scalus.patterns.UnorderedLinkedList` | Keys can be in any order. Only `prepend`/`append` for insertion. |

**When to use which:**
- **OrderedLinkedList**: When you need sorted data or efficient lookup by key range
- **UnorderedLinkedList**: When order doesn't matter and you only add to head/tail

#### Data Structures

```scala
import scalus.patterns.{Cons, Node, Common, Config}

// Node datum - stored in each UTxO
case class Cons(
    key: Option[TokenName],  // None = head node, Some(key) = regular node
    ref: Option[TokenName],  // Reference to next node (None = end of list)
    data: Data               // User data stored in this node
)

// Node representation for validation
case class Node(value: Value, cell: Cons)

// Shared transaction context
case class Common(policy: PolicyId, mint: Value, inputs: List[Node], outputs: List[Node])

// Configuration for the list
case class Config(init: TxOutRef, deadline: PosixTime, penalty: Address)
```

#### Operations

| Operation | Ordered | Unordered | Description |
|-----------|:-------:|:---------:|-------------|
| `init` | ✓ | ✓ | Create empty list (mint head NFT) |
| `deinit` | ✓ | ✓ | Destroy empty list (burn head NFT) |
| `insert` | ✓ | ✗ | Insert node at sorted position |
| `prepend` | ✓ | ✓ | Insert at beginning (after head) |
| `append` | ✓ | ✓ | Insert at end |
| `remove` | ✓ | ✓ | Remove node (burn node NFT) |

#### Example: Insert Operation (Ordered)

```
           ╭────────╮  ╭────────╮
           │ Banana ├─>│ Orange │   INPUTS
           ╰───┬────╯  ╰────┬───╯
               │            │
           ┏━━━V━━━━━━━━━━━━V━━━━━━━━━━━━━━┓
           ┃   Insert "Kiwi" Transaction   ┃
           ┗━━━┯━━━━━━━━━━┯━━━━━━━━━━┯━━━━━┛
               │          │          │
           ╭───V────╮  ╭──V───╮  ╭───V────╮
           │ Banana ├─>│ Kiwi ├─>│ Orange │  OUTPUTS  (Banana < Kiwi < Orange)
           ╰────────╯  ╰──────╯  ╰────────╯
```

#### Usage

```scala
// Ordered variant - keys must maintain sorted order
import scalus.patterns.OrderedLinkedList as LinkedList

val (common, inputs, outputs, signatories, validRange) = LinkedList.mkCommon(ownPolicy, tx)

redeemer match
    case Init                      => LinkedList.init(common)
    case Deinit                    => LinkedList.deinit(common)
    case Insert(key, covering)     => LinkedList.insert(common, key, covering)
    case Prepend(key, covering)    => LinkedList.prepend(common, key, covering)
    case Append(key, covering)     => LinkedList.append(common, key, covering)
    case Remove(key, covering)     => LinkedList.remove(common, key, covering)
```

```scala
// Unordered variant - keys can be in any order
import scalus.patterns.UnorderedLinkedList as LinkedList

val (common, inputs, outputs, signatories, validRange) = LinkedList.mkCommon(ownPolicy, tx)

redeemer match
    case Init                      => LinkedList.init(common)
    case Deinit                    => LinkedList.deinit(common)
    case Prepend(key, covering)    => LinkedList.prepend(common, key, covering)
    case Append(key, covering)     => LinkedList.append(common, key, covering)
    case Remove(key, covering)     => LinkedList.remove(common, key, covering)
```

See `scalus.examples.OrderedLinkedList` and `scalus.examples.UnorderedLinkedList` for
complete validator implementations.

> [!NOTE]
> Keys must be unique within a list. For `OrderedLinkedList`, the invariant
> `node.key < node.ref` must hold for all nodes.

## Utilities

### Validity Range Normalization

The `Interval` type in Cardano allows values that are either meaningless or have
multiple representations. For example, since values are integers, the inclusive
flag is redundant - an exclusive bound can always be converted to an inclusive one.

This pattern provides a `NormalizedInterval` type that eliminates meaningless
intervals and redundant representations.

#### The Type

```scala
import scalus.patterns.NormalizedInterval

enum NormalizedInterval:
    case ClosedRange(lower: PosixTime, upper: PosixTime)  // [lower, upper]
    case FromNegInf(upper: PosixTime)                     // (-∞, upper]
    case ToPosInf(lower: PosixTime)                       // [lower, +∞)
    case Always                                           // (-∞, +∞)
```

All bounds are **inclusive** after normalization. Improper intervals (e.g., `Interval.never`)
return `None` from `tryNormalize` or throw an error from `normalize`.

#### API

```scala
import scalus.patterns.NormalizedInterval
import scalus.ledger.api.v1.*

// Extension methods on Interval
val interval: Interval = txInfo.validRange

// Safe normalization - returns Option
interval.tryNormalize match
    case Option.Some(NormalizedInterval.ClosedRange(start, end)) =>
        // Valid time window
    case Option.Some(NormalizedInterval.Always) =>
        // No time constraints
    case Option.None =>
        // Improper interval (e.g., Interval.never)

// Unsafe normalization - fails on improper intervals
val normalized: NormalizedInterval = interval.normalize
```

#### Examples

```scala
// Exclusive bounds are converted to inclusive
val interval = Interval(
  from = IntervalBound(IntervalBoundType.Finite(10), false),  // exclusive
  to = IntervalBound(IntervalBoundType.Finite(20), false)     // exclusive
)
interval.normalize  // ClosedRange(11, 19)

// Infinite bounds
val openEnded = Interval(
  from = IntervalBound(IntervalBoundType.Finite(100), true),
  to = IntervalBound(IntervalBoundType.PosInf, false)
)
openEnded.normalize  // ToPosInf(100)

// Improper interval returns None
val never = Interval(
  from = IntervalBound(IntervalBoundType.Finite(200), true),
  to = IntervalBound(IntervalBoundType.Finite(100), true)
)
never.tryNormalize  // None
```

#### Type Class Instances

`NormalizedInterval` provides `Eq`, `Ord`, and `Show` instances for use in on-chain code:

```scala
val range1 = NormalizedInterval.ClosedRange(100, 200)
val range2 = NormalizedInterval.ClosedRange(100, 300)

range1 === range2      // false
range1 < range2        // true (compares lower, then upper)
range1.show            // "NormalizedInterval.ClosedRange(100, 200)"
```


## License

[MIT license](./LICENSE):

THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
SOFTWARE.
