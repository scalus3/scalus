# Unified Script Witness API for TxBuilder

## Executive Summary

Design analysis for adding script witness support to TxBuilder's `mint` and certificate/withdrawal
methods. The goal is to create a **unified, fluent API** using factory methods that work
consistently across all operations.

## Problem Statement

The `TxBuilder` class has inconsistent APIs for script operations:

- `spend` has a flexible base method: `def spend(utxo: Utxo, witness: SpendWitness)`
- `mint` uses different overloads for reference vs attached (policyId vs script first arg)
- Certificate methods only support `PubKeyWitness`, not script witnesses

## Design Alternatives Analyzed

### Approach 1: Many Overloads

Add 4 overloads per method: reference/attached x immediate/delayed

**Pros**: Discoverable, type-safe, no witness construction needed
**Cons**: 40+ new methods, code bloat, maintenance burden

### Approach 2: Single Witness Parameter

Add ONE new overload per method that accepts the witness directly.

**Pros**: Minimal API surface, follows `spend(utxo, witness)` pattern, maximum flexibility
**Cons**: Users must construct witness themselves, less discoverable

### Approach 3: Witness DSL/Builders

Add helper methods for constructing witnesses in a separate object.

**Pros**: Fluent, discoverable, good IDE support
**Cons**: New DSL to learn, additional abstraction layer

### Approach 4: Extension Methods

Add extension methods in separate object.

**Pros**: Optional import, separates concerns
**Cons**: Less discoverable, fragmented API

## Chosen Solution: Hybrid Approach

Combine Approach 2 (single witness parameter) with Approach 3 (witness DSL):

1. Create `ScriptWitness` type alias: `NativeScriptWitness | TwoArgumentPlutusScriptWitness`
2. Add factory methods to companion objects: `attached()`, `reference()`
3. Add one witness-parameter overload per method

### Type Alias

```scala
/** Witness type for minting, certificates, withdrawals, and voting operations.
 * Supports both native scripts and Plutus scripts.
 */
type ScriptWitness = NativeScriptWitness | TwoArgumentPlutusScriptWitness
```

### Factory Methods

```scala
object TwoArgumentPlutusScriptWitness {
  // Attached script with immediate/delayed redeemer
  def attached[T: ToData](script: PlutusScript, redeemer: T, signers: Set[AddrKeyHash] = Set.empty)

  def attached(script: PlutusScript, redeemerBuilder: Transaction => Data, signers: Set[AddrKeyHash] = Set.empty)

  // Reference script with immediate/delayed redeemer
  def reference[T: ToData](redeemer: T, signers: Set[AddrKeyHash] = Set.empty)

  def reference(redeemerBuilder: Transaction => Data, signers: Set[AddrKeyHash] = Set.empty)
}

object NativeScriptWitness {
  def attached(script: Script.Native, signers: Set[AddrKeyHash] = Set.empty)

  def reference(signers: Set[AddrKeyHash] = Set.empty)
}
```

## API Usage Examples

### Minting

**Before (inconsistent):**

```scala
// Reference script - policyId first
builder.mint(policyId, assets, redeemer)

// Attached script - script first (confusing!)
builder.mint(script, assets, redeemer)
```

**After (unified):**

```scala
import TwoArgumentPlutusScriptWitness.*

// Both use policyId first, witness determines script source
builder.mint(policyId, assets, attached(script, redeemer)) // attached
builder.mint(policyId, assets, reference(redeemer)) // reference
```

### Certificates/Withdrawals

**Before (PubKey only):**

```scala
builder
  .withdrawRewards(stakeAddress, Coin(1000))
  .delegateTo(stakeAddress, poolId)
```

**After (Script support):**

```scala
import TwoArgumentPlutusScriptWitness.*

// With attached script
builder
  .withdrawRewards(scriptAddr, Coin(1000), attached(script, redeemer))
  .delegateTo(scriptAddr, poolId, attached(script, redeemer))

// With reference script
builder
  .references(scriptRefUtxo)
  .withdrawRewards(scriptAddr, Coin(1000), reference(redeemer))
  .delegateTo(scriptAddr, poolId, reference(redeemer))

// With delayed redeemer
builder
  .withdrawRewards(scriptAddr, Coin(1000), attached(script, tx => computeRedeemer(tx)))
```

## Methods Added

### Minting (1 method)

- `mint(policyId, assets, witness: ScriptWitness)` - unified minting API

### Staking/Certificates (5 methods)

- `registerStake(stakeAddress, witness)`
- `deregisterStake(stakeAddress, refund, witness)`
- `withdrawRewards(stakeAddress, amount, witness)`
- `delegateTo(stakeAddress, poolId, witness)`
- `stakeAndDelegate(stakeAddress, poolId, witness)`

### Governance (7 methods)

- `delegateVoteToDRep(stakeAddress, drep, witness)`
- `registerAndDelegateVoteToDRep(stakeAddress, drep, witness)`
- `delegateToPoolAndDRep(stakeAddress, poolId, drep, witness)`
- `registerAndDelegateToPoolAndDRep(stakeAddress, poolId, drep, witness)`
- `registerDRep(drepCredential, anchor, witness)`
- `unregisterDRep(drepCredential, refund, witness)`
- `updateDRep(drepCredential, anchor, witness)`

## Breaking Changes

**None** - all existing method signatures remain unchanged. New methods are overloads.

## Benefits

1. **Single consistent API** across mint, certificates, and withdrawals
2. **Supports both Native and Plutus scripts** via `ScriptWitness` union type
3. **Consistent with `spend(utxo, witness)` pattern**
4. **Fluent factory methods**: `attached(script, redeemer)`, `reference(redeemer)`
5. **Immediate and delayed redeemers** via overloading (same method name)
6. **Existing overloads preserved** for backwards compatibility
