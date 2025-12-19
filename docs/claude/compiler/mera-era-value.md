# MaryEraValue (CIP-0153) Implementation Plan

## Overview

CIP-0153 introduces `MaryEraValue` as a builtin type in Plutus Core, representing Cardano's multi-asset value structure natively. This replaces the current `Data`-encoded representation with a more efficient builtin type.

**CIP Reference:** https://cips.cardano.org/cip/CIP-0153

## Haskell Plutus Reference Implementation

Localy available repository: `/Users/rssh/packages/IntersectMBO/plutus/`

The Haskell Plutus implementation is available at:
- **Type definition:** `/Users/rssh/packages/IntersectMBO/plutus/plutus-core/plutus-core/src/PlutusCore/Value.hs`
- **Universe registration:** `/Users/rssh/packages/IntersectMBO/plutus/plutus-core/plutus-core/src/PlutusCore/Default/Universe.hs:126`
- **Builtin functions:** `/Users/rssh/packages/IntersectMBO/plutus/plutus-core/plutus-core/src/PlutusCore/Default/Builtins.hs:199-205`

### Plutus Value Type Structure

```haskell
-- Key type (currency symbol or token name, max 32 bytes)
newtype K = UnsafeK { unK :: ByteString }

-- Quantity (signed 128-bit integer)
newtype Quantity = UnsafeQuantity { unQuantity :: Integer }

-- Value: nested map with cached metadata for efficient costing
data Value = Value
    !NestedMap           -- Map K (Map K Quantity)
    !(IntMap Int)        -- Size distribution for costing
    {-# UNPACK #-} !Int  -- Total size
    {-# UNPACK #-} !Int  -- Count of negative amounts

type NestedMap = Map K (Map K Quantity)
```

### Plutus Builtin Functions

| Function | Signature | Description |
|----------|-----------|-------------|
| `InsertCoin` | `ByteString -> ByteString -> Integer -> Value -> Value` | Insert/update token amount |
| `LookupCoin` | `ByteString -> ByteString -> Value -> Integer` | Lookup token amount |
| `UnionValue` | `Value -> Value -> Value` | Merge two values (with overflow check) |
| `ValueContains` | `Value -> Value -> Bool` | Check if first value contains second |
| `ValueData` | `Value -> Data` | Convert Value to Data encoding |
| `UnValueData` | `Data -> Value` | Convert Data to Value |
| `ScaleValue` | `Integer -> Value -> Value` | Multiply all amounts by constant |

### Plutus Encoding

- Universe tag: `13` for `DefaultUniValue`
- Builtin function codes: `94` (InsertCoin), `96` (UnionValue), `100` (ScaleValue), etc.

---

## Scalus Implementation Plan

### Phase 1: Core Type Definitions

#### 1.1 Add BuiltinValue Type in `scalus.builtin`

**File:** `scalus-core/shared/src/main/scala/scalus/builtin/BuiltinValue.scala` (new)

> **Note:** We use `BuiltinValue` instead of `Value` to avoid naming conflict with `scalus.ledger.api.v1.Value` from the Plutus API.

> **Note:** `BuiltinValue` is a **primitive opaque type** (like BLS12_381 elements). In UPLC you cannot construct it from a SortedMap or extract the inner map - you can only use builtin operations (`insertCoin`, `lookupCoin`, `unionValue`, `valueData`, `unValueData`, etc.).

```scala
package scalus.builtin

import scala.collection.immutable.SortedMap

/** MaryEraValue (CIP-0153) - multi-asset value representation
  *
  * This is a primitive opaque type in UPLC. The inner representation
  * is hidden - use builtin operations to work with values.
  *
  * Invariants (maintained by operations):
  * - No empty inner maps
  * - No zero quantities
  * - Keys max 32 bytes, quantities in signed 128-bit range
  */
final class BuiltinValue private[scalus] (
    private[scalus] val inner: SortedMap[ByteString, SortedMap[ByteString, BigInt]]
)
```

#### 1.1.1 Add BuiltinValueOps Helper to CEK Machine

**File:** `scalus-core/shared/src/main/scala/scalus/uplc/eval/BuiltinValueOps.scala` (new)

All validation, construction, and costing helpers for CEK machine:

```scala
package scalus.uplc.eval

import scalus.builtin.{ByteString, BuiltinValue}
import scala.collection.immutable.SortedMap

/** Internal helpers for BuiltinValue operations in CEK machine */
object BuiltinValueOps:
    // Validation constants
    val maxKeyLen: Int = 32
    val quantityMin: BigInt = -(BigInt(2).pow(127))
    val quantityMax: BigInt = BigInt(2).pow(127) - 1

    def isValidKey(bs: ByteString): Boolean = bs.length <= maxKeyLen
    def isValidQuantity(i: BigInt): Boolean = i >= quantityMin && i <= quantityMax

    val empty: BuiltinValue = new BuiltinValue(SortedMap.empty)

    def unsafeFromMap(m: SortedMap[ByteString, SortedMap[ByteString, BigInt]]): BuiltinValue =
        new BuiltinValue(m)

    // Costing helpers
    def totalSize(v: BuiltinValue): Int = v.inner.values.map(_.size).sum
    def maxInnerSize(v: BuiltinValue): Int = if v.inner.isEmpty then 0 else v.inner.values.map(_.size).max
    def negativeCount(v: BuiltinValue): Int = v.inner.values.flatMap(_.values).count(_ < 0)

    // Builtin operation implementations
    def insertCoin(currency: ByteString, token: ByteString, amount: BigInt, value: BuiltinValue): BuiltinValue = ???
    def lookupCoin(currency: ByteString, token: ByteString, value: BuiltinValue): BigInt = ???
    def unionValue(v1: BuiltinValue, v2: BuiltinValue): BuiltinValue = ???
    def valueContains(v1: BuiltinValue, v2: BuiltinValue): Boolean = ???
    def scaleValue(scalar: BigInt, value: BuiltinValue): BuiltinValue = ???
    // valueData and unValueData convert between BuiltinValue and Data
```

**File:** `scalus-core/shared/src/main/scala/scalus/uplc/eval/MemoryUsage.scala`

Add memory usage for BuiltinValue:

```scala
def memoryUsageBuiltinValue(v: builtin.BuiltinValue): CostingInteger =
    CostingInteger(BuiltinValueOps.totalSize(v).toLong)
```

#### 1.2 Update DefaultUni

**File:** `scalus-core/shared/src/main/scala/scalus/uplc/DefaultUni.scala`

Add after `ProtoArray`:
```scala
case object BuiltinValue extends DefaultUni
```

Update `encodeUni`:
```scala
case DefaultUni.BuiltinValue => Seq(13)
```

Update `decodeUni`:
```scala
case 13 => DefaultUni.BuiltinValue
```

#### 1.3 Update Constant

**File:** `scalus-core/shared/src/main/scala/scalus/uplc/Constant.scala`

Add:
```scala
case class BuiltinValue(value: builtin.BuiltinValue) extends Constant
```

Update `fromValue` and pattern matching.

### Phase 2: Builtin Functions

#### 2.1 Update DefaultFun

**File:** `scalus-core/shared/src/main/scala/scalus/uplc/DefaultFun.scala`

Add enum cases:
```scala
// MaryEraValue operations (CIP-0153)
case InsertCoin      // 94
case LookupCoin      // 95
case UnionValue      // 96
case ValueContains   // 97
case ValueData       // 98
case UnValueData     // 99
case ScaleValue      // 100
```

Update encode/decode mappings with appropriate codes.

#### 2.2 Implement Builtin Runtime

**File:** `scalus-core/shared/src/main/scala/scalus/uplc/Builtin.scala`

Add to `CardanoBuiltins`:
```scala
val InsertCoin: BuiltinRuntime = mkMeaning(
    DefaultUni.ByteString ->: DefaultUni.ByteString ->: DefaultUni.Integer ->:
    DefaultUni.BuiltinValue ->: DefaultUni.BuiltinValue,
    (_: Logger, args: Seq[CekValue]) =>
        val currency = extractByteString(args(0))
        val token = extractByteString(args(1))
        val amount = extractInteger(args(2))
        val value = extractBuiltinValue(args(3))
        VCon(Constant.BuiltinValue(builtin.BuiltinValue.insertCoin(currency, token, amount, value)))
    ,
    builtinCostModel.insertCoin
)

// Similar implementations for LookupCoin, UnionValue, ValueContains,
// ValueData, UnValueData, ScaleValue
```

### Phase 3: SIR Integration

#### 3.1 Add SIRType

**File:** `scalus-core/shared/src/main/scala/scalus/compiler/sir/SIRType.scala`

Add as primitive type (like BLS12_381 elements):
```scala
case object BuiltinValue extends Primitive:
    override def uplcType: DefaultUni = DefaultUni.BuiltinValue
    override def show: String = "BuiltinValue"
```

Also update `PrimitiveRepresentation.isCompatibleWithType` to include `BuiltinValue`.

#### 3.2 Add SIR Builtins

**File:** `scalus-core/shared/src/main/scala/scalus/compiler/sir/SIRBuiltins.scala`

Add builtin definitions:
```scala
val insertCoin: SIR.Builtin = SIR.Builtin(
    DefaultFun.InsertCoin,
    SIRType.ByteString ->: SIRType.ByteString ->: SIRType.Integer ->:
    SIRType.BuiltinValue ->: SIRType.BuiltinValue,
    AnnotationsDecl.empty
)

val lookupCoin: SIR.Builtin = SIR.Builtin(
    DefaultFun.LookupCoin,
    SIRType.ByteString ->: SIRType.ByteString ->: SIRType.BuiltinValue ->: SIRType.Integer,
    AnnotationsDecl.empty
)

val unionValue: SIR.Builtin = SIR.Builtin(
    DefaultFun.UnionValue,
    SIRType.BuiltinValue ->: SIRType.BuiltinValue ->: SIRType.BuiltinValue,
    AnnotationsDecl.empty
)

val valueContains: SIR.Builtin = SIR.Builtin(
    DefaultFun.ValueContains,
    SIRType.BuiltinValue ->: SIRType.BuiltinValue ->: SIRType.Bool,
    AnnotationsDecl.empty
)

val valueData: SIR.Builtin = SIR.Builtin(
    DefaultFun.ValueData,
    SIRType.BuiltinValue ->: SIRType.Data,
    AnnotationsDecl.empty
)

val unValueData: SIR.Builtin = SIR.Builtin(
    DefaultFun.UnValueData,
    SIRType.Data ->: SIRType.BuiltinValue,
    AnnotationsDecl.empty
)

val scaleValue: SIR.Builtin = SIR.Builtin(
    DefaultFun.ScaleValue,
    SIRType.Integer ->: SIRType.BuiltinValue ->: SIRType.BuiltinValue,
    AnnotationsDecl.empty
)
```

#### 3.3 Add Type Generator (Primitive Pattern)

**File:** `scalus-core/shared/src/main/scala/scalus/compiler/sir/lowering/LoweredValueRepresentation.scala`

Update `PrimitiveRepresentation.isCompatibleWithType` to include `BuiltinValue`:

```scala
sealed trait PrimitiveRepresentation(...) extends LoweredValueRepresentation {
    override def isCompatibleWithType(tp: SIRType): Boolean = {
        tp match {
            case SIRType.Integer | SIRType.Data() | SIRType.ByteString | SIRType.String |
                SIRType.Boolean | SIRType.Unit | SIRType.BLS12_381_G1_Element |
                SIRType.BLS12_381_G2_Element | SIRType.BLS12_381_MlResult |
                SIRType.BuiltinValue =>  // <-- add this
                true
            case _ => false
        }
    }
}
```

**File:** `scalus-core/shared/src/main/scala/scalus/compiler/sir/lowering/typegens/PrimitiveSirTypeGenerators.scala`

Add `BuiltinValueSirTypeGenerator` following `BLS12_381_G1_SirTypeGenerator` pattern:

```scala
object BuiltinValueSirTypeGenerator extends PrimitiveSirTypeGenerator {

    // Constant -> PackedData: apply valueData builtin
    override def uplcToDataValue(input: LoweredValue, pos: SIRPosition)(using LoweringContext): LoweredValue =
        lvBuiltinApply(SIRBuiltins.valueData, input, SIRType.Data(), PrimitiveRepresentation.PackedData, pos)

    // PackedData -> Constant: apply unValueData builtin
    override def dataToUplcValue(input: LoweredValue, pos: SIRPosition)(using LoweringContext): LoweredValue =
        lvBuiltinApply(SIRBuiltins.unValueData, input, SIRType.BuiltinValue, PrimitiveRepresentation.Constant, pos)
}
```

Register in `SirTypeUplcGenerator` dispatcher.

### Phase 4: Prelude Integration

#### 4.1 Add Scala API in Prelude

**File:** `scalus-core/shared/src/main/scala/scalus/prelude/BuiltinValue.scala` (new)

```scala
package scalus.prelude

import scalus.Compile
import scalus.builtin.ByteString

/** MaryEraValue operations for smart contracts */
@Compile
object BuiltinValueOps:
    /** Insert or update a token amount in a value */
    def insertCoin(
        currencySymbol: ByteString,
        tokenName: ByteString,
        amount: BigInt,
        value: scalus.builtin.BuiltinValue
    ): scalus.builtin.BuiltinValue =
        scalus.builtin.Builtins.insertCoin(currencySymbol, tokenName, amount, value)

    // ... other operations
```

### Phase 5: Cost Model

#### 5.1 Add Cost Model Parameters

**File:** `scalus-core/shared/src/main/scala/scalus/uplc/eval/BuiltinCostModel.scala`

Add cost model entries for all Value operations following existing patterns.

### Phase 6: Serialization

#### 6.1 Flat Encoding

Update flat codec to handle Value constants with tag 13.

#### 6.2 CBOR Encoding

Add CBOR serialization for Value type (nested map structure).

---

## Testing Strategy

1. **Unit tests** for Value type operations
2. **Round-trip tests** for serialization (Flat, CBOR)
3. **CEK evaluation tests** for each builtin function
4. **Integration tests** comparing results with Haskell Plutus
5. **Property-based tests** for Value invariants

## File Summary

| File | Changes |
|------|---------|
| `scalus-core/shared/.../builtin/BuiltinValue.scala` | New - BuiltinValue type definition |
| `scalus-core/shared/.../uplc/DefaultUni.scala` | Add BuiltinValue case |
| `scalus-core/shared/.../uplc/DefaultFun.scala` | Add 7 builtin function cases |
| `scalus-core/shared/.../uplc/Constant.scala` | Add BuiltinValue constant |
| `scalus-core/shared/.../uplc/Builtin.scala` | Implement builtin functions |
| `scalus-core/shared/.../uplc/eval/BuiltinValueOps.scala` | New - validation, construction, costing, operations |
| `scalus-core/shared/.../uplc/eval/MemoryUsage.scala` | Add memoryUsageBuiltinValue |
| `scalus-core/shared/.../uplc/eval/BuiltinCostModel.scala` | Add cost parameters |
| `scalus-core/shared/.../compiler/sir/SIRType.scala` | Add BuiltinValue primitive |
| `scalus-core/shared/.../compiler/sir/SIRBuiltins.scala` | Add builtin definitions |
| `scalus-core/shared/.../compiler/sir/lowering/LoweredValueRepresentation.scala` | Update PrimitiveRepresentation.isCompatibleWithType |
| `scalus-core/shared/.../compiler/sir/lowering/typegens/PrimitiveSirTypeGenerators.scala` | Add BuiltinValueSirTypeGenerator |
| `scalus-core/shared/.../prelude/BuiltinValue.scala` | New - Prelude API |

## Notes

- **Naming:** We use `BuiltinValue` instead of `Value` to avoid conflict with `scalus.ledger.api.v1.Value`
- **Primitive opaque type:** `BuiltinValue` is a primitive type like BLS12_381 elements - inner representation is hidden, accessible only via builtin operations
- **No UPLC construction from SortedMap:** You cannot construct `BuiltinValue` from a Scala `SortedMap` in UPLC; use `unValueData` or builtin operations (`insertCoin`, `unionValue`, etc.)
- **BigInt for quantities:** Plutus uses signed 128-bit integer bounds; validation happens in builtin operations
- **ByteString for keys:** Key length validation (max 32 bytes) happens in operations
- Currency symbols are either empty (for ADA) or exactly 28 bytes (script hash)
- Token names can be 0-32 bytes
- BuiltinValue invariants: no empty inner maps, no zero quantities
- Operations must check for overflow in 128-bit range
