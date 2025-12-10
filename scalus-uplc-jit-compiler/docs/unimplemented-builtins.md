# Unimplemented Builtins in JIT Compilers

This document tracks the implementation status of UPLC builtins in the scalus-uplc-jit-compiler.

## Overview

The scalus-uplc-jit-compiler provides two JIT compiler implementations:
- **Native JIT** (nativestack): Direct code generation with native stack
- **Mincont JIT** (mincont): Continuation-based code generation

Both compilers share the same builtin implementation code (in `jitcommon/BuiltinEmitter.scala`), so they have the same coverage.

There is also a **Hybrid JIT** compiler that falls back to the Cek machine for unimplemented features.

## Implementation Status

**Total Builtins**: 87
**Implemented**: 54 
**Unimplemented**: 33 

## Implemented Builtins (54)

### Integer Operations (10)
- ✓ AddInteger
- ✓ SubtractInteger
- ✓ MultiplyInteger
- ✓ DivideInteger
- ✓ QuotientInteger
- ✓ RemainderInteger
- ✓ ModInteger
- ✓ EqualsInteger
- ✓ LessThanInteger
- ✓ LessThanEqualsInteger

### ByteString Operations (8)
- ✓ AppendByteString
- ✓ ConsByteString
- ✓ SliceByteString
- ✓ LengthOfByteString
- ✓ IndexByteString
- ✓ EqualsByteString
- ✓ LessThanByteString
- ✓ LessThanEqualsByteString

### Hash Functions (3)
- ✓ Sha2_256
- ✓ Sha3_256
- ✓ Blake2b_256

### String Operations (4)
- ✓ AppendString
- ✓ EqualsString
- ✓ EncodeUtf8
- ✓ DecodeUtf8

### Control Flow (3)
- ✓ IfThenElse
- ✓ Trace
- ✓ ChooseUnit

### Pair Operations (2)
- ✓ FstPair
- ✓ SndPair

### List Operations (5)
- ✓ ChooseList
- ✓ MkCons
- ✓ HeadList
- ✓ TailList
- ✓ NullList

### Data Operations (16)
- ✓ ConstrData
- ✓ MapData
- ✓ ListData
- ✓ IData
- ✓ BData
- ✓ UnConstrData
- ✓ UnMapData
- ✓ UnListData
- ✓ UnIData
- ✓ UnBData
- ✓ EqualsData
- ✓ SerialiseData
- ✓ MkPairData
- ✓ MkNilData
- ✓ MkNilPairData
- ✓ ChooseData

### Cryptographic Signatures (3)
- ✗ VerifyEd25519Signature
- ✗ VerifyEcdsaSecp256k1Signature
- ✗ VerifySchnorrSecp256k1Signature

## Unimplemented Builtins (34)

### Control Flow (1)

### BLS12_381 G1 Operations (7)
- ✗ Bls12_381_G1_add
- ✗ Bls12_381_G1_neg
- ✗ Bls12_381_G1_scalarMul
- ✗ Bls12_381_G1_equal
- ✗ Bls12_381_G1_hashToGroup
- ✗ Bls12_381_G1_compress
- ✗ Bls12_381_G1_uncompress

### BLS12_381 G2 Operations (7)
- ✗ Bls12_381_G2_add
- ✗ Bls12_381_G2_neg
- ✗ Bls12_381_G2_scalarMul
- ✗ Bls12_381_G2_equal
- ✗ Bls12_381_G2_hashToGroup
- ✗ Bls12_381_G2_compress
- ✗ Bls12_381_G2_uncompress

### BLS12_381 Pairing Operations (3)
- ✗ Bls12_381_millerLoop
- ✗ Bls12_381_mulMlResult
- ✗ Bls12_381_finalVerify

### Additional Hash Functions (3)
- ✗ Keccak_256
- ✗ Blake2b_224
- ✗ Ripemd_160

### Conversions and Bitwise Operations (13)
- ✗ IntegerToByteString
- ✗ ByteStringToInteger
- ✗ AndByteString
- ✗ OrByteString
- ✗ XorByteString
- ✗ ComplementByteString
- ✗ ReadBit
- ✗ WriteBits
- ✗ ReplicateByte
- ✗ ShiftByteString
- ✗ RotateByteString
- ✗ CountSetBits
- ✗ FindFirstSetBit

## Priority Recommendations

Based on common usage patterns in Cardano smart contracts, the following builtins should be prioritized for implementation:

### High Priority
All high-priority data operations have been implemented! (SerialiseData, ChooseData, MkPairData, UnMapData)

### Medium Priority
1. **VerifyEd25519Signature** - Used in signature verification
2. **VerifyEcdsaSecp256k1Signature** - Alternative signature scheme
3. **VerifySchnorrSecp256k1Signature** - Alternative signature scheme
4. **IntegerToByteString** - Conversion operations
5. **ByteStringToInteger** - Conversion operations
6. **Keccak_256** - Alternative hash function
7. **Blake2b_224** - Alternative hash function
8. **Ripemd_160** - Alternative hash function

### Low Priority (Less Common Usage)
1. **BLS12_381 operations** (17 builtins) - Specialized cryptographic operations, less commonly used
2. **Bitwise operations** (AndByteString, OrByteString, XorByteString, etc.) - Specialized operations
3. **ChooseUnit** - Less frequently used control flow

## Implementation Notes

### Reference Implementation
All builtins are fully implemented in the Cek interpreter located at:
- `scalus-core/shared/src/main/scala/scalus/uplc/Builtin.scala`

### JIT Implementation Location
JIT builtin implementations are in:
- `scalus-uplc-jit-compiler/src/main/scala/scalus/uplc/eval/jitcommon/BuiltinEmitter.scala`
- `scalus-uplc-jit-compiler/src/main/scala/scalus/uplc/eval/jitcommon/BuiltinSnippets.scala`
- `scalus-uplc-jit-compiler/src/main/scala/scalus/uplc/eval/jitcommon/BuiltinAppliedGenerator.scala`

### Fallback Behavior
When the JIT compiler encounters an unimplemented builtin:
- The **hybrid compiler** will automatically fall back to the Cek machine
- The **native** and **mincont** compilers will throw an error indicating the builtin is not supported

## Contributing

To implement a missing builtin:

1. Study the reference implementation in `scalus-core/.../Builtin.scala`
2. Add the case to `BuiltinEmitter.emitBuiltin()` for unapplied builtins
3. Add optimization cases to `emitAppliedBuiltin1()` or `emitAppliedBuiltin2()` if applicable
4. Implement the code generation in `BuiltinSnippets.scala` or `BuiltinAppliedGenerator.scala`
5. Add tests to verify correctness
6. Update this document

---

*Last Updated: 2025-10-29*

## Recent Changes

### 2025-10-29 (Batch 3)

### 2025-10-29 (Batch 2 - High Priority Data Operations)
- ✓ Implemented **SerialiseData** - Serialize Data to ByteString
- ✓ Implemented **ChooseData** - Pattern match on Data types
- ✓ Implemented **MkPairData** - Create Data pairs
- ✓ Implemented **UnMapData** - Extract map from Data
- Updated implementation count: 50/87 builtins (57.5%)

### 2025-10-29 (Batch 1 - ByteString Operations)
- ✓ Implemented **ConsByteString** - Prepend a byte to a ByteString
- ✓ Implemented **SliceByteString** - Extract a slice from a ByteString
- ✓ Implemented **IndexByteString** - Get byte at specific index
- Updated implementation count: 46/87 builtins (52.9%)
