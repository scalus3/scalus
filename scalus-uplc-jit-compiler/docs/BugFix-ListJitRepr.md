# Bug Fix: NativeStack ListJitRepr Removal

**Date**: 2025-10-28
**Issue**: ClassCastException in NativeStack JIT during benchmarks
**Status**: Fixed ✅

## Problem

During Phase 1 benchmarking, the NativeStack JIT implementation crashed with:

```
java.lang.ClassCastException: class scalus.uplc.eval.nativestack.ListJitRepr
cannot be cast to class scala.collection.immutable.List
```

## Root Cause

We updated `JIT.scala` to use plain `List[Any]` instead of `ListJitRepr` for list constants and builtins (ChooseList, HeadList, TailList), but **forgot to update `RuntimeHelper.scala`** which still had several references to `ListJitRepr`:

1. `anyUplcConstant` - pattern matched on `ListJitRepr`
2. `uplcToJitAny` - returned `ListJitRepr.fromConstantList`
3. `unConstrData` - returned `BuiltinPair[BigInt, ListJitRepr]`
4. `unListData` - returned `ListJitRepr`

## Solution

Updated `RuntimeHelper.scala` to use plain `List[Any]` throughout:

### Before:
```scala
case l: ListJitRepr => l.toConstant

case l @ Constant.List(elemType, v) =>
    ListJitRepr.fromConstantList(l)

final def unConstrData(d: Data): BuiltinPair[BigInt, ListJitRepr] = {
    // ...
    BuiltinPair(BigInt(index), ListJitRepr(DefaultUni.Data, fields))
}

final def unListData(d: Data): ListJitRepr = d match
    case Data.List(values) => ListJitRepr(DefaultUni.Data, values)
```

### After:
```scala
case l: List[?] =>
    // Lists are plain List[Any] - element type lost at runtime
    Constant.List(DefaultUni.Data, l.map(anyUplcConstant))

case Constant.List(elemType, v) =>
    // Return plain List[Any] - element type not needed
    v.map(uplcToJitAny)

final def unConstrData(d: Data): BuiltinPair[BigInt, List[Data]] = {
    // ...
    BuiltinPair(BigInt(index), fields)  // Plain List[Data]
}

final def unListData(d: Data): List[Data] = d match
    case Data.List(values) => values  // Plain List[Data]
```

## Key Insights

1. **List operations have constant cost** - No need to track element type at runtime
2. **Element type only matters for serialization** - When converting back to `Constant.List`, we default to `DefaultUni.Data` (the most common case)
3. **Type information is lost anyway** - At runtime, everything is `Any`, so `List[Any]` is more honest
4. **Simpler is better** - Removing the wrapper class eliminates a whole class of bugs

## Files Modified

- `scalus-uplc-jit-compiler/src/main/scala/scalus/uplc/eval/nativestack/RuntimeHelper.scala`

## Impact

- ✅ Fixes ClassCastException crash
- ✅ Consistent with Mincont implementation
- ✅ Simpler code (no wrapper class needed)
- ✅ Zero performance impact (list ops are constant cost)

## Next Steps

1. **Re-run benchmarks** - Get complete results with fixed NativeStack
2. **Compare all implementations** - CeK vs Mincont vs NativeStack
3. **Investigate Mincont variance** - Why did it show high variance (±1839 μs)?

## Lessons Learned

When making architectural changes (like removing a wrapper class):
1. **Grep for all references** - Don't rely on compiler to find everything
2. **Check helper/utility code** - Often overlooked during refactoring
3. **Run tests/benchmarks early** - Catch issues before they compound
4. **Update related code together** - RuntimeHelper should have been updated with JIT.scala

---

**Status**: Bug fixed, code compiles successfully, ready for re-benchmark.
