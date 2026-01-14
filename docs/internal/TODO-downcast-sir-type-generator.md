# TODO: Consider adding downcast operation to SirTypeGenerator

## Context

When casting from a sum type to one of its product type children (e.g., `TestStatus` to `TestStatus.Done`),
the lowering needs to handle the representation conversion properly.

When we have a type-test pattern like:
```scala
case lhsDone: TestStatus.Done => lhsDone.result
```

The pattern matching compiler creates a `Cast` from the sum type to the product type.

## Current Solution

The issue is now solved using the `defaultTypeVarRepresentation` mechanism in `lvCast`:

1. In `lvCast` (LoweredValue.scala), when `changeRepresentation` is true:
   - Convert the source value to its own `defaultTypeVarRepresentation` (e.g., `DataConstr` for sum types)
   - Use the target type's `defaultTypeVarRepresentation` as the result representation (e.g., `ProdDataConstr` for product types)

2. This works because `DataConstr` and `ProdDataConstr` are compatible representations - they have the
   same underlying Data constructor structure.

## Open Question

It's unclear whether adding a dedicated `downcast` operation to `SirTypeGenerator` would provide additional
benefits beyond the current `defaultTypeVarRepresentation` approach. Potential considerations:

1. **Type safety**: A dedicated downcast could verify that the target type is indeed a child of the source sum type
2. **Runtime checks**: Could potentially add runtime verification (though this adds overhead)
3. **Cleaner semantics**: Explicit downcast vs implicit representation compatibility

For now, the `defaultTypeVarRepresentation` mechanism works correctly and all tests pass.

## Related Files

- `scalus-core/shared/src/main/scala/scalus/compiler/sir/lowering/LoweredValue.scala` - `lvCast` function
- `scalus-core/shared/src/main/scala/scalus/compiler/sir/lowering/typegens/ProductCaseSirTypeGenerator.scala`
- `scalus-core/shared/src/main/scala/scalus/compiler/sir/lowering/typegens/SumCaseSirTypeGenerator.scala`
- `scalus-plugin/src/main/scala/scalus/compiler/plugin/PatternMatchingCompiler.scala` - generates Cast for type-test patterns
