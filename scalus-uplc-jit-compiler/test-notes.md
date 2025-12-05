# JIT Compiler Test Notes

## Case on Builtins (Integer, Bool, Unit)

The JIT compiler supports case expressions on builtin types (Integer, Bool, Unit) in addition to Constr.

### Testing Strategy

No dedicated unit tests are added for case on builtins in the JIT compiler because:

1. The Plutus conformance tests will be run against the JIT compiler in the future
2. These conformance tests cover case on Integer, Bool, and Unit thoroughly
3. Running conformance tests on JIT ensures behavior matches the CEK machine exactly

### Implementation Notes

- `CaseHelper.scala` provides `genCaseDispatch` (for nativestack JIT) and `genCaseDispatchCont` (for mincont JIT)
- Runtime type dispatch is used to distinguish between Constr `(Long, List[Any])` and builtin types
- In the future, external UPLC attribution could allow static dispatch at JIT compile time, avoiding runtime type checks
