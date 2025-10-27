# Known Issues in Defunctionalized JIT

## Nested Lambda Applications with Builtins

**Status:** 9/10 tests passing
**Failing Test:** `(\x -> x + 1) 5` returns a Function instead of `6`

### Problem
When a lambda body contains nested Apply nodes with builtins that return curried functions (like AddInteger), the final result is a partially-applied function instead of the computed value.

### Test Case
```scala
Term.Apply(
  Term.LamAbs("x", 
    Term.Apply(
      Term.Apply(Term.Builtin(AddInteger), Term.Var("x")),
      Term.Const(1)
    )
  ),
  Term.Const(5)
)
```

Expected: `6`
Actual: `Function1` object

### What Works
- ✅ Identity function: `(\x -> x) 42`
- ✅ Simple operations: `5 + 10`
- ✅ Nested operations: `(5 + 10) * 2`
- ✅ Constants, builtins, variable lookup

### Investigation
The compilation scheme appears correct according to manual trace analysis. The issue may be related to:
1. How OP_APPLY calculates return addresses for nested Applies
2. Frame stack management when mixing defunctionalized control flow with Scala Function1 objects
3. Interaction between FRAME_RESTORE_ENV and FRAME_APPLY_EXEC

### Next Steps
- Add comprehensive debug logging to trace actual execution
- Compare instruction sequences with mincont JIT
- Consider alternative compilation strategies for nested Applies
