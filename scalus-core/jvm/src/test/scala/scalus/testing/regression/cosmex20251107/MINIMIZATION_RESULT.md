# Minimization Result

## Success! Bug Reproduced in Step 4

The minimal test case that reproduces the shadowing bug is in **ContractStep4.scala**.

## What Triggers the Bug

The bug requires ALL of the following elements:

1. **Enum with case containing the parameter to be shadowed**:
   ```scala
   enum Action:
       case Close(party: Party, signedSnapshot: SignedSnapshot)
   ```

2. **Pattern match on enum extracts the parameter**:
   ```scala
   action match
       case Close(party, signedSnapshot) =>  // Extract signedSnapshot
           handleClose(..., signedSnapshot, ...)  // Pass to next function
   ```

3. **Nested function with parameter that will be shadowed**:
   ```scala
   inline def handleClose(..., newSignedSnapshot: SignedSnapshot, ...)
   ```

4. **Inner pattern match shadows a field name**:
   ```scala
   newSignedSnapshot match
       case SignedSnapshot(signedSnapshot, ...) =>  // Shadow! Field name = signedSnapshot
   ```

5. **Use of shadowed variable and outer parameter**:
   ```scala
   balancedSnapshot(signedSnapshot.snapshotTradingState, ...)  // Use shadowed
   && validSignedSnapshot(newSignedSnapshot, ...)  // Use outer parameter
   ```

The compiler incorrectly resolves `newSignedSnapshot` to the shadowed `signedSnapshot: Snapshot` instead of the parameter `newSignedSnapshot: SignedSnapshot`.

## Error Message

```
Cannot calculate apply type for scalus.testing.regression.cosmex20251107.step4.SignedSnapshot -> 
scalus.ledger.api.v3.TxOutRef -> ByteString -> ByteString -> Boolean to 
scalus.testing.regression.cosmex20251107.step4.Snapshot, function type does not match argument type.
Failed unification scalus.testing.regression.cosmex20251107.step4.SignedSnapshot and 
scalus.testing.regression.cosmex20251107.step4.Snapshot, path= constrDecl
```

## Minimization Steps Taken

1. **Step 1**: Single match with shadowing - PASS (no bug)
2. **Step 2**: Nested match with shadowing - PASS (no bug)
3. **Step 3**: Triple nested match with state - PASS (no bug)
4. **Step 4**: Added Action enum match - **FAIL (bug reproduced!)** ✅

## Files

- `ContractStep4.scala` - Minimal reproduction (~180 lines)
- `CompileCosmexStep4Test.scala` - Test that fails with the bug
- `ContractFull.scala` - Original full contract (~950 lines)
- `CompileCosmexFullTest.scala` - Original failing test

## Recommendation

Use `ContractStep4.scala` as the basis for:
1. Creating a focused unit test for the compiler bug fix
2. Understanding the root cause in the SIR type system
3. Regression testing once the bug is fixed

Both Step4 and Full tests should start passing once the bug is fixed.
