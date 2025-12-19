# Add `copy` method support for case classes

Currently in Scalus it is impossible to use `copy` for case classes in scripts compiled by the compiler plugin to UPLC.

## Background

When Scala compiles a `copy` call like:
```scala
case class User(name: String, age: BigInt)
val user = User("John", BigInt(30))
user.copy(age = BigInt(31))
```

It desugars this to:
```scala
user.copy(User.copy$default$1(user), BigInt(31))
```

Where `copy$default$N` are synthetic methods that return the default values for parameters not explicitly provided (i.e., they extract the corresponding field from the original object).

## Implementation Design

### 1. Handle `copy` method calls
- `copy` for case class should be translated to SIR as call of `Constr`
- Pattern: `Apply(Select(obj, nme.copy), args)` where `obj` is a case class instance
- Use existing `compileNewConstructor` to generate the `SIR.Constr`
- Location: `SIRCompiler.scala` in `compileExpr2` method

### 2. Handle `copy$default$N` synthetic methods
- In Scala 3, `copy$default$N` is accessed as `obj.copy$default$N` (a Select on the instance)
- Pattern: `Select(obj, DefaultGetterName(nme.copy, idx))` where `obj` is a case class instance
- Compile as `obj.fieldN` (SIR.Select with the N-th field name)
- The `idx` from `DefaultGetterName` is 0-indexed
- Location: `SIRCompiler.scala` in `compileExpr2` method, in the `Select` case

### 3. No changes to lowering
- The existing SIR to UPLC lowering handles `Constr` and field selects already

### 4. Tests
- Test in `scalus-core/jvm/src/test/scala/scalus/CompilerPluginEvalTest.scala`
- Test case: `CopyTestUser` case class with `copy` using partial arguments
