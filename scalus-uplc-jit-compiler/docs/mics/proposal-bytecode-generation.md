# Bytecode Generation Design for UPLC JIT

## Overview

This document describes the technical design for implementing direct JVM bytecode generation for UPLC terms, replacing the current Scala 3 staging-based approach.

## Goals

1. **Performance:** 5-10x faster than current JIT implementation
2. **Memory:** 3-5x less allocation than current JIT
3. **Correctness:** 100% compatible with CekMachine semantics
4. **Maintainability:** Clean, testable, documented code

## Architecture

### High-Level Flow

```
UPLC Term
    ↓
[Analysis Phase]
    ↓ 
[Bytecode Generation]
    ↓
JVM Class Definition
    ↓
[ClassLoader]
    ↓
Compiled Function
    ↓
[Execution]
    ↓
Result
```

### Module Structure

```
scalus.uplc.eval.bytecode/
  ├── BytecodeCompiler.scala       // Main compilation entry point
  ├── BytecodeGenerator.scala      // ASM-based bytecode emission
  ├── TermAnalyzer.scala           // Static analysis of UPLC terms
  ├── TypeInference.scala          // Infer types for specialization
  ├── BuiltinHandlers.scala        // Specialized builtin implementations
  ├── RuntimeSupport.scala         // Runtime helper methods
  └── ClassCache.scala             // Compiled class caching
```

## Design Details

### 1. Compilation Strategy

#### Class Generation

For each UPLC term, generate a class implementing `CompiledFunction`:

```scala
trait CompiledFunction {
  def execute(
    logger: Logger,
    budgetSpender: BudgetSpender, 
    params: MachineParams
  ): Any
}
```

**Generated Class Structure:**

```java
// Example: Compiled version of (λx. x + 1)
public class Compiled_Lambda_<hash> implements CompiledFunction {
    
    // Constants pool
    private static final BigInteger CONST_1 = BigInteger.ONE;
    
    @Override
    public Object execute(Logger logger, BudgetSpender budget, MachineParams params) {
        budget.spendBudget(
            ExBudgetCategory.Startup$.MODULE$,
            params.machineCosts().startupCost(),
            ArraySeq.empty()
        );
        
        // Return lambda function
        return new Function1<Object, Object>() {
            public Object apply(Object x) {
                budget.spendBudget(
                    ExBudgetCategory.Step$.MODULE$.apply(StepKind.Apply$.MODULE$),
                    params.machineCosts().applyCost(),
                    ArraySeq.empty()
                );
                
                BigInteger xVal = (BigInteger) x;
                return xVal.add(CONST_1);
            }
        };
    }
}
```

### 2. Type System for Specialization

#### Value Types

```scala
sealed trait ValueType
object ValueType {
  case object TInteger extends ValueType
  case object TByteString extends ValueType  
  case object TString extends ValueType
  case object TBool extends ValueType
  case object TUnit extends ValueType
  case object TData extends ValueType
  case class TList(elem: ValueType) extends ValueType
  case class TPair(a: ValueType, b: ValueType) extends ValueType
  case class TFunction(args: List[ValueType], ret: ValueType) extends ValueType
  case object TAny extends ValueType  // Unknown type
}
```

#### Type Inference

```scala
class TypeInference {
  def inferTypes(term: Term): Map[Term, ValueType] = {
    // Forward type propagation
    val types = mutable.HashMap[Term, ValueType]()
    
    def infer(term: Term): ValueType = term match {
      case Term.Const(c) => constantType(c)
      case Term.Var(name) => types.getOrElse(name, ValueType.TAny)
      case Term.LamAbs(param, body) =>
        val paramType = types.getOrElse(param, ValueType.TAny)
        val retType = infer(body)
        ValueType.TFunction(List(paramType), retType)
      case Term.Apply(fun, arg) =>
        infer(fun) match {
          case ValueType.TFunction(args, ret) => ret
          case _ => ValueType.TAny
        }
      case Term.Builtin(bn) => builtinReturnType(bn)
      // ... other cases
    }
    
    infer(term)
    types.toMap
  }
}
```

### 3. Bytecode Generation with ASM

#### Method Generation

```scala
class BytecodeGenerator(cw: ClassWriter) {
  
  def generateExecuteMethod(term: Term, types: Map[Term, ValueType]): Unit = {
    val mv = cw.visitMethod(
      ACC_PUBLIC,
      "execute",
      "(Lscalus/uplc/eval/Logger;Lscalus/uplc/eval/BudgetSpender;Lscalus/uplc/eval/MachineParams;)Ljava/lang/Object;",
      null,
      null
    )
    
    mv.visitCode()
    
    // Startup budget
    emitBudgetSpend(mv, "Startup", "startupCost")
    
    // Generate term code
    generateTerm(mv, term, types, LocalVars(logger=1, budget=2, params=3))
    
    mv.visitInsn(ARETURN)
    mv.visitMaxs(0, 0)  // Auto-compute
    mv.visitEnd()
  }
  
  private def generateTerm(
    mv: MethodVisitor,
    term: Term,
    types: Map[Term, ValueType],
    locals: LocalVars
  ): Unit = term match {
    
    case Term.Const(Constant.Integer(value)) =>
      // Load BigInteger constant
      mv.visitLdcInsn(value.toString)
      mv.visitTypeInsn(NEW, "java/math/BigInteger")
      mv.visitInsn(DUP_X1)
      mv.visitInsn(SWAP)
      mv.visitMethodInsn(
        INVOKESPECIAL,
        "java/math/BigInteger",
        "<init>",
        "(Ljava/lang/String;)V",
        false
      )
      
    case Term.Var(name) =>
      // Load from local variable
      val idx = locals.getVar(name)
      mv.visitVarInsn(ALOAD, idx)
      
    case Term.LamAbs(param, body) =>
      // Generate inner class for lambda
      generateLambdaClass(mv, param, body, types, locals)
      
    case Term.Apply(fun, arg) =>
      // Emit budget spend
      emitBudgetSpend(mv, "Apply", "applyCost", locals)
      
      // Generate function
      generateTerm(mv, fun, types, locals)
      
      // Generate argument  
      generateTerm(mv, arg, types, locals)
      
      // Call apply method
      mv.visitMethodInsn(
        INVOKEINTERFACE,
        "scala/Function1",
        "apply",
        "(Ljava/lang/Object;)Ljava/lang/Object;",
        true
      )
      
    case Term.Builtin(bn) =>
      generateBuiltin(mv, bn, types, locals)
      
    // ... other cases
  }
}
```

#### Builtin Optimization

For frequently used builtins, generate optimized code directly:

```scala
private def generateBuiltin(
  mv: MethodVisitor,
  bn: DefaultFun,
  types: Map[Term, ValueType],
  locals: LocalVars
): Unit = bn match {
  
  case DefaultFun.AddInteger =>
    // Generate: (x: BigInt) => (y: BigInt) => x + y
    val addLambdaClass = generateInnerClass(
      "AddInteger",
      { inner =>
        // Outer lambda: takes x
        val outerApply = inner.visitMethod(ACC_PUBLIC, "apply", "(Ljava/lang/Object;)Ljava/lang/Object;", null, null)
        outerApply.visitCode()
        
        // Store x in field
        outerApply.visitVarInsn(ALOAD, 0) // this
        outerApply.visitVarInsn(ALOAD, 1) // x
        outerApply.visitFieldInsn(PUTFIELD, inner.name, "x", "Ljava/math/BigInteger;")
        
        // Return inner lambda that takes y
        val innerLambdaClass = generateInnerClass(
          "AddIntegerInner",
          { innerInner =>
            val innerApply = innerInner.visitMethod(ACC_PUBLIC, "apply", "(Ljava/lang/Object;)Ljava/lang/Object;", null, null)
            innerApply.visitCode()
            
            // Spend budget
            emitBudgetSpend(innerApply, "Builtin", locals)
            
            // Load x from outer.x
            innerApply.visitVarInsn(ALOAD, 0) // this  
            innerApply.visitFieldInsn(GETFIELD, innerInner.name, "outer", s"L${inner.name};")
            innerApply.visitFieldInsn(GETFIELD, inner.name, "x", "Ljava/math/BigInteger;")
            
            // Load y from parameter
            innerApply.visitVarInsn(ALOAD, 1)
            innerApply.visitTypeInsn(CHECKCAST, "java/math/BigInteger")
            
            // Call x.add(y)
            innerApply.visitMethodInsn(
              INVOKEVIRTUAL,
              "java/math/BigInteger",
              "add",
              "(Ljava/math/BigInteger;)Ljava/math/BigInteger;",
              false
            )
            
            innerApply.visitInsn(ARETURN)
            innerApply.visitMaxs(0, 0)
            innerApply.visitEnd()
          }
        )
        
        // Instantiate inner lambda
        outerApply.visitTypeInsn(NEW, innerLambdaClass)
        outerApply.visitInsn(DUP)
        outerApply.visitVarInsn(ALOAD, 0) // outer reference
        outerApply.visitMethodInsn(INVOKESPECIAL, innerLambdaClass, "<init>", s"(L${inner.name};)V", false)
        
        outerApply.visitInsn(ARETURN)
        outerApply.visitMaxs(0, 0)
        outerApply.visitEnd()
      }
    )
    
    // Instantiate outer lambda
    mv.visitTypeInsn(NEW, addLambdaClass)
    mv.visitInsn(DUP)
    mv.visitMethodInsn(INVOKESPECIAL, addLambdaClass, "<init>", "()V", false)
    
  case _ =>
    // Fallback to runtime helper
    generateRuntimeBuiltinCall(mv, bn, locals)
}
```

### 4. Optimizations

#### A. Constant Folding

```scala
def constantFold(term: Term): Term = term match {
  case Apply(Apply(Builtin(AddInteger), Const(Constant.Integer(a))), Const(Constant.Integer(b))) =>
    Const(Constant.Integer(a + b))
    
  case Apply(Apply(Builtin(MultiplyInteger), Const(Constant.Integer(a))), Const(Constant.Integer(b))) =>
    Const(Constant.Integer(a * b))
    
  case Apply(Apply(Apply(Builtin(IfThenElse), Const(Constant.Bool(cond))), thenBranch), elseBranch) =>
    if cond then thenBranch else elseBranch
    
  case _ => term
}
```

#### B. Inline Expansion

```scala
def shouldInline(term: Term): Boolean = {
  val cost = estimateCost(term)
  val benefit = estimateBenefit(term)
  cost < 100 && benefit > cost * 2
}

def inlineTerm(call: Term.Apply, lambda: Term.LamAbs): Term = {
  // Substitute parameter with argument
  substitute(lambda.body, lambda.param, call.arg)
}
```

#### C. Tail Call Optimization

```scala
def optimizeTailCalls(term: Term): Term = {
  // Detect tail-recursive calls
  // Transform to iterative loop
  term match {
    case LamAbs(param, body) if isTailRecursive(body, param) =>
      generateIterativeLoop(param, body)
    case _ => term
  }
}
```

#### D. Specialization

Generate specialized methods for common type combinations:

```scala
// Instead of: Object apply(Object x)
// Generate specialized versions:
BigInteger apply_BigInt_BigInt(BigInteger x, BigInteger y)
Boolean apply_Bool_Bool(Boolean x, Boolean y)
```

### 5. Runtime Support

```scala
object RuntimeSupport {
  
  // Fast path for common operations - called from generated bytecode
  
  @inline
  def addInteger(x: BigInt, y: BigInt): BigInt = x + y
  
  @inline
  def lessThanInteger(x: BigInt, y: BigInt): Boolean = x < y
  
  @inline
  def ifThenElse[T](cond: Boolean, thenBranch: () => T, elseBranch: () => T): T =
    if cond then thenBranch() else elseBranch()
    
  // Data constructors
  def constrData(tag: Long, args: List[Any]): Data.Constr =
    Data.Constr(tag, args.map(_.asInstanceOf[Data]))
    
  // Error handling  
  def uplcError(): Nothing = throw new RuntimeException("UPLC Error")
  
  // Type conversions
  def unIData(d: Data): BigInt = d match {
    case Data.I(i) => i
    case _ => throw new BuiltinException(s"Expected I, got $d")
  }
}
```

### 6. Class Loading and Caching

```scala
class CompiledClassLoader extends ClassLoader {
  
  def defineClass(name: String, bytecode: Array[Byte]): Class[_] = {
    super.defineClass(name, bytecode, 0, bytecode.length)
  }
}

class ClassCache {
  private val cache = new ConcurrentHashMap[String, CompiledFunction]()
  private val loader = new CompiledClassLoader()
  
  def getOrCompile(term: Term): CompiledFunction = {
    val hash = termHash(term)
    cache.computeIfAbsent(hash, _ => {
      val bytecode = BytecodeCompiler.compile(term)
      val clazz = loader.defineClass(s"Compiled_$hash", bytecode)
      clazz.getDeclaredConstructor().newInstance().asInstanceOf[CompiledFunction]
    })
  }
  
  private def termHash(term: Term): String = {
    val digest = MessageDigest.getInstance("SHA-256")
    val bytes = term.toString.getBytes(StandardCharsets.UTF_8)
    digest.digest(bytes).map("%02x".format(_)).mkString
  }
}
```

### 7. Error Handling and Debugging

#### Debug Information

```scala
def emitDebugInfo(mv: MethodVisitor, term: Term, lineNumber: Int): Unit = {
  val label = new Label()
  mv.visitLabel(label)
  mv.visitLineNumber(lineNumber, label)
  
  // Add local variable debug info
  mv.visitLocalVariable(
    "term",
    "Ljava/lang/String;",
    null,
    label,
    label,
    localVarIndex
  )
}
```

#### Stack Traces

Generate descriptive stack traces that map back to UPLC terms:

```scala
// Wrap each operation in try-catch for better error messages
try {
  // ... generated code ...
} catch {
  case e: Exception =>
    throw new MachineError(
      s"Error evaluating ${term.show} at position ${term.sourcePos}",
      e
    )
}
```

## Implementation Phases

### Phase 1: Foundation (Week 1-2)
- [ ] Set up ASM dependency
- [ ] Implement basic BytecodeGenerator  
- [ ] Generate simple Const/Var cases
- [ ] Implement CompiledFunction interface
- [ ] Basic ClassLoader and caching

### Phase 2: Core Features (Week 3-4)
- [ ] Implement LamAbs generation (inner classes)
- [ ] Implement Apply with function calls
- [ ] Add Delay/Force support
- [ ] Implement 10 most common builtins

### Phase 3: Complete Implementation (Week 5-6)
- [ ] Implement all builtins
- [ ] Add Constr/Case support
- [ ] Implement budget tracking
- [ ] Add logging support
- [ ] Error handling and stack traces

### Phase 4: Optimization (Week 7-8)
- [ ] Type inference
- [ ] Constant folding
- [ ] Inline expansion
- [ ] Tail call optimization
- [ ] Specialized method generation

### Phase 5: Testing and Tuning (Week 9-10)
- [ ] Comprehensive test suite
- [ ] Performance benchmarking
- [ ] Memory profiling
- [ ] Comparison with CekMachine
- [ ] Documentation

## Testing Strategy

### Unit Tests
- Test each bytecode generation case independently
- Verify correct JVM bytecode is generated
- Test with simple UPLC terms

### Integration Tests
- Test against full UPLC programs
- Compare results with CekMachine
- Verify budget calculations match

### Performance Tests
- Benchmark against CekMachine
- Measure compilation time
- Profile memory usage
- Test with real Plutus scripts

### Property-Based Tests
- Generate random UPLC terms
- Verify JIT and CekMachine produce same results
- Test edge cases

## Success Criteria

1. **Correctness:** 100% test pass rate, matches CekMachine exactly
2. **Performance:** 5x faster than current JIT (210μs → 40μs on auction_1-1)
3. **Memory:** 3x less allocation (547KB → 180KB per operation)
4. **Completeness:** All UPLC features supported
5. **Stability:** No crashes, proper error handling

## References

- ASM Documentation: https://asm.ow2.io/asm4-guide.pdf
- JVM Specification: https://docs.oracle.com/javase/specs/jvms/se17/html/
- Current JIT: `scalus-uplc-jit-compiler/src/main/scala/scalus/uplc/eval/JIT.scala`
- CEK Machine: `scalus-core/shared/src/main/scala/scalus/uplc/eval/Cek.scala`
