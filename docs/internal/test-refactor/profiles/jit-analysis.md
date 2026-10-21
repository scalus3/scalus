CPU samples: 723; allocation sampled: 9.4 GiB; GC: 89 collections, 337 ms of pauses

### CPU by test suite (outermost suite frame)

| share | item |
|---|---|
|  87.1 % | `(no test frame: harness, JIT, GC threads)` |
|   7.9 % | `scalus.uplc.jit.JitConformanceTest` |
|   1.5 % | `scalus.uplc.jit.JitCorpusTest` |
|   1.4 % | `scalus.uplc.jit.JitShapeTest` |
|   1.1 % | `scalus.uplc.jit.UplcCompilerTest` |
|   0.7 % | `scalus.uplc.jit.JitConcurrentCompileTest` |
|   0.3 % | `scalus.uplc.jit.JITTest` |

### CPU inclusive by scalus.* method

| share | item |
|---|---|
|  45.2 % | `scalus.uplc.jit.StagingCompilerThread$.onThread` |
|  45.1 % | `scalus.uplc.jit.StagingCompilerThread$.run` |
|  45.1 % | `scalus.uplc.jit.ConstantPool$.compile` |
|  45.1 % | `scalus.uplc.jit.StagingCompilerThread$.1 (lambda)` |
|  45.1 % | `scalus.uplc.jit.StagingCompilerThread$.run$$anonfun$1` |
|  45.1 % | `scalus.uplc.jit.StagingCompilerThread$.compile` |
|  45.1 % | `scalus.uplc.jit.nativestack.JIT$.jitUplc$$anonfun$1` |
|   6.6 % | `scalus.uplc.jit.nativestack.JIT$.genCode$1` |
|   6.5 % | `scalus.uplc.jit.nativestack.JIT$.genCodeFromTerm` |
|   6.5 % | `scalus.uplc.jit.nativestack.JIT$.jitUplc$$anonfun$1$$anonfun$1` |
|   4.8 % | `scalus.uplc.jit.nativestack.JIT$.gen$1` |
|   4.4 % | `scalus.uplc.Program$.parseUplc` |
|   4.3 % | `scalus.uplc.jit.nativestack.JIT$.genCodeFromTerm$$anonfun$1` |
|   4.3 % | `scalus.uplc.jit.nativestack.JIT$.genCodeFromTerm$$anonfun$adapted$1` |
|   4.1 % | `scalus.uplc.jit.nativestack.JIT$.genCodeFromTerm$$anonfun$1$$anonfun$1` |
|   3.6 % | `scalus.uplc.jit.BuiltinEmitter$.emitApplied` |
|   3.6 % | `scalus.uplc.UplcParser.parseProgram` |
|   3.3 % | `scalus.uplc.jit.BuiltinEmitter$.emitApplied$$anonfun$adapted$1` |
|   3.3 % | `scalus.uplc.jit.BuiltinEmitter$.bind$1$$anonfun$adapted$1` |
|   3.3 % | `scalus.uplc.jit.BuiltinEmitter$.emitApplied$$anonfun$1` |
|   3.3 % | `scalus.uplc.jit.BuiltinEmitter$.bind$1` |
|   3.3 % | `scalus.uplc.jit.BuiltinEmitter$.bind$1$$anonfun$1` |
|   3.2 % | `scalus.uplc.jit.nativestack.JIT$.genCode$1$$anonfun$adapted$4` |
|   3.2 % | `scalus.uplc.jit.nativestack.JIT$.genCode$1$$anonfun$5` |
|   3.0 % | `scalus.uplc.jit.nativestack.JIT$.genCode$1$$anonfun$adapted$3` |

### CPU self by first scalus.* frame

| share | item |
|---|---|
|  38.6 % | `scalus.uplc.jit.StagingCompilerThread$.compile` |
|   2.6 % | `scalus.uplc.jit.nativestack.JIT$.genCode$1` |
|   2.4 % | `scalus.uplc.jit.nativestack.JIT$.genCodeFromTerm` |
|   1.7 % | `scalus.uplc.UplcParser$.$init$$$anonfun$9` |
|   1.4 % | `scalus.uplc.builtin.IntegerToByteString$.goLELimit` |
|   1.0 % | `scalus.uplc.jit.BuiltinEmitter$.callBridge` |
|   0.8 % | `scalus.uplc.UplcParser.parseProgram` |
|   0.7 % | `scalus.uplc.jit.BuiltinEmitter$.bind$1` |
|   0.7 % | `scalus.uplc.jit.BuiltinEmitter$.callImpl` |
|   0.7 % | `scalus.uplc.jit.GenCtx$.spendStep` |
|   0.6 % | `scalus.uplc.UplcParser$.constantOf` |
|   0.6 % | `scalus.uplc.eval.TallyingBudgetSpender.spendBudget` |
|   0.4 % | `scalus.uplc.eval.CekMachine.loop$1` |
|   0.4 % | `scalus.uplc.eval.CekMachine.returnCek` |
|   0.4 % | `scalus.uplc.jit.JitCekDiff$.2 (lambda)` |
|   0.3 % | `scalus.uplc.jit.BuiltinEmitter$.emitApplied` |
|   0.3 % | `scalus.uplc.UplcParser$.<clinit>` |
|   0.3 % | `scalus.uplc.UplcParser$.$init$$$anonfun$13` |
|   0.3 % | `scalus.uplc.builtin.JVMPlatformSpecific.blake2b_224` |
|   0.3 % | `scalus.uplc.UplcParser.$init$$$anonfun$2` |
|   0.3 % | `scalus.uplc.jit.ConstantPool.read` |
|   0.3 % | `scalus.uplc.builtin.IntegerToByteString$.goLENoLimit` |
|   0.3 % | `scalus.uplc.Constant$Integer.toString` |
|   0.3 % | `scalus.uplc.DeBruijn$.go$1` |
|   0.1 % | `scalus.compiler.sir.linking.SIRLinker$.nest` |

### CPU self by top frame

| share | item |
|---|---|
|   4.8 % | `dotty.tools.dotc.ast.Positioned.include$1` |
|   3.0 % | `java.util.Objects.equals` |
|   2.1 % | `java.util.IdentityHashMap.get` |
|   2.1 % | `dotty.tools.dotc.inlines.Inlines$Reposition$1.transform` |
|   2.1 % | `dotty.tools.dotc.core.SymDenotations$SymDenotation.isEmptyPackage` |
|   1.9 % | `dotty.tools.dotc.ast.Trees$Instance$TreeMap.transform` |
|   1.8 % | `dotty.tools.dotc.core.Symbols$Symbol.computeDenot` |
|   1.8 % | `dotty.tools.dotc.util.Spans$Span$.start$extension` |
|   1.7 % | `java.math.BigInteger.destructiveMulAdd` |
|   1.4 % | `dotty.tools.dotc.core.SymDenotations$SymDenotation.isTopLevelClass` |
|   1.4 % | `dotty.tools.dotc.ast.Positioned.withSpan` |
|   1.2 % | `dotty.tools.dotc.ast.Positioned.cloneIn` |
|   1.2 % | `dotty.tools.dotc.inlines.Inlines$Reposition$1.fixSpan$1` |
|   1.1 % | `dotty.tools.dotc.core.Types$Type.isError` |
|   1.1 % | `scala.runtime.BoxesRunTime.equals2` |
|   1.1 % | `dotty.tools.dotc.ast.Trees$.recur$1` |
|   1.1 % | `dotty.tools.dotc.ast.Trees$Instance$TreeMap.transform$$anonfun$1` |
|   1.0 % | `dotty.tools.dotc.core.Symbols$Symbol.denot` |
|   1.0 % | `java.math.BigInteger.getInt` |
|   0.8 % | `scala.collection.IterableOnceOps.foreach` |
|   0.8 % | `dotty.tools.dotc.ast.Positioned.envelope` |
|   0.8 % | `dotty.tools.dotc.core.SymDenotations$SymDenotation$$anon$2.hasNext` |
|   0.8 % | `dotty.tools.dotc.ast.Trees$Instance.transformCtx` |
|   0.7 % | `dotty.tools.dotc.core.Contexts$Context.lookup` |
|   0.7 % | `java.lang.Class.copyMethods` |

### Allocation by class

| share | item |
|---|---|
|  10.5 % | `int[]` |
|  10.0 % | `dotty.tools.dotc.ast.Trees$Ident` |
|   9.9 % | `scala.collection.immutable.$colon$colon` |
|   9.7 % | `byte[]` |
|   7.4 % | `dotty.tools.dotc.core.Contexts$FreshContext` |
|   6.7 % | `dotty.tools.dotc.util.WeakHashSet$Entry[]` |
|   4.2 % | `java.lang.Object[]` |
|   3.8 % | `dotty.tools.dotc.ast.Trees$ValDef` |
|   3.5 % | `dotty.tools.dotc.ast.Trees$Select` |
|   3.4 % | `dotty.tools.dotc.ast.Trees$Apply` |
|   2.5 % | `dotty.tools.dotc.ast.Trees$Block` |
|   2.4 % | `dotty.tools.dotc.ast.Trees$TypeTree` |
|   1.8 % | `dotty.tools.dotc.ast.Trees$TypeApply` |
|   1.5 % | `java.lang.reflect.Method` |
|   1.4 % | `java.lang.String` |
|   1.3 % | `dotty.tools.dotc.core.Denotations$MultiDenotation` |
|   1.1 % | `scala.collection.immutable.BitmapIndexedMapNode` |
|   1.0 % | `scala.Tuple2` |
|   0.7 % | `java.lang.StringBuilder` |
|   0.5 % | `dotty.tools.dotc.util.Property$Key` |
|   0.5 % | `dotty.tools.dotc.ast.Trees$Bind` |
|   0.5 % | `char[]` |
|   0.5 % | `dotty.tools.backend.jvm.LabelNode1` |
|   0.5 % | `dotty.tools.dotc.ast.Trees$Return` |
|   0.5 % | `dotty.tools.dotc.ast.Trees$DefDef` |

### Allocation by first scalus.* frame

| share | item |
|---|---|
|  43.0 % | `(non-scalus)` |
|  32.5 % | `scalus.uplc.jit.StagingCompilerThread$.compile` |
|   7.4 % | `scalus.uplc.builtin.IntegerToByteString$.goLELimit` |
|   2.2 % | `scalus.uplc.jit.nativestack.JIT$.genCode$1` |
|   1.7 % | `scalus.uplc.eval.CekMachine.applyEvaluate` |
|   1.6 % | `scalus.uplc.jit.nativestack.JIT$.genCodeFromTerm` |
|   1.2 % | `scalus.uplc.jit.JitCekDiff$.2 (lambda)` |
|   0.7 % | `scalus.uplc.builtin.IntegerToByteString$.goLENoLimit` |
|   0.7 % | `scalus.uplc.UplcParser.$init$$$anonfun$2` |
|   0.6 % | `scalus.uplc.UplcParser.parseProgram` |
|   0.5 % | `scalus.uplc.jit.ConstantPool$.compile` |
|   0.5 % | `scalus.uplc.jit.BuiltinEmitter$.bind$1` |
|   0.4 % | `scalus.uplc.UplcParser$.<clinit>` |
|   0.4 % | `scalus.uplc.jit.BuiltinEmitter$.emitApplied` |
|   0.3 % | `scalus.uplc.jit.GenCtx$.spendStep` |
|   0.3 % | `scalus.compiler.sir.lowering.LoweredValue.$init$` |
|   0.3 % | `scalus.uplc.Term$LamAbs$.apply` |
|   0.3 % | `scalus.uplc.Term$Delay$.apply` |
|   0.3 % | `scalus.uplc.builtin.IntegerToByteString$.integerToByteString` |
|   0.2 % | `scalus.uplc.Constant$Integer.toString` |
|   0.2 % | `scalus.uplc.UplcParser$.$init$$$anonfun$13` |
|   0.2 % | `scalus.uplc.jit.BuiltinEmitter$.callBridge` |
|   0.2 % | `scalus.uplc.builtin.JVMPlatformSpecific.blake2b_224` |
|   0.2 % | `scalus.uplc.jit.JitCekDiff$.runJit` |
|   0.2 % | `scalus.cardano.ledger.ExUnits$.apply` |

