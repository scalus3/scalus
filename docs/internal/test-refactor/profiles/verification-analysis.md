CPU samples: 71; allocation sampled: 2.9 GiB; GC: 30 collections, 36 ms of pauses

### CPU by test suite (outermost suite frame)

| share | item |
|---|---|
|  43.7 % | `scalus.verify.uplcblaster.UplcBlasterTest` |
|  25.4 % | `scalus.verify.uplcblaster.PreludeProofsTest` |
|  11.3 % | `scalus.verify.examples.BigIntVerificationTest` |
|   9.9 % | `(no test frame: harness, JIT, GC threads)` |
|   2.8 % | `scalus.verify.uplcblaster.ObligationsTest` |
|   2.8 % | `scalus.verify.uplcblaster.SpecificationsTest` |
|   1.4 % | `scalus.verify.lean.LeanWorkspaceToolTest` |
|   1.4 % | `scalus.verify.PropTest` |
|   1.4 % | `scalus.verify.uplcblaster.LeanProofsTest` |

### CPU inclusive by scalus.* method

| share | item |
|---|---|
|  76.1 % | `scalus.verify.uplcblaster.UplcBlaster$.loop$2` |
|  76.1 % | `scalus.verify.uplcblaster.UplcBlaster$.lower` |
|  76.1 % | `scalus.verify.uplcblaster.UplcBlaster$.lower$$anonfun$1` |
|  69.0 % | `scalus.verify.uplcblaster.UplcBlaster$.compile$1` |
|  67.6 % | `scalus.verify.uplcblaster.UplcBlaster$.compileSirFunction` |
|  67.6 % | `scalus.verify.uplcblaster.UplcBlaster$.loop$2$$anonfun$2` |
|  67.6 % | `scalus.compiler.sir.lowering.UplcPipeline$.run` |
|  62.0 % | `scalus.package$.toUplc` |
|  53.5 % | `scalus.compiler.sir.lowering.ScalusRuntime$.initContext` |
|  53.5 % | `scalus.compiler.sir.lowering.LoweredValue$Builder$.lvLamAbs` |
|  53.5 % | `scalus.verify.Verifier.verify` |
|  53.5 % | `scalus.compiler.sir.lowering.SirToUplcV3Lowering.newLoweringContext` |
|  53.5 % | `scalus.verify.Verifier.prove` |
|  53.5 % | `scalus.compiler.sir.lowering.SirToUplcV3Lowering.lower` |
|  52.1 % | `scalus.compiler.sir.lowering.ScalusRuntime$.initSupportBindings$$anonfun$1` |
|  52.1 % | `scalus.compiler.sir.lowering.Lowering$.lowerLet` |
|  52.1 % | `scalus.compiler.sir.lowering.Lowering$.lowerSIR` |
|  52.1 % | `scalus.verify.Verifier.prepare` |
|  52.1 % | `scalus.compiler.sir.lowering.Lowering$.loweringBinding` |
|  52.1 % | `scalus.compiler.sir.lowering.LoweringContext.lower` |
|  52.1 % | `scalus.verify.uplcblaster.UplcBlaster.prepare` |
|  52.1 % | `scalus.compiler.sir.lowering.LoweringContext.withFunction` |
|  52.1 % | `scalus.compiler.sir.lowering.Lowering$.anonfun (lambda)` |
|  52.1 % | `scalus.compiler.sir.lowering.ScalusRuntime$.initSupportBindings` |
|  52.1 % | `scalus.compiler.sir.lowering.ScalusRuntime$.1 (lambda)` |

### CPU self by first scalus.* frame

| share | item |
|---|---|
|   5.6 % | `scalus.compiler.sir.lowering.SumCaseClassRepresentation$SumUplcConstr.stableKeyInternal` |
|   4.2 % | `scalus.compiler.sir.lowering.Lowering$.lowerSIR` |
|   2.8 % | `scalus.compiler.sir.SIRType$.partitionGround` |
|   2.8 % | `scalus.compiler.sir.lowering.ProductCaseClassRepresentation$ProdUplcConstr.stableKeyInternal` |
|   2.8 % | `scalus.compiler.sir.lowering.Lowering$.filterAndCountVars` |
|   2.8 % | `scalus.compiler.sir.SIRUnify$.unifyTypeNoRec` |
|   2.8 % | `scalus.compiler.sir.SIRUnify$.scalus$compiler$sir$SIRUnify$$$unifyType` |
|   1.4 % | `scalus.verify.examples.BigIntVerificationTest.<init>` |
|   1.4 % | `scalus.compiler.sir.lowering.VariableLoweredValue.toRepresentation` |
|   1.4 % | `scalus.cardano.ledger.NonNegativeInterval.toBigDecimal` |
|   1.4 % | `scalus.uplc.Term.fillEmptyAnnotationsBottomUp` |
|   1.4 % | `scalus.compiler.sir.SIRUnify$.12 (lambda)` |
|   1.4 % | `scalus.verify.lean.LeanWorkspaceTool$.write` |
|   1.4 % | `scalus.verify.uplcblaster.ObligationsTest.org$scalatest$BeforeAndAfterAll$$super$run` |
|   1.4 % | `scalus.verify.Verifier.declareObligations$$anonfun$2$$anonfun$2` |
|   1.4 % | `scalus.verify.uplcblaster.UplcBlasterTest.<init>` |
|   1.4 % | `scalus.compiler.sir.lowering.SumDispatch$.toRepresentation` |
|   1.4 % | `scalus.compiler.sir.lowering.TypeVarRepresentation.toString` |
|   1.4 % | `scalus.uplc.DeBruijnedProgram$given_Flat_DeBruijnedProgram$.bitSize` |
|   1.4 % | `scalus.compiler.sir.lowering.LocalScope.add` |
|   1.4 % | `scalus.compiler.sir.lowering.LoweredValue$Builder$.lvApply` |
|   1.4 % | `scalus.compiler.sir.DataDecl.hashCode` |
|   1.4 % | `scalus.compiler.sir.lowering.ApplyLoweredValue.cUsedVars` |
|   1.4 % | `scalus.verify.uplcblaster.UplcBlaster$.scalus$verify$uplcblaster$UplcBlaster$$$freeVariables` |
|   1.4 % | `scalus.compiler.sir.SIRType$Annotated.hashCode` |

### CPU self by top frame

| share | item |
|---|---|
|   7.0 % | `scala.runtime.Statics.anyHash` |
|   4.2 % | `scalus.compiler.sir.lowering.Lowering$.lowerSIR` |
|   2.8 % | `scalus.compiler.sir.SIRUnify$.unifyTypeNoRec` |
|   1.4 % | `java.lang.Class.copyMethods` |
|   1.4 % | `java.util.HashMap.get` |
|   1.4 % | `jdk.internal.loader.URLClassPath$JarLoader$1.<init>` |
|   1.4 % | `java.util.Properties$LineReader.readLine` |
|   1.4 % | `scala.collection.mutable.Stack$.from` |
|   1.4 % | `scalus.compiler.sir.lowering.VariableLoweredValue.toRepresentation` |
|   1.4 % | `scala.collection.mutable.Stack$.empty` |
|   1.4 % | `scala.math.Ordering$Int$.on` |
|   1.4 % | `java.math.BigInteger.add` |
|   1.4 % | `java.lang.invoke.MethodType.checkPtypes` |
|   1.4 % | `java.lang.invoke.InvokerBytecodeGenerator.methodDesc` |
|   1.4 % | `scalus.compiler.sir.SIRUnify$.12 (lambda)` |
|   1.4 % | `sun.security.jca.GetInstance.getInstance` |
|   1.4 % | `java.nio.file.Path.resolveSibling` |
|   1.4 % | `org.scalatest.Suite.run` |
|   1.4 % | `java.lang.invoke.MethodHandles.insertArgumentsChecks` |
|   1.4 % | `scala.collection.mutable.HashMap$.apply` |
|   1.4 % | `scala.collection.IterableOnceOps.exists` |
|   1.4 % | `scalus.compiler.sir.lowering.SumDispatch$.toRepresentation` |
|   1.4 % | `scala.runtime.ScalaRunTime$._toString` |
|   1.4 % | `scalus.uplc.DeBruijnedProgram$given_Flat_DeBruijnedProgram$.bitSize` |
|   1.4 % | `java.lang.StringLatin1.hashCode` |

### Allocation by class

| share | item |
|---|---|
|  22.3 % | `java.lang.Object[]` |
|  16.5 % | `byte[]` |
|  11.5 % | `int[]` |
|   8.1 % | `long[]` |
|   7.2 % | `scala.collection.immutable.$colon$colon` |
|   4.1 % | `scalus.compiler.sir.SIRUnify$Env` |
|   3.2 % | `scalus.compiler.sir.lowering.typegens.SirTypeUplcGenerator$$$Lambda.0x00007fe0012ec5f8` |
|   3.1 % | `java.lang.String` |
|   2.9 % | `scala.collection.immutable.RedBlackTree$Tree` |
|   2.9 % | `scala.Tuple2` |
|   2.2 % | `short[]` |
|   1.1 % | `scala.collection.immutable.LazyList` |
|   1.0 % | `scala.collection.mutable.HashSet$Node[]` |
|   0.8 % | `scala.collection.immutable.Map$Map1` |
|   0.7 % | `scala.collection.immutable.LazyList$$Lambda.0x00007fe0012b5ca0` |
|   0.7 % | `scala.collection.immutable.Map$Map2` |
|   0.6 % | `scala.collection.immutable.BitmapIndexedMapNode` |
|   0.5 % | `scalus.compiler.sir.SIRType[]` |
|   0.4 % | `scala.collection.immutable.LazyList$$$Lambda.0x00007fe0012b5430` |
|   0.4 % | `scalus.compiler.sir.SIRUnify$UnificationSuccess` |
|   0.4 % | `scala.collection.immutable.Set$Set1` |
|   0.4 % | `scala.collection.mutable.HashSet$Node` |
|   0.4 % | `java.lang.StringBuilder` |
|   0.4 % | `scala.Some` |
|   0.4 % | `scala.collection.mutable.StringBuilder` |

### Allocation by first scalus.* frame

| share | item |
|---|---|
|  34.4 % | `scalus.compiler.sir.lowering.LoweredValue.$init$` |
|   7.9 % | `scalus.compiler.sir.lowering.ProductCaseClassRepresentation$ProdUplcConstr.stableKeyInternal` |
|   7.1 % | `scalus.compiler.sir.lowering.SumCaseClassRepresentation$SumUplcConstr.stableKeyInternal` |
|   5.9 % | `scalus.compiler.sir.SIRType$.advance$1` |
|   3.4 % | `scalus.compiler.sir.lowering.typegens.SumUplcConstrOps$.buildSumUplcConstr` |
|   3.4 % | `scalus.compiler.sir.lowering.typegens.SirTypeUplcGenerator$.containsFun` |
|   3.1 % | `scalus.compiler.sir.SIRUnify$.unifyTypeNoRec` |
|   3.0 % | `scalus.compiler.sir.SIRType$.partitionGround` |
|   2.6 % | `scalus.compiler.sir.SIRUnify$Env$.apply` |
|   2.5 % | `scalus.compiler.sir.lowering.SumCaseClassRepresentation$SumReprProxy.stableKeyInternal` |
|   1.7 % | `(non-scalus)` |
|   1.6 % | `scalus.compiler.sir.lowering.LocalScope.add` |
|   1.5 % | `scalus.compiler.sir.SIRUnify$Env.copy` |
|   1.5 % | `scalus.compiler.sir.SIRUnify$.scalus$compiler$sir$SIRUnify$$$unifyType` |
|   1.4 % | `scalus.serialization.flat.HashConsedRef$$anon$1.<init>` |
|   1.0 % | `scalus.compiler.sir.SIRType$.isPolyFunOrFun$default$2` |
|   0.9 % | `scalus.compiler.sir.SIRType$SetBasedTypeVarGenerationContext.importSetFromType` |
|   0.8 % | `scalus.compiler.sir.lowering.Lowering$.addUsedVarsToCounts$$anonfun$1` |
|   0.7 % | `scalus.compiler.sir.SIRUnify$UnificationFailure.<init>` |
|   0.6 % | `scalus.compiler.sir.lowering.TypeVarRepresentation.toString` |
|   0.6 % | `scalus.compiler.sir.lowering.VariableLoweredValue$.$lessinit$greater$default$7` |
|   0.5 % | `scalus.compiler.sir.lowering.SumCaseClassRepresentation$.scalus$compiler$sir$lowering$SumCaseClassRepresentation$SumUplcConstr$$_$stableKeyInternal$$anonfun$2` |
|   0.5 % | `scalus.compiler.sir.lowering.typegens.SirTypeUplcGenerator$.4 (lambda)` |
|   0.5 % | `scalus.compiler.sir.SIRType$.calculateApplyTypeWithUnifyEnv` |
|   0.4 % | `scalus.compiler.sir.SIRUnify$UnificationSuccess$.apply` |

