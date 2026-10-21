CPU samples: 1381; allocation sampled: 72.4 GiB; GC: 204 collections, 516 ms of pauses

### CPU by test suite (outermost suite frame)

| share | item |
|---|---|
|  22.4 % | `scalus.uplc.transform.EtaReduceCekConformanceTest` |
|   6.6 % | `scalus.cardano.onchain.plutus.prelude.ListTest` |
|   6.1 % | `scalus.cardano.onchain.plutus.v1.ValueTest` |
|   6.0 % | `scalus.testing.regression.cosmex20251107.CompileCosmexFullTest` |
|   5.4 % | `scalus.testing.regression.hydrozoa20250804.SameCborTest` |
|   4.8 % | `scalus.uplc.eval.ExprSizeAndBudgetTest` |
|   3.3 % | `scalus.cardano.onchain.plutus.prelude.SortedMapTest` |
|   3.2 % | `scalus.cardano.onchain.plutus.v3.TxInfoTest` |
|   2.9 % | `scalus.uplc.transform.CommonSubexpressionEliminationTest` |
|   2.3 % | `(no test frame: harness, JIT, GC threads)` |
|   2.2 % | `scalus.compiler.plugin.CustomUnapplyErrorTest` |
|   1.7 % | `scalus.testing.regression.ParseScriptInfoOldBackendTest` |
|   1.7 % | `scalus.compiler.sir.lowering.ValueIntrinsicsLoweringTest` |
|   1.4 % | `scalus.uplc.test.ArbitraryInstancesBoundsTest` |
|   1.4 % | `scalus.uplc.transform.LetChainRegroupCekConformanceTest` |
|   1.3 % | `scalus.compiler.plugin.AlternativeAliasErrorTest` |
|   1.3 % | `scalus.uplc.eval.PlutusConformanceTest` |
|   1.1 % | `scalus.cardano.onchain.plutus.v1.IntervalTest` |
|   0.9 % | `scalus.cardano.onchain.plutus.crypto.accumulator.G2AccumulatorTest` |
|   0.9 % | `scalus.cardano.onchain.plutus.prelude.MathTest` |
|   0.8 % | `scalus.compiler.sir.lowering.SirToUplcSmLoweringTest` |
|   0.8 % | `scalus.compiler.sir.lowering.EqOpsLoweringTest` |
|   0.8 % | `scalus.crypto.trie.FusedMerklePatriciaForestryBenchmarkTest` |
|   0.7 % | `scalus.compiler.sir.lowering.UplcConstrTest` |
|   0.7 % | `scalus.cardano.onchain.plutus.prelude.OptionTest` |

### CPU inclusive by scalus.* method

| share | item |
|---|---|
|  46.4 % | `scalus.compiler.sir.lowering.UplcPipeline$.run` |
|  25.4 % | `scalus.uplc.CompiledPlutus.program` |
|  25.4 % | `scalus.uplc.CompiledPlutus.toUplc` |
|  25.4 % | `scalus.uplc.CompiledPlutus.program$lzyINIT1` |
|  23.8 % | `scalus.compiler.sir.lowering.SirToUplcV3Lowering.lower` |
|  22.4 % | `scalus.uplc.transform.V3Optimizer.apply` |
|  22.2 % | `scalus.compiler.sir.lowering.Lowering$.lowerSIR` |
|  22.0 % | `scalus.compiler.sir.lowering.LoweredValue$Builder$.lvLamAbs` |
|  21.6 % | `scalus.compiler.sir.lowering.Lowering$.loweringBinding` |
|  21.4 % | `scalus.compiler.sir.lowering.Lowering$.anonfun (lambda)` |
|  21.4 % | `scalus.compiler.sir.lowering.LoweringContext.lower` |
|  21.3 % | `scalus.compiler.sir.lowering.LoweringContext.withFunction` |
|  21.1 % | `scalus.package$.toUplc` |
|  20.7 % | `scalus.compiler.sir.lowering.Lowering$.lowerLet` |
|  20.2 % | `scalus.compiler.sir.lowering.SirToUplcV3Lowering.newLoweringContext` |
|  20.1 % | `scalus.compiler.sir.lowering.ScalusRuntime$.initContext` |
|  19.3 % | `scalus.compiler.sir.lowering.ScalusRuntime$.initSupportBindings$$anonfun$1$$anonfun$1` |
|  19.3 % | `scalus.compiler.sir.lowering.ScalusRuntime$.initSupportBindings` |
|  19.3 % | `scalus.compiler.sir.lowering.ScalusRuntime$.initSupportBindings$$anonfun$1` |
|  18.9 % | `scalus.compiler.sir.lowering.ScalusRuntime$.1 (lambda)` |
|  14.2 % | `scalus.compiler.sir.lowering.Lowering$.lowerApp` |
|  14.0 % | `scalus.compiler.sir.lowering.Lowering$.lowerNormalApp` |
|  14.0 % | `scalus.compiler.sir.lowering.Lowering$.15 (lambda)` |
|  13.3 % | `scalus.compiler.sir.lowering.LoweredValue$Builder$.lvApply` |
|  12.9 % | `scalus.package$.toUplcOptimized` |

### CPU self by first scalus.* frame

| share | item |
|---|---|
|  13.7 % | `scalus.uplc.transform.EtaReduceCekConformanceTest.genConst` |
|   6.6 % | `scalus.uplc.transform.TermAnalysis$.freeVars` |
|   5.4 % | `scalus.uplc.TermPrinter$.prettyTermWithDepth` |
|   4.8 % | `scalus.uplc.transform.EtaReduceCekConformanceTest.genTerm` |
|   4.4 % | `scalus.compiler.plugin.SnippetCompilation.compileSnippetWithOutput` |
|   3.6 % | `scalus.uplc.transform.CommonSubexpressionElimination$.scalus$uplc$transform$CommonSubexpressionElimination$$$isAncestorOrSelf` |
|   2.3 % | `scalus.uplc.test.ArbitraryInstances.sizedTermGen$1` |
|   1.9 % | `scalus.compiler.sir.SIRType$.advance$1` |
|   1.9 % | `scalus.uplc.Term.$tilde$eq$tilde` |
|   1.9 % | `scalus.uplc.eval.TallyingBudgetSpenderLogger.spendBudget` |
|   1.9 % | `scalus.uplc.transform.EtaReduceCekConformanceTest.genCaseConstrWrapperLet` |
|   1.4 % | `scalus.uplc.test.ArbitraryInstances.sizedTree$2` |
|   1.2 % | `scalus.uplc.transform.EtaReduceCekConformanceTest.genPlainWrapperLet` |
|   1.2 % | `scalus.compiler.sir.SIRUnify$.scalus$compiler$sir$SIRUnify$$$unifyType` |
|   0.9 % | `scalus.compiler.sir.lowering.SumCaseClassRepresentation$SumUplcConstr.stableKeyInternal` |
|   0.9 % | `scalus.compiler.plugin.Plugin$.retrieveCompilerOptions` |
|   0.9 % | `scalus.compiler.plugin.SIRCompiler.<init>` |
|   0.9 % | `scalus.uplc.transform.CommonSubexpressionElimination$TermKey.equals` |
|   0.8 % | `scalus.compiler.sir.SIRType$.partitionGround` |
|   0.8 % | `scalus.compiler.sir.lowering.ComplexLoweredValue.$init$$$anonfun$2` |
|   0.8 % | `scalus.uplc.transform.CommonSubexpressionElimination.add$1` |
|   0.8 % | `scalus.uplc.eval.CekMachine.spendBudget` |
|   0.7 % | `scalus.uplc.Term.show` |
|   0.7 % | `scalus.compiler.sir.SIRUnify$.unifyTypeNoRec` |
|   0.7 % | `scalus.compiler.sir.lowering.TypeVarRepresentation.toString` |

### CPU self by top frame

| share | item |
|---|---|
|   5.7 % | `scala.collection.Iterator$$anon$6.hasNext` |
|   4.9 % | `scala.runtime.BoxesRunTime.equals2` |
|   2.5 % | `scala.runtime.Statics.anyHash` |
|   2.4 % | `scala.collection.immutable.RedBlackTree$MapHelper.mutableUpd` |
|   2.2 % | `scala.collection.LinearSeqOps.foldLeft` |
|   2.1 % | `scala.collection.mutable.Growable.addAll` |
|   2.0 % | `org.scalacheck.Gen.<init>` |
|   1.8 % | `scalus.uplc.Term.$tilde$eq$tilde` |
|   1.6 % | `scala.collection.immutable.List.$colon$colon` |
|   1.3 % | `org.typelevel.paiges.Doc.loop$5` |
|   1.3 % | `scala.collection.SeqOps.$plus$colon` |
|   1.2 % | `scala.collection.immutable.SetBuilderImpl.addOne` |
|   1.2 % | `scala.collection.mutable.HashMap.put0` |
|   0.9 % | `scala.collection.SetOps.concat` |
|   0.9 % | `scala.runtime.ScalaRunTime$.array_update` |
|   0.9 % | `scala.collection.immutable.RedBlackTree$Helper.mutableBalanceRight` |
|   0.9 % | `scala.collection.immutable.ArraySeq.appendedAll` |
|   0.9 % | `scala.collection.IterableOnceOps.addString` |
|   0.9 % | `scalus.uplc.transform.CommonSubexpressionElimination$TermKey.equals` |
|   0.9 % | `scala.collection.View$Concat.knownSize` |
|   0.8 % | `scalus.uplc.eval.CekMachine.spendBudget` |
|   0.7 % | `scala.collection.Iterator$ConcatIterator.hasNext` |
|   0.7 % | `scalus.compiler.sir.lowering.ComplexLoweredValue.$init$$$anonfun$2` |
|   0.7 % | `java.lang.invoke.DirectMethodHandle.allocateInstance` |
|   0.7 % | `java.util.Objects.equals` |

### Allocation by class

| share | item |
|---|---|
|  17.0 % | `java.lang.Object[]` |
|   9.0 % | `byte[]` |
|   7.9 % | `scala.collection.immutable.$colon$colon` |
|   6.7 % | `scala.Tuple2` |
|   5.8 % | `long[]` |
|   5.2 % | `int[]` |
|   5.1 % | `org.scalacheck.Gen$$anon$7` |
|   3.7 % | `org.scalacheck.Gen$RetryUntilException$` |
|   2.4 % | `scala.collection.immutable.RedBlackTree$Tree` |
|   2.0 % | `java.lang.String` |
|   2.0 % | `short[]` |
|   1.9 % | `scala.collection.immutable.ArraySeq$ofRef` |
|   1.4 % | `scala.Some` |
|   1.2 % | `scala.collection.immutable.NewVectorIterator` |
|   1.1 % | `scalus.compiler.sir.SIRUnify$Env` |
|   1.1 % | `org.typelevel.paiges.Doc$Concat` |
|   1.1 % | `scala.collection.immutable.VectorBuilder` |
|   1.1 % | `scala.collection.immutable.LazyList` |
|   1.1 % | `scala.collection.Iterator$$anon$6` |
|   1.0 % | `scalus.compiler.sir.lowering.typegens.DataConstrEmitter$$$Lambda.0x00007e00013d6d50` |
|   0.9 % | `org.scalacheck.Gen$$Lambda.0x00007e00013109a0` |
|   0.7 % | `scala.collection.immutable.ArraySeq$ofBoolean` |
|   0.6 % | `org.scalacheck.Gen$Choose$$anon$10$$Lambda.0x00007e000130c000` |
|   0.6 % | `scala.collection.ArrayOps$ArrayIterator` |
|   0.5 % | `scala.collection.Iterator$$anon$9` |

### Allocation by first scalus.* frame

| share | item |
|---|---|
|  15.8 % | `scalus.uplc.transform.EtaReduceCekConformanceTest.genConst` |
|  13.7 % | `scalus.compiler.sir.lowering.LoweredValue.$init$` |
|   7.9 % | `scalus.uplc.TermPrinter$.prettyTermWithDepth` |
|   4.6 % | `scalus.compiler.sir.lowering.SumCaseClassRepresentation$SumUplcConstr.stableKeyInternal` |
|   4.2 % | `scalus.uplc.transform.EtaReduceCekConformanceTest.genTerm` |
|   3.0 % | `scalus.uplc.test.ArbitraryInstances.sizedTree$2` |
|   2.8 % | `scalus.uplc.transform.TermAnalysis$.freeVars` |
|   2.7 % | `scalus.compiler.sir.lowering.SumCaseClassRepresentation$.scalus$compiler$sir$lowering$SumCaseClassRepresentation$SumUplcConstr$$_$stableKeyInternal$$anonfun$2` |
|   2.6 % | `scalus.compiler.sir.SIRType$.partitionGround` |
|   2.4 % | `scalus.compiler.sir.SIRType$.advance$1` |
|   2.2 % | `scalus.compiler.sir.lowering.ProductCaseClassRepresentation$ProdUplcConstr.stableKeyInternal` |
|   1.7 % | `scalus.uplc.transform.EtaReduceCekConformanceTest.genCaseConstrWrapperLet` |
|   1.7 % | `scalus.uplc.test.ArbitraryInstances.sizedTermGen$1` |
|   1.5 % | `scalus.uplc.test.ArbitraryInstances.listGen$2` |
|   1.2 % | `scalus.compiler.sir.SIRUnify$.scalus$compiler$sir$SIRUnify$$$unifyType` |
|   1.1 % | `scalus.compiler.sir.lowering.typegens.DataConstrEmitter$.prepareCases` |
|   1.0 % | `scalus.compiler.sir.SIRUnify$Env.copy` |
|   1.0 % | `scalus.compiler.sir.SIRType$SetBasedTypeVarGenerationContext.importSetFromType` |
|   1.0 % | `scalus.compiler.sir.lowering.LocalScope.add` |
|   1.0 % | `scalus.uplc.transform.EtaReduceCekConformanceTest.genPlainWrapperLet` |
|   0.9 % | `scalus.compiler.plugin.SnippetCompilation.compileSnippetWithOutput` |
|   0.9 % | `scalus.uplc.eval.TallyingBudgetSpenderLogger.spendBudget` |
|   0.8 % | `scalus.compiler.sir.SIRType$.isPolyFunOrFun$default$2` |
|   0.8 % | `scalus.uplc.Term.show` |
|   0.8 % | `scalus.compiler.sir.lowering.TypeVarRepresentation.toString` |

