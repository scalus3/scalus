CPU samples: 9502; allocation sampled: 226.8 GiB; GC: 624 collections, 1281 ms of pauses

### CPU by test suite (outermost suite frame)

| share | item |
|---|---|
|  45.4 % | `scalus.examples.linkedlist.LinkedListTest` |
|  14.6 % | `scalus.examples.amm.AmmTest` |
|   4.9 % | `scalus.examples.BooleanOptimizerImpactTest` |
|   4.6 % | `scalus.examples.PipelineDeterminismCrossJvmTest` |
|   3.8 % | `scalus.examples.auction.AuctionTestKitTest` |
|   2.6 % | `scalus.benchmarks.KnightsTest` |
|   2.4 % | `scalus.examples.auction.AuctionValidatorTest` |
|   1.7 % | `scalus.examples.decentralizedidentity.DecentralizedIdentityTest` |
|   1.7 % | `scalus.examples.paymentsplitter.PaymentSplitterTxBuilderTest` |
|   1.6 % | `(no test frame: harness, JIT, GC threads)` |
|   1.3 % | `scalus.examples.pricebet.PricebetValidatorTest` |
|   1.2 % | `scalus.examples.cape.fibonacci.FibonacciCapeTest` |
|   1.2 % | `scalus.examples.CceImpactMeasurementTest` |
|   1.0 % | `scalus.examples.crowdfunding.CrowdfundingValidatorTest` |
|   0.9 % | `scalus.examples.crowdfunding.CrowdfundingScenarioTest` |
|   0.9 % | `scalus.examples.lottery.LotteryValidatorTest` |
|   0.8 % | `scalus.examples.storage.StorageTest` |
|   0.8 % | `scalus.examples.CcePipelineTest` |
|   0.8 % | `scalus.examples.editablenft.EditableNftValidatorTest` |
|   0.7 % | `scalus.examples.crowdfunding.CrowdfundingScalaCheckCommandTest` |
|   0.7 % | `scalus.benchmarks.knightsdata.KnightsDataTest` |
|   0.6 % | `scalus.examples.crowdfunding.CrowdfundingEmulatorTest` |
|   0.6 % | `scalus.benchmarks.ClausifyTest` |
|   0.5 % | `scalus.examples.paymentsplitter.PaymentSplitterVerificationTest` |
|   0.4 % | `scalus.examples.auction.CrossInstanceDoubleSatisfactionTest` |

### CPU inclusive by scalus.* method

| share | item |
|---|---|
|  91.9 % | `scalus.compiler.sir.lowering.UplcPipeline$.run` |
|  89.7 % | `scalus.uplc.transform.V3Optimizer.apply` |
|  78.5 % | `scalus.uplc.CompiledPlutus.program$lzyINIT1` |
|  78.5 % | `scalus.uplc.CompiledPlutus.program` |
|  78.5 % | `scalus.uplc.CompiledPlutus.toUplc` |
|  74.5 % | `scalus.uplc.PlutusV3.script$lzyINIT4` |
|  74.5 % | `scalus.uplc.PlutusV3.script` |
|  67.2 % | `scalus.uplc.transform.V3Optimizer.adapted (lambda)` |
|  67.2 % | `scalus.uplc.transform.V3Optimizer.1 (lambda)` |
|  59.7 % | `scalus.uplc.transform.CommonSubexpressionElimination.apply` |
|  57.2 % | `scalus.uplc.transform.CommonSubexpressionElimination.collect` |
|  50.2 % | `scalus.uplc.transform.CommonSubexpressionElimination.go$1` |
|  50.0 % | `scalus.uplc.transform.CommonSubexpressionElimination.go$1$$anonfun$2` |
|  48.2 % | `scalus.uplc.transform.CommonSubexpressionElimination.add$1` |
|  45.5 % | `scalus.examples.linkedlist.LinkedListOffchain.<init>` |
|  45.5 % | `scalus.examples.linkedlist.LinkedListOffchain$.apply` |
|  43.7 % | `scalus.uplc.transform.CommonSubexpressionElimination.go$1$$anonfun$1` |
|  27.9 % | `scalus.uplc.transform.CommonSubexpressionElimination$.scalus$uplc$transform$CommonSubexpressionElimination$$$isAncestorOrSelf` |
|  27.1 % | `scalus.uplc.transform.Inliner.go` |
|  27.1 % | `scalus.uplc.transform.Inliner.apply` |
|  23.3 % | `scalus.uplc.transform.Inliner.tryPartialEval` |
|  22.6 % | `scalus.uplc.transform.TermAnalysis$.freeVars` |
|  15.3 % | `scalus.uplc.transform.CommonSubexpressionElimination$AlphaTermKey.equals` |
|  15.1 % | `scalus.uplc.transform.CommonSubexpressionElimination$TermKey.equals` |
|  15.0 % | `scalus.uplc.transform.CommonSubexpressionElimination.add$1$$anonfun$1` |

### CPU self by first scalus.* frame

| share | item |
|---|---|
|  27.9 % | `scalus.uplc.transform.CommonSubexpressionElimination$.scalus$uplc$transform$CommonSubexpressionElimination$$$isAncestorOrSelf` |
|  22.6 % | `scalus.uplc.transform.TermAnalysis$.freeVars` |
|   9.9 % | `scalus.uplc.Term.$tilde$eq$tilde` |
|   5.2 % | `scalus.uplc.transform.CommonSubexpressionElimination$TermKey.equals` |
|   4.5 % | `scalus.uplc.transform.CommonSubexpressionElimination.add$1` |
|   4.1 % | `scalus.uplc.TermPrinter$.prettyTermWithDepth` |
|   3.6 % | `scalus.uplc.transform.CommonSubexpressionElimination$.scalus$uplc$transform$CommonSubexpressionElimination$$$longestCommonPrefix` |
|   2.3 % | `scalus.uplc.transform.CommonSubexpressionElimination.collect$$anonfun$2$$anonfun$2` |
|   1.4 % | `scalus.uplc.transform.CommonSubexpressionElimination.go$2` |
|   1.4 % | `scalus.uplc.transform.Inliner.analyzeOccurrence` |
|   1.1 % | `scalus.uplc.transform.CommonSubexpressionElimination.go$1` |
|   0.9 % | `scalus.uplc.eval.TallyingBudgetSpenderLogger.spendBudget` |
|   0.7 % | `scalus.uplc.transform.CommonSubexpressionElimination.go$1$$anonfun$2` |
|   0.6 % | `scalus.uplc.transform.CommonSubexpressionElimination.4 (lambda)` |
|   0.5 % | `scalus.uplc.Term.show` |
|   0.5 % | `scalus.uplc.transform.CommonSubexpressionElimination.collect$$anonfun$2` |
|   0.5 % | `scalus.uplc.eval.CekMachine.spendBudget` |
|   0.4 % | `scalus.uplc.transform.CommonSubexpressionElimination.collect` |
|   0.3 % | `scalus.uplc.transform.CommonSubexpressionElimination.keyOf$5` |
|   0.3 % | `scalus.uplc.eval.CekMachine.evalBuiltinApp` |
|   0.3 % | `scalus.uplc.eval.CekMachine.applyEvaluate` |
|   0.2 % | `scalus.compiler.sir.SIRType$.advance$1` |
|   0.2 % | `scalus.uplc.eval.CekMachine.returnCek` |
|   0.2 % | `scalus.uplc.DeBruijn$.process$1` |
|   0.2 % | `scalus.compiler.sir.AnnotationsDecl.hashCode` |

### CPU self by top frame

| share | item |
|---|---|
|  23.6 % | `scala.runtime.BoxesRunTime.equals2` |
|   9.8 % | `scalus.uplc.Term.$tilde$eq$tilde` |
|   5.2 % | `scalus.uplc.transform.CommonSubexpressionElimination$TermKey.equals` |
|   4.6 % | `scala.collection.immutable.SetBuilderImpl.addOne` |
|   4.3 % | `scala.collection.SeqOps.startsWith` |
|   2.8 % | `scala.runtime.Statics.anyHash` |
|   2.7 % | `scalus.uplc.transform.TermAnalysis$.freeVars` |
|   2.4 % | `org.typelevel.paiges.Doc.loop$5` |
|   2.1 % | `scala.collection.SetOps.concat` |
|   1.9 % | `scala.collection.immutable.NewVectorIterator.advanceSlice` |
|   1.8 % | `scala.collection.mutable.LinkedHashMap$LinkedEntry.findEntry` |
|   1.7 % | `scala.collection.View$Concat.knownSize` |
|   1.5 % | `scala.collection.LinearSeqOps.foldLeft` |
|   1.5 % | `scala.collection.mutable.Growable.addAll` |
|   1.4 % | `scalus.uplc.transform.Inliner.analyzeOccurrence` |
|   1.3 % | `scala.collection.immutable.NewVectorIterator.next` |
|   1.2 % | `scala.collection.SeqOps.$colon$plus` |
|   1.2 % | `scala.collection.IterableOnceOps.forall` |
|   1.1 % | `scala.collection.mutable.LinkedHashMap.put0` |
|   1.1 % | `scala.collection.immutable.BitmapIndexedSetNode.updated` |
|   1.0 % | `scala.collection.immutable.Set$.from` |
|   0.9 % | `scala.collection.immutable.Vector.iterator` |
|   0.7 % | `scala.runtime.Statics.anyHashNumber` |
|   0.7 % | `scala.collection.Iterator$ConcatIterator.hasNext` |
|   0.6 % | `scala.collection.IterableOnceOps.forall$` |

### Allocation by class

| share | item |
|---|---|
|  27.7 % | `scala.collection.immutable.NewVectorIterator` |
|  11.2 % | `java.lang.Object[]` |
|  10.4 % | `scala.Tuple2` |
|   8.2 % | `scala.collection.immutable.$colon$colon` |
|   3.9 % | `int[]` |
|   2.9 % | `org.typelevel.paiges.Doc$Concat` |
|   2.1 % | `scala.collection.immutable.SetBuilderImpl` |
|   2.1 % | `long[]` |
|   1.9 % | `byte[]` |
|   1.8 % | `scala.collection.immutable.BitmapIndexedSetNode` |
|   1.4 % | `scala.collection.Iterator$ConcatIterator` |
|   1.4 % | `scala.collection.View$Concat` |
|   1.4 % | `scala.collection.Iterator$$anon$20` |
|   1.4 % | `scala.collection.immutable.Set$Set1` |
|   1.1 % | `scala.collection.immutable.Vector2` |
|   1.0 % | `scala.collection.ArrayOps$ArrayIterator` |
|   0.9 % | `scala.collection.Iterator$ConcatIteratorCell` |
|   0.9 % | `scalus.uplc.transform.CommonSubexpressionElimination$$$Lambda.0x00007e00014427a8` |
|   0.9 % | `java.lang.String[]` |
|   0.8 % | `scala.collection.immutable.ArraySeq$ofRef` |
|   0.8 % | `scala.collection.mutable.CheckedIndexedSeqView$CheckedIterator` |
|   0.7 % | `short[]` |
|   0.6 % | `scala.collection.View$Concat$$Lambda.0x00007e0001166998` |
|   0.6 % | `scala.collection.mutable.ArrayBufferView` |
|   0.6 % | `scala.collection.mutable.ArrayBuffer` |

### Allocation by first scalus.* frame

| share | item |
|---|---|
|  19.6 % | `scalus.uplc.transform.CommonSubexpressionElimination$.scalus$uplc$transform$CommonSubexpressionElimination$$$isAncestorOrSelf` |
|  18.8 % | `scalus.uplc.transform.TermAnalysis$.freeVars` |
|  17.3 % | `scalus.uplc.TermPrinter$.prettyTermWithDepth` |
|   9.3 % | `scalus.uplc.transform.CommonSubexpressionElimination$.scalus$uplc$transform$CommonSubexpressionElimination$$$longestCommonPrefix` |
|   5.0 % | `scalus.compiler.sir.lowering.LoweredValue.$init$` |
|   3.8 % | `scalus.uplc.transform.CommonSubexpressionElimination.add$1` |
|   2.2 % | `scalus.uplc.transform.CommonSubexpressionElimination.4 (lambda)` |
|   1.5 % | `scalus.uplc.transform.CommonSubexpressionElimination.go$1$$anonfun$2` |
|   1.5 % | `scalus.uplc.Term.show` |
|   1.3 % | `scalus.uplc.transform.CommonSubexpressionElimination.go$1` |
|   1.0 % | `scalus.uplc.eval.CekMachine.applyEvaluate` |
|   0.9 % | `scalus.uplc.transform.CommonSubexpressionElimination.3 (lambda)` |
|   0.7 % | `scalus.uplc.transform.CommonSubexpressionElimination.collect$$anonfun$2$$anonfun$2` |
|   0.7 % | `scalus.compiler.sir.SIRType$.advance$1` |
|   0.6 % | `scalus.uplc.transform.CommonSubexpressionElimination.collect$$anonfun$2` |
|   0.5 % | `scalus.uplc.eval.TallyingBudgetSpenderLogger.spendBudget` |
|   0.5 % | `scalus.uplc.transform.CommonSubexpressionElimination.go$2` |
|   0.5 % | `scalus.compiler.sir.SIRType$.partitionGround` |
|   0.5 % | `scalus.uplc.DeBruijn$.fromDeBruijnTerm` |
|   0.5 % | `scalus.uplc.Term$Apply$.apply` |
|   0.4 % | `scalus.uplc.transform.CommonSubexpressionElimination.children` |
|   0.4 % | `scalus.uplc.eval.ExBudgetCategory$Step$.apply` |
|   0.3 % | `scalus.serialization.flat.HashConsedRef$$anon$1.<init>` |
|   0.3 % | `scalus.uplc.transform.CommonSubexpressionElimination.collect` |
|   0.3 % | `scalus.compiler.sir.lowering.ProductCaseClassRepresentation$ProdUplcConstr.stableKeyInternal` |

