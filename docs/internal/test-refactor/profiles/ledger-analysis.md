CPU samples: 277; allocation sampled: 7.7 GiB; GC: 62 collections, 100 ms of pauses

### CPU by test suite (outermost suite frame)

| share | item |
|---|---|
|  33.2 % | `scalus.cardano.txbuilder.ChangeOutputDiffHandlerTest` |
|  10.1 % | `scalus.testing.conformance.CardanoLedgerConformanceTest` |
|   8.7 % | `scalus.cardano.wallet.hd.HdAccountTest` |
|   6.5 % | `scalus.cardano.node.EmulatorConcurrencyTest` |
|   5.8 % | `(no test frame: harness, JIT, GC threads)` |
|   4.0 % | `scalus.testing.conformance.TxInfoTranslationGoldenTest` |
|   4.0 % | `scalus.cardano.ledger.rules.PlutusScriptsTransactionMutatorTest` |
|   2.9 % | `scalus.cardano.wallet.hd.HdKeyPairTest` |
|   2.9 % | `scalus.cardano.txbuilder.TransactionSignerTest` |
|   2.5 % | `scalus.cardano.ledger.rules.AllInputsMustBeInUtxoValidatorTest` |
|   1.8 % | `scalus.cardano.txbuilder.PlutusScriptEvaluationExceptionTest` |
|   1.8 % | `scalus.cardano.node.EmulatorTest` |
|   1.4 % | `scalus.cardano.txbuilder.DiagnosticReplayTest` |
|   1.1 % | `scalus.cardano.wallet.hd.Bip32Ed25519Test` |
|   1.1 % | `scalus.cardano.txbuilder.TxBuilderCompleteTest` |
|   1.1 % | `scalus.cardano.txbuilder.TxBuilderPerformanceTest` |
|   0.7 % | `scalus.cardano.txbuilder.UtxoPoolTest` |
|   0.7 % | `scalus.cardano.txbuilder.TransactionBuilderTest` |
|   0.7 % | `scalus.cardano.wallet.hd.Bip39Test` |
|   0.7 % | `scalus.cardano.ledger.rules.TooManyCollateralInputsValidatorTest` |
|   0.7 % | `scalus.cardano.ledger.rules.InputsAndReferenceInputsDisjointValidatorTest` |
|   0.7 % | `scalus.cardano.node.EmulatorParityTest` |
|   0.7 % | `scalus.cardano.txbuilder.TxBuilderTest` |
|   0.7 % | `scalus.testing.conformance.LedgerStateComparisonTest` |
|   0.7 % | `scalus.cardano.ledger.rules.MissingKeyHashesValidatorTest` |

### CPU inclusive by scalus.* method

| share | item |
|---|---|
|  29.6 % | `scalus.cardano.txbuilder.TransactionBuilder$.loop$1` |
|  29.6 % | `scalus.cardano.txbuilder.TransactionBuilder$.balanceFeeAndChangeWithTokens` |
|  19.5 % | `scalus.cardano.txbuilder.TransactionBuilder$.computeScriptsWitness` |
|  19.5 % | `scalus.cardano.txbuilder.TransactionBuilder$.computeScriptsWitness$$anonfun$1` |
|  17.3 % | `scalus.cardano.ledger.PlutusScriptEvaluator.evalPlutusScripts` |
|  17.3 % | `scalus.cardano.ledger.PlutusScriptEvaluator$DefaultImpl.evalPlutusScriptsWithContexts` |
|  17.3 % | `scalus.cardano.ledger.PlutusScriptEvaluator.evalPlutusScripts$` |
|  17.3 % | `scalus.cardano.ledger.PlutusScriptEvaluator$DefaultImpl.evalPlutusScripts` |
|   8.7 % | `scalus.crypto.Hmac$.hmacSha512` |
|   8.7 % | `scalus.crypto.Pbkdf2$.deriveKey` |
|   8.7 % | `scalus.crypto.Pbkdf2$.f` |
|   8.7 % | `scalus.cardano.txbuilder.TransactionBuilder$.8 (lambda)` |
|   8.3 % | `scalus.cardano.wallet.hd.Bip32Ed25519$.masterKeyFromMnemonic` |
|   8.3 % | `scalus.cardano.wallet.hd.Bip32Ed25519$.masterKeyFromEntropy` |
|   7.9 % | `scalus.cardano.ledger.PlutusScriptEvaluator$DefaultImpl.allResolvedScripts` |
|   7.9 % | `scalus.cardano.ledger.rules.STS$Mutator$.transit` |
|   7.6 % | `scalus.cardano.ledger.Transaction.id` |
|   7.2 % | `scalus.cardano.ledger.Transaction.id$lzyINIT1` |
|   7.2 % | `scalus.uplc.builtin.JVMPlatformSpecific$.sha2_512` |
|   7.2 % | `scalus.utils.Utils$.sha2_512` |
|   7.2 % | `scalus.crypto.Hmac$.sha512` |
|   7.2 % | `scalus.uplc.builtin.JVMPlatformSpecific.sha2_512` |
|   7.2 % | `scalus.uplc.builtin.JVMPlatformSpecific.sha2_512$` |
|   7.2 % | `scalus.cardano.ledger.utils.AllResolvedScripts$.allResolvedScriptsView` |
|   6.9 % | `scalus.cardano.wallet.hd.Bip32Ed25519$.deriveFromPath` |

### CPU self by first scalus.* frame

| share | item |
|---|---|
|   7.2 % | `scalus.utils.Utils$.sha2_512` |
|   5.1 % | `scalus.uplc.builtin.JVMPlatformSpecific.blake2b_256` |
|   4.0 % | `scalus.cardano.ledger.utils.AllNeededScriptHashes$.allNeededScriptHashes$$anonfun$1` |
|   3.2 % | `scalus.cardano.ledger.PlutusScriptEvaluator$DefaultImpl.evalPlutusScriptsWithContexts` |
|   2.5 % | `scalus.testing.conformance.LedgerStateComparison$.show` |
|   2.2 % | `scalus.cardano.txbuilder.TransactionSignerTest.generateKeyPair` |
|   1.8 % | `scalus.utils.Hex$.hexToBytes` |
|   1.8 % | `scalus.cardano.ledger.TransactionBody$given_Encoder_TransactionBody$.write` |
|   1.8 % | `scalus.cardano.ledger.utils.ExactValue.convert` |
|   1.4 % | `scalus.crypto.Hmac$.hmacSha512` |
|   1.4 % | `scalus.cardano.wallet.hd.HdAccountTest.testFun$proxy15$1` |
|   1.4 % | `scalus.cardano.txbuilder.ChangeOutputDiffHandlerTest.forAll` |
|   1.1 % | `scalus.testing.conformance.GoldenTranslationVectors$Cbor$.read` |
|   1.1 % | `scalus.compiler.sir.SIRType$.advance$1` |
|   1.1 % | `scalus.cardano.wallet.hd.HdAccountTest.testFun$proxy22$1$$anonfun$1` |
|   1.1 % | `scalus.cardano.txbuilder.Change$.changeOutputDiffHandler` |
|   1.1 % | `scalus.cardano.ledger.ScriptDataHashGenerator$.computeScriptDataHash$$anonfun$1$$anonfun$1` |
|   1.1 % | `scalus.uplc.builtin.AbstractBuiltins.appendByteString` |
|   0.7 % | `scalus.cardano.ledger.rules.ValidatorRulesTestKit.randomTransactionWithIsValidField` |
|   0.7 % | `scalus.cardano.ledger.internal.LoggerPlatform$$anon$1.debug` |
|   0.7 % | `scalus.cardano.ledger.ScriptDataHashGenerator$.computeScriptDataHash` |
|   0.7 % | `scalus.cardano.ledger.utils.AllNeededScriptHashes$.allNeededVotingProceduresScriptIndexHashesView` |
|   0.7 % | `scalus.cardano.ledger.RefScriptFee.fee` |
|   0.7 % | `scalus.cardano.txbuilder.RedeemerManagement$.attachRedeemers` |
|   0.7 % | `scalus.cardano.ledger.utils.AllProvidedReferenceScripts$.nonDistinctReferenceScripts` |

### CPU self by top frame

| share | item |
|---|---|
|   8.3 % | `sun.security.provider.ByteArrayAccess.l2bBig` |
|   3.6 % | `org.bouncycastle.crypto.digests.Blake2bDigest.compress` |
|   2.5 % | `scala.collection.mutable.Growable.addAll` |
|   1.8 % | `scala.collection.View$ZipWithIndex.iterator` |
|   1.4 % | `scalus.crypto.Hmac$.hmacSha512` |
|   1.4 % | `com.sun.crypto.provider.PBKDF2KeyImpl.deriveKey` |
|   1.4 % | `scala.collection.Iterator$$anon$10.hasNext` |
|   1.1 % | `scalus.testing.conformance.GoldenTranslationVectors$Cbor$.read` |
|   1.1 % | `scalus.cardano.ledger.utils.ExactValue.convert` |
|   1.1 % | `scalus.cardano.txbuilder.Change$.changeOutputDiffHandler` |
|   1.1 % | `scala.collection.Iterator$$anon$16.hasNext` |
|   1.1 % | `io.bullet.borer.output.ToByteArrayOutput$ToByteArray.inline$allocateBuffer` |
|   0.7 % | `scalus.utils.Utils$.sha2_512` |
|   0.7 % | `scala.collection.immutable.LazyList.initState` |
|   0.7 % | `java.lang.String.length` |
|   0.7 % | `scala.collection.SeqView$Id.length` |
|   0.7 % | `scalus.cardano.ledger.RefScriptFee.fee` |
|   0.7 % | `org.scalacheck.Gen$Choose$.org$scalacheck$Gen$Choose$$$chLng` |
|   0.7 % | `scalus.cardano.txbuilder.TransactionBuilder$.anonfun (lambda)` |
|   0.7 % | `scala.runtime.BoxesRunTime.equals2` |
|   0.7 % | `java.lang.invoke.DirectMethodHandle.allocateInstance` |
|   0.7 % | `scala.util.Either.flatMap` |
|   0.7 % | `scala.collection.IterableOps.view (lambda)` |
|   0.7 % | `scala.collection.View$Concat.knownSize` |
|   0.7 % | `java.util.Objects.equals` |

### Allocation by class

| share | item |
|---|---|
|  48.2 % | `byte[]` |
|   7.5 % | `io.bullet.borer.output.ToByteArrayOutput$ToByteArray` |
|   6.9 % | `java.lang.Object[]` |
|   3.4 % | `int[]` |
|   2.7 % | `scalus.cardano.ledger.MultiAsset$$$Lambda.0x000003ff0138ee90` |
|   2.6 % | `long[]` |
|   1.7 % | `scala.Tuple2` |
|   1.3 % | `org.scalacheck.rng.Seed$apply` |
|   1.2 % | `java.lang.String` |
|   1.0 % | `scala.collection.mutable.HashMap$Node[]` |
|   0.8 % | `scala.collection.View$Concat` |
|   0.7 % | `scala.collection.immutable.$colon$colon` |
|   0.7 % | `sun.security.provider.SHA5$SHA512` |
|   0.6 % | `scala.Some` |
|   0.6 % | `scala.collection.immutable.RedBlackTree$KeysIterator` |
|   0.5 % | `short[]` |
|   0.5 % | `scala.collection.Iterator$$anon$6` |
|   0.5 % | `scala.collection.mutable.HashMap` |
|   0.5 % | `scala.math.BigInt` |
|   0.4 % | `scala.collection.immutable.VectorBuilder` |
|   0.4 % | `scalus.cardano.ledger.Hashes$package$Hash$$anon$1` |
|   0.4 % | `org.scalacheck.Gen$$anon$6` |
|   0.4 % | `scala.collection.immutable.RedBlackTree$Tree` |
|   0.4 % | `net.i2p.crypto.eddsa.math.ed25519.Ed25519FieldElement` |
|   0.4 % | `scala.collection.View$ZipWithIndex` |

### Allocation by first scalus.* frame

| share | item |
|---|---|
|  17.0 % | `scalus.serialization.cbor.Cbor$.encode` |
|  14.8 % | `scalus.cardano.ledger.TransactionBody$given_Encoder_TransactionBody$.write` |
|   7.3 % | `scalus.uplc.builtin.ByteStringOffchainApi.apply` |
|   6.6 % | `scalus.crypto.Hmac$.hmacSha512` |
|   4.6 % | `scalus.utils.Utils$.sha2_512` |
|   2.9 % | `scalus.cardano.ledger.MultiAsset$.binOp` |
|   2.3 % | `scalus.compiler.sir.lowering.LoweredValue.$init$` |
|   2.1 % | `scalus.compiler.sir.lowering.LocalScope.add` |
|   1.5 % | `scalus.cardano.ledger.rules.ValidatorRulesTestKit.randomTransactionWithIsValidField` |
|   1.3 % | `scalus.cardano.txbuilder.TransactionSignerTest.generateKeyPair` |
|   1.2 % | `scalus.utils.Hex$.hexToBytes` |
|   1.2 % | `scalus.cardano.ledger.PlutusScriptEvaluator$DefaultImpl.evalPlutusScriptsWithContexts` |
|   1.2 % | `scalus.cardano.txbuilder.ChangeOutputDiffHandlerTest.forAll` |
|   1.1 % | `scalus.testing.conformance.CardanoLedgerVectors$.conformanceVectorsPath$lzyINIT1$$anonfun$1$$anonfun$1$$anonfun$1` |
|   1.1 % | `scalus.cardano.ledger.utils.ExactValue$Accumulator.<init>` |
|   1.0 % | `(non-scalus)` |
|   1.0 % | `scalus.cardano.txbuilder.ChangeOutputDiffHandlerTest.mkTx` |
|   1.0 % | `scalus.uplc.builtin.JVMPlatformSpecific.blake2b_256` |
|   0.9 % | `scalus.cardano.ledger.Certificate$given_Decoder_Certificate$.read` |
|   0.9 % | `scalus.cardano.ledger.utils.AllProvidedReferenceScripts$.allProvidedReferenceScriptsNonDistinctSeq` |
|   0.8 % | `scalus.serialization.flat.HashConsedRef$$anon$1.<init>` |
|   0.7 % | `scalus.testing.conformance.GoldenTranslationVectors$Cbor$.read` |
|   0.7 % | `scalus.cardano.ledger.utils.ExactValue$Accumulator.add` |
|   0.6 % | `scalus.cardano.ledger.EvaluatorReportConfig$.fromEnv$default$2` |
|   0.6 % | `scalus.cardano.ledger.utils.AllNeededScriptHashes$.allNeededScriptHashes$$anonfun$1` |

