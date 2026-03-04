package scalus.examples.setbench

import org.scalatest.funsuite.AnyFunSuite
import scalus.cardano.blueprint.Blueprint
import scalus.cardano.ledger.{ExUnitPrices, ExUnits, NonNegativeInterval}
import scalus.crypto.trie.MerklePatriciaTrie as Mpf16o
import scalus.cardano.onchain.plutus.crypto.trie.MerklePatriciaTrie.{Proof as Mpf16oProof, ProofStep as Mpf16oStep}
import scalus.cardano.onchain.plutus.prelude.List as PList
import scalus.cardano.onchain.plutus.prelude.Option as POption
import scalus.cardano.onchain.plutus.v3.*
import scalus.compiler.Options
import scalus.compiler.sir.TargetLoweringBackend
import scalus.uplc.builtin.Data.{toData, B, Constr, I}
import scalus.uplc.builtin.{ByteString, Data}
import scalus.uplc.eval.{ExBudgetCategory, PlutusVM, Result}
import scalus.uplc.{Constant, PlutusV3, Program, Term}
import scalus.uplc.Term.asTerm

/** Cost analysis: per-builtin breakdown comparing Scalus MPF16o vs Aiken MPF.
  *
  * The Aiken validator is a full spending validator so we can't call just the MPF logic in
  * isolation. Instead we compare: (1) standalone Scalus `has()` per-builtin costs, and (2) UPLC
  * code structure of both validators.
  *
  * Run with:
  * {{{
  * sbtn "scalusExamplesJVM/testOnly *MpfCostAnalysisTest"
  * }}}
  */
class MpfCostAnalysisTest extends AnyFunSuite {

    private given Options = Options(
      targetLoweringBackend = TargetLoweringBackend.SirToUplcV3Lowering,
      generateErrorTraces = false,
      optimizeUplc = true,
      debug = false
    )

    private given PlutusVM = PlutusVM.makePlutusV3VM()

    private val exPrices = ExUnitPrices(
      priceMemory = NonNegativeInterval(0.0577, precision = 15),
      priceSteps = NonNegativeInterval(0.0000721, precision = 15)
    )

    // --- Proof encoding helpers ---

    private def mpf16oStepToData(step: Mpf16oStep): Data = step match
        case Mpf16oStep.Branch(skip, neighbors) =>
            Constr(0, PList(I(skip), B(neighbors)))
        case Mpf16oStep.Fork(skip, neighbor) =>
            val neighborData =
                Constr(0, PList(I(neighbor.nibble), B(neighbor.prefix), B(neighbor.root)))
            Constr(1, PList(I(skip), neighborData))
        case Mpf16oStep.Leaf(skip, key, value) =>
            Constr(2, PList(I(skip), B(key), B(value)))

    private def mpf16oProofToData(proof: Mpf16oProof): Data = {
        val steps = proof.toScalaList.map(mpf16oStepToData)
        Data.List(steps.foldRight(PList.Nil: PList[Data]) { (d, acc) => PList.Cons(d, acc) })
    }

    // --- Compiled Scalus MPF16o has() ---

    private val mpf16oHasProgram = PlutusV3.compile {
        (rootD: Data, keyD: Data, valueD: Data, proofD: Data) =>
            import scalus.cardano.onchain.plutus.crypto.trie.MerklePatriciaTrie
            import scalus.cardano.onchain.plutus.crypto.trie.MerklePatriciaTrie.*
            import scalus.uplc.builtin.Builtins.*
            val trie = MerklePatriciaTrie(unBData(rootD))
            trie.has(unBData(keyD), unBData(valueD), proofD.to[Proof])
    }

    // --- Aiken MPF validator ---

    private lazy val aikenProgram: Program = {
        val fname = "/scalus/examples/AikenMpfData/plutus.json"
        val inputStream = this.getClass.getResourceAsStream(fname)
        if inputStream == null then throw new RuntimeException(s"Resource not found: $fname")
        try
            val blueprint = Blueprint.fromJson(inputStream)
            blueprint.validators.head.compiledCode.map(Program.fromCborHex).get
        finally inputStream.close()
    }

    private def buildTrieAndProof(n: Int, elemIdx: Int = 42) = {
        val rng = new scala.util.Random(42)
        val elems = Vector.tabulate(n) { i =>
            val key = ByteString.fromString(s"element-${rng.nextInt()}-$i")
            val value = ByteString.fromString(s"value-$i")
            (key, value)
        }
        val trie = Mpf16o.fromList(elems)
        val (key, value) = elems(elemIdx)
        val proofData = mpf16oProofToData(trie.proveMembership(key))
        (trie, key, value, proofData)
    }

    private def showCostComparison(
        label: String,
        costs: collection.Map[ExBudgetCategory, collection.Seq[scalus.cardano.ledger.ExUnits]]
    ): Unit = {
        info(s"\n=== $label ===")
        info(Result.showCosts(costs, exPrices))

        // Summarize builtins vs CEK steps
        var builtinCpu = 0L
        var builtinMem = 0L
        var builtinCount = 0
        var stepCpu = 0L
        var stepMem = 0L
        var stepCount = 0
        for (cat, exunits) <- costs do
            val totalCpu = exunits.map(_.steps).sum
            val totalMem = exunits.map(_.memory).sum
            cat match
                case ExBudgetCategory.BuiltinApp(_) =>
                    builtinCpu += totalCpu; builtinMem += totalMem; builtinCount += exunits.size
                case ExBudgetCategory.Step(_) =>
                    stepCpu += totalCpu; stepMem += totalMem; stepCount += exunits.size
                case _ => ()
        info(
          f"\nBuiltins: count=$builtinCount%,d  cpu=$builtinCpu%,d  mem=$builtinMem%,d"
        )
        info(
          f"CEK steps: count=$stepCount%,d  cpu=$stepCpu%,d  mem=$stepMem%,d"
        )
        info(
          f"Builtin/total cpu ratio: ${builtinCpu.toDouble / (builtinCpu + stepCpu)}%.3f"
        )
    }

    test("Scalus MPF16o has() per-builtin cost breakdown (N=1000)") {
        val (trie, key, value, proofData) = buildTrieAndProof(1000)

        val applied = mpf16oHasProgram.program.term $
            B(trie.rootHash).asTerm $
            B(key).asTerm $
            B(value).asTerm $
            proofData.asTerm

        applied.evaluateDebug match
            case Result.Success(_, budget, costs, _) =>
                info(
                  s"Total: mem=${budget.memory} cpu=${budget.steps} fee=${budget.fee(exPrices).value}"
                )
                showCostComparison("Scalus MPF16o has() N=1000", costs)
            case Result.Failure(ex, budget, costs, _) =>
                info(s"FAILED: ${ex.getMessage}")
                showCostComparison("Scalus MPF16o has() N=1000 (FAILED)", costs)
                fail(ex)
    }

    test("Scalus MPF16o has() per-builtin cost breakdown (N=32000)") {
        val (trie, key, value, proofData) = buildTrieAndProof(32000)

        val applied = mpf16oHasProgram.program.term $
            B(trie.rootHash).asTerm $
            B(key).asTerm $
            B(value).asTerm $
            proofData.asTerm

        applied.evaluateDebug match
            case Result.Success(_, budget, costs, _) =>
                info(
                  s"Total: mem=${budget.memory} cpu=${budget.steps} fee=${budget.fee(exPrices).value}"
                )
                showCostComparison("Scalus MPF16o has() N=32000", costs)
            case Result.Failure(ex, budget, costs, _) =>
                info(s"FAILED: ${ex.getMessage}")
                showCostComparison("Scalus MPF16o has() N=32000 (FAILED)", costs)
                fail(ex)
    }

    test("Dump UPLC to files for comparison") {
        val scalusUplc = mpf16oHasProgram.program.show
        val aikenUplc = aikenProgram.show

        val scalusFile = java.io.File("/tmp/scalus_mpf16o.uplc")
        java.nio.file.Files.writeString(scalusFile.toPath, scalusUplc)
        info(s"Scalus UPLC written to ${scalusFile.getAbsolutePath} (${scalusUplc.length} chars)")

        val aikenFile = java.io.File("/tmp/aiken_mpf.uplc")
        java.nio.file.Files.writeString(aikenFile.toPath, aikenUplc)
        info(s"Aiken UPLC written to ${aikenFile.getAbsolutePath} (${aikenUplc.length} chars)")
    }

    // --- Individual function UPLC comparison ---
    // These compile the same functions as the Aiken bench validator

    private val scalusCombineProgram = PlutusV3.compile { (left: ByteString, right: ByteString) =>
        import scalus.cardano.onchain.plutus.crypto.trie.Merkling.*
        combine(left, right)
    }

    private val scalusNibbleProgram = PlutusV3.compile { (path: ByteString, index: BigInt) =>
        import scalus.cardano.onchain.plutus.crypto.trie.Merkling.*
        nibble(path, index)
    }

    private val scalusSuffixProgram = PlutusV3.compile { (path: ByteString, cursor: BigInt) =>
        import scalus.cardano.onchain.plutus.crypto.trie.Merkling.*
        suffix(path, cursor)
    }

    private val scalusNibblesProgram = PlutusV3.compile {
        (path: ByteString, start: BigInt, end: BigInt) =>
            import scalus.cardano.onchain.plutus.crypto.trie.Merkling.*
            nibbles(path, start, end)
    }

    private val scalusMerkle4Program = PlutusV3.compile {
        (branch: BigInt, root: ByteString, neighbor2: ByteString, neighbor1: ByteString) =>
            import scalus.cardano.onchain.plutus.crypto.trie.Merkling.*
            merkle4(branch, root, neighbor2, neighbor1)
    }

    test("Dump individual function UPLC for side-by-side comparison") {
        val dir = java.io.File("/tmp/uplc-compare")
        dir.mkdirs()

        val funcs = Seq(
          ("combine", scalusCombineProgram.program),
          ("nibble", scalusNibbleProgram.program),
          ("suffix", scalusSuffixProgram.program),
          ("nibbles", scalusNibblesProgram.program),
          ("merkle4", scalusMerkle4Program.program)
        )

        for (name, prog) <- funcs do
            val uplc = prog.show
            val path = java.nio.file.Path.of(s"/tmp/uplc-compare/scalus_$name.uplc")
            java.nio.file.Files.writeString(path, uplc)
            info(
              s"$name: ${prog.cborEncoded.length} CBOR bytes, ${uplc.split('\n').length} UPLC lines"
            )
            // Also show builtin usage
            val builtins = scala.collection.mutable.Map[String, Int]().withDefaultValue(0)
            def walk(t: Term): Unit = t match
                case Term.Builtin(fun, _)      => builtins(fun.toString) += 1
                case Term.LamAbs(_, body, _)   => walk(body)
                case Term.Apply(f, arg, _)     => walk(f); walk(arg)
                case Term.Force(t, _)          => walk(t)
                case Term.Delay(t, _)          => walk(t)
                case Term.Constr(_, args, _)   => args.foreach(walk)
                case Term.Case(scrut, alts, _) => walk(scrut); alts.foreach(walk)
                case _                         => ()
            walk(prog.term)
            info(
              s"  builtins: ${builtins.toSeq.sortBy(-_._2).map((n, c) => s"$n($c)").mkString(", ")}"
            )
    }

    test("UPLC term count comparison") {
        def countTerms(term: Term): Int = term match
            case Term.Var(_, _)            => 1
            case Term.LamAbs(_, body, _)   => 1 + countTerms(body)
            case Term.Apply(f, arg, _)     => 1 + countTerms(f) + countTerms(arg)
            case Term.Force(t, _)          => 1 + countTerms(t)
            case Term.Delay(t, _)          => 1 + countTerms(t)
            case Term.Const(_, _)          => 1
            case Term.Builtin(_, _)        => 1
            case _: Term.Error             => 1
            case Term.Constr(_, args, _)   => 1 + args.map(countTerms).sum
            case Term.Case(scrut, alts, _) => 1 + countTerms(scrut) + alts.map(countTerms).sum

        def countBuiltins(term: Term): Map[String, Int] = {
            val counts = collection.mutable.Map[String, Int]().withDefaultValue(0)
            def walk(t: Term): Unit = t match
                case Term.Builtin(fun, _)      => counts(fun.toString) += 1
                case Term.LamAbs(_, body, _)   => walk(body)
                case Term.Apply(f, arg, _)     => walk(f); walk(arg)
                case Term.Force(t, _)          => walk(t)
                case Term.Delay(t, _)          => walk(t)
                case Term.Constr(_, args, _)   => args.foreach(walk)
                case Term.Case(scrut, alts, _) => walk(scrut); alts.foreach(walk)
                case _                         => ()
            walk(term)
            counts.toMap
        }

        val scalusTerm = mpf16oHasProgram.program.term
        val aikenTerm = aikenProgram.term

        info(s"Scalus MPF16o: ${countTerms(scalusTerm)} UPLC terms")
        info(s"Aiken MPF:     ${countTerms(aikenTerm)} UPLC terms")

        info(s"\nScalus CBOR size: ${mpf16oHasProgram.program.cborEncoded.length} bytes")
        info(s"Aiken CBOR size:  ${aikenProgram.cborEncoded.length} bytes")

        info("\n=== Scalus builtin usage (in UPLC source) ===")
        for (name, count) <- countBuiltins(scalusTerm).toSeq.sortBy(-_._2) do
            info(f"  $name%-30s $count%4d")

        info("\n=== Aiken builtin usage (in UPLC source) ===")
        for (name, count) <- countBuiltins(aikenTerm).toSeq.sortBy(-_._2) do
            info(f"  $name%-30s $count%4d")
    }

    // --- Side-by-side full-validator comparison ---

    private lazy val scalusLightProgram: Program = Mpf16oLightContract.program

    /** Build a minimal ScriptContext for a spending validator with the given datum and redeemer. */
    private def makeScriptContext(datumData: Data, redeemerData: Data): ScriptContext = {
        val dummyTxId = TxId(ByteString.fromHex("00" * 32))
        val txOutRef = TxOutRef(dummyTxId, 0)
        ScriptContext(
          txInfo = TxInfo.placeholder,
          redeemer = redeemerData,
          scriptInfo = ScriptInfo.SpendingScript(txOutRef, POption.Some(datumData))
        )
    }

    /** Extract per-builtin runtime call counts from cost map. */
    private def runtimeBuiltinCounts(
        costs: collection.Map[ExBudgetCategory, collection.Seq[ExUnits]]
    ): Map[String, Int] = {
        costs.collect { case (ExBudgetCategory.BuiltinApp(name), exunits) =>
            name.toString -> exunits.size
        }.toMap
    }

    /** Extract summary stats from cost map: (builtinCpu, builtinMem, builtinCount, cekCpu, cekMem,
      * cekCount).
      */
    private def costSummary(
        costs: collection.Map[ExBudgetCategory, collection.Seq[ExUnits]]
    ): (Long, Long, Int, Long, Long, Int) = {
        var builtinCpu = 0L; var builtinMem = 0L; var builtinCount = 0
        var cekCpu = 0L; var cekMem = 0L; var cekCount = 0
        for (cat, exunits) <- costs do
            val totalCpu = exunits.map(_.steps).sum
            val totalMem = exunits.map(_.memory).sum
            cat match
                case ExBudgetCategory.BuiltinApp(_) =>
                    builtinCpu += totalCpu; builtinMem += totalMem; builtinCount += exunits.size
                case ExBudgetCategory.Step(_) =>
                    cekCpu += totalCpu; cekMem += totalMem; cekCount += exunits.size
                case _ => ()
        (builtinCpu, builtinMem, builtinCount, cekCpu, cekMem, cekCount)
    }

    test("Side-by-side full-validator comparison (N=1000)") {
        sideBySideComparison(1000)
    }

    test("Side-by-side full-validator comparison (N=32000)") {
        sideBySideComparison(32000)
    }

    private def sideBySideComparison(n: Int): Unit = {
        val (trie, key, value, proofData) = buildTrieAndProof(n)

        // Build datum and redeemer as Data
        val datumData = SetBenchDatum(BigInt(1_000_000), trie.rootHash).toData
        val redeemerData = SetBenchRedeemer.Withdraw(key, value, proofData).toData
        val ctx = makeScriptContext(datumData, redeemerData)
        val ctxData = ctx.toData

        // Evaluate Scalus light validator
        val scalusApplied = scalusLightProgram $ ctxData
        val scalusResult = scalusApplied.evaluateDebug

        // Evaluate Aiken validator
        val aikenApplied = aikenProgram $ ctxData
        val aikenResult = aikenApplied.evaluateDebug

        info(s"\n========== Side-by-side comparison N=$n ==========")

        // Show totals
        val scalusBudget = scalusResult.budget
        val aikenBudget = aikenResult.budget
        info(f"\n${"Metric"}%-30s ${"Scalus"}%15s ${"Aiken"}%15s ${"Ratio"}%10s")
        info("-" * 72)
        info(
          f"${"Total CPU steps"}%-30s ${scalusBudget.steps}%,15d ${aikenBudget.steps}%,15d ${scalusBudget.steps.toDouble / aikenBudget.steps}%10.2fx"
        )
        info(
          f"${"Total memory"}%-30s ${scalusBudget.memory}%,15d ${aikenBudget.memory}%,15d ${scalusBudget.memory.toDouble / aikenBudget.memory}%10.2fx"
        )
        info(
          f"${"Fee (lovelace)"}%-30s ${scalusBudget.fee(exPrices).value}%,15d ${aikenBudget.fee(exPrices).value}%,15d ${scalusBudget.fee(exPrices).value.toDouble / aikenBudget.fee(exPrices).value}%10.2fx"
        )

        // Summarize builtin vs CEK breakdown
        val (sBiCpu, sBiMem, sBiCnt, sCekCpu, sCekMem, sCekCnt) =
            costSummary(scalusResult.costs)
        val (aBiCpu, aBiMem, aBiCnt, aCekCpu, aCekMem, aCekCnt) =
            costSummary(aikenResult.costs)

        info(f"\n${""}%-30s ${"Scalus"}%15s ${"Aiken"}%15s ${"Ratio"}%10s")
        info("-" * 72)
        info(
          f"${"Builtin invocations"}%-30s ${sBiCnt}%,15d ${aBiCnt}%,15d ${sBiCnt.toDouble / aBiCnt}%10.2fx"
        )
        info(
          f"${"Builtin CPU"}%-30s ${sBiCpu}%,15d ${aBiCpu}%,15d ${sBiCpu.toDouble / aBiCpu}%10.2fx"
        )
        info(
          f"${"CEK step count"}%-30s ${sCekCnt}%,15d ${aCekCnt}%,15d ${sCekCnt.toDouble / aCekCnt}%10.2fx"
        )
        info(
          f"${"CEK CPU"}%-30s ${sCekCpu}%,15d ${aCekCpu}%,15d ${sCekCpu.toDouble / aCekCpu}%10.2fx"
        )
        info(
          f"${"CEK overhead (CEK/builtin)"}%-30s ${sCekCpu.toDouble / sBiCpu}%15.3f ${aCekCpu.toDouble / aBiCpu}%15.3f"
        )

        // Per-builtin runtime call count comparison
        val scalusBuiltins = runtimeBuiltinCounts(scalusResult.costs)
        val aikenBuiltins = runtimeBuiltinCounts(aikenResult.costs)
        val allBuiltinNames =
            (scalusBuiltins.keySet ++ aikenBuiltins.keySet).toSeq.sorted

        info(f"\n${"Builtin"}%-30s ${"Scalus"}%10s ${"Aiken"}%10s ${"Diff"}%10s")
        info("-" * 62)
        for name <- allBuiltinNames do
            val sc = scalusBuiltins.getOrElse(name, 0)
            val ac = aikenBuiltins.getOrElse(name, 0)
            val diff = sc - ac
            val diffStr = if diff > 0 then f"+$diff%d" else if diff < 0 then f"$diff%d" else "="
            info(f"$name%-30s $sc%,10d $ac%,10d $diffStr%10s")

        // Verify both succeeded
        scalusResult match
            case Result.Failure(ex, _, _, logs) =>
                info(s"\nScalus FAILED: ${ex.getMessage}")
                info(s"Logs: ${logs.mkString(", ")}")
                fail(s"Scalus validator failed: ${ex.getMessage}")
            case _ => ()
        aikenResult match
            case Result.Failure(ex, _, _, logs) =>
                info(s"\nAiken FAILED: ${ex.getMessage}")
                info(s"Logs: ${logs.mkString(", ")}")
                fail(s"Aiken validator failed: ${ex.getMessage}")
            case _ => ()
    }
}
