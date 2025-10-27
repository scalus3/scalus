package scalus.patterns

import org.scalatest.funsuite.AnyFunSuite
import scalus.*
import scalus.sir.PrettyPrinter
import scalus.builtin.ByteString.*
import scalus.ledger.api.v1.Address
import scalus.testkit.Mock

class DumpLinkedListSir extends AnyFunSuite:
    inline given scalus.Compiler.Options = scalus.Compiler.Options(
      targetLoweringBackend = scalus.Compiler.TargetLoweringBackend.SirToUplcV3Lowering,
      generateErrorTraces = true,
      optimizeUplc = true,
      debug = false
    )

    test("Dump SIR and UPLC code"):
        // Get compiled SIR
        val compiledSir = LinkedListContract.compiled

        // Pretty print SIR
        val sirStr = PrettyPrinter.pretty(compiledSir, PrettyPrinter.Style.Normal).render(120)
        println(s"\n=== SIR Program ===")
        println(s"SIR length: ${sirStr.length} chars")

        // Write SIR to file
        import java.nio.file.{Files, Paths}
        val sirOutputPath = sys.env.getOrElse("SIR_OUTPUT", "/tmp/linkedlist.sir")
        Files.write(Paths.get(sirOutputPath), sirStr.getBytes())
        println(s"SIR written to $sirOutputPath")

        // Generate UPLC
        val policyId = Mock.mockScriptHash(1)
        val initRef = Mock.mockTxOutRef(1, 1)
        val royalty = Mock.mockScriptHash(2)
        val config = Config(
          init = initRef,
          deadline = 86_400_000L,
          penalty = Address.fromScriptHash(royalty)
        )

        val compiledUplc = LinkedListContract.make(config)
        val uplcStr = PrettyPrinter.pretty(compiledUplc, PrettyPrinter.Style.Normal).render(120)
        println(s"\n=== UPLC Program ===")
        println(s"UPLC length: ${uplcStr.length} chars")
        println(s"UPLC size: ${compiledUplc.doubleCborEncoded.length} bytes")

        // Write UPLC to file
        val uplcOutputPath = sys.env.getOrElse("UPLC_OUTPUT", "/tmp/linkedlist.uplc")
        Files.write(Paths.get(uplcOutputPath), uplcStr.getBytes())
        println(s"UPLC written to $uplcOutputPath")
