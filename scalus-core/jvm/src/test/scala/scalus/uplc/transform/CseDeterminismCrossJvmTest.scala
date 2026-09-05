package scalus.uplc
package transform

import org.scalatest.funsuite.AnyFunSuite
import scalus.uplc.DefaultFun.*
import scalus.uplc.Term.*
import scalus.utils.Hex

import java.nio.charset.StandardCharsets.UTF_8
import java.nio.file.Paths

/** The honest determinism check: the optimizer must produce the same bytes in a JVM whose
  * identity-hash algorithm is different. HotSpot's experimental `-XX:hashCode=N` selects the
  * algorithm (2 = every identity hash is 1, 3 = a global counter), which maximally perturbs the
  * iteration order of any hash map keyed on identity hashes relative to this JVM.
  *
  * See docs/internal/UPLC_OPTIMIZER_DETERMINISM.md.
  */
class CseDeterminismCrossJvmTest extends AnyFunSuite {

    test("optimizer output is identical in child JVMs with a different identity-hash algorithm") {
        val expected = CseDeterminismChild.render()
        assert(expected.nonEmpty)
        for mode <- Seq("2", "3") do
            val javaBin = Paths.get(System.getProperty("java.home"), "bin", "java").toString
            val cmd = java.util.List.of(
              javaBin,
              "-XX:+UnlockExperimentalVMOptions",
              s"-XX:hashCode=$mode",
              "-Xss16m",
              "-cp",
              System.getProperty("java.class.path"),
              classOf[CseDeterminismChild.type].getName.stripSuffix("$")
            )
            val process = new ProcessBuilder(cmd).start()
            val out = new String(process.getInputStream.readAllBytes(), UTF_8)
            val err = new String(process.getErrorStream.readAllBytes(), UTF_8)
            val exit = process.waitFor()
            if exit != 0 && (err.contains("Unrecognized VM option") || err.contains(
                  "Could not create"
                ))
            then
                cancel(s"this JVM does not support -XX:hashCode=$mode: ${err.linesIterator.next()}")
            assert(exit == 0, s"child JVM (hashCode=$mode) failed: $err")
            assert(
              out == expected,
              s"optimizer output differs in a JVM with -XX:hashCode=$mode:\n--- this JVM\n$expected\n--- child\n$out"
            )
    }
}

/** Runs the optimizer on terms with tied CSE candidates and prints one line per result. Used both
  * in-process and as the main class of the child JVMs.
  */
object CseDeterminismChild {

    private def v(name: String): Term = Var(NamedDeBruijn(name))
    private def add(a: Term, b: Term): Term = Apply(Apply(Builtin(AddInteger), a), b)
    private def twice(t: Term): Term = add(t, t)

    /** Post-ForcedBuiltinsExtractor shape: forced builtins are variables, saturated ones stay. */
    private def fieldChain(x: String): Term =
        Apply(
          v("__HeadList"),
          Apply(
            v("__TailList"),
            Apply(v("__TailList"), Apply(v("__SndPair"), Apply(Builtin(UnConstrData), v(x))))
          )
        )

    /** Raw shape, as the optimizer sees it before forced builtins are extracted. */
    private def forcedChain(x: String): Term =
        Apply(
          Force(Builtin(HeadList)),
          Apply(Force(Builtin(TailList)), Apply(Force(Builtin(TailList)), v(x)))
        )

    private def intChain(x: String): Term =
        Apply(
          Builtin(UnIData),
          Apply(
            v("__HeadList"),
            Apply(v("__TailList"), Apply(v("__SndPair"), Apply(Builtin(UnConstrData), v(x))))
          )
        )

    private def bytesChain(x: String): Term =
        Apply(
          Builtin(UnBData),
          Apply(
            v("__HeadList"),
            Apply(v("__TailList"), Apply(v("__SndPair"), Apply(Builtin(UnConstrData), v(x))))
          )
        )

    /** Binds the helper variables and the given parameters, so the term is closed. */
    private def closed(params: Seq[String], body: Term): Term =
        (Seq("__HeadList", "__TailList", "__SndPair") ++ params)
            .foldRight(body)((n, acc) => LamAbs(n, acc))

    /** Every term has several same-size candidates over different variables that tie on the (size,
      * 60-char prefix) sort key and share a bind point.
      */
    val terms: Seq[Term] = Seq(
      closed(Seq("x", "y"), add(twice(fieldChain("x")), twice(fieldChain("y")))),
      closed(Seq("x", "y"), add(twice(forcedChain("x")), twice(forcedChain("y")))),
      closed(
        Seq("a", "b", "c"),
        add(add(twice(intChain("a")), twice(intChain("b"))), twice(intChain("c")))
      ),
      closed(Seq("p", "q"), add(twice(bytesChain("p")), twice(bytesChain("q")))),
      closed(
        Seq("d1", "d2", "d3", "d4"),
        add(
          add(twice(fieldChain("d1")), twice(fieldChain("d2"))),
          add(twice(fieldChain("d3")), twice(fieldChain("d4")))
        )
      ),
      closed(Seq("m", "n"), add(twice(intChain("m")), twice(bytesChain("n"))))
    )

    def render(): String = {
        val sb = new StringBuilder
        for (t, i) <- terms.zipWithIndex do
            val cse = CommonSubexpressionElimination(t)
            val opt = new V3Optimizer(cseIterations = 2, cceEnabled = true).apply(t)
            sb.append(s"$i cse ${Hex.bytesToHex(Program.plutusV3(cse).flatEncoded)}\n")
            sb.append(s"$i v3 ${Hex.bytesToHex(Program.plutusV3(opt).flatEncoded)}\n")
        sb.toString
    }

    def main(args: Array[String]): Unit = {
        print(render())
        System.out.flush()
    }
}
