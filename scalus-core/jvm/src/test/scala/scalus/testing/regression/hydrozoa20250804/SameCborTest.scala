package scalus.testing.regression.hydrozoa20250804

import scalus.*
import scalus.Compiler.compile
import org.scalatest.funsuite.AnyFunSuite

import java.util.concurrent.atomic.AtomicReference

/** Test is check the determenstic behavior of the compiler. UPLC shoudl be the same.
  *
  * Problem:
  *
  * "scalusJVM/Test/runMain scalus.bugtracking.hydrozoa20250804.disputeResolutionValidatorRun"
  *
  * retun different result in different runs.
  *
  * This is attempt to reproduce the problem. (unsuccessful) So, the problem is not in the JVM
  * initialization, but in the different compilations.
  */
class SameCborTest extends AnyFunSuite {

    test("compile same script twice should give same cbor") {
        val sir1 = compile {
            DisputeResolutionValidator.validate
        }

        // println(s"SIR: ${sir1.pretty.render(100)}")

        val uplc1_1 = sir1.toUplcOptimized()
        val uplc1_2 = sir1.toUplcOptimized()

        val cbor1_1 = uplc1_1.plutusV3.cborByteString
        val cbor1_2 = uplc1_2.plutusV3.cborByteString

        assert(cbor1_1 == cbor1_2, "Compiled scripts should be the same")
    }

    test("create two JVM-s and compile same script each in own JVM") {
        val classpath = System.getProperty("java.class.path")
        val pb1 = new ProcessBuilder(
          "java",
          "-cp",
          classpath,
          "scalus.testing.regression.hydrozoa20250804.disputeResolutionValidatorRun"
        )
        val process1 = pb1.start()
        val output1 = readOutput(process1)
        val exitCode1 = process1.waitFor()
        val pb2 = new ProcessBuilder(
          "java",
          "-cp",
          classpath,
          "scalus.testing.regression.hydrozoa20250804.disputeResolutionValidatorRun"
        )
        val process2 = pb2.start()
        val output2 = readOutput(process2)
        val exitCode2 = process2.waitFor()
//        println(s"Output from first JVM: ${output1.get()}")
//        println(s"Output from second JVM: ${output2.get()}")
        assert(exitCode1 == 0, "First JVM should exit with code 0")
        assert(exitCode2 == 0, "Second JVM should exit with code 0")
        assert(output1.get() == output2.get(), "Outputs from both JVMs should be the same")
    }

    def readOutput(process: Process): AtomicReference[String] = {
        val output = new AtomicReference[String]()
        val inputStream = process.getInputStream
        val reader = scala.io.Source.fromInputStream(inputStream)
        val lines = reader.getLines().mkString("")
        output.set(lines)
        output
    }

}
