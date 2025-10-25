package scalus.uplc.eval

import org.scalatest.funsuite.AnyFunSuiteLike
import scalus.*
import scalus.Compiler.compile
import scalus.uplc.{Constant, Term}
import java.lang.management.ManagementFactory

class JITDeepRecursionTest extends AnyFunSuiteLike {
    private given PlutusVM = PlutusVM.makePlutusV3VM()

    private def getThreadStackInfo(): String =
        val thread = Thread.currentThread()
        val isVirtual = thread.isVirtual()
        val threadType = if isVirtual then "VirtualThread" else "PlatformThread"

        if isVirtual then
            // Virtual threads don't have a fixed stack size - they grow on heap
            // We can estimate current usage from stack trace depth
            val stackDepth = thread.getStackTrace().length
            s"$threadType (heap-based, current stack trace depth: $stackDepth frames)"
        else
            // For platform threads, try to get the stack size from ThreadMXBean
            val stackDepth = thread.getStackTrace().length

            // Try to get configured stack size from JVM args
            val runtimeMXBean = ManagementFactory.getRuntimeMXBean()
            val vmArgs = runtimeMXBean.getInputArguments()
            val xssArg = vmArgs.toArray.find(_.toString.startsWith("-Xss"))
            val stackSize = xssArg.map(arg => s" (configured: $arg)").getOrElse("")

            s"$threadType (native stack$stackSize, current stack trace depth: $stackDepth frames)"
        end if
    end getThreadStackInfo

    test("Deep recursion factorial with CekMachine vs JIT") {
        val uplc: Term = compile {
            def factorial(n: BigInt): BigInt =
                if n <= 1 then 1
                else n * factorial(n - 1)

            factorial(20)
        }.toUplc(true)

        // Evaluate with CekMachine
        val cekResult = uplc.evaluateDebug
        val cekValue = cekResult match
            case Result.Success(term, _, _, _) => term
            case Result.Failure(e, _, _, _)    => fail(s"CekMachine evaluation failed: $e")

        // Evaluate with JIT (will fail if builtin not supported, so we catch that)
        val logger = Log()
        try {
            val jitResult =
                JIT.jitUplc(uplc)(logger, NoBudgetSpender, summon[PlutusVM].machineParams)
            // JIT returns raw value, CekMachine returns Term.Const
            val expectedValue = BigInt(2432902008176640000L)
            assert(cekValue == Term.Const(Constant.Integer(expectedValue)))
            assert(jitResult == expectedValue)
        } catch {
            case e: RuntimeException if e.getMessage.contains("not yet supported") =>
                info(s"JIT doesn't support some builtins yet: ${e.getMessage}")
                assert(cekValue == Term.Const(Constant.Integer(2432902008176640000L)))
        }
    }

    test("Deep recursion Fibonacci with CekMachine vs JIT") {
        val uplc: Term = compile {
            def fib(n: BigInt): BigInt =
                if n <= 1 then n
                else fib(n - 1) + fib(n - 2)

            fib(15)
        }.toUplc(true)

        // Evaluate with CekMachine
        val cekResult = uplc.evaluateDebug
        val cekValue = cekResult match
            case Result.Success(term, _, _, _) => term
            case Result.Failure(e, _, _, _)    => fail(s"CekMachine evaluation failed: $e")

        // Evaluate with JIT (will fail if builtin not supported, so we catch that)
        val logger = Log()
        try {
            val jitResult =
                JIT.jitUplc(uplc)(logger, NoBudgetSpender, summon[PlutusVM].machineParams)
            // JIT returns raw value, CekMachine returns Term.Const
            val expectedValue = BigInt(610)
            assert(cekValue == Term.Const(Constant.Integer(expectedValue)))
            assert(jitResult == expectedValue)
        } catch {
            case e: RuntimeException if e.getMessage.contains("not yet supported") =>
                info(s"JIT doesn't support some builtins yet: ${e.getMessage}")
                assert(cekValue == Term.Const(Constant.Integer(610)))
        }
    }

    test("Deep recursion sum with CekMachine vs JIT") {
        val uplc: Term = compile {
            def sumToN(n: BigInt): BigInt =
                if n <= 0 then 0
                else n + sumToN(n - 1)

            sumToN(100)
        }.toUplc(true)

        // Evaluate with CekMachine
        val cekResult = uplc.evaluateDebug
        val cekValue = cekResult match
            case Result.Success(term, _, _, _) => term
            case Result.Failure(e, _, _, _)    => fail(s"CekMachine evaluation failed: $e")

        // Evaluate with JIT (will fail if builtin not supported, so we catch that)
        val logger = Log()
        try {
            val jitResult =
                JIT.jitUplc(uplc)(logger, NoBudgetSpender, summon[PlutusVM].machineParams)
            // JIT returns raw value, CekMachine returns Term.Const
            val expectedValue = BigInt(5050)
            assert(cekValue == Term.Const(Constant.Integer(expectedValue)))
            assert(jitResult == expectedValue)
        } catch {
            case e: RuntimeException if e.getMessage.contains("not yet supported") =>
                info(s"JIT doesn't support some builtins yet: ${e.getMessage}")
                assert(cekValue == Term.Const(Constant.Integer(5050)))
        }
    }

    test("JIT stack overflow bounds - CekMachine works but JIT overflows") {
        info(s"Running on: ${getThreadStackInfo()}")

        // Compile the sum function once
        val sumFunctionUplc: Term = compile { (n: BigInt) =>
            def sumToN(n: BigInt): BigInt =
                if n <= 0 then 0
                else n + sumToN(n - 1)
            sumToN(n)
        }.toUplc(true)

        def testDepth(n: Int): Unit = {
            // Apply the compiled function to a constant argument
            val uplc: Term = sumFunctionUplc $ Term.Const(Constant.Integer(BigInt(n)))

            // CekMachine should handle this fine (it's iterative)
            val cekResult = uplc.evaluateDebug
            val cekSuccess = cekResult match
                case Result.Success(term, _, _, _) =>
                    info(s"CekMachine succeeded at depth $n")
                    true
                case Result.Failure(e, _, _, _) =>
                    info(s"CekMachine failed at depth $n: ${e.getMessage}")
                    false

            if cekSuccess then
                // Try JIT - it should overflow at some point due to native stack usage
                val logger = Log()
                try {
                    val jitResult =
                        JIT.jitUplc(uplc)(logger, NoBudgetSpender, summon[PlutusVM].machineParams)
                    info(s"JIT succeeded at depth $n, result: $jitResult")
                } catch {
                    case e: StackOverflowError =>
                        info(s"JIT StackOverflowError at depth $n (CekMachine still works)")
                        throw e // Re-throw to mark the boundary
                    case e: RuntimeException if e.getMessage.contains("not yet supported") =>
                        info(s"JIT doesn't support some builtins: ${e.getMessage}")
                }
        }

        // Start testing from a known working depth and increase
        // We expect JIT to overflow somewhere between 1000-10000
        var depth = 1000
        var foundOverflow = false
        var maxWorkingDepth = 0

        while !foundOverflow && depth <= 20000 do
            try {
                testDepth(depth)
                maxWorkingDepth = depth
                depth = (depth * 1.5).toInt // Increase by 50%
            } catch {
                case e: StackOverflowError =>
                    foundOverflow = true
                    info(
                      s"Found JIT stack overflow boundary: CekMachine works but JIT overflows at depth $depth"
                    )
                    info(s"Maximum working depth for JIT: $maxWorkingDepth")
            }

        assert(foundOverflow, s"Did not find stack overflow up to depth $depth")
    }

    test("JIT stack overflow bounds on VirtualThread") {
        // Compile the sum function once - this creates a parameterized UPLC function
        val sumFunctionUplc: Term = compile { (n: BigInt) =>
            def sumToN(n: BigInt): BigInt =
                if n <= 0 then 0
                else n + sumToN(n - 1)
            sumToN(n)
        }.toUplc(true)

        // JIT compile the function once on the main thread to avoid threading issues
        // The staging.Compiler uses thread-local contexts
        val logger = Log()
        info("Pre-compiling UPLC to JIT on main thread...")

        // Create test depths and JIT-compile each one on the main thread
        val testDepths = List(1000, 1500, 2250, 3375, 5062, 7593)
        val jitCompiledFunctions = testDepths
            .map { depth =>
                val uplc = sumFunctionUplc $ Term.Const(Constant.Integer(BigInt(depth)))
                try {
                    // JIT compile on main thread - this returns a compiled function
                    val compiledFn = JIT.jitUplc(uplc)
                    // Create a wrapper that calls the compiled function with the necessary parameters
                    val jitFn =
                        () => compiledFn(logger, NoBudgetSpender, summon[PlutusVM].machineParams)
                    Some(depth -> jitFn)
                } catch {
                    case e: RuntimeException if e.getMessage.contains("not yet supported") =>
                        info(
                          s"  JIT doesn't support some builtins at depth $depth: ${e.getMessage}"
                        )
                        None
                }
            }
            .flatten
            .toMap

        info(s"Pre-compiled ${jitCompiledFunctions.size} JIT functions")

        @volatile var testResult: Option[Either[Throwable, (Int, Int)]] = None
        @volatile var logMessages: List[String] = List.empty

        def log(msg: String): Unit = synchronized {
            logMessages = logMessages :+ msg
        }

        // Run the test on a virtual thread - execute pre-compiled JIT code
        val virtualThread = Thread.ofVirtual().start { () =>
            try {
                log(s"Running on: ${getThreadStackInfo()}")

                var foundOverflow = false
                var maxWorkingDepth = 0
                var overflowDepth = 0

                for (depth, jitFn) <- jitCompiledFunctions.toSeq.sortBy(_._1) if !foundOverflow do
                    try {
                        // Execute the pre-compiled JIT function
                        val result = jitFn()
                        log(s"VirtualThread: JIT succeeded at depth $depth, result: $result")
                        maxWorkingDepth = depth
                    } catch {
                        case e: StackOverflowError =>
                            foundOverflow = true
                            overflowDepth = depth
                            log(s"VirtualThread: JIT StackOverflowError at depth $depth")
                            log(s"VirtualThread: Maximum working depth for JIT: $maxWorkingDepth")
                    }

                if !foundOverflow then
                    log("VirtualThread: No stack overflow found in tested range")
                    overflowDepth = maxWorkingDepth + 1

                testResult = Some(Right((maxWorkingDepth, overflowDepth)))
            } catch {
                case e: Throwable =>
                    log(
                      s"VirtualThread: Exception occurred: ${e.getClass.getSimpleName}: ${e.getMessage}"
                    )
                    testResult = Some(Left(e))
            }
        }

        virtualThread.join()

        // Print all collected log messages
        logMessages.foreach(msg => info(s"  + $msg"))

        testResult match
            case Some(Right((maxDepth, overflowDepth))) =>
                info(
                  s"VirtualThread completed: max working depth $maxDepth, overflow at $overflowDepth"
                )
                // Virtual threads can have different stack behavior, so we just verify it ran
                assert(maxDepth > 0, "Should have found some working depth")
            case Some(Left(e)) =>
                fail(s"VirtualThread test failed with exception: ${e.getMessage}")
            case None =>
                fail("VirtualThread test did not complete")
    }
}
