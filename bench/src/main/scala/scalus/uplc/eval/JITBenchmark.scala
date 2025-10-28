package scalus.uplc.eval

import org.openjdk.jmh.annotations.*
import scalus.*
import scalus.uplc.{DeBruijnedProgram, DefaultFun, Program, Term}

import java.nio.file.{Files, Path, Paths}
import java.util.concurrent.TimeUnit

@State(Scope.Benchmark)
class JITBenchmark:
    @Param(
      Array(
        "auction_1-1.flat",
        "auction_1-2.flat",
        "auction_1-3.flat",
        "auction_1-4.flat"
      )
    )
    private var file: String = ""
    private var path: Path = null
    private var program: DeBruijnedProgram | Null = null

    // Compile with all JIT implementations
    private var jittedMincont: (Logger, BudgetSpender, MachineParams) => Any | Null = null
    private var jittedNativeStack: (Logger, BudgetSpender, MachineParams) => Any | Null = null
    private var jittedHybrid: (Logger, BudgetSpender, MachineParams) => Any | Null = null

    private val params = MachineParams.defaultPlutusV2PostConwayParams

    @Setup
    def readProgram(): Unit = {
        path = Paths.get(s"src/main/resources/data/$file")
        val bytes = Files.readAllBytes(path)
        program = DeBruijnedProgram.fromFlatEncoded(bytes)
        jittedMincont = Test.getJitted(program.toProgram, JITImplementation.Mincont)
        jittedNativeStack = Test.getJitted(program.toProgram, JITImplementation.NativeStack)
        jittedHybrid = Test.getJitted(program.toProgram, JITImplementation.Hybrid())
    }

    @Benchmark
    @BenchmarkMode(Array(Mode.AverageTime))
    @OutputTimeUnit(TimeUnit.MICROSECONDS)
    def benchJIT_Mincont(): Unit = {
        jittedMincont(NoLogger, NoBudgetSpender, params)
    }

    @Benchmark
    @BenchmarkMode(Array(Mode.AverageTime))
    @OutputTimeUnit(TimeUnit.MICROSECONDS)
    def benchJIT_NativeStack(): Unit = {
        jittedNativeStack(NoLogger, NoBudgetSpender, params)
    }

    @Benchmark
    @BenchmarkMode(Array(Mode.AverageTime))
    @OutputTimeUnit(TimeUnit.MICROSECONDS)
    def benchJIT_Hybrid(): Unit = {
        jittedHybrid(NoLogger, NoBudgetSpender, params)
    }

private object Test {
    private val path = Paths.get(s"src/main/resources/data/auction_1-1.flat")
    private val bytes = Files.readAllBytes(path)
    private val program = DeBruijnedProgram.fromFlatEncoded(bytes).toProgram

    private def collect(t: Term): Set[DefaultFun] = t match
        case Term.Var(name)          => Set()
        case Term.LamAbs(name, term) => collect(term)
        case Term.Apply(f, arg)      => collect(f) ++ collect(arg)
        case Term.Force(term)        => collect(term)
        case Term.Delay(term)        => collect(term)
        case Term.Const(const)       => Set()
        case Term.Builtin(bn)        => Set(bn)
        case Term.Error              => Set()
        case Term.Constr(tag, args)  => args.flatMap(collect).toSet
        case Term.Case(arg, cases)   => collect(arg) ++ cases.flatMap(collect)

    private val builtins = collect(program.term).toSeq.sorted

    def getJitted(prog: Program, impl: JITImplementation): (Logger, BudgetSpender, MachineParams) => Any = {
        println(s"Starting JIT compilation (${impl.name}) at ${System.currentTimeMillis()}")
        val start = System.currentTimeMillis()
        println(s"Calling ${impl.name}.compile...")
        val r = impl.compile(prog.term)
        val end = System.currentTimeMillis()
        println(s"JIT compilation (${impl.name}) completed in ${end - start} ms")
        r
    }

    @main def run() = {
        given PlutusVM = PlutusVM.makePlutusV2VM()
        println("Builtins used:")
        println(builtins)
        println("\nProgram evaluation (CeK):")
        println(program.evaluateDebug)

        // Test all JIT implementations
        for impl <- JITImplementation.standaloneImplementations do
            println(s"\n=== Testing ${impl.name} JIT ===")
            println(s"Stack-safe: ${impl.isStackSafe}")
            val spender = CountingBudgetSpender()
            val jitted = getJitted(program, impl)
            val result = jitted(NoLogger, spender, MachineParams.defaultPlutusV2PostConwayParams)
            println(s"Result: $result")
            println(s"Budget: ${spender.getSpentBudget.showJson}")
    }
}
