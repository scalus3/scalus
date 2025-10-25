package scalus.uplc.eval

import org.openjdk.jmh.annotations.*
import scalus.*
import scalus.uplc.{DeBruijnedProgram, DefaultFun, Program, Term}

import java.nio.file.{Files, Paths}
import java.util.concurrent.TimeUnit

@State(Scope.Benchmark)
class JITBenchmark:
    private var program: DeBruijnedProgram | Null = null
    private val jitted = Test.getJitted()
    private val params = MachineParams.defaultPlutusV2PostConwayParams

    @Setup
    def readProgram(): Unit = {
        val path = Paths.get(s"src/main/resources/data/auction_1-1.flat")
        val bytes = Files.readAllBytes(path)
        program = DeBruijnedProgram.fromFlatEncoded(bytes)
    }

    @Benchmark
    @BenchmarkMode(Array(Mode.AverageTime))
    @OutputTimeUnit(TimeUnit.MICROSECONDS)
    def benchJIT_auction_1_1(): Unit = {
        jitted(NoLogger, NoBudgetSpender, params)
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

    def getJitted(): (Logger, BudgetSpender, MachineParams) => Any = {
        println(s"Starting JIT compilation at ${System.currentTimeMillis()}")
        val start = System.currentTimeMillis()
        println("Calling JIT.jitUplc...")
        val r = JIT.jitUplc(program.term)
        val end = System.currentTimeMillis()
        println(s"JIT completed in ${end - start} ms")
        r
    }

    @main def run() = {
        given PlutusVM = PlutusVM.makePlutusV2VM()
        println(builtins)
        println(program.evaluateDebug)
        //        println(program.showHighlighted)
        //        println(JIT.jitUplc(program.term)())
        val spender = CountingBudgetSpender()
        getJitted()(NoLogger, spender, MachineParams.defaultPlutusV2PostConwayParams)
        println(spender.getSpentBudget.showJson)
    }
}
