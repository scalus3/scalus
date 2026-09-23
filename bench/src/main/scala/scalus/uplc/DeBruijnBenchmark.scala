package scalus.uplc

import org.openjdk.jmh.annotations.Benchmark
import org.openjdk.jmh.annotations.BenchmarkMode
import org.openjdk.jmh.annotations.Mode
import org.openjdk.jmh.annotations.OutputTimeUnit
import org.openjdk.jmh.annotations.Param
import org.openjdk.jmh.annotations.Scope
import org.openjdk.jmh.annotations.Setup
import org.openjdk.jmh.annotations.State

import java.nio.file.Files
import java.nio.file.Paths
import java.util.concurrent.TimeUnit
import scala.annotation.nowarn

/** Benchmark for [[DeBruijn]] conversions of the Plutus `validation` benchmark programs.
  *
  *   - `fromDeBruijn`: De Bruijn indices to named variables, as `DeBruijnedProgram.toProgram` does
  *     after decoding a script.
  *   - `toDeBruijn`: named variables to De Bruijn indices, as `Program.deBruijnedProgram` does for
  *     a compiled script.
  */
@State(Scope.Benchmark)
class DeBruijnBenchmark:
    @Param(Array("auction_1-1.flat", "future-increase-margin-5.flat", "stablecoin_1-5.flat"))
    @nowarn("msg=unset private variable")
    private var file: String = ""
    private var deBruijned: Term = null
    private var named: Term = null

    @Setup
    def readProgram() = {
        val bytes = Files.readAllBytes(Paths.get(s"src/main/resources/data/$file"))
        deBruijned = DeBruijnedProgram.fromFlatEncoded(bytes).term
        named = DeBruijn.fromDeBruijnTerm(deBruijned)
    }

    @Benchmark
    @BenchmarkMode(Array(Mode.AverageTime))
    @OutputTimeUnit(TimeUnit.MICROSECONDS)
    def fromDeBruijn() = DeBruijn.fromDeBruijnTerm(deBruijned)

    @Benchmark
    @BenchmarkMode(Array(Mode.AverageTime))
    @OutputTimeUnit(TimeUnit.MICROSECONDS)
    def toDeBruijn() = DeBruijn.deBruijnTerm(named)
