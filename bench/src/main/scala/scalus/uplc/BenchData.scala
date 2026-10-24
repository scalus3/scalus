package scalus.uplc

import java.nio.file.{Files, Paths}

/** The `.flat` scripts of the benchmarks: `src/main/resources/data` (78 of them), or the directory
  * in the system property `bench.data`, for example plutus-vm-benchmark's `data/plutus_use_cases`
  * (all 89):
  * {{{
  * sbtn "bench/Jmh/run -jvmArgsAppend -Dbench.data=<dir> -p file=auction_1-1.flat .*CekJVMBenchmark.*"
  * }}}
  */
object BenchData {

    /** The bytes of the script `file`, for example `auction_1-1.flat`. */
    def read(file: String): Array[Byte] =
        Files.readAllBytes(
          Paths.get(sys.props.getOrElse("bench.data", "src/main/resources/data"), file)
        )
}
