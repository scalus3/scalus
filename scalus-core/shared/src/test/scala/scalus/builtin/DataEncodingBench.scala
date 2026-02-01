package scalus.builtin

import org.scalatest.funsuite.AnyFunSuite
import scalus.*
import scalus.uplc.builtin.Data
import scalus.uplc.builtin.Data.*
import scalus.uplc.builtin.ToData
import scalus.uplc.builtin.FromData
import scalus.uplc.Program
import scalus.cardano.ledger.ExUnits

case class LargeRecord(
    f1: BigInt,
    f2: BigInt,
    f3: BigInt,
    f4: BigInt,
    f5: BigInt,
    f6: BigInt,
    f7: BigInt,
    f8: BigInt,
    f9: BigInt,
    f10: BigInt
)

@Compile
object LargeRecord {
    given ToData[LargeRecord] = ToData.derived
    given FromData[LargeRecord] = FromData.derived
}

case class DeepSum(
    f: LargeRecord
)
@Compile
object DeepSum {
    given ToData[DeepSum] = ToData.derived
    given FromData[DeepSum] = FromData.derived
}

class DataEncodingBench extends AnyFunSuite {
    test("Baseline performance measurement") {
        val record = LargeRecord(1, 2, 3, 4, 5, 6, 7, 8, 9, 10)
        
        // Measure ToData size
        val toDataProg = compile { (r: LargeRecord) => r.toData }
        val toDataUplc = toDataProg.toUplc()
        println(s"ToData UPLC size: ${toDataUplc.flatEncoded.length} bytes")
        
        // Measure FromData size and cost
        val fromDataProg = compile { (d: Data) => 
            val r = d.to[LargeRecord]
            r.f1 + r.f10
        }
        val fromDataUplc = fromDataProg.toUplc()
        println(s"FromData UPLC size: ${fromDataUplc.flatEncoded.length} bytes")
        
        val data = record.toData
        val result = (fromDataUplc $ data).evaluateDebug
        println(s"FromData Execution Cost: ${result.budget}")
        println(s"FromData Logs: ${result.logs.mkString(", ")}")
        
        assert(result.isSuccess)
    }
}
