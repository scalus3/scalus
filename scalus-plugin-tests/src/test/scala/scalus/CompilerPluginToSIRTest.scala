package scalus

import org.scalatest.funsuite.AnyFunSuite
import org.scalatestplus.scalacheck.ScalaCheckPropertyChecks
import scalus.builtin.ByteString.*
import scalus.builtin.Data.toData
import scalus.builtin.{Builtins, ByteString, Data, ToData}
import scalus.compiler.compile
import scalus.uplc.*

import scala.annotation.nowarn

@Compile
object TotoDataInstances {
    given Data.ToData[BigInt] = (a: BigInt) => builtin.Builtins.iData(a)
    given Data.ToData[String] = (a: String) => builtin.Builtins.iData(1)
}

case class Test(a: BigInt, b: String)
@Compile
object Test {
    import TotoDataInstances.given
    given Data.ToData[Test] = ToData.derived
}

class CustomError extends Exception("custom error")

class CompilerPluginToSIRTest extends AnyFunSuite with ScalaCheckPropertyChecks:
    val deadbeef = Constant.ByteString(hex"deadbeef")

    test("compile error") {
        val sir = compile {
            @nowarn
            def err(msg: String): Nothing = throw new RuntimeException(msg)
            err("test")
        }
        // println(sir)
        // println(sir.pretty.render(100))
        // println(sir.toUplcOptimized().showHighlighted)
    }
