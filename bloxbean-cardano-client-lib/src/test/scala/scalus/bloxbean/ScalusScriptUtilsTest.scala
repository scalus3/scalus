package scalus.bloxbean

import com.bloxbean.cardano.client.plutus.spec.*
import org.scalatest.funsuite.AnyFunSuite
import scalus.*
import scalus.builtin.Data
import scalus.compiler.compile
import scalus.uplc.*
import scalus.uplc.Constant.given
import scalus.uplc.Term.asTerm
import scalus.uplc.eval.PlutusVM

import java.math.BigInteger

class ScalusScriptUtilsTest extends AnyFunSuite:
    private val program =
        compile((one: Data, two: Data) => one.toBigInt + two.toBigInt)
            .toUplc()
            .plutusV2
            .doubleCborHex

    given PlutusVM = PlutusVM.makePlutusV2VM()

    test("applyParamsToScript with PlutusData varargs") {
        val applied = ScalusScriptUtils.applyParamsToScript(
          program,
          BigIntPlutusData(BigInteger.ONE),
          BigIntPlutusData(BigInteger.TWO)
        )
        val script = DeBruijnedProgram.fromDoubleCborHex(applied)
        assert(script.evaluate == BigInt(3).asTerm)
    }

    test("applyParamsToScript with ListPlutusData") {
        val params =
            ListPlutusData
                .builder()
                .plutusDataList(
                  java.util.List
                      .of(BigIntPlutusData(BigInteger.ONE), BigIntPlutusData(BigInteger.TWO))
                )
                .build();
        val applied = ScalusScriptUtils.applyParamsToScript(program, params)
        val script = DeBruijnedProgram.fromDoubleCborHex(applied)
        assert(script.evaluate == BigInt(3).asTerm)
    }
