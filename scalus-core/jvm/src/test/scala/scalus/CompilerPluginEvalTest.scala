package scalus

import org.scalatest.funsuite.AnyFunSuite
import scalus.Compiler.compile
import scalus.builtin.ByteString.*
import scalus.builtin.{Builtins, ByteString, JVMPlatformSpecific}
import scalus.cardano.ledger.{CardanoInfo, Language}
import scalus.ledger.api.v1.*
import scalus.sir.*
import scalus.sir.SIR.*
import scalus.uplc.*
import scalus.uplc.Term.asTerm
import scalus.uplc.Constant.given
import scalus.uplc.eval.Result.Success
import scalus.uplc.eval.{MachineParams, PlutusVM, Result}

import scala.language.implicitConversions

class CompilerPluginEvalTest extends AnyFunSuite {

    private given PlutusVM = PlutusVM.makePlutusV2VM()

    given scalus.Compiler.Options = scalus.Compiler.Options(
      targetLoweringBackend = scalus.sir.TargetLoweringBackend.SirToUplcV3Lowering,
      generateErrorTraces = true,
      optimizeUplc = true,
      debug = false
    )

    val deadbeef = Constant.ByteString(hex"deadbeef")

    test("compile Tuple2 construction/matching") {
        val compiled = compile {
            type Pair = (Boolean, Boolean)
            val t: Pair = (true, false)
            t match
                case (a, _) => a && t._2
        }
        // println(compiled.show)
        val evaled = compiled.toUplc().evaluate
        assert(evaled == false.asTerm)
    }

    test("compile match on a case class") {
        val compiled = compile {
            val pkh = new scalus.ledger.api.v1.PubKeyHash(hex"deadbeef")
            pkh match
                case PubKeyHash(hash) => hash
        }
        // println(compiled.show)
        val evaled = compiled.toUplc().evaluate
        assert(evaled == hex"deadbeef".asTerm)
    }

    test("compile match on ADT") {

        import scalus.prelude.List
        import scalus.prelude.List.*
        val compiled = compile {
            val ls: List[BigInt] = single(BigInt(1))
            ls match
                case Cons(h, _) => h
                case Nil        => BigInt(0)
        }
        // println(compiled.show)
        val compiledToUplc = compiled.toUplc()
        // println(s"uplc:${compiledToUplc.show} ")
        try
            val evaled = compiledToUplc.evaluate
            // println(evaled.show)
            assert(evaled == BigInt(1).asTerm)
        catch
            case e: Throwable =>
                println(s"compile match on ADT: error in evaled: ${e.getMessage}")
                println(s"compile match on ADT: SIR=${compiled.pretty.render(100)}")
                println(s"compile match on ADT: UPLC=${compiledToUplc.pretty.render(100)}")
                compiled match
                    case SIR.Decl(data, term) =>
                        println(
                          s"compile match on ADT:dataDecl, ${data.name}, constrNames=${data.constructors
                                  .map(_.name)}"
                        )
                    case SIR.Let(bindings, body, flags, _) =>
                        println(
                          s"compile match on ADT: bindings=${bindings.mkString("\n")}"
                        )
                    case _ =>
                        println(s"compile match on ADT: not a Let, but $compiled")
                throw e
    }

    test("compile wildcard match on ADT") {
        import scalus.prelude.These
        val compiled = compile {
            val t: These[BigInt, Boolean] = new These.This(BigInt(1))
            t match
                case These.This(h) => h
                case _             => BigInt(0)
        }
        val uplc = compiled.toUplc()
        val evaled = uplc.evaluate
        assert(evaled == BigInt(1).asTerm)
    }

    test("compile inner matches") {
        import scalus.prelude.List
        import scalus.prelude.List.*
        val compiled = compile {
            val ls: List[(BigInt, TxOutRef)] =
                List.single((1, new TxOutRef(new TxId(hex"deadbeef"), 2)))
            ls match
                case Cons(h @ (a, TxOutRef(TxId(_), idx)), _) => a + idx
                case Nil                                      => BigInt(0)
        }
        // println(compiled.show)
        val evaled = compiled.toUplc().evaluate
        // println(evaled.show)
        assert(evaled == BigInt(3).asTerm)
    }

    test("compile multiple inner matches") {
        val compiled = compile {
            ((true, "test"), (false, "test")) match
                case ((a, _), (b, _)) => a == b
        }
        // println(compiled.show)
        val evaled = compiled.toUplc().evaluate
        // println(evaled.show)
        assert(evaled == false.asTerm)
    }

    test("? operator produces a debug log") {
        import scalus.prelude.?
        val compiled = compile {
            val oneEqualsTwo = BigInt(1) == BigInt(2)
            oneEqualsTwo.?
        }
        val script = compiled.toUplc().plutusV2
        script.evaluateDebug match
            case Result.Success(evaled, _, _, logs) =>
                assert(evaled == false.asTerm)
                val logExpr = "oneEqualsTwo \\? False.*".r
                logs.head match {
                    case `logExpr`() =>
                    // log is as expected
                    case _ =>
                        fail(s"Unexpected logs: $logs")
                }
                // assert(logs == List("oneEqualsTwo ? False: { mem: 0.002334, cpu: 0.539980 }"))
            case Result.Failure(exception, _, _, _) => fail(exception)
    }

    test("compile large script") {
        // this test ensures that the compiler can handle large scripts
        inline def generate(n: Int): String =
            if n == 0 then "asdfasdfasdfasdfasdfasdfasdfasdfasdfasdfasdfasdfasdf"
            else
                Builtins.appendString(
                  generate(n - 1),
                  "asdfasdfasdfasdfasdfasdfasdfasdfasdfasdfasdfasdfasdfasdfasdfasdfasdfasdf"
                      + "asdfasdfasdfasdfasdfasdfasdfasdfasdfasdfasdfasdfasdfasdfasdfasdfasdfasdf"
                      + "asdfasdfasdfasdfasdfasdfasdfasdfasdfasdfasdfasdfasdfasdfasdfasdfasdfasdf"
                      + "asdfasdfasdfasdfasdfasdfasdfasdfasdfasdfasdfasdfasdfasdfasdfasdfasdfasdf"
                      + "asdfasdfasdfasdfasdfasdfasdfasdfasdfasdfasdfasdfasdfasdfasdfasdfasdfasdf"
                      + "asdfasdfasdfasdfasdfasdfasdfasdfasdfasdfasdfasdfasdfasdfasdfasdfasdfasdf"
                      + "asdfasdfasdfasdfasdfasdfasdfasdfasdfasdfasdfasdfasdfasdfasdfasdfasdfasdf"
                      + "asdfasdfasdfasdfasdfasdfasdfasdfasdfasdfasdfasdfasdfasdfasdfasdfasdfasdf"
                      + "asdfasdfasdfasdfasdfasdfasdfasdfasdfasdfasdfasdfasdfasdfasdfasdfasdfasdf"
                      + "asdfasdfasdfasdfasdfasdfasdfasdfasdfasdfasdfasdfasdfasdfasdfasdfasdfasdf"
                      + "asdfasdfasdfasdfasdfasdfasdfasdfasdfasdfasdfasdfasdfasdfasdfasdfasdfasdf"
                      + "asdfasdfasdfasdfasdfasdfasdfasdfasdfasdfasdfasdfasdfasdfasdfasdfasdfasdf"
                      + "asdfasdfasdfasdfasdfasdfasdfasdfasdfasdfasdfasdfasdfasdfasdfasdfasdfasdf"
                )

        // this generates a script with 99 calls to appendString
        // appendString(appendString(..., "asdf..."), ..., "asdf...")
        val compiled = compile {
            generate(99)
        }
        assert(compiled.toUplc().plutusV3.flatEncoded.length == 93652)
    }

    test("compile and eval custom Builtins") {
        val platform = new JVMPlatformSpecific {
            override def sha2_256(bs: ByteString): ByteString = hex"deadbeef"
        }
        object CustomBuiltins extends Builtins(using platform)

        given PlutusVM =
            val params = MachineParams.defaultPlutusV3Params
            val semanticVariant = BuiltinSemanticsVariant.fromProtocolAndPlutusVersion(
              CardanoInfo.mainnet.majorProtocolVersion,
              Language.PlutusV3
            )
            new PlutusVM(
              Language.PlutusV3,
              params,
              semanticVariant,
              platform
            )

        val sir = compile(CustomBuiltins.sha2_256(hex"12"))
        // check that SIRCompiler compiles the custom builtin
        // check that the custom builtin is correctly evaluated on the JVM
        assert(CustomBuiltins.sha2_256(hex"12") == hex"deadbeef")
        // check that PlutusVM uses the custom builtin
        assert(sir.toUplc().evaluate == hex"deadbeef".asTerm)
    }

    test("compile valargs") {
        // pending

        import scalus.prelude.*
        val compiled = compile {
            def sum(x: BigInt*): BigInt = {
                x.list.foldLeft(BigInt(0))(_ + _)
            }

            val result = sum(1, 2, 3, 4, 5)
            result
        }

        // println(s"sir=${compiled.pretty.render(100)}")
        val uplc = compiled.toUplc(generateErrorTraces = true)
        val evaluated = uplc.evaluate
        assert(evaluated == BigInt(15).asTerm)

        def mySum(x: BigInt*) = {
            x.list.foldLeft(BigInt(0))(_ + _)
        }
        val jvmResult = mySum(1, 2, 3, 4, 5)
        assert(jvmResult == 15)

    }

    test("compile inner matches on enum") {
        import scalus.prelude.*
        val sir = compile { (x: List[Option[BigInt]]) =>
            x match
                case List.Cons(Option.Some(v), _) => v
                case List.Cons(Option.None, _)    => BigInt(0)
                case List.Nil                     => BigInt(-1)
        }

        val uplc = sir.toUplc(generateErrorTraces = true)
        val arg1 = compile {
            List.Cons(Option.Some(BigInt(42)), List.Nil)
        }.toUplc()
        val arg2 = compile {
            List.Cons(Option.None, List.Nil)
        }.toUplc()
        val arg3 = compile {
            List.Nil: List[Option[BigInt]]
        }.toUplc()
        val r1 = (uplc $ arg1).evaluate
        assert(r1 == BigInt(42).asTerm)
        val r2 = (uplc $ arg2).evaluate
        assert(r2 == BigInt(0).asTerm)
        val r3 = (uplc $ arg3).evaluate
        assert(r3 == BigInt(-1).asTerm)
    }

}
