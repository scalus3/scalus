package scalus

import org.scalatest.funsuite.AnyFunSuite
import org.scalatestplus.scalacheck.ScalaCheckPropertyChecks
import scalus.Compiler.{compile, fieldAsData}
import scalus.builtin.ByteString.*
import scalus.builtin.{Builtins, ByteString, Data}
import scalus.prelude.List.{Cons, Nil}
import scalus.sir.SIR.*
import scalus.sir.*
import scalus.sir.SIRType.{Boolean, Fun, TypeVar}
import scalus.uplc.*
import scalus.uplc.eval.Result.Success
import scalus.uplc.eval.{PlutusVM, Result}

import scala.annotation.nowarn
import scala.collection.immutable
import scala.language.implicitConversions

object CompilerPluginToSIRSpecScope:

    case class ThreeInts(a: BigInt, b: BigInt, c: BigInt)

end CompilerPluginToSIRSpecScope

class CompilerPluginToSIRTest extends AnyFunSuite with ScalaCheckPropertyChecks:
    private given PlutusVM = PlutusVM.makePlutusV2VM()
    val deadbeef = Constant.ByteString(hex"deadbeef")

    val sirData = SIRType.Data
    val sirBool = SIRType.Boolean
    val sirInt = SIRType.Integer
    val sirString = SIRType.String
    val sirByteString = SIRType.ByteString
    val sirVoid = SIRType.Unit
    def sirList(tpe: SIRType) = SIRType.List(tpe)
    def sirBuiltinList(tpe: SIRType) = SIRType.BuiltinList(tpe)
    def sirPair(t1: SIRType, t2: SIRType) = SIRType.BuiltinPair(t1, t2)

    def sirConst(x: Int) = Const(Constant.Integer(x), SIRType.Integer, AnnotationsDecl.empty)
    def sirConst(x: BigInt) = Const(Constant.Integer(x), SIRType.Integer, AnnotationsDecl.empty)
    def sirConst(x: Boolean) = Const(Constant.Bool(x), SIRType.Boolean, AnnotationsDecl.empty)
    def sirConst(x: String) = Const(Constant.String(x), SIRType.String, AnnotationsDecl.empty)
    def sirConst(x: ByteString) =
        Const(Constant.ByteString(x), SIRType.ByteString, AnnotationsDecl.empty)
    def sirConst(x: Data) = Const(Constant.Data(x), SIRType.Data, AnnotationsDecl.empty)
    def sirConstUnit = Const(Constant.Unit, SIRType.Unit, AnnotationsDecl.empty)

    def AnE = AnnotationsDecl.empty

    given scalus.Compiler.Options = scalus.Compiler.Options(
      targetLoweringBackend = scalus.Compiler.TargetLoweringBackend.SirToUplcV3Lowering,
      generateErrorTraces = true,
      optimizeUplc = true,
      debug = false
    )

    test("compile literals") {
        assert(
          compile(false) ~=~ Const(Constant.Bool(false), SIRType.Boolean, AnnotationsDecl.empty)
        )
        assert(compile(true) ~=~ Const(Constant.Bool(true), SIRType.Boolean, AnnotationsDecl.empty))
        assert(compile(()) ~=~ Const(Constant.Unit, SIRType.Unit, AnnotationsDecl.empty))
        assert(
          compile("foo") ~=~ Const(Constant.String("foo"), SIRType.String, AnnotationsDecl.empty)
        )
        assert(
          compile(BigInt("15511210043330985984000000")) ~=~ Const(
            Constant.Integer(BigInt("15511210043330985984000000")),
            SIRType.Integer,
            AnnotationsDecl.empty
          )
        )
        assert(
          compile(12: BigInt) ~=~ Const(
            Constant.Integer(BigInt("12")),
            SIRType.Integer,
            AnnotationsDecl.empty
          )
        )
        assert(
          compile(scala.math.BigInt.int2bigInt(12)) ~=~ Const(
            Constant.Integer(BigInt("12")),
            SIRType.Integer,
            AnnotationsDecl.empty
          )
        )

        // ByteStrings
        assert(
          compile(builtin.ByteString.empty) ~=~ Const(
            Constant.ByteString(builtin.ByteString.empty),
            SIRType.ByteString,
            AnnotationsDecl.empty
          )
        )

        assert(
          compile(builtin.ByteString.fromHex("deadbeef")) ~=~ Const(
            deadbeef,
            SIRType.ByteString,
            AnnotationsDecl.empty
          )
        )
        assert(
          compile(hex"deadbeef") ~=~ Const(deadbeef, SIRType.ByteString, AnnotationsDecl.empty)
        )
        assert(
          compile(builtin.ByteString.fromString("deadbeef")) ~=~ Const(
            Constant.ByteString(builtin.ByteString.fromString("deadbeef")),
            SIRType.ByteString,
            AnnotationsDecl.empty
          )
        )
    }

    test("compile if-then-else") {
        assert(
          compile {
              if Builtins.equalsInteger(1, 2) then () else ()
          } ~=~ SIR.IfThenElse(
            Apply(
              Apply(
                SIRBuiltins.equalsInteger,
                Const(Constant.Integer(1), SIRType.Integer, AnnotationsDecl.empty),
                SIRType.Fun(SIRType.Integer, SIRType.Boolean),
                AnnotationsDecl.empty
              ),
              Const(Constant.Integer(2), SIRType.Integer, AnnotationsDecl.empty),
              SIRType.Boolean,
              AnnotationsDecl.empty
            ),
            Const(Constant.Unit, SIRType.Unit, AnnotationsDecl.empty),
            Const(Constant.Unit, SIRType.Unit, AnnotationsDecl.empty),
            SIRType.Unit,
            AnnotationsDecl.empty
          )
        )
    }

    test("compile val def") {
        assert(
          compile {
              val a = true
              a
          } ~=~ Let(
            immutable.List(
              Binding(
                "a",
                SIRType.Boolean,
                Const(Constant.Bool(true), SIRType.Boolean, AnnotationsDecl.empty)
              )
            ),
            Var("a", SIRType.Boolean, AnnotationsDecl.empty),
            SIR.LetFlags.None,
            AnnotationsDecl.empty
          )
        )
    }

    test("compile def") {

        val compiled = compile {
            def b() = true

            def c(x: Boolean): Boolean = c(x)

            c(b())
        }

        val exprected = Let(
          immutable.List(
            Binding(
              "b",
              SIRType.Fun(SIRType.Unit, SIRType.Boolean),
              LamAbs(
                Var("_", SIRType.Unit, AnE),
                Const(Constant.Bool(true), SIRType.Boolean, AnE),
                List.empty,
                AnE
              )
            )
          ),
          Let(
            immutable.List(
              Binding(
                "c",
                SIRType.Fun(SIRType.Boolean, SIRType.Boolean),
                LamAbs(
                  Var("x", SIRType.Boolean, AnE),
                  Apply(
                    Var("c", SIRType.Fun(Boolean, SIRType.Boolean), AnE),
                    Var("x", SIRType.Boolean, AnE),
                    SIRType.Boolean,
                    AnE
                  ),
                  List.empty,
                  AnE
                )
              )
            ),
            Apply(
              Var("c", SIRType.Fun(Boolean, SIRType.Boolean), AnE),
              Apply(
                Var("b", SIRType.Fun(SIRType.Unit, SIRType.Boolean), AnE),
                Const(Constant.Unit, SIRType.Unit, AnE),
                SIRType.Boolean,
                AnE
              ),
              SIRType.Boolean,
              AnE
            ),
            LetFlags.Recursivity,
            AnE
          ),
          LetFlags.None,
          AnE
        )

        // println(s"comileDef: compiled: ${compiled.pretty.render(100)}")
        // println(s"comileDef: expected: ${exprected.pretty.render(100)}")

        assert(compiled ~=~ exprected)
    }

    test("compile lambda with args with type parameters") {
        // tail has a MethodType, check if it compiles
        val sir = compile {
            (
                tail: [A] => builtin.BuiltinList[A] => builtin.BuiltinList[A],
                ctx: builtin.BuiltinList[Data]
            ) =>
                tail[Data](ctx)
        }

        val compiledTp = sir.tp

        val a = TypeVar("A", Some(1), false)
        val builtinListA = SIRType.BuiltinList(a)
        val builtinListData = SIRType.BuiltinList(SIRType.Data)
        val tailType = SIRType.TypeLambda(List(a), Fun(builtinListA, builtinListA))

        val constructedExpr = LamAbs(
          Var("tail", tailType, AnE),
          LamAbs(
            Var("ctx", builtinListData, AnE),
            Apply(
              Var("tail", tailType, AnE),
              Var("ctx", builtinListData, AnE),
              builtinListData,
              AnE
            ),
            List.empty,
            AnE
          ),
          List.empty,
          AnE
        )

        // SIRUnify.unifyType(sir.asInstanceOf[SIRExpr].tp, constructedExpr.tp, SIRUnify.Env.empty.copy(debug = true)) match {
        //    case success@SIRUnify.UnificationSuccess(env,tp) => println("unifyType success")
        //    case failure@SIRUnify.UnificationFailure(path, left, right) => println(s"unifyType failure: ${failure}")
        // }

        assert(
          sir ~=~ LamAbs(
            Var("tail", tailType, AnE),
            LamAbs(
              Var("ctx", builtinListData, AnE),
              Apply(
                Var("tail", tailType, AnE),
                Var("ctx", builtinListData, AnE),
                builtinListData,
                AnE
              ),
              List.empty,
              AnE
            ),
            List.empty,
            AnE
          )
        )

    }

    test("compile inline def") {
        assert(
          compile {
              inline def b = true

              b
          } ~=~ Const(Constant.Bool(true), sirBool, AnE)
        )
    }

    test("compile lambda") {
        assert(
          compile {
              val a = (x: Boolean) => x
              a(true)
          } ~=~ Let(
            List(
              Binding(
                "a",
                SIRType.Fun(sirBool, sirBool),
                LamAbs(Var("x", sirBool, AnE), Var("x", sirBool, AnE), List.empty, AnE)
              )
            ),
            Apply(
              Var("a", SIRType.Fun(sirBool, sirBool), AnE),
              Const(Constant.Bool(true), sirBool, AnE),
              sirBool,
              AnE
            ),
            SIR.LetFlags.None,
            AnE
          )
        )
    }

    test("compile throw") {
        class CustomException extends RuntimeException("CustomException")
        def foo(): Throwable = new CustomException
        inline def err(inline msg: String): Nothing = throw new RuntimeException(msg)
        // Compile message in the 1st Exception string literal argument to Error(message)
        assert(compile { throw new RuntimeException("foo") } ~=~ Error("foo", AnE))
        // Otherwise, compile Error(code.show)
        val sir = compile {
            throw new RuntimeException(s"Not a literal: ${1 + 1}"): @nowarn
        }
        assert(
          sir ~=~
              Error(
                "new RuntimeException(\n  _root_.scala.StringContext.apply([\"Not a literal: \",\"\" : String]).s([2 : Any])\n  ):RuntimeException @nowarn()",
                AnE
              )
        )
        // compile custom exceptions <:< Throwable as Error(exception.getSimpleName)
        assert(compile { throw new CustomException } ~=~ Error("CustomException", AnE))
        // compile throw code as Error(code.show)
        assert(compile { throw foo() } ~=~ Error("foo()", AnE))
        // handle inlines correctly
        assert(compile { err("test") } ~=~ Error("test", AnE))
        //
        val expected = Let(
          List(Binding("msg", SIRType.String, Const(Constant.String("test"), SIRType.String, AnE))),
          Error(Var("msg", SIRType.String, AnE), null),
          SIR.LetFlags.None,
          AnE
        )
        assert(compile {
            val msg = "test"
            throw new RuntimeException(msg)
        } ~=~ expected)
    }

    test("compile ToData") {
        import scalus.builtin.Data.*
        val compiled = compile {
            BigInt(1).toData
        }
        val expected = {
            if summon[
                  Compiler.Options
                ].targetLoweringBackend == Compiler.TargetLoweringBackend.SirToUplcV3Lowering
            then {
                val a1Tp = SIRType.TypeVar("A", Some(1), false)
                val a2Tp = SIRType.TypeVar("A", Some(2), false)
                val a3Tp = SIRType.TypeVar("A", Some(3), false)
                Let(
                  immutable.List(
                    Binding("a$proxy1", sirInt, Const(Constant.Integer(1), sirInt, AnE))
                  ),
                  Apply(
                    ExternalVar(
                      "scalus.builtin.internal.UniversalDataConversion$",
                      "scalus.builtin.internal.UniversalDataConversion$.toData",
                      SIRType.Fun(sirInt, sirData),
                      AnE
                    ),
                    Var("a$proxy1", sirInt, AnE),
                    sirData,
                    AnE
                  ),
                  SIR.LetFlags.None,
                  AnE
                )
            } else
                Let(
                  immutable.List(
                    Binding("a$proxy1", sirInt, Const(Constant.Integer(1), sirInt, AnE))
                  ),
                  Apply(
                    SIRBuiltins.iData,
                    Var("a$proxy1", sirInt, AnE),
                    sirData,
                    AnE
                  ),
                  SIR.LetFlags.None,
                  AnE
                )
        }

        println(s"compiled=${compiled.pretty.render(100)}")
        println(s"exprected=${expected.pretty.render(100)}")
        //
        // SIRUnify.unifySIR(compiled, expected, SIRUnify.Env.empty) match
        //    case SIRUnify.UnificationSuccess(env, sir) =>
        //    case SIRUnify.UnificationFailure(path, left, right) =>
        //        println(s"Unification failure: $path, left=$left, right=$right")

        assert(compiled ~=~ expected)
        // val term = compiled.toUplc()
        // assert(VM.evaluateTerm(term) == Data.I(1))
    }

    test("compile type-safe equality") {
        import scalus.prelude.*
        val compiled = compile {
            val a = BigInt(0)
            val bs = hex"deadbeef"
            val s = "string"
            a === a && bs === bs && s === s
        }
        // println(compiled.show)
        val evaled = compiled.toUplc().evaluate
        assert(evaled == scalus.uplc.Term.Const(Constant.Bool(true)))
    }

    private val pubKeyHashDataDecl = DataDecl(
      "scalus.ledger.api.v1.PubKeyHash",
      List(
        ConstrDecl(
          "scalus.ledger.api.v1.PubKeyHash",
          List(TypeBinding("hash", sirByteString)),
          List.empty,
          List.empty,
          AnE
        )
      ),
      List.empty,
      AnE
    )

    test("compile datatypes") {
        val compiled = compile {
            val pkh = new scalus.ledger.api.v1.PubKeyHash(hex"deadbeef")
            pkh.hash
        }

        val expected = Decl(
          pubKeyHashDataDecl,
          Let(
            List(
              Binding(
                "pkh",
                pubKeyHashDataDecl.constrType("scalus.ledger.api.v1.PubKeyHash"),
                Constr(
                  "scalus.ledger.api.v1.PubKeyHash",
                  pubKeyHashDataDecl,
                  List(Const(uplc.Constant.ByteString(hex"DEADBEEF"), sirByteString, AnE)),
                  pubKeyHashDataDecl.constrType("scalus.ledger.api.v1.PubKeyHash"),
                  AnE
                )
              )
            ),
            Select(
              Var("pkh", pubKeyHashDataDecl.constrType("scalus.ledger.api.v1.PubKeyHash"), AnE),
              "hash",
              sirByteString,
              AnE
            ),
            SIR.LetFlags.None,
            AnE
          )
        )

        // SIRUnify.unifySIR(compiled, expected, SIRUnify.Env.empty.copy(debug = true)) match
        //    case SIRUnify.UnificationSuccess(env, unificator) =>
        //    case SIRUnify.UnificationFailure(path,left,right) =>
        //        if left.isInstanceOf[SIR] then
        //            println(s"compile datatypes: unify left:\n${left.asInstanceOf[SIR].show}")
        //        if right.isInstanceOf[SIR] then
        //            println(s"compile datatypes: unify right:\n${right.asInstanceOf[SIR].show}")

        assert(
          compiled ~=~ expected
        )
    }

    test("compile companion object apply as a primary constructor") {
        val compiled = compile {
            scalus.ledger.api.v1.PubKeyHash(hex"deadbeef")
        }

        assert(
          compiled ~=~
              Decl(
                pubKeyHashDataDecl,
                Constr(
                  "scalus.ledger.api.v1.PubKeyHash",
                  pubKeyHashDataDecl,
                  List(Const(Constant.ByteString(hex"deadbeef"), SIRType.ByteString, AnE)),
                  SIRType.CaseClass(pubKeyHashDataDecl.constructors.head, scala.Nil, None),
                  AnE
                )
              )
        )
    }

    test("compile fieldAsData macro") {
        import scalus.ledger.api.v1.*

        val compiled = compile { (ctx: scalus.builtin.Data) =>
            // check multiple nested fields
            val sigsData = fieldAsData[ScriptContext](_.txInfo.signatories)(ctx)
            // check type aliased fields
            val from = ctx.field[ScriptContext](_.txInfo.validRange.from).to[IntervalBound]
            // check tuples
            val data = ctx.field[(ByteString, Data)](_._2)
            val sigs = Builtins.unListData(sigsData)
            Builtins.unBData(sigs.head)
        }

        val term = compiled.toUplc()

        val scriptContext =
            ScriptContext(
              TxInfo(
                Nil,
                Nil,
                Value.zero,
                Value.zero,
                Nil,
                Nil,
                Interval.always,
                Cons(PubKeyHash(hex"deadbeef"), Nil),
                Nil,
                TxId(hex"bb")
              ),
              ScriptPurpose.Spending(TxOutRef(TxId(hex"deadbeef"), 0))
            )
        import DefaultUni.asConstant
        import scalus.builtin.Data.*
        val appliedScript = term.plutusV1 $ scriptContext.toData
        assert(appliedScript.evaluate == scalus.uplc.Term.Const(asConstant(hex"deadbeef")))
        val flatBytesLength = appliedScript.flatEncoded.length
        summon[Compiler.Options].targetLoweringBackend match
            case Compiler.TargetLoweringBackend.SirToUplcV3Lowering =>
                assert(flatBytesLength == 168)
            case _ =>
                assert(flatBytesLength == 348)
    }

    test("@Ignore annotation") {
        @Ignore
        def foo() = 1

        assert(compile {
            @Ignore val a = true

            @Ignore def foo() = true
        } ~=~ Const(Constant.Unit, SIRType.Unit, AnE))
    }

    test("compile pattern in val with one argument") {
        import scalus.builtin.{FromData, ToData}
        import scalus.prelude.Option
        val compiled = compile { (x: Data) =>
            val Option.Some(v0) = summon[FromData[Option[BigInt]]](x): @unchecked
            // val Option.Some(v) = x
            v0

        }
        val uplcFun = compiled.toUplc(generateErrorTraces = true)

        val dataSome1 = summon[ToData[Option[BigInt]]].apply(Option.Some(BigInt(1)))
        val uplc1 = uplcFun $ Term.Const(Constant.Data(dataSome1))
        val script1 = uplc1.plutusV3

        script1.evaluateDebug match
            case Result.Success(evaled, _, _, logs) =>
                assert(evaled == scalus.uplc.Term.Const(Constant.Integer(1)))
            case Result.Failure(exception, _, _, _) =>
                println("failure: exception=" + exception.getMessage)
                fail(exception)

        val dataNone = summon[ToData[Option[BigInt]]].apply(Option.None)
        val uplc2 = uplcFun $ Term.Const(Constant.Data(dataNone))
        val script2 = uplc2.plutusV3

        script2.evaluateDebug match
            case Result.Success(evaled, _, _, logs) =>
                fail("should not be successful")
            case Result.Failure(exception, _, _, logs) =>
                val condition = logs.exists(msg =>
                    msg.contains("Unexpected case") || msg.contains("Match failure")
                )
                if !condition then {
                    println("failure: exception=" + exception.getMessage)
                    println("logs=" + logs.mkString("\n"))
                }
                assert(condition, s"logs did not contain expected message: ${logs}")
            // assert(logs == List("Pattern match failure: expected Some but got None"))
    }

    test("compile pattern with val with two arguments") {
        import scalus.builtin.{FromData, ToData}
        import scalus.prelude.List

        val compiled = compile { (x: Data) =>
            val List.Cons(head, tail) = summon[FromData[List[BigInt]]](x): @unchecked
            // val Option.Some(v) = x
            head + tail.length
        }

        val uplcFun = compiled.toUplc(generateErrorTraces = true)

        val dataCons1 = summon[ToData[List[BigInt]]].apply(List.Cons(BigInt(1), List.Nil))
        val uplc1 = uplcFun $ Term.Const(Constant.Data(dataCons1))
        val script1 = uplc1.plutusV3
        script1.evaluateDebug match
            case Result.Success(evaled, _, _, logs) =>
                assert(evaled == scalus.uplc.Term.Const(Constant.Integer(1)))
            case Result.Failure(exception, _, _, logs) =>
                println("failure: exception=" + exception.getMessage)
                fail(exception)

        val dataCons2 =
            summon[ToData[List[BigInt]]].apply(List.Cons(BigInt(1), List.Cons(BigInt(2), List.Nil)))
        val uplc2 = uplcFun $ Term.Const(Constant.Data(dataCons2))
        val script2 = uplc2.plutusV3
        script2.evaluateDebug match
            case Result.Success(evaled, _, _, logs) =>
                assert(evaled == scalus.uplc.Term.Const(Constant.Integer(2)))
            case Result.Failure(exception, _, _, logs) =>
                println("failure: exception=" + exception.getMessage)
                fail(exception)

        val dataNil = summon[ToData[List[BigInt]]].apply(List.Nil)
        val uplc3 = uplcFun $ Term.Const(Constant.Data(dataNil))
        val script3 = uplc3.plutusV3
        script3.evaluateDebug match
            case Result.Success(evaled, _, _, logs) =>
                fail("should not be successful")
            case Result.Failure(exception, _, _, logs) =>
                val condition = logs.exists(msg =>
                    msg.contains("Unexpected case") || msg.contains("Match failure")
                )
                if !condition then {
                    println("failure: exception=" + exception.getMessage)
                    println("logs=" + logs.mkString("\n"))
                }
                assert(condition, s"logs did not contain expected message: ${logs}")

    }

    test("Compile pattern in val with three arguments") {
        import scalus.builtin.Data.{FromData, ToData}
        import scalus.ledger.api.v3.*
        import scalus.prelude.{List, Option}

        val compiled = compile { (x: Data) =>
            val ScriptContext(txInfo, redeemer, scriptInfo) = summon[FromData[ScriptContext]](x)
            // val Option.Some(v) = x
            txInfo
        }

        val uplcFun = compiled.toUplc(generateErrorTraces = true)

        val txInfo = TxInfo(
          inputs = Nil,
          id = TxId(hex"bb")
        )
        val redeemer = Data.unit
        val scriptInfo = ScriptInfo.SpendingScript(
          TxOutRef(TxId(hex"deadbeef"), 0),
          Option.None
        )
        val dataScriptInfo =
            summon[ToData[ScriptContext]].apply(ScriptContext(txInfo, redeemer, scriptInfo))
        val uplc1 = uplcFun $ Term.Const(Constant.Data(dataScriptInfo))
        val script1 = uplc1.plutusV3
        script1.evaluateDebug match
            case Result.Success(evaled, _, _, logs) =>
            // val txInfo1Data = summon[ToData[TxInfo]].apply()
            case Result.Failure(exception, _, _, logs) =>
                println("failure: exception=" + exception.getMessage)
                fail(exception)

    }

    test("compile scala function with zero arguments") {
        val compiled = compile {
            def fooZeroArgs = BigInt(1)
            val z = fooZeroArgs
            z
        }

        val evaluated = compiled.toUplc().evaluate

        assert(evaluated == scalus.uplc.Term.Const(Constant.Integer(1)))

    }

    test("compile scala functions with zero arguments and type parameter") {
        val compiled = compile {
            def fooZeroArgsTp[T]: scalus.prelude.List[T] = scalus.prelude.List.empty[T]
            val z = fooZeroArgsTp[BigInt]
            z.isEmpty
        }

        // println(s"sir=${compiled.pretty.render(100)}")

        val uplc = compiled.toUplc(generateErrorTraces = true)
        // println(s"uplc=${uplc.pretty.render(100)}")

        val evaluated = compiled.toUplc().evaluate

        assert(evaluated == scalus.uplc.Term.Const(Constant.Bool(true)))

        // println(s"evaluated=${evaluated}")
    }

    test("compile scala methd with zero arguments") {
        val compiled = compile {
            import scalus.prelude.*
            val m: SortedMap[BigInt, BigInt] = SortedMap.empty[BigInt, BigInt]
            m.get(BigInt(1)) match {
                case Option.None    => BigInt(0)
                case Option.Some(v) => v
            }
        }

        // println(s"sir=${compiled.pretty.render(100)}")

        def retrieveLastSIRComponent(sir: SIR): SIR =
            sir match
                case SIR.Decl(data, term) => retrieveLastSIRComponent(term)
                case _                    => sir

        def findLetForVar(sir: SIR, name: String): Option[SIR.Let] =
            sir match
                case SIR.Let(bindings, body, flags, _) =>
                    bindings.find(_.name == name) match
                        case Some(binding) =>
                            Some(
                              SIR.Let(
                                List(binding),
                                body,
                                flags,
                                AnE
                              )
                            )
                        case None => findLetForVar(body, name)
                case _ => None

        val mLet = findLetForVar(retrieveLastSIRComponent(compiled), "m").get

        val mBindingCompiled = mLet.bindings.head

        val mBindingExpected =
            Binding(
              "m",
              SIRType.FreeUnificator,
              Apply(
                SIR.ExternalVar(
                  "scalus.prelude.SortedMap$",
                  "scalus.prelude.SortedMap$.empty",
                  SIRType.FreeUnificator,
                  AnE
                ),
                sirConstUnit,
                SIRType.FreeUnificator,
                AnE
              )
            )

        assert(
          mBindingCompiled.value ~=~ mBindingExpected.value
        )

        val uplc = compiled.toUplcOptimized(generateErrorTraces = true)
        val evaluated = uplc.evaluate
        assert(evaluated == scalus.uplc.Term.Const(Constant.Integer(0)))

    }

    test("compile scala method with zero arguments in non-var position") {

        val compiled = compile {
            import scalus.prelude.*
            SortedMap.empty[BigInt, BigInt].get(BigInt(1)) match {
                case Option.None    => BigInt(0)
                case Option.Some(v) => v
            }
        }

        val uplc = compiled.toUplcOptimized(generateErrorTraces = true)
        val evaluated = uplc.evaluate
        assert(evaluated == scalus.uplc.Term.Const(Constant.Integer(0)))

    }
