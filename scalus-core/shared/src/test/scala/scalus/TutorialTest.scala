package scalus

import org.scalatest.funsuite.AnyFunSuite
import scalus.*
import scalus.Compiler.compile
import scalus.builtin.Builtins
import scalus.builtin.ByteString
import scalus.builtin.ByteString.*
import scalus.builtin.Data
import scalus.builtin.Data.FromData
import scalus.builtin.Data.fromData
import scalus.builtin.FromData
import scalus.cardano.ledger.{Language, ProtocolParams}
import scalus.prelude.===
import scalus.uplc.eval.CountingBudgetSpender
import scalus.uplc.eval.Log
import scalus.uplc.eval.MachineParams
import scalus.uplc.eval.NoLogger
import scalus.uplc.eval.Result
import scalus.uplc.eval.StackTraceMachineError
import scalus.uplc.eval.TallyingBudgetSpender

val constants = compile {
    val unit = ()
    val bool = true
    val int = BigInt(123)
    val bigint = BigInt("12345678901234567890")
    val implicitBigIng: BigInt = 123
    val emptyByteString = ByteString.empty
    val byteString = ByteString.fromHex("deadbeef")
    val byteStringUtf8 = ByteString.fromString("hello") // utf8 encoded
    val byteString2 = hex"deadbeef"
    val string = "Scalus Rocks!"
    val emptyList = builtin.BuiltinList.empty[BigInt]
    val list = builtin.BuiltinList[BigInt](1, 2, 3)
    val pair = builtin.BuiltinPair(true, ())
}

val builtinFunctions = compile {
    // See scalus.builtin.Builtins for what is available
    val data = Builtins.iData(123)
    val eqData = data == Builtins.iData(123) || data != Builtins.iData(123)
    val eq = Builtins.equalsByteString(hex"deadbeef", ByteString.empty)
    val byteStringEq = hex"deadbeef" == ByteString.empty || hex"deadbeef" != ByteString.empty
    val stringEq = "deadbeef" == "" || "deadbeef" != ""
    val a = BigInt(1)
    val sum = a + 1 - a * 3 / 4 // arithmetic operators
    val intEquality = a == sum || a != sum
    val bool = !true || (false == true) != false && true // boolean operators
    val equals = a === sum // comparison operators
}

case class Account(hash: ByteString, balance: BigInt)

enum State:
    case Empty
    case Active(account: Account)

import State.*
val dataTypes = compile {
    // Tuple2 literals are supported
    val tuple = (true, BigInt(123))
    val empty = State.Empty // simple constructor
    // Use `new` to create an instance
    val account = new Account(ByteString.empty, tuple._2) // access tuple fields
    // or use a companion object apply method
    val active: State = State.Active(account)
    val hash = account.hash // access case class fields
    // A simple pattern matching is supported
    // no guards, no type ascriptions.
    // Inner matches can be done only on single constructor case classes
    // Wildcard patterns are supported
    active match
        case Empty                                 => true
        case Active(account @ Account(_, balance)) => balance == BigInt(123)
    // all cases must be covered or there must be a default case
    val isEmpty = active match
        case Empty => true
        case _     => false
}

val controlFlow = compile {
    val a = BigInt(1)
    // if-then-else
    if a == BigInt(2) then ()
    // throwing an exception compiles to Plutus ERROR,
    // which aborts the evaluation of the script
    // the exception message can be translated to a trace message
    // using sir.toUplc(generateErrorTraces = true)
    else throw new Exception("not 2")
}

val functions = compile {
    val nonRecursiveLambda = (a: BigInt) => a + 1
    def recursive(a: BigInt): BigInt =
        if a == BigInt(0) then 0
        else recursive(a - 1)
}

@Compile
object ReusableCode {
    val constant = BigInt(1)
    def usefulFunction(a: BigInt): BigInt = a + 1
    @Ignore // this function is not compiled to UPLC
    def shouldNotBeInUplc() = ???
}

val modules = compile {
    ReusableCode.usefulFunction(ReusableCode.constant)
}

val fromDataExample = compile {
    // The `fromData` function is used to convert a `Data` value to a Scalus value.
    val data = Builtins.iData(123)
    // fromData is a summoner method for the `FromData` type class
    // there are instances for all built-in types
    val a = fromData[BigInt](data)
    // also you can use extension method `to` on Data
    val b = data.to[BigInt]

    // you can define your own `FromData` instances
    {
        given FromData[Account] = (d: Data) => {
            val args = Builtins.unConstrData(d).snd
            new Account(args.head.to[ByteString], args.tail.head.to[BigInt])
        }
        val account = data.to[Account]
    }

    // or your can you a macro to derive the FromData instance
    {
        given FromData[Account] = FromData.derived
        given FromData[State] = FromData.derived
    }
}

import scalus.ledger.api.v3.*
import scalus.prelude.List
val pubKeyValidator = compile {
    def validator(ctxData: Data) = {
        val ctx = ctxData.to[ScriptContext]
        ctx.txInfo.signatories.find { _.hash === hex"deadbeef" }
    }
}

val serializeToDoubleCborHex = {
    import scalus.*
    // convert to UPLC
    // generateErrorTraces = true will add trace messages to the UPLC program
    val uplc = pubKeyValidator.toUplc(generateErrorTraces = true)
    val program = uplc.plutusV3
    val flatEncoded = program.flatEncoded // if needed
    val cbor = program.cborEncoded // if needed
    val doubleEncoded = program.doubleCborEncoded // if needed
    // in most cases you want to use the hex representation of the double CBOR encoded program
    program.doubleCborHex
    // also you can produce a pubKeyValidator.plutus file for use with cardano-cli
    import scalus.utils.Utils
    Utils.writePlutusFile(
      "pubKeyValidator.plutus",
      program.deBruijnedProgram,
      Language.PlutusV2
    )
    // or simply
    program.writePlutusFile("pubKeyValidator.plutus", Language.PlutusV2)
}

def evaluation() = {
    import scalus.*
    import scalus.uplc.eval.PlutusVM
    val term = modules.toUplc()
    // setup a given PlutusVM for the PlutusV2 language and default parameters
    given v2VM: PlutusVM = PlutusVM.makePlutusV2VM()
    // simply evaluate the term with CEK machine
    term.evaluate.show // (con integer 2)

    // you can get the actual execution costs from protocol parameters JSON from cardano-cli
    lazy val pparams = ProtocolParams.fromCardanoCliJson("JSON with protocol parameters")
    lazy val machineParams = MachineParams.fromProtocolParams(pparams, Language.PlutusV3)
    // or from blockfrost API
    lazy val pparams2 = ProtocolParams.fromBlockfrostJson("JSON with protocol parameters")
    lazy val machineParams2 = MachineParams.fromProtocolParams(pparams2, Language.PlutusV3)
    // use latest PlutusV3 VM with explicit machine parameters
    val v3vm: PlutusVM = PlutusVM.makePlutusV3VM(machineParams)
    // evaluate a Plutus V3 script considering CIP-117
    // calculate the execution budget, all builtins costs, and collect logs
    val script = term.plutusV3.deBruijnedProgram
    script.evaluateDebug(using v3vm) match
        case r @ Result.Success(evaled, budget, costs, logs) =>
            println(r)
        case Result.Failure(exception, budget, costs, logs) =>
            println(s"Exception: $exception, logs: $logs")

    // evaluate a flat encoded script and calculate the execution budget and logs

    // TallyingBudgetSpender is a budget spender that counts the costs of each operation
    val tallyingBudgetSpender = TallyingBudgetSpender(CountingBudgetSpender())
    val logger = Log()
    // use NoLogger to disable logging
    val noopLogger = NoLogger
    try {
        v3vm.evaluateScript(script, tallyingBudgetSpender, logger)
    } catch {
        case e: StackTraceMachineError =>
            println(s"Error: ${e.getMessage}")
            println(s"Stacktrace: ${e.getCekStack}")
            println(s"Env: ${e.env}")
    }
    println(s"Execution budget: ${tallyingBudgetSpender.budgetSpender.getSpentBudget}")
    println(s"Logs: ${logger.getLogs.mkString("\n")}")
    println(
      s"Execution stats:\n${tallyingBudgetSpender.costs.toArray
              .sortBy(_._1.toString())
              .map { case (k, v) =>
                  s"$k: $v"
              }
              .mkString("\n")}"
    )
}

def fieldAsDataExample() = {
    import Compiler.*, builtin.{Data, Builtins}, Builtins.*
    import scalus.ledger.api.v3.*
    val pubKeyValidator = compile:
        def validator(ctxData: Data) =
            // this generates headList(...headList(sndPair(unConstrData(ctxData)))) code
            // to extract the signatories field from the ScriptContext
            val signatories = fieldAsData[ScriptContext](_.txInfo.signatories)(ctxData)
            // or like this, which is equivalent
            val signatories2 = ctxData.field[ScriptContext](_.txInfo.signatories)
            val sigs = unListData(signatories)
            // or like this, which is equivalent
            val sigs2 = signatories2.toList
            unBData(sigs.head) == hex"deadbeef"
            // same as above
            sigs2.head.toByteString == hex"deadbeef"
    println(pubKeyValidator.showHighlighted)
}

def inlineExample() = {
    import Compiler.compile, builtin.{Data, Builtins}, Builtins.*
    inline def validator(
        inline pubKeyHash: ByteString
    )(datum: Data, redeemer: Data, ctxData: Data) =
        verifyEd25519Signature(pubKeyHash, datum.toByteString, redeemer.toByteString)
    val script = compile:
        validator(hex"deadbeef")
    println(script.showHighlighted)
}

def debugFlagExample() = {
    import Compiler.compile, builtin.{Data, Builtins}, Builtins.*
    inline def dbg[A](msg: String)(a: A)(using debug: Boolean): A =
        inline if debug then trace(msg)(a) else a
    inline def validator(using debug: Boolean)(datum: Data, redeemer: Data, ctxData: Data) =
        dbg("datum")(datum)
    val releaseScript = compile(validator(using false))
    val debugScript = compile(validator(using true))
    println(releaseScript.showHighlighted)
    println(debugScript.showHighlighted)
}

class TutorialTest extends AnyFunSuite {
    test("pretty print") {
        // println(constants.showHighlighted)
        // println(builtinFunctions.showHighlighted)
        // println(dataTypes.showHighlighted)
        // println(controlFlow.showHighlighted)
        // println(functions.showHighlighted)
        // println(modules.showHighlighted)
        // println(fromDataExample.showHighlighted)
        // println(pubKeyValidator.showHighlighted)
        // evaluation()
        // fieldAsDataExample()
        // inlineExample()
        // debugFlagExample()
    }
}
