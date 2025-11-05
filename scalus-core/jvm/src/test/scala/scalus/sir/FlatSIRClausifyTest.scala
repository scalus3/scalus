package scalus.sir

import org.scalatest.funsuite.AnyFunSuite
import scalus.*
import scalus.Compiler.compile
import scalus.serialization.flat.FlatInstances.{*, given}
import scalus.serialization.flat.{DecoderState, EncoderState, HSRIdentityHashMap, HashConsed, HashConsedDecoderState, HashConsedEncoderState}

import java.io.{BufferedInputStream, File, FileInputStream}

object FlatSIRClausifyTestScope {

    type Var = BigInt
    type LRVars = (List[Var], List[Var])

    enum Formula:
        case Sym(arg: Var)
        case Not(arg: Formula)
        case And(arg1: Formula, arg2: Formula)
        case Or(arg1: Formula, arg2: Formula)
        case Implication(arg1: Formula, arg2: Formula)
        case Equivalence(arg1: Formula, arg2: Formula)
}

class FlatSIRClausifyTest extends AnyFunSuite {

    import FlatSIRClausifyTestScope.*

    def findDecl(sir: SIR, name: String): Option[DataDecl] = {
        sir match {
            case SIR.Decl(dataDecl, term) =>
                if dataDecl.name == name then Some(dataDecl)
                else findDecl(term, name)
            case other => None
        }
    }

    test("compile recursive formula declaration and sericalie/deserialize it") {

        val sir = compile {
            val x = BigInt(1)
            val y = BigInt(2)
            val z = BigInt(3)
            val formula1: Formula.And = Formula.And(Formula.Sym(x), Formula.Not(Formula.Sym(y)))
            val formula2: Formula.Or =
                Formula.Or(Formula.Sym(y), Formula.Implication(Formula.Sym(x), Formula.Sym(z)))
            val r1 = formula1.arg2
            val r2 = formula2.arg1
            r1
        }

        val myDecl = findDecl(sir, "scalus.sir.FlatSIRClausifyTestScope$.Formula")
        assert(myDecl.isDefined, "Declaration for Formula should be found")

        val myDeclTp = myDecl.get.tp

        val hcBitSizeState = scalus.serialization.flat.HashConsed.State.empty
        val hcBitSize = SIRTypeHashConsedFlat.bitSizeHC(myDeclTp, hcBitSizeState)
        val byteSize = (hcBitSize / 8) + 1
        val hcEncodeState = new HashConsedEncoderState(
          new EncoderState(byteSize),
          scalus.serialization.flat.HashConsed.State.empty,
          debug = false
        )
        serialization.flat.FlatInstances.SIRTypeHashConsedFlat.encodeHC(myDeclTp, hcEncodeState)

        val hcDecodeState =
            new HashConsedDecoderState(
              new DecoderState(hcEncodeState.encode.result),
              HashConsed.State.empty,
              debug = true
            )
        val decodedTpRef =
            serialization.flat.FlatInstances.SIRTypeHashConsedFlat.decodeHC(hcDecodeState)
        val decodedTp = decodedTpRef.finValue(hcDecodeState.hashConsed, 0, new HSRIdentityHashMap)

        SIRUnify.topLevelUnifyType(decodedTp, myDeclTp, SIRUnify.Env.empty) match {
            case SIRUnify.UnificationFailure(path, l, r) =>
                fail(s"Unification failed: ${path} ${l}!= ${r}")
            case SIRUnify.UnificationSuccess(_, _) =>
            // Unification succeeded
        }

    }

    test("desericalize ClausifyTest SIR from file") {
        val fname =
            "../../scalus-examples/jvm/target/scala-3.3.6/test-classes/scalus/benchmarks/ClausifyTest$.sir"

        val fname1 = "../../scalus-examples"
        val file = new File(fname)
        if !file.exists() then {
            pending // (s"File ${fname} does not exist")
        }
        val fileReader = new BufferedInputStream(new FileInputStream(file))
        val data = fileReader.readAllBytes()
        val module = summon[serialization.flat.Flat[Module]].decode(new DecoderState(data))
        val toPrint = false
        if toPrint then
            println(s"module.version = ${module.version}")
            for df <- module.defs do {
                println(s"df = ${df.name}")
                val tp: SIRType = df.tp
                println(s"tp = ${tp.show}")
                val sir: SIR = df.value
                println(s"sir = ${sir.pretty.render(100)}")
            }
    }

    test("deserelialize latin1 string with compilation") {
        val fname =
            "../../scalus-examples/jvm/target/scala-3.3.6/test-classes/ClausifyTest.scala_544.sir"

        val file = new File(fname)
        if !file.exists() then {
            pending // (s"File ${fname} does not exist")
        }
        val fileReader = new BufferedInputStream(new FileInputStream(file))
        val bytes = fileReader.readAllBytes()
        val grouped = bytes.grouped(45000).toList
        val latin1String = grouped
            .map(b => new String(b, java.nio.charset.StandardCharsets.ISO_8859_1))
            .mkString
        // val sir = ToExprHSSIRFlat.decodeStringLatin1(latin1String)
        val sir = ToExprHSSIRFlat.decode(new DecoderState(bytes))

        // println(sir.pretty.render(100))
    }

}
