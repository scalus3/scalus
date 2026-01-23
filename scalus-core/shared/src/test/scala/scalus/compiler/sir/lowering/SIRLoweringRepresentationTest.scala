package scalus.compiler.sir.lowering

import org.scalatest.funsuite.AnyFunSuite
import scalus.compiler.sir.lowering.typegens.SirTypeUplcGenerator
import scalus.compiler.sir.*

object SIRLoweringRepresentationTest {

    case class TestCaseClass(a: BigInt, b: String)

    enum TestSum:
        case CaseA(a: BigInt)
        case CaseB(b: String)
        case CaseC(c: Boolean)

    case class TestCaseClassOneElement(a: BigInt)

}

class SIRLoweringRepresentationTest extends AnyFunSuite {

    given scalus.compiler.Options = scalus.compiler.Options(
      targetLoweringBackend = scalus.compiler.sir.TargetLoweringBackend.SirToUplcV3Lowering,
      generateErrorTraces = true,
      optimizeUplc = false,
      debug = false
    )

    test("Get default representation for List") {
        val listType = SIRType.List(SIRType.Integer)
        given LoweringContext = LoweringContext()
        val generator = SirTypeUplcGenerator(listType)
        // println(s"generator = ${generator} ")
        assert(generator.defaultRepresentation(listType) == SumCaseClassRepresentation.SumDataList)
    }

    test("Get default representation for Case Class") {
        pending
    }

    test("get default representation for Option.None") {
        val optionTypeVar = SIRType.TypeVar("A", Some(11L), false)
        val someTypeVar = SIRType.TypeVar("A", Some(12L), false)
        val optionProxy = SIRType.TypeProxy(null)
        val someConstrDecl = ConstrDecl(
          "scalus.cardano.onchain.plutus.prelude.Option$.Some",
          List(TypeBinding("x", someTypeVar)),
          List(someTypeVar),
          List(someTypeVar),
          AnnotationsDecl.empty
        )
        val noneConstrDecl = ConstrDecl(
          "scalus.cardano.onchain.plutus.prelude.Option$.None",
          List.empty,
          List.empty,
          List(SIRType.TypeNothing),
          AnnotationsDecl.empty
        )
        val optionDataDecl = DataDecl(
          "scalus.cardano.onchain.plutus.prelude.Option",
          List(someConstrDecl, noneConstrDecl),
          List(optionTypeVar),
          AnnotationsDecl.empty
        )

        def optionType(a: SIRType) = SIRType.typeApply(optionDataDecl.tp, List(a))

        def someType(a: SIRType) =
            SIRType.typeApply(optionDataDecl.constrType(someConstrDecl.name), List(a))

        val noneType = optionDataDecl.constrType(noneConstrDecl.name)

        val loweringContext = LoweringContext()
        given LoweringContext = loweringContext

        val optionGen = SirTypeUplcGenerator(optionType(SIRType.ByteString))
        val optionRepr = optionGen.defaultRepresentation(optionType(SIRType.ByteString))
        // println(s"optionRepr = ${optionRepr}")

        assert(optionRepr == SumCaseClassRepresentation.DataConstr)

        val noneGen = SirTypeUplcGenerator(noneType)
        val noneRepr = noneGen.defaultRepresentation(noneType)
        val noneDataRepr = noneGen.defaultDataRepresentation(noneType)

        assert(noneRepr == ProductCaseClassRepresentation.ProdDataList)
        assert(noneDataRepr == ProductCaseClassRepresentation.ProdDataConstr)

        // println(s"noneRepr =  ${noneRepr}, noneDataRepr = ${noneDataRepr}")

        val nilConstr = optionGen.genConstr(
          SIR.Constr(
            noneConstrDecl.name,
            optionDataDecl,
            List.empty,
            optionType(SIRType.ByteString),
            AnnotationsDecl.empty
          )
        )

    }

    test("get default representation of BLS_12-381 G2") {
        val g2Type = SIRType.BLS12_381_G2_Element
        given LoweringContext = LoweringContext()
        val generator = SirTypeUplcGenerator(g2Type)
        // println(s"generator = ${generator} ")
        assert(generator.defaultRepresentation(g2Type) == PrimitiveRepresentation.Constant)
    }

}
