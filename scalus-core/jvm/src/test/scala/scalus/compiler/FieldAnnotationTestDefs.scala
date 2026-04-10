package scalus.compiler

import scalus.Compile
import scalus.compiler.UplcRepresentation
import scalus.uplc.builtin.ByteString

@Compile
object FieldAnnotationTestDefs {

    case class MyRecord(
        value: BigInt,
        @UplcRepr(UplcRepresentation.Data) special: ByteString
    )

    case class WithNativeList(
        name: BigInt,
        @UplcRepr(UplcRepresentation.SumBuiltinList(UplcRepresentation.UplcConstr))
        items: scalus.cardano.onchain.plutus.prelude.List[BigInt]
    )

}
