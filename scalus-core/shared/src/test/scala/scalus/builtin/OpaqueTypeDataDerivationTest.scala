package scalus.builtin

import org.scalatest.funsuite.AnyFunSuite
import scalus.uplc.builtin.{ByteString, Data, FromData, ToData}
import scalus.uplc.builtin.Data.*

// Opaque type for testing FromData/ToData derivation with opaque type fields
opaque type OpaqueId = ByteString

object OpaqueId {
    def apply(bs: ByteString): OpaqueId = bs
    extension (id: OpaqueId) inline def value: ByteString = id
}

case class RecordWithOpaqueField(
    name: BigInt,
    id: OpaqueId
)

object RecordWithOpaqueField {
    given FromData[RecordWithOpaqueField] = FromData.derived
    given ToData[RecordWithOpaqueField] = ToData.derived
}

class OpaqueTypeDataDerivationTest extends AnyFunSuite {

    test("derived FromData/ToData roundtrip for case class with opaque type field") {
        val id = OpaqueId(ByteString.fromHex("deadbeef"))
        val record = RecordWithOpaqueField(BigInt(42), id)

        // ToData should encode the opaque type field as its underlying ByteString
        val d = record.toData
        d match
            case Data.Constr(0, args) =>
                assert(args.head == Data.I(42))
                assert(args.tail.head == Data.B(ByteString.fromHex("deadbeef")))
            case _ => fail(s"Expected Constr(0, ...) but got $d")

        // FromData should reconstruct the record with the opaque type field
        val record2 = fromData[RecordWithOpaqueField](d)
        assert(record2 == record)
    }
}
