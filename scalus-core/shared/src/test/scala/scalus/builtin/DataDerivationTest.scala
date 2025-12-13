package scalus.builtin

import org.scalatest.funsuite.AnyFunSuite
import org.scalatestplus.scalacheck.ScalaCheckPropertyChecks
import scalus.builtin.ByteString.*
import scalus.builtin.Data.*
import scalus.ledger.api.v1.*
import scalus.prelude.List as PList

class DataDerivationTest extends AnyFunSuite with ScalaCheckPropertyChecks with ArbitraryInstances:
    test("Simple derivation") {
        assert(TxId(hex"deadbeef").toData == Constr(0, PList(B(hex"deadbeef"))))
        assert(
          TxInfo(
            scalus.prelude.List.Nil,
            scalus.prelude.List.Nil,
            Value.zero,
            Value.zero,
            scalus.prelude.List.Nil,
            scalus.prelude.List.Nil,
            Interval.always,
            scalus.prelude.List.Nil,
            scalus.prelude.List.Nil,
            TxId(hex"bb")
          ).toData == Constr(
            0,
            PList(
              List(PList.Nil),
              List(PList.Nil),
              Map(PList.Nil),
              Map(PList.Nil),
              List(PList.Nil),
              List(PList.Nil),
              Constr(
                0,
                PList(
                  Constr(0, PList(Constr(0, PList.Nil), Constr(1, PList.Nil))),
                  Constr(0, PList(Constr(2, PList.Nil), Constr(1, PList.Nil)))
                )
              ),
              List(PList.Nil),
              List(PList.Nil),
              Constr(0, PList(B(hex"BB")))
            )
          )
        )
        assert(
          ScriptPurpose.Minting(hex"deadbeef").toData == Constr(
            0,
            PList(hex"deadbeef".toData)
          )
        )
        assert(
          ScriptPurpose.Spending(TxOutRef(TxId(hex"deadbeef"), 2)).toData == Constr(
            1,
            PList(Constr(0, PList(Constr(0, PList(hex"deadbeef".toData)), I(2))))
          )
        )
    }
