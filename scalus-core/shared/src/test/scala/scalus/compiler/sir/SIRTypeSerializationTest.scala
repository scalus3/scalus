package scalus.compiler.sir

import org.scalatest.funsuite.AnyFunSuite
import scalus.*
import scalus.uplc.builtin.*
import scalus.cardano.onchain.plutus.v2.OutputDatum
import scalus.cardano.onchain.plutus.v3.*
import scalus.cardano.onchain.plutus.prelude.*

class SIRTypeSerializationTest extends AnyFunSuite {

    def encodeDecodeSIR(sir: SIR): SIR = {
        import scalus.serialization.flat.*
        val bitSize = ToExprHSSIRFlat.bitSize(sir)
        val encoded = EncoderState(bitSize / 8 + 1)
        try ToExprHSSIRFlat.encode(sir, encoded)
        catch
            case e @ ToExprHSSIRFlat.SelfCheckException(msg, origin, decoded, path, l, r) =>
                println(s"Error encoding SIR: ${msg}")
                println(s"Origin SIR: ${origin.pretty.render(100)}")
                println(s"Decoded SIR: ${decoded.pretty.render(100)}")
                println(s"Path: ${path}")
                println(s"Left: ${l}")
                println(s"Right: ${r}")
                throw e
        val decoded = ToExprHSSIRFlat.decode(DecoderState(encoded.buffer))
        decoded
    }

    def encodeDecodeSIRType(sirType: SIRType): SIRType = {
        import scalus.serialization.flat.*
        val bitSize = ToExprHSSIRTypeFlat.bitSize(sirType)
        val encoded = EncoderState(bitSize / 8 + 1)
        ToExprHSSIRTypeFlat.encode(sirType, encoded)
        val decoded = ToExprHSSIRTypeFlat.decode(DecoderState(encoded.buffer))
        decoded
    }

    test("check order of parameters in TxnInfo") {
        val sir = scalus.compiler.compile {
            // TODO:  resolve apply for case-classes.
            val txIdPrev = new TxId(ByteString.fromString("0x12345678"))
            val outRef = new TxOutRef(txIdPrev, 0)
            val credential1 =
                new Credential.PubKeyCredential(new PubKeyHash(ByteString.fromString("pkh1")))
            val addressIn =
                new Address(credential = credential1, stakingCredential = prelude.Option.None)
            val valueIn = Value(ByteString.empty, ByteString.empty, 4)
            val outRefResolved = new TxOut(
              address = addressIn,
              value = valueIn,
              datum = OutputDatum.NoOutputDatum,
              referenceScript = prelude.Option.None
            )
            val inputs = scalus.cardano.onchain.plutus.prelude.List
                .single(new TxInInfo(outRef, outRefResolved))
            val credential2 =
                new Credential.PubKeyCredential(new PubKeyHash(ByteString.fromString("5")))
            val addressOut =
                new Address(credential = credential2, stakingCredential = prelude.Option.None)
            val valueOut = Value(ByteString.empty, ByteString.empty, 2)
            val outputs = scalus.cardano.onchain.plutus.prelude.List.single(
              new TxOut(addressOut, valueOut, OutputDatum.NoOutputDatum, prelude.Option.None)
            )
            val txId = new TxId(ByteString.fromString("0x123456789"))
            val fee = BigInt(2)
            val mint = Value.zero
            def txInfo = new TxInfo(
              inputs = inputs,
              referenceInputs = scalus.cardano.onchain.plutus.prelude.List.empty,
              outputs = outputs,
              fee = fee,
              mint = mint,
              certificates = scalus.cardano.onchain.plutus.prelude.List.empty,
              withdrawals = SortedMap.empty,
              validRange = Interval.always,
              signatories = scalus.cardano.onchain.plutus.prelude.List.empty,
              redeemers = SortedMap.empty,
              data = SortedMap.empty,
              id = txId,
              votes = SortedMap.empty,
              proposalProcedures = scalus.cardano.onchain.plutus.prelude.List.empty,
              currentTreasuryAmount = prelude.Option.None,
              treasuryDonation = prelude.Option.None
            )
            txInfo
        }
        // println(sir.pretty.render(100))
        // println(s"sir.tp=${sir.tp.show}")
        val txInfoSIRType = sir.tp match
            case SIRType.Fun(SIRType.Unit, tp) => tp
            case _ => fail(s"expected function type, we have ${sir.tp.show}")
        val constrDecl = txInfoSIRType match
            case SIRType.CaseClass(constrDecl, typeArgs, _) => constrDecl
            case SIRType.SumCaseClass(dataDecl, typeArgs) =>
                if dataDecl.constructors.length == 1 then
                    val constrDecl = dataDecl.constructors.head
                    constrDecl
                else
                    fail(
                      s"expected single case class, we have ${dataDecl.constructors.length} case classes"
                    )
            case _ => fail(s"case class expected, we have ${txInfoSIRType.show}: ${txInfoSIRType}")
        // println(s"constrDecl=${constrDecl}")
        val params = constrDecl.params
        // should be the same as in definition.
        assert(params(0).name == "inputs")
        assert(params(1).name == "referenceInputs")
        assert(params(2).name == "outputs")
        assert(params(3).name == "fee")
        assert(params(4).name == "mint")
        assert(params(5).name == "certificates")
        assert(params(6).name == "withdrawals")
        assert(params(7).name == "validRange")
        assert(params(8).name == "signatories")
        assert(params(9).name == "redeemers")
        assert(params(10).name == "data")
        assert(params(11).name == "id")
        assert(params(12).name == "votes")
        assert(params(13).name == "proposalProcedures")
        assert(params(14).name == "currentTreasuryAmount")
        assert(params(15).name == "treasuryDonation")
        val tp1 = encodeDecodeSIRType(txInfoSIRType)
        val constrDecl1 = tp1 match
            case SIRType.CaseClass(constrDecl, typeArgs, parentTypeArgs) => constrDecl
            case SIRType.SumCaseClass(dataDecl, typeArgs) =>
                if dataDecl.constructors.length == 1 then
                    val constrDecl = dataDecl.constructors.head
                    constrDecl
                else
                    fail(
                      s"expected single case class, we have ${dataDecl.constructors.length} case classes"
                    )
            case _ => fail(s"case class expected, we have ${tp1.show}: ${tp1}")
        val params1 = constrDecl1.params
        // should be the same as in definition.
        assert(params1(0).name == "inputs")
        assert(params1(1).name == "referenceInputs")
        assert(params1(2).name == "outputs")
        assert(params1(3).name == "fee")
        assert(params1(4).name == "mint")
        assert(params1(5).name == "certificates")
        assert(params1(6).name == "withdrawals")
        assert(params1(7).name == "validRange")
        assert(params1(8).name == "signatories")

        val sir2 = encodeDecodeSIR(sir)
        val tp2 = sir2.tp match {
            case SIRType.Fun(SIRType.Unit, tp) => tp
            case _ => fail(s"expected function type, we have ${sir2.tp.show}")
        }
        val constrDecl2 = tp2 match
            case SIRType.CaseClass(constrDecl, typeArgs, _) => constrDecl
            case SIRType.SumCaseClass(dataDecl, typeArgs) =>
                if dataDecl.constructors.length == 1 then
                    val constrDecl = dataDecl.constructors.head
                    constrDecl
                else
                    fail(
                      s"expected single case class, we have ${dataDecl.constructors.length} case classes"
                    )
            case _ => fail(s"case class expected, we have ${tp2.show}: ${tp2}")
        val params2 = constrDecl2.params
        // should be the same as in definition.
        assert(params2(0).name == "inputs")
    }

    // test("check order of parame")

}
