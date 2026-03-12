package scalus.serialization.flat

import org.scalatest.funsuite.AnyFunSuite
import org.scalatestplus.scalacheck.ScalaCheckPropertyChecks
import scalus.serialization.flat.FlatInstances.given
import scalus.compiler.sir.{AnnotationsDecl, Binding, Module, SIR, SIRPosition, SIRType}

class SIRFlatSerializationTest extends AnyFunSuite with ScalaCheckPropertyChecks:

    test("serialize and deserialize SIR Module") {
        val fl = summon[Flat[Module]]
        val sir =
            SIR.Const(scalus.uplc.Constant.Integer(23), SIRType.Integer, AnnotationsDecl.empty)
        val binding = Binding("x", SIRType.Integer, sir)
        val module = Module((1, 0), "test-module", false, None, List(binding))
        val enc = EncoderState(fl.bitSize(module) / 8 + 1)
        encode(module, enc)
        // now filler inside the encoder.
        //  TODO: rethink.
        // enc.filler()
        val dec = DecoderState(enc.buffer)
        val module2 = decode[Module](dec)
        assert(module == module2)
    }

    test("serialize and deserialize SIR Module with annotations") {
        val fl = summon[Flat[Module]]
        val sir =
            SIR.Const(scalus.uplc.Constant.Integer(42), SIRType.Integer, AnnotationsDecl.empty)
        val binding = Binding("y", SIRType.Integer, sir)
        val moduleAnns = AnnotationsDecl(
          SIRPosition.empty,
          data = Map(
            "scalus.compiler.TestAnnot:key1" -> SIR.Const(
              scalus.uplc.Constant.String("value1"),
              SIRType.String,
              AnnotationsDecl.empty
            ),
            "scalus.compiler.TestAnnot:key2" -> SIR.Const(
              scalus.uplc.Constant.Integer(123),
              SIRType.Integer,
              AnnotationsDecl.empty
            )
          )
        )
        val module = Module((5, 0), "test-module-anns", false, None, List(binding), moduleAnns)
        val enc = EncoderState(fl.bitSize(module) / 8 + 1)
        encode(module, enc)
        val dec = DecoderState(enc.buffer)
        val module2 = decode[Module](dec)
        assert(module2.anns.data.size == 2)
        assert(
          module2.anns.data("scalus.compiler.TestAnnot:key1") == moduleAnns.data(
            "scalus.compiler.TestAnnot:key1"
          )
        )
        assert(
          module2.anns.data("scalus.compiler.TestAnnot:key2") == moduleAnns.data(
            "scalus.compiler.TestAnnot:key2"
          )
        )
        assert(module == module2)
    }
