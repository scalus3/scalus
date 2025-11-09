package scalus.serialization.flat

import org.scalatest.funsuite.AnyFunSuite
import org.scalatestplus.scalacheck.ScalaCheckPropertyChecks
import scalus.serialization.flat.FlatInstances.given
import scalus.compiler.sir.{AnnotationsDecl, Binding, Module, SIR, SIRType}

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
