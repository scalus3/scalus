package scalus.uplc

import scalus.serialization.cbor.Cbor

/** Flat-encoded Plutus 1.0.0 programs written bit by bit, for inputs that Scalus's own encoder
  * cannot produce, such as a `data` constant with bytes after its CBOR item or a `string` constant
  * that is not UTF-8. It is a tiny term language and its writer, following
  * `PlutusCore/FlatInstances.hs` and `UntypedPlutusCore/Core/Instance/Flat.hs`; test code, so terms
  * are written by plain recursion.
  */
object FlatProgramBuilder {
    sealed trait Tm
    case class Lam(body: Tm) extends Tm
    case class Ap(function: Tm, argument: Tm) extends Tm
    case class Builtin(tag: Int) extends Tm

    /** A constant: its type tags (preorder) and the writer of its value. */
    case class Const(types: Vector[Int], value: Bits => Unit) extends Tm

    val unit: Tm = Const(Vector(3), _ => ())

    /** A `data` constant whose flat byte string holds exactly `cbor`, trailing bytes and all. */
    def data(cbor: Array[Byte]): Tm = Const(Vector(8), _.byteString(cbor))

    /** A `string` constant whose flat byte string holds exactly `raw`, UTF-8 or not. */
    def string(raw: Array[Byte]): Tm = Const(Vector(2), _.byteString(raw))

    /** `levels` definite CBOR lists, each holding the next, around the integer 0. */
    def nestedList(levels: Int): Array[Byte] =
        Array.fill[Byte](levels)(0x81.toByte) :+ 0x00.toByte

    /** `(\_ -> unit) term`: evaluates to unit whatever `term` is, as long as it decodes. */
    def discard(term: Tm): Tm = Ap(Lam(unit), term)

    /** Decodes the program `discard(constant)` and returns the constant as Scalus read it. */
    def decodeDiscarded(constant: Tm): Constant =
        DeBruijnedProgram.fromFlatEncoded(flat(discard(constant))).term match
            case Term.Apply(_, Term.Const(decoded, _), _) => decoded
            case other => throw new IllegalArgumentException(s"not a discarded constant: $other")

    final class Bits {
        private val out = new StringBuilder

        def bit(b: Int): Unit = out.append(if b == 0 then '0' else '1')

        def bits(value: Int, width: Int): Unit =
            (width - 1 to 0 by -1).foreach(i => bit((value >> i) & 1))

        /** Zero or more 0 bits and a 1 bit, up to a byte boundary. */
        def filler(): Unit = {
            while (out.length + 1) % 8 != 0 do bit(0)
            bit(1)
        }

        def byteString(b: Array[Byte]): Unit = {
            filler()
            b.grouped(255).foreach { chunk =>
                bits(chunk.length, 8)
                chunk.foreach(x => bits(x & 0xff, 8))
            }
            bits(0, 8)
        }

        /** The bits so far followed by the minimal filler. */
        def bytes: Array[Byte] = {
            filler()
            out.toString.grouped(8).map(Integer.parseInt(_, 2).toByte).toArray
        }
    }

    private def term(t: Tm, b: Bits): Unit = t match
        case Lam(x)   => b.bits(2, 4); term(x, b)
        case Ap(f, a) => b.bits(3, 4); term(f, b); term(a, b)
        case Const(ts, v) =>
            b.bits(4, 4)
            ts.foreach { tag =>
                b.bit(1)
                b.bits(tag, 4)
            }
            b.bit(0)
            v(b)
        case Builtin(tag) => b.bits(7, 4); b.bits(tag, 7)

    /** The flat bytes of the program `(program 1.0.0 t)`. */
    def flat(t: Tm): Array[Byte] = {
        val b = new Bits
        // each component of the version is a natural below 128, written as one byte
        Seq(1, 0, 0).foreach(b.bits(_, 8))
        term(t, b)
        b.bytes
    }

    /** The script as the ledger holds it: a CBOR byte string around the flat bytes. */
    def script(t: Tm): Array[Byte] = Cbor.encode(flat(t))
}
