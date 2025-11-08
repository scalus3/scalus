package scalus.uplc

import io.bullet.borer.Cbor
import scalus.builtin.Data
import scalus.cardano.ledger.Word64
import scalus.serialization.flat.{DecoderState, EncoderState, Flat, given}
import scalus.serialization.flat
import scalus.uplc.CommonFlatInstances.*
import scalus.uplc.CommonFlatInstances.given

object FlatInstantces:
    val termTagWidth = 4

    given Flat[Data] with

        def bitSize(a: Data): Int =
            summon[Flat[Array[Byte]]].bitSize(Cbor.encode(a).toByteArray)

        def encode(a: Data, encode: EncoderState): Unit =
            flat.encode(Cbor.encode(a).toByteArray, encode)

        def decode(decode: DecoderState): Data =
            val bytes = summon[Flat[Array[Byte]]].decode(decode)
            Cbor.decode(bytes).to[Data].value

    given Flat[Term] with
        import Term.*

        def bitSize(a: Term): Int = a match
            case Var(name) =>
                // in Plutus See Note [Index (Word64) (de)serialized through Natural]
                if name.index < 0 then
                    throw new IllegalArgumentException(
                      s"Cannot serialize UPLC Var with negative de Bruijn index. " +
                          s"Variable '${name.name}' has index ${name.index}, which indicates an unbound/free variable. " +
                          s"This usually means the variable is not properly bound in the scope."
                    )
                termTagWidth + summon[Flat[Word64]].bitSize(Word64(name.index))
            case Const(c)     => termTagWidth + flatConstant.bitSize(c)
            case Apply(f, x)  => termTagWidth + bitSize(f) + bitSize(x)
            case LamAbs(x, t) => termTagWidth + bitSize(t)
            case Force(term)  => termTagWidth + bitSize(term)
            case Delay(term)  => termTagWidth + bitSize(term)
            case Builtin(bn)  => termTagWidth + summon[Flat[DefaultFun]].bitSize(bn)
            case Error        => termTagWidth
            case Constr(tag, args) =>
                termTagWidth + summon[Flat[Word64]].bitSize(tag) + summon[Flat[List[Term]]]
                    .bitSize(args)
            case Case(arg, cases) =>
                termTagWidth + bitSize(arg) + summon[Flat[List[Term]]].bitSize(cases)

        def encode(a: Term, enc: EncoderState): Unit =
            a match
                case Term.Var(name) =>
                    if name.index < 0 then
                        throw new IllegalArgumentException(
                          s"Cannot serialize UPLC Var with negative de Bruijn index. " +
                              s"Variable '${name.name}' has index ${name.index}, which indicates an unbound/free variable. " +
                              s"This usually means the variable is not properly bound in the scope."
                        )
                    enc.bits(termTagWidth, 0)
                    summon[Flat[Word64]].encode(Word64(name.index), enc)
                case Term.Delay(term) =>
                    enc.bits(termTagWidth, 1)
                    encode(term, enc)
                case Term.LamAbs(name, term) =>
                    enc.bits(termTagWidth, 2)
                    encode(term, enc)
                case Term.Apply(f, arg) =>
                    enc.bits(termTagWidth, 3)
                    encode(f, enc); encode(arg, enc)
                case Term.Const(const) =>
                    enc.bits(termTagWidth, 4)
                    flatConstant.encode(const, enc)
                case Term.Force(term) =>
                    enc.bits(termTagWidth, 5)
                    encode(term, enc)
                case Term.Error =>
                    enc.bits(termTagWidth, 6)
                case Term.Builtin(bn) =>
                    enc.bits(termTagWidth, 7)
                    flat.encode(bn, enc)
                case Constr(tag, args) =>
                    enc.bits(termTagWidth, 8)
                    flat.encode(tag, enc)
                    flat.encode(args, enc)
                case Case(arg, cases) =>
                    enc.bits(termTagWidth, 9)
                    flat.encode(arg, enc)
                    flat.encode(cases, enc)

        def decode(decoder: DecoderState): Term =
            val tag = decoder.bits8(termTagWidth)
            tag match
                case 0 =>
                    val index = summon[Flat[Word64]].decode(decoder).value.toInt
                    val name = s"i$index"
                    Term.Var(NamedDeBruijn(name, index))
                case 1 =>
                    val term = decode(decoder)
                    Term.Delay(term)
                case 2 =>
                    val term = decode(decoder)
                    // in plutus-core it's super-duper over complicated, but it all boils down to this
                    // https://github.com/input-output-hk/plutus/blob/a56c96598b4b25c9e28215214d25189331087244/plutus-core/plutus-core/src/PlutusCore/Flat.hs#L357
                    Term.LamAbs("i0", term)
                case 3 =>
                    val f = decode(decoder)
                    val arg = decode(decoder)
                    Term.Apply(f, arg)
                case 4 =>
                    Term.Const(flatConstant.decode(decoder))
                case 5 =>
                    val term = decode(decoder)
                    Term.Force(term)
                case 6 =>
                    Term.Error
                case 7 =>
                    Term.Builtin(flat.decode(decoder))
                case 8 =>
                    Term.Constr(
                      tag = flat.decode(decoder),
                      args = flat.decode(decoder)
                    )
                case 9 =>
                    Term.Case(arg = decode(decoder), cases = flat.decode(decoder))
