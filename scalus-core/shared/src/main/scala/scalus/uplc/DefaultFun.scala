package scalus.uplc

import scalus.serialization.flat.{DecoderState, EncoderState, Flat}

enum DefaultFun extends Enum[DefaultFun]:
    // Integers
    case AddInteger
    case SubtractInteger
    case MultiplyInteger
    case DivideInteger
    case QuotientInteger
    case RemainderInteger
    case ModInteger
    case EqualsInteger
    case LessThanInteger
    case LessThanEqualsInteger
    // Bytestrings
    case AppendByteString
    case ConsByteString
    case SliceByteString
    case LengthOfByteString
    case IndexByteString
    case EqualsByteString
    case LessThanByteString
    case LessThanEqualsByteString
    // Cryptography and hashes
    case Sha2_256
    case Sha3_256
    case Blake2b_256
    case VerifyEd25519Signature // formerly verifySignature
    case VerifyEcdsaSecp256k1Signature
    case VerifySchnorrSecp256k1Signature

    // Strings
    case AppendString
    case EqualsString
    case EncodeUtf8
    case DecodeUtf8

    // Bool
    case IfThenElse

    // Unit
    case ChooseUnit

    // Tracing
    case Trace

    // Pairs
    case FstPair

    case SndPair

    // Lists
    case ChooseList
    case MkCons
    case HeadList
    case TailList
    case NullList

    // Data
    // See Note [Pattern matching on built-in types].
    // It is convenient to have a "choosing" function for a data type that has more than two
    // constructors to get pattern matching over it and we may end up having multiple such data
    // types, hence we include the name of the data type as a suffix.
    case ChooseData
    case ConstrData
    case MapData
    case ListData
    case IData
    case BData
    case UnConstrData
    case UnMapData
    case UnListData
    case UnIData
    case UnBData
    case EqualsData
    case SerialiseData

    // Misc monomorphized constructors.
    // We could simply replace those with constants, but we use built-in functions for consistency
    // with monomorphic built-in types. Polymorphic built-in constructors are generally problematic,
    // See note [Representable built-in functions over polymorphic built-in types].
    case MkPairData
    case MkNilData
    case MkNilPairData
    // BLS12_381 operations
    // G1 operations
    case Bls12_381_G1_add
    case Bls12_381_G1_neg
    case Bls12_381_G1_scalarMul
    case Bls12_381_G1_equal
    case Bls12_381_G1_hashToGroup
    case Bls12_381_G1_compress
    case Bls12_381_G1_uncompress

    // G2 operations
    case Bls12_381_G2_add
    case Bls12_381_G2_neg
    case Bls12_381_G2_scalarMul
    case Bls12_381_G2_equal
    case Bls12_381_G2_hashToGroup
    case Bls12_381_G2_compress
    case Bls12_381_G2_uncompress

    // Pairing operations
    case Bls12_381_millerLoop
    case Bls12_381_mulMlResult
    case Bls12_381_finalVerify

    // Hash functions
    case Keccak_256
    case Blake2b_224

    // Conversions
    case IntegerToByteString
    case ByteStringToInteger
    case AndByteString
    case OrByteString
    case XorByteString
    case ComplementByteString
    case ReadBit
    case WriteBits
    case ReplicateByte
    case ShiftByteString
    case RotateByteString
    case CountSetBits
    case FindFirstSetBit
    case Ripemd_160

    // Plutus 1.53 new builtins
    case DropList

    // Array builtins
    case LengthOfArray
    case ListToArray
    case IndexArray

object DefaultFun {
    given Flat[DefaultFun] with
        def bitSize(a: DefaultFun): Int = 7

        def encode(a: DefaultFun, encode: EncoderState): Unit =
            val code = a match
                case AddInteger            => 0
                case SubtractInteger       => 1
                case MultiplyInteger       => 2
                case DivideInteger         => 3
                case QuotientInteger       => 4
                case RemainderInteger      => 5
                case ModInteger            => 6
                case EqualsInteger         => 7
                case LessThanInteger       => 8
                case LessThanEqualsInteger => 9

                case AppendByteString         => 10
                case ConsByteString           => 11
                case SliceByteString          => 12
                case LengthOfByteString       => 13
                case IndexByteString          => 14
                case EqualsByteString         => 15
                case LessThanByteString       => 16
                case LessThanEqualsByteString => 17

                case Sha2_256               => 18
                case Sha3_256               => 19
                case Blake2b_256            => 20
                case VerifyEd25519Signature => 21

                case AppendString => 22
                case EqualsString => 23
                case EncodeUtf8   => 24
                case DecodeUtf8   => 25

                case IfThenElse => 26

                case ChooseUnit => 27

                case Trace => 28

                case FstPair => 29
                case SndPair => 30

                case ChooseList => 31
                case MkCons     => 32
                case HeadList   => 33
                case TailList   => 34
                case NullList   => 35

                case ChooseData                      => 36
                case ConstrData                      => 37
                case MapData                         => 38
                case ListData                        => 39
                case IData                           => 40
                case BData                           => 41
                case UnConstrData                    => 42
                case UnMapData                       => 43
                case UnListData                      => 44
                case UnIData                         => 45
                case UnBData                         => 46
                case EqualsData                      => 47
                case MkPairData                      => 48
                case MkNilData                       => 49
                case MkNilPairData                   => 50
                case SerialiseData                   => 51
                case VerifyEcdsaSecp256k1Signature   => 52
                case VerifySchnorrSecp256k1Signature => 53
                case Bls12_381_G1_add                => 54
                case Bls12_381_G1_neg                => 55
                case Bls12_381_G1_scalarMul          => 56
                case Bls12_381_G1_equal              => 57
                case Bls12_381_G1_compress           => 58
                case Bls12_381_G1_uncompress         => 59
                case Bls12_381_G1_hashToGroup        => 60
                case Bls12_381_G2_add                => 61
                case Bls12_381_G2_neg                => 62
                case Bls12_381_G2_scalarMul          => 63
                case Bls12_381_G2_equal              => 64
                case Bls12_381_G2_compress           => 65
                case Bls12_381_G2_uncompress         => 66
                case Bls12_381_G2_hashToGroup        => 67
                case Bls12_381_millerLoop            => 68
                case Bls12_381_mulMlResult           => 69
                case Bls12_381_finalVerify           => 70
                case Keccak_256                      => 71
                case Blake2b_224                     => 72

                case IntegerToByteString  => 73
                case ByteStringToInteger  => 74
                case AndByteString        => 75
                case OrByteString         => 76
                case XorByteString        => 77
                case ComplementByteString => 78
                case ReadBit              => 79
                case WriteBits            => 80
                case ReplicateByte        => 81

                case ShiftByteString  => 82
                case RotateByteString => 83
                case CountSetBits     => 84
                case FindFirstSetBit  => 85
                case Ripemd_160       => 86

                // Plutus 1.53 new builtins
                case DropList => 88

                // Array builtins
                case LengthOfArray => 89
                case ListToArray   => 90
                case IndexArray    => 91

            encode.bits(7, code.toByte)

        def decode(decode: DecoderState): DefaultFun =
            decode.bits8(7) match
                case 0  => AddInteger
                case 1  => SubtractInteger
                case 2  => MultiplyInteger
                case 3  => DivideInteger
                case 4  => QuotientInteger
                case 5  => RemainderInteger
                case 6  => ModInteger
                case 7  => EqualsInteger
                case 8  => LessThanInteger
                case 9  => LessThanEqualsInteger
                case 10 => AppendByteString
                case 11 => ConsByteString
                case 12 => SliceByteString
                case 13 => LengthOfByteString
                case 14 => IndexByteString
                case 15 => EqualsByteString
                case 16 => LessThanByteString
                case 17 => LessThanEqualsByteString
                case 18 => Sha2_256
                case 19 => Sha3_256
                case 20 => Blake2b_256
                case 21 => VerifyEd25519Signature
                case 22 => AppendString
                case 23 => EqualsString
                case 24 => EncodeUtf8
                case 25 => DecodeUtf8
                case 26 => IfThenElse
                case 27 => ChooseUnit
                case 28 => Trace
                case 29 => FstPair
                case 30 => SndPair
                case 31 => ChooseList
                case 32 => MkCons
                case 33 => HeadList
                case 34 => TailList
                case 35 => NullList
                case 36 => ChooseData
                case 37 => ConstrData
                case 38 => MapData
                case 39 => ListData
                case 40 => IData
                case 41 => BData
                case 42 => UnConstrData
                case 43 => UnMapData
                case 44 => UnListData
                case 45 => UnIData
                case 46 => UnBData
                case 47 => EqualsData
                case 48 => MkPairData
                case 49 => MkNilData
                case 50 => MkNilPairData
                case 51 => SerialiseData
                case 52 => VerifyEcdsaSecp256k1Signature
                case 53 => VerifySchnorrSecp256k1Signature
                case 54 => Bls12_381_G1_add
                case 55 => Bls12_381_G1_neg
                case 56 => Bls12_381_G1_scalarMul
                case 57 => Bls12_381_G1_equal
                case 58 => Bls12_381_G1_compress
                case 59 => Bls12_381_G1_uncompress
                case 60 => Bls12_381_G1_hashToGroup
                case 61 => Bls12_381_G2_add
                case 62 => Bls12_381_G2_neg
                case 63 => Bls12_381_G2_scalarMul
                case 64 => Bls12_381_G2_equal
                case 65 => Bls12_381_G2_compress
                case 66 => Bls12_381_G2_uncompress
                case 67 => Bls12_381_G2_hashToGroup
                case 68 => Bls12_381_millerLoop
                case 69 => Bls12_381_mulMlResult
                case 70 => Bls12_381_finalVerify
                case 71 => Keccak_256
                case 72 => Blake2b_224
                case 73 => IntegerToByteString
                case 74 => ByteStringToInteger
                case 75 => AndByteString
                case 76 => OrByteString
                case 77 => XorByteString
                case 78 => ComplementByteString
                case 79 => ReadBit
                case 80 => WriteBits
                case 81 => ReplicateByte
                case 82 => ShiftByteString
                case 83 => RotateByteString
                case 84 => CountSetBits
                case 85 => FindFirstSetBit
                case 86 => Ripemd_160
                // Plutus 1.53 new builtins
                case 88 => DropList
                // Array builtins
                case 89 => LengthOfArray
                case 90 => ListToArray
                case 91 => IndexArray
                case c  => throw new Exception(s"Invalid builtin function code: $c")

}

given DefaultFunOrdering: Ordering[DefaultFun] with
    def compare(x: DefaultFun, y: DefaultFun): Int = x.ordinal - y.ordinal
