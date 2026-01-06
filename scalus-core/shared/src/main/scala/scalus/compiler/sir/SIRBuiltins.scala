package scalus.compiler.sir

import scalus.uplc.DefaultFun

object SIRBuiltins {

    val addInteger: SIR.Builtin = SIR.Builtin(
      DefaultFun.AddInteger,
      SIRType.Integer ->: SIRType.Integer ->: SIRType.Integer,
      AnnotationsDecl.empty
    )
    val subtractInteger: SIR.Builtin = SIR.Builtin(
      DefaultFun.SubtractInteger,
      SIRType.Integer ->: SIRType.Integer ->: SIRType.Integer,
      AnnotationsDecl.empty
    )
    val multiplyInteger: SIR.Builtin = SIR.Builtin(
      DefaultFun.MultiplyInteger,
      SIRType.Integer ->: SIRType.Integer ->: SIRType.Integer,
      AnnotationsDecl.empty
    )
    val divideInteger: SIR.Builtin = SIR.Builtin(
      DefaultFun.DivideInteger,
      SIRType.Integer ->: SIRType.Integer ->: SIRType.Integer,
      AnnotationsDecl.empty
    )
    val quotientInteger: SIR.Builtin = SIR.Builtin(
      DefaultFun.QuotientInteger,
      SIRType.Integer ->: SIRType.Integer ->: SIRType.Integer,
      AnnotationsDecl.empty
    )
    val remainderInteger: SIR.Builtin = SIR.Builtin(
      DefaultFun.RemainderInteger,
      SIRType.Integer ->: SIRType.Integer ->: SIRType.Integer,
      AnnotationsDecl.empty
    )
    val modInteger: SIR.Builtin = SIR.Builtin(
      DefaultFun.ModInteger,
      SIRType.Integer ->: SIRType.Integer ->: SIRType.Integer,
      AnnotationsDecl.empty
    )
    val equalsInteger: SIR.Builtin = SIR.Builtin(
      DefaultFun.EqualsInteger,
      SIRType.Integer ->: SIRType.Integer ->: SIRType.Boolean,
      AnnotationsDecl.empty
    )
    val lessThanInteger: SIR.Builtin = SIR.Builtin(
      DefaultFun.LessThanInteger,
      SIRType.Integer ->: SIRType.Integer ->: SIRType.Boolean,
      AnnotationsDecl.empty
    )
    val lessThanEqualsInteger: SIR.Builtin = SIR.Builtin(
      DefaultFun.LessThanEqualsInteger,
      SIRType.Integer ->: SIRType.Integer ->: SIRType.Boolean,
      AnnotationsDecl.empty
    )
    val expModInteger: SIR.Builtin = SIR.Builtin(
      DefaultFun.ExpModInteger,
      SIRType.Integer ->: SIRType.Integer ->: SIRType.Integer ->: SIRType.Integer,
      AnnotationsDecl.empty
    )

    // Bytestrings
    val appendByteString: SIR.Builtin = SIR.Builtin(
      DefaultFun.AppendByteString,
      SIRType.ByteString ->: SIRType.ByteString ->: SIRType.ByteString,
      AnnotationsDecl.empty
    )
    val consByteString: SIR.Builtin = SIR.Builtin(
      DefaultFun.ConsByteString,
      SIRType.Integer ->: SIRType.ByteString ->: SIRType.ByteString,
      AnnotationsDecl.empty
    )
    val sliceByteString: SIR.Builtin = SIR.Builtin(
      DefaultFun.SliceByteString,
      SIRType.Integer ->: SIRType.Integer ->: SIRType.ByteString ->: SIRType.ByteString,
      AnnotationsDecl.empty
    )
    val lengthOfByteString: SIR.Builtin = SIR.Builtin(
      DefaultFun.LengthOfByteString,
      SIRType.ByteString ->: SIRType.Integer,
      AnnotationsDecl.empty
    )
    val indexByteString: SIR.Builtin = SIR.Builtin(
      DefaultFun.IndexByteString,
      SIRType.ByteString ->: SIRType.Integer ->: SIRType.Integer,
      AnnotationsDecl.empty
    )
    val equalsByteString: SIR.Builtin = SIR.Builtin(
      DefaultFun.EqualsByteString,
      SIRType.ByteString ->: SIRType.ByteString ->: SIRType.Boolean,
      AnnotationsDecl.empty
    )
    val lessThanByteString: SIR.Builtin = SIR.Builtin(
      DefaultFun.LessThanByteString,
      SIRType.ByteString ->: SIRType.ByteString ->: SIRType.Boolean,
      AnnotationsDecl.empty
    )
    val lessThanEqualsByteString: SIR.Builtin = SIR.Builtin(
      DefaultFun.LessThanEqualsByteString,
      SIRType.ByteString ->: SIRType.ByteString ->: SIRType.Boolean,
      AnnotationsDecl.empty
    )

    // Cryptography and hashes
    val sha2_256: SIR.Builtin = SIR.Builtin(
      DefaultFun.Sha2_256,
      SIRType.ByteString ->: SIRType.ByteString,
      AnnotationsDecl.empty
    )
    val sha3_256: SIR.Builtin = SIR.Builtin(
      DefaultFun.Sha3_256,
      SIRType.ByteString ->: SIRType.ByteString,
      AnnotationsDecl.empty
    )
    val blake2b_256: SIR.Builtin = SIR.Builtin(
      DefaultFun.Blake2b_256,
      SIRType.ByteString ->: SIRType.ByteString,
      AnnotationsDecl.empty
    )
    val verifyEd25519Signature: SIR.Builtin = SIR.Builtin(
      DefaultFun.VerifyEd25519Signature,
      SIRType.ByteString ->: SIRType.ByteString ->: SIRType.ByteString ->: SIRType.Boolean,
      AnnotationsDecl.empty
    )
    val verifyEcdsaSecp256k1Signature: SIR.Builtin = SIR.Builtin(
      DefaultFun.VerifyEcdsaSecp256k1Signature,
      SIRType.ByteString ->: SIRType.ByteString ->: SIRType.ByteString ->: SIRType.Boolean,
      AnnotationsDecl.empty
    )
    val verifySchnorrSecp256k1Signature: SIR.Builtin = SIR.Builtin(
      DefaultFun.VerifySchnorrSecp256k1Signature,
      SIRType.ByteString ->: SIRType.ByteString ->: SIRType.ByteString ->: SIRType.Boolean,
      AnnotationsDecl.empty
    )

    // Strings
    val appendString: SIR.Builtin = SIR.Builtin(
      DefaultFun.AppendString,
      SIRType.String ->: SIRType.String ->: SIRType.String,
      AnnotationsDecl.empty
    )
    val equalsString: SIR.Builtin = SIR.Builtin(
      DefaultFun.EqualsString,
      SIRType.String ->: SIRType.String ->: SIRType.Boolean,
      AnnotationsDecl.empty
    )
    val encodeUtf8: SIR.Builtin =
        SIR.Builtin(
          DefaultFun.EncodeUtf8,
          SIRType.String ->: SIRType.ByteString,
          AnnotationsDecl.empty
        )
    val decodeUtf8: SIR.Builtin =
        SIR.Builtin(
          DefaultFun.DecodeUtf8,
          SIRType.ByteString ->: SIRType.String,
          AnnotationsDecl.empty
        )

    // Bool
    val ifThenElse: SIR.Builtin = SIR.Builtin(
      DefaultFun.IfThenElse, {
          val a = SIRType.TypeVar("A", Some("ifThenElse_A".hashCode), true)
          a :=>> (SIRType.Boolean ->: a ->: a ->: a)
      },
      AnnotationsDecl.empty
    )

    // Unit
    val chooseUnit: SIR.Builtin =
        SIR.Builtin(
          DefaultFun.ChooseUnit, {
              val a = SIRType.TypeVar("A", Some("chooseUnit_A".hashCode), true)
              a :=>> (SIRType.Unit ->: a ->: a)
          },
          AnnotationsDecl.empty
        )

    // Tracing
    //   TODO: move to SIR construction
    val trace: SIR.Builtin = SIR.Builtin(
      DefaultFun.Trace,
      SIRType.TypeLambda("trace_A", a => SIRType.String ->: a ->: a, true),
      AnnotationsDecl.empty
    )

    // Pairs
    val fstPair: SIR.Builtin = SIR.Builtin(
      DefaultFun.FstPair,
      SIRType
          .TypeLambda2("fstPair_A", "fstPair_B", (a, b) => SIRType.BuiltinPair(a, b) ->: a, true),
      AnnotationsDecl.empty
    )
    val sndPair: SIR.Builtin = SIR.Builtin(
      DefaultFun.SndPair,
      SIRType
          .TypeLambda2("sndPair_A", "sndPair_B", (a, b) => SIRType.BuiltinPair(a, b) ->: b, true),
      AnnotationsDecl.empty
    )

    // Lists
    val chooseList: SIR.Builtin = SIR.Builtin(
      DefaultFun.ChooseList,
      SIRType.TypeLambda2(
        "chooseList_A",
        "chooseList_B",
        (a, b) => SIRType.BuiltinList(a) ->: b ->: b ->: b,
        true
      ),
      AnnotationsDecl.empty
    )
    val mkCons: SIR.Builtin = SIR.Builtin(
      DefaultFun.MkCons,
      SIRType.TypeLambda(
        "mkCons_A",
        a => a ->: SIRType.BuiltinList(a) ->: SIRType.BuiltinList(a),
        true
      ),
      AnnotationsDecl.empty
    )
    val headList: SIR.Builtin =
        SIR.Builtin(
          DefaultFun.HeadList,
          SIRType.TypeLambda("headList_A", a => SIRType.BuiltinList(a) ->: a, true),
          AnnotationsDecl.empty
        )
    val tailList: SIR.Builtin = SIR.Builtin(
      DefaultFun.TailList,
      SIRType
          .TypeLambda("tailList_A", a => SIRType.BuiltinList(a) ->: SIRType.BuiltinList(a), true),
      AnnotationsDecl.empty
    )
    val nullList: SIR.Builtin = SIR.Builtin(
      DefaultFun.NullList,
      SIRType.TypeLambda("nullList_A", a => SIRType.BuiltinList(a) ->: SIRType.Boolean, true),
      AnnotationsDecl.empty
    )

    // Data
    val chooseData: SIR.Builtin =
        SIR.Builtin(
          DefaultFun.ChooseData,
          SIRType.TypeLambda(
            "chooseData_A",
            a => SIRType.Data.tp ->: a ->: a ->: a ->: a ->: a ->: a,
            true
          ),
          AnnotationsDecl.empty
        )
    val constrData: SIR.Builtin = SIR.Builtin(
      DefaultFun.ConstrData,
      SIRType.Integer ->: SIRType.BuiltinList(SIRType.Data.tp) ->: SIRType.Data.tp,
      AnnotationsDecl.empty
    )
    val mapData: SIR.Builtin = SIR.Builtin(
      DefaultFun.MapData,
      SIRType.BuiltinList(
        SIRType.BuiltinPair(SIRType.Data.tp, SIRType.Data.tp)
      ) ->: SIRType.Data.tp,
      AnnotationsDecl.empty
    )
    val listData: SIR.Builtin =
        SIR.Builtin(
          DefaultFun.ListData,
          SIRType.BuiltinList(SIRType.Data.tp) ->: SIRType.Data.tp,
          AnnotationsDecl.empty
        )
    val iData: SIR.Builtin =
        SIR.Builtin(DefaultFun.IData, SIRType.Integer ->: SIRType.Data.tp, AnnotationsDecl.empty)
    val bData: SIR.Builtin =
        SIR.Builtin(DefaultFun.BData, SIRType.ByteString ->: SIRType.Data.tp, AnnotationsDecl.empty)
    val unConstrData: SIR.Builtin = SIR.Builtin(
      DefaultFun.UnConstrData,
      SIRType.Data.tp ->: SIRType
          .BuiltinPair(SIRType.Integer, SIRType.BuiltinList(SIRType.Data.tp)),
      AnnotationsDecl.empty
    )
    val unMapData: SIR.Builtin = SIR.Builtin(
      DefaultFun.UnMapData,
      SIRType.Data.tp ->: SIRType.BuiltinList(
        SIRType.BuiltinPair(SIRType.Data.tp, SIRType.Data.tp)
      ),
      AnnotationsDecl.empty
    )
    val unListData: SIR.Builtin =
        SIR.Builtin(
          DefaultFun.UnListData,
          SIRType.Data.tp ->: SIRType.BuiltinList(SIRType.Data.tp),
          AnnotationsDecl.empty
        )
    val unIData: SIR.Builtin =
        SIR.Builtin(DefaultFun.UnIData, SIRType.Data.tp ->: SIRType.Integer, AnnotationsDecl.empty)
    val unBData: SIR.Builtin =
        SIR.Builtin(
          DefaultFun.UnBData,
          SIRType.Data.tp ->: SIRType.ByteString,
          AnnotationsDecl.empty
        )
    val equalsData: SIR.Builtin =
        SIR.Builtin(
          DefaultFun.EqualsData,
          SIRType.Data.tp ->: SIRType.Data.tp ->: SIRType.Boolean,
          AnnotationsDecl.empty
        )
    val serialiseData: SIR.Builtin =
        SIR.Builtin(
          DefaultFun.SerialiseData,
          SIRType.Data.tp ->: SIRType.ByteString,
          AnnotationsDecl.empty
        )

    //   TODO: think about pair
    val mkPairData: SIR.Builtin =
        SIR.Builtin(
          DefaultFun.MkPairData,
          SIRType.Data.tp ->: SIRType.Data.tp ->: SIRType
              .BuiltinPair(SIRType.Data.tp, SIRType.Data.tp),
          AnnotationsDecl.empty
        )
    val mkNilData: SIR.Builtin =
        SIR.Builtin(
          DefaultFun.MkNilData,
          SIRType.Unit ->: SIRType.BuiltinList(SIRType.Data.tp),
          AnnotationsDecl.empty
        )
    val mkNilPairData: SIR.Builtin =
        SIR.Builtin(
          DefaultFun.MkNilPairData,
          SIRType.Unit ->: SIRType.BuiltinList(
            SIRType.BuiltinPair(SIRType.Data.tp, SIRType.Data.tp)
          ),
          AnnotationsDecl.empty
        )

    // BLS12_381 operations
    // G1 operations
    val bls12_381_G1_add: SIR.Builtin = SIR.Builtin(
      DefaultFun.Bls12_381_G1_add,
      SIRType.BLS12_381_G1_Element ->: SIRType.BLS12_381_G1_Element ->: SIRType.BLS12_381_G1_Element,
      AnnotationsDecl.empty
    )
    val bls12_381_G1_neg: SIR.Builtin = SIR.Builtin(
      DefaultFun.Bls12_381_G1_neg,
      SIRType.BLS12_381_G1_Element ->: SIRType.BLS12_381_G1_Element,
      AnnotationsDecl.empty
    )
    val bls12_381_G1_scalarMul: SIR.Builtin = SIR.Builtin(
      DefaultFun.Bls12_381_G1_scalarMul,
      SIRType.Integer ->: SIRType.BLS12_381_G1_Element ->: SIRType.BLS12_381_G1_Element,
      AnnotationsDecl.empty
    )
    val bls12_381_G1_equal: SIR.Builtin = SIR.Builtin(
      DefaultFun.Bls12_381_G1_equal,
      SIRType.BLS12_381_G1_Element ->: SIRType.BLS12_381_G1_Element ->: SIRType.Boolean,
      AnnotationsDecl.empty
    )
    val bls12_381_G1_hashToGroup: SIR.Builtin = SIR.Builtin(
      DefaultFun.Bls12_381_G1_hashToGroup,
      SIRType.ByteString ->: SIRType.ByteString ->: SIRType.BLS12_381_G1_Element,
      AnnotationsDecl.empty
    )
    val bls12_381_G1_compress: SIR.Builtin = SIR.Builtin(
      DefaultFun.Bls12_381_G1_compress,
      SIRType.BLS12_381_G1_Element ->: SIRType.ByteString,
      AnnotationsDecl.empty
    )
    val bls12_381_G1_uncompress: SIR.Builtin = SIR.Builtin(
      DefaultFun.Bls12_381_G1_uncompress,
      SIRType.ByteString ->: SIRType.BLS12_381_G1_Element,
      AnnotationsDecl.empty
    )

    // G2 operations
    val bls12_381_G2_add: SIR.Builtin = SIR.Builtin(
      DefaultFun.Bls12_381_G2_add,
      SIRType.BLS12_381_G2_Element ->: SIRType.BLS12_381_G2_Element ->: SIRType.BLS12_381_G2_Element,
      AnnotationsDecl.empty
    )
    val bls12_381_G2_neg: SIR.Builtin = SIR.Builtin(
      DefaultFun.Bls12_381_G2_neg,
      SIRType.BLS12_381_G2_Element ->: SIRType.BLS12_381_G2_Element,
      AnnotationsDecl.empty
    )
    val bls12_381_G2_scalarMul: SIR.Builtin = SIR.Builtin(
      DefaultFun.Bls12_381_G2_scalarMul,
      SIRType.Integer ->: SIRType.BLS12_381_G2_Element ->: SIRType.BLS12_381_G2_Element,
      AnnotationsDecl.empty
    )
    val bls12_381_G2_equal: SIR.Builtin = SIR.Builtin(
      DefaultFun.Bls12_381_G2_equal,
      SIRType.BLS12_381_G2_Element ->: SIRType.BLS12_381_G2_Element ->: SIRType.Boolean,
      AnnotationsDecl.empty
    )
    val bls12_381_G2_hashToGroup: SIR.Builtin = SIR.Builtin(
      DefaultFun.Bls12_381_G2_hashToGroup,
      SIRType.ByteString ->: SIRType.ByteString ->: SIRType.BLS12_381_G2_Element,
      AnnotationsDecl.empty
    )
    val bls12_381_G2_compress: SIR.Builtin = SIR.Builtin(
      DefaultFun.Bls12_381_G2_compress,
      SIRType.BLS12_381_G2_Element ->: SIRType.ByteString,
      AnnotationsDecl.empty
    )
    val bls12_381_G2_uncompress: SIR.Builtin = SIR.Builtin(
      DefaultFun.Bls12_381_G2_uncompress,
      SIRType.ByteString ->: SIRType.BLS12_381_G2_Element,
      AnnotationsDecl.empty
    )

    val bls12_381_mulMlResult: SIR.Builtin = SIR.Builtin(
      DefaultFun.Bls12_381_mulMlResult,
      SIRType.BLS12_381_MlResult ->: SIRType.BLS12_381_MlResult ->: SIRType.BLS12_381_MlResult,
      AnnotationsDecl.empty
    )

    // Miller loop
    val bls12_381_millerLoop: SIR.Builtin = SIR.Builtin(
      DefaultFun.Bls12_381_millerLoop,
      SIRType.BLS12_381_G1_Element ->: SIRType.BLS12_381_G2_Element ->: SIRType.BLS12_381_MlResult,
      AnnotationsDecl.empty
    )
    // Final verification
    val bls12_381_finalVerify: SIR.Builtin = SIR.Builtin(
      DefaultFun.Bls12_381_finalVerify,
      SIRType.BLS12_381_MlResult ->: SIRType.BLS12_381_MlResult ->: SIRType.Boolean,
      AnnotationsDecl.empty
    )

    // Multi-scalar multiplication
    val bls12_381_G1_multiScalarMul: SIR.Builtin = SIR.Builtin(
      DefaultFun.Bls12_381_G1_multiScalarMul,
      SIRType.List(SIRType.Integer) ->: SIRType.List(
        SIRType.BLS12_381_G1_Element
      ) ->: SIRType.BLS12_381_G1_Element,
      AnnotationsDecl.empty
    )
    val bls12_381_G2_multiScalarMul: SIR.Builtin = SIR.Builtin(
      DefaultFun.Bls12_381_G2_multiScalarMul,
      SIRType.List(SIRType.Integer) ->: SIRType.List(
        SIRType.BLS12_381_G2_Element
      ) ->: SIRType.BLS12_381_G2_Element,
      AnnotationsDecl.empty
    )

    // keccak_256
    val keccak_256: SIR.Builtin = SIR.Builtin(
      DefaultFun.Keccak_256,
      SIRType.ByteString ->: SIRType.ByteString,
      AnnotationsDecl.empty
    )
    // blake
    val blake2b_224: SIR.Builtin = SIR.Builtin(
      DefaultFun.Blake2b_224,
      SIRType.ByteString ->: SIRType.ByteString,
      AnnotationsDecl.empty
    )
    // IntegerToByteString
    val integerToByteString: SIR.Builtin = SIR.Builtin(
      DefaultFun.IntegerToByteString,
      SIRType.Boolean ->: SIRType.Integer ->: SIRType.Integer ->: SIRType.ByteString,
      AnnotationsDecl.empty
    )
    // ByteStringToInteger
    val byteStringToInteger: SIR.Builtin = SIR.Builtin(
      DefaultFun.ByteStringToInteger,
      SIRType.Boolean ->: SIRType.ByteString ->: SIRType.Integer,
      AnnotationsDecl.empty
    )

    val andByteString: SIR.Builtin = SIR.Builtin(
      DefaultFun.AndByteString,
      SIRType.Boolean ->: SIRType.ByteString ->: SIRType.ByteString ->: SIRType.ByteString,
      AnnotationsDecl.empty
    )

    val orByteString: SIR.Builtin = SIR.Builtin(
      DefaultFun.OrByteString,
      SIRType.Boolean ->: SIRType.ByteString ->: SIRType.ByteString ->: SIRType.ByteString,
      AnnotationsDecl.empty
    )

    val xorByteString: SIR.Builtin = SIR.Builtin(
      DefaultFun.XorByteString,
      SIRType.Boolean ->: SIRType.ByteString ->: SIRType.ByteString ->: SIRType.ByteString,
      AnnotationsDecl.empty
    )

    val complementByteString: SIR.Builtin = SIR.Builtin(
      DefaultFun.ComplementByteString,
      SIRType.ByteString ->: SIRType.ByteString,
      AnnotationsDecl.empty
    )

    val readBit: SIR.Builtin = SIR.Builtin(
      DefaultFun.ReadBit,
      SIRType.ByteString ->: SIRType.Integer ->: SIRType.Boolean,
      AnnotationsDecl.empty
    )

    val writeBits: SIR.Builtin = SIR.Builtin(
      DefaultFun.WriteBits,
      SIRType.ByteString ->:
          SIRType.BuiltinList(SIRType.Integer) ->: SIRType.Boolean ->: SIRType.ByteString,
      AnnotationsDecl.empty
    )

    val replicateByte: SIR.Builtin = SIR.Builtin(
      DefaultFun.ReplicateByte,
      SIRType.Integer ->: SIRType.Integer ->: SIRType.ByteString,
      AnnotationsDecl.empty
    )

    val shiftByteString: SIR.Builtin = SIR.Builtin(
      DefaultFun.ShiftByteString,
      SIRType.ByteString ->: SIRType.Integer ->: SIRType.ByteString,
      AnnotationsDecl.empty
    )

    val rotateByteString: SIR.Builtin = SIR.Builtin(
      DefaultFun.RotateByteString,
      SIRType.ByteString ->: SIRType.Integer ->: SIRType.ByteString,
      AnnotationsDecl.empty
    )

    val countSetBits: SIR.Builtin = SIR.Builtin(
      DefaultFun.CountSetBits,
      SIRType.ByteString ->: SIRType.Integer,
      AnnotationsDecl.empty
    )

    val findFirstSetBit: SIR.Builtin = SIR.Builtin(
      DefaultFun.FindFirstSetBit,
      SIRType.ByteString ->: SIRType.Integer,
      AnnotationsDecl.empty
    )

    val ripemd_160: SIR.Builtin = SIR.Builtin(
      DefaultFun.Ripemd_160,
      SIRType.ByteString ->: SIRType.ByteString,
      AnnotationsDecl.empty
    )

    // Plutus 1.53 new builtins
    val dropList: SIR.Builtin = SIR.Builtin(
      DefaultFun.DropList,
      SIRType.TypeLambda(
        "dropList_A",
        a => SIRType.Integer ->: SIRType.BuiltinList(a) ->: SIRType.BuiltinList(a),
        true
      ),
      AnnotationsDecl.empty
    )

    // Array builtins
    val lengthOfArray: SIR.Builtin = SIR.Builtin(
      DefaultFun.LengthOfArray,
      SIRType.TypeLambda("lengthOfArray_A", a => SIRType.BuiltinArray(a) ->: SIRType.Integer, true),
      AnnotationsDecl.empty
    )

    val listToArray: SIR.Builtin = SIR.Builtin(
      DefaultFun.ListToArray,
      SIRType.TypeLambda(
        "listToArray_A",
        a => SIRType.BuiltinList(a) ->: SIRType.BuiltinArray(a),
        true
      ),
      AnnotationsDecl.empty
    )

    val indexArray: SIR.Builtin = SIR.Builtin(
      DefaultFun.IndexArray,
      SIRType
          .TypeLambda("indexArray_A", a => SIRType.BuiltinArray(a) ->: SIRType.Integer ->: a, true),
      AnnotationsDecl.empty
    )

    // multiIndexArray :: forall a. List Integer -> Array a -> List a
    val multiIndexArray: SIR.Builtin = SIR.Builtin(
      DefaultFun.MultiIndexArray,
      SIRType.TypeLambda(
        "multiIndexArray_A",
        a =>
            SIRType.BuiltinList(SIRType.Integer) ->: SIRType.BuiltinArray(a) ->: SIRType
                .BuiltinList(a),
        true
      ),
      AnnotationsDecl.empty
    )

    // MaryEraValue builtins (CIP-0153)
    // insertCoin :: ByteString -> ByteString -> Integer -> BuiltinValue -> BuiltinValue
    val insertCoin: SIR.Builtin = SIR.Builtin(
      DefaultFun.InsertCoin,
      SIRType.ByteString ->: SIRType.ByteString ->: SIRType.Integer ->: SIRType.BuiltinValue ->: SIRType.BuiltinValue,
      AnnotationsDecl.empty
    )

    // lookupCoin :: ByteString -> ByteString -> BuiltinValue -> Integer
    val lookupCoin: SIR.Builtin = SIR.Builtin(
      DefaultFun.LookupCoin,
      SIRType.ByteString ->: SIRType.ByteString ->: SIRType.BuiltinValue ->: SIRType.Integer,
      AnnotationsDecl.empty
    )

    // unionValue :: BuiltinValue -> BuiltinValue -> BuiltinValue
    val unionValue: SIR.Builtin = SIR.Builtin(
      DefaultFun.UnionValue,
      SIRType.BuiltinValue ->: SIRType.BuiltinValue ->: SIRType.BuiltinValue,
      AnnotationsDecl.empty
    )

    // valueContains :: BuiltinValue -> BuiltinValue -> Bool
    val valueContains: SIR.Builtin = SIR.Builtin(
      DefaultFun.ValueContains,
      SIRType.BuiltinValue ->: SIRType.BuiltinValue ->: SIRType.Boolean,
      AnnotationsDecl.empty
    )

    // valueData :: BuiltinValue -> Data
    val valueData: SIR.Builtin = SIR.Builtin(
      DefaultFun.ValueData,
      SIRType.BuiltinValue ->: SIRType.Data(),
      AnnotationsDecl.empty
    )

    // unValueData :: Data -> BuiltinValue
    val unValueData: SIR.Builtin = SIR.Builtin(
      DefaultFun.UnValueData,
      SIRType.Data() ->: SIRType.BuiltinValue,
      AnnotationsDecl.empty
    )

    // scaleValue :: Integer -> BuiltinValue -> BuiltinValue
    val scaleValue: SIR.Builtin = SIR.Builtin(
      DefaultFun.ScaleValue,
      SIRType.Integer ->: SIRType.BuiltinValue ->: SIRType.BuiltinValue,
      AnnotationsDecl.empty
    )

    def fromUplc(uplcFun: DefaultFun): SIR.Builtin =
        uplcFun match
            case DefaultFun.AddInteger                      => addInteger
            case DefaultFun.SubtractInteger                 => subtractInteger
            case DefaultFun.MultiplyInteger                 => multiplyInteger
            case DefaultFun.DivideInteger                   => divideInteger
            case DefaultFun.QuotientInteger                 => quotientInteger
            case DefaultFun.RemainderInteger                => remainderInteger
            case DefaultFun.ModInteger                      => modInteger
            case DefaultFun.EqualsInteger                   => equalsInteger
            case DefaultFun.LessThanInteger                 => lessThanInteger
            case DefaultFun.LessThanEqualsInteger           => lessThanEqualsInteger
            case DefaultFun.AppendByteString                => appendByteString
            case DefaultFun.ConsByteString                  => consByteString
            case DefaultFun.SliceByteString                 => sliceByteString
            case DefaultFun.LengthOfByteString              => lengthOfByteString
            case DefaultFun.IndexByteString                 => indexByteString
            case DefaultFun.EqualsByteString                => equalsByteString
            case DefaultFun.LessThanByteString              => lessThanByteString
            case DefaultFun.LessThanEqualsByteString        => lessThanEqualsByteString
            case DefaultFun.Sha2_256                        => sha2_256
            case DefaultFun.Sha3_256                        => sha3_256
            case DefaultFun.Blake2b_256                     => blake2b_256
            case DefaultFun.VerifyEd25519Signature          => verifyEd25519Signature
            case DefaultFun.VerifyEcdsaSecp256k1Signature   => verifyEcdsaSecp256k1Signature
            case DefaultFun.VerifySchnorrSecp256k1Signature => verifySchnorrSecp256k1Signature
            case DefaultFun.AppendString                    => appendString
            case DefaultFun.EqualsString                    => equalsString
            case DefaultFun.EncodeUtf8                      => encodeUtf8
            case DefaultFun.DecodeUtf8                      => decodeUtf8
            case DefaultFun.IfThenElse                      => ifThenElse
            case DefaultFun.ChooseUnit                      => chooseUnit
            case DefaultFun.Trace                           => trace
            case DefaultFun.FstPair                         => fstPair
            case DefaultFun.SndPair                         => sndPair
            case DefaultFun.ChooseList                      => chooseList
            case DefaultFun.MkCons                          => mkCons
            case DefaultFun.HeadList                        => headList
            case DefaultFun.TailList                        => tailList
            case DefaultFun.NullList                        => nullList
            case DefaultFun.ChooseData                      => chooseData
            case DefaultFun.ConstrData                      => constrData
            case DefaultFun.MapData                         => mapData
            case DefaultFun.ListData                        => listData
            case DefaultFun.IData                           => iData
            case DefaultFun.BData                           => bData
            case DefaultFun.UnConstrData                    => unConstrData
            case DefaultFun.UnMapData                       => unMapData
            case DefaultFun.UnListData                      => unListData
            case DefaultFun.UnIData                         => unIData
            case DefaultFun.UnBData                         => unBData
            case DefaultFun.EqualsData                      => equalsData
            case DefaultFun.SerialiseData                   => serialiseData
            case DefaultFun.MkPairData                      => mkPairData
            case DefaultFun.MkNilData                       => mkNilData
            case DefaultFun.MkNilPairData                   => mkNilPairData
            case DefaultFun.Bls12_381_G1_add                => bls12_381_G1_add
            case DefaultFun.Bls12_381_G1_neg                => bls12_381_G1_neg
            case DefaultFun.Bls12_381_G1_scalarMul          => bls12_381_G1_scalarMul
            case DefaultFun.Bls12_381_G1_equal              => bls12_381_G1_equal
            case DefaultFun.Bls12_381_G1_hashToGroup        => bls12_381_G1_hashToGroup
            case DefaultFun.Bls12_381_G1_compress           => bls12_381_G1_compress
            case DefaultFun.Bls12_381_G1_uncompress         => bls12_381_G1_uncompress
            case DefaultFun.Bls12_381_G2_add                => bls12_381_G2_add
            case DefaultFun.Bls12_381_G2_neg                => bls12_381_G2_neg
            case DefaultFun.Bls12_381_G2_scalarMul          => bls12_381_G2_scalarMul
            case DefaultFun.Bls12_381_G2_equal              => bls12_381_G2_equal
            case DefaultFun.Bls12_381_G2_hashToGroup        => bls12_381_G2_hashToGroup
            case DefaultFun.Bls12_381_G2_compress           => bls12_381_G2_compress
            case DefaultFun.Bls12_381_G2_uncompress         => bls12_381_G2_uncompress
            case DefaultFun.Bls12_381_millerLoop            => bls12_381_millerLoop
            case DefaultFun.Bls12_381_mulMlResult           => bls12_381_mulMlResult
            case DefaultFun.Bls12_381_finalVerify           => bls12_381_finalVerify
            case DefaultFun.Bls12_381_G1_multiScalarMul     => bls12_381_G1_multiScalarMul
            case DefaultFun.Bls12_381_G2_multiScalarMul     => bls12_381_G2_multiScalarMul
            case DefaultFun.Keccak_256                      => keccak_256
            case DefaultFun.Blake2b_224                     => blake2b_224
            case DefaultFun.IntegerToByteString             => integerToByteString
            case DefaultFun.ByteStringToInteger             => byteStringToInteger
            case DefaultFun.AndByteString                   => andByteString
            case DefaultFun.OrByteString                    => orByteString
            case DefaultFun.XorByteString                   => xorByteString
            case DefaultFun.ComplementByteString            => complementByteString
            case DefaultFun.ReadBit                         => readBit
            case DefaultFun.WriteBits                       => writeBits
            case DefaultFun.ReplicateByte                   => replicateByte
            case DefaultFun.ShiftByteString                 => shiftByteString
            case DefaultFun.RotateByteString                => rotateByteString
            case DefaultFun.CountSetBits                    => countSetBits
            case DefaultFun.FindFirstSetBit                 => findFirstSetBit
            case DefaultFun.Ripemd_160                      => ripemd_160
            case DefaultFun.ExpModInteger                   => expModInteger
            // Plutus 1.53 new builtins
            case DefaultFun.DropList => dropList
            // Array builtins
            case DefaultFun.LengthOfArray   => lengthOfArray
            case DefaultFun.ListToArray     => listToArray
            case DefaultFun.IndexArray      => indexArray
            case DefaultFun.MultiIndexArray => multiIndexArray
            // MaryEraValue builtins (CIP-0153)
            case DefaultFun.InsertCoin    => insertCoin
            case DefaultFun.LookupCoin    => lookupCoin
            case DefaultFun.UnionValue    => unionValue
            case DefaultFun.ValueContains => valueContains
            case DefaultFun.ValueData     => valueData
            case DefaultFun.UnValueData   => unValueData
            case DefaultFun.ScaleValue    => scaleValue
}
