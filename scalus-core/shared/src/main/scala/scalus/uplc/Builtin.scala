package scalus.uplc

import scalus.builtin.*
import scalus.builtin.Builtins.*
import scalus.uplc.Constant.given
import scalus.uplc.DefaultUni.{asConstant, Bool, Integer, given}
import scalus.uplc.eval.CekValue.*
import scalus.uplc.eval.*
import scalus.utils.Macros

import scala.collection.immutable
import scala.collection.immutable.ArraySeq

case class BuiltinRuntime(
    typeScheme: TypeScheme,
    f: (Logger, Seq[CekValue]) => CekValue,
    args: Seq[CekValue],
    costFunction: CostingFun
) {
    def apply(logger: Logger): CekValue = f(logger, args)

    def calculateCost: ExBudget = costFunction.calculateCost(args*)
}

@annotation.nowarn(
  "msg=deprecated"
) // suppress deprecation warning for extending BuiltinsMeaning, remove later along with BuiltinsMeaning

class CardanoBuiltins(
    builtinCostModel: BuiltinCostModel,
    platformSpecific: PlatformSpecific,
    semanticVariant: BuiltinSemanticsVariant
) extends BuiltinsMeaning(
      builtinCostModel,
      platformSpecific,
      semanticVariant
    )

@deprecated("Use CardanoBuiltins instead", "0.12.1")
class BuiltinsMeaning(
    builtinCostModel: BuiltinCostModel,
    platformSpecific: PlatformSpecific,
    semanticVariant: BuiltinSemanticsVariant
):
    // local extension used to create a TypeScheme from a DefaultUni
    extension (x: DefaultUni)
        def ->:(t: DefaultUni): TypeScheme =
            TypeScheme.Arrow(TypeScheme.Type(t), TypeScheme.Type(x))
        infix def $(t: TypeScheme): TypeScheme = TypeScheme.Type(x) $ t
        infix def $(t: String): TypeScheme = TypeScheme.Type(x) $ t

    def mkMeaning(
        t: TypeScheme,
        f: (logger: Logger, args: Seq[CekValue]) => CekValue,
        costFunction: CostingFun
    ) =
        BuiltinRuntime(t, f, ArraySeq.empty, costFunction)
    import TypeScheme.*

    val AddInteger: BuiltinRuntime =
        mkMeaning(
          Integer ->: Integer ->: Integer,
          (_: Logger, args: Seq[CekValue]) =>
              val a = args(0).asInteger
              val b = args(1).asInteger
              VCon(asConstant(addInteger(a, b)))
          ,
          builtinCostModel.addInteger
        )

    val SubtractInteger: BuiltinRuntime =
        mkMeaning(
          Integer ->: Integer ->: Integer,
          (_: Logger, args: Seq[CekValue]) =>
              val a = args(0).asInteger
              val b = args(1).asInteger
              VCon(asConstant(subtractInteger(a, b)))
          ,
          builtinCostModel.subtractInteger
        )

    val MultiplyInteger: BuiltinRuntime =
        mkMeaning(
          Integer ->: Integer ->: Integer,
          (_: Logger, args: Seq[CekValue]) =>
              val a = args(0).asInteger
              val b = args(1).asInteger
              VCon(asConstant(multiplyInteger(a, b)))
          ,
          builtinCostModel.multiplyInteger
        )

    val DivideInteger: BuiltinRuntime =
        mkMeaning(
          Integer ->: Integer ->: Integer,
          (_: Logger, args: Seq[CekValue]) =>
              val a = args(0).asInteger
              val b = args(1).asInteger
              VCon(asConstant(divideInteger(a, b)))
          ,
          builtinCostModel.divideInteger
        )

    val QuotientInteger: BuiltinRuntime =
        mkMeaning(
          Integer ->: Integer ->: Integer,
          (_: Logger, args: Seq[CekValue]) =>
              val a = args(0).asInteger
              val b = args(1).asInteger
              VCon(asConstant(quotientInteger(a, b)))
          ,
          builtinCostModel.quotientInteger
        )

    val RemainderInteger: BuiltinRuntime =
        mkMeaning(
          Integer ->: Integer ->: Integer,
          (_: Logger, args: Seq[CekValue]) =>
              val a = args(0).asInteger
              val b = args(1).asInteger
              VCon(asConstant(remainderInteger(a, b)))
          ,
          builtinCostModel.remainderInteger
        )

    val ModInteger: BuiltinRuntime =
        mkMeaning(
          Integer ->: Integer ->: Integer,
          (_: Logger, args: Seq[CekValue]) =>
              val a = args(0).asInteger
              val b = args(1).asInteger
              VCon(asConstant(modInteger(a, b)))
          ,
          builtinCostModel.modInteger
        )

    val EqualsInteger: BuiltinRuntime =
        mkMeaning(
          Integer ->: Integer ->: Bool,
          (_: Logger, args: Seq[CekValue]) =>
              val a = args(0).asInteger
              val b = args(1).asInteger
              VCon(asConstant(equalsInteger(a, b)))
          ,
          builtinCostModel.equalsInteger
        )

    val LessThanEqualsInteger: BuiltinRuntime =
        mkMeaning(
          Integer ->: Integer ->: Bool,
          (_: Logger, args: Seq[CekValue]) =>
              val aa = args(0).asInteger
              val bb = args(1).asInteger
              VCon(asConstant(lessThanEqualsInteger(aa, bb)))
          ,
          builtinCostModel.lessThanEqualsInteger
        )

    val LessThanInteger: BuiltinRuntime =
        mkMeaning(
          Integer ->: Integer ->: Bool,
          (_: Logger, args: Seq[CekValue]) =>
              val a = args(0).asInteger
              val b = args(1).asInteger
              VCon(asConstant(lessThanInteger(a, b)))
          ,
          builtinCostModel.lessThanInteger
        )

    val AppendByteString: BuiltinRuntime =
        mkMeaning(
          DefaultUni.ByteString ->: DefaultUni.ByteString ->: DefaultUni.ByteString,
          (_: Logger, args: Seq[CekValue]) =>
              val a = args(0).asByteString
              val b = args(1).asByteString
              VCon(asConstant(appendByteString(a, b)))
          ,
          builtinCostModel.appendByteString
        )

    val ConsByteString: BuiltinRuntime =
        mkMeaning(
          DefaultUni.Integer ->: DefaultUni.ByteString ->: DefaultUni.ByteString,
          (_: Logger, args: Seq[CekValue]) =>
              val char = args(0).asInteger
              val byteString = args(1).asByteString
              val result = semanticVariant match
                  case BuiltinSemanticsVariant.A | BuiltinSemanticsVariant.B =>
                      // essentially, char % 256
                      ByteString.unsafeFromArray(char.toByte +: byteString.bytes)
                  case BuiltinSemanticsVariant.C =>
                      if char < 0 || char > 255 then
                          throw new BuiltinException(s"consByteString: invalid byte value: $char")
                      ByteString.unsafeFromArray(char.toByte +: byteString.bytes)
              VCon(asConstant(result))
          ,
          builtinCostModel.consByteString
        )

    val SliceByteString: BuiltinRuntime =
        mkMeaning(
          DefaultUni.Integer ->: DefaultUni.Integer ->: DefaultUni.ByteString ->: DefaultUni.ByteString,
          (_: Logger, args: Seq[CekValue]) =>
              val start = args(0).asInteger
              val n = args(1).asInteger
              val bs = args(2).asByteString
              VCon(asConstant(sliceByteString(start, n, bs)))
          ,
          builtinCostModel.sliceByteString
        )

    val IndexByteString: BuiltinRuntime =
        mkMeaning(
          DefaultUni.ByteString ->: DefaultUni.Integer ->: DefaultUni.Integer,
          (_: Logger, args: Seq[CekValue]) =>
              val aa = args(0).asByteString
              val bb = args(1).asInteger
              VCon(asConstant(indexByteString(aa, bb)))
          ,
          builtinCostModel.indexByteString
        )

    val LengthOfByteString: BuiltinRuntime =
        mkMeaning(
          DefaultUni.ByteString ->: DefaultUni.Integer,
          (_: Logger, args: Seq[CekValue]) =>
              val aa = args(0).asByteString
              VCon(asConstant(lengthOfByteString(aa)))
          ,
          builtinCostModel.lengthOfByteString
        )

    val EqualsByteString: BuiltinRuntime =
        mkMeaning(
          DefaultUni.ByteString ->: DefaultUni.ByteString ->: Bool,
          (_: Logger, args: Seq[CekValue]) =>
              val aa = args(0).asByteString
              val bb = args(1).asByteString
              VCon(asConstant(aa == bb))
          ,
          builtinCostModel.equalsByteString
        )

    val LessThanByteString: BuiltinRuntime =
        mkMeaning(
          DefaultUni.ByteString ->: DefaultUni.ByteString ->: Bool,
          (_: Logger, args: Seq[CekValue]) =>
              val aa = args(0).asByteString
              val bb = args(1).asByteString
              VCon(asConstant(lessThanByteString(aa, bb)))
          ,
          builtinCostModel.lessThanByteString
        )

    val LessThanEqualsByteString: BuiltinRuntime =
        mkMeaning(
          DefaultUni.ByteString ->: DefaultUni.ByteString ->: Bool,
          (_: Logger, args: Seq[CekValue]) =>
              val aa = args(0).asByteString
              val bb = args(1).asByteString
              VCon(asConstant(lessThanEqualsByteString(aa, bb)))
          ,
          builtinCostModel.lessThanEqualsByteString
        )

    val Sha2_256: BuiltinRuntime =
        mkMeaning(
          DefaultUni.ByteString ->: DefaultUni.ByteString,
          (_: Logger, args: Seq[CekValue]) =>
              val aa = args(0).asByteString
              VCon(asConstant(platformSpecific.sha2_256(aa)))
          ,
          builtinCostModel.sha2_256
        )

    val Sha3_256: BuiltinRuntime =
        mkMeaning(
          DefaultUni.ByteString ->: DefaultUni.ByteString,
          (_: Logger, args: Seq[CekValue]) =>
              val aa = args(0).asByteString
              VCon(asConstant(platformSpecific.sha3_256(aa)))
          ,
          builtinCostModel.sha3_256
        )

    val Blake2b_256: BuiltinRuntime =
        mkMeaning(
          DefaultUni.ByteString ->: DefaultUni.ByteString,
          (_: Logger, args: Seq[CekValue]) =>
              val aa = args(0).asByteString
              VCon(asConstant(platformSpecific.blake2b_256(aa)))
          ,
          builtinCostModel.blake2b_256
        )

    val Blake2b_224: BuiltinRuntime =
        mkMeaning(
          DefaultUni.ByteString ->: DefaultUni.ByteString,
          (_: Logger, args: Seq[CekValue]) =>
              val aa = args(0).asByteString
              VCon(asConstant(platformSpecific.blake2b_224(aa)))
          ,
          builtinCostModel.blake2b_224
        )

    val VerifyEd25519Signature: BuiltinRuntime =
        mkMeaning(
          DefaultUni.ByteString ->: DefaultUni.ByteString ->: DefaultUni.ByteString ->: DefaultUni.Bool,
          (_: Logger, args: Seq[CekValue]) =>
              val pk = args(0).asByteString
              val msg = args(1).asByteString
              val sig = args(2).asByteString
              VCon(asConstant(platformSpecific.verifyEd25519Signature(pk, msg, sig)))
          ,
          builtinCostModel.verifyEd25519Signature
        )

    val VerifyEcdsaSecp256k1Signature: BuiltinRuntime =
        mkMeaning(
          DefaultUni.ByteString ->: DefaultUni.ByteString ->: DefaultUni.ByteString ->: DefaultUni.Bool,
          (_: Logger, args: Seq[CekValue]) =>
              val pk = args(0).asByteString
              val msg = args(1).asByteString
              val sig = args(2).asByteString
              VCon(asConstant(platformSpecific.verifyEcdsaSecp256k1Signature(pk, msg, sig)))
          ,
          builtinCostModel.verifyEcdsaSecp256k1Signature
        )

    val VerifySchnorrSecp256k1Signature: BuiltinRuntime =
        mkMeaning(
          DefaultUni.ByteString ->: DefaultUni.ByteString ->: DefaultUni.ByteString ->: DefaultUni.Bool,
          (_: Logger, args: Seq[CekValue]) =>
              val pk = args(0).asByteString
              val msg = args(1).asByteString
              val sig = args(2).asByteString
              VCon(asConstant(platformSpecific.verifySchnorrSecp256k1Signature(pk, msg, sig)))
          ,
          builtinCostModel.verifySchnorrSecp256k1Signature
        )

    val AppendString: BuiltinRuntime =
        mkMeaning(
          DefaultUni.String ->: DefaultUni.String ->: DefaultUni.String,
          (_: Logger, args: Seq[CekValue]) =>
              val aa = args(0).asString
              val bb = args(1).asString
              VCon(asConstant(appendString(aa, bb)))
          ,
          builtinCostModel.appendString
        )

    val EqualsString: BuiltinRuntime =
        mkMeaning(
          DefaultUni.String ->: DefaultUni.String ->: Bool,
          (_: Logger, args: Seq[CekValue]) =>
              val aa = args(0).asString
              val bb = args(1).asString
              VCon(asConstant(equalsString(aa, bb)))
          ,
          builtinCostModel.equalsString
        )

    val EncodeUtf8: BuiltinRuntime =
        mkMeaning(
          DefaultUni.String ->: DefaultUni.ByteString,
          (_: Logger, args: Seq[CekValue]) =>
              val aa = args(0).asString
              VCon(asConstant(encodeUtf8(aa)))
          ,
          builtinCostModel.encodeUtf8
        )

    val DecodeUtf8: BuiltinRuntime =
        mkMeaning(
          DefaultUni.ByteString ->: DefaultUni.String,
          (_: Logger, args: Seq[CekValue]) =>
              val aa = args(0).asByteString
              VCon(asConstant(decodeUtf8(aa)))
          ,
          builtinCostModel.decodeUtf8
        )

    val IfThenElse: BuiltinRuntime =
        mkMeaning(
          All("a", Bool ->: TVar("a") ->: TVar("a") ->: TVar("a")),
          (_: Logger, args: Seq[CekValue]) =>
              val bb = args(0).asBool
              val t = args(1)
              val f = args(2)
              ifThenElse(bb, t, f)
          ,
          builtinCostModel.ifThenElse
        )

    val ChooseUnit: BuiltinRuntime =
        mkMeaning(
          All("a", DefaultUni.Unit ->: TVar("a") ->: TVar("a")),
          (_: Logger, args: Seq[CekValue]) =>
              args(0) match
                  case VCon(Constant.Unit) => args(1)
                  case _ => throw new DeserializationError(DefaultFun.ChooseUnit, args(0))
          ,
          builtinCostModel.chooseUnit
        )

    val Trace: BuiltinRuntime =
        mkMeaning(
          All("a", DefaultUni.String ->: TVar("a") ->: TVar("a")),
          (logger: Logger, args: Seq[CekValue]) =>
              val aa = args(0).asString
              logger.log(aa)
              args(1)
          ,
          builtinCostModel.trace
        )

    // [ forall a, forall b, pair(a, b) ] -> a
    val FstPair: BuiltinRuntime =
        mkMeaning(
          All("a", All("b", (DefaultUni.ProtoPair $ "a" $ "b") ->: TVar("a"))),
          (_: Logger, args: Seq[CekValue]) =>
              val (fst, _) = args(0).asPair
              VCon(fst)
          ,
          builtinCostModel.fstPair
        )

    // [ forall a, forall b, pair(a, b) ] -> b
    val SndPair: BuiltinRuntime =
        mkMeaning(
          All("a", All("b", (DefaultUni.ProtoPair $ "a" $ "b") ->: TVar("b"))),
          (_: Logger, args: Seq[CekValue]) =>
              val (_, snd) = args(0).asPair
              VCon(snd)
          ,
          builtinCostModel.sndPair
        )

    // [ forall a, forall b, list(a), b, b ] -> b
    val ChooseList: BuiltinRuntime =
        mkMeaning(
          All(
            "a",
            All("b", (DefaultUni.ProtoList $ "a") ->: TVar("b") ->: TVar("b") ->: TVar("b"))
          ),
          (_: Logger, args: Seq[CekValue]) =>
              val ls = args(0).asList
              if ls.isEmpty then args(1) else args(2)
          ,
          builtinCostModel.chooseList
        )

    // [ forall a, a, list(a) ] -> list(a)
    val MkCons: BuiltinRuntime =
        mkMeaning(
          All("a", TVar("a") ->: (DefaultUni.ProtoList $ "a") ->: (DefaultUni.ProtoList $ "a")),
          (_: Logger, args: Seq[CekValue]) =>
              (args(0), args(1)) match
                  // Checking that the type of the constant is the same as the type of the elements
                  // of the unlifted list. Note that there's no way we could enforce this statically
                  // since in UPLC one can create an ill-typed program that attempts to prepend
                  // a value of the wrong type to a list.
                  case (VCon(aCon), VCon(Constant.List(tp, l))) =>
                      if aCon.tpe != tp then throw new KnownTypeUnliftingError(tp, args(0))
                      else VCon(Constant.List(tp, aCon :: l))
                  case _ => throw new DeserializationError(DefaultFun.MkCons, args(1))
          ,
          builtinCostModel.mkCons
        )

    // [ forall a, list(a) ] -> a
    val HeadList: BuiltinRuntime =
        mkMeaning(
          All("a", (DefaultUni.ProtoList $ "a") ->: TVar("a")),
          (_: Logger, args: Seq[CekValue]) => VCon(args(0).asList.head),
          builtinCostModel.headList
        )

    // [ forall a, list(a) ] -> list(a)
    val TailList: BuiltinRuntime =
        mkMeaning(
          All("a", (DefaultUni.ProtoList $ "a") ->: (DefaultUni.ProtoList $ "a")),
          (_: Logger, args: Seq[CekValue]) =>
              args(0) match
                  case VCon(Constant.List(tpe, ls)) => VCon(Constant.List(tpe, ls.tail))
                  case _ => throw new DeserializationError(DefaultFun.TailList, args(0))
          ,
          builtinCostModel.tailList
        )

    // [ forall a, list(a) ] -> bool
    val NullList: BuiltinRuntime =
        mkMeaning(
          All("a", (DefaultUni.ProtoList $ "a") ->: Type(Bool)),
          (_: Logger, args: Seq[CekValue]) => VCon(asConstant(args(0).asList.isEmpty)),
          builtinCostModel.nullList
        )

    val ChooseData: BuiltinRuntime =
        mkMeaning(
          All(
            "a",
            DefaultUni.Data ->: TVar("a") ->: TVar("a") ->: TVar("a") ->: TVar("a") ->: TVar(
              "a"
            ) ->: TVar("a")
          ),
          (_: Logger, args: Seq[CekValue]) =>
              val aa = args(0).asData
              chooseData(aa, args(1), args(2), args(3), args(4), args(5))
          ,
          builtinCostModel.chooseData
        )

    val ConstrData: BuiltinRuntime =
        mkMeaning(
          Integer ->: DefaultUni.List(DefaultUni.Data) ->: DefaultUni.Data,
          (_: Logger, args: Seq[CekValue]) =>
              val i = args(0).asInteger
              val argsList = args(1).asList.map {
                  case Constant.Data(d) => d
                  case _ => throw new DeserializationError(DefaultFun.ConstrData, args(1))
              }
              VCon(Constant.Data(Data.Constr(i.longValue, argsList)))
          ,
          builtinCostModel.constrData
        )

    val MapData: BuiltinRuntime =
        mkMeaning(
          DefaultUni.List(DefaultUni.Pair(DefaultUni.Data, DefaultUni.Data)) ->: DefaultUni.Data,
          (_: Logger, args: Seq[CekValue]) =>
              val aa = args(0).asList
              VCon(
                Constant.Data(Data.Map(aa.map {
                    case Constant.Pair(Constant.Data(a), Constant.Data(b)) => (a, b)
                    case _ =>
                        throw new KnownTypeUnliftingError(
                          DefaultUni.Pair(DefaultUni.Data, DefaultUni.Data),
                          args(0)
                        )
                }))
              )
          ,
          builtinCostModel.mapData
        )

    val ListData: BuiltinRuntime =
        mkMeaning(
          DefaultUni.List(DefaultUni.Data) ->: DefaultUni.Data,
          (_: Logger, args: Seq[CekValue]) =>
              val aa = args(0).asList
              VCon(Constant.Data(Data.List(aa.map {
                  case Constant.Data(value) => value
                  case _ => throw new KnownTypeUnliftingError(DefaultUni.Data, args(0))
              })))
          ,
          builtinCostModel.listData
        )

    val IData: BuiltinRuntime =
        mkMeaning(
          Integer ->: DefaultUni.Data,
          (_: Logger, args: Seq[CekValue]) =>
              val aa = args(0).asInteger
              VCon(Constant.Data(Data.I(aa)))
          ,
          builtinCostModel.iData
        )

    val BData: BuiltinRuntime =
        mkMeaning(
          DefaultUni.ByteString ->: DefaultUni.Data,
          (_: Logger, args: Seq[CekValue]) =>
              val aa = args(0).asByteString
              VCon(Constant.Data(Data.B(aa)))
          ,
          builtinCostModel.bData
        )

    /*
    unConstrData : [ data ] -> pair(integer, list(data))
     */
    val UnConstrData: BuiltinRuntime =
        mkMeaning(
          DefaultUni.Data ->: DefaultUni.Pair(Integer, DefaultUni.List(DefaultUni.Data)),
          (_: Logger, args: Seq[CekValue]) =>
              args(0) match
                  case VCon(Constant.Data(Data.Constr(i, ls))) =>
                      VCon(Constant.Pair(asConstant(i), asConstant(ls)))
                  case _ => throw new DeserializationError(DefaultFun.UnConstrData, args(0))
          ,
          builtinCostModel.unConstrData
        )

    val UnMapData: BuiltinRuntime =
        mkMeaning(
          DefaultUni.Data ->: DefaultUni.List(DefaultUni.Pair(DefaultUni.Data, DefaultUni.Data)),
          (_: Logger, args: Seq[CekValue]) =>
              args(0) match
                  case VCon(Constant.Data(Data.Map(values))) =>
                      VCon(
                        Constant.List(
                          DefaultUni.Pair(DefaultUni.Data, DefaultUni.Data),
                          values.map { case (k, v) =>
                              Constant.Pair(asConstant(k), asConstant(v))
                          }
                        )
                      )
                  case _ => throw new DeserializationError(DefaultFun.UnMapData, args(0))
          ,
          builtinCostModel.unMapData
        )

    val UnListData: BuiltinRuntime =
        mkMeaning(
          DefaultUni.Data ->: DefaultUni.List(DefaultUni.Data),
          (_: Logger, args: Seq[CekValue]) =>
              args(0) match
                  case VCon(Constant.Data(Data.List(values))) =>
                      VCon(Constant.List(DefaultUni.Data, values.map(asConstant)))
                  case _ => throw new DeserializationError(DefaultFun.UnListData, args(0))
          ,
          builtinCostModel.unListData
        )

    val UnIData: BuiltinRuntime =
        mkMeaning(
          DefaultUni.Data ->: DefaultUni.Integer,
          (_: Logger, args: Seq[CekValue]) =>
              args(0) match
                  case VCon(Constant.Data(Data.I(i))) =>
                      VCon(asConstant(i))
                  case _ => throw new DeserializationError(DefaultFun.UnIData, args(0))
          ,
          builtinCostModel.unIData
        )

    val UnBData: BuiltinRuntime =
        mkMeaning(
          DefaultUni.Data ->: DefaultUni.ByteString,
          (_: Logger, args: Seq[CekValue]) =>
              args(0) match
                  case VCon(Constant.Data(Data.B(b))) =>
                      VCon(asConstant(b))
                  case _ => throw new DeserializationError(DefaultFun.UnBData, args(0))
          ,
          builtinCostModel.unBData
        )

    val EqualsData: BuiltinRuntime =
        mkMeaning(
          DefaultUni.Data ->: DefaultUni.Data ->: DefaultUni.Bool,
          (_: Logger, args: Seq[CekValue]) =>
              args(0) match
                  case VCon(Constant.Data(aa)) =>
                      args(1) match
                          case VCon(Constant.Data(bb)) =>
                              VCon(Constant.Bool(equalsData(aa, bb)))
                          case _ => throw new DeserializationError(DefaultFun.EqualsData, args(1))

                  case _ => throw new DeserializationError(DefaultFun.EqualsData, args(0))
          ,
          builtinCostModel.equalsData
        )

    val SerialiseData: BuiltinRuntime =
        mkMeaning(
          DefaultUni.Data ->: DefaultUni.ByteString,
          (_: Logger, args: Seq[CekValue]) =>
              args(0) match
                  case VCon(Constant.Data(data)) =>
                      VCon(Constant.ByteString(serialiseData(data)))
                  case _ => throw new DeserializationError(DefaultFun.SerialiseData, args(0))
          ,
          builtinCostModel.serialiseData
        )

    val MkPairData: BuiltinRuntime =
        mkMeaning(
          DefaultUni.Data ->: DefaultUni.Data ->: DefaultUni.Pair(DefaultUni.Data, DefaultUni.Data),
          (_: Logger, args: Seq[CekValue]) =>
              val aa = args(0).asData
              val bb = args(1).asData
              VCon(Constant.Pair(asConstant(aa), asConstant(bb)))
          ,
          builtinCostModel.mkPairData
        )

    val MkNilData: BuiltinRuntime =
        mkMeaning(
          DefaultUni.Unit ->: DefaultUni.List(DefaultUni.Data),
          (_: Logger, args: Seq[CekValue]) =>
              val _ = args(0).asUnit
              VCon(Constant.List(DefaultUni.Data, Nil))
          ,
          builtinCostModel.mkNilData
        )

    val MkNilPairData: BuiltinRuntime = mkMeaning(
      DefaultUni.Unit ->: DefaultUni.Pair(DefaultUni.Data, DefaultUni.Data),
      (_: Logger, args: Seq[CekValue]) =>
          val _ = args(0).asUnit
          VCon(Constant.List(DefaultUni.Pair(DefaultUni.Data, DefaultUni.Data), Nil))
      ,
      builtinCostModel.mkNilPairData
    )

    val Keccak_256: BuiltinRuntime =
        mkMeaning(
          DefaultUni.ByteString ->: DefaultUni.ByteString,
          (_: Logger, args: Seq[CekValue]) =>
              val aa = args(0).asByteString
              VCon(asConstant(platformSpecific.keccak_256(aa)))
          ,
          builtinCostModel.keccak_256
        )

    val Bls12_381_G1_add: BuiltinRuntime = mkMeaning(
      DefaultUni.BLS12_381_G1_Element ->: DefaultUni.BLS12_381_G1_Element ->: DefaultUni.BLS12_381_G1_Element,
      (_: Logger, args: Seq[CekValue]) =>
          val aa = args(0) match {
              case VCon(Constant.BLS12_381_G1_Element(p)) => p
              case _ => throw new KnownTypeUnliftingError(DefaultUni.BLS12_381_G1_Element, args(0))
          }
          val bb = args(1) match {
              case VCon(Constant.BLS12_381_G1_Element(p)) => p
              case _ => throw new KnownTypeUnliftingError(DefaultUni.BLS12_381_G1_Element, args(1))
          }
          VCon(Constant.BLS12_381_G1_Element(platformSpecific.bls12_381_G1_add(aa, bb)))
      ,
      builtinCostModel.bls12_381_G1_add
    )

    val Bls12_381_G1_neg: BuiltinRuntime = mkMeaning(
      DefaultUni.BLS12_381_G1_Element ->: DefaultUni.BLS12_381_G1_Element,
      (_: Logger, args: Seq[CekValue]) =>
          val aa = args(0) match {
              case VCon(Constant.BLS12_381_G1_Element(p)) => p
              case _ => throw new KnownTypeUnliftingError(DefaultUni.BLS12_381_G1_Element, args(0))
          }
          VCon(Constant.BLS12_381_G1_Element(platformSpecific.bls12_381_G1_neg(aa)))
      ,
      builtinCostModel.bls12_381_G1_neg
    )

    val Bls12_381_G1_scalarMul: BuiltinRuntime = mkMeaning(
      DefaultUni.Integer ->: DefaultUni.BLS12_381_G1_Element ->: DefaultUni.BLS12_381_G1_Element,
      (_: Logger, args: Seq[CekValue]) =>
          val aa = args(0).asInteger
          val bb = args(1) match {
              case VCon(Constant.BLS12_381_G1_Element(p)) => p
              case _ => throw new KnownTypeUnliftingError(DefaultUni.BLS12_381_G1_Element, args(1))
          }
          VCon(Constant.BLS12_381_G1_Element(platformSpecific.bls12_381_G1_scalarMul(aa, bb)))
      ,
      builtinCostModel.bls12_381_G1_scalarMul
    )

    val Bls12_381_G1_equal: BuiltinRuntime = mkMeaning(
      DefaultUni.BLS12_381_G1_Element ->: DefaultUni.BLS12_381_G1_Element ->: DefaultUni.Bool,
      (_: Logger, args: Seq[CekValue]) =>
          val aa = args(0) match {
              case VCon(Constant.BLS12_381_G1_Element(p)) => p
              case _ => throw new KnownTypeUnliftingError(DefaultUni.BLS12_381_G1_Element, args(0))
          }
          val bb = args(1) match {
              case VCon(Constant.BLS12_381_G1_Element(p)) => p
              case _ => throw new KnownTypeUnliftingError(DefaultUni.BLS12_381_G1_Element, args(1))
          }
          VCon(asConstant(platformSpecific.bls12_381_G1_equal(aa, bb)))
      ,
      builtinCostModel.bls12_381_G1_equal
    )

    val Bls12_381_G1_compress: BuiltinRuntime = mkMeaning(
      DefaultUni.BLS12_381_G1_Element ->: DefaultUni.ByteString,
      (_: Logger, args: Seq[CekValue]) =>
          val aa = args(0) match {
              case VCon(Constant.BLS12_381_G1_Element(p)) => p
              case _ => throw new KnownTypeUnliftingError(DefaultUni.BLS12_381_G1_Element, args(0))
          }
          VCon(Constant.ByteString(platformSpecific.bls12_381_G1_compress(aa)))
      ,
      builtinCostModel.bls12_381_G1_compress
    )

    val Bls12_381_G1_uncompress: BuiltinRuntime = mkMeaning(
      DefaultUni.ByteString ->: DefaultUni.BLS12_381_G1_Element,
      (_: Logger, args: Seq[CekValue]) =>
          val aa = args(0).asByteString
          VCon(Constant.BLS12_381_G1_Element(platformSpecific.bls12_381_G1_uncompress(aa)))
      ,
      builtinCostModel.bls12_381_G1_uncompress
    )

    val Bls12_381_G1_hashToGroup: BuiltinRuntime = mkMeaning(
      DefaultUni.ByteString ->: DefaultUni.ByteString ->: DefaultUni.BLS12_381_G1_Element,
      (_: Logger, args: Seq[CekValue]) =>
          val aa = args(0).asByteString
          val bb = args(1).asByteString
          VCon(Constant.BLS12_381_G1_Element(platformSpecific.bls12_381_G1_hashToGroup(aa, bb)))
      ,
      builtinCostModel.bls12_381_G1_hashToGroup
    )

    val Bls12_381_G2_add: BuiltinRuntime = mkMeaning(
      DefaultUni.BLS12_381_G2_Element ->: DefaultUni.BLS12_381_G2_Element ->: DefaultUni.BLS12_381_G2_Element,
      (_: Logger, args: Seq[CekValue]) =>
          val aa = args(0) match {
              case VCon(Constant.BLS12_381_G2_Element(p)) => p
              case _ => throw new KnownTypeUnliftingError(DefaultUni.BLS12_381_G2_Element, args(0))
          }
          val bb = args(1) match {
              case VCon(Constant.BLS12_381_G2_Element(p)) => p
              case _ => throw new KnownTypeUnliftingError(DefaultUni.BLS12_381_G2_Element, args(1))
          }
          VCon(Constant.BLS12_381_G2_Element(platformSpecific.bls12_381_G2_add(aa, bb)))
      ,
      builtinCostModel.bls12_381_G2_add
    )

    val Bls12_381_G2_neg: BuiltinRuntime = mkMeaning(
      DefaultUni.BLS12_381_G2_Element ->: DefaultUni.BLS12_381_G2_Element,
      (_: Logger, args: Seq[CekValue]) =>
          val aa = args(0) match {
              case VCon(Constant.BLS12_381_G2_Element(p)) => p
              case _ => throw new KnownTypeUnliftingError(DefaultUni.BLS12_381_G2_Element, args(0))
          }
          VCon(Constant.BLS12_381_G2_Element(platformSpecific.bls12_381_G2_neg(aa)))
      ,
      builtinCostModel.bls12_381_G2_neg
    )

    val Bls12_381_G2_scalarMul: BuiltinRuntime = mkMeaning(
      DefaultUni.Integer ->: DefaultUni.BLS12_381_G2_Element ->: DefaultUni.BLS12_381_G2_Element,
      (_: Logger, args: Seq[CekValue]) =>
          val aa = args(0).asInteger
          val bb = args(1) match {
              case VCon(Constant.BLS12_381_G2_Element(p)) => p
              case _ => throw new KnownTypeUnliftingError(DefaultUni.BLS12_381_G2_Element, args(1))
          }
          VCon(Constant.BLS12_381_G2_Element(platformSpecific.bls12_381_G2_scalarMul(aa, bb)))
      ,
      builtinCostModel.bls12_381_G2_scalarMul
    )

    val Bls12_381_G2_equal: BuiltinRuntime = mkMeaning(
      DefaultUni.BLS12_381_G2_Element ->: DefaultUni.BLS12_381_G2_Element ->: DefaultUni.Bool,
      (_: Logger, args: Seq[CekValue]) =>
          val aa = args(0) match {
              case VCon(Constant.BLS12_381_G2_Element(p)) => p
              case _ => throw new KnownTypeUnliftingError(DefaultUni.BLS12_381_G2_Element, args(0))
          }
          val bb = args(1) match {
              case VCon(Constant.BLS12_381_G2_Element(p)) => p
              case _ => throw new KnownTypeUnliftingError(DefaultUni.BLS12_381_G2_Element, args(1))
          }
          VCon(asConstant(platformSpecific.bls12_381_G2_equal(aa, bb)))
      ,
      builtinCostModel.bls12_381_G2_equal
    )

    val Bls12_381_G2_compress: BuiltinRuntime = mkMeaning(
      DefaultUni.BLS12_381_G2_Element ->: DefaultUni.ByteString,
      (_: Logger, args: Seq[CekValue]) =>
          val aa = args(0) match {
              case VCon(Constant.BLS12_381_G2_Element(p)) => p
              case _ => throw new KnownTypeUnliftingError(DefaultUni.BLS12_381_G2_Element, args(0))
          }
          VCon(Constant.ByteString(platformSpecific.bls12_381_G2_compress(aa)))
      ,
      builtinCostModel.bls12_381_G2_compress
    )

    val Bls12_381_G2_uncompress: BuiltinRuntime = mkMeaning(
      DefaultUni.ByteString ->: DefaultUni.BLS12_381_G2_Element,
      (_: Logger, args: Seq[CekValue]) =>
          val aa = args(0).asByteString
          VCon(Constant.BLS12_381_G2_Element(platformSpecific.bls12_381_G2_uncompress(aa)))
      ,
      builtinCostModel.bls12_381_G2_uncompress
    )

    val Bls12_381_G2_hashToGroup: BuiltinRuntime = mkMeaning(
      DefaultUni.ByteString ->: DefaultUni.ByteString ->: DefaultUni.BLS12_381_G2_Element,
      (_: Logger, args: Seq[CekValue]) =>
          val aa = args(0).asByteString
          val bb = args(1).asByteString
          VCon(Constant.BLS12_381_G2_Element(platformSpecific.bls12_381_G2_hashToGroup(aa, bb)))
      ,
      builtinCostModel.bls12_381_G2_hashToGroup
    )

    val Bls12_381_millerLoop: BuiltinRuntime = mkMeaning(
      DefaultUni.BLS12_381_G1_Element ->: DefaultUni.BLS12_381_G2_Element ->: DefaultUni.BLS12_381_MlResult,
      (_: Logger, args: Seq[CekValue]) =>
          val aa = args(0) match {
              case VCon(Constant.BLS12_381_G1_Element(p)) => p
              case _ => throw new KnownTypeUnliftingError(DefaultUni.BLS12_381_G1_Element, args(0))
          }
          val bb = args(1) match {
              case VCon(Constant.BLS12_381_G2_Element(p)) => p
              case _ => throw new KnownTypeUnliftingError(DefaultUni.BLS12_381_G2_Element, args(1))
          }
          VCon(Constant.BLS12_381_MlResult(platformSpecific.bls12_381_millerLoop(aa, bb)))
      ,
      builtinCostModel.bls12_381_millerLoop
    )

    val Bls12_381_mulMlResult: BuiltinRuntime = mkMeaning(
      DefaultUni.BLS12_381_MlResult ->: DefaultUni.BLS12_381_MlResult ->: DefaultUni.BLS12_381_MlResult,
      (_: Logger, args: Seq[CekValue]) =>
          val aa = args(0) match {
              case VCon(Constant.BLS12_381_MlResult(p)) => p
              case _ => throw new KnownTypeUnliftingError(DefaultUni.BLS12_381_MlResult, args(0))
          }
          val bb = args(1) match {
              case VCon(Constant.BLS12_381_MlResult(p)) => p
              case _ => throw new KnownTypeUnliftingError(DefaultUni.BLS12_381_MlResult, args(1))
          }
          VCon(Constant.BLS12_381_MlResult(platformSpecific.bls12_381_mulMlResult(aa, bb)))
      ,
      builtinCostModel.bls12_381_mulMlResult
    )

    val Bls12_381_finalVerify: BuiltinRuntime = mkMeaning(
      DefaultUni.BLS12_381_MlResult ->: DefaultUni.BLS12_381_MlResult ->: DefaultUni.Bool,
      (_: Logger, args: Seq[CekValue]) =>
          val aa = args(0) match {
              case VCon(Constant.BLS12_381_MlResult(p)) => p
              case _ => throw new KnownTypeUnliftingError(DefaultUni.BLS12_381_MlResult, args(0))
          }
          val bb = args(1) match {
              case VCon(Constant.BLS12_381_MlResult(p)) => p
              case _ => throw new KnownTypeUnliftingError(DefaultUni.BLS12_381_MlResult, args(1))
          }
          VCon(asConstant(platformSpecific.bls12_381_finalVerify(aa, bb)))
      ,
      builtinCostModel.bls12_381_finalVerify
    )

    val IntegerToByteString: BuiltinRuntime =
        mkMeaning(
          Bool ->: Integer ->: Integer ->: DefaultUni.ByteString,
          (_: Logger, args: Seq[CekValue]) =>
              val endianness = args(0).asBool
              val length = args(1).asInteger
              val input = args(2).asInteger
              VCon(asConstant(integerToByteString(endianness, length, input)))
          ,
          builtinCostModel.integerToByteString
        )

    val ByteStringToInteger: BuiltinRuntime =
        mkMeaning(
          Bool ->: DefaultUni.ByteString ->: Integer,
          (_: Logger, args: Seq[CekValue]) =>
              val endianness = args(0).asBool
              val input = args(1).asByteString
              VCon(asConstant(byteStringToInteger(endianness, input)))
          ,
          builtinCostModel.byteStringToInteger
        )

    val AndByteString: BuiltinRuntime =
        mkMeaning(
          DefaultUni.Bool ->: DefaultUni.ByteString ->: DefaultUni.ByteString ->: DefaultUni.ByteString,
          (_: Logger, args: Seq[CekValue]) =>
              val shouldPad = args(0).asBool
              val lhs = args(1).asByteString
              val rhs = args(2).asByteString
              VCon(asConstant(andByteString(shouldPad, lhs, rhs)))
          ,
          builtinCostModel.andByteString
        )

    val OrByteString: BuiltinRuntime =
        mkMeaning(
          DefaultUni.Bool ->: DefaultUni.ByteString ->: DefaultUni.ByteString ->: DefaultUni.ByteString,
          (_: Logger, args: Seq[CekValue]) =>
              val shouldPad = args(0).asBool
              val lhs = args(1).asByteString
              val rhs = args(2).asByteString
              VCon(asConstant(orByteString(shouldPad, lhs, rhs)))
          ,
          builtinCostModel.orByteString
        )

    val XorByteString: BuiltinRuntime =
        mkMeaning(
          DefaultUni.Bool ->: DefaultUni.ByteString ->: DefaultUni.ByteString ->: DefaultUni.ByteString,
          (_: Logger, args: Seq[CekValue]) =>
              val shouldPad = args(0).asBool
              val lhs = args(1).asByteString
              val rhs = args(2).asByteString
              VCon(asConstant(xorByteString(shouldPad, lhs, rhs)))
          ,
          builtinCostModel.xorByteString
        )

    val ComplementByteString: BuiltinRuntime =
        mkMeaning(
          DefaultUni.ByteString ->: DefaultUni.ByteString,
          (_: Logger, args: Seq[CekValue]) =>
              val byteString = args(0).asByteString
              VCon(asConstant(complementByteString(byteString)))
          ,
          builtinCostModel.complementByteString
        )

    val ReadBit: BuiltinRuntime =
        mkMeaning(
          DefaultUni.ByteString ->: DefaultUni.Integer ->: DefaultUni.Bool,
          (_: Logger, args: Seq[CekValue]) =>
              val byteString = args(0).asByteString
              val index = args(1).asInteger
              VCon(asConstant(readBit(byteString, index)))
          ,
          builtinCostModel.readBit
        )

    val WriteBits: BuiltinRuntime =
        mkMeaning(
          DefaultUni.ByteString ->: DefaultUni.List(
            DefaultUni.Integer
          ) ->: DefaultUni.Bool ->: DefaultUni.ByteString,
          (_: Logger, args: Seq[CekValue]) =>
              val byteString = args(0).asByteString
              val indexes = args(1).asList.map {
                  case Constant.Integer(i) => i
                  case _ => throw new KnownTypeUnliftingError(DefaultUni.Integer, args(1))
              }
              val bit = args(2).asBool
              VCon(asConstant(writeBits(byteString, BuiltinList.from(indexes), bit)))
          ,
          builtinCostModel.writeBits
        )

    val ReplicateByte: BuiltinRuntime =
        mkMeaning(
          DefaultUni.Integer ->: DefaultUni.Integer ->: DefaultUni.ByteString,
          (_: Logger, args: Seq[CekValue]) =>
              val length = args(0).asInteger
              val byte = args(1).asInteger
              VCon(asConstant(replicateByte(length, byte)))
          ,
          builtinCostModel.replicateByte
        )

    val ShiftByteString: BuiltinRuntime =
        mkMeaning(
          DefaultUni.ByteString ->: DefaultUni.Integer ->: DefaultUni.ByteString,
          (_: Logger, args: Seq[CekValue]) =>
              val byteString = args(0).asByteString
              val shift = args(1).asInteger
              VCon(asConstant(shiftByteString(byteString, shift)))
          ,
          builtinCostModel.shiftByteString
        )

    val RotateByteString: BuiltinRuntime =
        mkMeaning(
          DefaultUni.ByteString ->: DefaultUni.Integer ->: DefaultUni.ByteString,
          (_: Logger, args: Seq[CekValue]) =>
              val byteString = args(0).asByteString
              val rotation = args(1).asInteger
              VCon(asConstant(rotateByteString(byteString, rotation)))
          ,
          builtinCostModel.rotateByteString
        )

    val CountSetBits: BuiltinRuntime =
        mkMeaning(
          DefaultUni.ByteString ->: DefaultUni.Integer,
          (_: Logger, args: Seq[CekValue]) =>
              val byteString = args(0).asByteString
              VCon(asConstant(countSetBits(byteString)))
          ,
          builtinCostModel.countSetBits
        )

    val FindFirstSetBit: BuiltinRuntime =
        mkMeaning(
          DefaultUni.ByteString ->: DefaultUni.Integer,
          (_: Logger, args: Seq[CekValue]) =>
              val byteString = args(0).asByteString
              VCon(asConstant(findFirstSetBit(byteString)))
          ,
          builtinCostModel.findFirstSetBit
        )

    val Ripemd_160: BuiltinRuntime =
        mkMeaning(
          DefaultUni.ByteString ->: DefaultUni.ByteString,
          (_: Logger, args: Seq[CekValue]) =>
              val byteString = args(0).asByteString
              VCon(asConstant(platformSpecific.ripemd_160(byteString)))
          ,
          builtinCostModel.ripemd_160
        )

    private inline def mkGetBuiltinRuntime: DefaultFun => BuiltinRuntime = ${
        Macros.mkGetBuiltinRuntime('this)
    }

    def getBuiltinRuntime(fun: DefaultFun): BuiltinRuntime = mkGetBuiltinRuntime(fun)

    lazy val BuiltinMeanings: immutable.Map[DefaultFun, BuiltinRuntime] = DefaultFun.values.map {
        fun => fun -> getBuiltinRuntime(fun)
    }.toMap

    /** A map of all UPLC builtins to their forced versions.
      *
      * In UPLC, built-in functions can have polymorphic types, which means they can operate on
      * different types of data, like:
      *
      * `ifThenElse : forall a. Boolean -> a -> a -> a`
      *
      * During erasure, type abstractions are replaced with `delay` and type applications with
      * `force`. So on each use of a polymorphic builtin, we need to `force` all its type arguments.
      *
      * This map provides the forced versions of all builtins.
      */
    lazy val forcedBuiltins: Map[DefaultFun, Term] =
        def forceBuiltin(scheme: TypeScheme, term: Term): Term = scheme match
            case TypeScheme.All(_, t) => Term.Force(forceBuiltin(t, term))
            case _                    => term

        BuiltinMeanings.map((bi, rt) => bi -> forceBuiltin(rt.typeScheme, Term.Builtin(bi)))
