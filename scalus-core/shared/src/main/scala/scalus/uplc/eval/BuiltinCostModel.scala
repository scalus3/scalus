package scalus.uplc.eval

import scalus.cardano.ledger.{CardanoInfo, Language}
import scalus.uplc.eval.MachineParams.fromProtocolParams
import scalus.uplc.{BuiltinSemanticsVariant, PlutusParams}
import scalus.utils.Macros
import upickle.default.*

case class BuiltinCostModel(
    // Integers
    addInteger: DefaultCostingFun[TwoArguments],
    subtractInteger: DefaultCostingFun[TwoArguments],
    multiplyInteger: DefaultCostingFun[TwoArguments],
    divideInteger: DefaultCostingFun[TwoArguments],
    quotientInteger: DefaultCostingFun[TwoArguments],
    remainderInteger: DefaultCostingFun[TwoArguments],
    modInteger: DefaultCostingFun[TwoArguments],
    equalsInteger: DefaultCostingFun[TwoArguments],
    lessThanInteger: DefaultCostingFun[TwoArguments],
    lessThanEqualsInteger: DefaultCostingFun[TwoArguments],
    // Bytestrings
    appendByteString: DefaultCostingFun[TwoArguments],
    consByteString: DefaultCostingFun[TwoArguments],
    sliceByteString: DefaultCostingFun[ThreeArguments],
    lengthOfByteString: ConstCostingFun,
    indexByteString: ConstCostingFun,
    equalsByteString: DefaultCostingFun[TwoArguments],
    lessThanByteString: DefaultCostingFun[TwoArguments],
    lessThanEqualsByteString: DefaultCostingFun[TwoArguments],
    // // Cryptography and hashes
    sha2_256: DefaultCostingFun[OneArgument],
    sha3_256: DefaultCostingFun[OneArgument],
    blake2b_256: DefaultCostingFun[OneArgument],
    verifyEd25519Signature: DefaultCostingFun[ThreeArguments],
    verifyEcdsaSecp256k1Signature: ConstCostingFun,
    verifySchnorrSecp256k1Signature: DefaultCostingFun[ThreeArguments],
    // Strings
    appendString: DefaultCostingFun[TwoArguments],
    equalsString: DefaultCostingFun[TwoArguments],
    encodeUtf8: DefaultCostingFun[OneArgument],
    decodeUtf8: DefaultCostingFun[OneArgument],
    // Bool
    // ifThenElse: DefaultCostingFun[ThreeArguments],
    ifThenElse: ConstCostingFun,
    // Unit
    chooseUnit: ConstCostingFun,
    // Tracing
    trace: ConstCostingFun,
    // Pairs
    fstPair: ConstCostingFun,
    sndPair: ConstCostingFun,
    // Lists
    chooseList: ConstCostingFun,
    mkCons: ConstCostingFun,
    headList: ConstCostingFun,
    tailList: ConstCostingFun,
    nullList: ConstCostingFun,
    // Data
    chooseData: ConstCostingFun,
    constrData: ConstCostingFun,
    mapData: ConstCostingFun,
    listData: ConstCostingFun,
    iData: ConstCostingFun,
    bData: ConstCostingFun,
    unConstrData: ConstCostingFun,
    unMapData: ConstCostingFun,
    unListData: ConstCostingFun,
    unIData: ConstCostingFun,
    unBData: ConstCostingFun,
    equalsData: DefaultCostingFun[TwoArguments],
    // Misc constructors
    mkPairData: ConstCostingFun,
    mkNilData: ConstCostingFun,
    mkNilPairData: ConstCostingFun,
    serialiseData: DefaultCostingFun[OneArgument],
    blake2b_224: DefaultCostingFun[OneArgument],
    keccak_256: DefaultCostingFun[OneArgument],
    // BLS
    bls12_381_G1_add: ConstCostingFun,
    bls12_381_G1_neg: ConstCostingFun,
    bls12_381_G1_scalarMul: DefaultCostingFun[TwoArguments],
    bls12_381_G1_equal: ConstCostingFun,
    bls12_381_G1_compress: ConstCostingFun,
    bls12_381_G1_uncompress: ConstCostingFun,
    bls12_381_G1_hashToGroup: DefaultCostingFun[TwoArguments],
    bls12_381_G2_add: ConstCostingFun,
    bls12_381_G2_neg: ConstCostingFun,
    bls12_381_G2_scalarMul: DefaultCostingFun[TwoArguments],
    bls12_381_G2_equal: ConstCostingFun,
    bls12_381_G2_compress: ConstCostingFun,
    bls12_381_G2_uncompress: ConstCostingFun,
    bls12_381_G2_hashToGroup: DefaultCostingFun[TwoArguments],
    bls12_381_millerLoop: ConstCostingFun,
    bls12_381_mulMlResult: ConstCostingFun,
    bls12_381_finalVerify: ConstCostingFun,
    integerToByteString: IntegerToByteStringCostingFun,
    byteStringToInteger: DefaultCostingFun[TwoArguments],
    andByteString: DefaultCostingFun[ThreeArguments],
    orByteString: DefaultCostingFun[ThreeArguments],
    xorByteString: DefaultCostingFun[ThreeArguments],
    complementByteString: DefaultCostingFun[OneArgument],
    readBit: ConstCostingFun,
    writeBits: WriteBitsCostingFun,
    replicateByte: ReplicateByteCostingFun,
    shiftByteString: ShiftOrRotateByteStringCostingFun,
    rotateByteString: ShiftOrRotateByteStringCostingFun,
    countSetBits: DefaultCostingFun[OneArgument],
    findFirstSetBit: DefaultCostingFun[OneArgument],
    ripemd_160: DefaultCostingFun[OneArgument],
    // Plutus 1.53 new builtins
    dropList: DropListCostingFun
) {

    /** Convert a [[BuiltinCostModel]] to a flat map of cost parameters
      *
      * @return
      *   a flat map of cost parameters, like Map("addInteger-cpu-arguments-intercept" -> 205665,
      *   ...)
      */
    def flattenCostModel: Map[String, Long] = {
        val obj = writeJs(this).obj
        // recursively flatten the object
        def flatten(obj: ujson.Obj, prefix: Option[String] = None): Map[String, Long] = {
            obj.value.flatMap {
                case (k, v: ujson.Obj) =>
                    prefix match
                        case Some(p) => flatten(v, Some(s"$p-$k"))
                        case None    => flatten(v, Some(k))
                case (k, v: ujson.Num) =>
                    prefix match
                        case Some(p) => Map(s"$p-$k" -> v.num.toLong)
                        case None    => Map(s"$k" -> v.num.toLong)
                case (k, v) => Map.empty
            }.toMap
        }
        flatten(obj)
    }

    def toJsonString: String = write(this)
}

object BuiltinCostModel {

    private inline def inlineBuiltinCostModelJson(name: String) = ${
        Macros.inlineResource('name)
    }
    // Deprecated. We recommend using the cost models from CardanoInfo instead.
    // These will stay for backward compatibility but may be removed in future versions.
    // We don't need previous versions of the cost models in the library itself as we don't support old versions of Cardano.
    // These vals increase the size of the JavaScript bundle significantly.
    @deprecated(
      "Use CardanoInfo.mainnet.protocolParams.builtinCostModel for current models or load builtinCostModelA.json in your code",
      "0.12.1"
    )
    lazy val defaultCostModelA: BuiltinCostModel =
        BuiltinCostModel.fromJsonString(inlineBuiltinCostModelJson("builtinCostModelA.json"))

    @deprecated(
      "Use CardanoInfo.mainnet.protocolParams.builtinCostModel for current models or load builtinCostModelB.json in your code",
      "0.12.1"
    )
    lazy val defaultCostModelB: BuiltinCostModel =
        BuiltinCostModel.fromJsonString(inlineBuiltinCostModelJson("builtinCostModelB.json"))

    @deprecated(
      "Use CardanoInfo.mainnet.protocolParams.builtinCostModel for current models or load builtinCostModelC.json in your code",
      "0.12.1"
    )
    lazy val defaultCostModelC: BuiltinCostModel =
        fromProtocolParams(CardanoInfo.mainnet.protocolParams, Language.PlutusV3).builtinCostModel

    given ReadWriter[BuiltinCostModel] = readwriter[ujson.Value].bimap(
      model =>
          ujson.Obj(
            "addInteger" -> writeJs(model.addInteger),
            "subtractInteger" -> writeJs(model.subtractInteger),
            "multiplyInteger" -> writeJs(model.multiplyInteger),
            "divideInteger" -> writeJs(model.divideInteger),
            "quotientInteger" -> writeJs(model.quotientInteger),
            "remainderInteger" -> writeJs(model.remainderInteger),
            "modInteger" -> writeJs(model.modInteger),
            "equalsInteger" -> writeJs(model.equalsInteger),
            "lessThanInteger" -> writeJs(model.lessThanInteger),
            "lessThanEqualsInteger" -> writeJs(model.lessThanEqualsInteger),
            "appendByteString" -> writeJs(model.appendByteString),
            "consByteString" -> writeJs(model.consByteString),
            "sliceByteString" -> writeJs(model.sliceByteString),
            "lengthOfByteString" -> writeJs(model.lengthOfByteString.toDefaultFun[OneArgument]),
            "indexByteString" -> writeJs(model.indexByteString.toDefaultFun[TwoArguments]),
            "equalsByteString" -> writeJs(model.equalsByteString),
            "lessThanByteString" -> writeJs(model.lessThanByteString),
            "lessThanEqualsByteString" -> writeJs(model.lessThanEqualsByteString),
            "sha2_256" -> writeJs(model.sha2_256),
            "sha3_256" -> writeJs(model.sha3_256),
            "blake2b_256" -> writeJs(model.blake2b_256),
            "verifyEd25519Signature" -> writeJs(model.verifyEd25519Signature),
            "verifyEcdsaSecp256k1Signature" -> writeJs(
              model.verifyEcdsaSecp256k1Signature.toDefaultFun[ThreeArguments]
            ),
            "verifySchnorrSecp256k1Signature" -> writeJs(model.verifySchnorrSecp256k1Signature),
            "appendString" -> writeJs(model.appendString),
            "equalsString" -> writeJs(model.equalsString),
            "encodeUtf8" -> writeJs(model.encodeUtf8),
            "decodeUtf8" -> writeJs(model.decodeUtf8),
            "ifThenElse" -> writeJs(model.ifThenElse.toDefaultFun[ThreeArguments]),
            "chooseUnit" -> writeJs(model.chooseUnit.toDefaultFun[TwoArguments]),
            "trace" -> writeJs(model.trace.toDefaultFun[TwoArguments]),
            "fstPair" -> writeJs(model.fstPair.toDefaultFun[OneArgument]),
            "sndPair" -> writeJs(model.sndPair.toDefaultFun[OneArgument]),
            "chooseList" -> writeJs(model.chooseList.toDefaultFun[ThreeArguments]),
            "mkCons" -> writeJs(model.mkCons.toDefaultFun[TwoArguments]),
            "headList" -> writeJs(model.headList.toDefaultFun[OneArgument]),
            "tailList" -> writeJs(model.tailList.toDefaultFun[OneArgument]),
            "nullList" -> writeJs(model.nullList.toDefaultFun[OneArgument]),
            "chooseData" -> writeJs(model.chooseData.toDefaultFun[SixArguments]),
            "constrData" -> writeJs(model.constrData.toDefaultFun[TwoArguments]),
            "mapData" -> writeJs(model.mapData.toDefaultFun[OneArgument]),
            "listData" -> writeJs(model.listData.toDefaultFun[OneArgument]),
            "iData" -> writeJs(model.iData.toDefaultFun[OneArgument]),
            "bData" -> writeJs(model.bData.toDefaultFun[OneArgument]),
            "unConstrData" -> writeJs(model.unConstrData.toDefaultFun[OneArgument]),
            "unMapData" -> writeJs(model.unMapData.toDefaultFun[OneArgument]),
            "unListData" -> writeJs(model.unListData.toDefaultFun[OneArgument]),
            "unIData" -> writeJs(model.unIData.toDefaultFun[OneArgument]),
            "unBData" -> writeJs(model.unBData.toDefaultFun[OneArgument]),
            "equalsData" -> writeJs(model.equalsData),
            "mkPairData" -> writeJs(model.mkPairData.toDefaultFun[TwoArguments]),
            "mkNilData" -> writeJs(model.mkNilData.toDefaultFun[OneArgument]),
            "mkNilPairData" -> writeJs(model.mkNilPairData.toDefaultFun[OneArgument]),
            "serialiseData" -> writeJs(model.serialiseData),
            "blake2b_224" -> writeJs(model.blake2b_224),
            "keccak_256" -> writeJs(model.keccak_256),
            "bls12_381_G1_add" -> writeJs(model.bls12_381_G1_add.toDefaultFun[TwoArguments]),
            "bls12_381_G1_neg" -> writeJs(model.bls12_381_G1_neg.toDefaultFun[OneArgument]),
            "bls12_381_G1_scalarMul" -> writeJs(model.bls12_381_G1_scalarMul),
            "bls12_381_G1_equal" -> writeJs(model.bls12_381_G1_equal.toDefaultFun[TwoArguments]),
            "bls12_381_G1_compress" -> writeJs(
              model.bls12_381_G1_compress.toDefaultFun[OneArgument]
            ),
            "bls12_381_G1_uncompress" -> writeJs(
              model.bls12_381_G1_uncompress.toDefaultFun[OneArgument]
            ),
            "bls12_381_G1_hashToGroup" -> writeJs(model.bls12_381_G1_hashToGroup),
            "bls12_381_G2_add" -> writeJs(model.bls12_381_G2_add.toDefaultFun[TwoArguments]),
            "bls12_381_G2_neg" -> writeJs(model.bls12_381_G2_neg.toDefaultFun[OneArgument]),
            "bls12_381_G2_scalarMul" -> writeJs(model.bls12_381_G2_scalarMul),
            "bls12_381_G2_equal" -> writeJs(model.bls12_381_G2_equal.toDefaultFun[TwoArguments]),
            "bls12_381_G2_compress" -> writeJs(
              model.bls12_381_G2_compress.toDefaultFun[OneArgument]
            ),
            "bls12_381_G2_uncompress" -> writeJs(
              model.bls12_381_G2_uncompress.toDefaultFun[OneArgument]
            ),
            "bls12_381_G2_hashToGroup" -> writeJs(model.bls12_381_G2_hashToGroup),
            "bls12_381_millerLoop" -> writeJs(
              model.bls12_381_millerLoop.toDefaultFun[TwoArguments]
            ),
            "bls12_381_mulMlResult" -> writeJs(
              model.bls12_381_mulMlResult.toDefaultFun[TwoArguments]
            ),
            "bls12_381_finalVerify" -> writeJs(
              model.bls12_381_finalVerify.toDefaultFun[TwoArguments]
            ),
            "integerToByteString" -> writeJs(model.integerToByteString),
            "byteStringToInteger" -> writeJs(model.byteStringToInteger),
            "andByteString" -> writeJs(model.andByteString),
            "orByteString" -> writeJs(model.orByteString),
            "xorByteString" -> writeJs(model.xorByteString),
            "complementByteString" -> writeJs(model.complementByteString),
            "readBit" -> writeJs(model.readBit.toDefaultFun[TwoArguments]),
            "writeBits" -> writeJs(model.writeBits),
            "replicateByte" -> writeJs(model.replicateByte),
            "shiftByteString" -> writeJs(model.shiftByteString),
            "rotateByteString" -> writeJs(model.rotateByteString),
            "countSetBits" -> writeJs(model.countSetBits),
            "findFirstSetBit" -> writeJs(model.findFirstSetBit),
            "ripemd_160" -> writeJs(model.ripemd_160),
            // Plutus 1.53 new builtins
            "dropList" -> writeJs(model.dropList)
          ),
      json =>
          BuiltinCostModel(
            addInteger = read[DefaultCostingFun[TwoArguments]](json("addInteger")),
            subtractInteger = read[DefaultCostingFun[TwoArguments]](json("subtractInteger")),
            multiplyInteger = read[DefaultCostingFun[TwoArguments]](json("multiplyInteger")),
            divideInteger = read[DefaultCostingFun[TwoArguments]](json("divideInteger")),
            quotientInteger = read[DefaultCostingFun[TwoArguments]](json("quotientInteger")),
            remainderInteger = read[DefaultCostingFun[TwoArguments]](json("remainderInteger")),
            modInteger = read[DefaultCostingFun[TwoArguments]](json("modInteger")),
            equalsInteger = read[DefaultCostingFun[TwoArguments]](json("equalsInteger")),
            lessThanInteger = read[DefaultCostingFun[TwoArguments]](json("lessThanInteger")),
            lessThanEqualsInteger =
                read[DefaultCostingFun[TwoArguments]](json("lessThanEqualsInteger")),
            appendByteString = read[DefaultCostingFun[TwoArguments]](json("appendByteString")),
            consByteString = read[DefaultCostingFun[TwoArguments]](json("consByteString")),
            sliceByteString = read[DefaultCostingFun[ThreeArguments]](json("sliceByteString")),
            lengthOfByteString = ConstCostingFun
                .readDefaultFun[OneArgument]("lengthOfByteString", json("lengthOfByteString")),
            indexByteString = ConstCostingFun
                .readDefaultFun[TwoArguments]("indexByteString", json("indexByteString")),
            equalsByteString = read[DefaultCostingFun[TwoArguments]](json("equalsByteString")),
            lessThanByteString = read[DefaultCostingFun[TwoArguments]](json("lessThanByteString")),
            lessThanEqualsByteString =
                read[DefaultCostingFun[TwoArguments]](json("lessThanEqualsByteString")),
            sha2_256 = read[DefaultCostingFun[OneArgument]](json("sha2_256")),
            sha3_256 = read[DefaultCostingFun[OneArgument]](json("sha3_256")),
            blake2b_256 = read[DefaultCostingFun[OneArgument]](json("blake2b_256")),
            verifyEd25519Signature =
                read[DefaultCostingFun[ThreeArguments]](json("verifyEd25519Signature")),
            verifyEcdsaSecp256k1Signature = ConstCostingFun.readDefaultFun[ThreeArguments](
              "verifyEcdsaSecp256k1Signature",
              json("verifyEcdsaSecp256k1Signature")
            ),
            verifySchnorrSecp256k1Signature =
                read[DefaultCostingFun[ThreeArguments]](json("verifySchnorrSecp256k1Signature")),
            appendString = read[DefaultCostingFun[TwoArguments]](json("appendString")),
            equalsString = read[DefaultCostingFun[TwoArguments]](json("equalsString")),
            encodeUtf8 = read[DefaultCostingFun[OneArgument]](json("encodeUtf8")),
            decodeUtf8 = read[DefaultCostingFun[OneArgument]](json("decodeUtf8")),
            ifThenElse = ConstCostingFun
                .fromDefaultFun(
                  read[DefaultCostingFun[TwoArguments]](json("ifThenElse"))
                )
                .getOrElse {
                    throw new IllegalArgumentException(
                      "ifThenElse must be a constant costing function"
                    )
                },
            chooseUnit =
                ConstCostingFun.readDefaultFun[TwoArguments]("chooseUnit", json("chooseUnit")),
            trace = ConstCostingFun.readDefaultFun[TwoArguments]("trace", json("trace")),
            fstPair = ConstCostingFun.readDefaultFun[OneArgument]("fstPair", json("fstPair")),
            sndPair = ConstCostingFun.readDefaultFun[OneArgument]("sndPair", json("sndPair")),
            chooseList =
                ConstCostingFun.readDefaultFun[ThreeArguments]("chooseList", json("chooseList")),
            mkCons = ConstCostingFun.readDefaultFun[TwoArguments]("mkCons", json("mkCons")),
            headList = ConstCostingFun.readDefaultFun[OneArgument]("headList", json("headList")),
            tailList = ConstCostingFun.readDefaultFun[OneArgument]("tailList", json("tailList")),
            nullList = ConstCostingFun.readDefaultFun[OneArgument]("nullList", json("nullList")),
            chooseData =
                ConstCostingFun.readDefaultFun[SixArguments]("chooseData", json("chooseData")),
            constrData =
                ConstCostingFun.readDefaultFun[TwoArguments]("constrData", json("constrData")),
            mapData = ConstCostingFun.readDefaultFun[OneArgument]("mapData", json("mapData")),
            listData = ConstCostingFun.readDefaultFun[OneArgument]("listData", json("listData")),
            iData = ConstCostingFun.readDefaultFun[OneArgument]("iData", json("iData")),
            bData = ConstCostingFun.readDefaultFun[OneArgument]("bData", json("bData")),
            unConstrData =
                ConstCostingFun.readDefaultFun[OneArgument]("unConstrData", json("unConstrData")),
            unMapData = ConstCostingFun.readDefaultFun[OneArgument]("unMapData", json("unMapData")),
            unListData =
                ConstCostingFun.readDefaultFun[OneArgument]("unListData", json("unListData")),
            unIData = ConstCostingFun.readDefaultFun[OneArgument]("unIData", json("unIData")),
            unBData = ConstCostingFun.readDefaultFun[OneArgument]("unBData", json("unBData")),
            equalsData = read[DefaultCostingFun[TwoArguments]](json("equalsData")),
            mkPairData =
                ConstCostingFun.readDefaultFun[TwoArguments]("mkPairData", json("mkPairData")),
            mkNilData = ConstCostingFun.readDefaultFun[OneArgument]("mkNilData", json("mkNilData")),
            mkNilPairData =
                ConstCostingFun.readDefaultFun[OneArgument]("mkNilPairData", json("mkNilPairData")),
            serialiseData = read[DefaultCostingFun[OneArgument]](json("serialiseData")),
            blake2b_224 = read[DefaultCostingFun[OneArgument]](json("blake2b_224")),
            keccak_256 = read[DefaultCostingFun[OneArgument]](json("keccak_256")),
            bls12_381_G1_add = ConstCostingFun
                .readDefaultFun[TwoArguments]("bls12_381_G1_add", json("bls12_381_G1_add")),
            bls12_381_G1_neg = ConstCostingFun
                .readDefaultFun[OneArgument]("bls12_381_G1_neg", json("bls12_381_G1_neg")),
            bls12_381_G1_scalarMul =
                read[DefaultCostingFun[TwoArguments]](json("bls12_381_G1_scalarMul")),
            bls12_381_G1_equal = ConstCostingFun
                .readDefaultFun[TwoArguments]("bls12_381_G1_equal", json("bls12_381_G1_equal")),
            bls12_381_G1_compress = ConstCostingFun.readDefaultFun[OneArgument](
              "bls12_381_G1_compress",
              json("bls12_381_G1_compress")
            ),
            bls12_381_G1_uncompress = ConstCostingFun.readDefaultFun[OneArgument](
              "bls12_381_G1_uncompress",
              json("bls12_381_G1_uncompress")
            ),
            bls12_381_G1_hashToGroup =
                read[DefaultCostingFun[TwoArguments]](json("bls12_381_G1_hashToGroup")),
            bls12_381_G2_add = ConstCostingFun
                .readDefaultFun[TwoArguments]("bls12_381_G2_add", json("bls12_381_G2_add")),
            bls12_381_G2_neg = ConstCostingFun
                .readDefaultFun[OneArgument]("bls12_381_G2_neg", json("bls12_381_G2_neg")),
            bls12_381_G2_scalarMul =
                read[DefaultCostingFun[TwoArguments]](json("bls12_381_G2_scalarMul")),
            bls12_381_G2_equal = ConstCostingFun
                .readDefaultFun[TwoArguments]("bls12_381_G2_equal", json("bls12_381_G2_equal")),
            bls12_381_G2_compress = ConstCostingFun.readDefaultFun[OneArgument](
              "bls12_381_G2_compress",
              json("bls12_381_G2_compress")
            ),
            bls12_381_G2_uncompress = ConstCostingFun.readDefaultFun[OneArgument](
              "bls12_381_G2_uncompress",
              json("bls12_381_G2_uncompress")
            ),
            bls12_381_G2_hashToGroup =
                read[DefaultCostingFun[TwoArguments]](json("bls12_381_G2_hashToGroup")),
            bls12_381_millerLoop = ConstCostingFun
                .readDefaultFun[TwoArguments]("bls12_381_millerLoop", json("bls12_381_millerLoop")),
            bls12_381_mulMlResult = ConstCostingFun.readDefaultFun[TwoArguments](
              "bls12_381_mulMlResult",
              json("bls12_381_mulMlResult")
            ),
            bls12_381_finalVerify = ConstCostingFun.readDefaultFun[TwoArguments](
              "bls12_381_finalVerify",
              json("bls12_381_finalVerify")
            ),
            integerToByteString = read[IntegerToByteStringCostingFun](json("integerToByteString")),
            byteStringToInteger =
                read[DefaultCostingFun[TwoArguments]](json("byteStringToInteger")),
            andByteString =
                if json.obj.keySet.contains("andByteString") then
                    read[DefaultCostingFun[ThreeArguments]](json("andByteString"))
                else null,
            orByteString =
                if json.obj.keySet.contains("orByteString") then
                    read[DefaultCostingFun[ThreeArguments]](json("orByteString"))
                else null,
            xorByteString =
                if json.obj.keySet.contains("xorByteString") then
                    read[DefaultCostingFun[ThreeArguments]](json("xorByteString"))
                else null,
            complementByteString =
                if json.obj.keySet.contains("complementByteString") then
                    read[DefaultCostingFun[OneArgument]](json("complementByteString"))
                else null,
            readBit =
                if json.obj.keySet.contains("readBit") then
                    ConstCostingFun.readDefaultFun[TwoArguments]("readBit", json("readBit"))
                else null,
            writeBits =
                if json.obj.keySet.contains("writeBits") then
                    read[WriteBitsCostingFun](json("writeBits"))
                else null,
            replicateByte =
                if json.obj.keySet.contains("replicateByte") then
                    read[ReplicateByteCostingFun](json("replicateByte"))
                else null,
            shiftByteString =
                if json.obj.keySet.contains("shiftByteString") then
                    read[ShiftOrRotateByteStringCostingFun](json("shiftByteString"))
                else null,
            rotateByteString =
                if json.obj.keySet.contains("rotateByteString") then
                    read[ShiftOrRotateByteStringCostingFun](json("rotateByteString"))
                else null,
            countSetBits =
                if json.obj.keySet.contains("countSetBits") then
                    read[DefaultCostingFun[OneArgument]](json("countSetBits"))
                else null,
            findFirstSetBit =
                if json.obj.keySet.contains("findFirstSetBit") then
                    read[DefaultCostingFun[OneArgument]](json("findFirstSetBit"))
                else null,
            ripemd_160 =
                if json.obj.keySet.contains("ripemd_160") then
                    read[DefaultCostingFun[OneArgument]](json("ripemd_160"))
                else null,
            // Plutus 1.53 new builtins
            dropList =
                if json.obj.keySet.contains("dropList") then
                    read[DropListCostingFun](json("dropList"))
                else null
          )
    )

    protected[eval] val defaultValue = 300_000_000L

    def fromPlutusParams(
        params: PlutusParams,
        language: Language,
        semvar: BuiltinSemanticsVariant,
    ): BuiltinCostModel = {
        // Local conversion from Long to CostingInteger for cleaner syntax
        import scala.language.implicitConversions
        given Conversion[Long, CostingInteger] = CostingInteger(_)

        val model = BuiltinCostModel(
          addInteger = DefaultCostingFun(
            cpu = TwoArguments.MaxSize(
              OneVariableLinearFunction(
                intercept = params.`addInteger-cpu-arguments-intercept`,
                slope = params.`addInteger-cpu-arguments-slope`
              )
            ),
            memory = TwoArguments.MaxSize(
              OneVariableLinearFunction(
                intercept = params.`addInteger-memory-arguments-intercept`,
                slope = params.`addInteger-memory-arguments-slope`
              )
            )
          ),
          subtractInteger = DefaultCostingFun(
            cpu = TwoArguments.MaxSize(
              OneVariableLinearFunction(
                intercept = params.`subtractInteger-cpu-arguments-intercept`,
                slope = params.`subtractInteger-cpu-arguments-slope`
              )
            ),
            memory = TwoArguments.MaxSize(
              OneVariableLinearFunction(
                intercept = params.`subtractInteger-memory-arguments-intercept`,
                slope = params.`subtractInteger-memory-arguments-slope`
              )
            )
          ),
          multiplyInteger = (language, semvar) match {
              case (Language.PlutusV1 | Language.PlutusV2, BuiltinSemanticsVariant.A) =>
                  DefaultCostingFun(
                    cpu = TwoArguments.AddedSizes(
                      OneVariableLinearFunction(
                        intercept = params.`multiplyInteger-cpu-arguments-intercept`,
                        slope = params.`multiplyInteger-cpu-arguments-slope`
                      )
                    ),
                    memory = TwoArguments.AddedSizes(
                      OneVariableLinearFunction(
                        intercept = params.`multiplyInteger-memory-arguments-intercept`,
                        slope = params.`multiplyInteger-memory-arguments-slope`
                      )
                    )
                  )
              case (Language.PlutusV1 | Language.PlutusV2, BuiltinSemanticsVariant.B) |
                  (Language.PlutusV3, BuiltinSemanticsVariant.C) =>
                  DefaultCostingFun(
                    cpu = TwoArguments.MultipliedSizes(
                      OneVariableLinearFunction(
                        intercept = params.`multiplyInteger-cpu-arguments-intercept`,
                        slope = params.`multiplyInteger-cpu-arguments-slope`
                      )
                    ),
                    memory = TwoArguments.AddedSizes(
                      OneVariableLinearFunction(
                        intercept = params.`multiplyInteger-memory-arguments-intercept`,
                        slope = params.`multiplyInteger-memory-arguments-slope`
                      )
                    )
                  )
              case _ =>
                  throw new IllegalArgumentException(
                    s"Unsupported combination of Plutus version $language and semantics variant $semvar for multiplyInteger"
                  )
          },
          divideInteger = language match
              case Language.PlutusV1 | Language.PlutusV2 =>
                  DefaultCostingFun(
                    cpu = TwoArguments.ConstAboveDiagonal(
                      ConstantOrTwoArguments(
                        constant = params.`divideInteger-cpu-arguments-constant`,
                        model = TwoArguments.MultipliedSizes(
                          OneVariableLinearFunction(
                            intercept =
                                params.`divideInteger-cpu-arguments-model-arguments-intercept`,
                            slope = params.`divideInteger-cpu-arguments-model-arguments-slope`
                          )
                        )
                      )
                    ),
                    memory = TwoArguments.SubtractedSizes(
                      SubtractedSizesLinearFunction(
                        intercept = params.`divideInteger-memory-arguments-intercept`,
                        slope = params.`divideInteger-memory-arguments-slope`,
                        minimum = params.`divideInteger-memory-arguments-minimum`
                      )
                    )
                  )
              case Language.PlutusV3 =>
                  DefaultCostingFun(
                    cpu = TwoArguments.ConstAboveDiagonal(
                      ConstantOrTwoArguments(
                        constant = params.`divideInteger-cpu-arguments-constant`,
                        model = TwoArguments.QuadraticInXAndY(
                          TwoVariableQuadraticFunction(
                            minimum = params.`divideInteger-cpu-arguments-minimum`,
                            c00 = params.`divideInteger-cpu-arguments-c00`,
                            c10 = params.`divideInteger-cpu-arguments-c10`,
                            c01 = params.`divideInteger-cpu-arguments-c01`,
                            c20 = params.`divideInteger-cpu-arguments-c20`,
                            c11 = params.`divideInteger-cpu-arguments-c11`,
                            c02 = params.`divideInteger-cpu-arguments-c02`
                          )
                        )
                      )
                    ),
                    memory = TwoArguments.SubtractedSizes(
                      SubtractedSizesLinearFunction(
                        intercept = params.`divideInteger-memory-arguments-intercept`,
                        slope = params.`divideInteger-memory-arguments-slope`,
                        minimum = params.`divideInteger-memory-arguments-minimum`
                      )
                    )
                  )
          ,
          quotientInteger = language match
              case Language.PlutusV1 | Language.PlutusV2 =>
                  DefaultCostingFun(
                    cpu = TwoArguments.ConstAboveDiagonal(
                      ConstantOrTwoArguments(
                        constant = params.`quotientInteger-cpu-arguments-constant`,
                        model = TwoArguments.MultipliedSizes(
                          OneVariableLinearFunction(
                            intercept =
                                params.`quotientInteger-cpu-arguments-model-arguments-intercept`,
                            slope = params.`quotientInteger-cpu-arguments-model-arguments-slope`
                          )
                        )
                      )
                    ),
                    memory = TwoArguments.SubtractedSizes(
                      SubtractedSizesLinearFunction(
                        intercept = params.`quotientInteger-memory-arguments-intercept`,
                        slope = params.`quotientInteger-memory-arguments-slope`,
                        minimum = params.`quotientInteger-memory-arguments-minimum`
                      )
                    )
                  )
              case Language.PlutusV3 =>
                  DefaultCostingFun(
                    cpu = TwoArguments.ConstAboveDiagonal(
                      ConstantOrTwoArguments(
                        constant = params.`quotientInteger-cpu-arguments-constant`,
                        model = TwoArguments.QuadraticInXAndY(
                          TwoVariableQuadraticFunction(
                            minimum =
                                params.`quotientInteger-cpu-arguments-model-arguments-minimum`,
                            c00 = params.`quotientInteger-cpu-arguments-model-arguments-c00`,
                            c10 = params.`quotientInteger-cpu-arguments-model-arguments-c10`,
                            c01 = params.`quotientInteger-cpu-arguments-model-arguments-c01`,
                            c20 = params.`quotientInteger-cpu-arguments-model-arguments-c20`,
                            c11 = params.`quotientInteger-cpu-arguments-model-arguments-c11`,
                            c02 = params.`quotientInteger-cpu-arguments-model-arguments-c02`
                          )
                        )
                      )
                    ),
                    memory = TwoArguments.SubtractedSizes(
                      SubtractedSizesLinearFunction(
                        intercept = params.`quotientInteger-memory-arguments-intercept`,
                        slope = params.`quotientInteger-memory-arguments-slope`,
                        minimum = params.`quotientInteger-memory-arguments-minimum`
                      )
                    )
                  )
          ,
          remainderInteger = language match
              case Language.PlutusV1 | Language.PlutusV2 =>
                  DefaultCostingFun(
                    cpu = TwoArguments.ConstAboveDiagonal(
                      ConstantOrTwoArguments(
                        constant = params.`remainderInteger-cpu-arguments-constant`,
                        model = TwoArguments.MultipliedSizes(
                          OneVariableLinearFunction(
                            intercept =
                                params.`remainderInteger-cpu-arguments-model-arguments-intercept`,
                            slope = params.`remainderInteger-cpu-arguments-model-arguments-slope`
                          )
                        )
                      )
                    ),
                    memory = TwoArguments.SubtractedSizes(
                      SubtractedSizesLinearFunction(
                        intercept = params.`remainderInteger-memory-arguments-intercept`,
                        slope = params.`remainderInteger-memory-arguments-slope`,
                        minimum = params.`remainderInteger-memory-arguments-minimum`
                      )
                    )
                  )
              case Language.PlutusV3 =>
                  // same as modInteger
                  DefaultCostingFun(
                    cpu = TwoArguments.ConstAboveDiagonal(
                      ConstantOrTwoArguments(
                        constant = params.`remainderInteger-cpu-arguments-constant`,
                        model = TwoArguments.QuadraticInXAndY(
                          TwoVariableQuadraticFunction(
                            minimum =
                                params.`remainderInteger-cpu-arguments-model-arguments-minimum`,
                            c00 = params.`remainderInteger-cpu-arguments-model-arguments-c00`,
                            c10 = params.`remainderInteger-cpu-arguments-model-arguments-c10`,
                            c01 = params.`remainderInteger-cpu-arguments-model-arguments-c01`,
                            c20 = params.`remainderInteger-cpu-arguments-model-arguments-c20`,
                            c11 = params.`remainderInteger-cpu-arguments-model-arguments-c11`,
                            c02 = params.`remainderInteger-cpu-arguments-model-arguments-c02`
                          )
                        )
                      )
                    ),
                    memory = TwoArguments.LinearInY(
                      OneVariableLinearFunction(
                        intercept = params.`remainderInteger-memory-arguments-intercept`,
                        slope = params.`remainderInteger-memory-arguments-slope`
                      )
                    )
                  )
          ,
          modInteger = language match
              case Language.PlutusV1 | Language.PlutusV2 =>
                  DefaultCostingFun(
                    cpu = TwoArguments.ConstAboveDiagonal(
                      ConstantOrTwoArguments(
                        constant = params.`modInteger-cpu-arguments-constant`,
                        model = TwoArguments.MultipliedSizes(
                          OneVariableLinearFunction(
                            intercept = params.`modInteger-cpu-arguments-model-arguments-intercept`,
                            slope = params.`modInteger-cpu-arguments-model-arguments-slope`
                          )
                        )
                      )
                    ),
                    memory = TwoArguments.SubtractedSizes(
                      SubtractedSizesLinearFunction(
                        intercept = params.`modInteger-memory-arguments-intercept`,
                        slope = params.`modInteger-memory-arguments-slope`,
                        minimum = params.`modInteger-memory-arguments-minimum`
                      )
                    )
                  )
              case Language.PlutusV3 =>
                  DefaultCostingFun(
                    cpu = TwoArguments.ConstAboveDiagonal(
                      ConstantOrTwoArguments(
                        constant = params.`modInteger-cpu-arguments-constant`,
                        model = TwoArguments.QuadraticInXAndY(
                          TwoVariableQuadraticFunction(
                            minimum = params.`modInteger-cpu-arguments-model-arguments-minimum`,
                            c00 = params.`modInteger-cpu-arguments-model-arguments-c00`,
                            c10 = params.`modInteger-cpu-arguments-model-arguments-c10`,
                            c01 = params.`modInteger-cpu-arguments-model-arguments-c01`,
                            c20 = params.`modInteger-cpu-arguments-model-arguments-c20`,
                            c11 = params.`modInteger-cpu-arguments-model-arguments-c11`,
                            c02 = params.`modInteger-cpu-arguments-model-arguments-c02`
                          )
                        )
                      )
                    ),
                    memory = TwoArguments.LinearInY(
                      OneVariableLinearFunction(
                        intercept = params.`modInteger-memory-arguments-intercept`,
                        slope = params.`modInteger-memory-arguments-slope`
                      )
                    )
                  )
          ,
          equalsInteger = DefaultCostingFun(
            cpu = TwoArguments.MinSize(
              OneVariableLinearFunction(
                intercept = params.`equalsInteger-cpu-arguments-intercept`,
                slope = params.`equalsInteger-cpu-arguments-slope`
              )
            ),
            memory = TwoArguments.ConstantCost(
              cost = params.`equalsInteger-memory-arguments`
            )
          ),
          lessThanInteger = DefaultCostingFun(
            cpu = TwoArguments.MinSize(
              OneVariableLinearFunction(
                intercept = params.`lessThanInteger-cpu-arguments-intercept`,
                slope = params.`lessThanInteger-cpu-arguments-slope`
              )
            ),
            memory = TwoArguments.ConstantCost(
              cost = params.`lessThanInteger-memory-arguments`
            )
          ),
          lessThanEqualsInteger = DefaultCostingFun(
            cpu = TwoArguments.MinSize(
              OneVariableLinearFunction(
                intercept = params.`lessThanEqualsInteger-cpu-arguments-intercept`,
                slope = params.`lessThanEqualsInteger-cpu-arguments-slope`
              )
            ),
            memory = TwoArguments.ConstantCost(
              cost = params.`lessThanEqualsInteger-memory-arguments`
            )
          ),
          appendByteString = DefaultCostingFun(
            cpu = TwoArguments.AddedSizes(
              OneVariableLinearFunction(
                intercept = params.`appendByteString-cpu-arguments-intercept`,
                slope = params.`appendByteString-cpu-arguments-slope`
              )
            ),
            memory = TwoArguments.AddedSizes(
              OneVariableLinearFunction(
                intercept = params.`appendByteString-memory-arguments-intercept`,
                slope = params.`appendByteString-memory-arguments-slope`
              )
            )
          ),
          consByteString = DefaultCostingFun(
            cpu = TwoArguments.LinearInY(
              OneVariableLinearFunction(
                intercept = params.`consByteString-cpu-arguments-intercept`,
                slope = params.`consByteString-cpu-arguments-slope`
              )
            ),
            memory = TwoArguments.AddedSizes(
              OneVariableLinearFunction(
                intercept = params.`consByteString-memory-arguments-intercept`,
                slope = params.`consByteString-memory-arguments-slope`
              )
            )
          ),
          sliceByteString = DefaultCostingFun(
            cpu = ThreeArguments.LinearInZ(
              OneVariableLinearFunction(
                intercept = params.`sliceByteString-cpu-arguments-intercept`,
                slope = params.`sliceByteString-cpu-arguments-slope`
              )
            ),
            memory = ThreeArguments.LinearInZ(
              OneVariableLinearFunction(
                intercept = params.`sliceByteString-memory-arguments-intercept`,
                slope = params.`sliceByteString-memory-arguments-slope`
              )
            )
          ),
          lengthOfByteString = ConstCostingFun(
            cpu = params.`lengthOfByteString-cpu-arguments`,
            memory = params.`lengthOfByteString-memory-arguments`
          ),
          indexByteString = ConstCostingFun(
            cpu = params.`indexByteString-cpu-arguments`,
            memory = params.`indexByteString-memory-arguments`
          ),
          equalsByteString = DefaultCostingFun(
            cpu = TwoArguments.LinearOnDiagonal(
              ConstantOrLinear(
                constant = params.`equalsByteString-cpu-arguments-constant`,
                intercept = params.`equalsByteString-cpu-arguments-intercept`,
                slope = params.`equalsByteString-cpu-arguments-slope`
              )
            ),
            memory = TwoArguments.ConstantCost(cost = params.`equalsByteString-memory-arguments`)
          ),
          lessThanByteString = DefaultCostingFun(
            cpu = TwoArguments.MinSize(
              OneVariableLinearFunction(
                intercept = params.`lessThanByteString-cpu-arguments-intercept`,
                slope = params.`lessThanByteString-cpu-arguments-slope`
              )
            ),
            memory = TwoArguments.ConstantCost(cost = params.`lessThanByteString-memory-arguments`)
          ),
          lessThanEqualsByteString = DefaultCostingFun(
            cpu = TwoArguments.MinSize(
              OneVariableLinearFunction(
                intercept = params.`lessThanEqualsByteString-cpu-arguments-intercept`,
                slope = params.`lessThanEqualsByteString-cpu-arguments-slope`
              )
            ),
            memory = TwoArguments.ConstantCost(
              cost = params.`lessThanEqualsByteString-memory-arguments`
            )
          ),
          sha2_256 = DefaultCostingFun(
            cpu = OneArgument.LinearInX(
              OneVariableLinearFunction(
                intercept = params.`sha2_256-cpu-arguments-intercept`,
                slope = params.`sha2_256-cpu-arguments-slope`
              )
            ),
            memory = OneArgument.ConstantCost(cost = params.`sha2_256-memory-arguments`)
          ),
          sha3_256 = DefaultCostingFun(
            cpu = OneArgument.LinearInX(
              OneVariableLinearFunction(
                intercept = params.`sha3_256-cpu-arguments-intercept`,
                slope = params.`sha3_256-cpu-arguments-slope`
              )
            ),
            memory = OneArgument.ConstantCost(cost = params.`sha3_256-memory-arguments`)
          ),
          blake2b_256 = DefaultCostingFun(
            cpu = OneArgument.LinearInX(
              OneVariableLinearFunction(
                intercept = params.`blake2b_256-cpu-arguments-intercept`,
                slope = params.`blake2b_256-cpu-arguments-slope`
              )
            ),
            memory = OneArgument.ConstantCost(
              cost = params.`blake2b_256-memory-arguments`
            )
          ),
          verifyEd25519Signature = DefaultCostingFun(
            cpu = ThreeArguments.LinearInY(
              OneVariableLinearFunction(
                intercept = params.`verifyEd25519Signature-cpu-arguments-intercept`,
                slope = params.`verifyEd25519Signature-cpu-arguments-slope`
              )
            ),
            memory = ThreeArguments.ConstantCost(
              cost = params.`verifyEd25519Signature-memory-arguments`
            )
          ),
          verifyEcdsaSecp256k1Signature = ConstCostingFun(
            cpu = params.`verifyEcdsaSecp256k1Signature-cpu-arguments`,
            memory = params.`verifyEcdsaSecp256k1Signature-memory-arguments`
          ),
          verifySchnorrSecp256k1Signature = DefaultCostingFun(
            cpu = ThreeArguments.LinearInY(
              OneVariableLinearFunction(
                intercept = params.`verifySchnorrSecp256k1Signature-cpu-arguments-intercept`,
                slope = params.`verifySchnorrSecp256k1Signature-cpu-arguments-slope`
              )
            ),
            memory = ThreeArguments.ConstantCost(
              cost = params.`verifySchnorrSecp256k1Signature-memory-arguments`
            )
          ),
          appendString = DefaultCostingFun(
            cpu = TwoArguments.AddedSizes(
              OneVariableLinearFunction(
                intercept = params.`appendString-cpu-arguments-intercept`,
                slope = params.`appendString-cpu-arguments-slope`
              )
            ),
            memory = TwoArguments.AddedSizes(
              OneVariableLinearFunction(
                intercept = params.`appendString-memory-arguments-intercept`,
                slope = params.`appendString-memory-arguments-slope`
              )
            )
          ),
          equalsString = DefaultCostingFun(
            cpu = TwoArguments.LinearOnDiagonal(
              ConstantOrLinear(
                constant = params.`equalsString-cpu-arguments-constant`,
                intercept = params.`equalsString-cpu-arguments-intercept`,
                slope = params.`equalsString-cpu-arguments-slope`
              )
            ),
            memory = TwoArguments.ConstantCost(
              cost = params.`equalsString-memory-arguments`
            )
          ),
          encodeUtf8 = DefaultCostingFun(
            cpu = OneArgument.LinearInX(
              OneVariableLinearFunction(
                intercept = params.`encodeUtf8-cpu-arguments-intercept`,
                slope = params.`encodeUtf8-cpu-arguments-slope`
              )
            ),
            memory = OneArgument.LinearInX(
              OneVariableLinearFunction(
                intercept = params.`encodeUtf8-memory-arguments-intercept`,
                slope = params.`encodeUtf8-memory-arguments-slope`
              )
            )
          ),
          decodeUtf8 = DefaultCostingFun(
            cpu = OneArgument.LinearInX(
              OneVariableLinearFunction(
                intercept = params.`decodeUtf8-cpu-arguments-intercept`,
                slope = params.`decodeUtf8-cpu-arguments-slope`
              )
            ),
            memory = OneArgument.LinearInX(
              OneVariableLinearFunction(
                intercept = params.`decodeUtf8-memory-arguments-intercept`,
                slope = params.`decodeUtf8-memory-arguments-slope`
              )
            )
          ),
          ifThenElse = ConstCostingFun(
            cpu = params.`ifThenElse-cpu-arguments`,
            memory = params.`ifThenElse-memory-arguments`
          ),
          chooseUnit = ConstCostingFun(
            cpu = params.`chooseUnit-cpu-arguments`,
            memory = params.`chooseUnit-memory-arguments`
          ),
          trace = ConstCostingFun(
            cpu = params.`trace-cpu-arguments`,
            memory = params.`trace-memory-arguments`
          ),
          fstPair = ConstCostingFun(
            cpu = params.`fstPair-cpu-arguments`,
            memory = params.`fstPair-memory-arguments`
          ),
          sndPair = ConstCostingFun(
            cpu = params.`sndPair-cpu-arguments`,
            memory = params.`sndPair-memory-arguments`
          ),
          chooseList = ConstCostingFun(
            cpu = params.`chooseList-cpu-arguments`,
            memory = params.`chooseList-memory-arguments`
          ),
          mkCons = ConstCostingFun(
            cpu = params.`mkCons-cpu-arguments`,
            memory = params.`mkCons-memory-arguments`
          ),
          headList = ConstCostingFun(
            cpu = params.`headList-cpu-arguments`,
            memory = params.`headList-memory-arguments`
          ),
          tailList = ConstCostingFun(
            cpu = params.`tailList-cpu-arguments`,
            memory = params.`tailList-memory-arguments`
          ),
          nullList = ConstCostingFun(
            cpu = params.`nullList-cpu-arguments`,
            memory = params.`nullList-memory-arguments`
          ),
          chooseData = ConstCostingFun(
            cpu = params.`chooseData-cpu-arguments`,
            memory = params.`chooseData-memory-arguments`
          ),
          constrData = ConstCostingFun(
            cpu = params.`constrData-cpu-arguments`,
            memory = params.`constrData-memory-arguments`
          ),
          mapData = ConstCostingFun(
            cpu = params.`mapData-cpu-arguments`,
            memory = params.`mapData-memory-arguments`
          ),
          listData = ConstCostingFun(
            cpu = params.`listData-cpu-arguments`,
            memory = params.`listData-memory-arguments`
          ),
          iData = ConstCostingFun(
            cpu = params.`iData-cpu-arguments`,
            memory = params.`iData-memory-arguments`
          ),
          bData = ConstCostingFun(
            cpu = params.`bData-cpu-arguments`,
            memory = params.`bData-memory-arguments`
          ),
          unConstrData = ConstCostingFun(
            cpu = params.`unConstrData-cpu-arguments`,
            memory = params.`unConstrData-memory-arguments`
          ),
          unMapData = ConstCostingFun(
            cpu = params.`unMapData-cpu-arguments`,
            memory = params.`unMapData-memory-arguments`
          ),
          unListData = ConstCostingFun(
            cpu = params.`unListData-cpu-arguments`,
            memory = params.`unListData-memory-arguments`
          ),
          unIData = ConstCostingFun(
            cpu = params.`unIData-cpu-arguments`,
            memory = params.`unIData-memory-arguments`
          ),
          unBData = ConstCostingFun(
            cpu = params.`unBData-cpu-arguments`,
            memory = params.`unBData-memory-arguments`
          ),
          equalsData = DefaultCostingFun(
            cpu = TwoArguments.MinSize(
              OneVariableLinearFunction(
                intercept = params.`equalsData-cpu-arguments-intercept`,
                slope = params.`equalsData-cpu-arguments-slope`
              )
            ),
            memory = TwoArguments.ConstantCost(
              cost = params.`equalsData-memory-arguments`
            )
          ),
          mkPairData = ConstCostingFun(
            cpu = params.`mkPairData-cpu-arguments`,
            memory = params.`mkPairData-memory-arguments`
          ),
          mkNilData = ConstCostingFun(
            cpu = params.`mkNilData-cpu-arguments`,
            memory = params.`mkNilData-memory-arguments`
          ),
          mkNilPairData = ConstCostingFun(
            cpu = params.`mkNilPairData-cpu-arguments`,
            memory = params.`mkNilPairData-memory-arguments`
          ),
          serialiseData = DefaultCostingFun(
            cpu = OneArgument.LinearInX(
              OneVariableLinearFunction(
                intercept = params.`serialiseData-cpu-arguments-intercept`,
                slope = params.`serialiseData-cpu-arguments-slope`
              )
            ),
            memory = OneArgument.LinearInX(
              OneVariableLinearFunction(
                intercept = params.`serialiseData-memory-arguments-intercept`,
                slope = params.`serialiseData-memory-arguments-slope`
              )
            )
          ),
          blake2b_224 = DefaultCostingFun(
            cpu = OneArgument.LinearInX(
              OneVariableLinearFunction(
                intercept = params.`blake2b_224-cpu-arguments-intercept`,
                slope = params.`blake2b_224-cpu-arguments-slope`
              )
            ),
            memory = OneArgument.ConstantCost(
              cost = params.`blake2b_224-memory-arguments`
            )
          ),
          keccak_256 = DefaultCostingFun(
            cpu = OneArgument.LinearInX(
              OneVariableLinearFunction(
                intercept = params.`keccak_256-cpu-arguments-intercept`,
                slope = params.`keccak_256-cpu-arguments-slope`
              )
            ),
            memory = OneArgument.ConstantCost(
              cost = params.`keccak_256-memory-arguments`
            )
          ),
          bls12_381_G1_add = ConstCostingFun(
            cpu = params.`bls12_381_G1_add-cpu-arguments`,
            memory = params.`bls12_381_G1_add-memory-arguments`
          ),
          bls12_381_G1_neg = ConstCostingFun(
            cpu = params.`bls12_381_G1_neg-cpu-arguments`,
            memory = params.`bls12_381_G1_neg-memory-arguments`
          ),
          bls12_381_G1_scalarMul = DefaultCostingFun(
            cpu = TwoArguments.LinearInX(
              OneVariableLinearFunction(
                intercept = params.`bls12_381_G1_scalarMul-cpu-arguments-intercept`,
                slope = params.`bls12_381_G1_scalarMul-cpu-arguments-slope`
              )
            ),
            memory = TwoArguments.ConstantCost(
              cost = params.`bls12_381_G1_scalarMul-memory-arguments`
            )
          ),
          bls12_381_G1_equal = ConstCostingFun(
            cpu = params.`bls12_381_G1_equal-cpu-arguments`,
            memory = params.`bls12_381_G1_equal-memory-arguments`
          ),
          bls12_381_G1_compress = ConstCostingFun(
            cpu = params.`bls12_381_G1_compress-cpu-arguments`,
            memory = params.`bls12_381_G1_compress-memory-arguments`
          ),
          bls12_381_G1_uncompress = ConstCostingFun(
            cpu = params.`bls12_381_G1_uncompress-cpu-arguments`,
            memory = params.`bls12_381_G1_uncompress-memory-arguments`
          ),
          bls12_381_G1_hashToGroup = DefaultCostingFun(
            cpu = TwoArguments.LinearInX(
              OneVariableLinearFunction(
                intercept = params.`bls12_381_G1_hashToGroup-cpu-arguments-intercept`,
                slope = params.`bls12_381_G1_hashToGroup-cpu-arguments-slope`
              )
            ),
            memory = TwoArguments.ConstantCost(
              cost = params.`bls12_381_G1_hashToGroup-memory-arguments`
            )
          ),
          bls12_381_G2_add = ConstCostingFun(
            cpu = params.`bls12_381_G2_add-cpu-arguments`,
            memory = params.`bls12_381_G2_add-memory-arguments`
          ),
          bls12_381_G2_neg = ConstCostingFun(
            cpu = params.`bls12_381_G2_neg-cpu-arguments`,
            memory = params.`bls12_381_G2_neg-memory-arguments`
          ),
          bls12_381_G2_scalarMul = DefaultCostingFun(
            cpu = TwoArguments.LinearInX(
              OneVariableLinearFunction(
                intercept = params.`bls12_381_G2_scalarMul-cpu-arguments-intercept`,
                slope = params.`bls12_381_G2_scalarMul-cpu-arguments-slope`
              )
            ),
            memory = TwoArguments.ConstantCost(
              cost = params.`bls12_381_G2_scalarMul-memory-arguments`
            )
          ),
          bls12_381_G2_equal = ConstCostingFun(
            cpu = params.`bls12_381_G2_equal-cpu-arguments`,
            memory = params.`bls12_381_G2_equal-memory-arguments`
          ),
          bls12_381_G2_compress = ConstCostingFun(
            cpu = params.`bls12_381_G2_compress-cpu-arguments`,
            memory = params.`bls12_381_G2_compress-memory-arguments`
          ),
          bls12_381_G2_uncompress = ConstCostingFun(
            cpu = params.`bls12_381_G2_uncompress-cpu-arguments`,
            memory = params.`bls12_381_G2_uncompress-memory-arguments`
          ),
          bls12_381_G2_hashToGroup = DefaultCostingFun(
            cpu = TwoArguments.LinearInX(
              OneVariableLinearFunction(
                intercept = params.`bls12_381_G2_hashToGroup-cpu-arguments-intercept`,
                slope = params.`bls12_381_G2_hashToGroup-cpu-arguments-slope`
              )
            ),
            memory = TwoArguments.ConstantCost(
              cost = params.`bls12_381_G2_hashToGroup-memory-arguments`
            )
          ),
          bls12_381_millerLoop = ConstCostingFun(
            cpu = params.`bls12_381_millerLoop-cpu-arguments`,
            memory = params.`bls12_381_millerLoop-memory-arguments`
          ),
          bls12_381_mulMlResult = ConstCostingFun(
            cpu = params.`bls12_381_mulMlResult-cpu-arguments`,
            memory = params.`bls12_381_mulMlResult-memory-arguments`
          ),
          bls12_381_finalVerify = ConstCostingFun(
            cpu = params.`bls12_381_finalVerify-cpu-arguments`,
            memory = params.`bls12_381_finalVerify-memory-arguments`
          ),
          integerToByteString = IntegerToByteStringCostingFun(
            cpu = ThreeArguments.QuadraticInZ(
              OneVariableQuadraticFunction(
                c0 = params.`integerToByteString-cpu-arguments-c0`,
                c1 = params.`integerToByteString-cpu-arguments-c1`,
                c2 = params.`integerToByteString-cpu-arguments-c2`
              )
            ),
            memory = ThreeArguments.LiteralInYOrLinearInZ(
              OneVariableLinearFunction(
                intercept = params.`integerToByteString-memory-arguments-intercept`,
                slope = params.`integerToByteString-memory-arguments-slope`
              )
            )
          ),
          byteStringToInteger = DefaultCostingFun(
            cpu = TwoArguments.QuadraticInY(
              OneVariableQuadraticFunction(
                c0 = params.`byteStringToInteger-cpu-arguments-c0`,
                c1 = params.`byteStringToInteger-cpu-arguments-c1`,
                c2 = params.`byteStringToInteger-cpu-arguments-c2`
              )
            ),
            memory = TwoArguments.LinearInY(
              OneVariableLinearFunction(
                intercept = params.`byteStringToInteger-memory-arguments-intercept`,
                slope = params.`byteStringToInteger-memory-arguments-slope`
              )
            )
          ),
          andByteString = DefaultCostingFun(
            cpu = ThreeArguments.LinearInYAndZ(
              TwoVariableLinearFunction(
                intercept = params.`andByteString-cpu-arguments-intercept`,
                slope1 = params.`andByteString-cpu-arguments-slope1`,
                slope2 = params.`andByteString-cpu-arguments-slope2`
              )
            ),
            memory = ThreeArguments.LinearInMaxYZ(
              OneVariableLinearFunction(
                intercept = params.`andByteString-memory-arguments-intercept`,
                slope = params.`andByteString-memory-arguments-slope`
              )
            )
          ),
          orByteString = DefaultCostingFun(
            cpu = ThreeArguments.LinearInYAndZ(
              TwoVariableLinearFunction(
                intercept = params.`orByteString-cpu-arguments-intercept`,
                slope1 = params.`orByteString-cpu-arguments-slope1`,
                slope2 = params.`orByteString-cpu-arguments-slope2`
              )
            ),
            memory = ThreeArguments.LinearInMaxYZ(
              OneVariableLinearFunction(
                intercept = params.`orByteString-memory-arguments-intercept`,
                slope = params.`orByteString-memory-arguments-slope`
              )
            )
          ),
          xorByteString = DefaultCostingFun(
            cpu = ThreeArguments.LinearInYAndZ(
              TwoVariableLinearFunction(
                intercept = params.`xorByteString-cpu-arguments-intercept`,
                slope1 = params.`xorByteString-cpu-arguments-slope1`,
                slope2 = params.`xorByteString-cpu-arguments-slope2`
              )
            ),
            memory = ThreeArguments.LinearInMaxYZ(
              OneVariableLinearFunction(
                intercept = params.`xorByteString-memory-arguments-intercept`,
                slope = params.`xorByteString-memory-arguments-slope`
              )
            )
          ),
          complementByteString = DefaultCostingFun(
            cpu = OneArgument.LinearInX(
              OneVariableLinearFunction(
                intercept = params.`complementByteString-cpu-arguments-intercept`,
                slope = params.`complementByteString-cpu-arguments-slope`
              )
            ),
            memory = OneArgument.LinearInX(
              OneVariableLinearFunction(
                intercept = params.`complementByteString-memory-arguments-intercept`,
                slope = params.`complementByteString-memory-arguments-slope`
              )
            )
          ),
          readBit = ConstCostingFun(
            cpu = params.`readBit-cpu-arguments`,
            memory = params.`readBit-memory-arguments`
          ),
          writeBits = WriteBitsCostingFun(
            cpu = ThreeArguments.LinearInY(
              OneVariableLinearFunction(
                intercept = params.`writeBits-cpu-arguments-intercept`,
                slope = params.`writeBits-cpu-arguments-slope`
              )
            ),
            memory = ThreeArguments.LinearInX(
              OneVariableLinearFunction(
                intercept = params.`writeBits-memory-arguments-intercept`,
                slope = params.`writeBits-memory-arguments-slope`
              )
            )
          ),
          replicateByte = ReplicateByteCostingFun(
            cpu = TwoArguments.LinearInX(
              OneVariableLinearFunction(
                intercept = params.`replicateByte-cpu-arguments-intercept`,
                slope = params.`replicateByte-cpu-arguments-slope`
              )
            ),
            memory = TwoArguments.LinearInX(
              OneVariableLinearFunction(
                intercept = params.`replicateByte-memory-arguments-intercept`,
                slope = params.`replicateByte-memory-arguments-slope`
              )
            )
          ),
          shiftByteString = ShiftOrRotateByteStringCostingFun(
            cpu = TwoArguments.LinearInX(
              OneVariableLinearFunction(
                intercept = params.`shiftByteString-cpu-arguments-intercept`,
                slope = params.`shiftByteString-cpu-arguments-slope`
              )
            ),
            memory = TwoArguments.LinearInX(
              OneVariableLinearFunction(
                intercept = params.`shiftByteString-memory-arguments-intercept`,
                slope = params.`shiftByteString-memory-arguments-slope`
              )
            )
          ),
          rotateByteString = ShiftOrRotateByteStringCostingFun(
            cpu = TwoArguments.LinearInX(
              OneVariableLinearFunction(
                intercept = params.`rotateByteString-cpu-arguments-intercept`,
                slope = params.`rotateByteString-cpu-arguments-slope`
              )
            ),
            memory = TwoArguments.LinearInX(
              OneVariableLinearFunction(
                intercept = params.`rotateByteString-memory-arguments-intercept`,
                slope = params.`rotateByteString-memory-arguments-slope`
              )
            )
          ),
          countSetBits = DefaultCostingFun(
            cpu = OneArgument.LinearInX(
              OneVariableLinearFunction(
                intercept = params.`countSetBits-cpu-arguments-intercept`,
                slope = params.`countSetBits-cpu-arguments-slope`
              )
            ),
            memory = OneArgument.ConstantCost(
              cost = params.`countSetBits-memory-arguments`
            )
          ),
          findFirstSetBit = DefaultCostingFun(
            cpu = OneArgument.LinearInX(
              OneVariableLinearFunction(
                intercept = params.`findFirstSetBit-cpu-arguments-intercept`,
                slope = params.`findFirstSetBit-cpu-arguments-slope`
              )
            ),
            memory = OneArgument.ConstantCost(
              cost = params.`findFirstSetBit-memory-arguments`
            )
          ),
          ripemd_160 = DefaultCostingFun(
            cpu = OneArgument.LinearInX(
              OneVariableLinearFunction(
                intercept = params.`ripemd_160-cpu-arguments-intercept`,
                slope = params.`ripemd_160-cpu-arguments-slope`
              )
            ),
            memory = OneArgument.ConstantCost(
              cost = params.`ripemd_160-memory-arguments`
            )
          ),
          // Plutus 1.53 new builtins - default cost model until protocol params are updated
          // Values from builtinCostModelC.json (Plutus 1.53 reference implementation)
          dropList = DropListCostingFun(
            cpu = TwoArguments.LinearInX(OneVariableLinearFunction(
              intercept = CostingInteger(116711L),
              slope = CostingInteger(1957L)
            )),
            memory = TwoArguments.ConstantCost(CostingInteger(4L))
          )
        )

        model
    }

    /** Read a BuiltinCostModel from an input stream of JSON
      *
      * @param input
      *   the input stream
      * @return
      *   a BuiltinCostModel
      */
    def fromInputStream(input: java.io.InputStream): BuiltinCostModel = {
        read[BuiltinCostModel](input)
    }

    /** Read a BuiltinCostModel from a string of JSON
      * @param json
      *   the JSON string
      * @return
      *   a BuiltinCostModel
      */
    def fromJsonString(json: String): BuiltinCostModel = {
        read[BuiltinCostModel](json)
    }
}
