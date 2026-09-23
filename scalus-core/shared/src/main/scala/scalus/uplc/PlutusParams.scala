package scalus.uplc

import scalus.utils.Macros
import upickle.default.*

import java.lang.reflect.Modifier

trait PlutusParams {
    def `addInteger-cpu-arguments-intercept`: Long
    def `addInteger-cpu-arguments-slope`: Long
    def `addInteger-memory-arguments-intercept`: Long
    def `addInteger-memory-arguments-slope`: Long
    def `appendByteString-cpu-arguments-intercept`: Long
    def `appendByteString-cpu-arguments-slope`: Long
    def `appendByteString-memory-arguments-intercept`: Long
    def `appendByteString-memory-arguments-slope`: Long
    def `appendString-cpu-arguments-intercept`: Long
    def `appendString-cpu-arguments-slope`: Long
    def `appendString-memory-arguments-intercept`: Long
    def `appendString-memory-arguments-slope`: Long
    def `bData-cpu-arguments`: Long
    def `bData-memory-arguments`: Long
    def `blake2b_256-cpu-arguments-intercept`: Long
    def `blake2b_256-cpu-arguments-slope`: Long
    def `blake2b_256-memory-arguments`: Long
    def `cekApplyCost-exBudgetCPU`: Long
    def `cekApplyCost-exBudgetMemory`: Long
    def `cekBuiltinCost-exBudgetCPU`: Long
    def `cekBuiltinCost-exBudgetMemory`: Long
    def `cekConstCost-exBudgetCPU`: Long
    def `cekConstCost-exBudgetMemory`: Long
    def `cekDelayCost-exBudgetCPU`: Long
    def `cekDelayCost-exBudgetMemory`: Long
    def `cekForceCost-exBudgetCPU`: Long
    def `cekForceCost-exBudgetMemory`: Long
    def `cekLamCost-exBudgetCPU`: Long
    def `cekLamCost-exBudgetMemory`: Long
    def `cekStartupCost-exBudgetCPU`: Long
    def `cekStartupCost-exBudgetMemory`: Long
    def `cekVarCost-exBudgetCPU`: Long
    def `cekVarCost-exBudgetMemory`: Long
    def `chooseData-cpu-arguments`: Long
    def `chooseData-memory-arguments`: Long
    def `chooseList-cpu-arguments`: Long
    def `chooseList-memory-arguments`: Long
    def `chooseUnit-cpu-arguments`: Long
    def `chooseUnit-memory-arguments`: Long
    def `consByteString-cpu-arguments-intercept`: Long
    def `consByteString-cpu-arguments-slope`: Long
    def `consByteString-memory-arguments-intercept`: Long
    def `consByteString-memory-arguments-slope`: Long
    def `constrData-cpu-arguments`: Long
    def `constrData-memory-arguments`: Long
    def `decodeUtf8-cpu-arguments-intercept`: Long
    def `decodeUtf8-cpu-arguments-slope`: Long
    def `decodeUtf8-memory-arguments-intercept`: Long
    def `decodeUtf8-memory-arguments-slope`: Long
    def `divideInteger-cpu-arguments-model-arguments-intercept`: Long // V1/V2
    def `divideInteger-cpu-arguments-model-arguments-slope`: Long // V1/V2
    def `divideInteger-cpu-arguments-constant`: Long
    def `divideInteger-cpu-arguments-c00`: Long
    def `divideInteger-cpu-arguments-c01`: Long
    def `divideInteger-cpu-arguments-c02`: Long
    def `divideInteger-cpu-arguments-c10`: Long
    def `divideInteger-cpu-arguments-c11`: Long
    def `divideInteger-cpu-arguments-c20`: Long
    def `divideInteger-cpu-arguments-minimum`: Long
    def `divideInteger-memory-arguments-intercept`: Long
    def `divideInteger-memory-arguments-minimum`: Long
    def `divideInteger-memory-arguments-slope`: Long
    def `encodeUtf8-cpu-arguments-intercept`: Long
    def `encodeUtf8-cpu-arguments-slope`: Long
    def `encodeUtf8-memory-arguments-intercept`: Long
    def `encodeUtf8-memory-arguments-slope`: Long
    def `equalsByteString-cpu-arguments-constant`: Long
    def `equalsByteString-cpu-arguments-intercept`: Long
    def `equalsByteString-cpu-arguments-slope`: Long
    def `equalsByteString-memory-arguments`: Long
    def `equalsData-cpu-arguments-intercept`: Long
    def `equalsData-cpu-arguments-slope`: Long
    def `equalsData-memory-arguments`: Long
    def `equalsInteger-cpu-arguments-intercept`: Long
    def `equalsInteger-cpu-arguments-slope`: Long
    def `equalsInteger-memory-arguments`: Long
    def `equalsString-cpu-arguments-constant`: Long
    def `equalsString-cpu-arguments-intercept`: Long
    def `equalsString-cpu-arguments-slope`: Long
    def `equalsString-memory-arguments`: Long
    def `fstPair-cpu-arguments`: Long
    def `fstPair-memory-arguments`: Long
    def `headList-cpu-arguments`: Long
    def `headList-memory-arguments`: Long
    def `iData-cpu-arguments`: Long
    def `iData-memory-arguments`: Long
    def `ifThenElse-cpu-arguments`: Long
    def `ifThenElse-memory-arguments`: Long
    def `indexByteString-cpu-arguments`: Long
    def `indexByteString-memory-arguments`: Long
    def `lengthOfByteString-cpu-arguments`: Long
    def `lengthOfByteString-memory-arguments`: Long
    def `lessThanByteString-cpu-arguments-intercept`: Long
    def `lessThanByteString-cpu-arguments-slope`: Long
    def `lessThanByteString-memory-arguments`: Long
    def `lessThanEqualsByteString-cpu-arguments-intercept`: Long
    def `lessThanEqualsByteString-cpu-arguments-slope`: Long
    def `lessThanEqualsByteString-memory-arguments`: Long
    def `lessThanEqualsInteger-cpu-arguments-intercept`: Long
    def `lessThanEqualsInteger-cpu-arguments-slope`: Long
    def `lessThanEqualsInteger-memory-arguments`: Long
    def `lessThanInteger-cpu-arguments-intercept`: Long
    def `lessThanInteger-cpu-arguments-slope`: Long
    def `lessThanInteger-memory-arguments`: Long
    def `listData-cpu-arguments`: Long
    def `listData-memory-arguments`: Long
    def `mapData-cpu-arguments`: Long
    def `mapData-memory-arguments`: Long
    def `mkCons-cpu-arguments`: Long
    def `mkCons-memory-arguments`: Long
    def `mkNilData-cpu-arguments`: Long
    def `mkNilData-memory-arguments`: Long
    def `mkNilPairData-cpu-arguments`: Long
    def `mkNilPairData-memory-arguments`: Long
    def `mkPairData-cpu-arguments`: Long
    def `mkPairData-memory-arguments`: Long
    def `modInteger-cpu-arguments-constant`: Long
    def `modInteger-cpu-arguments-model-arguments-intercept`: Long // V1/V2
    def `modInteger-cpu-arguments-model-arguments-slope`: Long // V1/V2
    def `modInteger-cpu-arguments-model-arguments-c00`: Long // V3
    def `modInteger-cpu-arguments-model-arguments-c01`: Long
    def `modInteger-cpu-arguments-model-arguments-c02`: Long
    def `modInteger-cpu-arguments-model-arguments-c10`: Long
    def `modInteger-cpu-arguments-model-arguments-c11`: Long
    def `modInteger-cpu-arguments-model-arguments-c20`: Long
    def `modInteger-cpu-arguments-model-arguments-minimum`: Long
    def `modInteger-memory-arguments-minimum`: Long // V1/V2
    def `modInteger-memory-arguments-intercept`: Long
    def `modInteger-memory-arguments-slope`: Long
    def `multiplyInteger-cpu-arguments-intercept`: Long
    def `multiplyInteger-cpu-arguments-slope`: Long
    def `multiplyInteger-memory-arguments-intercept`: Long
    def `multiplyInteger-memory-arguments-slope`: Long
    def `nullList-cpu-arguments`: Long
    def `nullList-memory-arguments`: Long
    def `quotientInteger-cpu-arguments-model-arguments-intercept`: Long // V1/V2
    def `quotientInteger-cpu-arguments-model-arguments-slope`: Long // V1/V2
    def `quotientInteger-cpu-arguments-constant`: Long
    def `quotientInteger-cpu-arguments-model-arguments-c00`: Long
    def `quotientInteger-cpu-arguments-model-arguments-c01`: Long
    def `quotientInteger-cpu-arguments-model-arguments-c02`: Long
    def `quotientInteger-cpu-arguments-model-arguments-c10`: Long
    def `quotientInteger-cpu-arguments-model-arguments-c11`: Long
    def `quotientInteger-cpu-arguments-model-arguments-c20`: Long
    def `quotientInteger-cpu-arguments-model-arguments-minimum`: Long
    def `quotientInteger-memory-arguments-intercept`: Long
    def `quotientInteger-memory-arguments-minimum`: Long
    def `quotientInteger-memory-arguments-slope`: Long
    def `remainderInteger-cpu-arguments-model-arguments-intercept`: Long // V1/V2
    def `remainderInteger-cpu-arguments-model-arguments-slope`: Long // V1/V2
    def `remainderInteger-cpu-arguments-constant`: Long
    def `remainderInteger-cpu-arguments-model-arguments-c00`: Long
    def `remainderInteger-cpu-arguments-model-arguments-c01`: Long
    def `remainderInteger-cpu-arguments-model-arguments-c02`: Long
    def `remainderInteger-cpu-arguments-model-arguments-c10`: Long
    def `remainderInteger-cpu-arguments-model-arguments-c11`: Long
    def `remainderInteger-cpu-arguments-model-arguments-c20`: Long
    def `remainderInteger-cpu-arguments-model-arguments-minimum`: Long
    def `remainderInteger-memory-arguments-minimum`: Long // V1/V2
    def `remainderInteger-memory-arguments-intercept`: Long
    def `remainderInteger-memory-arguments-slope`: Long
    def `serialiseData-cpu-arguments-intercept`: Long
    def `serialiseData-cpu-arguments-slope`: Long
    def `serialiseData-memory-arguments-intercept`: Long
    def `serialiseData-memory-arguments-slope`: Long
    def `sha2_256-cpu-arguments-intercept`: Long
    def `sha2_256-cpu-arguments-slope`: Long
    def `sha2_256-memory-arguments`: Long
    def `sha3_256-cpu-arguments-intercept`: Long
    def `sha3_256-cpu-arguments-slope`: Long
    def `sha3_256-memory-arguments`: Long
    def `sliceByteString-cpu-arguments-intercept`: Long
    def `sliceByteString-cpu-arguments-slope`: Long
    def `sliceByteString-memory-arguments-intercept`: Long
    def `sliceByteString-memory-arguments-slope`: Long
    def `sndPair-cpu-arguments`: Long
    def `sndPair-memory-arguments`: Long
    def `subtractInteger-cpu-arguments-intercept`: Long
    def `subtractInteger-cpu-arguments-slope`: Long
    def `subtractInteger-memory-arguments-intercept`: Long
    def `subtractInteger-memory-arguments-slope`: Long
    def `tailList-cpu-arguments`: Long
    def `tailList-memory-arguments`: Long
    def `trace-cpu-arguments`: Long
    def `trace-memory-arguments`: Long
    def `unBData-cpu-arguments`: Long
    def `unBData-memory-arguments`: Long
    def `unConstrData-cpu-arguments`: Long
    def `unConstrData-memory-arguments`: Long
    def `unIData-cpu-arguments`: Long
    def `unIData-memory-arguments`: Long
    def `unListData-cpu-arguments`: Long
    def `unListData-memory-arguments`: Long
    def `unMapData-cpu-arguments`: Long
    def `unMapData-memory-arguments`: Long
    def `verifyEcdsaSecp256k1Signature-cpu-arguments`: Long
    def `verifyEcdsaSecp256k1Signature-memory-arguments`: Long
    def `verifyEd25519Signature-cpu-arguments-intercept`: Long
    def `verifyEd25519Signature-cpu-arguments-slope`: Long
    def `verifyEd25519Signature-memory-arguments`: Long
    def `verifySchnorrSecp256k1Signature-cpu-arguments-intercept`: Long
    def `verifySchnorrSecp256k1Signature-cpu-arguments-slope`: Long
    def `verifySchnorrSecp256k1Signature-memory-arguments`: Long
    def `cekConstrCost-exBudgetCPU`: Long
    def `cekConstrCost-exBudgetMemory`: Long
    def `cekCaseCost-exBudgetCPU`: Long
    def `cekCaseCost-exBudgetMemory`: Long
    def `bls12_381_G1_add-cpu-arguments`: Long
    def `bls12_381_G1_add-memory-arguments`: Long
    def `bls12_381_G1_compress-cpu-arguments`: Long
    def `bls12_381_G1_compress-memory-arguments`: Long
    def `bls12_381_G1_equal-cpu-arguments`: Long
    def `bls12_381_G1_equal-memory-arguments`: Long
    def `bls12_381_G1_hashToGroup-cpu-arguments-intercept`: Long
    def `bls12_381_G1_hashToGroup-cpu-arguments-slope`: Long
    def `bls12_381_G1_hashToGroup-memory-arguments`: Long
    def `bls12_381_G1_neg-cpu-arguments`: Long
    def `bls12_381_G1_neg-memory-arguments`: Long
    def `bls12_381_G1_scalarMul-cpu-arguments-intercept`: Long
    def `bls12_381_G1_scalarMul-cpu-arguments-slope`: Long
    def `bls12_381_G1_scalarMul-memory-arguments`: Long
    def `bls12_381_G1_uncompress-cpu-arguments`: Long
    def `bls12_381_G1_uncompress-memory-arguments`: Long
    def `bls12_381_G2_add-cpu-arguments`: Long
    def `bls12_381_G2_add-memory-arguments`: Long
    def `bls12_381_G2_compress-cpu-arguments`: Long
    def `bls12_381_G2_compress-memory-arguments`: Long
    def `bls12_381_G2_equal-cpu-arguments`: Long
    def `bls12_381_G2_equal-memory-arguments`: Long
    def `bls12_381_G2_hashToGroup-cpu-arguments-intercept`: Long
    def `bls12_381_G2_hashToGroup-cpu-arguments-slope`: Long
    def `bls12_381_G2_hashToGroup-memory-arguments`: Long
    def `bls12_381_G2_neg-cpu-arguments`: Long
    def `bls12_381_G2_neg-memory-arguments`: Long
    def `bls12_381_G2_scalarMul-cpu-arguments-intercept`: Long
    def `bls12_381_G2_scalarMul-cpu-arguments-slope`: Long
    def `bls12_381_G2_scalarMul-memory-arguments`: Long
    def `bls12_381_G2_uncompress-cpu-arguments`: Long
    def `bls12_381_G2_uncompress-memory-arguments`: Long
    def `bls12_381_finalVerify-cpu-arguments`: Long
    def `bls12_381_finalVerify-memory-arguments`: Long
    def `bls12_381_millerLoop-cpu-arguments`: Long
    def `bls12_381_millerLoop-memory-arguments`: Long
    def `bls12_381_mulMlResult-cpu-arguments`: Long
    def `bls12_381_mulMlResult-memory-arguments`: Long
    def `keccak_256-cpu-arguments-intercept`: Long
    def `keccak_256-cpu-arguments-slope`: Long
    def `keccak_256-memory-arguments`: Long
    def `blake2b_224-cpu-arguments-intercept`: Long
    def `blake2b_224-cpu-arguments-slope`: Long
    def `blake2b_224-memory-arguments`: Long
    def `integerToByteString-cpu-arguments-c0`: Long
    def `integerToByteString-cpu-arguments-c1`: Long
    def `integerToByteString-cpu-arguments-c2`: Long
    def `integerToByteString-memory-arguments-intercept`: Long
    def `integerToByteString-memory-arguments-slope`: Long
    def `byteStringToInteger-cpu-arguments-c0`: Long
    def `byteStringToInteger-cpu-arguments-c1`: Long
    def `byteStringToInteger-cpu-arguments-c2`: Long
    def `byteStringToInteger-memory-arguments-intercept`: Long
    def `byteStringToInteger-memory-arguments-slope`: Long
    def `andByteString-cpu-arguments-intercept`: Long
    def `andByteString-cpu-arguments-slope1`: Long
    def `andByteString-cpu-arguments-slope2`: Long
    def `andByteString-memory-arguments-intercept`: Long
    def `andByteString-memory-arguments-slope`: Long
    def `orByteString-cpu-arguments-intercept`: Long
    def `orByteString-cpu-arguments-slope1`: Long
    def `orByteString-cpu-arguments-slope2`: Long
    def `orByteString-memory-arguments-intercept`: Long
    def `orByteString-memory-arguments-slope`: Long
    def `xorByteString-cpu-arguments-intercept`: Long
    def `xorByteString-cpu-arguments-slope1`: Long
    def `xorByteString-cpu-arguments-slope2`: Long
    def `xorByteString-memory-arguments-intercept`: Long
    def `xorByteString-memory-arguments-slope`: Long
    def `complementByteString-cpu-arguments-intercept`: Long
    def `complementByteString-cpu-arguments-slope`: Long
    def `complementByteString-memory-arguments-intercept`: Long
    def `complementByteString-memory-arguments-slope`: Long
    def `readBit-cpu-arguments`: Long
    def `readBit-memory-arguments`: Long
    def `writeBits-cpu-arguments-intercept`: Long
    def `writeBits-cpu-arguments-slope`: Long
    def `writeBits-memory-arguments-intercept`: Long
    def `writeBits-memory-arguments-slope`: Long
    def `replicateByte-cpu-arguments-intercept`: Long
    def `replicateByte-cpu-arguments-slope`: Long
    def `replicateByte-memory-arguments-intercept`: Long
    def `replicateByte-memory-arguments-slope`: Long
    def `shiftByteString-cpu-arguments-intercept`: Long
    def `shiftByteString-cpu-arguments-slope`: Long
    def `shiftByteString-memory-arguments-intercept`: Long
    def `shiftByteString-memory-arguments-slope`: Long
    def `rotateByteString-cpu-arguments-intercept`: Long
    def `rotateByteString-cpu-arguments-slope`: Long
    def `rotateByteString-memory-arguments-intercept`: Long
    def `rotateByteString-memory-arguments-slope`: Long
    def `countSetBits-cpu-arguments-intercept`: Long
    def `countSetBits-cpu-arguments-slope`: Long
    def `countSetBits-memory-arguments`: Long
    def `findFirstSetBit-cpu-arguments-intercept`: Long
    def `findFirstSetBit-cpu-arguments-slope`: Long
    def `findFirstSetBit-memory-arguments`: Long
    def `ripemd_160-cpu-arguments-intercept`: Long
    def `ripemd_160-cpu-arguments-slope`: Long
    def `ripemd_160-memory-arguments`: Long
    def `expModInteger-cpu-arguments-coefficient00`: Long
    def `expModInteger-cpu-arguments-coefficient11`: Long
    def `expModInteger-cpu-arguments-coefficient12`: Long
    def `expModInteger-memory-arguments-intercept`: Long
    def `expModInteger-memory-arguments-slope`: Long
    def `dropList-cpu-arguments-intercept`: Long
    def `dropList-cpu-arguments-slope`: Long
    def `dropList-memory-arguments`: Long
    def `lengthOfArray-cpu-arguments`: Long
    def `lengthOfArray-memory-arguments`: Long
    def `listToArray-cpu-arguments-intercept`: Long
    def `listToArray-cpu-arguments-slope`: Long
    def `listToArray-memory-arguments-intercept`: Long
    def `listToArray-memory-arguments-slope`: Long
    def `indexArray-cpu-arguments`: Long
    def `indexArray-memory-arguments`: Long
    def `bls12_381_G1_multiScalarMul-cpu-arguments-intercept`: Long
    def `bls12_381_G1_multiScalarMul-cpu-arguments-slope`: Long
    def `bls12_381_G1_multiScalarMul-memory-arguments`: Long
    def `bls12_381_G2_multiScalarMul-cpu-arguments-intercept`: Long
    def `bls12_381_G2_multiScalarMul-cpu-arguments-slope`: Long
    def `bls12_381_G2_multiScalarMul-memory-arguments`: Long
    def `insertCoin-cpu-arguments-intercept`: Long
    def `insertCoin-cpu-arguments-slope`: Long
    def `insertCoin-memory-arguments-intercept`: Long
    def `insertCoin-memory-arguments-slope`: Long
    def `lookupCoin-cpu-arguments-intercept`: Long
    def `lookupCoin-cpu-arguments-slope`: Long
    def `lookupCoin-memory-arguments`: Long
    def `unionValue-cpu-arguments-c00`: Long
    def `unionValue-cpu-arguments-c10`: Long
    def `unionValue-cpu-arguments-c01`: Long
    def `unionValue-cpu-arguments-c11`: Long
    def `unionValue-memory-arguments-intercept`: Long
    def `unionValue-memory-arguments-slope`: Long
    def `valueContains-cpu-arguments-constant`: Long
    def `valueContains-cpu-arguments-model-arguments-intercept`: Long
    def `valueContains-cpu-arguments-model-arguments-slope1`: Long
    def `valueContains-cpu-arguments-model-arguments-slope2`: Long
    def `valueContains-memory-arguments`: Long
    def `valueData-cpu-arguments-intercept`: Long
    def `valueData-cpu-arguments-slope`: Long
    def `valueData-memory-arguments-intercept`: Long
    def `valueData-memory-arguments-slope`: Long
    def `unValueData-cpu-arguments-c0`: Long
    def `unValueData-cpu-arguments-c1`: Long
    def `unValueData-cpu-arguments-c2`: Long
    def `unValueData-memory-arguments-intercept`: Long
    def `unValueData-memory-arguments-slope`: Long
    def `scaleValue-cpu-arguments-intercept`: Long
    def `scaleValue-cpu-arguments-slope`: Long
    def `scaleValue-memory-arguments-intercept`: Long
    def `scaleValue-memory-arguments-slope`: Long

    def toJson: String
    def numberOfParams: Int =
        this.getClass.getDeclaredFields
            .count(field =>
                !Modifier.isFinal(field.getModifiers) && // excludes vals
                    !Modifier.isStatic(field.getModifiers) // excludes static/object fields
            )
}

/*
  Funny thing is that JVM has a limit of 255 parameters in a method if the args are Ints.
  If it's Long, then the limit is 127.
  And we can't generate a constructor call for `PlutusV1Params` or `PlutusV2Params`
  which has more than 127 parameters.
  So I'm using Ints here, and that should be enough for the protocol parameters.
  Then, I've changed the `PlutusV1Params` and `PlutusV2Params` to have Longs
  and be a class with public fields.
  I also added a `JsonUtils` object to generate a `ReadWriter` for these classes.
 */

/** Plutus V1 cost model parameters.
  *
  * The names of the fields are taken from
  * [[https://github.com/input-output-hk/plutus/blob/1.63.0.0/plutus-ledger-api/src/PlutusLedgerApi/V1/ParamName.hs]]
  * and Blockfrost Protocol Parameters JSON uses these names as well in
  * `blockfrost-params-epoch-645.json`
  *
  * But what's really important is the order of the fields because that's the order of the
  * parameters in the protocol parameters array.
  */
class PlutusV1Params extends PlutusParams {
    var `addInteger-cpu-arguments-intercept`: Long = Long.MaxValue
    var `addInteger-cpu-arguments-slope`: Long = Long.MaxValue
    var `addInteger-memory-arguments-intercept`: Long = Long.MaxValue
    var `addInteger-memory-arguments-slope`: Long = Long.MaxValue
    var `appendByteString-cpu-arguments-intercept`: Long = Long.MaxValue
    var `appendByteString-cpu-arguments-slope`: Long = Long.MaxValue
    var `appendByteString-memory-arguments-intercept`: Long = Long.MaxValue
    var `appendByteString-memory-arguments-slope`: Long = Long.MaxValue
    var `appendString-cpu-arguments-intercept`: Long = Long.MaxValue
    var `appendString-cpu-arguments-slope`: Long = Long.MaxValue
    var `appendString-memory-arguments-intercept`: Long = Long.MaxValue
    var `appendString-memory-arguments-slope`: Long = Long.MaxValue
    var `bData-cpu-arguments`: Long = Long.MaxValue
    var `bData-memory-arguments`: Long = Long.MaxValue
    var `blake2b_256-cpu-arguments-intercept`: Long = Long.MaxValue
    var `blake2b_256-cpu-arguments-slope`: Long = Long.MaxValue
    var `blake2b_256-memory-arguments`: Long = Long.MaxValue
    var `cekApplyCost-exBudgetCPU`: Long = Long.MaxValue
    var `cekApplyCost-exBudgetMemory`: Long = Long.MaxValue
    var `cekBuiltinCost-exBudgetCPU`: Long = Long.MaxValue
    var `cekBuiltinCost-exBudgetMemory`: Long = Long.MaxValue
    var `cekConstCost-exBudgetCPU`: Long = Long.MaxValue
    var `cekConstCost-exBudgetMemory`: Long = Long.MaxValue
    var `cekDelayCost-exBudgetCPU`: Long = Long.MaxValue
    var `cekDelayCost-exBudgetMemory`: Long = Long.MaxValue
    var `cekForceCost-exBudgetCPU`: Long = Long.MaxValue
    var `cekForceCost-exBudgetMemory`: Long = Long.MaxValue
    var `cekLamCost-exBudgetCPU`: Long = Long.MaxValue
    var `cekLamCost-exBudgetMemory`: Long = Long.MaxValue
    var `cekStartupCost-exBudgetCPU`: Long = Long.MaxValue
    var `cekStartupCost-exBudgetMemory`: Long = Long.MaxValue
    var `cekVarCost-exBudgetCPU`: Long = Long.MaxValue
    var `cekVarCost-exBudgetMemory`: Long = Long.MaxValue
    var `chooseData-cpu-arguments`: Long = Long.MaxValue
    var `chooseData-memory-arguments`: Long = Long.MaxValue
    var `chooseList-cpu-arguments`: Long = Long.MaxValue
    var `chooseList-memory-arguments`: Long = Long.MaxValue
    var `chooseUnit-cpu-arguments`: Long = Long.MaxValue
    var `chooseUnit-memory-arguments`: Long = Long.MaxValue
    var `consByteString-cpu-arguments-intercept`: Long = Long.MaxValue
    var `consByteString-cpu-arguments-slope`: Long = Long.MaxValue
    var `consByteString-memory-arguments-intercept`: Long = Long.MaxValue
    var `consByteString-memory-arguments-slope`: Long = Long.MaxValue
    var `constrData-cpu-arguments`: Long = Long.MaxValue
    var `constrData-memory-arguments`: Long = Long.MaxValue
    var `decodeUtf8-cpu-arguments-intercept`: Long = Long.MaxValue
    var `decodeUtf8-cpu-arguments-slope`: Long = Long.MaxValue
    var `decodeUtf8-memory-arguments-intercept`: Long = Long.MaxValue
    var `decodeUtf8-memory-arguments-slope`: Long = Long.MaxValue
    var `divideInteger-cpu-arguments-constant`: Long = Long.MaxValue
    var `divideInteger-cpu-arguments-model-arguments-intercept`: Long = Long.MaxValue
    var `divideInteger-cpu-arguments-model-arguments-slope`: Long = Long.MaxValue
    var `divideInteger-memory-arguments-intercept`: Long = Long.MaxValue
    var `divideInteger-memory-arguments-minimum`: Long = Long.MaxValue
    var `divideInteger-memory-arguments-slope`: Long = Long.MaxValue
    var `encodeUtf8-cpu-arguments-intercept`: Long = Long.MaxValue
    var `encodeUtf8-cpu-arguments-slope`: Long = Long.MaxValue
    var `encodeUtf8-memory-arguments-intercept`: Long = Long.MaxValue
    var `encodeUtf8-memory-arguments-slope`: Long = Long.MaxValue
    var `equalsByteString-cpu-arguments-constant`: Long = Long.MaxValue
    var `equalsByteString-cpu-arguments-intercept`: Long = Long.MaxValue
    var `equalsByteString-cpu-arguments-slope`: Long = Long.MaxValue
    var `equalsByteString-memory-arguments`: Long = Long.MaxValue
    var `equalsData-cpu-arguments-intercept`: Long = Long.MaxValue
    var `equalsData-cpu-arguments-slope`: Long = Long.MaxValue
    var `equalsData-memory-arguments`: Long = Long.MaxValue
    var `equalsInteger-cpu-arguments-intercept`: Long = Long.MaxValue
    var `equalsInteger-cpu-arguments-slope`: Long = Long.MaxValue
    var `equalsInteger-memory-arguments`: Long = Long.MaxValue
    var `equalsString-cpu-arguments-constant`: Long = Long.MaxValue
    var `equalsString-cpu-arguments-intercept`: Long = Long.MaxValue
    var `equalsString-cpu-arguments-slope`: Long = Long.MaxValue
    var `equalsString-memory-arguments`: Long = Long.MaxValue
    var `fstPair-cpu-arguments`: Long = Long.MaxValue
    var `fstPair-memory-arguments`: Long = Long.MaxValue
    var `headList-cpu-arguments`: Long = Long.MaxValue
    var `headList-memory-arguments`: Long = Long.MaxValue
    var `iData-cpu-arguments`: Long = Long.MaxValue
    var `iData-memory-arguments`: Long = Long.MaxValue
    var `ifThenElse-cpu-arguments`: Long = Long.MaxValue
    var `ifThenElse-memory-arguments`: Long = Long.MaxValue
    var `indexByteString-cpu-arguments`: Long = Long.MaxValue
    var `indexByteString-memory-arguments`: Long = Long.MaxValue
    var `lengthOfByteString-cpu-arguments`: Long = Long.MaxValue
    var `lengthOfByteString-memory-arguments`: Long = Long.MaxValue
    var `lessThanByteString-cpu-arguments-intercept`: Long = Long.MaxValue
    var `lessThanByteString-cpu-arguments-slope`: Long = Long.MaxValue
    var `lessThanByteString-memory-arguments`: Long = Long.MaxValue
    var `lessThanEqualsByteString-cpu-arguments-intercept`: Long = Long.MaxValue
    var `lessThanEqualsByteString-cpu-arguments-slope`: Long = Long.MaxValue
    var `lessThanEqualsByteString-memory-arguments`: Long = Long.MaxValue
    var `lessThanEqualsInteger-cpu-arguments-intercept`: Long = Long.MaxValue
    var `lessThanEqualsInteger-cpu-arguments-slope`: Long = Long.MaxValue
    var `lessThanEqualsInteger-memory-arguments`: Long = Long.MaxValue
    var `lessThanInteger-cpu-arguments-intercept`: Long = Long.MaxValue
    var `lessThanInteger-cpu-arguments-slope`: Long = Long.MaxValue
    var `lessThanInteger-memory-arguments`: Long = Long.MaxValue
    var `listData-cpu-arguments`: Long = Long.MaxValue
    var `listData-memory-arguments`: Long = Long.MaxValue
    var `mapData-cpu-arguments`: Long = Long.MaxValue
    var `mapData-memory-arguments`: Long = Long.MaxValue
    var `mkCons-cpu-arguments`: Long = Long.MaxValue
    var `mkCons-memory-arguments`: Long = Long.MaxValue
    var `mkNilData-cpu-arguments`: Long = Long.MaxValue
    var `mkNilData-memory-arguments`: Long = Long.MaxValue
    var `mkNilPairData-cpu-arguments`: Long = Long.MaxValue
    var `mkNilPairData-memory-arguments`: Long = Long.MaxValue
    var `mkPairData-cpu-arguments`: Long = Long.MaxValue
    var `mkPairData-memory-arguments`: Long = Long.MaxValue
    var `modInteger-cpu-arguments-constant`: Long = Long.MaxValue
    var `modInteger-cpu-arguments-model-arguments-intercept`: Long = Long.MaxValue
    var `modInteger-cpu-arguments-model-arguments-slope`: Long = Long.MaxValue
    var `modInteger-memory-arguments-intercept`: Long = Long.MaxValue
    var `modInteger-memory-arguments-minimum`: Long = Long.MaxValue
    var `modInteger-memory-arguments-slope`: Long = Long.MaxValue
    var `multiplyInteger-cpu-arguments-intercept`: Long = Long.MaxValue
    var `multiplyInteger-cpu-arguments-slope`: Long = Long.MaxValue
    var `multiplyInteger-memory-arguments-intercept`: Long = Long.MaxValue
    var `multiplyInteger-memory-arguments-slope`: Long = Long.MaxValue
    var `nullList-cpu-arguments`: Long = Long.MaxValue
    var `nullList-memory-arguments`: Long = Long.MaxValue
    var `quotientInteger-cpu-arguments-constant`: Long = Long.MaxValue
    var `quotientInteger-cpu-arguments-model-arguments-intercept`: Long = Long.MaxValue
    var `quotientInteger-cpu-arguments-model-arguments-slope`: Long = Long.MaxValue
    var `quotientInteger-memory-arguments-intercept`: Long = Long.MaxValue
    var `quotientInteger-memory-arguments-minimum`: Long = Long.MaxValue
    var `quotientInteger-memory-arguments-slope`: Long = Long.MaxValue
    var `remainderInteger-cpu-arguments-constant`: Long = Long.MaxValue
    var `remainderInteger-cpu-arguments-model-arguments-intercept`: Long = Long.MaxValue
    var `remainderInteger-cpu-arguments-model-arguments-slope`: Long = Long.MaxValue
    var `remainderInteger-memory-arguments-intercept`: Long = Long.MaxValue
    var `remainderInteger-memory-arguments-minimum`: Long = Long.MaxValue
    var `remainderInteger-memory-arguments-slope`: Long = Long.MaxValue
    var `sha2_256-cpu-arguments-intercept`: Long = Long.MaxValue
    var `sha2_256-cpu-arguments-slope`: Long = Long.MaxValue
    var `sha2_256-memory-arguments`: Long = Long.MaxValue
    var `sha3_256-cpu-arguments-intercept`: Long = Long.MaxValue
    var `sha3_256-cpu-arguments-slope`: Long = Long.MaxValue
    var `sha3_256-memory-arguments`: Long = Long.MaxValue
    var `sliceByteString-cpu-arguments-intercept`: Long = Long.MaxValue
    var `sliceByteString-cpu-arguments-slope`: Long = Long.MaxValue
    var `sliceByteString-memory-arguments-intercept`: Long = Long.MaxValue
    var `sliceByteString-memory-arguments-slope`: Long = Long.MaxValue
    var `sndPair-cpu-arguments`: Long = Long.MaxValue
    var `sndPair-memory-arguments`: Long = Long.MaxValue
    var `subtractInteger-cpu-arguments-intercept`: Long = Long.MaxValue
    var `subtractInteger-cpu-arguments-slope`: Long = Long.MaxValue
    var `subtractInteger-memory-arguments-intercept`: Long = Long.MaxValue
    var `subtractInteger-memory-arguments-slope`: Long = Long.MaxValue
    var `tailList-cpu-arguments`: Long = Long.MaxValue
    var `tailList-memory-arguments`: Long = Long.MaxValue
    var `trace-cpu-arguments`: Long = Long.MaxValue
    var `trace-memory-arguments`: Long = Long.MaxValue
    var `unBData-cpu-arguments`: Long = Long.MaxValue
    var `unBData-memory-arguments`: Long = Long.MaxValue
    var `unConstrData-cpu-arguments`: Long = Long.MaxValue
    var `unConstrData-memory-arguments`: Long = Long.MaxValue
    var `unIData-cpu-arguments`: Long = Long.MaxValue
    var `unIData-memory-arguments`: Long = Long.MaxValue
    var `unListData-cpu-arguments`: Long = Long.MaxValue
    var `unListData-memory-arguments`: Long = Long.MaxValue
    var `unMapData-cpu-arguments`: Long = Long.MaxValue
    var `unMapData-memory-arguments`: Long = Long.MaxValue
    var `verifyEd25519Signature-cpu-arguments-intercept`: Long = Long.MaxValue
    var `verifyEd25519Signature-cpu-arguments-slope`: Long = Long.MaxValue
    var `verifyEd25519Signature-memory-arguments`: Long = Long.MaxValue
    var `serialiseData-cpu-arguments-intercept`: Long = Long.MaxValue
    var `serialiseData-cpu-arguments-slope`: Long = Long.MaxValue
    var `serialiseData-memory-arguments-intercept`: Long = Long.MaxValue
    var `serialiseData-memory-arguments-slope`: Long = Long.MaxValue
    var `verifyEcdsaSecp256k1Signature-cpu-arguments`: Long = Long.MaxValue
    var `verifyEcdsaSecp256k1Signature-memory-arguments`: Long = Long.MaxValue
    var `verifySchnorrSecp256k1Signature-cpu-arguments-intercept`: Long = Long.MaxValue
    var `verifySchnorrSecp256k1Signature-cpu-arguments-slope`: Long = Long.MaxValue
    var `verifySchnorrSecp256k1Signature-memory-arguments`: Long = Long.MaxValue
    var `cekConstrCost-exBudgetCPU`: Long = Long.MaxValue
    var `cekConstrCost-exBudgetMemory`: Long = Long.MaxValue
    var `cekCaseCost-exBudgetCPU`: Long = Long.MaxValue
    var `cekCaseCost-exBudgetMemory`: Long = Long.MaxValue
    var `bls12_381_G1_add-cpu-arguments`: Long = Long.MaxValue
    var `bls12_381_G1_add-memory-arguments`: Long = Long.MaxValue
    var `bls12_381_G1_compress-cpu-arguments`: Long = Long.MaxValue
    var `bls12_381_G1_compress-memory-arguments`: Long = Long.MaxValue
    var `bls12_381_G1_equal-cpu-arguments`: Long = Long.MaxValue
    var `bls12_381_G1_equal-memory-arguments`: Long = Long.MaxValue
    var `bls12_381_G1_hashToGroup-cpu-arguments-intercept`: Long = Long.MaxValue
    var `bls12_381_G1_hashToGroup-cpu-arguments-slope`: Long = Long.MaxValue
    var `bls12_381_G1_hashToGroup-memory-arguments`: Long = Long.MaxValue
    var `bls12_381_G1_neg-cpu-arguments`: Long = Long.MaxValue
    var `bls12_381_G1_neg-memory-arguments`: Long = Long.MaxValue
    var `bls12_381_G1_scalarMul-cpu-arguments-intercept`: Long = Long.MaxValue
    var `bls12_381_G1_scalarMul-cpu-arguments-slope`: Long = Long.MaxValue
    var `bls12_381_G1_scalarMul-memory-arguments`: Long = Long.MaxValue
    var `bls12_381_G1_uncompress-cpu-arguments`: Long = Long.MaxValue
    var `bls12_381_G1_uncompress-memory-arguments`: Long = Long.MaxValue
    var `bls12_381_G2_add-cpu-arguments`: Long = Long.MaxValue
    var `bls12_381_G2_add-memory-arguments`: Long = Long.MaxValue
    var `bls12_381_G2_compress-cpu-arguments`: Long = Long.MaxValue
    var `bls12_381_G2_compress-memory-arguments`: Long = Long.MaxValue
    var `bls12_381_G2_equal-cpu-arguments`: Long = Long.MaxValue
    var `bls12_381_G2_equal-memory-arguments`: Long = Long.MaxValue
    var `bls12_381_G2_hashToGroup-cpu-arguments-intercept`: Long = Long.MaxValue
    var `bls12_381_G2_hashToGroup-cpu-arguments-slope`: Long = Long.MaxValue
    var `bls12_381_G2_hashToGroup-memory-arguments`: Long = Long.MaxValue
    var `bls12_381_G2_neg-cpu-arguments`: Long = Long.MaxValue
    var `bls12_381_G2_neg-memory-arguments`: Long = Long.MaxValue
    var `bls12_381_G2_scalarMul-cpu-arguments-intercept`: Long = Long.MaxValue
    var `bls12_381_G2_scalarMul-cpu-arguments-slope`: Long = Long.MaxValue
    var `bls12_381_G2_scalarMul-memory-arguments`: Long = Long.MaxValue
    var `bls12_381_G2_uncompress-cpu-arguments`: Long = Long.MaxValue
    var `bls12_381_G2_uncompress-memory-arguments`: Long = Long.MaxValue
    var `bls12_381_finalVerify-cpu-arguments`: Long = Long.MaxValue
    var `bls12_381_finalVerify-memory-arguments`: Long = Long.MaxValue
    var `bls12_381_millerLoop-cpu-arguments`: Long = Long.MaxValue
    var `bls12_381_millerLoop-memory-arguments`: Long = Long.MaxValue
    var `bls12_381_mulMlResult-cpu-arguments`: Long = Long.MaxValue
    var `bls12_381_mulMlResult-memory-arguments`: Long = Long.MaxValue
    var `keccak_256-cpu-arguments-intercept`: Long = Long.MaxValue
    var `keccak_256-cpu-arguments-slope`: Long = Long.MaxValue
    var `keccak_256-memory-arguments`: Long = Long.MaxValue
    var `blake2b_224-cpu-arguments-intercept`: Long = Long.MaxValue
    var `blake2b_224-cpu-arguments-slope`: Long = Long.MaxValue
    var `blake2b_224-memory-arguments`: Long = Long.MaxValue
    var `integerToByteString-cpu-arguments-c0`: Long = Long.MaxValue
    var `integerToByteString-cpu-arguments-c1`: Long = Long.MaxValue
    var `integerToByteString-cpu-arguments-c2`: Long = Long.MaxValue
    var `integerToByteString-memory-arguments-intercept`: Long = Long.MaxValue
    var `integerToByteString-memory-arguments-slope`: Long = Long.MaxValue
    var `byteStringToInteger-cpu-arguments-c0`: Long = Long.MaxValue
    var `byteStringToInteger-cpu-arguments-c1`: Long = Long.MaxValue
    var `byteStringToInteger-cpu-arguments-c2`: Long = Long.MaxValue
    var `byteStringToInteger-memory-arguments-intercept`: Long = Long.MaxValue
    var `byteStringToInteger-memory-arguments-slope`: Long = Long.MaxValue
    var `andByteString-cpu-arguments-intercept`: Long = Long.MaxValue
    var `andByteString-cpu-arguments-slope1`: Long = Long.MaxValue
    var `andByteString-cpu-arguments-slope2`: Long = Long.MaxValue
    var `andByteString-memory-arguments-intercept`: Long = Long.MaxValue
    var `andByteString-memory-arguments-slope`: Long = Long.MaxValue
    var `orByteString-cpu-arguments-intercept`: Long = Long.MaxValue
    var `orByteString-cpu-arguments-slope1`: Long = Long.MaxValue
    var `orByteString-cpu-arguments-slope2`: Long = Long.MaxValue
    var `orByteString-memory-arguments-intercept`: Long = Long.MaxValue
    var `orByteString-memory-arguments-slope`: Long = Long.MaxValue
    var `xorByteString-cpu-arguments-intercept`: Long = Long.MaxValue
    var `xorByteString-cpu-arguments-slope1`: Long = Long.MaxValue
    var `xorByteString-cpu-arguments-slope2`: Long = Long.MaxValue
    var `xorByteString-memory-arguments-intercept`: Long = Long.MaxValue
    var `xorByteString-memory-arguments-slope`: Long = Long.MaxValue
    var `complementByteString-cpu-arguments-intercept`: Long = Long.MaxValue
    var `complementByteString-cpu-arguments-slope`: Long = Long.MaxValue
    var `complementByteString-memory-arguments-intercept`: Long = Long.MaxValue
    var `complementByteString-memory-arguments-slope`: Long = Long.MaxValue
    var `readBit-cpu-arguments`: Long = Long.MaxValue
    var `readBit-memory-arguments`: Long = Long.MaxValue
    var `writeBits-cpu-arguments-intercept`: Long = Long.MaxValue
    var `writeBits-cpu-arguments-slope`: Long = Long.MaxValue
    var `writeBits-memory-arguments-intercept`: Long = Long.MaxValue
    var `writeBits-memory-arguments-slope`: Long = Long.MaxValue
    var `replicateByte-cpu-arguments-intercept`: Long = Long.MaxValue
    var `replicateByte-cpu-arguments-slope`: Long = Long.MaxValue
    var `replicateByte-memory-arguments-intercept`: Long = Long.MaxValue
    var `replicateByte-memory-arguments-slope`: Long = Long.MaxValue
    var `shiftByteString-cpu-arguments-intercept`: Long = Long.MaxValue
    var `shiftByteString-cpu-arguments-slope`: Long = Long.MaxValue
    var `shiftByteString-memory-arguments-intercept`: Long = Long.MaxValue
    var `shiftByteString-memory-arguments-slope`: Long = Long.MaxValue
    var `rotateByteString-cpu-arguments-intercept`: Long = Long.MaxValue
    var `rotateByteString-cpu-arguments-slope`: Long = Long.MaxValue
    var `rotateByteString-memory-arguments-intercept`: Long = Long.MaxValue
    var `rotateByteString-memory-arguments-slope`: Long = Long.MaxValue
    var `countSetBits-cpu-arguments-intercept`: Long = Long.MaxValue
    var `countSetBits-cpu-arguments-slope`: Long = Long.MaxValue
    var `countSetBits-memory-arguments`: Long = Long.MaxValue
    var `findFirstSetBit-cpu-arguments-intercept`: Long = Long.MaxValue
    var `findFirstSetBit-cpu-arguments-slope`: Long = Long.MaxValue
    var `findFirstSetBit-memory-arguments`: Long = Long.MaxValue
    var `ripemd_160-cpu-arguments-intercept`: Long = Long.MaxValue
    var `ripemd_160-cpu-arguments-slope`: Long = Long.MaxValue
    var `ripemd_160-memory-arguments`: Long = Long.MaxValue
    var `expModInteger-cpu-arguments-coefficient00`: Long = Long.MaxValue
    var `expModInteger-cpu-arguments-coefficient11`: Long = Long.MaxValue
    var `expModInteger-cpu-arguments-coefficient12`: Long = Long.MaxValue
    var `expModInteger-memory-arguments-intercept`: Long = Long.MaxValue
    var `expModInteger-memory-arguments-slope`: Long = Long.MaxValue
    var `dropList-cpu-arguments-intercept`: Long = Long.MaxValue
    var `dropList-cpu-arguments-slope`: Long = Long.MaxValue
    var `dropList-memory-arguments`: Long = Long.MaxValue
    var `lengthOfArray-cpu-arguments`: Long = Long.MaxValue
    var `lengthOfArray-memory-arguments`: Long = Long.MaxValue
    var `listToArray-cpu-arguments-intercept`: Long = Long.MaxValue
    var `listToArray-cpu-arguments-slope`: Long = Long.MaxValue
    var `listToArray-memory-arguments-intercept`: Long = Long.MaxValue
    var `listToArray-memory-arguments-slope`: Long = Long.MaxValue
    var `indexArray-cpu-arguments`: Long = Long.MaxValue
    var `indexArray-memory-arguments`: Long = Long.MaxValue
    var `bls12_381_G1_multiScalarMul-cpu-arguments-intercept`: Long = Long.MaxValue
    var `bls12_381_G1_multiScalarMul-cpu-arguments-slope`: Long = Long.MaxValue
    var `bls12_381_G1_multiScalarMul-memory-arguments`: Long = Long.MaxValue
    var `bls12_381_G2_multiScalarMul-cpu-arguments-intercept`: Long = Long.MaxValue
    var `bls12_381_G2_multiScalarMul-cpu-arguments-slope`: Long = Long.MaxValue
    var `bls12_381_G2_multiScalarMul-memory-arguments`: Long = Long.MaxValue
    var `insertCoin-cpu-arguments-intercept`: Long = Long.MaxValue
    var `insertCoin-cpu-arguments-slope`: Long = Long.MaxValue
    var `insertCoin-memory-arguments-intercept`: Long = Long.MaxValue
    var `insertCoin-memory-arguments-slope`: Long = Long.MaxValue
    var `lookupCoin-cpu-arguments-intercept`: Long = Long.MaxValue
    var `lookupCoin-cpu-arguments-slope`: Long = Long.MaxValue
    var `lookupCoin-memory-arguments`: Long = Long.MaxValue
    var `unionValue-cpu-arguments-c00`: Long = Long.MaxValue
    var `unionValue-cpu-arguments-c10`: Long = Long.MaxValue
    var `unionValue-cpu-arguments-c01`: Long = Long.MaxValue
    var `unionValue-cpu-arguments-c11`: Long = Long.MaxValue
    var `unionValue-memory-arguments-intercept`: Long = Long.MaxValue
    var `unionValue-memory-arguments-slope`: Long = Long.MaxValue
    var `valueContains-cpu-arguments-constant`: Long = Long.MaxValue
    var `valueContains-cpu-arguments-model-arguments-intercept`: Long = Long.MaxValue
    var `valueContains-cpu-arguments-model-arguments-slope1`: Long = Long.MaxValue
    var `valueContains-cpu-arguments-model-arguments-slope2`: Long = Long.MaxValue
    var `valueContains-memory-arguments`: Long = Long.MaxValue
    var `valueData-cpu-arguments-intercept`: Long = Long.MaxValue
    var `valueData-cpu-arguments-slope`: Long = Long.MaxValue
    var `valueData-memory-arguments-intercept`: Long = Long.MaxValue
    var `valueData-memory-arguments-slope`: Long = Long.MaxValue
    var `unValueData-cpu-arguments-c0`: Long = Long.MaxValue
    var `unValueData-cpu-arguments-c1`: Long = Long.MaxValue
    var `unValueData-cpu-arguments-c2`: Long = Long.MaxValue
    var `unValueData-memory-arguments-intercept`: Long = Long.MaxValue
    var `unValueData-memory-arguments-slope`: Long = Long.MaxValue
    var `scaleValue-cpu-arguments-intercept`: Long = Long.MaxValue
    var `scaleValue-cpu-arguments-slope`: Long = Long.MaxValue
    var `scaleValue-memory-arguments-intercept`: Long = Long.MaxValue
    var `scaleValue-memory-arguments-slope`: Long = Long.MaxValue

    // Parameters absent from this language retain the PlutusParams sentinel convention.
    def `divideInteger-cpu-arguments-c00`: Long = Long.MaxValue
    def `divideInteger-cpu-arguments-c01`: Long = Long.MaxValue
    def `divideInteger-cpu-arguments-c02`: Long = Long.MaxValue
    def `divideInteger-cpu-arguments-c10`: Long = Long.MaxValue
    def `divideInteger-cpu-arguments-c11`: Long = Long.MaxValue
    def `divideInteger-cpu-arguments-c20`: Long = Long.MaxValue
    def `divideInteger-cpu-arguments-minimum`: Long = Long.MaxValue
    def `modInteger-cpu-arguments-model-arguments-c00`: Long = Long.MaxValue
    def `modInteger-cpu-arguments-model-arguments-c01`: Long = Long.MaxValue
    def `modInteger-cpu-arguments-model-arguments-c02`: Long = Long.MaxValue
    def `modInteger-cpu-arguments-model-arguments-c10`: Long = Long.MaxValue
    def `modInteger-cpu-arguments-model-arguments-c11`: Long = Long.MaxValue
    def `modInteger-cpu-arguments-model-arguments-c20`: Long = Long.MaxValue
    def `modInteger-cpu-arguments-model-arguments-minimum`: Long = Long.MaxValue
    def `quotientInteger-cpu-arguments-model-arguments-c00`: Long = Long.MaxValue
    def `quotientInteger-cpu-arguments-model-arguments-c01`: Long = Long.MaxValue
    def `quotientInteger-cpu-arguments-model-arguments-c02`: Long = Long.MaxValue
    def `quotientInteger-cpu-arguments-model-arguments-c10`: Long = Long.MaxValue
    def `quotientInteger-cpu-arguments-model-arguments-c11`: Long = Long.MaxValue
    def `quotientInteger-cpu-arguments-model-arguments-c20`: Long = Long.MaxValue
    def `quotientInteger-cpu-arguments-model-arguments-minimum`: Long = Long.MaxValue
    def `remainderInteger-cpu-arguments-model-arguments-c00`: Long = Long.MaxValue
    def `remainderInteger-cpu-arguments-model-arguments-c01`: Long = Long.MaxValue
    def `remainderInteger-cpu-arguments-model-arguments-c02`: Long = Long.MaxValue
    def `remainderInteger-cpu-arguments-model-arguments-c10`: Long = Long.MaxValue
    def `remainderInteger-cpu-arguments-model-arguments-c11`: Long = Long.MaxValue
    def `remainderInteger-cpu-arguments-model-arguments-c20`: Long = Long.MaxValue
    def `remainderInteger-cpu-arguments-model-arguments-minimum`: Long = Long.MaxValue

    def toJson: String = write(this)
}

/** Plutus V2 cost model parameters.
  *
  * The names of the fields are taken from
  * [[https://github.com/input-output-hk/plutus/blob/1.63.0.0/plutus-ledger-api/src/PlutusLedgerApi/V2/ParamName.hs]]
  * and Blockfrost Protocol Parameters JSON uses these names as well in
  * `blockfrost-params-epoch-645.json`
  *
  * But what's really important is the order of the fields because that's the order of the
  * parameters in the protocol parameters array.
  */
class PlutusV2Params extends PlutusParams {
    var `addInteger-cpu-arguments-intercept`: Long = Long.MaxValue
    var `addInteger-cpu-arguments-slope`: Long = Long.MaxValue
    var `addInteger-memory-arguments-intercept`: Long = Long.MaxValue
    var `addInteger-memory-arguments-slope`: Long = Long.MaxValue
    var `appendByteString-cpu-arguments-intercept`: Long = Long.MaxValue
    var `appendByteString-cpu-arguments-slope`: Long = Long.MaxValue
    var `appendByteString-memory-arguments-intercept`: Long = Long.MaxValue
    var `appendByteString-memory-arguments-slope`: Long = Long.MaxValue
    var `appendString-cpu-arguments-intercept`: Long = Long.MaxValue
    var `appendString-cpu-arguments-slope`: Long = Long.MaxValue
    var `appendString-memory-arguments-intercept`: Long = Long.MaxValue
    var `appendString-memory-arguments-slope`: Long = Long.MaxValue
    var `bData-cpu-arguments`: Long = Long.MaxValue
    var `bData-memory-arguments`: Long = Long.MaxValue
    var `blake2b_256-cpu-arguments-intercept`: Long = Long.MaxValue
    var `blake2b_256-cpu-arguments-slope`: Long = Long.MaxValue
    var `blake2b_256-memory-arguments`: Long = Long.MaxValue
    var `cekApplyCost-exBudgetCPU`: Long = Long.MaxValue
    var `cekApplyCost-exBudgetMemory`: Long = Long.MaxValue
    var `cekBuiltinCost-exBudgetCPU`: Long = Long.MaxValue
    var `cekBuiltinCost-exBudgetMemory`: Long = Long.MaxValue
    var `cekConstCost-exBudgetCPU`: Long = Long.MaxValue
    var `cekConstCost-exBudgetMemory`: Long = Long.MaxValue
    var `cekDelayCost-exBudgetCPU`: Long = Long.MaxValue
    var `cekDelayCost-exBudgetMemory`: Long = Long.MaxValue
    var `cekForceCost-exBudgetCPU`: Long = Long.MaxValue
    var `cekForceCost-exBudgetMemory`: Long = Long.MaxValue
    var `cekLamCost-exBudgetCPU`: Long = Long.MaxValue
    var `cekLamCost-exBudgetMemory`: Long = Long.MaxValue
    var `cekStartupCost-exBudgetCPU`: Long = Long.MaxValue
    var `cekStartupCost-exBudgetMemory`: Long = Long.MaxValue
    var `cekVarCost-exBudgetCPU`: Long = Long.MaxValue
    var `cekVarCost-exBudgetMemory`: Long = Long.MaxValue
    var `chooseData-cpu-arguments`: Long = Long.MaxValue
    var `chooseData-memory-arguments`: Long = Long.MaxValue
    var `chooseList-cpu-arguments`: Long = Long.MaxValue
    var `chooseList-memory-arguments`: Long = Long.MaxValue
    var `chooseUnit-cpu-arguments`: Long = Long.MaxValue
    var `chooseUnit-memory-arguments`: Long = Long.MaxValue
    var `consByteString-cpu-arguments-intercept`: Long = Long.MaxValue
    var `consByteString-cpu-arguments-slope`: Long = Long.MaxValue
    var `consByteString-memory-arguments-intercept`: Long = Long.MaxValue
    var `consByteString-memory-arguments-slope`: Long = Long.MaxValue
    var `constrData-cpu-arguments`: Long = Long.MaxValue
    var `constrData-memory-arguments`: Long = Long.MaxValue
    var `decodeUtf8-cpu-arguments-intercept`: Long = Long.MaxValue
    var `decodeUtf8-cpu-arguments-slope`: Long = Long.MaxValue
    var `decodeUtf8-memory-arguments-intercept`: Long = Long.MaxValue
    var `decodeUtf8-memory-arguments-slope`: Long = Long.MaxValue
    var `divideInteger-cpu-arguments-constant`: Long = Long.MaxValue
    var `divideInteger-cpu-arguments-model-arguments-intercept`: Long = Long.MaxValue
    var `divideInteger-cpu-arguments-model-arguments-slope`: Long = Long.MaxValue
    var `divideInteger-memory-arguments-intercept`: Long = Long.MaxValue
    var `divideInteger-memory-arguments-minimum`: Long = Long.MaxValue
    var `divideInteger-memory-arguments-slope`: Long = Long.MaxValue
    var `encodeUtf8-cpu-arguments-intercept`: Long = Long.MaxValue
    var `encodeUtf8-cpu-arguments-slope`: Long = Long.MaxValue
    var `encodeUtf8-memory-arguments-intercept`: Long = Long.MaxValue
    var `encodeUtf8-memory-arguments-slope`: Long = Long.MaxValue
    var `equalsByteString-cpu-arguments-constant`: Long = Long.MaxValue
    var `equalsByteString-cpu-arguments-intercept`: Long = Long.MaxValue
    var `equalsByteString-cpu-arguments-slope`: Long = Long.MaxValue
    var `equalsByteString-memory-arguments`: Long = Long.MaxValue
    var `equalsData-cpu-arguments-intercept`: Long = Long.MaxValue
    var `equalsData-cpu-arguments-slope`: Long = Long.MaxValue
    var `equalsData-memory-arguments`: Long = Long.MaxValue
    var `equalsInteger-cpu-arguments-intercept`: Long = Long.MaxValue
    var `equalsInteger-cpu-arguments-slope`: Long = Long.MaxValue
    var `equalsInteger-memory-arguments`: Long = Long.MaxValue
    var `equalsString-cpu-arguments-constant`: Long = Long.MaxValue
    var `equalsString-cpu-arguments-intercept`: Long = Long.MaxValue
    var `equalsString-cpu-arguments-slope`: Long = Long.MaxValue
    var `equalsString-memory-arguments`: Long = Long.MaxValue
    var `fstPair-cpu-arguments`: Long = Long.MaxValue
    var `fstPair-memory-arguments`: Long = Long.MaxValue
    var `headList-cpu-arguments`: Long = Long.MaxValue
    var `headList-memory-arguments`: Long = Long.MaxValue
    var `iData-cpu-arguments`: Long = Long.MaxValue
    var `iData-memory-arguments`: Long = Long.MaxValue
    var `ifThenElse-cpu-arguments`: Long = Long.MaxValue
    var `ifThenElse-memory-arguments`: Long = Long.MaxValue
    var `indexByteString-cpu-arguments`: Long = Long.MaxValue
    var `indexByteString-memory-arguments`: Long = Long.MaxValue
    var `lengthOfByteString-cpu-arguments`: Long = Long.MaxValue
    var `lengthOfByteString-memory-arguments`: Long = Long.MaxValue
    var `lessThanByteString-cpu-arguments-intercept`: Long = Long.MaxValue
    var `lessThanByteString-cpu-arguments-slope`: Long = Long.MaxValue
    var `lessThanByteString-memory-arguments`: Long = Long.MaxValue
    var `lessThanEqualsByteString-cpu-arguments-intercept`: Long = Long.MaxValue
    var `lessThanEqualsByteString-cpu-arguments-slope`: Long = Long.MaxValue
    var `lessThanEqualsByteString-memory-arguments`: Long = Long.MaxValue
    var `lessThanEqualsInteger-cpu-arguments-intercept`: Long = Long.MaxValue
    var `lessThanEqualsInteger-cpu-arguments-slope`: Long = Long.MaxValue
    var `lessThanEqualsInteger-memory-arguments`: Long = Long.MaxValue
    var `lessThanInteger-cpu-arguments-intercept`: Long = Long.MaxValue
    var `lessThanInteger-cpu-arguments-slope`: Long = Long.MaxValue
    var `lessThanInteger-memory-arguments`: Long = Long.MaxValue
    var `listData-cpu-arguments`: Long = Long.MaxValue
    var `listData-memory-arguments`: Long = Long.MaxValue
    var `mapData-cpu-arguments`: Long = Long.MaxValue
    var `mapData-memory-arguments`: Long = Long.MaxValue
    var `mkCons-cpu-arguments`: Long = Long.MaxValue
    var `mkCons-memory-arguments`: Long = Long.MaxValue
    var `mkNilData-cpu-arguments`: Long = Long.MaxValue
    var `mkNilData-memory-arguments`: Long = Long.MaxValue
    var `mkNilPairData-cpu-arguments`: Long = Long.MaxValue
    var `mkNilPairData-memory-arguments`: Long = Long.MaxValue
    var `mkPairData-cpu-arguments`: Long = Long.MaxValue
    var `mkPairData-memory-arguments`: Long = Long.MaxValue
    var `modInteger-cpu-arguments-constant`: Long = Long.MaxValue
    var `modInteger-cpu-arguments-model-arguments-intercept`: Long = Long.MaxValue
    var `modInteger-cpu-arguments-model-arguments-slope`: Long = Long.MaxValue
    var `modInteger-memory-arguments-intercept`: Long = Long.MaxValue
    var `modInteger-memory-arguments-minimum`: Long = Long.MaxValue
    var `modInteger-memory-arguments-slope`: Long = Long.MaxValue
    var `multiplyInteger-cpu-arguments-intercept`: Long = Long.MaxValue
    var `multiplyInteger-cpu-arguments-slope`: Long = Long.MaxValue
    var `multiplyInteger-memory-arguments-intercept`: Long = Long.MaxValue
    var `multiplyInteger-memory-arguments-slope`: Long = Long.MaxValue
    var `nullList-cpu-arguments`: Long = Long.MaxValue
    var `nullList-memory-arguments`: Long = Long.MaxValue
    var `quotientInteger-cpu-arguments-constant`: Long = Long.MaxValue
    var `quotientInteger-cpu-arguments-model-arguments-intercept`: Long = Long.MaxValue
    var `quotientInteger-cpu-arguments-model-arguments-slope`: Long = Long.MaxValue
    var `quotientInteger-memory-arguments-intercept`: Long = Long.MaxValue
    var `quotientInteger-memory-arguments-minimum`: Long = Long.MaxValue
    var `quotientInteger-memory-arguments-slope`: Long = Long.MaxValue
    var `remainderInteger-cpu-arguments-constant`: Long = Long.MaxValue
    var `remainderInteger-cpu-arguments-model-arguments-intercept`: Long = Long.MaxValue
    var `remainderInteger-cpu-arguments-model-arguments-slope`: Long = Long.MaxValue
    var `remainderInteger-memory-arguments-intercept`: Long = Long.MaxValue
    var `remainderInteger-memory-arguments-minimum`: Long = Long.MaxValue
    var `remainderInteger-memory-arguments-slope`: Long = Long.MaxValue
    var `serialiseData-cpu-arguments-intercept`: Long = Long.MaxValue
    var `serialiseData-cpu-arguments-slope`: Long = Long.MaxValue
    var `serialiseData-memory-arguments-intercept`: Long = Long.MaxValue
    var `serialiseData-memory-arguments-slope`: Long = Long.MaxValue
    var `sha2_256-cpu-arguments-intercept`: Long = Long.MaxValue
    var `sha2_256-cpu-arguments-slope`: Long = Long.MaxValue
    var `sha2_256-memory-arguments`: Long = Long.MaxValue
    var `sha3_256-cpu-arguments-intercept`: Long = Long.MaxValue
    var `sha3_256-cpu-arguments-slope`: Long = Long.MaxValue
    var `sha3_256-memory-arguments`: Long = Long.MaxValue
    var `sliceByteString-cpu-arguments-intercept`: Long = Long.MaxValue
    var `sliceByteString-cpu-arguments-slope`: Long = Long.MaxValue
    var `sliceByteString-memory-arguments-intercept`: Long = Long.MaxValue
    var `sliceByteString-memory-arguments-slope`: Long = Long.MaxValue
    var `sndPair-cpu-arguments`: Long = Long.MaxValue
    var `sndPair-memory-arguments`: Long = Long.MaxValue
    var `subtractInteger-cpu-arguments-intercept`: Long = Long.MaxValue
    var `subtractInteger-cpu-arguments-slope`: Long = Long.MaxValue
    var `subtractInteger-memory-arguments-intercept`: Long = Long.MaxValue
    var `subtractInteger-memory-arguments-slope`: Long = Long.MaxValue
    var `tailList-cpu-arguments`: Long = Long.MaxValue
    var `tailList-memory-arguments`: Long = Long.MaxValue
    var `trace-cpu-arguments`: Long = Long.MaxValue
    var `trace-memory-arguments`: Long = Long.MaxValue
    var `unBData-cpu-arguments`: Long = Long.MaxValue
    var `unBData-memory-arguments`: Long = Long.MaxValue
    var `unConstrData-cpu-arguments`: Long = Long.MaxValue
    var `unConstrData-memory-arguments`: Long = Long.MaxValue
    var `unIData-cpu-arguments`: Long = Long.MaxValue
    var `unIData-memory-arguments`: Long = Long.MaxValue
    var `unListData-cpu-arguments`: Long = Long.MaxValue
    var `unListData-memory-arguments`: Long = Long.MaxValue
    var `unMapData-cpu-arguments`: Long = Long.MaxValue
    var `unMapData-memory-arguments`: Long = Long.MaxValue
    var `verifyEcdsaSecp256k1Signature-cpu-arguments`: Long = Long.MaxValue
    var `verifyEcdsaSecp256k1Signature-memory-arguments`: Long = Long.MaxValue
    var `verifyEd25519Signature-cpu-arguments-intercept`: Long = Long.MaxValue
    var `verifyEd25519Signature-cpu-arguments-slope`: Long = Long.MaxValue
    var `verifyEd25519Signature-memory-arguments`: Long = Long.MaxValue
    var `verifySchnorrSecp256k1Signature-cpu-arguments-intercept`: Long = Long.MaxValue
    var `verifySchnorrSecp256k1Signature-cpu-arguments-slope`: Long = Long.MaxValue
    var `verifySchnorrSecp256k1Signature-memory-arguments`: Long = Long.MaxValue
    var `integerToByteString-cpu-arguments-c0`: Long = Long.MaxValue
    var `integerToByteString-cpu-arguments-c1`: Long = Long.MaxValue
    var `integerToByteString-cpu-arguments-c2`: Long = Long.MaxValue
    var `integerToByteString-memory-arguments-intercept`: Long = Long.MaxValue
    var `integerToByteString-memory-arguments-slope`: Long = Long.MaxValue
    var `byteStringToInteger-cpu-arguments-c0`: Long = Long.MaxValue
    var `byteStringToInteger-cpu-arguments-c1`: Long = Long.MaxValue
    var `byteStringToInteger-cpu-arguments-c2`: Long = Long.MaxValue
    var `byteStringToInteger-memory-arguments-intercept`: Long = Long.MaxValue
    var `byteStringToInteger-memory-arguments-slope`: Long = Long.MaxValue
    var `cekConstrCost-exBudgetCPU`: Long = Long.MaxValue
    var `cekConstrCost-exBudgetMemory`: Long = Long.MaxValue
    var `cekCaseCost-exBudgetCPU`: Long = Long.MaxValue
    var `cekCaseCost-exBudgetMemory`: Long = Long.MaxValue
    var `bls12_381_G1_add-cpu-arguments`: Long = Long.MaxValue
    var `bls12_381_G1_add-memory-arguments`: Long = Long.MaxValue
    var `bls12_381_G1_compress-cpu-arguments`: Long = Long.MaxValue
    var `bls12_381_G1_compress-memory-arguments`: Long = Long.MaxValue
    var `bls12_381_G1_equal-cpu-arguments`: Long = Long.MaxValue
    var `bls12_381_G1_equal-memory-arguments`: Long = Long.MaxValue
    var `bls12_381_G1_hashToGroup-cpu-arguments-intercept`: Long = Long.MaxValue
    var `bls12_381_G1_hashToGroup-cpu-arguments-slope`: Long = Long.MaxValue
    var `bls12_381_G1_hashToGroup-memory-arguments`: Long = Long.MaxValue
    var `bls12_381_G1_neg-cpu-arguments`: Long = Long.MaxValue
    var `bls12_381_G1_neg-memory-arguments`: Long = Long.MaxValue
    var `bls12_381_G1_scalarMul-cpu-arguments-intercept`: Long = Long.MaxValue
    var `bls12_381_G1_scalarMul-cpu-arguments-slope`: Long = Long.MaxValue
    var `bls12_381_G1_scalarMul-memory-arguments`: Long = Long.MaxValue
    var `bls12_381_G1_uncompress-cpu-arguments`: Long = Long.MaxValue
    var `bls12_381_G1_uncompress-memory-arguments`: Long = Long.MaxValue
    var `bls12_381_G2_add-cpu-arguments`: Long = Long.MaxValue
    var `bls12_381_G2_add-memory-arguments`: Long = Long.MaxValue
    var `bls12_381_G2_compress-cpu-arguments`: Long = Long.MaxValue
    var `bls12_381_G2_compress-memory-arguments`: Long = Long.MaxValue
    var `bls12_381_G2_equal-cpu-arguments`: Long = Long.MaxValue
    var `bls12_381_G2_equal-memory-arguments`: Long = Long.MaxValue
    var `bls12_381_G2_hashToGroup-cpu-arguments-intercept`: Long = Long.MaxValue
    var `bls12_381_G2_hashToGroup-cpu-arguments-slope`: Long = Long.MaxValue
    var `bls12_381_G2_hashToGroup-memory-arguments`: Long = Long.MaxValue
    var `bls12_381_G2_neg-cpu-arguments`: Long = Long.MaxValue
    var `bls12_381_G2_neg-memory-arguments`: Long = Long.MaxValue
    var `bls12_381_G2_scalarMul-cpu-arguments-intercept`: Long = Long.MaxValue
    var `bls12_381_G2_scalarMul-cpu-arguments-slope`: Long = Long.MaxValue
    var `bls12_381_G2_scalarMul-memory-arguments`: Long = Long.MaxValue
    var `bls12_381_G2_uncompress-cpu-arguments`: Long = Long.MaxValue
    var `bls12_381_G2_uncompress-memory-arguments`: Long = Long.MaxValue
    var `bls12_381_finalVerify-cpu-arguments`: Long = Long.MaxValue
    var `bls12_381_finalVerify-memory-arguments`: Long = Long.MaxValue
    var `bls12_381_millerLoop-cpu-arguments`: Long = Long.MaxValue
    var `bls12_381_millerLoop-memory-arguments`: Long = Long.MaxValue
    var `bls12_381_mulMlResult-cpu-arguments`: Long = Long.MaxValue
    var `bls12_381_mulMlResult-memory-arguments`: Long = Long.MaxValue
    var `keccak_256-cpu-arguments-intercept`: Long = Long.MaxValue
    var `keccak_256-cpu-arguments-slope`: Long = Long.MaxValue
    var `keccak_256-memory-arguments`: Long = Long.MaxValue
    var `blake2b_224-cpu-arguments-intercept`: Long = Long.MaxValue
    var `blake2b_224-cpu-arguments-slope`: Long = Long.MaxValue
    var `blake2b_224-memory-arguments`: Long = Long.MaxValue
    var `andByteString-cpu-arguments-intercept`: Long = Long.MaxValue
    var `andByteString-cpu-arguments-slope1`: Long = Long.MaxValue
    var `andByteString-cpu-arguments-slope2`: Long = Long.MaxValue
    var `andByteString-memory-arguments-intercept`: Long = Long.MaxValue
    var `andByteString-memory-arguments-slope`: Long = Long.MaxValue
    var `orByteString-cpu-arguments-intercept`: Long = Long.MaxValue
    var `orByteString-cpu-arguments-slope1`: Long = Long.MaxValue
    var `orByteString-cpu-arguments-slope2`: Long = Long.MaxValue
    var `orByteString-memory-arguments-intercept`: Long = Long.MaxValue
    var `orByteString-memory-arguments-slope`: Long = Long.MaxValue
    var `xorByteString-cpu-arguments-intercept`: Long = Long.MaxValue
    var `xorByteString-cpu-arguments-slope1`: Long = Long.MaxValue
    var `xorByteString-cpu-arguments-slope2`: Long = Long.MaxValue
    var `xorByteString-memory-arguments-intercept`: Long = Long.MaxValue
    var `xorByteString-memory-arguments-slope`: Long = Long.MaxValue
    var `complementByteString-cpu-arguments-intercept`: Long = Long.MaxValue
    var `complementByteString-cpu-arguments-slope`: Long = Long.MaxValue
    var `complementByteString-memory-arguments-intercept`: Long = Long.MaxValue
    var `complementByteString-memory-arguments-slope`: Long = Long.MaxValue
    var `readBit-cpu-arguments`: Long = Long.MaxValue
    var `readBit-memory-arguments`: Long = Long.MaxValue
    var `writeBits-cpu-arguments-intercept`: Long = Long.MaxValue
    var `writeBits-cpu-arguments-slope`: Long = Long.MaxValue
    var `writeBits-memory-arguments-intercept`: Long = Long.MaxValue
    var `writeBits-memory-arguments-slope`: Long = Long.MaxValue
    var `replicateByte-cpu-arguments-intercept`: Long = Long.MaxValue
    var `replicateByte-cpu-arguments-slope`: Long = Long.MaxValue
    var `replicateByte-memory-arguments-intercept`: Long = Long.MaxValue
    var `replicateByte-memory-arguments-slope`: Long = Long.MaxValue
    var `shiftByteString-cpu-arguments-intercept`: Long = Long.MaxValue
    var `shiftByteString-cpu-arguments-slope`: Long = Long.MaxValue
    var `shiftByteString-memory-arguments-intercept`: Long = Long.MaxValue
    var `shiftByteString-memory-arguments-slope`: Long = Long.MaxValue
    var `rotateByteString-cpu-arguments-intercept`: Long = Long.MaxValue
    var `rotateByteString-cpu-arguments-slope`: Long = Long.MaxValue
    var `rotateByteString-memory-arguments-intercept`: Long = Long.MaxValue
    var `rotateByteString-memory-arguments-slope`: Long = Long.MaxValue
    var `countSetBits-cpu-arguments-intercept`: Long = Long.MaxValue
    var `countSetBits-cpu-arguments-slope`: Long = Long.MaxValue
    var `countSetBits-memory-arguments`: Long = Long.MaxValue
    var `findFirstSetBit-cpu-arguments-intercept`: Long = Long.MaxValue
    var `findFirstSetBit-cpu-arguments-slope`: Long = Long.MaxValue
    var `findFirstSetBit-memory-arguments`: Long = Long.MaxValue
    var `ripemd_160-cpu-arguments-intercept`: Long = Long.MaxValue
    var `ripemd_160-cpu-arguments-slope`: Long = Long.MaxValue
    var `ripemd_160-memory-arguments`: Long = Long.MaxValue
    var `expModInteger-cpu-arguments-coefficient00`: Long = Long.MaxValue
    var `expModInteger-cpu-arguments-coefficient11`: Long = Long.MaxValue
    var `expModInteger-cpu-arguments-coefficient12`: Long = Long.MaxValue
    var `expModInteger-memory-arguments-intercept`: Long = Long.MaxValue
    var `expModInteger-memory-arguments-slope`: Long = Long.MaxValue
    var `dropList-cpu-arguments-intercept`: Long = Long.MaxValue
    var `dropList-cpu-arguments-slope`: Long = Long.MaxValue
    var `dropList-memory-arguments`: Long = Long.MaxValue
    var `lengthOfArray-cpu-arguments`: Long = Long.MaxValue
    var `lengthOfArray-memory-arguments`: Long = Long.MaxValue
    var `listToArray-cpu-arguments-intercept`: Long = Long.MaxValue
    var `listToArray-cpu-arguments-slope`: Long = Long.MaxValue
    var `listToArray-memory-arguments-intercept`: Long = Long.MaxValue
    var `listToArray-memory-arguments-slope`: Long = Long.MaxValue
    var `indexArray-cpu-arguments`: Long = Long.MaxValue
    var `indexArray-memory-arguments`: Long = Long.MaxValue
    var `bls12_381_G1_multiScalarMul-cpu-arguments-intercept`: Long = Long.MaxValue
    var `bls12_381_G1_multiScalarMul-cpu-arguments-slope`: Long = Long.MaxValue
    var `bls12_381_G1_multiScalarMul-memory-arguments`: Long = Long.MaxValue
    var `bls12_381_G2_multiScalarMul-cpu-arguments-intercept`: Long = Long.MaxValue
    var `bls12_381_G2_multiScalarMul-cpu-arguments-slope`: Long = Long.MaxValue
    var `bls12_381_G2_multiScalarMul-memory-arguments`: Long = Long.MaxValue
    var `insertCoin-cpu-arguments-intercept`: Long = Long.MaxValue
    var `insertCoin-cpu-arguments-slope`: Long = Long.MaxValue
    var `insertCoin-memory-arguments-intercept`: Long = Long.MaxValue
    var `insertCoin-memory-arguments-slope`: Long = Long.MaxValue
    var `lookupCoin-cpu-arguments-intercept`: Long = Long.MaxValue
    var `lookupCoin-cpu-arguments-slope`: Long = Long.MaxValue
    var `lookupCoin-memory-arguments`: Long = Long.MaxValue
    var `unionValue-cpu-arguments-c00`: Long = Long.MaxValue
    var `unionValue-cpu-arguments-c10`: Long = Long.MaxValue
    var `unionValue-cpu-arguments-c01`: Long = Long.MaxValue
    var `unionValue-cpu-arguments-c11`: Long = Long.MaxValue
    var `unionValue-memory-arguments-intercept`: Long = Long.MaxValue
    var `unionValue-memory-arguments-slope`: Long = Long.MaxValue
    var `valueContains-cpu-arguments-constant`: Long = Long.MaxValue
    var `valueContains-cpu-arguments-model-arguments-intercept`: Long = Long.MaxValue
    var `valueContains-cpu-arguments-model-arguments-slope1`: Long = Long.MaxValue
    var `valueContains-cpu-arguments-model-arguments-slope2`: Long = Long.MaxValue
    var `valueContains-memory-arguments`: Long = Long.MaxValue
    var `valueData-cpu-arguments-intercept`: Long = Long.MaxValue
    var `valueData-cpu-arguments-slope`: Long = Long.MaxValue
    var `valueData-memory-arguments-intercept`: Long = Long.MaxValue
    var `valueData-memory-arguments-slope`: Long = Long.MaxValue
    var `unValueData-cpu-arguments-c0`: Long = Long.MaxValue
    var `unValueData-cpu-arguments-c1`: Long = Long.MaxValue
    var `unValueData-cpu-arguments-c2`: Long = Long.MaxValue
    var `unValueData-memory-arguments-intercept`: Long = Long.MaxValue
    var `unValueData-memory-arguments-slope`: Long = Long.MaxValue
    var `scaleValue-cpu-arguments-intercept`: Long = Long.MaxValue
    var `scaleValue-cpu-arguments-slope`: Long = Long.MaxValue
    var `scaleValue-memory-arguments-intercept`: Long = Long.MaxValue
    var `scaleValue-memory-arguments-slope`: Long = Long.MaxValue

    // Parameters absent from this language retain the PlutusParams sentinel convention.
    def `divideInteger-cpu-arguments-c00`: Long = Long.MaxValue
    def `divideInteger-cpu-arguments-c01`: Long = Long.MaxValue
    def `divideInteger-cpu-arguments-c02`: Long = Long.MaxValue
    def `divideInteger-cpu-arguments-c10`: Long = Long.MaxValue
    def `divideInteger-cpu-arguments-c11`: Long = Long.MaxValue
    def `divideInteger-cpu-arguments-c20`: Long = Long.MaxValue
    def `divideInteger-cpu-arguments-minimum`: Long = Long.MaxValue
    def `modInteger-cpu-arguments-model-arguments-c00`: Long = Long.MaxValue
    def `modInteger-cpu-arguments-model-arguments-c01`: Long = Long.MaxValue
    def `modInteger-cpu-arguments-model-arguments-c02`: Long = Long.MaxValue
    def `modInteger-cpu-arguments-model-arguments-c10`: Long = Long.MaxValue
    def `modInteger-cpu-arguments-model-arguments-c11`: Long = Long.MaxValue
    def `modInteger-cpu-arguments-model-arguments-c20`: Long = Long.MaxValue
    def `modInteger-cpu-arguments-model-arguments-minimum`: Long = Long.MaxValue
    def `quotientInteger-cpu-arguments-model-arguments-c00`: Long = Long.MaxValue
    def `quotientInteger-cpu-arguments-model-arguments-c01`: Long = Long.MaxValue
    def `quotientInteger-cpu-arguments-model-arguments-c02`: Long = Long.MaxValue
    def `quotientInteger-cpu-arguments-model-arguments-c10`: Long = Long.MaxValue
    def `quotientInteger-cpu-arguments-model-arguments-c11`: Long = Long.MaxValue
    def `quotientInteger-cpu-arguments-model-arguments-c20`: Long = Long.MaxValue
    def `quotientInteger-cpu-arguments-model-arguments-minimum`: Long = Long.MaxValue
    def `remainderInteger-cpu-arguments-model-arguments-c00`: Long = Long.MaxValue
    def `remainderInteger-cpu-arguments-model-arguments-c01`: Long = Long.MaxValue
    def `remainderInteger-cpu-arguments-model-arguments-c02`: Long = Long.MaxValue
    def `remainderInteger-cpu-arguments-model-arguments-c10`: Long = Long.MaxValue
    def `remainderInteger-cpu-arguments-model-arguments-c11`: Long = Long.MaxValue
    def `remainderInteger-cpu-arguments-model-arguments-c20`: Long = Long.MaxValue
    def `remainderInteger-cpu-arguments-model-arguments-minimum`: Long = Long.MaxValue

    def toJson: String = write(this)
}

/** Plutus V3 cost model parameters.
  *
  * The names of the fields are taken from
  * [[https://github.com/input-output-hk/plutus/blob/1.63.0.0/plutus-ledger-api/src/PlutusLedgerApi/V3/ParamName.hs]]
  * and Blockfrost Protocol Parameters JSON uses these names as well in
  * `blockfrost-params-epoch-645.json`
  *
  * But what's really important is the order of the fields because that's the order of the
  * parameters in the protocol parameters array.
  */
class PlutusV3Params extends PlutusParams {
    var `addInteger-cpu-arguments-intercept`: Long = Long.MaxValue
    var `addInteger-cpu-arguments-slope`: Long = Long.MaxValue
    var `addInteger-memory-arguments-intercept`: Long = Long.MaxValue
    var `addInteger-memory-arguments-slope`: Long = Long.MaxValue
    var `appendByteString-cpu-arguments-intercept`: Long = Long.MaxValue
    var `appendByteString-cpu-arguments-slope`: Long = Long.MaxValue
    var `appendByteString-memory-arguments-intercept`: Long = Long.MaxValue
    var `appendByteString-memory-arguments-slope`: Long = Long.MaxValue
    var `appendString-cpu-arguments-intercept`: Long = Long.MaxValue
    var `appendString-cpu-arguments-slope`: Long = Long.MaxValue
    var `appendString-memory-arguments-intercept`: Long = Long.MaxValue
    var `appendString-memory-arguments-slope`: Long = Long.MaxValue
    var `bData-cpu-arguments`: Long = Long.MaxValue
    var `bData-memory-arguments`: Long = Long.MaxValue
    var `blake2b_256-cpu-arguments-intercept`: Long = Long.MaxValue
    var `blake2b_256-cpu-arguments-slope`: Long = Long.MaxValue
    var `blake2b_256-memory-arguments`: Long = Long.MaxValue
    var `cekApplyCost-exBudgetCPU`: Long = Long.MaxValue
    var `cekApplyCost-exBudgetMemory`: Long = Long.MaxValue
    var `cekBuiltinCost-exBudgetCPU`: Long = Long.MaxValue
    var `cekBuiltinCost-exBudgetMemory`: Long = Long.MaxValue
    var `cekConstCost-exBudgetCPU`: Long = Long.MaxValue
    var `cekConstCost-exBudgetMemory`: Long = Long.MaxValue
    var `cekDelayCost-exBudgetCPU`: Long = Long.MaxValue
    var `cekDelayCost-exBudgetMemory`: Long = Long.MaxValue
    var `cekForceCost-exBudgetCPU`: Long = Long.MaxValue
    var `cekForceCost-exBudgetMemory`: Long = Long.MaxValue
    var `cekLamCost-exBudgetCPU`: Long = Long.MaxValue
    var `cekLamCost-exBudgetMemory`: Long = Long.MaxValue
    var `cekStartupCost-exBudgetCPU`: Long = Long.MaxValue
    var `cekStartupCost-exBudgetMemory`: Long = Long.MaxValue
    var `cekVarCost-exBudgetCPU`: Long = Long.MaxValue
    var `cekVarCost-exBudgetMemory`: Long = Long.MaxValue
    var `chooseData-cpu-arguments`: Long = Long.MaxValue
    var `chooseData-memory-arguments`: Long = Long.MaxValue
    var `chooseList-cpu-arguments`: Long = Long.MaxValue
    var `chooseList-memory-arguments`: Long = Long.MaxValue
    var `chooseUnit-cpu-arguments`: Long = Long.MaxValue
    var `chooseUnit-memory-arguments`: Long = Long.MaxValue
    var `consByteString-cpu-arguments-intercept`: Long = Long.MaxValue
    var `consByteString-cpu-arguments-slope`: Long = Long.MaxValue
    var `consByteString-memory-arguments-intercept`: Long = Long.MaxValue
    var `consByteString-memory-arguments-slope`: Long = Long.MaxValue
    var `constrData-cpu-arguments`: Long = Long.MaxValue
    var `constrData-memory-arguments`: Long = Long.MaxValue
    var `decodeUtf8-cpu-arguments-intercept`: Long = Long.MaxValue
    var `decodeUtf8-cpu-arguments-slope`: Long = Long.MaxValue
    var `decodeUtf8-memory-arguments-intercept`: Long = Long.MaxValue
    var `decodeUtf8-memory-arguments-slope`: Long = Long.MaxValue
    var `divideInteger-cpu-arguments-constant`: Long = Long.MaxValue
    var `divideInteger-cpu-arguments-c00`: Long = Long.MaxValue
    var `divideInteger-cpu-arguments-c01`: Long = Long.MaxValue
    var `divideInteger-cpu-arguments-c02`: Long = Long.MaxValue
    var `divideInteger-cpu-arguments-c10`: Long = Long.MaxValue
    var `divideInteger-cpu-arguments-c11`: Long = Long.MaxValue
    var `divideInteger-cpu-arguments-c20`: Long = Long.MaxValue
    var `divideInteger-cpu-arguments-minimum`: Long = Long.MaxValue
    var `divideInteger-memory-arguments-intercept`: Long = Long.MaxValue
    var `divideInteger-memory-arguments-minimum`: Long = Long.MaxValue
    var `divideInteger-memory-arguments-slope`: Long = Long.MaxValue
    var `encodeUtf8-cpu-arguments-intercept`: Long = Long.MaxValue
    var `encodeUtf8-cpu-arguments-slope`: Long = Long.MaxValue
    var `encodeUtf8-memory-arguments-intercept`: Long = Long.MaxValue
    var `encodeUtf8-memory-arguments-slope`: Long = Long.MaxValue
    var `equalsByteString-cpu-arguments-constant`: Long = Long.MaxValue
    var `equalsByteString-cpu-arguments-intercept`: Long = Long.MaxValue
    var `equalsByteString-cpu-arguments-slope`: Long = Long.MaxValue
    var `equalsByteString-memory-arguments`: Long = Long.MaxValue
    var `equalsData-cpu-arguments-intercept`: Long = Long.MaxValue
    var `equalsData-cpu-arguments-slope`: Long = Long.MaxValue
    var `equalsData-memory-arguments`: Long = Long.MaxValue
    var `equalsInteger-cpu-arguments-intercept`: Long = Long.MaxValue
    var `equalsInteger-cpu-arguments-slope`: Long = Long.MaxValue
    var `equalsInteger-memory-arguments`: Long = Long.MaxValue
    var `equalsString-cpu-arguments-constant`: Long = Long.MaxValue
    var `equalsString-cpu-arguments-intercept`: Long = Long.MaxValue
    var `equalsString-cpu-arguments-slope`: Long = Long.MaxValue
    var `equalsString-memory-arguments`: Long = Long.MaxValue
    var `fstPair-cpu-arguments`: Long = Long.MaxValue
    var `fstPair-memory-arguments`: Long = Long.MaxValue
    var `headList-cpu-arguments`: Long = Long.MaxValue
    var `headList-memory-arguments`: Long = Long.MaxValue
    var `iData-cpu-arguments`: Long = Long.MaxValue
    var `iData-memory-arguments`: Long = Long.MaxValue
    var `ifThenElse-cpu-arguments`: Long = Long.MaxValue
    var `ifThenElse-memory-arguments`: Long = Long.MaxValue
    var `indexByteString-cpu-arguments`: Long = Long.MaxValue
    var `indexByteString-memory-arguments`: Long = Long.MaxValue
    var `lengthOfByteString-cpu-arguments`: Long = Long.MaxValue
    var `lengthOfByteString-memory-arguments`: Long = Long.MaxValue
    var `lessThanByteString-cpu-arguments-intercept`: Long = Long.MaxValue
    var `lessThanByteString-cpu-arguments-slope`: Long = Long.MaxValue
    var `lessThanByteString-memory-arguments`: Long = Long.MaxValue
    var `lessThanEqualsByteString-cpu-arguments-intercept`: Long = Long.MaxValue
    var `lessThanEqualsByteString-cpu-arguments-slope`: Long = Long.MaxValue
    var `lessThanEqualsByteString-memory-arguments`: Long = Long.MaxValue
    var `lessThanEqualsInteger-cpu-arguments-intercept`: Long = Long.MaxValue
    var `lessThanEqualsInteger-cpu-arguments-slope`: Long = Long.MaxValue
    var `lessThanEqualsInteger-memory-arguments`: Long = Long.MaxValue
    var `lessThanInteger-cpu-arguments-intercept`: Long = Long.MaxValue
    var `lessThanInteger-cpu-arguments-slope`: Long = Long.MaxValue
    var `lessThanInteger-memory-arguments`: Long = Long.MaxValue
    var `listData-cpu-arguments`: Long = Long.MaxValue
    var `listData-memory-arguments`: Long = Long.MaxValue
    var `mapData-cpu-arguments`: Long = Long.MaxValue
    var `mapData-memory-arguments`: Long = Long.MaxValue
    var `mkCons-cpu-arguments`: Long = Long.MaxValue
    var `mkCons-memory-arguments`: Long = Long.MaxValue
    var `mkNilData-cpu-arguments`: Long = Long.MaxValue
    var `mkNilData-memory-arguments`: Long = Long.MaxValue
    var `mkNilPairData-cpu-arguments`: Long = Long.MaxValue
    var `mkNilPairData-memory-arguments`: Long = Long.MaxValue
    var `mkPairData-cpu-arguments`: Long = Long.MaxValue
    var `mkPairData-memory-arguments`: Long = Long.MaxValue
    var `modInteger-cpu-arguments-constant`: Long = Long.MaxValue
    var `modInteger-cpu-arguments-model-arguments-c00`: Long = Long.MaxValue
    var `modInteger-cpu-arguments-model-arguments-c01`: Long = Long.MaxValue
    var `modInteger-cpu-arguments-model-arguments-c02`: Long = Long.MaxValue
    var `modInteger-cpu-arguments-model-arguments-c10`: Long = Long.MaxValue
    var `modInteger-cpu-arguments-model-arguments-c11`: Long = Long.MaxValue
    var `modInteger-cpu-arguments-model-arguments-c20`: Long = Long.MaxValue
    var `modInteger-cpu-arguments-model-arguments-minimum`: Long = Long.MaxValue
    var `modInteger-memory-arguments-intercept`: Long = Long.MaxValue
    var `modInteger-memory-arguments-slope`: Long = Long.MaxValue
    var `multiplyInteger-cpu-arguments-intercept`: Long = Long.MaxValue
    var `multiplyInteger-cpu-arguments-slope`: Long = Long.MaxValue
    var `multiplyInteger-memory-arguments-intercept`: Long = Long.MaxValue
    var `multiplyInteger-memory-arguments-slope`: Long = Long.MaxValue
    var `nullList-cpu-arguments`: Long = Long.MaxValue
    var `nullList-memory-arguments`: Long = Long.MaxValue
    var `quotientInteger-cpu-arguments-constant`: Long = Long.MaxValue
    var `quotientInteger-cpu-arguments-model-arguments-c00`: Long = Long.MaxValue
    var `quotientInteger-cpu-arguments-model-arguments-c01`: Long = Long.MaxValue
    var `quotientInteger-cpu-arguments-model-arguments-c02`: Long = Long.MaxValue
    var `quotientInteger-cpu-arguments-model-arguments-c10`: Long = Long.MaxValue
    var `quotientInteger-cpu-arguments-model-arguments-c11`: Long = Long.MaxValue
    var `quotientInteger-cpu-arguments-model-arguments-c20`: Long = Long.MaxValue
    var `quotientInteger-cpu-arguments-model-arguments-minimum`: Long = Long.MaxValue
    var `quotientInteger-memory-arguments-intercept`: Long = Long.MaxValue
    var `quotientInteger-memory-arguments-minimum`: Long = Long.MaxValue
    var `quotientInteger-memory-arguments-slope`: Long = Long.MaxValue
    var `remainderInteger-cpu-arguments-constant`: Long = Long.MaxValue
    var `remainderInteger-cpu-arguments-model-arguments-c00`: Long = Long.MaxValue
    var `remainderInteger-cpu-arguments-model-arguments-c01`: Long = Long.MaxValue
    var `remainderInteger-cpu-arguments-model-arguments-c02`: Long = Long.MaxValue
    var `remainderInteger-cpu-arguments-model-arguments-c10`: Long = Long.MaxValue
    var `remainderInteger-cpu-arguments-model-arguments-c11`: Long = Long.MaxValue
    var `remainderInteger-cpu-arguments-model-arguments-c20`: Long = Long.MaxValue
    var `remainderInteger-cpu-arguments-model-arguments-minimum`: Long = Long.MaxValue
    var `remainderInteger-memory-arguments-intercept`: Long = Long.MaxValue
    var `remainderInteger-memory-arguments-slope`: Long = Long.MaxValue
    var `serialiseData-cpu-arguments-intercept`: Long = Long.MaxValue
    var `serialiseData-cpu-arguments-slope`: Long = Long.MaxValue
    var `serialiseData-memory-arguments-intercept`: Long = Long.MaxValue
    var `serialiseData-memory-arguments-slope`: Long = Long.MaxValue
    var `sha2_256-cpu-arguments-intercept`: Long = Long.MaxValue
    var `sha2_256-cpu-arguments-slope`: Long = Long.MaxValue
    var `sha2_256-memory-arguments`: Long = Long.MaxValue
    var `sha3_256-cpu-arguments-intercept`: Long = Long.MaxValue
    var `sha3_256-cpu-arguments-slope`: Long = Long.MaxValue
    var `sha3_256-memory-arguments`: Long = Long.MaxValue
    var `sliceByteString-cpu-arguments-intercept`: Long = Long.MaxValue
    var `sliceByteString-cpu-arguments-slope`: Long = Long.MaxValue
    var `sliceByteString-memory-arguments-intercept`: Long = Long.MaxValue
    var `sliceByteString-memory-arguments-slope`: Long = Long.MaxValue
    var `sndPair-cpu-arguments`: Long = Long.MaxValue
    var `sndPair-memory-arguments`: Long = Long.MaxValue
    var `subtractInteger-cpu-arguments-intercept`: Long = Long.MaxValue
    var `subtractInteger-cpu-arguments-slope`: Long = Long.MaxValue
    var `subtractInteger-memory-arguments-intercept`: Long = Long.MaxValue
    var `subtractInteger-memory-arguments-slope`: Long = Long.MaxValue
    var `tailList-cpu-arguments`: Long = Long.MaxValue
    var `tailList-memory-arguments`: Long = Long.MaxValue
    var `trace-cpu-arguments`: Long = Long.MaxValue
    var `trace-memory-arguments`: Long = Long.MaxValue
    var `unBData-cpu-arguments`: Long = Long.MaxValue
    var `unBData-memory-arguments`: Long = Long.MaxValue
    var `unConstrData-cpu-arguments`: Long = Long.MaxValue
    var `unConstrData-memory-arguments`: Long = Long.MaxValue
    var `unIData-cpu-arguments`: Long = Long.MaxValue
    var `unIData-memory-arguments`: Long = Long.MaxValue
    var `unListData-cpu-arguments`: Long = Long.MaxValue
    var `unListData-memory-arguments`: Long = Long.MaxValue
    var `unMapData-cpu-arguments`: Long = Long.MaxValue
    var `unMapData-memory-arguments`: Long = Long.MaxValue
    var `verifyEcdsaSecp256k1Signature-cpu-arguments`: Long = Long.MaxValue
    var `verifyEcdsaSecp256k1Signature-memory-arguments`: Long = Long.MaxValue
    var `verifyEd25519Signature-cpu-arguments-intercept`: Long = Long.MaxValue
    var `verifyEd25519Signature-cpu-arguments-slope`: Long = Long.MaxValue
    var `verifyEd25519Signature-memory-arguments`: Long = Long.MaxValue
    var `verifySchnorrSecp256k1Signature-cpu-arguments-intercept`: Long = Long.MaxValue
    var `verifySchnorrSecp256k1Signature-cpu-arguments-slope`: Long = Long.MaxValue
    var `verifySchnorrSecp256k1Signature-memory-arguments`: Long = Long.MaxValue
    var `cekConstrCost-exBudgetCPU`: Long = Long.MaxValue
    var `cekConstrCost-exBudgetMemory`: Long = Long.MaxValue
    var `cekCaseCost-exBudgetCPU`: Long = Long.MaxValue
    var `cekCaseCost-exBudgetMemory`: Long = Long.MaxValue
    var `bls12_381_G1_add-cpu-arguments`: Long = Long.MaxValue
    var `bls12_381_G1_add-memory-arguments`: Long = Long.MaxValue
    var `bls12_381_G1_compress-cpu-arguments`: Long = Long.MaxValue
    var `bls12_381_G1_compress-memory-arguments`: Long = Long.MaxValue
    var `bls12_381_G1_equal-cpu-arguments`: Long = Long.MaxValue
    var `bls12_381_G1_equal-memory-arguments`: Long = Long.MaxValue
    var `bls12_381_G1_hashToGroup-cpu-arguments-intercept`: Long = Long.MaxValue
    var `bls12_381_G1_hashToGroup-cpu-arguments-slope`: Long = Long.MaxValue
    var `bls12_381_G1_hashToGroup-memory-arguments`: Long = Long.MaxValue
    var `bls12_381_G1_neg-cpu-arguments`: Long = Long.MaxValue
    var `bls12_381_G1_neg-memory-arguments`: Long = Long.MaxValue
    var `bls12_381_G1_scalarMul-cpu-arguments-intercept`: Long = Long.MaxValue
    var `bls12_381_G1_scalarMul-cpu-arguments-slope`: Long = Long.MaxValue
    var `bls12_381_G1_scalarMul-memory-arguments`: Long = Long.MaxValue
    var `bls12_381_G1_uncompress-cpu-arguments`: Long = Long.MaxValue
    var `bls12_381_G1_uncompress-memory-arguments`: Long = Long.MaxValue
    var `bls12_381_G2_add-cpu-arguments`: Long = Long.MaxValue
    var `bls12_381_G2_add-memory-arguments`: Long = Long.MaxValue
    var `bls12_381_G2_compress-cpu-arguments`: Long = Long.MaxValue
    var `bls12_381_G2_compress-memory-arguments`: Long = Long.MaxValue
    var `bls12_381_G2_equal-cpu-arguments`: Long = Long.MaxValue
    var `bls12_381_G2_equal-memory-arguments`: Long = Long.MaxValue
    var `bls12_381_G2_hashToGroup-cpu-arguments-intercept`: Long = Long.MaxValue
    var `bls12_381_G2_hashToGroup-cpu-arguments-slope`: Long = Long.MaxValue
    var `bls12_381_G2_hashToGroup-memory-arguments`: Long = Long.MaxValue
    var `bls12_381_G2_neg-cpu-arguments`: Long = Long.MaxValue
    var `bls12_381_G2_neg-memory-arguments`: Long = Long.MaxValue
    var `bls12_381_G2_scalarMul-cpu-arguments-intercept`: Long = Long.MaxValue
    var `bls12_381_G2_scalarMul-cpu-arguments-slope`: Long = Long.MaxValue
    var `bls12_381_G2_scalarMul-memory-arguments`: Long = Long.MaxValue
    var `bls12_381_G2_uncompress-cpu-arguments`: Long = Long.MaxValue
    var `bls12_381_G2_uncompress-memory-arguments`: Long = Long.MaxValue
    var `bls12_381_finalVerify-cpu-arguments`: Long = Long.MaxValue
    var `bls12_381_finalVerify-memory-arguments`: Long = Long.MaxValue
    var `bls12_381_millerLoop-cpu-arguments`: Long = Long.MaxValue
    var `bls12_381_millerLoop-memory-arguments`: Long = Long.MaxValue
    var `bls12_381_mulMlResult-cpu-arguments`: Long = Long.MaxValue
    var `bls12_381_mulMlResult-memory-arguments`: Long = Long.MaxValue
    var `keccak_256-cpu-arguments-intercept`: Long = Long.MaxValue
    var `keccak_256-cpu-arguments-slope`: Long = Long.MaxValue
    var `keccak_256-memory-arguments`: Long = Long.MaxValue
    var `blake2b_224-cpu-arguments-intercept`: Long = Long.MaxValue
    var `blake2b_224-cpu-arguments-slope`: Long = Long.MaxValue
    var `blake2b_224-memory-arguments`: Long = Long.MaxValue
    var `integerToByteString-cpu-arguments-c0`: Long = Long.MaxValue
    var `integerToByteString-cpu-arguments-c1`: Long = Long.MaxValue
    var `integerToByteString-cpu-arguments-c2`: Long = Long.MaxValue
    var `integerToByteString-memory-arguments-intercept`: Long = Long.MaxValue
    var `integerToByteString-memory-arguments-slope`: Long = Long.MaxValue
    var `byteStringToInteger-cpu-arguments-c0`: Long = Long.MaxValue
    var `byteStringToInteger-cpu-arguments-c1`: Long = Long.MaxValue
    var `byteStringToInteger-cpu-arguments-c2`: Long = Long.MaxValue
    var `byteStringToInteger-memory-arguments-intercept`: Long = Long.MaxValue
    var `byteStringToInteger-memory-arguments-slope`: Long = Long.MaxValue
    var `andByteString-cpu-arguments-intercept`: Long = Long.MaxValue
    var `andByteString-cpu-arguments-slope1`: Long = Long.MaxValue
    var `andByteString-cpu-arguments-slope2`: Long = Long.MaxValue
    var `andByteString-memory-arguments-intercept`: Long = Long.MaxValue
    var `andByteString-memory-arguments-slope`: Long = Long.MaxValue
    var `orByteString-cpu-arguments-intercept`: Long = Long.MaxValue
    var `orByteString-cpu-arguments-slope1`: Long = Long.MaxValue
    var `orByteString-cpu-arguments-slope2`: Long = Long.MaxValue
    var `orByteString-memory-arguments-intercept`: Long = Long.MaxValue
    var `orByteString-memory-arguments-slope`: Long = Long.MaxValue
    var `xorByteString-cpu-arguments-intercept`: Long = Long.MaxValue
    var `xorByteString-cpu-arguments-slope1`: Long = Long.MaxValue
    var `xorByteString-cpu-arguments-slope2`: Long = Long.MaxValue
    var `xorByteString-memory-arguments-intercept`: Long = Long.MaxValue
    var `xorByteString-memory-arguments-slope`: Long = Long.MaxValue
    var `complementByteString-cpu-arguments-intercept`: Long = Long.MaxValue
    var `complementByteString-cpu-arguments-slope`: Long = Long.MaxValue
    var `complementByteString-memory-arguments-intercept`: Long = Long.MaxValue
    var `complementByteString-memory-arguments-slope`: Long = Long.MaxValue
    var `readBit-cpu-arguments`: Long = Long.MaxValue
    var `readBit-memory-arguments`: Long = Long.MaxValue
    var `writeBits-cpu-arguments-intercept`: Long = Long.MaxValue
    var `writeBits-cpu-arguments-slope`: Long = Long.MaxValue
    var `writeBits-memory-arguments-intercept`: Long = Long.MaxValue
    var `writeBits-memory-arguments-slope`: Long = Long.MaxValue
    var `replicateByte-cpu-arguments-intercept`: Long = Long.MaxValue
    var `replicateByte-cpu-arguments-slope`: Long = Long.MaxValue
    var `replicateByte-memory-arguments-intercept`: Long = Long.MaxValue
    var `replicateByte-memory-arguments-slope`: Long = Long.MaxValue
    var `shiftByteString-cpu-arguments-intercept`: Long = Long.MaxValue
    var `shiftByteString-cpu-arguments-slope`: Long = Long.MaxValue
    var `shiftByteString-memory-arguments-intercept`: Long = Long.MaxValue
    var `shiftByteString-memory-arguments-slope`: Long = Long.MaxValue
    var `rotateByteString-cpu-arguments-intercept`: Long = Long.MaxValue
    var `rotateByteString-cpu-arguments-slope`: Long = Long.MaxValue
    var `rotateByteString-memory-arguments-intercept`: Long = Long.MaxValue
    var `rotateByteString-memory-arguments-slope`: Long = Long.MaxValue
    var `countSetBits-cpu-arguments-intercept`: Long = Long.MaxValue
    var `countSetBits-cpu-arguments-slope`: Long = Long.MaxValue
    var `countSetBits-memory-arguments`: Long = Long.MaxValue
    var `findFirstSetBit-cpu-arguments-intercept`: Long = Long.MaxValue
    var `findFirstSetBit-cpu-arguments-slope`: Long = Long.MaxValue
    var `findFirstSetBit-memory-arguments`: Long = Long.MaxValue
    var `ripemd_160-cpu-arguments-intercept`: Long = Long.MaxValue
    var `ripemd_160-cpu-arguments-slope`: Long = Long.MaxValue
    var `ripemd_160-memory-arguments`: Long = Long.MaxValue
    var `expModInteger-cpu-arguments-coefficient00`: Long = Long.MaxValue
    var `expModInteger-cpu-arguments-coefficient11`: Long = Long.MaxValue
    var `expModInteger-cpu-arguments-coefficient12`: Long = Long.MaxValue
    var `expModInteger-memory-arguments-intercept`: Long = Long.MaxValue
    var `expModInteger-memory-arguments-slope`: Long = Long.MaxValue
    var `dropList-cpu-arguments-intercept`: Long = Long.MaxValue
    var `dropList-cpu-arguments-slope`: Long = Long.MaxValue
    var `dropList-memory-arguments`: Long = Long.MaxValue
    var `lengthOfArray-cpu-arguments`: Long = Long.MaxValue
    var `lengthOfArray-memory-arguments`: Long = Long.MaxValue
    var `listToArray-cpu-arguments-intercept`: Long = Long.MaxValue
    var `listToArray-cpu-arguments-slope`: Long = Long.MaxValue
    var `listToArray-memory-arguments-intercept`: Long = Long.MaxValue
    var `listToArray-memory-arguments-slope`: Long = Long.MaxValue
    var `indexArray-cpu-arguments`: Long = Long.MaxValue
    var `indexArray-memory-arguments`: Long = Long.MaxValue
    var `bls12_381_G1_multiScalarMul-cpu-arguments-intercept`: Long = Long.MaxValue
    var `bls12_381_G1_multiScalarMul-cpu-arguments-slope`: Long = Long.MaxValue
    var `bls12_381_G1_multiScalarMul-memory-arguments`: Long = Long.MaxValue
    var `bls12_381_G2_multiScalarMul-cpu-arguments-intercept`: Long = Long.MaxValue
    var `bls12_381_G2_multiScalarMul-cpu-arguments-slope`: Long = Long.MaxValue
    var `bls12_381_G2_multiScalarMul-memory-arguments`: Long = Long.MaxValue
    var `insertCoin-cpu-arguments-intercept`: Long = Long.MaxValue
    var `insertCoin-cpu-arguments-slope`: Long = Long.MaxValue
    var `insertCoin-memory-arguments-intercept`: Long = Long.MaxValue
    var `insertCoin-memory-arguments-slope`: Long = Long.MaxValue
    var `lookupCoin-cpu-arguments-intercept`: Long = Long.MaxValue
    var `lookupCoin-cpu-arguments-slope`: Long = Long.MaxValue
    var `lookupCoin-memory-arguments`: Long = Long.MaxValue
    var `unionValue-cpu-arguments-c00`: Long = Long.MaxValue
    var `unionValue-cpu-arguments-c10`: Long = Long.MaxValue
    var `unionValue-cpu-arguments-c01`: Long = Long.MaxValue
    var `unionValue-cpu-arguments-c11`: Long = Long.MaxValue
    var `unionValue-memory-arguments-intercept`: Long = Long.MaxValue
    var `unionValue-memory-arguments-slope`: Long = Long.MaxValue
    var `valueContains-cpu-arguments-constant`: Long = Long.MaxValue
    var `valueContains-cpu-arguments-model-arguments-intercept`: Long = Long.MaxValue
    var `valueContains-cpu-arguments-model-arguments-slope1`: Long = Long.MaxValue
    var `valueContains-cpu-arguments-model-arguments-slope2`: Long = Long.MaxValue
    var `valueContains-memory-arguments`: Long = Long.MaxValue
    var `valueData-cpu-arguments-intercept`: Long = Long.MaxValue
    var `valueData-cpu-arguments-slope`: Long = Long.MaxValue
    var `valueData-memory-arguments-intercept`: Long = Long.MaxValue
    var `valueData-memory-arguments-slope`: Long = Long.MaxValue
    var `unValueData-cpu-arguments-c0`: Long = Long.MaxValue
    var `unValueData-cpu-arguments-c1`: Long = Long.MaxValue
    var `unValueData-cpu-arguments-c2`: Long = Long.MaxValue
    var `unValueData-memory-arguments-intercept`: Long = Long.MaxValue
    var `unValueData-memory-arguments-slope`: Long = Long.MaxValue
    var `scaleValue-cpu-arguments-intercept`: Long = Long.MaxValue
    var `scaleValue-cpu-arguments-slope`: Long = Long.MaxValue
    var `scaleValue-memory-arguments-intercept`: Long = Long.MaxValue
    var `scaleValue-memory-arguments-slope`: Long = Long.MaxValue

    // Not available in Plutus V3, old names kept for compatibility
    def `divideInteger-cpu-arguments-model-arguments-intercept`: Long = Long.MaxValue
    def `divideInteger-cpu-arguments-model-arguments-slope`: Long = Long.MaxValue
    def `modInteger-cpu-arguments-model-arguments-intercept`: Long = Long.MaxValue
    def `modInteger-cpu-arguments-model-arguments-slope`: Long = Long.MaxValue
    def `modInteger-memory-arguments-minimum`: Long = Long.MaxValue
    def `quotientInteger-cpu-arguments-model-arguments-intercept`: Long = Long.MaxValue
    def `quotientInteger-cpu-arguments-model-arguments-slope`: Long = Long.MaxValue
    def `remainderInteger-cpu-arguments-model-arguments-intercept`: Long = Long.MaxValue
    def `remainderInteger-cpu-arguments-model-arguments-slope`: Long = Long.MaxValue
    def `remainderInteger-memory-arguments-minimum`: Long = Long.MaxValue
    def toJson: String = write(this)
}

private object JsonUtils {

    /** Generates a [[ReadWriter]] for a class with fields that are not private
      *
      * @tparam A
      *   the type of the class
      *
      * @example
      *   {{{
      *  class Foo {
      *   var a: Int = 0
      *  }
      *  given rw = mkClassFieldsReadWriter[Foo]
      *   }}}
      */
    inline def mkClassFieldsReadWriter[A]: ReadWriter[A] = ${
        Macros.mkReadWriterImpl[A]
    }

    /** Generates a pair of functions to convert a class with fields to a sequence of longs and back
      * @example
      *   {{{
      *  class Foo {
      *  var a: Int = 0
      *  }
      *  val (toSeq, fromSeq) = mkClassFieldsFromSeqIso[Foo]
      *   }}}
      *   where `fromSeq` looks like this
      *   {{{
      *  val foo = new Foo()
      *  foo.a = seq(0)
      *  foo
      *   }}}
      */
    inline def mkClassFieldsFromSeqIso[A]: (A => Seq[Long], Seq[Long] => A) = ${
        Macros.mkClassFieldsFromSeqIsoImpl[A]
    }

}

object PlutusV1Params:
    given ReadWriter[PlutusV1Params] = JsonUtils.mkClassFieldsReadWriter[PlutusV1Params]
    val (toSeq, fromSeq) = JsonUtils.mkClassFieldsFromSeqIso[PlutusV1Params]

    /** How many positional cost parameters this language takes, counted from the class itself so it
      * cannot drift from the field list. Unlike [[PlutusParams.numberOfParams]] this needs no
      * reflection, so it links on Scala.js.
      */
    val numberOfParams: Int = toSeq(PlutusV1Params()).size

object PlutusV2Params:
    given ReadWriter[PlutusV2Params] = JsonUtils.mkClassFieldsReadWriter[PlutusV2Params]
    val (toSeq, fromSeq) = JsonUtils.mkClassFieldsFromSeqIso[PlutusV2Params]

    /** How many positional cost parameters this language takes, counted from the class itself so it
      * cannot drift from the field list. Unlike [[PlutusParams.numberOfParams]] this needs no
      * reflection, so it links on Scala.js.
      */
    val numberOfParams: Int = toSeq(PlutusV2Params()).size

object PlutusV3Params:
    given ReadWriter[PlutusV3Params] = JsonUtils.mkClassFieldsReadWriter[PlutusV3Params]
    val (toSeq, fromSeq) = JsonUtils.mkClassFieldsFromSeqIso[PlutusV3Params]

    /** How many positional cost parameters this language takes, counted from the class itself so it
      * cannot drift from the field list. Unlike [[PlutusParams.numberOfParams]] this needs no
      * reflection, so it links on Scala.js.
      */
    val numberOfParams: Int = toSeq(PlutusV3Params()).size

// TODO: Create a real PlutusV4Params class with costs for new builtins when they are finalized
// For now, PlutusV4 uses the same cost model parameters as PlutusV3
type PlutusV4Params = PlutusV3Params

object PlutusV4Params:
    export PlutusV3Params.{fromSeq, given_ReadWriter_PlutusV3Params as given_ReadWriter_PlutusV4Params, numberOfParams, toSeq}
