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
  * [[https://github.com/input-output-hk/plutus/blob/1.40.0.0/plutus-ledger-api/src/PlutusLedgerApi/V1/ParamName.hs]]
  * and Blockfrost Protocol Parameters JSON uses these names as well in
  * `blockfrost-params-epoch-544.json`
  *
  * But what's really important is the order of the fields because that's the order of the
  * parameters in the protocol parameters array.
  */
class PlutusV1Params extends PlutusParams {
    var `addInteger-cpu-arguments-intercept`: Long = 300_000_000L
    var `addInteger-cpu-arguments-slope`: Long = 300_000_000L
    var `addInteger-memory-arguments-intercept`: Long = 300_000_000L
    var `addInteger-memory-arguments-slope`: Long = 300_000_000L
    var `appendByteString-cpu-arguments-intercept`: Long = 300_000_000L
    var `appendByteString-cpu-arguments-slope`: Long = 300_000_000L
    var `appendByteString-memory-arguments-intercept`: Long = 300_000_000L
    var `appendByteString-memory-arguments-slope`: Long = 300_000_000L
    var `appendString-cpu-arguments-intercept`: Long = 300_000_000L
    var `appendString-cpu-arguments-slope`: Long = 300_000_000L
    var `appendString-memory-arguments-intercept`: Long = 300_000_000L
    var `appendString-memory-arguments-slope`: Long = 300_000_000L
    var `bData-cpu-arguments`: Long = 300_000_000L
    var `bData-memory-arguments`: Long = 300_000_000L
    var `blake2b_256-cpu-arguments-intercept`: Long = 300_000_000L
    var `blake2b_256-cpu-arguments-slope`: Long = 300_000_000L
    var `blake2b_256-memory-arguments`: Long = 300_000_000L
    var `cekApplyCost-exBudgetCPU`: Long = 300_000_000L
    var `cekApplyCost-exBudgetMemory`: Long = 300_000_000L
    var `cekBuiltinCost-exBudgetCPU`: Long = 300_000_000L
    var `cekBuiltinCost-exBudgetMemory`: Long = 300_000_000L
    var `cekConstCost-exBudgetCPU`: Long = 300_000_000L
    var `cekConstCost-exBudgetMemory`: Long = 300_000_000L
    var `cekDelayCost-exBudgetCPU`: Long = 300_000_000L
    var `cekDelayCost-exBudgetMemory`: Long = 300_000_000L
    var `cekForceCost-exBudgetCPU`: Long = 300_000_000L
    var `cekForceCost-exBudgetMemory`: Long = 300_000_000L
    var `cekLamCost-exBudgetCPU`: Long = 300_000_000L
    var `cekLamCost-exBudgetMemory`: Long = 300_000_000L
    var `cekStartupCost-exBudgetCPU`: Long = 300_000_000L
    var `cekStartupCost-exBudgetMemory`: Long = 300_000_000L
    var `cekVarCost-exBudgetCPU`: Long = 300_000_000L
    var `cekVarCost-exBudgetMemory`: Long = 300_000_000L
    var `chooseData-cpu-arguments`: Long = 300_000_000L
    var `chooseData-memory-arguments`: Long = 300_000_000L
    var `chooseList-cpu-arguments`: Long = 300_000_000L
    var `chooseList-memory-arguments`: Long = 300_000_000L
    var `chooseUnit-cpu-arguments`: Long = 300_000_000L
    var `chooseUnit-memory-arguments`: Long = 300_000_000L
    var `consByteString-cpu-arguments-intercept`: Long = 300_000_000L
    var `consByteString-cpu-arguments-slope`: Long = 300_000_000L
    var `consByteString-memory-arguments-intercept`: Long = 300_000_000L
    var `consByteString-memory-arguments-slope`: Long = 300_000_000L
    var `constrData-cpu-arguments`: Long = 300_000_000L
    var `constrData-memory-arguments`: Long = 300_000_000L
    var `decodeUtf8-cpu-arguments-intercept`: Long = 300_000_000L
    var `decodeUtf8-cpu-arguments-slope`: Long = 300_000_000L
    var `decodeUtf8-memory-arguments-intercept`: Long = 300_000_000L
    var `decodeUtf8-memory-arguments-slope`: Long = 300_000_000L
    var `divideInteger-cpu-arguments-constant`: Long = 300_000_000L
    var `divideInteger-cpu-arguments-model-arguments-intercept`: Long = 300_000_000L
    var `divideInteger-cpu-arguments-model-arguments-slope`: Long = 300_000_000L
    var `divideInteger-memory-arguments-intercept`: Long = 300_000_000L
    var `divideInteger-memory-arguments-minimum`: Long = 300_000_000L
    var `divideInteger-memory-arguments-slope`: Long = 300_000_000L
    var `encodeUtf8-cpu-arguments-intercept`: Long = 300_000_000L
    var `encodeUtf8-cpu-arguments-slope`: Long = 300_000_000L
    var `encodeUtf8-memory-arguments-intercept`: Long = 300_000_000L
    var `encodeUtf8-memory-arguments-slope`: Long = 300_000_000L
    var `equalsByteString-cpu-arguments-constant`: Long = 300_000_000L
    var `equalsByteString-cpu-arguments-intercept`: Long = 300_000_000L
    var `equalsByteString-cpu-arguments-slope`: Long = 300_000_000L
    var `equalsByteString-memory-arguments`: Long = 300_000_000L
    var `equalsData-cpu-arguments-intercept`: Long = 300_000_000L
    var `equalsData-cpu-arguments-slope`: Long = 300_000_000L
    var `equalsData-memory-arguments`: Long = 300_000_000L
    var `equalsInteger-cpu-arguments-intercept`: Long = 300_000_000L
    var `equalsInteger-cpu-arguments-slope`: Long = 300_000_000L
    var `equalsInteger-memory-arguments`: Long = 300_000_000L
    var `equalsString-cpu-arguments-constant`: Long = 300_000_000L
    var `equalsString-cpu-arguments-intercept`: Long = 300_000_000L
    var `equalsString-cpu-arguments-slope`: Long = 300_000_000L
    var `equalsString-memory-arguments`: Long = 300_000_000L
    var `fstPair-cpu-arguments`: Long = 300_000_000L
    var `fstPair-memory-arguments`: Long = 300_000_000L
    var `headList-cpu-arguments`: Long = 300_000_000L
    var `headList-memory-arguments`: Long = 300_000_000L
    var `iData-cpu-arguments`: Long = 300_000_000L
    var `iData-memory-arguments`: Long = 300_000_000L
    var `ifThenElse-cpu-arguments`: Long = 300_000_000L
    var `ifThenElse-memory-arguments`: Long = 300_000_000L
    var `indexByteString-cpu-arguments`: Long = 300_000_000L
    var `indexByteString-memory-arguments`: Long = 300_000_000L
    var `lengthOfByteString-cpu-arguments`: Long = 300_000_000L
    var `lengthOfByteString-memory-arguments`: Long = 300_000_000L
    var `lessThanByteString-cpu-arguments-intercept`: Long = 300_000_000L
    var `lessThanByteString-cpu-arguments-slope`: Long = 300_000_000L
    var `lessThanByteString-memory-arguments`: Long = 300_000_000L
    var `lessThanEqualsByteString-cpu-arguments-intercept`: Long = 300_000_000L
    var `lessThanEqualsByteString-cpu-arguments-slope`: Long = 300_000_000L
    var `lessThanEqualsByteString-memory-arguments`: Long = 300_000_000L
    var `lessThanEqualsInteger-cpu-arguments-intercept`: Long = 300_000_000L
    var `lessThanEqualsInteger-cpu-arguments-slope`: Long = 300_000_000L
    var `lessThanEqualsInteger-memory-arguments`: Long = 300_000_000L
    var `lessThanInteger-cpu-arguments-intercept`: Long = 300_000_000L
    var `lessThanInteger-cpu-arguments-slope`: Long = 300_000_000L
    var `lessThanInteger-memory-arguments`: Long = 300_000_000L
    var `listData-cpu-arguments`: Long = 300_000_000L
    var `listData-memory-arguments`: Long = 300_000_000L
    var `mapData-cpu-arguments`: Long = 300_000_000L
    var `mapData-memory-arguments`: Long = 300_000_000L
    var `mkCons-cpu-arguments`: Long = 300_000_000L
    var `mkCons-memory-arguments`: Long = 300_000_000L
    var `mkNilData-cpu-arguments`: Long = 300_000_000L
    var `mkNilData-memory-arguments`: Long = 300_000_000L
    var `mkNilPairData-cpu-arguments`: Long = 300_000_000L
    var `mkNilPairData-memory-arguments`: Long = 300_000_000L
    var `mkPairData-cpu-arguments`: Long = 300_000_000L
    var `mkPairData-memory-arguments`: Long = 300_000_000L
    var `modInteger-cpu-arguments-constant`: Long = 300_000_000L
    var `modInteger-cpu-arguments-model-arguments-intercept`: Long = 300_000_000L
    var `modInteger-cpu-arguments-model-arguments-slope`: Long = 300_000_000L
    var `modInteger-memory-arguments-intercept`: Long = 300_000_000L
    var `modInteger-memory-arguments-minimum`: Long = 300_000_000L
    var `modInteger-memory-arguments-slope`: Long = 300_000_000L
    var `multiplyInteger-cpu-arguments-intercept`: Long = 300_000_000L
    var `multiplyInteger-cpu-arguments-slope`: Long = 300_000_000L
    var `multiplyInteger-memory-arguments-intercept`: Long = 300_000_000L
    var `multiplyInteger-memory-arguments-slope`: Long = 300_000_000L
    var `nullList-cpu-arguments`: Long = 300_000_000L
    var `nullList-memory-arguments`: Long = 300_000_000L
    var `quotientInteger-cpu-arguments-constant`: Long = 300_000_000L
    var `quotientInteger-cpu-arguments-model-arguments-intercept`: Long = 300_000_000L
    var `quotientInteger-cpu-arguments-model-arguments-slope`: Long = 300_000_000L
    var `quotientInteger-memory-arguments-intercept`: Long = 300_000_000L
    var `quotientInteger-memory-arguments-minimum`: Long = 300_000_000L
    var `quotientInteger-memory-arguments-slope`: Long = 300_000_000L
    var `remainderInteger-cpu-arguments-constant`: Long = 300_000_000L
    var `remainderInteger-cpu-arguments-model-arguments-intercept`: Long = 300_000_000L
    var `remainderInteger-cpu-arguments-model-arguments-slope`: Long = 300_000_000L
    var `remainderInteger-memory-arguments-intercept`: Long = 300_000_000L
    var `remainderInteger-memory-arguments-minimum`: Long = 300_000_000L
    var `remainderInteger-memory-arguments-slope`: Long = 300_000_000L
    var `sha2_256-cpu-arguments-intercept`: Long = 300_000_000L
    var `sha2_256-cpu-arguments-slope`: Long = 300_000_000L
    var `sha2_256-memory-arguments`: Long = 300_000_000L
    var `sha3_256-cpu-arguments-intercept`: Long = 300_000_000L
    var `sha3_256-cpu-arguments-slope`: Long = 300_000_000L
    var `sha3_256-memory-arguments`: Long = 300_000_000L
    var `sliceByteString-cpu-arguments-intercept`: Long = 300_000_000L
    var `sliceByteString-cpu-arguments-slope`: Long = 300_000_000L
    var `sliceByteString-memory-arguments-intercept`: Long = 300_000_000L
    var `sliceByteString-memory-arguments-slope`: Long = 300_000_000L
    var `sndPair-cpu-arguments`: Long = 300_000_000L
    var `sndPair-memory-arguments`: Long = 300_000_000L
    var `subtractInteger-cpu-arguments-intercept`: Long = 300_000_000L
    var `subtractInteger-cpu-arguments-slope`: Long = 300_000_000L
    var `subtractInteger-memory-arguments-intercept`: Long = 300_000_000L
    var `subtractInteger-memory-arguments-slope`: Long = 300_000_000L
    var `tailList-cpu-arguments`: Long = 300_000_000L
    var `tailList-memory-arguments`: Long = 300_000_000L
    var `trace-cpu-arguments`: Long = 300_000_000L
    var `trace-memory-arguments`: Long = 300_000_000L
    var `unBData-cpu-arguments`: Long = 300_000_000L
    var `unBData-memory-arguments`: Long = 300_000_000L
    var `unConstrData-cpu-arguments`: Long = 300_000_000L
    var `unConstrData-memory-arguments`: Long = 300_000_000L
    var `unIData-cpu-arguments`: Long = 300_000_000L
    var `unIData-memory-arguments`: Long = 300_000_000L
    var `unListData-cpu-arguments`: Long = 300_000_000L
    var `unListData-memory-arguments`: Long = 300_000_000L
    var `unMapData-cpu-arguments`: Long = 300_000_000L
    var `unMapData-memory-arguments`: Long = 300_000_000L
    var `verifyEd25519Signature-cpu-arguments-intercept`: Long = 300_000_000L
    var `verifyEd25519Signature-cpu-arguments-slope`: Long = 300_000_000L
    var `verifyEd25519Signature-memory-arguments`: Long = 300_000_000L
    def toJson: String = write(this)

    // Not available in Plutus V1
    def `divideInteger-cpu-arguments-c00`: Long = 300_000_000L
    def `divideInteger-cpu-arguments-c01`: Long = 300_000_000L
    def `divideInteger-cpu-arguments-c02`: Long = 300_000_000L
    def `divideInteger-cpu-arguments-c10`: Long = 300_000_000L
    def `divideInteger-cpu-arguments-c11`: Long = 300_000_000L
    def `divideInteger-cpu-arguments-c20`: Long = 300_000_000L
    def `divideInteger-cpu-arguments-minimum`: Long = 300_000_000L
    def `modInteger-cpu-arguments-model-arguments-c00`: Long = 300_000_000L
    def `modInteger-cpu-arguments-model-arguments-c01`: Long = 300_000_000L
    def `modInteger-cpu-arguments-model-arguments-c02`: Long = 300_000_000L
    def `modInteger-cpu-arguments-model-arguments-c10`: Long = 300_000_000L
    def `modInteger-cpu-arguments-model-arguments-c11`: Long = 300_000_000L
    def `modInteger-cpu-arguments-model-arguments-c20`: Long = 300_000_000L
    def `modInteger-cpu-arguments-model-arguments-minimum`: Long = 300_000_000L
    def `quotientInteger-cpu-arguments-model-arguments-c00`: Long = 300_000_000L
    def `quotientInteger-cpu-arguments-model-arguments-c01`: Long = 300_000_000L
    def `quotientInteger-cpu-arguments-model-arguments-c02`: Long = 300_000_000L
    def `quotientInteger-cpu-arguments-model-arguments-c10`: Long = 300_000_000L
    def `quotientInteger-cpu-arguments-model-arguments-c11`: Long = 300_000_000L
    def `quotientInteger-cpu-arguments-model-arguments-c20`: Long = 300_000_000L
    def `quotientInteger-cpu-arguments-model-arguments-minimum`: Long = 300_000_000L
    def `remainderInteger-cpu-arguments-model-arguments-c00`: Long = 300_000_000L
    def `remainderInteger-cpu-arguments-model-arguments-c01`: Long = 300_000_000L
    def `remainderInteger-cpu-arguments-model-arguments-c02`: Long = 300_000_000L
    def `remainderInteger-cpu-arguments-model-arguments-c10`: Long = 300_000_000L
    def `remainderInteger-cpu-arguments-model-arguments-c11`: Long = 300_000_000L
    def `remainderInteger-cpu-arguments-model-arguments-c20`: Long = 300_000_000L
    def `remainderInteger-cpu-arguments-model-arguments-minimum`: Long = 300_000_000L
    def `serialiseData-cpu-arguments-intercept`: Long = 300_000_000L
    def `serialiseData-cpu-arguments-slope`: Long = 300_000_000L
    def `serialiseData-memory-arguments-intercept`: Long = 300_000_000L
    def `serialiseData-memory-arguments-slope`: Long = 300_000_000L
    def `verifyEcdsaSecp256k1Signature-cpu-arguments`: Long = 300_000_000L
    def `verifyEcdsaSecp256k1Signature-memory-arguments`: Long = 300_000_000L
    def `verifySchnorrSecp256k1Signature-cpu-arguments-intercept`: Long = 300_000_000L
    def `verifySchnorrSecp256k1Signature-cpu-arguments-slope`: Long = 300_000_000L
    def `verifySchnorrSecp256k1Signature-memory-arguments`: Long = 300_000_000L
    def `cekConstrCost-exBudgetCPU`: Long = 300_000_000L
    def `cekConstrCost-exBudgetMemory`: Long = 300_000_000L
    def `cekCaseCost-exBudgetCPU`: Long = 300_000_000L
    def `cekCaseCost-exBudgetMemory`: Long = 300_000_000L
    def `bls12_381_G1_add-cpu-arguments`: Long = 300_000_000L
    def `bls12_381_G1_add-memory-arguments`: Long = 300_000_000L
    def `bls12_381_G1_compress-cpu-arguments`: Long = 300_000_000L
    def `bls12_381_G1_compress-memory-arguments`: Long = 300_000_000L
    def `bls12_381_G1_equal-cpu-arguments`: Long = 300_000_000L
    def `bls12_381_G1_equal-memory-arguments`: Long = 300_000_000L
    def `bls12_381_G1_hashToGroup-cpu-arguments-intercept`: Long = 300_000_000L
    def `bls12_381_G1_hashToGroup-cpu-arguments-slope`: Long = 300_000_000L
    def `bls12_381_G1_hashToGroup-memory-arguments`: Long = 300_000_000L
    def `bls12_381_G1_neg-cpu-arguments`: Long = 300_000_000L
    def `bls12_381_G1_neg-memory-arguments`: Long = 300_000_000L
    def `bls12_381_G1_scalarMul-cpu-arguments-intercept`: Long = 300_000_000L
    def `bls12_381_G1_scalarMul-cpu-arguments-slope`: Long = 300_000_000L
    def `bls12_381_G1_scalarMul-memory-arguments`: Long = 300_000_000L
    def `bls12_381_G1_uncompress-cpu-arguments`: Long = 300_000_000L
    def `bls12_381_G1_uncompress-memory-arguments`: Long = 300_000_000L
    def `bls12_381_G2_add-cpu-arguments`: Long = 300_000_000L
    def `bls12_381_G2_add-memory-arguments`: Long = 300_000_000L
    def `bls12_381_G2_compress-cpu-arguments`: Long = 300_000_000L
    def `bls12_381_G2_compress-memory-arguments`: Long = 300_000_000L
    def `bls12_381_G2_equal-cpu-arguments`: Long = 300_000_000L
    def `bls12_381_G2_equal-memory-arguments`: Long = 300_000_000L
    def `bls12_381_G2_hashToGroup-cpu-arguments-intercept`: Long = 300_000_000L
    def `bls12_381_G2_hashToGroup-cpu-arguments-slope`: Long = 300_000_000L
    def `bls12_381_G2_hashToGroup-memory-arguments`: Long = 300_000_000L
    def `bls12_381_G2_neg-cpu-arguments`: Long = 300_000_000L
    def `bls12_381_G2_neg-memory-arguments`: Long = 300_000_000L
    def `bls12_381_G2_scalarMul-cpu-arguments-intercept`: Long = 300_000_000L
    def `bls12_381_G2_scalarMul-cpu-arguments-slope`: Long = 300_000_000L
    def `bls12_381_G2_scalarMul-memory-arguments`: Long = 300_000_000L
    def `bls12_381_G2_uncompress-cpu-arguments`: Long = 300_000_000L
    def `bls12_381_G2_uncompress-memory-arguments`: Long = 300_000_000L
    def `bls12_381_finalVerify-cpu-arguments`: Long = 300_000_000L
    def `bls12_381_finalVerify-memory-arguments`: Long = 300_000_000L
    def `bls12_381_millerLoop-cpu-arguments`: Long = 300_000_000L
    def `bls12_381_millerLoop-memory-arguments`: Long = 300_000_000L
    def `bls12_381_mulMlResult-cpu-arguments`: Long = 300_000_000L
    def `bls12_381_mulMlResult-memory-arguments`: Long = 300_000_000L
    def `keccak_256-cpu-arguments-intercept`: Long = 300_000_000L
    def `keccak_256-cpu-arguments-slope`: Long = 300_000_000L
    def `keccak_256-memory-arguments`: Long = 300_000_000L
    def `blake2b_224-cpu-arguments-intercept`: Long = 300_000_000L
    def `blake2b_224-cpu-arguments-slope`: Long = 300_000_000L
    def `blake2b_224-memory-arguments`: Long = 300_000_000L
    def `integerToByteString-cpu-arguments-c0`: Long = 300_000_000L
    def `integerToByteString-cpu-arguments-c1`: Long = 300_000_000L
    def `integerToByteString-cpu-arguments-c2`: Long = 300_000_000L
    def `integerToByteString-memory-arguments-intercept`: Long = 300_000_000L
    def `integerToByteString-memory-arguments-slope`: Long = 300_000_000L
    def `byteStringToInteger-cpu-arguments-c0`: Long = 300_000_000L
    def `byteStringToInteger-cpu-arguments-c1`: Long = 300_000_000L
    def `byteStringToInteger-cpu-arguments-c2`: Long = 300_000_000L
    def `byteStringToInteger-memory-arguments-intercept`: Long = 300_000_000L
    def `byteStringToInteger-memory-arguments-slope`: Long = 300_000_000L
    def `andByteString-cpu-arguments-intercept`: Long = 300_000_000L
    def `andByteString-cpu-arguments-slope1`: Long = 300_000_000L
    def `andByteString-cpu-arguments-slope2`: Long = 300_000_000L
    def `andByteString-memory-arguments-intercept`: Long = 300_000_000L
    def `andByteString-memory-arguments-slope`: Long = 300_000_000L
    def `orByteString-cpu-arguments-intercept`: Long = 300_000_000L
    def `orByteString-cpu-arguments-slope1`: Long = 300_000_000L
    def `orByteString-cpu-arguments-slope2`: Long = 300_000_000L
    def `orByteString-memory-arguments-intercept`: Long = 300_000_000L
    def `orByteString-memory-arguments-slope`: Long = 300_000_000L
    def `xorByteString-cpu-arguments-intercept`: Long = 300_000_000L
    def `xorByteString-cpu-arguments-slope1`: Long = 300_000_000L
    def `xorByteString-cpu-arguments-slope2`: Long = 300_000_000L
    def `xorByteString-memory-arguments-intercept`: Long = 300_000_000L
    def `xorByteString-memory-arguments-slope`: Long = 300_000_000L
    def `complementByteString-cpu-arguments-intercept`: Long = 300_000_000L
    def `complementByteString-cpu-arguments-slope`: Long = 300_000_000L
    def `complementByteString-memory-arguments-intercept`: Long = 300_000_000L
    def `complementByteString-memory-arguments-slope`: Long = 300_000_000L
    def `readBit-cpu-arguments`: Long = 300_000_000L
    def `readBit-memory-arguments`: Long = 300_000_000L
    def `writeBits-cpu-arguments-intercept`: Long = 300_000_000L
    def `writeBits-cpu-arguments-slope`: Long = 300_000_000L
    def `writeBits-memory-arguments-intercept`: Long = 300_000_000L
    def `writeBits-memory-arguments-slope`: Long = 300_000_000L
    def `replicateByte-cpu-arguments-intercept`: Long = 300_000_000L
    def `replicateByte-cpu-arguments-slope`: Long = 300_000_000L
    def `replicateByte-memory-arguments-intercept`: Long = 300_000_000L
    def `replicateByte-memory-arguments-slope`: Long = 300_000_000L
    def `shiftByteString-cpu-arguments-intercept`: Long = 300_000_000L
    def `shiftByteString-cpu-arguments-slope`: Long = 300_000_000L
    def `shiftByteString-memory-arguments-intercept`: Long = 300_000_000L
    def `shiftByteString-memory-arguments-slope`: Long = 300_000_000L
    def `rotateByteString-cpu-arguments-intercept`: Long = 300_000_000L
    def `rotateByteString-cpu-arguments-slope`: Long = 300_000_000L
    def `rotateByteString-memory-arguments-intercept`: Long = 300_000_000L
    def `rotateByteString-memory-arguments-slope`: Long = 300_000_000L
    def `countSetBits-cpu-arguments-intercept`: Long = 300_000_000L
    def `countSetBits-cpu-arguments-slope`: Long = 300_000_000L
    def `countSetBits-memory-arguments`: Long = 300_000_000L
    def `findFirstSetBit-cpu-arguments-intercept`: Long = 300_000_000L
    def `findFirstSetBit-cpu-arguments-slope`: Long = 300_000_000L
    def `findFirstSetBit-memory-arguments`: Long = 300_000_000L
    def `ripemd_160-cpu-arguments-intercept`: Long = 300_000_000L
    def `ripemd_160-cpu-arguments-slope`: Long = 300_000_000L
    def `ripemd_160-memory-arguments`: Long = 300_000_000L
}

/** Plutus V2 cost model parameters.
  *
  * The names of the fields are taken from
  * [[https://github.com/input-output-hk/plutus/blob/1.40.0.0/plutus-ledger-api/src/PlutusLedgerApi/V2/ParamName.hs]]
  * and Blockfrost Protocol Parameters JSON uses these names as well in
  * `blockfrost-params-epoch-544.json`
  *
  * But what's really important is the order of the fields because that's the order of the
  * parameters in the protocol parameters array.
  */
class PlutusV2Params extends PlutusParams {
    var `addInteger-cpu-arguments-intercept`: Long = 300_000_000L
    var `addInteger-cpu-arguments-slope`: Long = 300_000_000L
    var `addInteger-memory-arguments-intercept`: Long = 300_000_000L
    var `addInteger-memory-arguments-slope`: Long = 300_000_000L
    var `appendByteString-cpu-arguments-intercept`: Long = 300_000_000L
    var `appendByteString-cpu-arguments-slope`: Long = 300_000_000L
    var `appendByteString-memory-arguments-intercept`: Long = 300_000_000L
    var `appendByteString-memory-arguments-slope`: Long = 300_000_000L
    var `appendString-cpu-arguments-intercept`: Long = 300_000_000L
    var `appendString-cpu-arguments-slope`: Long = 300_000_000L
    var `appendString-memory-arguments-intercept`: Long = 300_000_000L
    var `appendString-memory-arguments-slope`: Long = 300_000_000L
    var `bData-cpu-arguments`: Long = 300_000_000L
    var `bData-memory-arguments`: Long = 300_000_000L
    var `blake2b_256-cpu-arguments-intercept`: Long = 300_000_000L
    var `blake2b_256-cpu-arguments-slope`: Long = 300_000_000L
    var `blake2b_256-memory-arguments`: Long = 300_000_000L
    var `cekApplyCost-exBudgetCPU`: Long = 300_000_000L
    var `cekApplyCost-exBudgetMemory`: Long = 300_000_000L
    var `cekBuiltinCost-exBudgetCPU`: Long = 300_000_000L
    var `cekBuiltinCost-exBudgetMemory`: Long = 300_000_000L
    var `cekConstCost-exBudgetCPU`: Long = 300_000_000L
    var `cekConstCost-exBudgetMemory`: Long = 300_000_000L
    var `cekDelayCost-exBudgetCPU`: Long = 300_000_000L
    var `cekDelayCost-exBudgetMemory`: Long = 300_000_000L
    var `cekForceCost-exBudgetCPU`: Long = 300_000_000L
    var `cekForceCost-exBudgetMemory`: Long = 300_000_000L
    var `cekLamCost-exBudgetCPU`: Long = 300_000_000L
    var `cekLamCost-exBudgetMemory`: Long = 300_000_000L
    var `cekStartupCost-exBudgetCPU`: Long = 300_000_000L
    var `cekStartupCost-exBudgetMemory`: Long = 300_000_000L
    var `cekVarCost-exBudgetCPU`: Long = 300_000_000L
    var `cekVarCost-exBudgetMemory`: Long = 300_000_000L
    var `chooseData-cpu-arguments`: Long = 300_000_000L
    var `chooseData-memory-arguments`: Long = 300_000_000L
    var `chooseList-cpu-arguments`: Long = 300_000_000L
    var `chooseList-memory-arguments`: Long = 300_000_000L
    var `chooseUnit-cpu-arguments`: Long = 300_000_000L
    var `chooseUnit-memory-arguments`: Long = 300_000_000L
    var `consByteString-cpu-arguments-intercept`: Long = 300_000_000L
    var `consByteString-cpu-arguments-slope`: Long = 300_000_000L
    var `consByteString-memory-arguments-intercept`: Long = 300_000_000L
    var `consByteString-memory-arguments-slope`: Long = 300_000_000L
    var `constrData-cpu-arguments`: Long = 300_000_000L
    var `constrData-memory-arguments`: Long = 300_000_000L
    var `decodeUtf8-cpu-arguments-intercept`: Long = 300_000_000L
    var `decodeUtf8-cpu-arguments-slope`: Long = 300_000_000L
    var `decodeUtf8-memory-arguments-intercept`: Long = 300_000_000L
    var `decodeUtf8-memory-arguments-slope`: Long = 300_000_000L
    var `divideInteger-cpu-arguments-constant`: Long = 300_000_000L
    var `divideInteger-cpu-arguments-model-arguments-intercept`: Long = 300_000_000L
    var `divideInteger-cpu-arguments-model-arguments-slope`: Long = 300_000_000L
    var `divideInteger-memory-arguments-intercept`: Long = 300_000_000L
    var `divideInteger-memory-arguments-minimum`: Long = 300_000_000L
    var `divideInteger-memory-arguments-slope`: Long = 300_000_000L
    var `encodeUtf8-cpu-arguments-intercept`: Long = 300_000_000L
    var `encodeUtf8-cpu-arguments-slope`: Long = 300_000_000L
    var `encodeUtf8-memory-arguments-intercept`: Long = 300_000_000L
    var `encodeUtf8-memory-arguments-slope`: Long = 300_000_000L
    var `equalsByteString-cpu-arguments-constant`: Long = 300_000_000L
    var `equalsByteString-cpu-arguments-intercept`: Long = 300_000_000L
    var `equalsByteString-cpu-arguments-slope`: Long = 300_000_000L
    var `equalsByteString-memory-arguments`: Long = 300_000_000L
    var `equalsData-cpu-arguments-intercept`: Long = 300_000_000L
    var `equalsData-cpu-arguments-slope`: Long = 300_000_000L
    var `equalsData-memory-arguments`: Long = 300_000_000L
    var `equalsInteger-cpu-arguments-intercept`: Long = 300_000_000L
    var `equalsInteger-cpu-arguments-slope`: Long = 300_000_000L
    var `equalsInteger-memory-arguments`: Long = 300_000_000L
    var `equalsString-cpu-arguments-constant`: Long = 300_000_000L
    var `equalsString-cpu-arguments-intercept`: Long = 300_000_000L
    var `equalsString-cpu-arguments-slope`: Long = 300_000_000L
    var `equalsString-memory-arguments`: Long = 300_000_000L
    var `fstPair-cpu-arguments`: Long = 300_000_000L
    var `fstPair-memory-arguments`: Long = 300_000_000L
    var `headList-cpu-arguments`: Long = 300_000_000L
    var `headList-memory-arguments`: Long = 300_000_000L
    var `iData-cpu-arguments`: Long = 300_000_000L
    var `iData-memory-arguments`: Long = 300_000_000L
    var `ifThenElse-cpu-arguments`: Long = 300_000_000L
    var `ifThenElse-memory-arguments`: Long = 300_000_000L
    var `indexByteString-cpu-arguments`: Long = 300_000_000L
    var `indexByteString-memory-arguments`: Long = 300_000_000L
    var `lengthOfByteString-cpu-arguments`: Long = 300_000_000L
    var `lengthOfByteString-memory-arguments`: Long = 300_000_000L
    var `lessThanByteString-cpu-arguments-intercept`: Long = 300_000_000L
    var `lessThanByteString-cpu-arguments-slope`: Long = 300_000_000L
    var `lessThanByteString-memory-arguments`: Long = 300_000_000L
    var `lessThanEqualsByteString-cpu-arguments-intercept`: Long = 300_000_000L
    var `lessThanEqualsByteString-cpu-arguments-slope`: Long = 300_000_000L
    var `lessThanEqualsByteString-memory-arguments`: Long = 300_000_000L
    var `lessThanEqualsInteger-cpu-arguments-intercept`: Long = 300_000_000L
    var `lessThanEqualsInteger-cpu-arguments-slope`: Long = 300_000_000L
    var `lessThanEqualsInteger-memory-arguments`: Long = 300_000_000L
    var `lessThanInteger-cpu-arguments-intercept`: Long = 300_000_000L
    var `lessThanInteger-cpu-arguments-slope`: Long = 300_000_000L
    var `lessThanInteger-memory-arguments`: Long = 300_000_000L
    var `listData-cpu-arguments`: Long = 300_000_000L
    var `listData-memory-arguments`: Long = 300_000_000L
    var `mapData-cpu-arguments`: Long = 300_000_000L
    var `mapData-memory-arguments`: Long = 300_000_000L
    var `mkCons-cpu-arguments`: Long = 300_000_000L
    var `mkCons-memory-arguments`: Long = 300_000_000L
    var `mkNilData-cpu-arguments`: Long = 300_000_000L
    var `mkNilData-memory-arguments`: Long = 300_000_000L
    var `mkNilPairData-cpu-arguments`: Long = 300_000_000L
    var `mkNilPairData-memory-arguments`: Long = 300_000_000L
    var `mkPairData-cpu-arguments`: Long = 300_000_000L
    var `mkPairData-memory-arguments`: Long = 300_000_000L
    var `modInteger-cpu-arguments-constant`: Long = 300_000_000L
    var `modInteger-cpu-arguments-model-arguments-intercept`: Long = 300_000_000L
    var `modInteger-cpu-arguments-model-arguments-slope`: Long = 300_000_000L
    var `modInteger-memory-arguments-intercept`: Long = 300_000_000L
    var `modInteger-memory-arguments-minimum`: Long = 300_000_000L
    var `modInteger-memory-arguments-slope`: Long = 300_000_000L
    var `multiplyInteger-cpu-arguments-intercept`: Long = 300_000_000L
    var `multiplyInteger-cpu-arguments-slope`: Long = 300_000_000L
    var `multiplyInteger-memory-arguments-intercept`: Long = 300_000_000L
    var `multiplyInteger-memory-arguments-slope`: Long = 300_000_000L
    var `nullList-cpu-arguments`: Long = 300_000_000L
    var `nullList-memory-arguments`: Long = 300_000_000L
    var `quotientInteger-cpu-arguments-constant`: Long = 300_000_000L
    var `quotientInteger-cpu-arguments-model-arguments-intercept`: Long = 300_000_000L
    var `quotientInteger-cpu-arguments-model-arguments-slope`: Long = 300_000_000L
    var `quotientInteger-memory-arguments-intercept`: Long = 300_000_000L
    var `quotientInteger-memory-arguments-minimum`: Long = 300_000_000L
    var `quotientInteger-memory-arguments-slope`: Long = 300_000_000L
    var `remainderInteger-cpu-arguments-constant`: Long = 300_000_000L
    var `remainderInteger-cpu-arguments-model-arguments-intercept`: Long = 300_000_000L
    var `remainderInteger-cpu-arguments-model-arguments-slope`: Long = 300_000_000L
    var `remainderInteger-memory-arguments-intercept`: Long = 300_000_000L
    var `remainderInteger-memory-arguments-minimum`: Long = 300_000_000L
    var `remainderInteger-memory-arguments-slope`: Long = 300_000_000L
    var `serialiseData-cpu-arguments-intercept`: Long = 300_000_000L
    var `serialiseData-cpu-arguments-slope`: Long = 300_000_000L
    var `serialiseData-memory-arguments-intercept`: Long = 300_000_000L
    var `serialiseData-memory-arguments-slope`: Long = 300_000_000L
    var `sha2_256-cpu-arguments-intercept`: Long = 300_000_000L
    var `sha2_256-cpu-arguments-slope`: Long = 300_000_000L
    var `sha2_256-memory-arguments`: Long = 300_000_000L
    var `sha3_256-cpu-arguments-intercept`: Long = 300_000_000L
    var `sha3_256-cpu-arguments-slope`: Long = 300_000_000L
    var `sha3_256-memory-arguments`: Long = 300_000_000L
    var `sliceByteString-cpu-arguments-intercept`: Long = 300_000_000L
    var `sliceByteString-cpu-arguments-slope`: Long = 300_000_000L
    var `sliceByteString-memory-arguments-intercept`: Long = 300_000_000L
    var `sliceByteString-memory-arguments-slope`: Long = 300_000_000L
    var `sndPair-cpu-arguments`: Long = 300_000_000L
    var `sndPair-memory-arguments`: Long = 300_000_000L
    var `subtractInteger-cpu-arguments-intercept`: Long = 300_000_000L
    var `subtractInteger-cpu-arguments-slope`: Long = 300_000_000L
    var `subtractInteger-memory-arguments-intercept`: Long = 300_000_000L
    var `subtractInteger-memory-arguments-slope`: Long = 300_000_000L
    var `tailList-cpu-arguments`: Long = 300_000_000L
    var `tailList-memory-arguments`: Long = 300_000_000L
    var `trace-cpu-arguments`: Long = 300_000_000L
    var `trace-memory-arguments`: Long = 300_000_000L
    var `unBData-cpu-arguments`: Long = 300_000_000L
    var `unBData-memory-arguments`: Long = 300_000_000L
    var `unConstrData-cpu-arguments`: Long = 300_000_000L
    var `unConstrData-memory-arguments`: Long = 300_000_000L
    var `unIData-cpu-arguments`: Long = 300_000_000L
    var `unIData-memory-arguments`: Long = 300_000_000L
    var `unListData-cpu-arguments`: Long = 300_000_000L
    var `unListData-memory-arguments`: Long = 300_000_000L
    var `unMapData-cpu-arguments`: Long = 300_000_000L
    var `unMapData-memory-arguments`: Long = 300_000_000L
    var `verifyEcdsaSecp256k1Signature-cpu-arguments`: Long = 300_000_000L
    var `verifyEcdsaSecp256k1Signature-memory-arguments`: Long = 300_000_000L
    var `verifyEd25519Signature-cpu-arguments-intercept`: Long = 300_000_000L
    var `verifyEd25519Signature-cpu-arguments-slope`: Long = 300_000_000L
    var `verifyEd25519Signature-memory-arguments`: Long = 300_000_000L
    var `verifySchnorrSecp256k1Signature-cpu-arguments-intercept`: Long = 300_000_000L
    var `verifySchnorrSecp256k1Signature-cpu-arguments-slope`: Long = 300_000_000L
    var `verifySchnorrSecp256k1Signature-memory-arguments`: Long = 300_000_000L
    var `integerToByteString-cpu-arguments-c0`: Long = 300_000_000L
    var `integerToByteString-cpu-arguments-c1`: Long = 300_000_000L
    var `integerToByteString-cpu-arguments-c2`: Long = 300_000_000L
    var `integerToByteString-memory-arguments-intercept`: Long = 300_000_000L
    var `integerToByteString-memory-arguments-slope`: Long = 300_000_000L
    var `byteStringToInteger-cpu-arguments-c0`: Long = 300_000_000L
    var `byteStringToInteger-cpu-arguments-c1`: Long = 300_000_000L
    var `byteStringToInteger-cpu-arguments-c2`: Long = 300_000_000L
    var `byteStringToInteger-memory-arguments-intercept`: Long = 300_000_000L
    var `byteStringToInteger-memory-arguments-slope`: Long = 300_000_000L
    def toJson: String = write(this)

    // Not available in Plutus V2
    def `divideInteger-cpu-arguments-c00`: Long = 300_000_000L
    def `divideInteger-cpu-arguments-c01`: Long = 300_000_000L
    def `divideInteger-cpu-arguments-c02`: Long = 300_000_000L
    def `divideInteger-cpu-arguments-c10`: Long = 300_000_000L
    def `divideInteger-cpu-arguments-c11`: Long = 300_000_000L
    def `divideInteger-cpu-arguments-c20`: Long = 300_000_000L
    def `divideInteger-cpu-arguments-minimum`: Long = 300_000_000L
    def `modInteger-cpu-arguments-model-arguments-c00`: Long = 300_000_000L
    def `modInteger-cpu-arguments-model-arguments-c01`: Long = 300_000_000L
    def `modInteger-cpu-arguments-model-arguments-c02`: Long = 300_000_000L
    def `modInteger-cpu-arguments-model-arguments-c10`: Long = 300_000_000L
    def `modInteger-cpu-arguments-model-arguments-c11`: Long = 300_000_000L
    def `modInteger-cpu-arguments-model-arguments-c20`: Long = 300_000_000L
    def `modInteger-cpu-arguments-model-arguments-minimum`: Long = 300_000_000L
    def `quotientInteger-cpu-arguments-model-arguments-c00`: Long = 300_000_000L
    def `quotientInteger-cpu-arguments-model-arguments-c01`: Long = 300_000_000L
    def `quotientInteger-cpu-arguments-model-arguments-c02`: Long = 300_000_000L
    def `quotientInteger-cpu-arguments-model-arguments-c10`: Long = 300_000_000L
    def `quotientInteger-cpu-arguments-model-arguments-c11`: Long = 300_000_000L
    def `quotientInteger-cpu-arguments-model-arguments-c20`: Long = 300_000_000L
    def `quotientInteger-cpu-arguments-model-arguments-minimum`: Long = 300_000_000L
    def `remainderInteger-cpu-arguments-model-arguments-c00`: Long = 300_000_000L
    def `remainderInteger-cpu-arguments-model-arguments-c01`: Long = 300_000_000L
    def `remainderInteger-cpu-arguments-model-arguments-c02`: Long = 300_000_000L
    def `remainderInteger-cpu-arguments-model-arguments-c10`: Long = 300_000_000L
    def `remainderInteger-cpu-arguments-model-arguments-c11`: Long = 300_000_000L
    def `remainderInteger-cpu-arguments-model-arguments-c20`: Long = 300_000_000L
    def `remainderInteger-cpu-arguments-model-arguments-minimum`: Long = 300_000_000L
    def `cekConstrCost-exBudgetCPU`: Long = 300_000_000L
    def `cekConstrCost-exBudgetMemory`: Long = 300_000_000L
    def `cekCaseCost-exBudgetCPU`: Long = 300_000_000L
    def `cekCaseCost-exBudgetMemory`: Long = 300_000_000L
    def `bls12_381_G1_add-cpu-arguments`: Long = 300_000_000L
    def `bls12_381_G1_add-memory-arguments`: Long = 300_000_000L
    def `bls12_381_G1_compress-cpu-arguments`: Long = 300_000_000L
    def `bls12_381_G1_compress-memory-arguments`: Long = 300_000_000L
    def `bls12_381_G1_equal-cpu-arguments`: Long = 300_000_000L
    def `bls12_381_G1_equal-memory-arguments`: Long = 300_000_000L
    def `bls12_381_G1_hashToGroup-cpu-arguments-intercept`: Long = 300_000_000L
    def `bls12_381_G1_hashToGroup-cpu-arguments-slope`: Long = 300_000_000L
    def `bls12_381_G1_hashToGroup-memory-arguments`: Long = 300_000_000L
    def `bls12_381_G1_neg-cpu-arguments`: Long = 300_000_000L
    def `bls12_381_G1_neg-memory-arguments`: Long = 300_000_000L
    def `bls12_381_G1_scalarMul-cpu-arguments-intercept`: Long = 300_000_000L
    def `bls12_381_G1_scalarMul-cpu-arguments-slope`: Long = 300_000_000L
    def `bls12_381_G1_scalarMul-memory-arguments`: Long = 300_000_000L
    def `bls12_381_G1_uncompress-cpu-arguments`: Long = 300_000_000L
    def `bls12_381_G1_uncompress-memory-arguments`: Long = 300_000_000L
    def `bls12_381_G2_add-cpu-arguments`: Long = 300_000_000L
    def `bls12_381_G2_add-memory-arguments`: Long = 300_000_000L
    def `bls12_381_G2_compress-cpu-arguments`: Long = 300_000_000L
    def `bls12_381_G2_compress-memory-arguments`: Long = 300_000_000L
    def `bls12_381_G2_equal-cpu-arguments`: Long = 300_000_000L
    def `bls12_381_G2_equal-memory-arguments`: Long = 300_000_000L
    def `bls12_381_G2_hashToGroup-cpu-arguments-intercept`: Long = 300_000_000L
    def `bls12_381_G2_hashToGroup-cpu-arguments-slope`: Long = 300_000_000L
    def `bls12_381_G2_hashToGroup-memory-arguments`: Long = 300_000_000L
    def `bls12_381_G2_neg-cpu-arguments`: Long = 300_000_000L
    def `bls12_381_G2_neg-memory-arguments`: Long = 300_000_000L
    def `bls12_381_G2_scalarMul-cpu-arguments-intercept`: Long = 300_000_000L
    def `bls12_381_G2_scalarMul-cpu-arguments-slope`: Long = 300_000_000L
    def `bls12_381_G2_scalarMul-memory-arguments`: Long = 300_000_000L
    def `bls12_381_G2_uncompress-cpu-arguments`: Long = 300_000_000L
    def `bls12_381_G2_uncompress-memory-arguments`: Long = 300_000_000L
    def `bls12_381_finalVerify-cpu-arguments`: Long = 300_000_000L
    def `bls12_381_finalVerify-memory-arguments`: Long = 300_000_000L
    def `bls12_381_millerLoop-cpu-arguments`: Long = 300_000_000L
    def `bls12_381_millerLoop-memory-arguments`: Long = 300_000_000L
    def `bls12_381_mulMlResult-cpu-arguments`: Long = 300_000_000L
    def `bls12_381_mulMlResult-memory-arguments`: Long = 300_000_000L
    def `keccak_256-cpu-arguments-intercept`: Long = 300_000_000L
    def `keccak_256-cpu-arguments-slope`: Long = 300_000_000L
    def `keccak_256-memory-arguments`: Long = 300_000_000L
    def `blake2b_224-cpu-arguments-intercept`: Long = 300_000_000L
    def `blake2b_224-cpu-arguments-slope`: Long = 300_000_000L
    def `blake2b_224-memory-arguments`: Long = 300_000_000L
    def `andByteString-cpu-arguments-intercept`: Long = 300_000_000L
    def `andByteString-cpu-arguments-slope1`: Long = 300_000_000L
    def `andByteString-cpu-arguments-slope2`: Long = 300_000_000L
    def `andByteString-memory-arguments-intercept`: Long = 300_000_000L
    def `andByteString-memory-arguments-slope`: Long = 300_000_000L
    def `orByteString-cpu-arguments-intercept`: Long = 300_000_000L
    def `orByteString-cpu-arguments-slope1`: Long = 300_000_000L
    def `orByteString-cpu-arguments-slope2`: Long = 300_000_000L
    def `orByteString-memory-arguments-intercept`: Long = 300_000_000L
    def `orByteString-memory-arguments-slope`: Long = 300_000_000L
    def `xorByteString-cpu-arguments-intercept`: Long = 300_000_000L
    def `xorByteString-cpu-arguments-slope1`: Long = 300_000_000L
    def `xorByteString-cpu-arguments-slope2`: Long = 300_000_000L
    def `xorByteString-memory-arguments-intercept`: Long = 300_000_000L
    def `xorByteString-memory-arguments-slope`: Long = 300_000_000L
    def `complementByteString-cpu-arguments-intercept`: Long = 300_000_000L
    def `complementByteString-cpu-arguments-slope`: Long = 300_000_000L
    def `complementByteString-memory-arguments-intercept`: Long = 300_000_000L
    def `complementByteString-memory-arguments-slope`: Long = 300_000_000L
    def `readBit-cpu-arguments`: Long = 300_000_000L
    def `readBit-memory-arguments`: Long = 300_000_000L
    def `writeBits-cpu-arguments-intercept`: Long = 300_000_000L
    def `writeBits-cpu-arguments-slope`: Long = 300_000_000L
    def `writeBits-memory-arguments-intercept`: Long = 300_000_000L
    def `writeBits-memory-arguments-slope`: Long = 300_000_000L
    def `replicateByte-cpu-arguments-intercept`: Long = 300_000_000L
    def `replicateByte-cpu-arguments-slope`: Long = 300_000_000L
    def `replicateByte-memory-arguments-intercept`: Long = 300_000_000L
    def `replicateByte-memory-arguments-slope`: Long = 300_000_000L
    def `shiftByteString-cpu-arguments-intercept`: Long = 300_000_000L
    def `shiftByteString-cpu-arguments-slope`: Long = 300_000_000L
    def `shiftByteString-memory-arguments-intercept`: Long = 300_000_000L
    def `shiftByteString-memory-arguments-slope`: Long = 300_000_000L
    def `rotateByteString-cpu-arguments-intercept`: Long = 300_000_000L
    def `rotateByteString-cpu-arguments-slope`: Long = 300_000_000L
    def `rotateByteString-memory-arguments-intercept`: Long = 300_000_000L
    def `rotateByteString-memory-arguments-slope`: Long = 300_000_000L
    def `countSetBits-cpu-arguments-intercept`: Long = 300_000_000L
    def `countSetBits-cpu-arguments-slope`: Long = 300_000_000L
    def `countSetBits-memory-arguments`: Long = 300_000_000L
    def `findFirstSetBit-cpu-arguments-intercept`: Long = 300_000_000L
    def `findFirstSetBit-cpu-arguments-slope`: Long = 300_000_000L
    def `findFirstSetBit-memory-arguments`: Long = 300_000_000L
    def `ripemd_160-cpu-arguments-intercept`: Long = 300_000_000L
    def `ripemd_160-cpu-arguments-slope`: Long = 300_000_000L
    def `ripemd_160-memory-arguments`: Long = 300_000_000L
}

/** Plutus V3 cost model parameters.
  *
  * The names of the fields are taken from
  * [[https://github.com/input-output-hk/plutus/blob/1.40.0.0/plutus-ledger-api/src/PlutusLedgerApi/V3/ParamName.hs]]
  * and Blockfrost Protocol Parameters JSON uses these names as well in
  * `blockfrost-params-epoch-544.json`
  *
  * But what's really important is the order of the fields because that's the order of the
  * parameters in the protocol parameters array.
  */
class PlutusV3Params extends PlutusParams {
    var `addInteger-cpu-arguments-intercept`: Long = 300_000_000L
    var `addInteger-cpu-arguments-slope`: Long = 300_000_000L
    var `addInteger-memory-arguments-intercept`: Long = 300_000_000L
    var `addInteger-memory-arguments-slope`: Long = 300_000_000L
    var `appendByteString-cpu-arguments-intercept`: Long = 300_000_000L
    var `appendByteString-cpu-arguments-slope`: Long = 300_000_000L
    var `appendByteString-memory-arguments-intercept`: Long = 300_000_000L
    var `appendByteString-memory-arguments-slope`: Long = 300_000_000L
    var `appendString-cpu-arguments-intercept`: Long = 300_000_000L
    var `appendString-cpu-arguments-slope`: Long = 300_000_000L
    var `appendString-memory-arguments-intercept`: Long = 300_000_000L
    var `appendString-memory-arguments-slope`: Long = 300_000_000L
    var `bData-cpu-arguments`: Long = 300_000_000L
    var `bData-memory-arguments`: Long = 300_000_000L
    var `blake2b_256-cpu-arguments-intercept`: Long = 300_000_000L
    var `blake2b_256-cpu-arguments-slope`: Long = 300_000_000L
    var `blake2b_256-memory-arguments`: Long = 300_000_000L
    var `cekApplyCost-exBudgetCPU`: Long = 300_000_000L
    var `cekApplyCost-exBudgetMemory`: Long = 300_000_000L
    var `cekBuiltinCost-exBudgetCPU`: Long = 300_000_000L
    var `cekBuiltinCost-exBudgetMemory`: Long = 300_000_000L
    var `cekConstCost-exBudgetCPU`: Long = 300_000_000L
    var `cekConstCost-exBudgetMemory`: Long = 300_000_000L
    var `cekDelayCost-exBudgetCPU`: Long = 300_000_000L
    var `cekDelayCost-exBudgetMemory`: Long = 300_000_000L
    var `cekForceCost-exBudgetCPU`: Long = 300_000_000L
    var `cekForceCost-exBudgetMemory`: Long = 300_000_000L
    var `cekLamCost-exBudgetCPU`: Long = 300_000_000L
    var `cekLamCost-exBudgetMemory`: Long = 300_000_000L
    var `cekStartupCost-exBudgetCPU`: Long = 300_000_000L
    var `cekStartupCost-exBudgetMemory`: Long = 300_000_000L
    var `cekVarCost-exBudgetCPU`: Long = 300_000_000L
    var `cekVarCost-exBudgetMemory`: Long = 300_000_000L
    var `chooseData-cpu-arguments`: Long = 300_000_000L
    var `chooseData-memory-arguments`: Long = 300_000_000L
    var `chooseList-cpu-arguments`: Long = 300_000_000L
    var `chooseList-memory-arguments`: Long = 300_000_000L
    var `chooseUnit-cpu-arguments`: Long = 300_000_000L
    var `chooseUnit-memory-arguments`: Long = 300_000_000L
    var `consByteString-cpu-arguments-intercept`: Long = 300_000_000L
    var `consByteString-cpu-arguments-slope`: Long = 300_000_000L
    var `consByteString-memory-arguments-intercept`: Long = 300_000_000L
    var `consByteString-memory-arguments-slope`: Long = 300_000_000L
    var `constrData-cpu-arguments`: Long = 300_000_000L
    var `constrData-memory-arguments`: Long = 300_000_000L
    var `decodeUtf8-cpu-arguments-intercept`: Long = 300_000_000L
    var `decodeUtf8-cpu-arguments-slope`: Long = 300_000_000L
    var `decodeUtf8-memory-arguments-intercept`: Long = 300_000_000L
    var `decodeUtf8-memory-arguments-slope`: Long = 300_000_000L
    var `divideInteger-cpu-arguments-constant`: Long = 300_000_000L
    var `divideInteger-cpu-arguments-c00`: Long = 300_000_000L
    var `divideInteger-cpu-arguments-c01`: Long = 300_000_000L
    var `divideInteger-cpu-arguments-c02`: Long = 300_000_000L
    var `divideInteger-cpu-arguments-c10`: Long = 300_000_000L
    var `divideInteger-cpu-arguments-c11`: Long = 300_000_000L
    var `divideInteger-cpu-arguments-c20`: Long = 300_000_000L
    var `divideInteger-cpu-arguments-minimum`: Long = 300_000_000L
    var `divideInteger-memory-arguments-intercept`: Long = 300_000_000L
    var `divideInteger-memory-arguments-minimum`: Long = 300_000_000L
    var `divideInteger-memory-arguments-slope`: Long = 300_000_000L
    var `encodeUtf8-cpu-arguments-intercept`: Long = 300_000_000L
    var `encodeUtf8-cpu-arguments-slope`: Long = 300_000_000L
    var `encodeUtf8-memory-arguments-intercept`: Long = 300_000_000L
    var `encodeUtf8-memory-arguments-slope`: Long = 300_000_000L
    var `equalsByteString-cpu-arguments-constant`: Long = 300_000_000L
    var `equalsByteString-cpu-arguments-intercept`: Long = 300_000_000L
    var `equalsByteString-cpu-arguments-slope`: Long = 300_000_000L
    var `equalsByteString-memory-arguments`: Long = 300_000_000L
    var `equalsData-cpu-arguments-intercept`: Long = 300_000_000L
    var `equalsData-cpu-arguments-slope`: Long = 300_000_000L
    var `equalsData-memory-arguments`: Long = 300_000_000L
    var `equalsInteger-cpu-arguments-intercept`: Long = 300_000_000L
    var `equalsInteger-cpu-arguments-slope`: Long = 300_000_000L
    var `equalsInteger-memory-arguments`: Long = 300_000_000L
    var `equalsString-cpu-arguments-constant`: Long = 300_000_000L
    var `equalsString-cpu-arguments-intercept`: Long = 300_000_000L
    var `equalsString-cpu-arguments-slope`: Long = 300_000_000L
    var `equalsString-memory-arguments`: Long = 300_000_000L
    var `fstPair-cpu-arguments`: Long = 300_000_000L
    var `fstPair-memory-arguments`: Long = 300_000_000L
    var `headList-cpu-arguments`: Long = 300_000_000L
    var `headList-memory-arguments`: Long = 300_000_000L
    var `iData-cpu-arguments`: Long = 300_000_000L
    var `iData-memory-arguments`: Long = 300_000_000L
    var `ifThenElse-cpu-arguments`: Long = 300_000_000L
    var `ifThenElse-memory-arguments`: Long = 300_000_000L
    var `indexByteString-cpu-arguments`: Long = 300_000_000L
    var `indexByteString-memory-arguments`: Long = 300_000_000L
    var `lengthOfByteString-cpu-arguments`: Long = 300_000_000L
    var `lengthOfByteString-memory-arguments`: Long = 300_000_000L
    var `lessThanByteString-cpu-arguments-intercept`: Long = 300_000_000L
    var `lessThanByteString-cpu-arguments-slope`: Long = 300_000_000L
    var `lessThanByteString-memory-arguments`: Long = 300_000_000L
    var `lessThanEqualsByteString-cpu-arguments-intercept`: Long = 300_000_000L
    var `lessThanEqualsByteString-cpu-arguments-slope`: Long = 300_000_000L
    var `lessThanEqualsByteString-memory-arguments`: Long = 300_000_000L
    var `lessThanEqualsInteger-cpu-arguments-intercept`: Long = 300_000_000L
    var `lessThanEqualsInteger-cpu-arguments-slope`: Long = 300_000_000L
    var `lessThanEqualsInteger-memory-arguments`: Long = 300_000_000L
    var `lessThanInteger-cpu-arguments-intercept`: Long = 300_000_000L
    var `lessThanInteger-cpu-arguments-slope`: Long = 300_000_000L
    var `lessThanInteger-memory-arguments`: Long = 300_000_000L
    var `listData-cpu-arguments`: Long = 300_000_000L
    var `listData-memory-arguments`: Long = 300_000_000L
    var `mapData-cpu-arguments`: Long = 300_000_000L
    var `mapData-memory-arguments`: Long = 300_000_000L
    var `mkCons-cpu-arguments`: Long = 300_000_000L
    var `mkCons-memory-arguments`: Long = 300_000_000L
    var `mkNilData-cpu-arguments`: Long = 300_000_000L
    var `mkNilData-memory-arguments`: Long = 300_000_000L
    var `mkNilPairData-cpu-arguments`: Long = 300_000_000L
    var `mkNilPairData-memory-arguments`: Long = 300_000_000L
    var `mkPairData-cpu-arguments`: Long = 300_000_000L
    var `mkPairData-memory-arguments`: Long = 300_000_000L
    var `modInteger-cpu-arguments-constant`: Long = 300_000_000L
    var `modInteger-cpu-arguments-model-arguments-c00`: Long = 300_000_000L
    var `modInteger-cpu-arguments-model-arguments-c01`: Long = 300_000_000L
    var `modInteger-cpu-arguments-model-arguments-c02`: Long = 300_000_000L
    var `modInteger-cpu-arguments-model-arguments-c10`: Long = 300_000_000L
    var `modInteger-cpu-arguments-model-arguments-c11`: Long = 300_000_000L
    var `modInteger-cpu-arguments-model-arguments-c20`: Long = 300_000_000L
    var `modInteger-cpu-arguments-model-arguments-minimum`: Long = 300_000_000L
    var `modInteger-memory-arguments-intercept`: Long = 300_000_000L
    var `modInteger-memory-arguments-slope`: Long = 300_000_000L
    var `multiplyInteger-cpu-arguments-intercept`: Long = 300_000_000L
    var `multiplyInteger-cpu-arguments-slope`: Long = 300_000_000L
    var `multiplyInteger-memory-arguments-intercept`: Long = 300_000_000L
    var `multiplyInteger-memory-arguments-slope`: Long = 300_000_000L
    var `nullList-cpu-arguments`: Long = 300_000_000L
    var `nullList-memory-arguments`: Long = 300_000_000L
    var `quotientInteger-cpu-arguments-constant`: Long = 300_000_000L
    var `quotientInteger-cpu-arguments-model-arguments-c00`: Long = 300_000_000L
    var `quotientInteger-cpu-arguments-model-arguments-c01`: Long = 300_000_000L
    var `quotientInteger-cpu-arguments-model-arguments-c02`: Long = 300_000_000L
    var `quotientInteger-cpu-arguments-model-arguments-c10`: Long = 300_000_000L
    var `quotientInteger-cpu-arguments-model-arguments-c11`: Long = 300_000_000L
    var `quotientInteger-cpu-arguments-model-arguments-c20`: Long = 300_000_000L
    var `quotientInteger-cpu-arguments-model-arguments-minimum`: Long = 300_000_000L
    var `quotientInteger-memory-arguments-intercept`: Long = 300_000_000L
    var `quotientInteger-memory-arguments-minimum`: Long = 300_000_000L
    var `quotientInteger-memory-arguments-slope`: Long = 300_000_000L
    var `remainderInteger-cpu-arguments-constant`: Long = 300_000_000L
    var `remainderInteger-cpu-arguments-model-arguments-c00`: Long = 300_000_000L
    var `remainderInteger-cpu-arguments-model-arguments-c01`: Long = 300_000_000L
    var `remainderInteger-cpu-arguments-model-arguments-c02`: Long = 300_000_000L
    var `remainderInteger-cpu-arguments-model-arguments-c10`: Long = 300_000_000L
    var `remainderInteger-cpu-arguments-model-arguments-c11`: Long = 300_000_000L
    var `remainderInteger-cpu-arguments-model-arguments-c20`: Long = 300_000_000L
    var `remainderInteger-cpu-arguments-model-arguments-minimum`: Long = 300_000_000L
    var `remainderInteger-memory-arguments-intercept`: Long = 300_000_000L
    var `remainderInteger-memory-arguments-slope`: Long = 300_000_000L
    var `serialiseData-cpu-arguments-intercept`: Long = 300_000_000L
    var `serialiseData-cpu-arguments-slope`: Long = 300_000_000L
    var `serialiseData-memory-arguments-intercept`: Long = 300_000_000L
    var `serialiseData-memory-arguments-slope`: Long = 300_000_000L
    var `sha2_256-cpu-arguments-intercept`: Long = 300_000_000L
    var `sha2_256-cpu-arguments-slope`: Long = 300_000_000L
    var `sha2_256-memory-arguments`: Long = 300_000_000L
    var `sha3_256-cpu-arguments-intercept`: Long = 300_000_000L
    var `sha3_256-cpu-arguments-slope`: Long = 300_000_000L
    var `sha3_256-memory-arguments`: Long = 300_000_000L
    var `sliceByteString-cpu-arguments-intercept`: Long = 300_000_000L
    var `sliceByteString-cpu-arguments-slope`: Long = 300_000_000L
    var `sliceByteString-memory-arguments-intercept`: Long = 300_000_000L
    var `sliceByteString-memory-arguments-slope`: Long = 300_000_000L
    var `sndPair-cpu-arguments`: Long = 300_000_000L
    var `sndPair-memory-arguments`: Long = 300_000_000L
    var `subtractInteger-cpu-arguments-intercept`: Long = 300_000_000L
    var `subtractInteger-cpu-arguments-slope`: Long = 300_000_000L
    var `subtractInteger-memory-arguments-intercept`: Long = 300_000_000L
    var `subtractInteger-memory-arguments-slope`: Long = 300_000_000L
    var `tailList-cpu-arguments`: Long = 300_000_000L
    var `tailList-memory-arguments`: Long = 300_000_000L
    var `trace-cpu-arguments`: Long = 300_000_000L
    var `trace-memory-arguments`: Long = 300_000_000L
    var `unBData-cpu-arguments`: Long = 300_000_000L
    var `unBData-memory-arguments`: Long = 300_000_000L
    var `unConstrData-cpu-arguments`: Long = 300_000_000L
    var `unConstrData-memory-arguments`: Long = 300_000_000L
    var `unIData-cpu-arguments`: Long = 300_000_000L
    var `unIData-memory-arguments`: Long = 300_000_000L
    var `unListData-cpu-arguments`: Long = 300_000_000L
    var `unListData-memory-arguments`: Long = 300_000_000L
    var `unMapData-cpu-arguments`: Long = 300_000_000L
    var `unMapData-memory-arguments`: Long = 300_000_000L
    var `verifyEcdsaSecp256k1Signature-cpu-arguments`: Long = 300_000_000L
    var `verifyEcdsaSecp256k1Signature-memory-arguments`: Long = 300_000_000L
    var `verifyEd25519Signature-cpu-arguments-intercept`: Long = 300_000_000L
    var `verifyEd25519Signature-cpu-arguments-slope`: Long = 300_000_000L
    var `verifyEd25519Signature-memory-arguments`: Long = 300_000_000L
    var `verifySchnorrSecp256k1Signature-cpu-arguments-intercept`: Long = 300_000_000L
    var `verifySchnorrSecp256k1Signature-cpu-arguments-slope`: Long = 300_000_000L
    var `verifySchnorrSecp256k1Signature-memory-arguments`: Long = 300_000_000L
    var `cekConstrCost-exBudgetCPU`: Long = 300_000_000L
    var `cekConstrCost-exBudgetMemory`: Long = 300_000_000L
    var `cekCaseCost-exBudgetCPU`: Long = 300_000_000L
    var `cekCaseCost-exBudgetMemory`: Long = 300_000_000L
    var `bls12_381_G1_add-cpu-arguments`: Long = 300_000_000L
    var `bls12_381_G1_add-memory-arguments`: Long = 300_000_000L
    var `bls12_381_G1_compress-cpu-arguments`: Long = 300_000_000L
    var `bls12_381_G1_compress-memory-arguments`: Long = 300_000_000L
    var `bls12_381_G1_equal-cpu-arguments`: Long = 300_000_000L
    var `bls12_381_G1_equal-memory-arguments`: Long = 300_000_000L
    var `bls12_381_G1_hashToGroup-cpu-arguments-intercept`: Long = 300_000_000L
    var `bls12_381_G1_hashToGroup-cpu-arguments-slope`: Long = 300_000_000L
    var `bls12_381_G1_hashToGroup-memory-arguments`: Long = 300_000_000L
    var `bls12_381_G1_neg-cpu-arguments`: Long = 300_000_000L
    var `bls12_381_G1_neg-memory-arguments`: Long = 300_000_000L
    var `bls12_381_G1_scalarMul-cpu-arguments-intercept`: Long = 300_000_000L
    var `bls12_381_G1_scalarMul-cpu-arguments-slope`: Long = 300_000_000L
    var `bls12_381_G1_scalarMul-memory-arguments`: Long = 300_000_000L
    var `bls12_381_G1_uncompress-cpu-arguments`: Long = 300_000_000L
    var `bls12_381_G1_uncompress-memory-arguments`: Long = 300_000_000L
    var `bls12_381_G2_add-cpu-arguments`: Long = 300_000_000L
    var `bls12_381_G2_add-memory-arguments`: Long = 300_000_000L
    var `bls12_381_G2_compress-cpu-arguments`: Long = 300_000_000L
    var `bls12_381_G2_compress-memory-arguments`: Long = 300_000_000L
    var `bls12_381_G2_equal-cpu-arguments`: Long = 300_000_000L
    var `bls12_381_G2_equal-memory-arguments`: Long = 300_000_000L
    var `bls12_381_G2_hashToGroup-cpu-arguments-intercept`: Long = 300_000_000L
    var `bls12_381_G2_hashToGroup-cpu-arguments-slope`: Long = 300_000_000L
    var `bls12_381_G2_hashToGroup-memory-arguments`: Long = 300_000_000L
    var `bls12_381_G2_neg-cpu-arguments`: Long = 300_000_000L
    var `bls12_381_G2_neg-memory-arguments`: Long = 300_000_000L
    var `bls12_381_G2_scalarMul-cpu-arguments-intercept`: Long = 300_000_000L
    var `bls12_381_G2_scalarMul-cpu-arguments-slope`: Long = 300_000_000L
    var `bls12_381_G2_scalarMul-memory-arguments`: Long = 300_000_000L
    var `bls12_381_G2_uncompress-cpu-arguments`: Long = 300_000_000L
    var `bls12_381_G2_uncompress-memory-arguments`: Long = 300_000_000L
    var `bls12_381_finalVerify-cpu-arguments`: Long = 300_000_000L
    var `bls12_381_finalVerify-memory-arguments`: Long = 300_000_000L
    var `bls12_381_millerLoop-cpu-arguments`: Long = 300_000_000L
    var `bls12_381_millerLoop-memory-arguments`: Long = 300_000_000L
    var `bls12_381_mulMlResult-cpu-arguments`: Long = 300_000_000L
    var `bls12_381_mulMlResult-memory-arguments`: Long = 300_000_000L
    var `keccak_256-cpu-arguments-intercept`: Long = 300_000_000L
    var `keccak_256-cpu-arguments-slope`: Long = 300_000_000L
    var `keccak_256-memory-arguments`: Long = 300_000_000L
    var `blake2b_224-cpu-arguments-intercept`: Long = 300_000_000L
    var `blake2b_224-cpu-arguments-slope`: Long = 300_000_000L
    var `blake2b_224-memory-arguments`: Long = 300_000_000L
    var `integerToByteString-cpu-arguments-c0`: Long = 300_000_000L
    var `integerToByteString-cpu-arguments-c1`: Long = 300_000_000L
    var `integerToByteString-cpu-arguments-c2`: Long = 300_000_000L
    var `integerToByteString-memory-arguments-intercept`: Long = 300_000_000L
    var `integerToByteString-memory-arguments-slope`: Long = 300_000_000L
    var `byteStringToInteger-cpu-arguments-c0`: Long = 300_000_000L
    var `byteStringToInteger-cpu-arguments-c1`: Long = 300_000_000L
    var `byteStringToInteger-cpu-arguments-c2`: Long = 300_000_000L
    var `byteStringToInteger-memory-arguments-intercept`: Long = 300_000_000L
    var `byteStringToInteger-memory-arguments-slope`: Long = 300_000_000L
    var `andByteString-cpu-arguments-intercept`: Long = 300_000_000L
    var `andByteString-cpu-arguments-slope1`: Long = 300_000_000L
    var `andByteString-cpu-arguments-slope2`: Long = 300_000_000L
    var `andByteString-memory-arguments-intercept`: Long = 300_000_000L
    var `andByteString-memory-arguments-slope`: Long = 300_000_000L
    var `orByteString-cpu-arguments-intercept`: Long = 300_000_000L
    var `orByteString-cpu-arguments-slope1`: Long = 300_000_000L
    var `orByteString-cpu-arguments-slope2`: Long = 300_000_000L
    var `orByteString-memory-arguments-intercept`: Long = 300_000_000L
    var `orByteString-memory-arguments-slope`: Long = 300_000_000L
    var `xorByteString-cpu-arguments-intercept`: Long = 300_000_000L
    var `xorByteString-cpu-arguments-slope1`: Long = 300_000_000L
    var `xorByteString-cpu-arguments-slope2`: Long = 300_000_000L
    var `xorByteString-memory-arguments-intercept`: Long = 300_000_000L
    var `xorByteString-memory-arguments-slope`: Long = 300_000_000L
    var `complementByteString-cpu-arguments-intercept`: Long = 300_000_000L
    var `complementByteString-cpu-arguments-slope`: Long = 300_000_000L
    var `complementByteString-memory-arguments-intercept`: Long = 300_000_000L
    var `complementByteString-memory-arguments-slope`: Long = 300_000_000L
    var `readBit-cpu-arguments`: Long = 300_000_000L
    var `readBit-memory-arguments`: Long = 300_000_000L
    var `writeBits-cpu-arguments-intercept`: Long = 300_000_000L
    var `writeBits-cpu-arguments-slope`: Long = 300_000_000L
    var `writeBits-memory-arguments-intercept`: Long = 300_000_000L
    var `writeBits-memory-arguments-slope`: Long = 300_000_000L
    var `replicateByte-cpu-arguments-intercept`: Long = 300_000_000L
    var `replicateByte-cpu-arguments-slope`: Long = 300_000_000L
    var `replicateByte-memory-arguments-intercept`: Long = 300_000_000L
    var `replicateByte-memory-arguments-slope`: Long = 300_000_000L
    var `shiftByteString-cpu-arguments-intercept`: Long = 300_000_000L
    var `shiftByteString-cpu-arguments-slope`: Long = 300_000_000L
    var `shiftByteString-memory-arguments-intercept`: Long = 300_000_000L
    var `shiftByteString-memory-arguments-slope`: Long = 300_000_000L
    var `rotateByteString-cpu-arguments-intercept`: Long = 300_000_000L
    var `rotateByteString-cpu-arguments-slope`: Long = 300_000_000L
    var `rotateByteString-memory-arguments-intercept`: Long = 300_000_000L
    var `rotateByteString-memory-arguments-slope`: Long = 300_000_000L
    var `countSetBits-cpu-arguments-intercept`: Long = 300_000_000L
    var `countSetBits-cpu-arguments-slope`: Long = 300_000_000L
    var `countSetBits-memory-arguments`: Long = 300_000_000L
    var `findFirstSetBit-cpu-arguments-intercept`: Long = 300_000_000L
    var `findFirstSetBit-cpu-arguments-slope`: Long = 300_000_000L
    var `findFirstSetBit-memory-arguments`: Long = 300_000_000L
    var `ripemd_160-cpu-arguments-intercept`: Long = 300_000_000L
    var `ripemd_160-cpu-arguments-slope`: Long = 300_000_000L
    var `ripemd_160-memory-arguments`: Long = 300_000_000L

    // Not available in Plutus V3, old names kept for compatibility
    def `divideInteger-cpu-arguments-model-arguments-intercept`: Long = 300_000_000L
    def `divideInteger-cpu-arguments-model-arguments-slope`: Long = 300_000_000L
    def `modInteger-cpu-arguments-model-arguments-intercept`: Long = 300_000_000L
    def `modInteger-cpu-arguments-model-arguments-slope`: Long = 300_000_000L
    def `modInteger-memory-arguments-minimum`: Long = 300_000_000L
    def `quotientInteger-cpu-arguments-model-arguments-intercept`: Long = 300_000_000L
    def `quotientInteger-cpu-arguments-model-arguments-slope`: Long = 300_000_000L
    def `remainderInteger-cpu-arguments-model-arguments-intercept`: Long = 300_000_000L
    def `remainderInteger-cpu-arguments-model-arguments-slope`: Long = 300_000_000L
    def `remainderInteger-memory-arguments-minimum`: Long = 300_000_000L
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

object PlutusV2Params:
    given ReadWriter[PlutusV2Params] = JsonUtils.mkClassFieldsReadWriter[PlutusV2Params]
    val (toSeq, fromSeq) = JsonUtils.mkClassFieldsFromSeqIso[PlutusV2Params]

object PlutusV3Params:
    given ReadWriter[PlutusV3Params] = JsonUtils.mkClassFieldsReadWriter[PlutusV3Params]
    val (toSeq, fromSeq) = JsonUtils.mkClassFieldsFromSeqIso[PlutusV3Params]
