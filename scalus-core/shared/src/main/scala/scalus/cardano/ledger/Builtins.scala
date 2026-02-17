package scalus.cardano.ledger

import scalus.uplc.DefaultFun
import scala.collection.immutable.SortedMap

object Builtins {
    object Batches {
        // {- Batches of builtins which were introduced in the same hard fork (but perhaps
        //   not for all LLs): see the Plutus Core specification and
        //   `builtinsIntroducedIn` below.
        // -}
        //
        // {- If any new builtins are introduced after a batch has been deployed on the chain
        //  then a new `batch` object MUST be added to contain them and the
        //  `builtinsIntroducedIn` function must be updated; the contents of batches which
        //  have already been deployed must NOT be altered.  Also, remember to UPDATE THE
        //  TESTS in `Spec.Versions` and `Spec.Data.Versions` when a new batch is added.
        // -}
        //
        // {- It's tempting to try something like `fmap toEnum [0..50]` here, but that's
        //   dangerous because the order of the constructors in DefaultFun doesn't
        //   precisely match the order that the builtins were introduced in.  A safer
        //   alternative would be to use the flat tags, but they're not directly
        //   accessible at the moment.
        // -}
        // -- DO NOT CHANGE THIS.
        val batch1: Set[DefaultFun] =
            Set(
              // Integers
              DefaultFun.AddInteger,
              DefaultFun.SubtractInteger,
              DefaultFun.MultiplyInteger,
              DefaultFun.DivideInteger,
              DefaultFun.QuotientInteger,
              DefaultFun.RemainderInteger,
              DefaultFun.ModInteger,
              DefaultFun.EqualsInteger,
              DefaultFun.LessThanInteger,
              DefaultFun.LessThanEqualsInteger,
              // Bytestrings
              DefaultFun.AppendByteString,
              DefaultFun.ConsByteString,
              DefaultFun.SliceByteString,
              DefaultFun.LengthOfByteString,
              DefaultFun.IndexByteString,
              DefaultFun.EqualsByteString,
              DefaultFun.LessThanByteString,
              DefaultFun.LessThanEqualsByteString,
              // Crypto & hashes
              DefaultFun.Sha2_256,
              DefaultFun.Sha3_256,
              DefaultFun.Blake2b_256,
              DefaultFun.VerifyEd25519Signature,
              // Strings
              DefaultFun.AppendString,
              DefaultFun.EqualsString,
              DefaultFun.EncodeUtf8,
              DefaultFun.DecodeUtf8,
              // Bool / control
              DefaultFun.IfThenElse,
              DefaultFun.ChooseUnit,
              DefaultFun.Trace,
              // Pairs
              DefaultFun.FstPair,
              DefaultFun.SndPair,
              // Lists
              DefaultFun.ChooseList,
              DefaultFun.MkCons,
              DefaultFun.HeadList,
              DefaultFun.TailList,
              DefaultFun.NullList,
              // Data
              DefaultFun.ChooseData,
              DefaultFun.ConstrData,
              DefaultFun.MapData,
              DefaultFun.ListData,
              DefaultFun.IData,
              DefaultFun.BData,
              DefaultFun.UnConstrData,
              DefaultFun.UnMapData,
              DefaultFun.UnListData,
              DefaultFun.UnIData,
              DefaultFun.UnBData,
              DefaultFun.EqualsData,
              // Misc
              DefaultFun.MkPairData,
              DefaultFun.MkNilData,
              DefaultFun.MkNilPairData
            )

        // DO NOT CHANGE THIS.
        val batch2: Set[DefaultFun] =
            Set(DefaultFun.SerialiseData)

        // DO NOT CHANGE THIS.
        val batch3: Set[DefaultFun] =
            Set(
              DefaultFun.VerifyEcdsaSecp256k1Signature,
              DefaultFun.VerifySchnorrSecp256k1Signature
            )

        // -- `cekCase` and `cekConstr` costs come between Batch 3 and Batch 4 in the
        // -- PlutusV3 cost model parameters, although that's irrelevant here.
        //
        // -- batch4, excluding IntegerToByteString and ByteStringToInteger.
        // -- DO NOT CHANGE THIS.
        val batch4a: Set[DefaultFun] =
            Set(
              // BLS12-381
              DefaultFun.Bls12_381_G1_add,
              DefaultFun.Bls12_381_G1_neg,
              DefaultFun.Bls12_381_G1_scalarMul,
              DefaultFun.Bls12_381_G1_equal,
              DefaultFun.Bls12_381_G1_hashToGroup,
              DefaultFun.Bls12_381_G1_compress,
              DefaultFun.Bls12_381_G1_uncompress,
              DefaultFun.Bls12_381_G2_add,
              DefaultFun.Bls12_381_G2_neg,
              DefaultFun.Bls12_381_G2_scalarMul,
              DefaultFun.Bls12_381_G2_equal,
              DefaultFun.Bls12_381_G2_hashToGroup,
              DefaultFun.Bls12_381_G2_compress,
              DefaultFun.Bls12_381_G2_uncompress,
              DefaultFun.Bls12_381_millerLoop,
              DefaultFun.Bls12_381_mulMlResult,
              DefaultFun.Bls12_381_finalVerify,
              // Hashes
              DefaultFun.Keccak_256,
              DefaultFun.Blake2b_224
            )

        // {- batch4b: IntegerToByteString and ByteStringToInteger.  These were enabled in
        // PlutusV3 at PV9, along with batch4a, They were enabled in PlutusV2 at PV10 in
        // #6056 and #6065.  They are available on the chain, but they're prohibitively
        // expensive because the proposal to update the relevant protocol parameters has
        // not (yet) been enacted.  This has left a "gap" in the cost model paramters: for
        // PlutusV3, the parameters for Batch 3 are followed those for 4a, then 4b, but
        // for PlutusV2 those for Batch3 are followed by those for Batch 4a, and those for
        // 4b aren't in use yet.  Since you can't actually use the 4b builtins in PlutusV2
        // at the moment, it's tempting to insert the 4a parameter before the 4b
        // parameters and enable them all at PV11 and with a suitable parameter update.
        // However, if we do do this there's a theoretical risk of turning a phase 2
        // failure into a phase 1 failure: would that be problematic?
        // -}
        // -- DO NOT CHANGE THIS.
        val batch4b: Set[DefaultFun] =
            Set(DefaultFun.IntegerToByteString, DefaultFun.ByteStringToInteger)

        // DO NOT CHANGE THIS.
        val batch4: Set[DefaultFun] = batch4a ++ batch4b

        // DO NOT CHANGE THIS.
        val batch5: Set[DefaultFun] =
            Set(
              DefaultFun.AndByteString,
              DefaultFun.OrByteString,
              DefaultFun.XorByteString,
              DefaultFun.ComplementByteString,
              DefaultFun.ReadBit,
              DefaultFun.WriteBits,
              DefaultFun.ReplicateByte,
              DefaultFun.ShiftByteString,
              DefaultFun.RotateByteString,
              DefaultFun.CountSetBits,
              DefaultFun.FindFirstSetBit,
              DefaultFun.Ripemd_160
            )

        val batch6: Set[DefaultFun] =
            Set(
              DefaultFun.ExpModInteger
//              DefaultFun.DropList,
//              DefaultFun.LengthOfArray,
//              DefaultFun.ListToArray,
//              DefaultFun.IndexArray
            )
    }

    private val builtinsIntroducedIn
        : Map[Language, SortedMap[MajorProtocolVersion, Set[DefaultFun]]] = {

        val tmp = Map(
          Language.PlutusV1 -> SortedMap(
            MajorProtocolVersion.alonzoPV -> Batches.batch1,
            MajorProtocolVersion.vanRossemPV -> (
              Batches.batch2 ++ Batches.batch3 ++
                  Batches.batch4 ++ Batches.batch5 ++
                  Batches.batch6
            )
          ),
          Language.PlutusV2 -> SortedMap(
            MajorProtocolVersion.vasilPV -> (Batches.batch1 ++ Batches.batch2),
            MajorProtocolVersion.valentinePV -> Batches.batch3,
            MajorProtocolVersion.plominPV -> Batches.batch4b,
            MajorProtocolVersion.vanRossemPV -> (Batches.batch4a ++ Batches.batch5 ++ Batches.batch6)
          ),
          Language.PlutusV3 -> SortedMap(
            MajorProtocolVersion.changPV -> (
              Batches.batch1 ++ Batches.batch2 ++
                  Batches.batch3 ++ Batches.batch4
            ),
            MajorProtocolVersion.plominPV -> Batches.batch5,
            MajorProtocolVersion.vanRossemPV -> Batches.batch6
          ),
          Language.PlutusV4 -> SortedMap(
            MajorProtocolVersion.vanRossemPV -> (
              Batches.batch1 ++ Batches.batch2 ++
                  Batches.batch3 ++ Batches.batch4 ++
                  Batches.batch5 ++ Batches.batch6
            )
          )
        )

        tmp.view.mapValues { majorProtocolVersionBuiltins =>
            majorProtocolVersionBuiltins
                .foldLeft(
                  (SortedMap.empty[MajorProtocolVersion, Set[DefaultFun]], Set.empty[DefaultFun])
                ) { case ((resultMap, resultBuiltins), (majorProtocolVersion, builtins)) =>
                    val newResultBuiltins = resultBuiltins ++ builtins
                    (resultMap + (majorProtocolVersion -> newResultBuiltins), newResultBuiltins)
                }
                ._1
        }.toMap
    }

    def findBuiltinsIntroducedIn(
        language: Language,
        majorProtocolVersion: MajorProtocolVersion
    ): Set[DefaultFun] = {
        builtinsIntroducedIn.get(language) match
            case Some(majorProtocolVersionBuiltins) =>
                val builtins = majorProtocolVersionBuiltins.keySet.view
                    .takeWhile { currentMajorProtocolVersion =>
                        currentMajorProtocolVersion <= majorProtocolVersion
                    }
                    .lastOption
                    .flatMap(majorProtocolVersionBuiltins.get)
                    .getOrElse(Set.empty)

                builtins

            case None => Set.empty
    }
}
