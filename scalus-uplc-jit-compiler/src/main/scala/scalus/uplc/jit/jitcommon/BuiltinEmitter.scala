package scalus.uplc.jit.jitcommon

import scalus.uplc.DefaultFun
import scalus.uplc.DefaultFun.{SubtractInteger, VerifyEd25519Signature}
import scalus.uplc.eval.{BudgetSpender, ExBudgetCategory, Logger, MachineParams, StepKind}

import scala.quoted.*

/** Emits code for UPLC builtin functions across all JIT compiler variants.
  *
  * This object provides a unified interface for generating code for builtin functions, supporting
  * both direct code generation (nativestack) and continuation-based code generation (mincont).
  */
object BuiltinEmitter {

    /** Generates code for a standalone (unapplied) builtin term.
      *
      * @param bn
      *   The builtin function
      * @param logger
      *   The logger expression
      * @param budget
      *   The budget spender expression
      * @param params
      *   The machine parameters expression
      * @return
      *   The generated code expression for the builtin
      */
    def emitBuiltin(
        bn: DefaultFun,
        logger: Expr[Logger],
        budget: Expr[BudgetSpender],
        params: Expr[MachineParams]
    )(using Quotes): Expr[Any] = {
        val evalSnipped = bn match
            case DefaultFun.AddInteger =>
                '{ BuiltinSnippets.addInteger($budget, $params) }
            case DefaultFun.SubtractInteger =>
                '{ BuiltinSnippets.subtractInteger($budget, $params) }
            case DefaultFun.MultiplyInteger =>
                '{ BuiltinSnippets.multiplyInteger($budget, $params) }
            case DefaultFun.LessThanInteger =>
                '{ BuiltinSnippets.lessThanInteger($budget, $params) }
            case DefaultFun.LessThanEqualsInteger =>
                '{ BuiltinSnippets.lessThanEqualsInteger($budget, $params) }
            case DefaultFun.EqualsInteger =>
                '{ BuiltinSnippets.equalsInteger($budget, $params) }
            case DefaultFun.ConsByteString =>
                '{ BuiltinSnippets.consByteString($budget, $params) }
            case DefaultFun.SliceByteString =>
                '{ BuiltinSnippets.sliceByteString($budget, $params) }
            case DefaultFun.IndexByteString =>
                '{ BuiltinSnippets.indexByteString($budget, $params) }
            case DefaultFun.EqualsByteString =>
                '{ BuiltinSnippets.equalsByteString($budget, $params) }
            case DefaultFun.EqualsData =>
                '{ BuiltinSnippets.equalsData($budget, $params) }
            case DefaultFun.IfThenElse =>
                '{ BuiltinSnippets.ifThenElse($budget, $params) }
            case DefaultFun.Trace =>
                '{ BuiltinSnippets.trace($logger, $budget, $params) }
            case DefaultFun.FstPair =>
                '{ BuiltinSnippets.fstPair($budget, $params) }
            case DefaultFun.SndPair =>
                '{ BuiltinSnippets.sndPair($budget, $params) }
            case DefaultFun.ChooseList =>
                '{ BuiltinSnippets.chooseList($budget, $params) }
            case DefaultFun.Sha2_256 =>
                '{ BuiltinSnippets.sha2_256($budget, $params) }
            case DefaultFun.HeadList =>
                '{ BuiltinSnippets.headList($budget, $params) }
            case DefaultFun.TailList =>
                '{ BuiltinSnippets.tailList($budget, $params) }
            case DefaultFun.UnConstrData =>
                '{ BuiltinSnippets.unConstrData($budget, $params) }
            case DefaultFun.UnListData =>
                '{ BuiltinSnippets.unListData($budget, $params) }
            case DefaultFun.UnIData =>
                '{ BuiltinSnippets.unIData($budget, $params) }
            case DefaultFun.UnBData =>
                '{ BuiltinSnippets.unBData($budget, $params) }
            case DefaultFun.ConstrData =>
                '{ BuiltinSnippets.constrData($budget, $params) }
            case DefaultFun.IData =>
                '{ BuiltinSnippets.iData($budget, $params) }
            case DefaultFun.BData =>
                '{ BuiltinSnippets.bData($budget, $params) }
            case DefaultFun.ListData =>
                '{ BuiltinSnippets.listData($budget, $params) }
            case DefaultFun.MapData =>
                '{ BuiltinSnippets.mapData($budget, $params) }
            case DefaultFun.UnMapData =>
                '{ BuiltinSnippets.unMapData($budget, $params) }
            case DefaultFun.SerialiseData =>
                '{ BuiltinSnippets.serialiseData($budget, $params) }
            case DefaultFun.MkPairData =>
                '{ BuiltinSnippets.mkPairData($budget, $params) }
            case DefaultFun.ChooseData =>
                '{ BuiltinSnippets.chooseData($budget, $params) }
            case DefaultFun.MkCons =>
                '{ BuiltinSnippets.mkCons($budget, $params) }
            case DefaultFun.NullList =>
                '{ BuiltinSnippets.nullList($budget, $params) }
            case DefaultFun.MkNilData =>
                '{ BuiltinSnippets.mkNilData($budget, $params) }
            case DefaultFun.MkNilPairData =>
                '{ BuiltinSnippets.mkNilPairData($budget, $params) }
            case DefaultFun.VerifyEd25519Signature =>
                '{ BuiltinSnippets.verifyEd25519Signature($budget, $params) }
            case DefaultFun.VerifyEcdsaSecp256k1Signature =>
                '{ BuiltinSnippets.verifyEcdsaSecp256k1Signature($budget, $params) }
            case DefaultFun.VerifySchnorrSecp256k1Signature =>
                '{ BuiltinSnippets.verifySchnorrSecp256k1Signature($budget, $params) }
            case DefaultFun.ChooseUnit =>
                '{ BuiltinSnippets.chooseUnit($budget, $params) }
            case _ =>
                sys.error(
                  s"Builtin $bn is not yet supported by the JIT compiler. Please add implementation in the Builtin pattern matching section."
                )
        '{
            ${ budget }.spendBudget(
              ExBudgetCategory.Step(StepKind.Builtin),
              $params.machineCosts.builtinCost,
              Nil
            )
            ${ evalSnipped }
        }
    }

    /** Generates optimized code for a fully-applied 1-argument builtin with a simple argument.
      *
      * @param bn
      *   The builtin function
      * @param argCode
      *   The generated code for the argument
      * @param budget
      *   The budget spender expression
      * @param params
      *   The machine parameters expression
      * @return
      *   The generated code expression for the applied builtin
      */
    def emitAppliedBuiltin1(
        bn: DefaultFun,
        argCode: Expr[Any],
        budget: Expr[BudgetSpender],
        params: Expr[MachineParams]
    )(using Quotes): Expr[Any] =
        bn match
            case DefaultFun.Sha2_256 =>
                BuiltinAppliedGenerator.sha2_256(argCode, budget, params)
            case DefaultFun.Sha3_256 =>
                BuiltinAppliedGenerator.sha3_256(argCode, budget, params)
            case DefaultFun.Blake2b_256 =>
                BuiltinAppliedGenerator.blake2b_256(argCode, budget, params)
            case DefaultFun.UnConstrData =>
                BuiltinAppliedGenerator.unConstrData(argCode, budget, params)
            case DefaultFun.UnListData =>
                BuiltinAppliedGenerator.unListData(argCode, budget, params)
            case DefaultFun.UnIData =>
                BuiltinAppliedGenerator.unIData(argCode, budget, params)
            case DefaultFun.UnBData =>
                BuiltinAppliedGenerator.unBData(argCode, budget, params)
            case DefaultFun.LengthOfByteString =>
                BuiltinAppliedGenerator.lengthOfByteString(argCode, budget, params)
            case DefaultFun.EncodeUtf8 =>
                BuiltinAppliedGenerator.encodeUtf8(argCode, budget, params)
            case DefaultFun.DecodeUtf8 =>
                BuiltinAppliedGenerator.decodeUtf8(argCode, budget, params)
            case DefaultFun.HeadList =>
                BuiltinAppliedGenerator.headList(argCode, budget, params)
            case DefaultFun.TailList =>
                BuiltinAppliedGenerator.tailList(argCode, budget, params)
            case DefaultFun.IData =>
                BuiltinAppliedGenerator.iData(argCode, budget, params)
            case DefaultFun.BData =>
                BuiltinAppliedGenerator.bData(argCode, budget, params)
            case DefaultFun.ListData =>
                BuiltinAppliedGenerator.listData(argCode, budget, params)
            case DefaultFun.MapData =>
                BuiltinAppliedGenerator.mapData(argCode, budget, params)
            case DefaultFun.UnMapData =>
                BuiltinAppliedGenerator.unMapData(argCode, budget, params)
            case DefaultFun.SerialiseData =>
                BuiltinAppliedGenerator.serialiseData(argCode, budget, params)
            case DefaultFun.NullList =>
                BuiltinAppliedGenerator.nullList(argCode, budget, params)
            case DefaultFun.MkNilData =>
                BuiltinAppliedGenerator.mkNilData(argCode, budget, params)
            case DefaultFun.MkNilPairData =>
                BuiltinAppliedGenerator.mkNilPairData(argCode, budget, params)
            case _ =>
                throw IllegalStateException(
                  s"Short circuit optimization for 1-arg builtin $bn not implemented"
                )

    /** Generates optimized code for a fully-applied 2-argument builtin with simple arguments.
      *
      * @param bn
      *   The builtin function
      * @param arg1Code
      *   The generated code for the first argument
      * @param arg2Code
      *   The generated code for the second argument
      * @param budget
      *   The budget spender expression
      * @param params
      *   The machine parameters expression
      * @return
      *   The generated code expression for the applied builtin
      */
    def emitAppliedBuiltin2(
        bn: DefaultFun,
        arg1Code: Expr[Any],
        arg2Code: Expr[Any],
        budget: Expr[BudgetSpender],
        params: Expr[MachineParams]
    )(using Quotes): Expr[Any] =
        bn match
            // Integer operations
            case DefaultFun.AddInteger =>
                BuiltinAppliedGenerator.addInteger(arg1Code, arg2Code, budget, params)
            case DefaultFun.SubtractInteger =>
                BuiltinAppliedGenerator.subtractInteger(arg1Code, arg2Code, budget, params)
            case DefaultFun.MultiplyInteger =>
                BuiltinAppliedGenerator.multiplyInteger(arg1Code, arg2Code, budget, params)
            case DefaultFun.DivideInteger =>
                BuiltinAppliedGenerator.divideInteger(arg1Code, arg2Code, budget, params)
            case DefaultFun.QuotientInteger =>
                BuiltinAppliedGenerator.quotientInteger(arg1Code, arg2Code, budget, params)
            case DefaultFun.RemainderInteger =>
                BuiltinAppliedGenerator.remainderInteger(arg1Code, arg2Code, budget, params)
            case DefaultFun.ModInteger =>
                BuiltinAppliedGenerator.modInteger(arg1Code, arg2Code, budget, params)
            case DefaultFun.EqualsInteger =>
                BuiltinAppliedGenerator.equalsInteger(arg1Code, arg2Code, budget, params)
            case DefaultFun.LessThanInteger =>
                BuiltinAppliedGenerator.lessThanInteger(arg1Code, arg2Code, budget, params)
            case DefaultFun.LessThanEqualsInteger =>
                BuiltinAppliedGenerator.lessThanEqualsInteger(arg1Code, arg2Code, budget, params)
            // ByteString operations
            case DefaultFun.AppendByteString =>
                BuiltinAppliedGenerator.appendByteString(arg1Code, arg2Code, budget, params)
            case DefaultFun.ConsByteString =>
                BuiltinAppliedGenerator.consByteString(arg1Code, arg2Code, budget, params)
            case DefaultFun.IndexByteString =>
                BuiltinAppliedGenerator.indexByteString(arg1Code, arg2Code, budget, params)
            case DefaultFun.EqualsByteString =>
                BuiltinAppliedGenerator.equalsByteString(arg1Code, arg2Code, budget, params)
            case DefaultFun.LessThanByteString =>
                BuiltinAppliedGenerator.lessThanByteString(arg1Code, arg2Code, budget, params)
            case DefaultFun.LessThanEqualsByteString =>
                BuiltinAppliedGenerator.lessThanEqualsByteString(
                  arg1Code,
                  arg2Code,
                  budget,
                  params
                )
            // String operations
            case DefaultFun.AppendString =>
                BuiltinAppliedGenerator.appendString(arg1Code, arg2Code, budget, params)
            case DefaultFun.EqualsString =>
                BuiltinAppliedGenerator.equalsString(arg1Code, arg2Code, budget, params)
            // Data operations
            case DefaultFun.EqualsData =>
                BuiltinAppliedGenerator.equalsData(arg1Code, arg2Code, budget, params)
            case DefaultFun.ConstrData =>
                BuiltinAppliedGenerator.constrData(arg1Code, arg2Code, budget, params)
            case DefaultFun.MkPairData =>
                BuiltinAppliedGenerator.mkPairData(arg1Code, arg2Code, budget, params)
            // List operations
            case DefaultFun.MkCons =>
                BuiltinAppliedGenerator.mkCons(arg1Code, arg2Code, budget, params)
            case _ =>
                throw IllegalStateException(
                  s"Short circuit optimization for builtin $bn not implemented"
                )

}
