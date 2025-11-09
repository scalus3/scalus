package scalus.compiler.sir.lowering

import scalus.compiler.sir.lowering.LoweredValue.Builder.*
import scalus.sir.*
import scalus.sir.SIRType.TypeVar

object ScalusRuntime {

    val PAIRS_LIST_TO_DATA_LIST_NAME = "$PairsListToDataList"
    val TUPLES_LIST_TO_DATA_LIST_NAME = "$TuplesListToDataList"

    val DATA_LIST_TO_PAIRS_LIST_NAME = "$dataListToPairsList"
    val DATA_LIST_TO_TUPLES_LIST_NAME = "$dataListToTuplesList"

    /** Add to context scope lazy val with runtime functions.
      * @param lctx
      * @return
      */
    def initContext(lctx: LoweringContext): Unit = {
        initPairDataListToDataList(using lctx)
        initDataListToPairDataList(using lctx)
        lctx.zCombinatorNeeded = false
        // will set to true when some of initialized function will be used
    }

    def pairsListToDataList(using lctx: LoweringContext): LoweredValue = {
        retrieveRuntimeFunction(PAIRS_LIST_TO_DATA_LIST_NAME)
    }
    def tuplesListToDataList(using lctx: LoweringContext): LoweredValue = {
        // TODO: output performance warning.
        // throw new RuntimeException(TUPLES_LIST_TO_DATA_LIST_NAME)
        retrieveRuntimeFunction(TUPLES_LIST_TO_DATA_LIST_NAME)
    }
    def dataListToPairsList(using lctx: LoweringContext): LoweredValue = {
        retrieveRuntimeFunction(DATA_LIST_TO_PAIRS_LIST_NAME)
    }
    def dataListToTuplesList(using lctx: LoweringContext): LoweredValue = {
        // TODO: output performance warning.
        // throw new RuntimeException("DATA_LIST_TO_TUPLES_LIST_NAME")
        retrieveRuntimeFunction(DATA_LIST_TO_TUPLES_LIST_NAME)
    }

    private def retrieveRuntimeFunction(
        name: String
    )(using lctx: LoweringContext): LoweredValue = {
        lctx.scope.getByName(name) match {
            case Some(lv) =>
                lctx.zCombinatorNeeded = true
                lv
            case None =>
                println(s"scope=${lctx.scope}")
                throw IllegalStateException(
                  s"Can't find scalus runtime function ${name} in context, check that context is initialized"
                )
        }
    }

    private def initPairDataListToDataList(using lctx: LoweringContext): Unit = {
        val name = TUPLES_LIST_TO_DATA_LIST_NAME
        val name1 = "fun_" + name
        val rhs = genPairDataListToDataList(name1)(using lctx)
        val retvalForTuples =
            lvNewLazyNamedVar(name, rhs.sirType, rhs.representation, rhs, AnnotationsDecl.empty.pos)
        // will ne the same uplc code but different types
        val lambdaForPairs = SIRType.TypeLambda2(
          "A",
          "B",
          (ta, tb) =>
              SIRType.List(SIRType.BuiltinPair(ta, tb)) ->: SIRType.List(
                SIRType.BuiltinPair(ta, tb)
              ),
          false
        )
        val proxy = TypeRepresentationProxyLoweredValue(
          retvalForTuples,
          lambdaForPairs,
          LambdaRepresentation(
            lambdaForPairs,
            InOutRepresentationPair(
              SumCaseClassRepresentation.SumDataPairList,
              SumCaseClassRepresentation.SumDataList
            )
          ),
          AnnotationsDecl.empty.pos
        )
        val retvalForPairs =
            lvNewLazyNamedVar(
              PAIRS_LIST_TO_DATA_LIST_NAME,
              proxy.sirType,
              proxy.representation,
              proxy,
              AnnotationsDecl.empty.pos
            )
    }

    private def initDataListToPairDataList(using lctx: LoweringContext): LoweredValue = {
        val nameTuples = DATA_LIST_TO_TUPLES_LIST_NAME
        val name1 = "fun_" + nameTuples
        val rhs = genDataListToPairDataList(name1)(using lctx)
        // top-level, not need to set ScopeBrackets
        val funTuples = lvNewLazyNamedVar(
          nameTuples,
          rhs.sirType,
          rhs.representation,
          rhs,
          AnnotationsDecl.empty.pos
        )
        val lambdaType = SIRType.TypeLambda2(
          "A",
          "B",
          (ta, tb) => SIRType.List(SIRType.Tuple2(ta, tb)) ->: SIRType.List(SIRType.Tuple2(ta, tb)),
          false
        )
        val tailsProxy = TypeRepresentationProxyLoweredValue(
          funTuples,
          lambdaType,
          LambdaRepresentation(
            lambdaType,
            InOutRepresentationPair(
              SumCaseClassRepresentation.SumDataList,
              SumCaseClassRepresentation.SumDataPairList
            )
          ),
          AnnotationsDecl.empty.pos
        )
        lvNewLazyNamedVar(
          DATA_LIST_TO_PAIRS_LIST_NAME,
          tailsProxy.sirType,
          tailsProxy.representation,
          tailsProxy,
          AnnotationsDecl.empty.pos
        )

    }

    /*
    private def initDataListToPairDataList_Tuple(
        name: String
    )(using lctx: LoweringContext): LoweredValue = {
        val name1 = "fun_" + name
        val rhs = genDataListToPairDataList(name1)(using lctx)
        lvNewLazyNamedVar(name, rhs.sirType, rhs.representation, rhs, AnnotationsDecl.empty.pos)
    }
    
     */

    private def genPairDataListToDataList(name: String)(using LoweringContext): LoweredValue = {
        val hc = name.hashCode
        val tpA = SIRType.TypeVar("A", Some(hc), isBuiltin = false)
        val tpB = SIRType.TypeVar("B", Some(hc), isBuiltin = false)
        val tpInTuple = SIRType.Tuple2(tpA, tpB)
        val tpInTupleList = SIRType.List(tpInTuple)
        val tpOutTupleList = SIRType.List(SIRType.Tuple2(tpA, tpB))
        val funType =
            SIRType.Fun(tpInTupleList, tpOutTupleList)
        val lambdaType = SIRType.TypeLambda(List(tpA, tpB), funType)
        val lambdaRepr = LambdaRepresentation(
          lambdaType,
          InOutRepresentationPair(
            SumCaseClassRepresentation.SumDataPairList,
            SumCaseClassRepresentation.SumDataList
          )
        )

        val whenNil = {
            lvBuiltinApply0(
              SIRBuiltins.mkNilData,
              tpOutTupleList,
              SumCaseClassRepresentation.SumDataList,
              AnnotationsDecl.empty.pos
            )
        }

        def whenCons(
            l: IdentifiableLoweredValue,
            acceptHeadTail: (IdentifiableLoweredValue, IdentifiableLoweredValue) => LoweredValue
        ): LoweredValue = {
            processCons(
              l,
              acceptHeadTail,
              tpInTupleList,
              tpInTuple,
              SumCaseClassRepresentation.SumDataPairList,
              ProductCaseClassRepresentation.PairData
            )
        }

        def pairDataToTupleAsData(
            head: IdentifiableLoweredValue,
            tail: IdentifiableLoweredValue,
            recFun: IdentifiableLoweredValue,
        ): LoweredValue = {
            val first = lvBuiltinApply(
              SIRBuiltins.fstPair,
              head,
              tpA,
              TypeVarRepresentation(false),
              AnnotationsDecl.empty.pos
            )
            val second = lvBuiltinApply(
              SIRBuiltins.sndPair,
              head,
              tpB,
              TypeVarRepresentation(false),
              AnnotationsDecl.empty.pos
            )
            // val tupleDecl = SIRType.Tuple2.constrDecl
            val dataNil = lvDataNil(AnnotationsDecl.empty.pos)
            val t1 = lvBuiltinApply2(
              SIRBuiltins.mkCons,
              second,
              dataNil,
              SIRType.List(SIRType.Data),
              SumCaseClassRepresentation.SumDataList,
              AnnotationsDecl.empty.pos
            )
            val t2 = lvBuiltinApply2(
              SIRBuiltins.mkCons,
              first,
              t1,
              SIRType.List(SIRType.Data),
              SumCaseClassRepresentation.SumDataList,
              AnnotationsDecl.empty.pos
            )
            val tupleInTvRepr = lvBuiltinApply2(
              SIRBuiltins.constrData,
              lvIntConstant(0, AnnotationsDecl.empty.pos),
              t2,
              SIRType.Tuple2(tpA, tpB),
              ProductCaseClassRepresentation.ProdDataConstr,
              AnnotationsDecl.empty.pos
            )
            val recCons = lvApply(
              recFun,
              tail,
              AnnotationsDecl.empty.pos,
              Some(tpOutTupleList),
              Some(SumCaseClassRepresentation.SumDataList)
            )
            lvBuiltinApply2(
              SIRBuiltins.mkCons,
              tupleInTvRepr,
              recCons,
              tpOutTupleList,
              SumCaseClassRepresentation.SumDataList,
              AnnotationsDecl.empty.pos
            )
        }

        val letDef = lvLetRec(
          name,
          lambdaType,
          lambdaRepr,
          rec =>
              lvLamAbs(
                "list",
                tpInTupleList,
                SumCaseClassRepresentation.SumDataPairList,
                list =>
                    lvChooseList(
                      list,
                      whenNil,
                      whenCons(
                        list,
                        (head, tail) => pairDataToTupleAsData(head, tail, rec),
                      ),
                      tpOutTupleList,
                      SumCaseClassRepresentation.SumDataList
                    ),
                AnnotationsDecl.empty.pos
              ),
          rec => rec,
          AnnotationsDecl.empty.pos
        )
        letDef

    }

    private def genDataListToPairDataList(
        name: String
    )(using lctx: LoweringContext): LoweredValue = {
        val hc = name.hashCode
        val tpA = SIRType.TypeVar("A", Some(hc), isBuiltin = false)
        val tpB = SIRType.TypeVar("B", Some(hc), isBuiltin = false)
        val tpOutPair = SIRType.Tuple2(tpA, tpB)
        val tpOutPairList = SIRType.List(tpOutPair)
        val tpInTupleList = SIRType.List(SIRType.Tuple2(tpA, tpB))
        val funType =
            SIRType.Fun(tpInTupleList, tpOutPairList)
        val lambdaType = SIRType.TypeLambda(List(tpA, tpB), funType)
        val lambdaRepr = LambdaRepresentation(
          lambdaType,
          InOutRepresentationPair(
            SumCaseClassRepresentation.SumDataList,
            SumCaseClassRepresentation.SumDataPairList
          )
        )

        val whenNil = lvBuiltinApply0(
          SIRBuiltins.mkNilPairData,
          tpOutPairList,
          SumCaseClassRepresentation.SumDataPairList,
          AnnotationsDecl.empty.pos
        )

        def whenCons(
            l: IdentifiableLoweredValue,
            acceptHeadTail: (IdentifiableLoweredValue, IdentifiableLoweredValue) => LoweredValue
        ): LoweredValue = {
            processCons(
              l,
              acceptHeadTail,
              tpInTupleList,
              SIRType.Tuple2(tpA, tpB),
              SumCaseClassRepresentation.SumDataList,
              ProductCaseClassRepresentation.ProdDataConstr
            )
        }

        def mapTupleToPair(
            head: IdentifiableLoweredValue,
            tail: IdentifiableLoweredValue,
            funRec: LoweredValue
        ): LoweredValue = {
            val pairIntData = lvBuiltinApply(
              SIRBuiltins.unConstrData,
              head,
              SIRType.Tuple2(tpA, tpB),
              ProductCaseClassRepresentation.PairIntDataList,
              AnnotationsDecl.empty.pos
            )
            val prodList = lvBuiltinApply(
              SIRBuiltins.sndPair,
              pairIntData,
              SIRType.List(SIRType.Data),
              SumCaseClassRepresentation.SumDataList,
              AnnotationsDecl.empty.pos
            )
            val prodListId = lctx.uniqueVarName("prodList")
            val prodListVal = new VariableLoweredValue(
              id = prodListId,
              name = prodListId,
              sir = SIR.Var(prodListId, SIRType.List(SIRType.Data), AnnotationsDecl.empty),
              representation = SumCaseClassRepresentation.SumDataList,
              optRhs = Some(prodList),
            )
            val firstProdList = lvBuiltinApply(
              SIRBuiltins.headList,
              prodListVal,
              SIRType.Data,
              PrimitiveRepresentation.PackedData,
              AnnotationsDecl.empty.pos
            )
            val tailProdList = lvBuiltinApply(
              SIRBuiltins.tailList,
              prodListVal,
              SIRType.List(SIRType.Data),
              SumCaseClassRepresentation.SumDataList,
              AnnotationsDecl.empty.pos
            )
            val secondProdList = lvBuiltinApply(
              SIRBuiltins.headList,
              tailProdList,
              SIRType.Data,
              PrimitiveRepresentation.PackedData,
              AnnotationsDecl.empty.pos
            )
            val pair = lvBuiltinApply2(
              SIRBuiltins.mkPairData,
              firstProdList,
              secondProdList,
              tpOutPair,
              ProductCaseClassRepresentation.PairData,
              AnnotationsDecl.empty.pos
            )
            val recCons = lvApply(
              funRec,
              tail,
              AnnotationsDecl.empty.pos,
              Some(tpOutPairList),
              Some(SumCaseClassRepresentation.SumDataPairList)
            )
            val cons = lvBuiltinApply2(
              SIRBuiltins.mkCons,
              pair,
              recCons,
              tpOutPairList,
              SumCaseClassRepresentation.SumDataPairList,
              AnnotationsDecl.empty.pos
            )
            cons
        }

        val letDef = lvLetRec(
          name,
          lambdaType,
          lambdaRepr,
          rec =>
              lvLamAbs(
                "list",
                tpInTupleList,
                SumCaseClassRepresentation.SumDataList,
                list =>
                    lvChooseList(
                      list,
                      whenNil,
                      whenCons(list, (head, tail) => mapTupleToPair(head, tail, rec)),
                      tpOutPairList,
                      SumCaseClassRepresentation.SumDataPairList
                    ),
                AnnotationsDecl.empty.pos
              ),
          rec => rec,
          AnnotationsDecl.empty.pos
        )

        letDef
    }

    private def lvChooseList(
        l: IdentifiableLoweredValue,
        t1: LoweredValue,
        t2: LoweredValue,
        outType: SIRType,
        outRepresentation: LoweredValueRepresentation
    )(using lctx: LoweringContext): LoweredValue = {

        lvForce(
          lvApply(
            lvApply(
              lvBuiltinApply(
                SIRBuiltins.chooseList,
                l,
                outType ->: outType ->: outType,
                LambdaRepresentation(
                  outType ->: outType ->: outType,
                  InOutRepresentationPair(
                    outRepresentation,
                    LambdaRepresentation(
                      outType ->: outType,
                      InOutRepresentationPair(
                        outRepresentation,
                        outRepresentation
                      )
                    )
                  )
                ),
                AnnotationsDecl.empty.pos
              ),
              lvDelay(t1, AnnotationsDecl.empty.pos),
              AnnotationsDecl.empty.pos,
              Some(outType ->: outType),
              Some(
                LambdaRepresentation(
                  outType ->: outType,
                  InOutRepresentationPair(outRepresentation, outRepresentation)
                )
              )
            ),
            lvDelay(t2, AnnotationsDecl.empty.pos),
            AnnotationsDecl.empty.pos,
            Some(outType),
            Some(outRepresentation)
          ),
          AnnotationsDecl.empty.pos
        )
    }

    private def processCons(
        l: IdentifiableLoweredValue,
        acceptHeadTail: (IdentifiableLoweredValue, IdentifiableLoweredValue) => LoweredValue,
        inListType: SIRType,
        inElementType: SIRType,
        inListRepresentation: SumCaseClassRepresentation,
        inElementRepresentation: LoweredValueRepresentation,
    )(using lctx: LoweringContext): LoweredValue = {
        val head = lvBuiltinApply(
          SIRBuiltins.headList,
          l,
          inElementType,
          inElementRepresentation,
          AnnotationsDecl.empty.pos
        )
        val headValId = lctx.uniqueVarName("headVal")
        val headVal = new VariableLoweredValue(
          id = headValId,
          name = headValId,
          sir = SIR.Var(headValId, inElementType, AnnotationsDecl.empty),
          representation = inElementRepresentation,
          optRhs = Some(head)
        )
        val tail = lvBuiltinApply(
          SIRBuiltins.tailList,
          l,
          inListType,
          inListRepresentation,
          AnnotationsDecl.empty.pos
        )
        val tailValId = lctx.uniqueVarName("tailVal")
        val tailVal = new VariableLoweredValue(
          id = tailValId,
          name = tailValId,
          sir = SIR.Var(tailValId, inListType, AnnotationsDecl.empty),
          representation = inListRepresentation,
          optRhs = Some(tail)
        )
        acceptHeadTail(headVal, tailVal)
    }

}
