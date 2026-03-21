package scalus.compiler.sir.lowering

import scalus.cardano.ledger.MajorProtocolVersion
import scalus.compiler.sir.lowering.LoweredValue.Builder.*
import scalus.compiler.sir.*

object ScalusRuntime {

    val ARRAY_TO_LIST_NAME = "$arrayToList"
    val MAP_LIST_NAME = "$mapList"

    /** Add to context scope lazy val with runtime functions.
      * @param lctx
      * @return
      */
    def initContext(lctx: LoweringContext): Unit = {
        initArrayToList(using lctx)
        initMapList(using lctx)
        lctx.zCombinatorNeeded = false
        // will set to true when some of initialized function will be used
    }

    def arrayToList(using lctx: LoweringContext): LoweredValue = {
        retrieveRuntimeFunction(ARRAY_TO_LIST_NAME)
    }

    def mapList(using lctx: LoweringContext): LoweredValue = {
        retrieveRuntimeFunction(MAP_LIST_NAME)
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

    /** Unified list matching that uses Case on list for PlutusV4 and ChooseList for V1-V3.
      *
      * @param list
      *   the list to match on
      * @param nilBody
      *   the body for the empty list case
      * @param consBodyFn
      *   function that takes head and tail variables and returns the cons body
      * @param listType
      *   the SIR type of the list
      * @param elementType
      *   the SIR type of list elements
      * @param listRepresentation
      *   the representation of the list
      * @param elementRepresentation
      *   the representation of list elements
      * @param outType
      *   the output type
      * @param outRepresentation
      *   the output representation
      */
    private def lvMatchList(
        list: IdentifiableLoweredValue,
        nilBody: LoweredValue,
        consBodyFn: (IdentifiableLoweredValue, IdentifiableLoweredValue) => LoweredValue,
        listType: SIRType,
        elementType: SIRType,
        listRepresentation: SumCaseClassRepresentation,
        elementRepresentation: LoweredValueRepresentation,
        outType: SIRType,
        outRepresentation: LoweredValueRepresentation
    )(using lctx: LoweringContext): LoweredValue = {
        if lctx.targetProtocolVersion >= MajorProtocolVersion.vanRossemPV then {
            // For PlutusV4: use Case on list with head/tail as lambda parameters
            val headValId = lctx.uniqueVarName("headVal")
            val headVal = new VariableLoweredValue(
              id = headValId,
              name = headValId,
              sir = SIR.Var(headValId, elementType, AnnotationsDecl.empty),
              representation = elementRepresentation,
              optRhs = None // lambda parameter, not derived from builtin
            )
            val tailValId = lctx.uniqueVarName("tailVal")
            val tailVal = new VariableLoweredValue(
              id = tailValId,
              name = tailValId,
              sir = SIR.Var(tailValId, listType, AnnotationsDecl.empty),
              representation = listRepresentation,
              optRhs = None // lambda parameter, not derived from builtin
            )
            val consBody = consBodyFn(headVal, tailVal)
            CaseListLoweredValue(
              list,
              headVal,
              tailVal,
              consBody,
              Some(nilBody),
              outType,
              outRepresentation,
              AnnotationsDecl.empty.pos
            )
        } else {
            // For V1-V3: use ChooseList with head/tail derived from builtins
            val consBody = processCons(
              list,
              consBodyFn,
              listType,
              elementType,
              listRepresentation,
              elementRepresentation
            )
            lvChooseList(list, nilBody, consBody, outType, outRepresentation)
        }
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

    /** Initialize arrayToList runtime function.
      *
      * arrayToList converts a BuiltinArray[Data] to a BuiltinList[Data] by using multiIndexArray
      * with indices [0, 1, 2, ..., length-1].
      *
      * Implementation: arrayToList arr = multiIndexArray (mkIndices (lengthOfArray arr)) arr where
      * mkIndices builds a list [0, 1, 2, ..., n-1]
      */
    private def initArrayToList(using lctx: LoweringContext): Unit = {
        val name = ARRAY_TO_LIST_NAME
        val rhs = genArrayToList(name)
        lvNewLazyNamedVar(name, rhs.sirType, rhs.representation, rhs, AnnotationsDecl.empty.pos)
    }

    private def genArrayToList(name: String)(using lctx: LoweringContext): LoweredValue = {
        // Type: BuiltinArray[Data] -> BuiltinList[Data]
        // Implementation: iterate from index (n-1) down to 0, building the list
        // arrayToListFrom(arr, i) = if i < 0 then [] else mkCons(arr[i], arrayToListFrom(arr, i-1))
        // arrayToList(arr) = arrayToListFrom(arr, lengthOfArray(arr) - 1)
        //
        // Actually, we need to build from front to back to preserve order:
        // arrayToListFromTo(arr, i, n) = if i >= n then [] else mkCons(arr[i], arrayToListFromTo(arr, i+1, n))
        // arrayToList(arr) = arrayToListFromTo(arr, 0, lengthOfArray(arr))

        val helperName = name + "_helper"
        val elemType = SIRType.Data.tp
        val elemRepr = lctx.typeGenerator(elemType).defaultDataRepresentation(elemType)
        val listRepr = SumCaseClassRepresentation.SumBuiltinList(elemRepr)
        // Helper type: BuiltinArray[Data] -> Integer -> Integer -> BuiltinList[Data]
        val helperType =
            SIRType.BuiltinArray(
              elemType
            ) ->: SIRType.Integer ->: SIRType.Integer ->: SIRType
                .BuiltinList(elemType)

        val innerFunType =
            SIRType.Integer ->: SIRType.Integer ->: SIRType.BuiltinList(elemType)
        val innerInnerFunType = SIRType.Integer ->: SIRType.BuiltinList(elemType)

        val helperRepr = LambdaRepresentation(
          helperType,
          InOutRepresentationPair(
            PrimitiveRepresentation.Constant,
            LambdaRepresentation(
              innerFunType,
              InOutRepresentationPair(
                PrimitiveRepresentation.Constant,
                LambdaRepresentation(
                  innerInnerFunType,
                  InOutRepresentationPair(
                    PrimitiveRepresentation.Constant,
                    listRepr
                  )
                )
              )
            )
          )
        )

        // Build helper using letRec
        val helperDef = lvLetRec(
          helperName,
          helperType,
          helperRepr,
          rec =>
              lvLamAbs(
                "arr",
                SIRType.BuiltinArray(SIRType.Data.tp),
                PrimitiveRepresentation.Constant,
                arr =>
                    lvLamAbs(
                      "i",
                      SIRType.Integer,
                      PrimitiveRepresentation.Constant,
                      i =>
                          lvLamAbs(
                            "n",
                            SIRType.Integer,
                            PrimitiveRepresentation.Constant,
                            n => {
                                // if i >= n then [] else mkCons(arr[i], helper(arr, i+1, n))
                                val iGeN = lvBuiltinApply2(
                                  SIRBuiltins.lessThanEqualsInteger,
                                  n,
                                  i,
                                  SIRType.Boolean,
                                  PrimitiveRepresentation.Constant,
                                  AnnotationsDecl.empty.pos
                                )
                                val nilCase = lvDataNil(
                                  AnnotationsDecl.empty.pos,
                                  SIRType.BuiltinList(SIRType.Data.tp),
                                  listRepr
                                )
                                // arr[i]
                                val elem = lvBuiltinApply2(
                                  SIRBuiltins.indexArray,
                                  arr,
                                  i,
                                  SIRType.Data.tp,
                                  PrimitiveRepresentation.PackedData,
                                  AnnotationsDecl.empty.pos
                                )
                                // i + 1
                                val iPlus1 = lvBuiltinApply2(
                                  SIRBuiltins.addInteger,
                                  i,
                                  lvIntConstant(1, AnnotationsDecl.empty.pos),
                                  SIRType.Integer,
                                  PrimitiveRepresentation.Constant,
                                  AnnotationsDecl.empty.pos
                                )
                                // helper(arr, i+1, n)
                                val recCall = lvApply(
                                  lvApply(
                                    lvApply(
                                      rec,
                                      arr,
                                      AnnotationsDecl.empty.pos,
                                      Some(innerFunType),
                                      None
                                    ),
                                    iPlus1,
                                    AnnotationsDecl.empty.pos,
                                    Some(innerInnerFunType),
                                    None
                                  ),
                                  n,
                                  AnnotationsDecl.empty.pos,
                                  Some(SIRType.BuiltinList(SIRType.Data.tp)),
                                  Some(listRepr)
                                )
                                // mkCons(elem, recCall)
                                val consCase = lvBuiltinApply2(
                                  SIRBuiltins.mkCons,
                                  elem,
                                  recCall,
                                  SIRType.BuiltinList(SIRType.Data.tp),
                                  listRepr,
                                  AnnotationsDecl.empty.pos
                                )
                                lvIfThenElse(
                                  iGeN,
                                  nilCase,
                                  consCase,
                                  AnnotationsDecl.empty.pos,
                                  Some(SIRType.BuiltinList(SIRType.Data.tp))
                                )
                            },
                            AnnotationsDecl.empty.pos
                          ),
                      AnnotationsDecl.empty.pos
                    ),
                AnnotationsDecl.empty.pos
              ),
          identity,
          AnnotationsDecl.empty.pos
        )

        // Now build the main arrayToList function
        val arrayToListBody = lvLamAbs(
          "arr",
          SIRType.BuiltinArray(SIRType.Data.tp),
          PrimitiveRepresentation.Constant,
          arr => {
              // length = lengthOfArray arr
              val length = lvBuiltinApply(
                SIRBuiltins.lengthOfArray,
                arr,
                SIRType.Integer,
                PrimitiveRepresentation.Constant,
                AnnotationsDecl.empty.pos
              )
              // result = helper(arr, 0, length)
              lvApply(
                lvApply(
                  lvApply(
                    helperDef,
                    arr,
                    AnnotationsDecl.empty.pos,
                    Some(innerFunType),
                    None
                  ),
                  lvIntConstant(0, AnnotationsDecl.empty.pos),
                  AnnotationsDecl.empty.pos,
                  Some(innerInnerFunType),
                  None
                ),
                length,
                AnnotationsDecl.empty.pos,
                Some(SIRType.BuiltinList(SIRType.Data.tp)),
                Some(listRepr)
              )
          },
          AnnotationsDecl.empty.pos
        )

        arrayToListBody
    }

    /** Initialize mapList runtime function.
      *
      * mapList: (A -> B) -> List[B] -> List[A] -> List[B]
      *
      * Uses lvApplyDirect to avoid automatic representation conversion — the function operates on
      * lists with TypeVar element representations that don't match default representations.
      */
    private def initMapList(using lctx: LoweringContext): Unit = {
        val name = MAP_LIST_NAME
        val rhs = genMapList(name)
        lvNewLazyNamedVar(name, rhs.sirType, rhs.representation, rhs, AnnotationsDecl.empty.pos)
    }

    /** Generate mapList: (A -> B) -> List[B] -> List[A] -> List[B]
      *
      * Structure: mapList f nil = let rec go lst = case lst of [] -> nil (h :: t) -> mkCons(f(h),
      * go(t)) in go
      *
      * `f` and `nil` are captured in closure, only `lst` is passed recursively.
      */
    private def genMapList(name: String)(using lctx: LoweringContext): LoweredValue = {
        val hc = name.hashCode
        val tpA = SIRType.TypeVar("A", Some(hc), isBuiltin = true)
        val tpB = SIRType.TypeVar("B", Some(hc), isBuiltin = true)
        val tpInList = SIRType.BuiltinList(tpA)
        val tpOutList = SIRType.BuiltinList(tpB)
        val tpFn = SIRType.Fun(tpA, tpB)

        val funType = tpFn ->: tpOutList ->: tpInList ->: tpOutList
        val lambdaType = SIRType.TypeLambda(List(tpA, tpB), funType)

        val tvReprA = TypeVarRepresentation(isBuiltin = true)
        val tvReprB = TypeVarRepresentation(isBuiltin = true)
        val tvListReprIn = SumCaseClassRepresentation.SumBuiltinList(tvReprA)
        val tvListReprOut = SumCaseClassRepresentation.SumBuiltinList(tvReprB)
        val fnRepr = LambdaRepresentation(
          tpFn,
          InOutRepresentationPair(tvReprA, tvReprB)
        )

        // go: List[A] -> List[B]
        val goType = tpInList ->: tpOutList
        val goRepr = LambdaRepresentation(
          goType,
          InOutRepresentationPair(tvListReprIn, tvListReprOut)
        )

        val innerType1 = tpOutList ->: goType
        val innerRepr1 = LambdaRepresentation(
          innerType1,
          InOutRepresentationPair(tvListReprOut, goRepr)
        )
        val lambdaRepr = LambdaRepresentation(
          lambdaType,
          InOutRepresentationPair(fnRepr, innerRepr1)
        )

        // mapList f nil = let rec go lst = ... in go
        val outerDef = lvLamAbs(
          "f",
          tpFn,
          fnRepr,
          f =>
              lvLamAbs(
                "nil",
                tpOutList,
                tvListReprOut,
                nil =>
                    lvLetRec(
                      name,
                      goType,
                      goRepr,
                      go =>
                          lvLamAbs(
                            "lst",
                            tpInList,
                            tvListReprIn,
                            lst =>
                                lvMatchList(
                                  lst,
                                  nil,
                                  (head, tail) => {
                                      val mapped = lvApplyDirect(
                                        f,
                                        head,
                                        tpB,
                                        tvReprB,
                                        AnnotationsDecl.empty.pos
                                      )
                                      val recCall = lvApplyDirect(
                                        go,
                                        tail,
                                        tpOutList,
                                        tvListReprOut,
                                        AnnotationsDecl.empty.pos
                                      )
                                      lvBuiltinApply2(
                                        SIRBuiltins.mkCons,
                                        mapped,
                                        recCall,
                                        tpOutList,
                                        tvListReprOut,
                                        AnnotationsDecl.empty.pos
                                      )
                                  },
                                  tpInList,
                                  tpA,
                                  tvListReprIn,
                                  tvReprA,
                                  tpOutList,
                                  tvListReprOut
                                ),
                            AnnotationsDecl.empty.pos
                          ),
                      go => go,
                      AnnotationsDecl.empty.pos
                    ),
                AnnotationsDecl.empty.pos
              ),
          AnnotationsDecl.empty.pos
        )
        outerDef
    }

}
