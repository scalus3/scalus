package scalus.compiler.sir.lowering

import org.typelevel.paiges.Doc
import scalus.cardano.ledger.MajorProtocolVersion
import scalus.compiler.sir.lowering.LoweredValue.Builder.*
import scalus.compiler.sir.*
import scalus.uplc.{Term, UplcAnnotation}
import scalus.compiler.sir.lowering.typegens.SumUplcConstrSirTypeGenerator

object ScalusRuntime {

    val ARRAY_TO_LIST_NAME = "$arrayToList"
    val MAP_LIST_NAME = "$mapList"
    val BUILTIN_LIST_TO_UPLC_CONSTR_NAME = "$builtinListToUplcConstr"

    /** Add to context scope lazy val with runtime functions.
      * @param lctx
      * @return
      */
    def initContext(lctx: LoweringContext): Unit = {
        initArrayToList(using lctx)
        // mapList is initialized on demand when first used
        initSupportBindings(lctx)
        lctx.zCombinatorNeeded = false
        // will set to true when some of initialized function will be used
    }

    /** Eagerly materialize every support-module binding into `lctx.scope` as a lazy named var.
      *
      * Called from [[initContext]] so repeated `SIR.ExternalVar` references to a support-module
      * binding all resolve (via `lctx.scope.getByName`) to the SAME `VariableLoweredValue`, avoiding
      * duplicate definitions and leaking pattern-bound vars across support-binding bodies.
      *
      * For bindings in the `UplcConstrListOperations` / `UplcConstrOptionOperations` modules we
      * lower under `inUplcConstrListScope = true` so inner `List.Cons(...)` / `Option.Some(...)` /
      * `List.Nil` / `Option.None` emissions pick `SumCaseUplcConstrSirTypeGenerator`.
      */
    def initSupportBindings(lctx: LoweringContext): Unit = {
        val nativeConstrModules = Set(
          "scalus.compiler.intrinsics.UplcConstrListOperations$",
          "scalus.compiler.intrinsics.UplcConstrOptionOperations$",
        )
        lctx.supportModules.foreach { case (moduleName, module) =>
            val isNativeConstr = nativeConstrModules.contains(moduleName)
            module.defs.foreach { d =>
                given LoweringContext = lctx
                val prevFlag = lctx.inUplcConstrListScope
                if isNativeConstr then lctx.inUplcConstrListScope = true
                try
                    val lowered = Lowering.lowerSIR(d.value)
                    LoweredValue.Builder.lvNewLazyNamedVar(
                      d.name,
                      d.tp,
                      lowered.representation,
                      lowered,
                      SIRPosition.empty
                    )
                finally lctx.inUplcConstrListScope = prevFlag
            }
        }
    }

    def arrayToList(using lctx: LoweringContext): LoweredValue = {
        retrieveRuntimeFunction(ARRAY_TO_LIST_NAME)
    }

    def mapList(using lctx: LoweringContext): LoweredValue = {
        lctx.scope.getByName(MAP_LIST_NAME) match {
            case Some(lv) =>
                lctx.zCombinatorNeeded = true
                lv
            case None =>
                initMapList
                lctx.scope.getByName(MAP_LIST_NAME) match {
                    case Some(lv) =>
                        lctx.zCombinatorNeeded = true
                        lv
                    case None =>
                        throw IllegalStateException(
                          s"Can't find scalus runtime function ${MAP_LIST_NAME} after init"
                        )
                }
        }
    }

    private def retrieveRuntimeFunction(
        name: String
    )(using lctx: LoweringContext): LoweredValue = {
        lctx.scope.getByName(name) match {
            case Some(lv) =>
                lctx.zCombinatorNeeded = true
                lv
            case None =>
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
        val useCase = lctx.targetProtocolVersion >= MajorProtocolVersion.vanRossemPV
        if useCase then {
            // PV>=11: use Term.Case (caseList semantics) for both SumUplcConstr and SumBuiltinList.
            // Term.Case dispatches directly on Constant.List scrutinees (see Cek.scala:1144),
            // avoiding the Delay+Force+ChooseList overhead of the PV<11 path.
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

        val arrayRepr = ProductCaseClassRepresentation.ProdBuiltinArray(elemRepr)
        val helperRepr = LambdaRepresentation(
          helperType,
          InOutRepresentationPair(
            arrayRepr,
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
                arrayRepr,
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
          arrayRepr,
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
        val tpA = SIRType.TypeVar("A", Some(hc), SIRType.TypeVarKind.Transparent)
        val tpB = SIRType.TypeVar("B", Some(hc), SIRType.TypeVarKind.Transparent)
        val tpInList = SIRType.BuiltinList(tpA)
        val tpOutList = SIRType.BuiltinList(tpB)
        val tpFn = SIRType.Fun(tpA, tpB)

        val funType = tpFn ->: tpOutList ->: tpInList ->: tpOutList
        val lambdaType = SIRType.TypeLambda(List(tpA, tpB), funType)

        val tvReprA = TypeVarRepresentation(SIRType.TypeVarKind.Transparent)
        val tvReprB = TypeVarRepresentation(SIRType.TypeVarKind.Transparent)
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

    /** Convert a builtin list (SumBuiltinList repr) to a UplcConstr chain: Cons(h,t) = Constr(0,
      * [h, t]), Nil = Constr(1, []).
      *
      * @param input
      *   the builtin list value
      * @param outSum
      *   the target SumUplcConstr representation
      * @param listType
      *   the SIR type of the list (e.g., List[Point])
      * @param pos
      *   source position
      */
    def builtinListToUplcConstr(
        input: LoweredValue,
        outSum: SumCaseClassRepresentation.SumUplcConstr,
        listType: SIRType,
        pos: SIRPosition
    )(using lctx: LoweringContext): LoweredValue = {
        if lctx.warnListConversions then
            System.err.println(
              s"[warnListConversions] SumBuiltinList→SumUplcConstr at ${pos.show}: ${listType.show}"
            )
        val elemType = SumCaseClassRepresentation.SumBuiltinList
            .retrieveListElementType(listType)
            .getOrElse {
                throw new LoweringException(
                  s"Can't retrieve element type from list type ${listType.show} for builtinListToUplcConstr",
                  pos
                )
            }
        val inListRepr = input.representation match
            case s: SumCaseClassRepresentation => s
            case other =>
                throw LoweringException(
                  s"builtinListToUplcConstr: expected SumCaseClassRepresentation, got $other for ${listType.show}",
                  pos
                )
        val inElemRepr = input.representation match
            case SumCaseClassRepresentation.SumBuiltinList(er) => er
            case other =>
                throw LoweringException(
                  s"builtinListToUplcConstr: expected SumBuiltinList input repr, got $other for ${listType.show}",
                  pos
                )
        // Get element repr from the Cons variant (tag 1 for List: Nil=0, Cons=1)
        // Find the Cons variant (has fields). If no variant has fields (placeholder), skip element conversion.
        val outConsVariant = outSum.variants.values.find(_.fieldReprs.nonEmpty)
        val outElemRepr = outConsVariant match
            case Some(v) => v.fieldReprs.head
            case None    => inElemRepr // placeholder — skip element conversion
        val inListType = SIRType.BuiltinList(SIRType.Data.tp)
        val goType = inListType ->: listType
        val goRepr = LambdaRepresentation(
          goType,
          InOutRepresentationPair(inListRepr, outSum)
        )
        // Nil = Constr(0, []) — tag 0 (first constructor in List enum)
        val constrPos = AnnotationsDecl.empty.pos // avoid capturing outer `pos` in closure
        val nilVal = new ComplexLoweredValue(Set.empty) {
            override def sirType: SIRType = listType
            override def representation: LoweredValueRepresentation = outSum
            override def pos: SIRPosition = constrPos
            override def termInternal(gctx: TermGenerationContext): Term =
                Term.Constr(
                  scalus.cardano.ledger.Word64(0L),
                  scala.List.empty,
                  UplcAnnotation(constrPos)
                )
            override def docDef(ctx: LoweredValue.PrettyPrintingContext): Doc =
                Doc.text("Constr(0)")
            override def docRef(ctx: LoweredValue.PrettyPrintingContext): Doc = docDef(ctx)
        }
        // Cache the recursive helper by (direction, listType, inListRepr, outSum, captured-env).
        // Two conversion sites with the same (type, fromRepr, toRepr) produce identical helper
        // bodies — sharing a single letrec-bound var avoids emitting N copies of the same list
        // walk. Registered via `cachedTopLevelHelpers` + `pendingTopLevelLetRecs` in the same
        // manner as `LoweringEq`'s `sumEq` helpers.
        //
        // Use `stableKey` (not `show`) so structurally-equal reprs produce the same cache
        // key — avoids over-specializing on `SumReprProxy` identity, which leaks into `show`
        // via default `Object.toString` and causes identical conversions at different call
        // sites to create duplicate helpers.
        //
        // The `captureFingerprint` term discriminates by call-site `lctx` state (TypeVar repr
        // bindings, unify env, `inUplcConstrListScope`) restricted to TypeVars in the type
        // signature. Necessary because the lvLamAbs body internally consults this state during
        // construction (e.g. element-repr resolution); without it, two callers with different
        // env state would share a single first-built helper whose RHS only matches one of them.
        // See sessions 11-15 alone-vs-combined Heisenbug analysis.
        val cacheKey =
            s"builtinListToUplcConstr|${listType.show}|${inListRepr.stableKey}|${outSum.stableKey}" +
                s"|${lctx.captureFingerprint(listType)}"
        val goVar = lctx.cachedTopLevelHelpers.get(cacheKey) match
            case Some(v) =>
                LoweringContext.traceLetRec("HIT", "builtinListToUplcConstr", cacheKey)
                v
            case None =>
                val id = lctx.uniqueVarName(BUILTIN_LIST_TO_UPLC_CONSTR_NAME)
                val v = new VariableLoweredValue(
                  id = id,
                  name = id,
                  sir = SIR.Var(id, goType, AnnotationsDecl(pos)),
                  representation = goRepr
                )
                // Register early so recursive references inside the rhs see this var.
                lctx.cachedTopLevelHelpers(cacheKey) = v
                val rhs = lvLamAbs(
                  "lst",
                  inListType,
                  inListRepr,
                  lst =>
                      lvMatchList(
                        lst,
                        nilVal,
                        (head, tail) => {
                            val convertedHead =
                                if inElemRepr == outElemRepr then head
                                else head.toRepresentation(outElemRepr, pos)
                            val recCall = lvApplyDirect(
                              v,
                              tail,
                              listType,
                              outSum,
                              pos
                            )
                            new ComplexLoweredValue(
                              Set.empty,
                              convertedHead,
                              recCall
                            ) {
                                override def sirType: SIRType = listType
                                override def representation: LoweredValueRepresentation = outSum
                                override def pos: SIRPosition = AnnotationsDecl.empty.pos
                                override def termInternal(gctx: TermGenerationContext): Term =
                                    Term.Constr(
                                      scalus.cardano.ledger.Word64(1L),
                                      scala.List(
                                        convertedHead.termWithNeededVars(gctx),
                                        recCall.termWithNeededVars(gctx)
                                      ),
                                      UplcAnnotation(AnnotationsDecl.empty.pos)
                                    )
                                override def docDef(
                                    ctx: LoweredValue.PrettyPrintingContext
                                ): Doc =
                                    Doc.text("Constr(1, ") +
                                        convertedHead.docRef(ctx) +
                                        Doc.text(", ") +
                                        recCall.docRef(ctx) +
                                        Doc.text(")")
                                override def docRef(
                                    ctx: LoweredValue.PrettyPrintingContext
                                ): Doc =
                                    docDef(ctx)
                            }
                        },
                        inListType,
                        elemType,
                        inListRepr,
                        inElemRepr,
                        listType,
                        outSum
                      ),
                  pos
                )
                lctx.pendingTopLevelLetRecs += ((v, rhs))
                LoweringContext.traceLetRec("ADD", "builtinListToUplcConstr", cacheKey)
                v
        lctx.zCombinatorNeeded = true
        lvApplyDirect(goVar, input, listType, outSum, pos)
    }

    /** Convert a UplcConstr list (Constr chain) to a builtin list (SumBuiltinList).
      *
      * Iterates the Constr chain using Case-based matching, converts each element to the target
      * element repr, and builds a builtin list with mkCons. Reverse of builtinListToUplcConstr.
      */
    def uplcConstrToBuiltinList(
        input: LoweredValue,
        outListRepr: SumCaseClassRepresentation.SumBuiltinList,
        pos: SIRPosition
    )(using lctx: LoweringContext): LoweredValue = {
        if lctx.warnListConversions then
            System.err.println(
              s"[warnListConversions] SumUplcConstr→SumBuiltinList at ${pos.show}: ${input.sirType.show}"
            )
        val listType = input.sirType
        val elemType = SumCaseClassRepresentation.SumBuiltinList
            .retrieveListElementType(listType)
            .getOrElse(SIRType.Data.tp)
        val outElemRepr = outListRepr.elementRepr
        // Resolve target element repr: TypeVarRepresentation(Fixed) → defaultTypeVarRepresentation
        val resolvedOutElemRepr = outElemRepr match
            case tvr: TypeVarRepresentation if !tvr.isBuiltin =>
                lctx.typeGenerator(elemType).defaultTypeVarReperesentation(elemType)
            case tvr: TypeVarRepresentation if tvr.isBuiltin =>
                lctx.typeGenerator(elemType).defaultRepresentation(elemType)
            case other => other
        val resolvedOutListRepr = SumCaseClassRepresentation.SumBuiltinList(resolvedOutElemRepr)
        if elemType.isInstanceOf[SIRType.TypeVar] then
            throw LoweringException(
              s"uplcConstrToBuiltinList: cannot convert with TypeVar element ${elemType.show}. " +
                  s"This conversion requires concrete element type for Data encoding. " +
                  s"Stack: ${Thread.currentThread().getStackTrace.take(20).mkString("\n  ")}",
              pos
            )
        val inSumRepr = input.representation.asInstanceOf[SumCaseClassRepresentation.SumUplcConstr]
        // Get element repr from the Cons variant (has fields)
        val inElemRepr = inSumRepr.variants.values
            .find(_.fieldReprs.nonEmpty)
            .map(_.fieldReprs.head)
            .getOrElse(resolvedOutElemRepr)
        val goType = listType ->: listType
        val goRepr = LambdaRepresentation(
          goType,
          InOutRepresentationPair(inSumRepr, resolvedOutListRepr)
        )
        // Nil for the output builtin list
        val outNil = ConstantLoweredValue(
          SIR.Const(
            scalus.uplc.Constant.List(resolvedOutElemRepr.defaultUni(elemType), scala.List.empty),
            listType,
            AnnotationsDecl(pos)
          ),
          resolvedOutListRepr
        )
        // Cache by (direction, listType, inSumRepr, resolvedOutListRepr, captured-env). See
        // the mirror caching in `builtinListToUplcConstr` for rationale.
        val cacheKey =
            s"uplcConstrToBuiltinList|${listType.show}|${inSumRepr.stableKey}|${resolvedOutListRepr.stableKey}" +
                s"|${lctx.captureFingerprint(listType)}"
        val goVar = lctx.cachedTopLevelHelpers.get(cacheKey) match
            case Some(v) =>
                LoweringContext.traceLetRec("HIT", "uplcConstrToBuiltinList", cacheKey)
                v
            case None =>
                val id = lctx.uniqueVarName("$uplcConstrToBuiltinList")
                val v = new VariableLoweredValue(
                  id = id,
                  name = id,
                  sir = SIR.Var(id, goType, AnnotationsDecl(pos)),
                  representation = goRepr
                )
                lctx.cachedTopLevelHelpers(cacheKey) = v
                val rhs = lvLamAbs(
                  "lst",
                  listType,
                  inSumRepr,
                  lst => {
                      SumUplcConstrSirTypeGenerator.genMatchUplcConstrDirect(
                        lst,
                        inSumRepr,
                        listType,
                        listType,
                        resolvedOutListRepr,
                        pos,
                        nilBody = outNil,
                        consBody = (head, tail) => {
                            val convertedHead =
                                if inElemRepr == resolvedOutElemRepr then head
                                else head.toRepresentation(resolvedOutElemRepr, pos)
                            val recCall = lvApplyDirect(
                              v,
                              tail,
                              listType,
                              resolvedOutListRepr,
                              pos
                            )
                            lvBuiltinApply2(
                              SIRBuiltins.mkCons,
                              convertedHead,
                              recCall,
                              listType,
                              resolvedOutListRepr,
                              pos
                            )
                        }
                      )
                  },
                  pos
                )
                lctx.pendingTopLevelLetRecs += ((v, rhs))
                LoweringContext.traceLetRec("ADD", "uplcConstrToBuiltinList", cacheKey)
                v
        lctx.zCombinatorNeeded = true
        lvApplyDirect(goVar, input, listType, resolvedOutListRepr, pos)
    }

}
