package scalus.compiler.sir.lowering
package typegens

import scalus.cardano.ledger.MajorProtocolVersion
import scalus.compiler.sir.lowering.LoweredValue.Builder.*
import scalus.compiler.sir.*

import scala.util.control.NonFatal

/** handle next cases: scalus.cardano.onchain.plutus.prelude.List[A]
  * scalus.uplc.builtin.BuiltinList[A] scalus.cardano.onchain.plutus.prelude.PairList[A, B]
  */
trait SumListCommonSirTypeGenerator extends SirTypeUplcGenerator {

    def defaultListRepresentation(tp: SIRType, pos: SIRPosition)(using
        LoweringContext
    ): LoweredValueRepresentation

    def defaultElementRepresentation(tp: SIRType, pos: SIRPosition)(using
        LoweringContext
    ): LoweredValueRepresentation

    /** mkNilData and mkNilPairData
      * @param pos
      * @return
      */
    def genNil(resType: SIRType, pos: SIRPosition)(using LoweringContext): LoweredValue

    override def canBeConvertedToData(tp: SIRType)(using lctx: LoweringContext): Boolean = true

    /** Convert a BuiltinList between different resolved element representations.
      *
      * Builds a conversion lambda and applies ScalusRuntime.mapList using lvApplyDirect.
      *
      * @param outputRepresentation
      *   the original target representation from toRepresentation caller
      */
    private def convertBuiltinList(
        input: LoweredValue,
        elemType: SIRType,
        resolvedIn: LoweredValueRepresentation,
        resolvedOut: LoweredValueRepresentation,
        outputRepresentation: LoweredValueRepresentation,
        pos: SIRPosition
    )(using lctx: LoweringContext): LoweredValue = {
        val inElemUplcType = resolvedIn.uplcType(elemType)
        val outElemUplcType = resolvedOut.uplcType(elemType)
        val inListType = SIRType.BuiltinList(inElemUplcType)
        val outListType = SIRType.BuiltinList(outElemUplcType)
        val isPairIn = resolvedIn match
            case _: ProductCaseClassRepresentation.ProdBuiltinPair => true
            case _                                                 => false
        val isPairOut = resolvedOut match
            case _: ProductCaseClassRepresentation.ProdBuiltinPair => true
            case _                                                 => false
        val inListRepr: LoweredValueRepresentation =
            if isPairIn then
                SumCaseClassRepresentation.SumPairBuiltinList.fromElementType(elemType, pos)
            else SumCaseClassRepresentation.SumBuiltinList(resolvedIn)
        val outListRepr: LoweredValueRepresentation =
            if isPairOut then
                SumCaseClassRepresentation.SumPairBuiltinList.fromElementType(elemType, pos)
            else SumCaseClassRepresentation.SumBuiltinList(resolvedOut)
        val outNil =
            if isPairOut then SumPairBuiltinListSirTypeGenerator.genNil(outListType, pos)
            else new SumBuiltinListSirTypeGenerator(resolvedOut).genNil(outListType, pos)
        val fnType = inElemUplcType ->: outElemUplcType
        val fnRepr = LambdaRepresentation(fnType, InOutRepresentationPair(resolvedIn, resolvedOut))
        val convFn = lvLamAbs(
          "elem",
          elemType,
          resolvedIn,
          elem => elem.toRepresentation(resolvedOut, pos),
          pos
        )
        // mapList: (A->B) -> List[B] -> List[A] -> List[B]
        val afterFnType = outListType ->: inListType ->: outListType
        val afterFnRepr = LambdaRepresentation(
          afterFnType,
          InOutRepresentationPair(
            outListRepr,
            LambdaRepresentation(
              inListType ->: outListType,
              InOutRepresentationPair(inListRepr, outListRepr)
            )
          )
        )
        val afterNilType = inListType ->: outListType
        val afterNilRepr =
            LambdaRepresentation(afterNilType, InOutRepresentationPair(inListRepr, outListRepr))

        val result = lvApplyDirect(
          lvApplyDirect(
            lvApplyDirect(
              ScalusRuntime.mapList,
              convFn,
              afterFnType,
              afterFnRepr,
              pos
            ),
            outNil,
            afterNilType,
            afterNilRepr,
            pos
          ),
          input,
          input.sirType,
          outListRepr,
          pos
        )
        // If the caller's outputRepresentation differs from outListRepr, proxy it
        if outputRepresentation == outListRepr then result
        else RepresentationProxyLoweredValue(result, outputRepresentation, pos)
    }

    private def hasConstantOrTypeVar(
        a: LoweredValueRepresentation,
        b: LoweredValueRepresentation
    ): Boolean =
        a == PrimitiveRepresentation.Constant || b == PrimitiveRepresentation.Constant ||
            a.isInstanceOf[TypeVarRepresentation] || b.isInstanceOf[TypeVarRepresentation]

    override def toRepresentation(
        input: LoweredValue,
        outputRepresentation: LoweredValueRepresentation,
        pos: SIRPosition
    )(using lctx: LoweringContext): LoweredValue = {
        (input.representation, outputRepresentation) match
            // === SumBuiltinList identity ===
            case (
                  SumCaseClassRepresentation.SumBuiltinList(inElemRepr),
                  SumCaseClassRepresentation.SumBuiltinList(outElemRepr)
                ) if inElemRepr == outElemRepr =>
                input
            // === SumBuiltinList cross-element-repr conversions ===
            // Handle when at least one side involves Constant or TypeVarRepresentation.
            case (
                  SumCaseClassRepresentation.SumBuiltinList(inElemRepr),
                  SumCaseClassRepresentation.SumBuiltinList(outElemRepr)
                ) if hasConstantOrTypeVar(inElemRepr, outElemRepr) =>
                val elemType = retrieveElementType(input.sirType, pos)
                def resolveElementRepr(
                    repr: LoweredValueRepresentation
                ): LoweredValueRepresentation =
                    repr match
                        case TypeVarRepresentation(isBuiltin) =>
                            val gen = lctx.typeGenerator(elemType)
                            if isBuiltin then gen.defaultRepresentation(elemType)
                            else gen.defaultTypeVarReperesentation(elemType)
                        case other => other
                val resolvedIn = resolveElementRepr(inElemRepr)
                val resolvedOut = resolveElementRepr(outElemRepr)
                // Builtin TypeVars are polymorphic — no conversion needed when
                // the other side has a concrete repr. UPLC builtins accept any list type.
                val hasBuiltinTypeVar = (inElemRepr, outElemRepr) match
                    case (TypeVarRepresentation(true), _) | (_, TypeVarRepresentation(true)) => true
                    case _ => false
                if resolvedIn == resolvedOut
                    || elemType == SIRType.FreeUnificator
                    || elemType == SIRType.TypeNothing
                    || hasBuiltinTypeVar
                then RepresentationProxyLoweredValue(input, outputRepresentation, pos)
                else
                    convertBuiltinList(
                      input,
                      elemType,
                      resolvedIn,
                      resolvedOut,
                      outputRepresentation,
                      pos
                    )
            // === SumBuiltinList(Constant) special cases ===
            case (
                  SumCaseClassRepresentation.SumBuiltinList(PrimitiveRepresentation.Constant),
                  SumCaseClassRepresentation.PackedSumDataList
                ) =>
                val elemType = retrieveElementType(input.sirType, pos)
                val elemRepr = lctx.typeGenerator(elemType).defaultDataRepresentation(elemType)
                input
                    .toRepresentation(SumCaseClassRepresentation.SumBuiltinList(elemRepr), pos)
                    .toRepresentation(SumCaseClassRepresentation.PackedSumDataList, pos)
            case (
                  SumCaseClassRepresentation.PackedSumDataList,
                  SumCaseClassRepresentation.SumBuiltinList(PrimitiveRepresentation.Constant)
                ) =>
                val elemType = retrieveElementType(input.sirType, pos)
                val elemRepr = lctx.typeGenerator(elemType).defaultDataRepresentation(elemType)
                input
                    .toRepresentation(SumCaseClassRepresentation.SumBuiltinList(elemRepr), pos)
                    .toRepresentation(outputRepresentation, pos)
            case (
                  SumCaseClassRepresentation.SumBuiltinList(PrimitiveRepresentation.Constant),
                  tv @ TypeVarRepresentation(_)
                ) =>
                input
                    .toRepresentation(SumCaseClassRepresentation.PackedSumDataList, pos)
                    .toRepresentation(tv, pos)
            case (
                  tv @ TypeVarRepresentation(_),
                  SumCaseClassRepresentation.SumBuiltinList(PrimitiveRepresentation.Constant)
                ) =>
                input
                    .toRepresentation(SumCaseClassRepresentation.PackedSumDataList, pos)
                    .toRepresentation(outputRepresentation, pos)
            // === SumBuiltinList → PackedSumDataList (via listData) ===
            case (
                  SumCaseClassRepresentation.SumBuiltinList(_),
                  SumCaseClassRepresentation.PackedSumDataList
                ) =>
                val elemType = retrieveElementType(input.sirType, pos)
                val asDataList =
                    if elemType == SIRType.TypeNothing || elemType == SIRType.FreeUnificator then
                        input
                    else
                        val elemRepr =
                            lctx.typeGenerator(elemType).defaultDataRepresentation(elemType)
                        input.toRepresentation(
                          SumCaseClassRepresentation.SumBuiltinList(elemRepr),
                          pos
                        )
                lvBuiltinApply(
                  SIRBuiltins.listData,
                  asDataList,
                  input.sirType,
                  SumCaseClassRepresentation.PackedSumDataList,
                  pos
                )
            // === SumBuiltinList catch-all (compatible element reprs) ===
            case (
                  SumCaseClassRepresentation.SumBuiltinList(inElemRepr),
                  out @ SumCaseClassRepresentation.SumBuiltinList(outElemRepr)
                ) =>
                if inElemRepr == outElemRepr then input
                else if (inElemRepr == PrimitiveRepresentation.PackedData && outElemRepr == SumCaseClassRepresentation.DataData)
                    || (inElemRepr == SumCaseClassRepresentation.DataData && outElemRepr == PrimitiveRepresentation.PackedData)
                then
                    throw LoweringException(
                      s"PackedData/DataData mismatch in SumBuiltinList: ${inElemRepr} -> ${outElemRepr} " +
                          s"type=${input.sirType.show} createdEx=${input.createdEx}",
                      pos
                    )
                else
                    val elemType = retrieveElementType(input.sirType, pos)
                    if inElemRepr.isCompatibleOn(elemType, outElemRepr, pos) then
                        RepresentationProxyLoweredValue(input, out, pos)
                    else convertBuiltinList(input, elemType, inElemRepr, outElemRepr, out, pos)
            // === SumBuiltinList → SumDataAssocMap (go through SumPairBuiltinList) ===
            case (
                  SumCaseClassRepresentation.SumBuiltinList(_),
                  SumCaseClassRepresentation.SumDataAssocMap
                ) =>
                val elemType = retrieveElementType(input.sirType, pos)
                val pairRepr =
                    SumCaseClassRepresentation.SumPairBuiltinList.fromElementType(elemType, pos)
                input
                    .toRepresentation(pairRepr, pos)
                    .toRepresentation(SumCaseClassRepresentation.SumDataAssocMap, pos)
            // === PackedSumDataList conversions ===
            case (
                  SumCaseClassRepresentation.PackedSumDataList,
                  out @ SumCaseClassRepresentation.SumBuiltinList(_)
                ) =>
                val elemType = retrieveElementType(input.sirType, pos)
                val elemRepr = lctx.typeGenerator(elemType).defaultDataRepresentation(elemType)
                val dataListRepr = SumCaseClassRepresentation.SumBuiltinList(elemRepr)
                val asDataList = lvBuiltinApply(
                  SIRBuiltins.unListData,
                  input,
                  input.sirType,
                  dataListRepr,
                  pos
                )
                if out == dataListRepr then asDataList
                else asDataList.toRepresentation(out, pos)
            case (
                  SumCaseClassRepresentation.PackedSumDataList,
                  SumCaseClassRepresentation.PackedSumDataList
                ) =>
                input
            case (
                  SumCaseClassRepresentation.PackedSumDataList,
                  SumCaseClassRepresentation.SumDataAssocMap
                ) =>
                val elemType = retrieveElementType(input.sirType, pos)
                val elemRepr = lctx.typeGenerator(elemType).defaultDataRepresentation(elemType)
                val pairRepr =
                    SumCaseClassRepresentation.SumPairBuiltinList.fromElementType(elemType, pos)
                input
                    .toRepresentation(SumCaseClassRepresentation.SumBuiltinList(elemRepr), pos)
                    .toRepresentation(pairRepr, pos)
                    .toRepresentation(SumCaseClassRepresentation.SumDataAssocMap, pos)
            case (
                  SumCaseClassRepresentation.PackedSumDataList,
                  out @ SumCaseClassRepresentation.SumPairBuiltinList(_, _)
                ) =>
                val elemType = retrieveElementType(input.sirType, pos)
                val elemRepr = lctx.typeGenerator(elemType).defaultDataRepresentation(elemType)
                input
                    .toRepresentation(SumCaseClassRepresentation.SumBuiltinList(elemRepr), pos)
                    .toRepresentation(out, pos)
            // === SumPairBuiltinList conversions ===
            case (
                  SumCaseClassRepresentation.SumPairBuiltinList(_, _),
                  SumCaseClassRepresentation.SumPairBuiltinList(_, _)
                ) =>
                input
            case (
                  SumCaseClassRepresentation.SumPairBuiltinList(_, _),
                  SumCaseClassRepresentation.SumDataAssocMap
                ) =>
                lvBuiltinApply(
                  SIRBuiltins.mapData,
                  input,
                  input.sirType,
                  SumCaseClassRepresentation.SumDataAssocMap,
                  pos
                )
            case (
                  SumCaseClassRepresentation.SumPairBuiltinList(_, _),
                  SumCaseClassRepresentation.PackedSumDataList
                ) =>
                input
                    .toRepresentation(SumCaseClassRepresentation.SumDataAssocMap, pos)
                    .toRepresentation(SumCaseClassRepresentation.PackedSumDataList, pos)
            case (
                  SumCaseClassRepresentation.SumPairBuiltinList(inKeyRepr, inValueRepr),
                  out @ SumCaseClassRepresentation.SumBuiltinList(outElemRepr)
                ) =>
                // PairList → regular list: element-wise conversion via convertBuiltinList
                val elemType = retrieveElementType(input.sirType, pos)
                val inElemRepr =
                    ProductCaseClassRepresentation.ProdBuiltinPair(inKeyRepr, inValueRepr)
                convertBuiltinList(input, elemType, inElemRepr, outElemRepr, out, pos)
            case (
                  SumCaseClassRepresentation.SumBuiltinList(inElemRepr),
                  out @ SumCaseClassRepresentation.SumPairBuiltinList(outKeyRepr, outValueRepr)
                ) =>
                // Regular list → PairList: element-wise conversion via convertBuiltinList
                if isNilType(input.sirType) then lvPairDataNil(pos, input.sirType, out)
                else
                    val elemType = retrieveElementType(input.sirType, pos)
                    val outElemRepr =
                        ProductCaseClassRepresentation.ProdBuiltinPair(outKeyRepr, outValueRepr)
                    convertBuiltinList(input, elemType, inElemRepr, outElemRepr, out, pos)
            // === SumDataAssocMap conversions ===
            case (
                  SumCaseClassRepresentation.SumDataAssocMap,
                  SumCaseClassRepresentation.SumPairBuiltinList(_, _)
                ) =>
                val elemType = retrieveElementType(input.sirType, pos)
                val pairRepr =
                    SumCaseClassRepresentation.SumPairBuiltinList.fromElementType(elemType, pos)
                lvBuiltinApply(
                  SIRBuiltins.unMapData,
                  input,
                  input.sirType,
                  pairRepr,
                  pos
                )
            case (SumCaseClassRepresentation.SumDataAssocMap, _) =>
                val elemType = retrieveElementType(input.sirType, pos)
                val pairRepr =
                    SumCaseClassRepresentation.SumPairBuiltinList.fromElementType(elemType, pos)
                input
                    .toRepresentation(pairRepr, pos)
                    .toRepresentation(outputRepresentation, pos)
            // === TypeVarRepresentation ===
            case (_, tv @ TypeVarRepresentation(isBuiltin)) =>
                if isBuiltin then input
                else {
                    val inputAsData =
                        input.toRepresentation(SumCaseClassRepresentation.PackedSumDataList, pos)
                    new RepresentationProxyLoweredValue(inputAsData, tv, pos)
                }
            case (TypeVarRepresentation(isBuiltin), _) =>
                if isBuiltin then RepresentationProxyLoweredValue(input, outputRepresentation, pos)
                else if input.representation == outputRepresentation then input
                else
                    val r0 = RepresentationProxyLoweredValue(
                      input,
                      SumCaseClassRepresentation.PackedSumDataList,
                      pos
                    )
                    r0.toRepresentation(outputRepresentation, pos)
            case _ =>
                throw LoweringException(
                  s"Unexpected representation conversion for ${input.sirType.show} from ${input.representation} to ${outputRepresentation}",
                  pos
                )
    }

    override def upcastOne(
        input: LoweredValue,
        targetType: SIRType,
        pos: SIRPosition
    )(using lctx: LoweringContext): LoweredValue = {
        val targetElementType = retrieveElementType(
          targetType,
          pos
        )
        // Preserve the input's element type when the target uses FreeUnificator.
        // subtypeSeq replaces type params with FreeUnificator (no variance tracking),
        // but losing the concrete element type breaks downstream repr conversions.
        val effectiveTargetType =
            if targetElementType == SIRType.FreeUnificator then input.sirType
            else targetType
        input.representation match {
            case SumCaseClassRepresentation.SumBuiltinList(_) |
                SumCaseClassRepresentation.SumPairBuiltinList(_, _) |
                SumCaseClassRepresentation.PackedSumDataList =>
                TypeRepresentationProxyLoweredValue(
                  input,
                  effectiveTargetType,
                  input.representation,
                  pos
                )
            case SumCaseClassRepresentation.SumDataAssocMap =>
                if SirTypeUplcGenerator.isPairOrTuple2(targetElementType) then
                    TypeRepresentationProxyLoweredValue(
                      input,
                      effectiveTargetType,
                      input.representation,
                      pos
                    )
                else
                    TypeRepresentationProxyLoweredValue(
                      input,
                      effectiveTargetType,
                      input.representation,
                      pos
                    )
            case TypeVarRepresentation(isBuiltin) =>
                val targetRepresentation = {
                    if isBuiltin then defaultRepresentation(input.sirType)
                    else this.defaultTypeVarReperesentation(input.sirType)
                }
                val alignedInput = input.toRepresentation(
                  targetRepresentation,
                  pos
                )
                upcastOne(alignedInput, effectiveTargetType, pos)
            case _ =>
                throw LoweringException(
                  s"Unexpected representation ${input.representation.show} for List upcast from ${input.sirType.show} to ${targetType.show}",
                  pos
                )
        }
    }

    override def genConstr(constr: SIR.Constr)(using lctx: LoweringContext): LoweredValue = {
        import SumListCommonSirTypeGenerator.*
        constr.name match
            case SIRType.List.NilConstr.name | SIRType.BuiltinList.Nil.name | PairNilName =>
                genNil(constr.tp, constr.anns.pos)
            case SIRType.List.Cons.name | SIRType.BuiltinList.Cons.name | PairConsName =>
                if constr.args.size != 2 then
                    throw LoweringException(
                      s"Constr construnctor with ${constr.args.size} args, should be 2",
                      constr.anns.pos
                    )
                val head = lctx.lower(constr.args.head)
                val tail = lctx.lower(constr.args.tail.head)
                val elementType = retrieveElementType(constr.tp, constr.anns.pos)
                val headElementUpcasted = head
                    .maybeUpcast(elementType, constr.anns.pos)
                val headElementRepr =
                    try
                        headElementUpcasted.toRepresentation(
                          defaultElementRepresentation(elementType, constr.anns.pos),
                          constr.anns.pos
                        )
                    catch
                        case NonFatal(ex) =>
                            println(
                              s"error in genConstr for List, head.sirType: ${head.sirType.show}, tail.sirType: ${tail.sirType.show}, constr.tp=${constr.tp.show}"
                            )
                            println(s"elementType: ${elementType.show}")
                            println(
                              s"defaultElementRepresentation: ${defaultElementRepresentation(elementType, constr.anns.pos).show}"
                            )
                            throw ex
                // special case when tail is Nil, than have type List[Nothing]
                val listRepr = defaultListRepresentation(constr.tp, constr.anns.pos)
                val fixedTail =
                    if isNilType(tail.sirType) then fixNilInConstr(tail, constr.tp, listRepr)
                    else tail

                val tailElementRepr = {
                    try fixedTail.toRepresentation(listRepr, constr.anns.pos)
                    catch
                        case NonFatal(ex) =>
                            println(
                              s"error in genConstr for List, head.sirType: ${head.sirType.show}, tail.sirType: ${tail.sirType.show}, constr.tp=${constr.tp.show}"
                            )
                            println(s"relementType: ${elementType.show}")
                            println(s"defaultListRepresentation: ${listRepr.show}")
                            println(s"tail.sirType: ${tail.sirType.show}")
                            throw ex
                }
                lvBuiltinApply2(
                  SIRBuiltins.mkCons,
                  headElementRepr,
                  tailElementRepr,
                  constr.tp,
                  listRepr,
                  constr.anns.pos
                )
            case _ =>
                throw LoweringException(
                  s"Unknown constructor ${constr.name} for List",
                  constr.anns.pos
                )
    }

    def isNilType(tp: SIRType): Boolean = {
        import SumListCommonSirTypeGenerator.*
        SIRType.retrieveConstrDecl(tp) match {
            case Left(r) => false
            case Right(constrDecl) =>
                constrDecl.name == SIRType.List.NilConstr.name
                || constrDecl.name == SIRType.BuiltinList.Nil.name
                || constrDecl.name == PairNilName
        }
    }

    def fixNilInConstr(
        input: LoweredValue,
        targetListType: SIRType,
        targetRepr: LoweredValueRepresentation
    )(using lctx: LoweringContext): LoweredValue = {
        if input.representation == targetRepr then input
        else if input.isConstant then genNil(targetListType, input.pos)
        else
            throw LoweringException(
              s"Implementation restriction: can't use non-standard expression of type Nil",
              input.pos
            )
    }

    override def genSelect(sel: SIR.Select, loweredScrutinee: LoweredValue)(using
        lctx: LoweringContext
    ): LoweredValue = {
        val (scrutineeReady, listRepr, elemRepr) = loweredScrutinee.representation match
            case sbl @ SumCaseClassRepresentation.SumBuiltinList(er) =>
                (loweredScrutinee, sbl, er)
            case _ =>
                val elemType = retrieveElementType(loweredScrutinee.sirType, sel.anns.pos)
                val er = lctx.typeGenerator(elemType).defaultDataRepresentation(elemType)
                val lr = SumCaseClassRepresentation.SumBuiltinList(er)
                (loweredScrutinee.toRepresentation(lr, sel.anns.pos), lr, er)
        sel.field match {
            case "head" =>
                lvBuiltinApply(
                  SIRBuiltins.headList,
                  scrutineeReady,
                  sel.tp,
                  elemRepr,
                  sel.anns.pos
                )
            case "tail" =>
                lvBuiltinApply(
                  SIRBuiltins.tailList,
                  scrutineeReady,
                  sel.tp,
                  listRepr,
                  sel.anns.pos
                )
            case "isNull" =>
                // isNull is not a field, but a method, that returns true if list is empty
                lvBuiltinApply(
                  SIRBuiltins.nullList,
                  scrutineeReady,
                  SIRType.Boolean,
                  PrimitiveRepresentation.Constant,
                  sel.anns.pos
                )
            case _ =>
                throw LoweringException(
                  s"Unknown field ${sel.field} for List, which have 'head' and 'tail' fields and isNull method",
                  sel.anns.pos
                )
        }

    }

    def retrieveElementType(tp: SIRType, pos: SIRPosition)(using lctx: LoweringContext): SIRType = {
        import SumListCommonSirTypeGenerator.*
        tp match {
            case SIRType.SumCaseClass(decl, typeArgs) =>
                if decl.name == PairListDataDeclName then
                    // PairList[A, B] has element type (A, B) — get it from PairCons head param
                    val pairCons = decl.constructors
                        .find(_.name == PairConsName)
                        .getOrElse(
                          throw LoweringException(
                            s"PairCons constructor not found in ${decl.name}",
                            pos
                          )
                        )
                    SIRType.substitute(
                      pairCons.params.head.tp,
                      decl.typeParams.zip(typeArgs).toMap,
                      Map.empty
                    )
                else typeArgs.head
            case SIRType.CaseClass(constrDecl, typeArgs, optParent) =>
                if constrDecl.name == SIRType.List.NilConstr.name
                    || constrDecl.name == SIRType.BuiltinList.Nil.name
                    || constrDecl.name == PairNilName
                then SIRType.FreeUnificator
                else if constrDecl.name == SIRType.List.Cons.name
                    || constrDecl.name == SIRType.BuiltinList.Cons.name
                then typeArgs.head
                else if constrDecl.name == PairConsName then
                    // PairCons[A, B] has head: (A, B) — substitute type args
                    SIRType.substitute(
                      constrDecl.params.head.tp,
                      constrDecl.typeParams.zip(typeArgs).toMap,
                      Map.empty
                    )
                else
                    throw LoweringException(
                      s"Unknown case class ${constrDecl.name} for List",
                      pos
                    )
            case SIRType.TypeLambda(params, body) =>
                retrieveElementType(body, pos)
            case SIRType.TypeProxy(ref) =>
                retrieveElementType(ref, pos)
            case _ =>
                throw LoweringException(
                  s"Cannot retrieve element type from ${tp.show}, expected List type",
                  pos
                )
        }
    }

    def isBuiltinList(tp: SIRType): Boolean = {
        SIRType.retrieveDataDecl(tp) match {
            case Right(dataDecl) => dataDecl.name == SIRType.BuiltinList.name
            case Left(_) =>
                SIRType.retrieveConstrDecl(tp) match
                    case Right(constrDecl) =>
                        constrDecl.name == SIRType.BuiltinList.Cons.name ||
                        constrDecl.name == SIRType.BuiltinList.Nil.name
                    case Left(_) => false
        }
    }

    override def genMatch(
        matchData: SIR.Match,
        loweredScrutinee: LoweredValue,
        optTargetType: Option[SIRType]
    )(using
        lctx: LoweringContext
    ): LoweredValue = {
        // Nil, Cons
        import SumListCommonSirTypeGenerator.*
        var optNilCase: Option[SIR.Case] = None
        var optConsCase: Option[SIR.Case] = None
        var optWildcardCase: Option[SIR.Case] = None
        var noBindingInConsCase = false
        matchData.cases.foreach { cs =>
            cs.pattern match
                case SIR.Pattern.Constr(constrDecl, _, _)
                    if constrDecl.name == SIRType.List.NilConstr.name
                        || constrDecl.name == SIRType.BuiltinList.Nil.name
                        || constrDecl.name == PairNilName =>
                    optNilCase = Some(cs)
                case SIR.Pattern.Constr(constrDecl, _, _)
                    if constrDecl.name == SIRType.List.Cons.name
                        || constrDecl.name == SIRType.BuiltinList.Cons.name
                        || constrDecl.name == PairConsName =>
                    optConsCase = Some(cs)
                case SIR.Pattern.Wildcard =>
                    optWildcardCase = Some(cs)
                case _ =>
                    throw LoweringException(s"Unknown pattern ${cs.pattern}", cs.anns.pos)
        }
        val isUnchecked = matchData.anns.data.contains("unchecked")
        val optEffectiveNilCase = optNilCase
            .orElse(optWildcardCase)
            .orElse(
              if !isUnchecked then {
                  println("debug: no Nil case in List match")
                  println("annotation keys: " + matchData.anns.data.keys.mkString(", "))
                  throw LoweringException("No Nil case in match", matchData.anns.pos)
              } else None
            )
        val optEffectiveConsCase = optConsCase
            .orElse(optWildcardCase)
            .orElse(
              if !isUnchecked then
                  throw LoweringException("No Cons case in match", matchData.anns.pos)
              else None
            )
        // At least one case must be present
        if optEffectiveNilCase.isEmpty && optEffectiveConsCase.isEmpty then
            throw LoweringException("Match must have at least one case", matchData.anns.pos)
        val nilCase = optEffectiveNilCase
        val consCase = optEffectiveConsCase

        val (consHeadName, consTailName) = consCase match
            case Some(cs) =>
                cs.pattern match
                    case SIR.Pattern.Constr(_, List(h, t), _) => (h, t)
                    case SIR.Pattern.Constr(_, _, _) =>
                        throw LoweringException(
                          s"Cons case should have two bindings, but found ${cs.pattern}",
                          cs.anns.pos
                        )
                    case SIR.Pattern.Const(_) =>
                        throw LoweringException(
                          s"Constant pattern not supported for list matching",
                          cs.anns.pos
                        )
                    case SIR.Pattern.Wildcard => ("_head", "_tail")
            case None => ("_head", "_tail")

        val listInputId = lctx.uniqueVarName("listInput")
        val listType = matchData.scrutinee.tp
        val listRepr = defaultListRepresentation(listType, matchData.anns.pos)
        val listInput = new VariableLoweredValue(
          id = listInputId,
          name = listInputId,
          sir = SIR.Var(
            listInputId,
            matchData.scrutinee.tp,
            matchData.anns
          ),
          representation = listRepr,
          optRhs = Some(
            loweredScrutinee.toRepresentation(
              listRepr,
              matchData.anns.pos
            )
          )
        )

        val elementType = retrieveElementType(matchData.scrutinee.tp, matchData.anns.pos)

        val consHeadRepresentation = defaultElementRepresentation(elementType, matchData.anns.pos)

        val useCaseOnList = lctx.targetProtocolVersion >= MajorProtocolVersion.vanRossemPV

        // For PlutusV4 Case on list, head/tail are lambda parameters (no optRhs)
        // For ChooseList, they're derived from headList/tailList builtins
        val consHead = new VariableLoweredValue(
          id = lctx.uniqueVarName("consHead"),
          name = consHeadName,
          sir = SIR.Var(
            consHeadName,
            elementType,
            matchData.anns
          ),
          representation = consHeadRepresentation,
          optRhs =
              if useCaseOnList then None
              else
                  Some(
                    lvBuiltinApply(
                      SIRBuiltins.headList,
                      listInput,
                      elementType,
                      consHeadRepresentation,
                      matchData.anns.pos
                    )
                  )
        )

        val consTail = new VariableLoweredValue(
          id = lctx.uniqueVarName("consTail"),
          name = consTailName,
          sir = SIR.Var(
            consTailName,
            listType,
            matchData.anns
          ),
          representation = listRepr,
          optRhs =
              if useCaseOnList then None
              else
                  Some(
                    lvBuiltinApply(
                      SIRBuiltins.tailList,
                      listInput,
                      listType,
                      listRepr,
                      matchData.anns.pos
                    )
                  )
        )

        val prevScope = lctx.scope
        lctx.scope = lctx.scope.addAll(
          List(listInput, consHead, consTail)
        )

        val resType = optTargetType.getOrElse(matchData.tp)

        val optLoweredConsBody = consCase.map { cs =>
            lctx.lower(cs.body, Some(resType)).maybeUpcast(resType, cs.anns.pos)
        }

        lctx.scope = prevScope

        val optLoweredNilBody = nilCase.map { cs =>
            lctx.lower(cs.body, Some(resType)).maybeUpcast(resType, cs.anns.pos)
        }

        if SIRType.isProd(loweredScrutinee.sirType) then
            val constrDecl = SIRType
                .retrieveConstrDecl(loweredScrutinee.sirType)
                .getOrElse(
                  throw LoweringException(
                    s"Cannot retrieve constrDecl from ${loweredScrutinee.sirType.show}",
                    matchData.anns.pos
                  )
                )
            if constrDecl.name == SIRType.List.NilConstr.name
                || constrDecl.name == SIRType.BuiltinList.Nil.name
                || constrDecl.name == PairNilName
            then {
                println("info: unused case Cons in List match will be removed")
                optLoweredNilBody.getOrElse(
                  throw LoweringException(
                    "Nil case required for Nil-typed scrutinee",
                    matchData.anns.pos
                  )
                )
            } else if constrDecl.name == SIRType.List.Cons.name
                || constrDecl.name == SIRType.BuiltinList.Cons.name
                || constrDecl.name == PairConsName
            then {
                println("info: unused case Nil in List match will be removed")
                optLoweredConsBody.getOrElse(
                  throw LoweringException(
                    "Cons case required for Cons-typed scrutinee",
                    matchData.anns.pos
                  )
                )
            } else
                throw LoweringException(
                  s"Unknown list constructior ${constrDecl.name}",
                  matchData.anns.pos
                )
        else
            // Need at least the Cons branch for Case on List / ChooseList
            val loweredConsBody = optLoweredConsBody.getOrElse(
              throw LoweringException("Cons case is required for list match", matchData.anns.pos)
            )

            val allBranches = Seq(loweredConsBody) ++ optLoweredNilBody.toSeq
            val resRepr = LoweredValue.chooseCommonRepresentation(
              allBranches,
              resType,
              matchData.anns.pos
            )
            val loweredConsBodyR =
                loweredConsBody.toRepresentation(resRepr, consCase.get.anns.pos)
            val optLoweredNilBodyR =
                optLoweredNilBody.map(nb => nb.toRepresentation(resRepr, nilCase.get.anns.pos))

            // For PlutusV4, use Case on list; otherwise use ChooseList builtin
            val retval =
                if useCaseOnList then
                    CaseListLoweredValue(
                      listInput,
                      consHead,
                      consTail,
                      loweredConsBodyR,
                      optLoweredNilBodyR,
                      resType,
                      resRepr,
                      matchData.anns.pos
                    )
                else
                    ChooseListLoweredValue(
                      listInput,
                      consHead,
                      consTail,
                      loweredConsBodyR,
                      optLoweredNilBodyR.getOrElse(
                        throw LoweringException(
                          "Nil case is required for ChooseList (V3). Use @unchecked only with targetProtocolVersion >= vanRossemPV",
                          matchData.anns.pos
                        )
                      ),
                      resType,
                      resRepr,
                      matchData.anns.pos
                    )

            retval
    }

}

object SumListCommonSirTypeGenerator {
    val PairListDataDeclName = SIRType.PairList.DataDeclName
    val PairNilName = SIRType.PairList.PairNilName
    val PairConsName = SIRType.PairList.PairConsName
}
