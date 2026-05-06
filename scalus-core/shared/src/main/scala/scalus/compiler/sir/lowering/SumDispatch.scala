package scalus.compiler.sir.lowering

import scalus.compiler.sir.*
import scalus.compiler.sir.lowering.LoweredValue.Builder.*
import scalus.compiler.sir.lowering.typegens.*

/** Dispatch layer for sum-typed operations. Single entry point for Sum-side
  * `toRepresentation`; per-typegen overrides throw `dispatcherBypass` to
  * surface any caller that bypassed this object. See
  * `docs/local/claude/compiler/sum-prod-dispatch-design.md`.
  */
object SumDispatch {

    import SumCaseClassRepresentation.*

    /** Throw used by Sum typegens' `toRepresentation` overrides to assert that
      * dispatch goes through this object. Centralized so the message format
      * stays consistent and the four call sites are one-liners.
      */
    def dispatcherBypass(genName: String): Nothing =
        throw new IllegalStateException(
          s"$genName.toRepresentation called directly — dispatch via SumDispatch.toRepresentation"
        )

    def toRepresentation(
        input: LoweredValue,
        target: LoweredValueRepresentation,
        pos: SIRPosition
    )(using lctx: LoweringContext): LoweredValue = {
        val gen = lctx.typeGenerator(input.sirType)
        gen match
            case SumCaseSirTypeGenerator =>
                sumCaseImpl(input, target, pos)
            case SumCaseUplcConstrSirTypeGenerator =>
                sumCaseUplcConstrImpl(input, target, pos)
            case SumCaseUplcOnlySirTypeGenerator =>
                sumCaseImpl(input, target, pos)
            case listGen: SumListCommonSirTypeGenerator =>
                sumListCommonImpl(listGen, input, target, pos)
            case ProductCaseSirTypeGenerator
                | ProductCaseUplcConstrSirTypeGenerator
                | ProductCaseUplcOnlySirTypeGenerator
                | _: ProductCaseOneElementSirTypeGenerator =>
                ProdDispatch.toRepresentation(input, target, pos)
            case _ =>
                gen.toRepresentation(input, target, pos)
    }

    /** Plain sum-class source path: handles DataConstr / PairIntDataList /
      * SumBuiltinList / PackedSumDataList sources, delegating to
      * `SumUplcConstrSirTypeGenerator` for native-UplcConstr cases and to
      * `sumListCommonImpl` for list-shape conversions.
      */
    private def sumCaseImpl(
        input: LoweredValue,
        representation: LoweredValueRepresentation,
        pos: SIRPosition
    )(using lctx: LoweringContext): LoweredValue = {
        (input.representation, representation) match {
            case (DataConstr, DataConstr) =>
                input
            case (DataConstr, PairIntDataList) =>
                lvBuiltinApply(SIRBuiltins.unConstrData, input, input.sirType, PairIntDataList, pos)
            case (DataConstr, SumBuiltinList(elementRepr)) =>
                // DataConstr → SumBuiltinList: go through PairIntDataList
                input
                    .toRepresentation(PairIntDataList, pos)
                    .toRepresentation(SumBuiltinList(elementRepr), pos)
            case (DataConstr, PackedSumDataList) =>
                input
                    .toRepresentation(PairIntDataList, pos)
                    .toRepresentation(PackedSumDataList, pos)
            case (DataConstr, SumUplcConstr(_)) =>
                // DataConstr → SumUplcConstr: go through PairIntDataList
                input
                    .toRepresentation(PairIntDataList, pos)
                    .toRepresentation(representation, pos)
            // === PairIntDataList → list reprs: delegate to list generator ===
            case (PairIntDataList, SumBuiltinList(_)) | (PairIntDataList, PackedSumDataList) =>
                val elemType =
                    SumBuiltinList.retrieveListElementType(input.sirType).getOrElse(SIRType.Data.tp)
                val elemRepr = lctx.typeGenerator(elemType).defaultDataRepresentation(elemType)
                sumListCommonImpl(
                  new SumBuiltinListSirTypeGenerator(elemRepr),
                  input,
                  representation,
                  pos
                )
            // PairIntDataList → SumUplcConstr: delegate
            case (PairIntDataList, _: SumUplcConstr) =>
                SumUplcConstrSirTypeGenerator.toRepresentation(input, representation, pos)
            // All SumBuiltinList source conversions delegate to list-shape impl
            case (SumBuiltinList(inElemRepr), _) =>
                sumListCommonImpl(
                  new SumBuiltinListSirTypeGenerator(inElemRepr),
                  input,
                  representation,
                  pos
                )
            case (PackedSumDataList, PackedSumDataList) =>
                input
            // All other PackedSumDataList source conversions delegate to list-shape impl
            case (PackedSumDataList, _) =>
                val elemType =
                    SumBuiltinList.retrieveListElementType(input.sirType).getOrElse(SIRType.Data.tp)
                val elemRepr = lctx.typeGenerator(elemType).defaultDataRepresentation(elemType)
                sumListCommonImpl(
                  new SumBuiltinListSirTypeGenerator(elemRepr),
                  input,
                  representation,
                  pos
                )
            // All SumUplcConstr source/target conversions delegate to SumUplcConstrSirTypeGenerator
            case (_: SumUplcConstr, _) =>
                SumUplcConstrSirTypeGenerator.toRepresentation(input, representation, pos)
            case (_, _: SumUplcConstr) =>
                SumUplcConstrSirTypeGenerator.toRepresentation(input, representation, pos)
            case (_: ProductCaseClassRepresentation.ProdUplcConstr, _) =>
                SumUplcConstrSirTypeGenerator.toRepresentation(input, representation, pos)
            case (inTvr: TypeVarRepresentation, _) =>
                import SIRType.TypeVarKind.*
                inTvr.kind match
                    case Transparent =>
                        RepresentationProxyLoweredValue(input, representation, pos)
                    case Unwrapped =>
                        // Source bytes are in defaultRepresentation form. Relabel as that
                        // underlying repr, then convert.
                        val sourceUnderlying =
                            SumCaseSirTypeGenerator.defaultRepresentation(input.sirType)
                        val r0 = RepresentationProxyLoweredValue(input, sourceUnderlying, pos)
                        sumCaseImpl(r0, representation, pos)
                    case Fixed =>
                        val r0 = RepresentationProxyLoweredValue(input, DataConstr, pos)
                        sumCaseImpl(r0, representation, pos)
            case (_, outTvr: TypeVarRepresentation) =>
                import SIRType.TypeVarKind.*
                outTvr.kind match
                    case Transparent => input
                    case Unwrapped   =>
                        // Convert input to defaultRepresentation form, then relabel as Unwrapped.
                        val targetUnderlying =
                            SumCaseSirTypeGenerator.defaultRepresentation(input.sirType)
                        val converted = input.toRepresentation(targetUnderlying, pos)
                        new RepresentationProxyLoweredValue(converted, outTvr, pos)
                    case Fixed =>
                        sumCaseImpl(input, DataConstr, pos)
            case (_, _) =>
                throw LoweringException(
                  s"Unsupported conversion for ${input.sirType.show} from ${input.representation} to $representation",
                  pos
                )
        }
    }

    /** `@UplcRepr(UplcConstr)` sum source path. Mostly forwards to `sumCaseImpl`
      * or `SumUplcConstrSirTypeGenerator` depending on (input, target) shape.
      */
    private def sumCaseUplcConstrImpl(
        input: LoweredValue,
        representation: LoweredValueRepresentation,
        pos: SIRPosition
    )(using lctx: LoweringContext): LoweredValue = {
        (input.representation, representation) match
            case (a, b) if a == b => input
            case (a, b) if a.isCompatibleOn(input.sirType, b, pos) =>
                RepresentationProxyLoweredValue(input, representation, pos)
            // SumBuiltinList → DataConstr: listData(input) → PackedSumDataList → DataConstr
            case (SumBuiltinList(elemRepr), DataConstr) =>
                val asPackedList = sumListCommonImpl(
                  new SumBuiltinListSirTypeGenerator(elemRepr),
                  input,
                  PackedSumDataList,
                  pos
                )
                lvBuiltinApply(SIRBuiltins.listData, asPackedList, input.sirType, DataConstr, pos)
            // SumBuiltinList input → delegate to list-shape impl
            case (SumBuiltinList(elemRepr), _) =>
                sumListCommonImpl(
                  new SumBuiltinListSirTypeGenerator(elemRepr),
                  input,
                  representation,
                  pos
                )
            // PackedSumDataList input → delegate to list-shape impl
            case (PackedSumDataList, _) =>
                sumListCommonImpl(
                  new SumBuiltinListSirTypeGenerator(PrimitiveRepresentation.PackedData),
                  input,
                  representation,
                  pos
                )
            // DataConstr/PairIntDataList → SumBuiltinList/PackedSumDataList/PairIntDataList:
            // delegate to sumCaseImpl which handles these without BuiltinList type hack
            case (DataConstr, _: SumBuiltinList) | (DataConstr, PackedSumDataList) |
                (DataConstr, PairIntDataList) | (PairIntDataList, _: SumBuiltinList) |
                (PairIntDataList, PackedSumDataList) =>
                sumCaseImpl(input, representation, pos)
            // TypeVar source: dispatch by kind
            case (inTvr: TypeVarRepresentation, _) =>
                import SIRType.TypeVarKind.*
                inTvr.kind match
                    case Transparent =>
                        SumUplcConstrSirTypeGenerator.toRepresentation(input, representation, pos)
                    case Unwrapped =>
                        val sourceUnderlying =
                            SumCaseUplcConstrSirTypeGenerator.defaultRepresentation(input.sirType)
                        val r0 = RepresentationProxyLoweredValue(input, sourceUnderlying, pos)
                        sumCaseUplcConstrImpl(r0, representation, pos)
                    case Fixed =>
                        SumUplcConstrSirTypeGenerator.toRepresentation(input, representation, pos)
            // TypeVar target: dispatch by kind
            case (_, outTvr: TypeVarRepresentation) =>
                import SIRType.TypeVarKind.*
                outTvr.kind match
                    case Transparent => input
                    case Unwrapped =>
                        val targetUnderlying =
                            SumCaseUplcConstrSirTypeGenerator.defaultRepresentation(input.sirType)
                        val converted = input.toRepresentation(targetUnderlying, pos)
                        new RepresentationProxyLoweredValue(converted, outTvr, pos)
                    case Fixed =>
                        val targetUnderlying =
                            SumCaseUplcConstrSirTypeGenerator
                                .defaultTypeVarReperesentation(input.sirType)
                        val converted = input.toRepresentation(targetUnderlying, pos)
                        new RepresentationProxyLoweredValue(converted, outTvr, pos)
            // SumUplcConstr, DataConstr, PairIntDataList → SumUplcConstrSirTypeGenerator
            case (_: SumUplcConstr, _) | (DataConstr, _) | (PairIntDataList, _) =>
                SumUplcConstrSirTypeGenerator.toRepresentation(input, representation, pos)
            case (inRepr, outRepr) =>
                throw LoweringException(
                  s"SumCaseUplcConstr unhandled conversion $inRepr → $outRepr for ${input.sirType.show}",
                  pos
                )
    }

    /** List-shape source path: SumBuiltinList(_) / SumPairBuiltinList(_,_) /
      * PackedSumDataList / SumDataAssocMap, plus the suspicious PairIntDataList
      * fallback (see arm comment) and TypeVar-kind handling.
      *
      * `gen` provides access to subclass-specific `defaultRepresentation`,
      * `retrieveElementType`, and `isNilType` — SumBuiltinList vs
      * SumPairBuiltinList override these differently. The parameter is a
      * transitional shim; later phases move those methods to a companion so
      * the dispatcher can call them statically.
      */
    private def sumListCommonImpl(
        gen: SumListCommonSirTypeGenerator,
        input: LoweredValue,
        outputRepresentation: LoweredValueRepresentation,
        pos: SIRPosition
    )(using lctx: LoweringContext): LoweredValue = {
        if input.representation == outputRepresentation then return input
        if input.representation.isCompatibleOn(input.sirType, outputRepresentation, pos) then
            return RepresentationProxyLoweredValue(input, outputRepresentation, pos)
        (input.representation, outputRepresentation) match
            // === SumBuiltinList identity ===
            case (SumBuiltinList(inElemRepr), SumBuiltinList(outElemRepr))
                if inElemRepr == outElemRepr =>
                input
            // === SumBuiltinList cross-element-repr conversions ===
            case (SumBuiltinList(inElemRepr), SumBuiltinList(outElemRepr))
                if hasConstantOrTypeVar(inElemRepr, outElemRepr) =>
                val elemType = gen.retrieveElementType(input.sirType, pos)
                def resolveElementRepr(
                    repr: LoweredValueRepresentation
                ): LoweredValueRepresentation =
                    repr match
                        case tvr: TypeVarRepresentation =>
                            val elemGen = lctx.typeGenerator(elemType)
                            if !tvr.isPackedData then elemGen.defaultRepresentation(elemType)
                            else elemGen.defaultTypeVarReperesentation(elemType)
                        case other => other
                val resolvedIn = resolveElementRepr(inElemRepr)
                val resolvedOut = resolveElementRepr(outElemRepr)
                val hasNativeTv = (inElemRepr, outElemRepr) match
                    case (tvr: TypeVarRepresentation, _) if isNativeTypeVar(tvr) => true
                    case (_, tvr: TypeVarRepresentation) if isNativeTypeVar(tvr) => true
                    case _                                                       => false
                if resolvedIn == resolvedOut
                    || elemType == SIRType.FreeUnificator
                    || elemType == SIRType.TypeNothing
                    || hasNativeTv
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
            case (SumBuiltinList(PrimitiveRepresentation.Constant), PackedSumDataList) =>
                val elemType = gen.retrieveElementType(input.sirType, pos)
                val elemRepr = lctx.typeGenerator(elemType).defaultDataRepresentation(elemType)
                input
                    .toRepresentation(SumBuiltinList(elemRepr), pos)
                    .toRepresentation(PackedSumDataList, pos)
            case (PackedSumDataList, SumBuiltinList(PrimitiveRepresentation.Constant)) =>
                val elemType = gen.retrieveElementType(input.sirType, pos)
                val elemRepr = lctx.typeGenerator(elemType).defaultDataRepresentation(elemType)
                input
                    .toRepresentation(SumBuiltinList(elemRepr), pos)
                    .toRepresentation(outputRepresentation, pos)
            case (SumBuiltinList(PrimitiveRepresentation.Constant), tv @ TypeVarRepresentation(kind)) =>
                import SIRType.TypeVarKind.*
                kind match
                    case Transparent =>
                        new RepresentationProxyLoweredValue(input, tv, pos)
                    case Unwrapped =>
                        new RepresentationProxyLoweredValue(input, tv, pos)
                    case Fixed =>
                        input
                            .toRepresentation(PackedSumDataList, pos)
                            .toRepresentation(tv, pos)
            case (tv @ TypeVarRepresentation(kind), SumBuiltinList(PrimitiveRepresentation.Constant)) =>
                import SIRType.TypeVarKind.*
                kind match
                    case Transparent =>
                        new RepresentationProxyLoweredValue(input, outputRepresentation, pos)
                    case Unwrapped =>
                        new RepresentationProxyLoweredValue(input, outputRepresentation, pos)
                    case Fixed =>
                        input
                            .toRepresentation(PackedSumDataList, pos)
                            .toRepresentation(outputRepresentation, pos)
            // === SumBuiltinList → PackedSumDataList (via listData) ===
            case (SumBuiltinList(_), PackedSumDataList) =>
                val elemType = gen.retrieveElementType(input.sirType, pos)
                val asDataList =
                    if elemType == SIRType.TypeNothing || elemType == SIRType.FreeUnificator then
                        input
                    else
                        val elemRepr =
                            lctx.typeGenerator(elemType).defaultDataRepresentation(elemType)
                        input.toRepresentation(SumBuiltinList(elemRepr), pos)
                lvBuiltinApply(
                  SIRBuiltins.listData,
                  asDataList,
                  input.sirType,
                  PackedSumDataList,
                  pos
                )
            // === SumBuiltinList catch-all (compatible element reprs) ===
            case (SumBuiltinList(inElemRepr), out @ SumBuiltinList(outElemRepr)) =>
                if inElemRepr == outElemRepr then input
                else if (inElemRepr == PrimitiveRepresentation.PackedData && outElemRepr == DataData)
                    || (inElemRepr == DataData && outElemRepr == PrimitiveRepresentation.PackedData)
                then
                    throw LoweringException(
                      s"PackedData/DataData mismatch in SumBuiltinList: $inElemRepr -> $outElemRepr " +
                          s"type=${input.sirType.show} createdEx=${input.createdEx}",
                      pos
                    )
                else
                    val elemType = gen.retrieveElementType(input.sirType, pos)
                    if inElemRepr.isCompatibleOn(elemType, outElemRepr, pos) then
                        RepresentationProxyLoweredValue(input, out, pos)
                    else convertBuiltinList(input, elemType, inElemRepr, outElemRepr, out, pos)
            // === SumBuiltinList → SumDataAssocMap (go through SumPairBuiltinList) ===
            case (SumBuiltinList(_), SumDataAssocMap) =>
                val elemType = gen.retrieveElementType(input.sirType, pos)
                val pairRepr = SumPairBuiltinList.fromElementType(elemType, pos)
                input
                    .toRepresentation(pairRepr, pos)
                    .toRepresentation(SumDataAssocMap, pos)
            // === PackedSumDataList conversions ===
            case (PackedSumDataList, out @ SumBuiltinList(_)) =>
                val elemType = gen.retrieveElementType(input.sirType, pos)
                val elemRepr = lctx.typeGenerator(elemType).defaultDataRepresentation(elemType)
                val dataListRepr = SumBuiltinList(elemRepr)
                val asDataList =
                    lvBuiltinApply(SIRBuiltins.unListData, input, input.sirType, dataListRepr, pos)
                if out == dataListRepr then asDataList
                else asDataList.toRepresentation(out, pos)
            case (PackedSumDataList, PackedSumDataList) =>
                input
            case (PackedSumDataList, SumDataAssocMap) =>
                val elemType = gen.retrieveElementType(input.sirType, pos)
                val elemRepr = lctx.typeGenerator(elemType).defaultDataRepresentation(elemType)
                val pairRepr = SumPairBuiltinList.fromElementType(elemType, pos)
                input
                    .toRepresentation(SumBuiltinList(elemRepr), pos)
                    .toRepresentation(pairRepr, pos)
                    .toRepresentation(SumDataAssocMap, pos)
            case (PackedSumDataList, out @ SumPairBuiltinList(_, _)) =>
                val elemType = gen.retrieveElementType(input.sirType, pos)
                val elemRepr = lctx.typeGenerator(elemType).defaultDataRepresentation(elemType)
                input
                    .toRepresentation(SumBuiltinList(elemRepr), pos)
                    .toRepresentation(out, pos)
            // === SumPairBuiltinList conversions ===
            case (SumPairBuiltinList(_, _), SumPairBuiltinList(_, _)) =>
                input
            case (SumPairBuiltinList(_, _), SumDataAssocMap) =>
                lvBuiltinApply(SIRBuiltins.mapData, input, input.sirType, SumDataAssocMap, pos)
            case (SumPairBuiltinList(_, _), PackedSumDataList) =>
                val elemType = gen.retrieveElementType(input.sirType, pos)
                val dataElemRepr =
                    lctx.typeGenerator(elemType).defaultDataRepresentation(elemType)
                input
                    .toRepresentation(SumBuiltinList(dataElemRepr), pos)
                    .toRepresentation(PackedSumDataList, pos)
            case (
                  SumPairBuiltinList(inKeyRepr, inValueRepr),
                  out @ SumBuiltinList(outElemRepr)
                ) =>
                val elemType = gen.retrieveElementType(input.sirType, pos)
                val inElemRepr =
                    ProductCaseClassRepresentation.ProdBuiltinPair(inKeyRepr, inValueRepr)
                convertBuiltinList(input, elemType, inElemRepr, outElemRepr, out, pos)
            case (SumBuiltinList(inElemRepr), out @ SumPairBuiltinList(outKeyRepr, outValueRepr)) =>
                if gen.isNilType(input.sirType) then lvPairDataNil(pos, input.sirType, out)
                else
                    val elemType = gen.retrieveElementType(input.sirType, pos)
                    val outElemRepr =
                        ProductCaseClassRepresentation.ProdBuiltinPair(outKeyRepr, outValueRepr)
                    convertBuiltinList(input, elemType, inElemRepr, outElemRepr, out, pos)
            // === SumDataAssocMap conversions ===
            case (SumDataAssocMap, SumPairBuiltinList(_, _)) =>
                val elemType = gen.retrieveElementType(input.sirType, pos)
                val pairRepr = SumPairBuiltinList.fromElementType(elemType, pos)
                lvBuiltinApply(SIRBuiltins.unMapData, input, input.sirType, pairRepr, pos)
            case (SumDataAssocMap, _) =>
                val elemType = gen.retrieveElementType(input.sirType, pos)
                val pairRepr = SumPairBuiltinList.fromElementType(elemType, pos)
                input
                    .toRepresentation(pairRepr, pos)
                    .toRepresentation(outputRepresentation, pos)
            // === PairIntDataList for list-typed values (suspicious — DataConstr should
            //     not exist for List types per design, but PairIntDataList is its
            //     unpacked form. Probe with logging if hit during refactor).
            case (PairIntDataList, SumBuiltinList(elementRepr)) =>
                throw LoweringException(
                  s"PairIntDataList → SumBuiltinList($elementRepr) for ${input.sirType.show}: " +
                      "DataConstr should not exist for List types.",
                  pos
                )
            case (PairIntDataList, _) =>
                // Reconstruct DataConstr from (tag, fieldList), then route via PackedSumDataList.
                val asDataConstr = lvBuiltinApply2(
                  SIRBuiltins.constrData,
                  lvBuiltinApply(
                    SIRBuiltins.fstPair,
                    input,
                    SIRType.Integer,
                    PrimitiveRepresentation.Constant,
                    pos
                  ),
                  lvBuiltinApply(
                    SIRBuiltins.sndPair,
                    input,
                    SIRType.List(SIRType.Data.tp),
                    SumBuiltinList(DataData),
                    pos
                  ),
                  input.sirType,
                  DataConstr,
                  pos
                )
                asDataConstr
                    .toRepresentation(PackedSumDataList, pos)
                    .toRepresentation(outputRepresentation, pos)
            // === TypeVarRepresentation ===
            case (_, tv: TypeVarRepresentation) =>
                import SIRType.TypeVarKind.*
                tv.kind match
                    case Transparent =>
                        new RepresentationProxyLoweredValue(input, tv, pos)
                    case Unwrapped =>
                        val targetUnderlying = gen.defaultRepresentation(input.sirType)
                        val converted = input.toRepresentation(targetUnderlying, pos)
                        new RepresentationProxyLoweredValue(converted, tv, pos)
                    case Fixed =>
                        val inputAsData = input.toRepresentation(PackedSumDataList, pos)
                        new RepresentationProxyLoweredValue(inputAsData, tv, pos)
            case (tv: TypeVarRepresentation, _) =>
                import SIRType.TypeVarKind.*
                tv.kind match
                    case Transparent =>
                        new RepresentationProxyLoweredValue(input, outputRepresentation, pos)
                    case Unwrapped =>
                        val sourceUnderlying = gen.defaultRepresentation(input.sirType)
                        val r0 = new RepresentationProxyLoweredValue(input, sourceUnderlying, pos)
                        r0.toRepresentation(outputRepresentation, pos)
                    case Fixed =>
                        if input.representation == outputRepresentation then input
                        else
                            val r0 = new RepresentationProxyLoweredValue(
                              input,
                              PackedSumDataList,
                              pos
                            )
                            r0.toRepresentation(outputRepresentation, pos)
            // SumReprProxy: unwrap and delegate
            case (_: SumReprProxy, _) =>
                SumUplcConstrSirTypeGenerator.toRepresentation(input, outputRepresentation, pos)
            case (_, _: SumReprProxy) =>
                SumUplcConstrSirTypeGenerator.toRepresentation(input, outputRepresentation, pos)
            // SumUplcConstr → anything: delegate
            case (_: SumUplcConstr, _) =>
                SumUplcConstrSirTypeGenerator.toRepresentation(input, outputRepresentation, pos)
            // anything → SumUplcConstr: delegate
            case (_, _: SumUplcConstr) =>
                SumUplcConstrSirTypeGenerator.toRepresentation(input, outputRepresentation, pos)
            // ProdUplcConstr value at a sum-list-type site: wrap as singleton SumUplcConstr
            case (puc: ProductCaseClassRepresentation.ProdUplcConstr, _) =>
                val wrappedRepr =
                    SumUplcConstr(scala.collection.immutable.SortedMap(puc.tag -> puc))
                val wrapped = new RepresentationProxyLoweredValue(input, wrappedRepr, pos)
                SumUplcConstrSirTypeGenerator.toRepresentation(
                  wrapped,
                  outputRepresentation,
                  pos
                )
            case _ =>
                throw LoweringException(
                  s"Unexpected representation conversion for ${input.sirType.show} from ${input.representation} to $outputRepresentation",
                  pos
                )
    }

    /** Cross-element-repr conversion for BuiltinList, applied via ScalusRuntime.mapList. */
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
            if isPairIn then SumPairBuiltinList.fromElementType(elemType, pos)
            else SumBuiltinList(resolvedIn)
        val outListRepr: LoweredValueRepresentation =
            if isPairOut then SumPairBuiltinList.fromElementType(elemType, pos)
            else SumBuiltinList(resolvedOut)
        val outNil =
            if isPairOut then SumPairBuiltinListSirTypeGenerator.genNil(outListType, pos)
            else new SumBuiltinListSirTypeGenerator(resolvedOut).genNil(outListType, pos)
        val convFn = lvLamAbs(
          "elem",
          elemType,
          resolvedIn,
          elem => elem.toRepresentation(resolvedOut, pos),
          pos
        )
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
            lvApplyDirect(ScalusRuntime.mapList, convFn, afterFnType, afterFnRepr, pos),
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
        if outputRepresentation == outListRepr then result
        else RepresentationProxyLoweredValue(result, outputRepresentation, pos)
    }

    private def hasConstantOrTypeVar(
        a: LoweredValueRepresentation,
        b: LoweredValueRepresentation
    ): Boolean =
        a == PrimitiveRepresentation.Constant || b == PrimitiveRepresentation.Constant ||
            a.isInstanceOf[TypeVarRepresentation] || b.isInstanceOf[TypeVarRepresentation]

    private def isNativeTypeVar(tvr: TypeVarRepresentation)(using
        lctx: LoweringContext
    ): Boolean =
        import SIRType.TypeVarKind.*
        tvr.kind match
            case Transparent | Unwrapped => true
            case Fixed                   => false

    /** Choose the typegen that should emit this `Constr`, considering both the
      * static type and surrounding-arg/context cues. Centralizes the four rules
      * formerly in `ConstrDispatcher.shouldDelegateToUplcConstr`:
      *
      *   1. Parent sum's default representation is `SumUplcConstr`.
      *   2. `List.Cons` whose tail's actual representation is `SumUplcConstr`.
      *   3. Caller's `optTargetType` is `@UplcRepr(UplcConstr)`-annotated.
      *   4. `lctx.inUplcConstrListScope == true` AND parent sum is List/Option.
      *
      * Returns the typegen to dispatch to; default is the static-type's own typegen.
      */
    def chooseConstrOutputRepr(
        constr: SIR.Constr,
        loweredArgs: scala.List[LoweredValue],
        optTargetType: Option[SIRType]
    )(using lctx: LoweringContext): typegens.SirTypeUplcGenerator = {
        val resolvedType = lctx.resolveTypeVarIfNeeded(constr.tp)

        // 1. Parent's default repr is SumUplcConstr.
        resolvedType match
            case SIRType.CaseClass(_, typeArgs, Some(parent)) =>
                val substitutedParent = parent match
                    case SIRType.TypeLambda(params, body) if params.length == typeArgs.length =>
                        SIRType.substitute(body, params.zip(typeArgs).toMap, Map.empty)
                    case _ => parent
                val parentGen = lctx.typeGenerator(substitutedParent)
                parentGen.defaultRepresentation(substitutedParent) match
                    case _: SumUplcConstr => return parentGen
                    case _                =>
            case _ =>

        // 2. List.Cons + tail has SumUplcConstr repr.
        resolvedType match
            case SIRType.CaseClass(cd, _, Some(_))
                if cd.name.endsWith("List$.Cons") && loweredArgs.length >= 2 =>
                loweredArgs.last.representation match
                    case _: SumUplcConstr => return typegens.SumCaseUplcConstrSirTypeGenerator
                    case _                =>
            case _ =>

        // 3. Caller's target type is @UplcRepr(UplcConstr) annotated.
        val targetUsesUplcConstr = optTargetType.exists {
            case SIRType.Annotated(inner, anns) =>
                anns.data.contains("uplcRepr") &&
                IntrinsicResolver.isUplcConstrListOrOption(inner)
            case _ => false
        }
        if targetUsesUplcConstr then return typegens.SumCaseUplcConstrSirTypeGenerator

        // 4. inUplcConstrListScope flag + this constr's parent sum is List/Option.
        val parentIsListOrOption = resolvedType match
            case SIRType.CaseClass(_, _, Some(parent)) =>
                IntrinsicResolver.isUplcConstrListOrOption(parent)
            case _ =>
                IntrinsicResolver.isUplcConstrListOrOption(resolvedType)
        if lctx.inUplcConstrListScope && parentIsListOrOption then
            return typegens.SumCaseUplcConstrSirTypeGenerator

        lctx.typeGenerator(resolvedType)
    }

    /** Pick the type generator for a `Nil` constructor with the lowering context's
      * hints. Equivalent to `lctx.typeGenerator(target-or-resolved-type)` plus the
      * `inUplcConstrListScope` override. Returns `(generator, effective-constr)` —
      * `effective-constr` carries `targetType` as its `tp` when an `optTargetType`
      * is supplied (so the generator sees the right type info for type-correct
      * Nil emission). The `inUplcConstrListScope` flag overrides the static-type
      * choice for List/Option results so a Nil produced inside an UplcConstr-scoped
      * intrinsic body emits as `SumUplcConstr` even when the surrounding target
      * type is unannotated — this is the same effect the typegen-internal
      * `ConstrDispatcher.shouldDelegateToUplcConstr` rule #4 used to apply (and
      * which we lose by routing `Nil` straight to `typeGenerator.genConstrLowered`
      * from `Lowering.lowerSIR`, bypassing `chooseConstrOutputRepr`).
      */
    def dispatchNil(
        constr: SIR.Constr,
        resolvedType: SIRType,
        optTargetType: Option[SIRType]
    )(using lctx: LoweringContext): (typegens.SirTypeUplcGenerator, SIR.Constr) = {
        optTargetType match
            case Some(targetType) =>
                val byTarget = lctx.typeGenerator(targetType)
                val gen =
                    if byTarget == typegens.SumCaseUplcConstrSirTypeGenerator then byTarget
                    else if lctx.inUplcConstrListScope &&
                        IntrinsicResolver.isUplcConstrListOrOption(targetType)
                    then typegens.SumCaseUplcConstrSirTypeGenerator
                    else byTarget
                (gen, constr.copy(tp = targetType))
            case None =>
                val gen =
                    if lctx.inUplcConstrListScope &&
                        IntrinsicResolver.isUplcConstrListOrOption(resolvedType)
                    then typegens.SumCaseUplcConstrSirTypeGenerator
                    else lctx.typeGenerator(resolvedType)
                (gen, constr)
    }

    def genConstr(
        constr: SIR.Constr,
        loweredArgs: scala.List[LoweredValue],
        optTargetType: Option[SIRType]
    )(using lctx: LoweringContext): LoweredValue = {
        val gen = chooseConstrOutputRepr(constr, loweredArgs, optTargetType)
        gen.genConstrLowered(constr, loweredArgs, optTargetType)
    }

    def genMatch(
        matchData: SIR.Match,
        loweredScrutinee: LoweredValue,
        optTargetType: Option[SIRType]
    )(using lctx: LoweringContext): LoweredValue =
        lctx.typeGenerator(loweredScrutinee.sirType)
            .genMatch(matchData, loweredScrutinee, optTargetType)

    /** Choose-and-align step for `SumUplcConstr` matches when any branch carries a
      * `(Sum|Prod)UplcConstr` whose field reprs include a Transparent TypeVar.
      * Returns `Some((sumRepr, aligned))` when the override fires, `None`
      * otherwise (caller should defer to its own convergence logic, typically
      * `LoweredValue.chooseCommonRepresentation`).
      *
      * The override fires because Transparent TypeVar fields cannot be folded
      * to a Data-shaped repr — we must keep a `SumUplcConstr` shape and align
      * branches structurally. Alignment rule: branches that already are
      * `(Sum|Prod)UplcConstr` (or `ErrorRepresentation`) stay as-is; others
      * route through `toRepresentation(sumRepr, pos)` for Data→UplcConstr
      * conversion.
      *
      * The synthesized `SumUplcConstr` uses `buildSumUplcConstr`'s default
      * variants for tags without a matching branch puc. This forces the right
      * type-arg substitution and TypeVar-field handling that hand-rolled
      * `defaultRepresentation` lookups get wrong (DataDecl `A` carries
      * `Fixed` kind which would leak into downstream inference).
      *
      * This is the Phase 3b extraction point. Callers that don't need the
      * Transparent-UplcConstr override can keep their existing convergence
      * (e.g. `LoweredValue.chooseCommonRepresentation`).
      */
    def transparentSumUplcConstrAlignment(
        branches: Seq[LoweredValue],
        resultType: SIRType,
        pos: SIRPosition
    )(using lctx: LoweringContext): Option[(SumUplcConstr, Seq[LoweredValue])] = {
        def hasTransparentFields(repr: LoweredValueRepresentation): Boolean = repr match
            case puc: ProductCaseClassRepresentation.ProdUplcConstr =>
                puc.fieldReprs.exists {
                    case tvr: TypeVarRepresentation => tvr.isBuiltin
                    case _                          => false
                }
            case suc: SumUplcConstr =>
                suc.variants.values.exists(puc => hasTransparentFields(puc))
            case _ => false
        if !branches.exists(b => hasTransparentFields(b.representation)) then None
        else
            val branchPucByTag = branches.flatMap { b =>
                b.representation match
                    case puc: ProductCaseClassRepresentation.ProdUplcConstr =>
                        Some(puc.tag -> puc)
                    case _ => None
            }.toMap
            val defaultSumRepr =
                typegens.SumUplcConstrSirTypeGenerator.buildSumUplcConstr(resultType, pos)
            val variants = defaultSumRepr.variants.map { case (idx, defaultPuc) =>
                branchPucByTag.get(idx) match
                    case Some(puc) => (idx, puc)
                    case None      => (idx, defaultPuc)
            }
            val sumRepr = SumUplcConstr(variants)
            val aligned = branches.map { branch =>
                branch.representation match
                    case _: ProductCaseClassRepresentation.ProdUplcConstr |
                        _: SumCaseClassRepresentation.SumUplcConstr =>
                        branch
                    case ErrorRepresentation => branch
                    case _                   => branch.toRepresentation(sumRepr, pos)
            }
            Some((sumRepr, aligned))
    }

    def genSelect(
        sel: SIR.Select,
        loweredScrutinee: LoweredValue
    )(using lctx: LoweringContext): LoweredValue =
        lctx.typeGenerator(loweredScrutinee.sirType).genSelect(sel, loweredScrutinee)

    def upcastOne(
        input: LoweredValue,
        targetType: SIRType,
        pos: SIRPosition
    )(using lctx: LoweringContext): LoweredValue =
        lctx.typeGenerator(input.sirType).upcastOne(input, targetType, pos)

}
