package scalus.compiler.sir.lowering

import org.typelevel.paiges.Doc
import scalus.compiler.sir.*
import scalus.uplc.DefaultUni

import scala.annotation.tailrec

/** representation, depends on the type of the value.
  */
sealed trait LoweredValueRepresentation {
    def isPackedData: Boolean
    def isDataCentric: Boolean
    def isCompatibleOn(tp: SIRType, repr: LoweredValueRepresentation, pos: SIRPosition)(using
        lctx: LoweringContext
    ): Boolean =
        this == repr
    def isCompatibleWithType(tp: SIRType)(using LoweringContext): Boolean
    def doc: Doc = Doc.text(this.toString)
    def show = doc.render(80)

    /** Stable structural key for use as cache-key / identifier-suffix. Two structurally-equal
      * representations return equal `stableKey` strings.
      *
      * Distinct from `toString`: for `SumReprProxy` the default `Object.toString` returns
      * `ClassName@HEX` (identity-based), leaking proxy-instance identity into any key that
      * embeds the repr rendering (e.g. `LoweringEq`'s sumEq helper cache, UPLC var-name
      * derivation). Use `stableKey` there instead.
      *
      * Implemented in terms of [[stableKeyInternal]] which threads a `seen` set through the
      * recursion so self-referential `SumReprProxy` produces `SumProxy(cycle)` as a stable
      * placeholder. Override [[stableKeyInternal]] in every subclass that recursively contains
      * a [[LoweredValueRepresentation]] field so the `seen` set is propagated.
      */
    final def stableKey: String = stableKeyInternal(Set.empty)

    /** Recursive worker for [[stableKey]]. Default delegates to `toString`, which is correct
      * for leaf case-objects and case-classes whose fields don't recursively contain a
      * [[LoweredValueRepresentation]]. Any subclass that does contain a nested repr must
      * override and call `child.stableKeyInternal(seen)` on each nested field.
      */
    def stableKeyInternal(seen: Set[SumCaseClassRepresentation.SumReprProxy]): String = toString

    /** The SIR type that values actually have at the UPLC level for this representation.
      *
      * @param semanticType
      *   the high-level SIR type (e.g., `Integer`, `List[BigInt]`)
      * @return
      *   the UPLC-level SIR type (e.g., `Integer` for Constant, `Data` for PackedData)
      */
    def uplcType(semanticType: SIRType)(using LoweringContext): SIRType = semanticType

    /** The UPLC DefaultUni type for values in this representation.
      *
      * Used to generate correctly-typed nil constants for builtin lists.
      *
      * @param semanticType
      *   the high-level SIR type (e.g., `Integer`, `List[BigInt]`)
      * @return
      *   the DefaultUni type (e.g., `DefaultUni.Integer` for Constant, `DefaultUni.Data` for
      *   PackedData)
      */
    def defaultUni(semanticType: SIRType)(using LoweringContext): DefaultUni
}

object LoweredValueRepresentation {

    def constRepresentation(tp: SIRType)(using lc: LoweringContext): LoweredValueRepresentation = {
        tp match
            case SIRType.BuiltinList(elemType) =>
                if typegens.SirTypeUplcGenerator.isPair(elemType) then
                    SumCaseClassRepresentation.SumPairBuiltinList.fromElementType(elemType)
                else
                    SumCaseClassRepresentation.SumBuiltinList(
                      typegens.SirTypeUplcGenerator.elementReprFor(elemType)
                    )
            case SIRType.BuiltinArray(elemType) =>
                ProductCaseClassRepresentation.ProdBuiltinArray(constRepresentation(elemType))
            case SIRType.SumCaseClass(decl, typeArgs) =>
                if decl.name == SIRType.Data.name then SumCaseClassRepresentation.DataData
                else if decl.name == "scalus.cardano.onchain.plutus.prelude.List" || decl.name == SIRType.BuiltinList.name
                then
                    if typeArgs.nonEmpty then
                        if ProductCaseClassRepresentation.ProdBuiltinPair.isPairOrTuple2(
                              typeArgs.head
                            )
                        then
                            SumCaseClassRepresentation.SumPairBuiltinList.fromElementType(
                              typeArgs.head
                            )
                        else
                            SumCaseClassRepresentation.SumBuiltinList(
                              typegens.SirTypeUplcGenerator.elementReprFor(typeArgs.head)
                            )
                    else
                        throw LoweringException(
                          s"List type without type parameter in constant representation",
                          SIRPosition.empty
                        )
                else SumCaseClassRepresentation.DataConstr
            case SIRType.CaseClass(constrDecl, targs, parent) =>
                ProductCaseClassRepresentation.ProdDataConstr
            case SIRType.TypeLambda(params, body) =>
                constRepresentation(body)
            case SIRType.Integer | SIRType.ByteString | SIRType.String | SIRType.Boolean |
                SIRType.Unit | SIRType.BLS12_381_G1_Element | SIRType.BLS12_381_G2_Element |
                SIRType.BLS12_381_MlResult | SIRType.BuiltinValue =>
                PrimitiveRepresentation.Constant
            case SIRType.Fun(in, out) =>
                val inRepresentation = lc.typeGenerator(in).defaultRepresentation(in)
                val outRepresentation = lc.typeGenerator(out).defaultRepresentation(out)
                LambdaRepresentation(
                  tp,
                  InOutRepresentationPair(inRepresentation, outRepresentation)
                )
            case tv: SIRType.TypeVar =>
                lc.typeUnifyEnv.filledTypes.get(tv) match
                    case Some(tp) => constRepresentation(tp)
                    case None =>
                        TypeVarRepresentation(tv.kind)
            case SIRType.FreeUnificator =>
                TypeVarRepresentation(SIRType.TypeVarKind.Fixed)
            case proxy: SIRType.TypeProxy =>
                constRepresentation(proxy.ref)
            case SIRType.TypeNothing => ErrorRepresentation
            case SIRType.Annotated(tp, _) =>
                constRepresentation(tp)
            case SIRType.TypeNonCaseModule(name) =>
                throw LoweringException(
                  "TypeNonCaseModule is not supported in lowered value representation",
                  SIRPosition.empty
                )
            case null =>
                throw LoweringException(
                  "Type is null, this is a bug in the compiler",
                  SIRPosition.empty
                )
    }
}

/** Mixin for representations that always store values as Data at the UPLC level. */
trait PackedDataDefaultUni extends LoweredValueRepresentation {
    override def defaultUni(semanticType: SIRType)(using LoweringContext): DefaultUni =
        DefaultUni.Data
}

sealed trait SumCaseClassRepresentation(
    override val isPackedData: Boolean,
    override val isDataCentric: Boolean
) extends LoweredValueRepresentation {

    override def isCompatibleWithType(tp: SIRType)(using LoweringContext): Boolean = {
        SIRType.isSum(tp)
    }

}

object SumCaseClassRepresentation {

    /** Representation for sum case classes that are represented as a Data with DataConstr and
      * DataUnconstr operators to work with the data. the index of the constructor and x is a field.
      */
    case object DataConstr
        extends SumCaseClassRepresentation(true, true)
        with PackedDataDefaultUni {
        override def isCompatibleOn(
            tp: SIRType,
            repr: LoweredValueRepresentation,
            pos: SIRPosition
        )(using LoweringContext): Boolean =
            repr match {
                case DataConstr               => true
                case TypeVarRepresentation(_) => true
                case other                    => false
            }
        override def uplcType(semanticType: SIRType)(using LoweringContext): SIRType =
            SIRType.Data.tp
    }

    /** Representation for the builtin Data type and its constructors (Constr, Map, List, I, B).
      * Data values are stored as raw UPLC Data.
      */
    case object DataData extends SumCaseClassRepresentation(true, true) with PackedDataDefaultUni {
        override def uplcType(semanticType: SIRType)(using LoweringContext): SIRType =
            SIRType.Data.tp
        override def isCompatibleWithType(tp: SIRType)(using LoweringContext): Boolean = {
            tp match
                case SIRType.SumCaseClass(decl, _) if decl.name == SIRType.Data.name => true
                case SIRType.CaseClass(constrDecl, _, _)
                    if constrDecl.name == SIRType.Data.Constr.name ||
                        constrDecl.name == SIRType.Data.Map.name ||
                        constrDecl.name == SIRType.Data.List.name ||
                        constrDecl.name == SIRType.Data.I.name ||
                        constrDecl.name == SIRType.Data.B.name =>
                    true
                case _ => false
        }

        override def isCompatibleOn(
            tp: SIRType,
            repr: LoweredValueRepresentation,
            pos: SIRPosition
        )(using LoweringContext): Boolean =
            repr match {
                case DataData                           => true
                case PrimitiveRepresentation.PackedData => true
                case TypeVarRepresentation(_)           => true
                case _                                  => false
            }
    }

    /** Representation for sum case classes that are represented as a Pair of Int and DataList.
      */
    case object PairIntDataList extends SumCaseClassRepresentation(false, true) {
        override def uplcType(semanticType: SIRType)(using LoweringContext): SIRType =
            SIRType.BuiltinPair(SIRType.Integer, SIRType.List(SIRType.Data.tp))
        override def defaultUni(semanticType: SIRType)(using LoweringContext): DefaultUni =
            DefaultUni.Apply(
              DefaultUni.Apply(DefaultUni.ProtoPair, DefaultUni.Integer),
              DefaultUni.Apply(DefaultUni.ProtoList, DefaultUni.Data)
            )
    }

    /** Parameterized representation for builtin lists (BuiltinList[Data]).
      *
      * `elementRepr` describes how each element is stored at the UPLC level. Serialized via
      * `listData`/`unListData`.
      *
      * For pair lists (BuiltinList[BuiltinPair[Data,Data]]), use `SumPairBuiltinList` instead —
      * those use `mapData`/`unMapData` and have fundamentally different UPLC type.
      */
    case class SumBuiltinList(elementRepr: LoweredValueRepresentation)
        extends SumCaseClassRepresentation(false, elementRepr.isDataCentric) {

        override def uplcType(semanticType: SIRType)(using lctx: LoweringContext): SIRType = {
            val elemSemType =
                SumBuiltinList.retrieveListElementType(semanticType).getOrElse(SIRType.Data.tp)
            val elemUplcType = elementRepr.uplcType(elemSemType)
            SIRType.BuiltinList(elemUplcType)
        }

        override def defaultUni(semanticType: SIRType)(using lctx: LoweringContext): DefaultUni = {
            val elemSemType =
                SumBuiltinList.retrieveListElementType(semanticType).getOrElse(SIRType.Data.tp)
            DefaultUni.List(elementRepr.defaultUni(elemSemType))
        }

        override def isCompatibleOn(
            tp: SIRType,
            repr: LoweredValueRepresentation,
            pos: SIRPosition
        )(using lctx: LoweringContext): Boolean =
            repr match {
                case SumBuiltinList(otherElemRepr) =>
                    if elementRepr == otherElemRepr then true
                    else
                        SumBuiltinList.retrieveListElementType(tp) match
                            case Some(elemType) =>
                                elementRepr.isCompatibleOn(elemType, otherElemRepr, pos)
                            case None =>
                                // FreeUnificator or unknown — compatible if both are data-centric
                                elementRepr.isDataCentric && otherElemRepr.isDataCentric
                case SumPairBuiltinList(otherKeyRepr, otherValueRepr) =>
                    SumBuiltinList.retrieveListElementType(tp) match
                        case Some(elemType) =>
                            val otherElemRepr =
                                ProductCaseClassRepresentation.ProdBuiltinPair(
                                  otherKeyRepr,
                                  otherValueRepr
                                )
                            elementRepr.isCompatibleOn(elemType, otherElemRepr, pos)
                        case None => elementRepr.isDataCentric
                // SumUplcConstr is compatible with SumBuiltinList(Transparent) —
                // a Transparent (unknown/passthrough) element TypeVar accepts any list
                // representation as proxy. Unwrapped/Fixed have concrete element-shape
                // expectations and need explicit conversion.
                // SumBuiltinList (a UPLC cons-list) and SumUplcConstr (a Constr chain) are
                // different runtime structures — conversion is always needed, regardless of
                // the element repr. Returning `true` here would cause `toRepresentation` to
                // emit a no-op Proxy wrapper, leaving a BuiltinList value where UplcConstr
                // is expected at runtime.
                case _: SumUplcConstr => false
                case _                => this == repr
            }

        override def isCompatibleWithType(tp: SIRType)(using LoweringContext): Boolean = {
            SIRType.retrieveDataDecl(tp) match
                case Left(_) => false
                case Right(decl) =>
                    decl.name == SIRType.List.dataDecl.name ||
                    decl.name == SIRType.BuiltinList.dataDecl.name
        }

        override def stableKeyInternal(seen: Set[SumReprProxy]): String =
            s"SumBuiltinList(${elementRepr.stableKeyInternal(seen)})"
    }

    object SumBuiltinList {
        def retrieveListElementType(tp: SIRType): Option[SIRType] = {
            tp match
                case SIRType.SumCaseClass(decl, typeArgs) =>
                    typeArgs.headOption
                case SIRType.TypeLambda(params, body) =>
                    retrieveListElementType(body)
                case SIRType.Annotated(inner, _) =>
                    retrieveListElementType(inner)
                case SIRType.TypeProxy(ref) if ref != null =>
                    retrieveListElementType(ref)
                case _ =>
                    None
        }
    }

    /** Representation for pair lists (BuiltinList[BuiltinPair[K,V]]). Serialized via
      * `mapData`/`unMapData` when data-centric. Separate from `SumBuiltinList` because pair lists
      * have a fundamentally different UPLC type and serialization path.
      */
    case class SumPairBuiltinList(
        keyRepr: LoweredValueRepresentation,
        valueRepr: LoweredValueRepresentation
    ) extends SumCaseClassRepresentation(
          false,
          keyRepr.isDataCentric && valueRepr.isDataCentric
        ) {

        override def uplcType(semanticType: SIRType)(using LoweringContext): SIRType = {
            val elemType = SumBuiltinList.retrieveListElementType(semanticType)
            val (keyType, valueType) = elemType match
                case Some(et) => SumPairBuiltinList.extractKeyValueTypes(et)
                case None     => (SIRType.Data.tp, SIRType.Data.tp)
            SIRType.BuiltinList(
              SIRType.BuiltinPair(keyRepr.uplcType(keyType), valueRepr.uplcType(valueType))
            )
        }

        override def defaultUni(semanticType: SIRType)(using LoweringContext): DefaultUni = {
            val elemType = SumBuiltinList.retrieveListElementType(semanticType)
            val (keyType, valueType) = elemType match
                case Some(et) => SumPairBuiltinList.extractKeyValueTypes(et)
                case None     => (SIRType.Data.tp, SIRType.Data.tp)
            DefaultUni.List(
              DefaultUni.Pair(keyRepr.defaultUni(keyType), valueRepr.defaultUni(valueType))
            )
        }

        override def isCompatibleOn(
            tp: SIRType,
            repr: LoweredValueRepresentation,
            pos: SIRPosition
        )(using lctx: LoweringContext): Boolean =
            repr match {
                case SumPairBuiltinList(otherKeyRepr, otherValueRepr) =>
                    SumBuiltinList.retrieveListElementType(tp) match
                        case Some(elemType) =>
                            val (keyType, valueType) =
                                SumPairBuiltinList.extractKeyValueTypes(elemType)
                            keyRepr.isCompatibleOn(keyType, otherKeyRepr, pos) &&
                            valueRepr.isCompatibleOn(valueType, otherValueRepr, pos)
                        case None =>
                            keyRepr.isDataCentric && otherKeyRepr.isDataCentric &&
                            valueRepr.isDataCentric && otherValueRepr.isDataCentric
                case SumBuiltinList(otherElemRepr) =>
                    SumBuiltinList.retrieveListElementType(tp) match
                        case Some(elemType) =>
                            val thisElemRepr =
                                ProductCaseClassRepresentation.ProdBuiltinPair(keyRepr, valueRepr)
                            otherElemRepr.isCompatibleOn(elemType, thisElemRepr, pos)
                        case None => keyRepr.isDataCentric && valueRepr.isDataCentric
                case _ => this == repr
            }

        override def isCompatibleWithType(tp: SIRType)(using LoweringContext): Boolean = {
            SIRType.retrieveDataDecl(tp) match
                case Left(_) => false
                case Right(decl) =>
                    if decl.name == SIRType.PairList.DataDeclName then true
                    else
                        val isList =
                            decl.name == SIRType.List.dataDecl.name ||
                                decl.name == SIRType.BuiltinList.dataDecl.name
                        if !isList then false
                        else
                            SumBuiltinList.retrieveListElementType(tp) match
                                case Some(elementType) =>
                                    ProductCaseClassRepresentation.ProdBuiltinPair.isPairOrTuple2(
                                      elementType
                                    )
                                case None => false
        }

        override def stableKeyInternal(seen: Set[SumReprProxy]): String =
            s"SumPairBuiltinList(${keyRepr.stableKeyInternal(seen)},${valueRepr.stableKeyInternal(seen)})"
    }

    object SumPairBuiltinList {
        def extractKeyValueTypes(elemType: SIRType): (SIRType, SIRType) =
            elemType match
                case SIRType.CaseClass(constrDecl, keyType :: valueType :: _, _)
                    if constrDecl.name == "scalus.uplc.builtin.BuiltinPair" || constrDecl.name == "scala.Tuple2" =>
                    (keyType, valueType)
                case SIRType.TypeLambda(_, body) => extractKeyValueTypes(body)
                case _                           => (SIRType.Data.tp, SIRType.Data.tp)

        /** Compute SumPairBuiltinList from the pair element type (BuiltinPair or Tuple2). */
        def fromElementType(elemType: SIRType, pos: SIRPosition = SIRPosition.empty)(using
            lctx: LoweringContext
        ): SumPairBuiltinList = {
            elemType match
                case SIRType.CaseClass(constrDecl, keyType :: valueType :: _, _)
                    if constrDecl.name == "scalus.uplc.builtin.BuiltinPair" || constrDecl.name == "scala.Tuple2" =>
                    def reprForType(tp: SIRType): LoweredValueRepresentation = tp match
                        case SIRType.TypeNothing | SIRType.FreeUnificator =>
                            TypeVarRepresentation(false)
                        case _: SIRType.TypeVar => TypeVarRepresentation(false)
                        case _ => lctx.typeGenerator(tp).defaultDataRepresentation(tp)
                    SumPairBuiltinList(reprForType(keyType), reprForType(valueType))
                case SIRType.TypeLambda(_, body) =>
                    fromElementType(body, pos)
                case _: SIRType.TypeVar | SIRType.FreeUnificator | SIRType.TypeNothing =>
                    // TypeVar, FreeUnificator, or TypeNothing — key/value types unknown
                    SumPairBuiltinList(TypeVarRepresentation(false), TypeVarRepresentation(false))
                case _ =>
                    throw LoweringException(
                      s"SumPairBuiltinList.fromElementType: expected pair or tuple type, got ${elemType.show}",
                      pos
                    )
        }

    }

    /** Sentinel for native primitive lists — used by intrinsic type proxies. The actual element
      * type is determined by refinement in the lowering from the concrete type context.
      */
    val SumBuiltinListNative: SumBuiltinList = SumBuiltinList(PrimitiveRepresentation.Constant)

    /** SumPairBuiltinList packed as AssocMap (via mapData)
      */
    case object SumDataAssocMap
        extends SumCaseClassRepresentation(true, true)
        with PackedDataDefaultUni {
        override def isCompatibleWithType(tp: SIRType)(using LoweringContext): Boolean = {
            SIRType.retrieveDataDecl(tp) match
                case Left(_) => false
                case Right(decl) =>
                    if decl.name == SIRType.PairList.DataDeclName then true
                    else
                        val isList =
                            decl.name == SIRType.List.dataDecl.name ||
                                decl.name == SIRType.BuiltinList.dataDecl.name
                        if !isList then false
                        else
                            SumBuiltinList.retrieveListElementType(tp) match
                                case Some(elementType) =>
                                    ProductCaseClassRepresentation.ProdBuiltinPair.isPairOrTuple2(
                                      elementType
                                    )
                                case None => false
        }
        override def uplcType(semanticType: SIRType)(using LoweringContext): SIRType =
            SIRType.Data.tp
    }

    /** packed in data representation as a list of data elements. i.e. unListData for unpacking into
      * DataList
      */
    case object PackedSumDataList
        extends SumCaseClassRepresentation(true, true)
        with PackedDataDefaultUni {
        override def isCompatibleWithType(tp: SIRType)(using lctx: LoweringContext): Boolean = {
            SIRType.retrieveDataDecl(tp) match
                case Left(_) => false
                case Right(decl) =>
                    val isList = decl.name == SIRType.List.dataDecl.name ||
                        decl.name == SIRType.BuiltinList.dataDecl.name
                    if !isList then false
                    else
                        SumBuiltinList.retrieveListElementType(tp) match
                            case Some(elemType) =>
                                lctx.typeGenerator(elemType).canBeConvertedToData(elemType)
                            case None => true
        }
        override def uplcType(semanticType: SIRType)(using LoweringContext): SIRType =
            SIRType.Data.tp
    }

    /** Representation as tern Constr(i,x1,...,xn) where i is the index of the constructor and x is
      * a field
      */
    /** Parameterized sum constructor representation. Each variant (constructor tag) has its own
      * `ProdUplcConstr` with per-field representations.
      *
      * UPLC form: `Constr(tag, [v1, v2, ...])` where fields are in the variant's representations.
      * Pattern matching uses the `Case` builtin to dispatch on the tag.
      *
      * @param variants
      *   map from constructor tag to its ProdUplcConstr (field representations)
      */
    case class SumUplcConstr(
        variants: Map[Int, ProductCaseClassRepresentation.ProdUplcConstr]
    ) extends SumCaseClassRepresentation(false, false) {
        override def defaultUni(semanticType: SIRType)(using LoweringContext): DefaultUni =
            DefaultUni.BuiltinValue // Native Constr values stored as BuiltinValue in lists

        override def isCompatibleOn(
            tp: SIRType,
            repr: LoweredValueRepresentation,
            pos: SIRPosition
        )(using lctx: LoweringContext): Boolean =
            isCompatibleOnTracked(tp, repr, Set.empty, pos)

        /** Proxy-tracked compatibility check. `tp` is the SIR type of the sum we're checking
          * (used to derive per-variant field types for the Unwrapped-kind case). The `seen`
          * set tracks SumReprProxy instances already being compared to detect cycles in
          * recursive types (e.g., List tail → List).
          */
        private[lowering] def isCompatibleOnTracked(
            tp: SIRType,
            repr: LoweredValueRepresentation,
            seen: Set[SumReprProxy],
            pos: SIRPosition
        )(using lctx: LoweringContext): Boolean =
            // Fast structural short-circuit: equal `stableKey` ⇒ same repr (cycle-broken by
            // `SumProxy(cycle)`).
            if this.stableKey == repr.stableKey then true
            else
                repr match
                    case proxy: SumReprProxy =>
                        if seen.contains(proxy) then true // cycle — already comparing
                        else if proxy.ref != null then
                            isCompatibleOnTracked(tp, proxy.ref, seen + proxy, pos)
                        else false // unset proxy with a non-matching stableKey
                    case other: SumUplcConstr =>
                        variants.forall { (tag, inProd) =>
                            other.variants.get(tag) match
                                case None => true
                                case Some(outProd) =>
                                    SumUplcConstr.fieldsCompatibleTracked(
                                      inProd.fieldReprs,
                                      outProd.fieldReprs,
                                      seen,
                                      pos
                                    )
                        }
                    case tvr: TypeVarRepresentation =>
                        import SIRType.TypeVarKind.*
                        tvr.kind match
                            case Transparent => true
                            case Fixed       => false
                            case Unwrapped   =>
                                // Compatible if every variant's field reprs are each field
                                // type's default representation. Requires resolving `tp` to
                                // a concrete DataDecl/constructor chain so we can pair each
                                // ProdUplcConstr's fieldReprs with constructor param types.
                                variantsMatchDefaultReprs(tp, pos)
                    case other => this == other

        /** Unwrapped-kind compatibility helper: for each variant, check every field repr is
          * the default for that field's (substituted) SIR type. Returns false if `tp` can't
          * be resolved to a concrete DataDecl (e.g. unresolved TypeVar / FreeUnificator).
          */
        private def variantsMatchDefaultReprs(tp: SIRType, pos: SIRPosition)(using
            lctx: LoweringContext
        ): Boolean = {
            val (constructors, typeArgs, typeParams) = tp match
                case SIRType.SumCaseClass(decl, tArgs) =>
                    (decl.constructors, tArgs, decl.typeParams)
                case SIRType.CaseClass(cd, tArgs, Some(SIRType.SumCaseClass(decl, pArgs))) =>
                    (decl.constructors, pArgs, decl.typeParams)
                case SIRType.CaseClass(cd, tArgs, None) =>
                    (scala.List(cd), tArgs, cd.typeParams)
                case SIRType.TypeLambda(_, body) =>
                    return variantsMatchDefaultReprs(body, pos)
                case SIRType.TypeProxy(ref) if ref != null =>
                    return variantsMatchDefaultReprs(ref, pos)
                case SIRType.Annotated(inner, _) =>
                    return variantsMatchDefaultReprs(inner, pos)
                case _ => return false
            val typeSubst: Map[SIRType.TypeVar, SIRType] =
                typeParams.zip(typeArgs.map(lctx.resolveTypeVarIfNeeded)).toMap
            constructors.zipWithIndex.forall { case (constrDecl, idx) =>
                variants.get(idx) match
                    case None => true
                    case Some(prod) =>
                        prod.fieldReprs.zip(constrDecl.params).forall { case (fr, param) =>
                            val fieldType =
                                SIRType.substitute(param.tp, typeSubst, Map.empty)
                            val defaultRepr =
                                lctx.typeGenerator(fieldType).defaultRepresentation(fieldType)
                            fr == defaultRepr || fr.stableKey == defaultRepr.stableKey
                        }
            }
        }

        override def stableKeyInternal(seen: Set[SumReprProxy]): String =
            variants.toSeq
                .sortBy(_._1)
                .map { case (tag, prod) => s"$tag:${prod.stableKeyInternal(seen)}" }
                .mkString("SumUplcConstr(", ",", ")")
    }

    object SumUplcConstr {

        /** Compare field repr lists with proxy tracking. `tp` unavailable at this level; nested
          * sum checks pass `FreeUnificator` (the Unwrapped-kind branch returns false for
          * un-resolved tp per agreed semantics).
          */
        private[lowering] def fieldsCompatibleTracked(
            inFields: scala.List[LoweredValueRepresentation],
            outFields: scala.List[LoweredValueRepresentation],
            seen: Set[SumReprProxy],
            pos: SIRPosition
        )(using LoweringContext): Boolean =
            inFields.size == outFields.size &&
                inFields.zip(outFields).forall { (inR, outR) =>
                    (inR, outR) match
                        case (a: SumUplcConstr, b) =>
                            a.isCompatibleOnTracked(SIRType.FreeUnificator, b, seen, pos)
                        case (a: SumReprProxy, b: SumReprProxy) =>
                            if seen.contains(a) || seen.contains(b) then true
                            else if a.ref != null && b.ref != null then
                                a.ref match
                                    case aSum: SumUplcConstr =>
                                        aSum.isCompatibleOnTracked(
                                          SIRType.FreeUnificator,
                                          b.ref,
                                          seen + a + b,
                                          pos
                                        )
                                    case _ => a.ref == b.ref
                            else true
                        case (a: SumReprProxy, b) =>
                            if seen.contains(a) then true
                            else if a.ref != null then
                                a.ref match
                                    case aSum: SumUplcConstr =>
                                        aSum.isCompatibleOnTracked(
                                          SIRType.FreeUnificator,
                                          b,
                                          seen + a,
                                          pos
                                        )
                                    case _ => a.ref.isCompatibleOn(SIRType.FreeUnificator, b, pos)
                            else true
                        case (a, b: SumReprProxy) =>
                            if seen.contains(b) then true
                            else if b.ref != null then
                                a.isCompatibleOn(SIRType.FreeUnificator, b.ref, pos)
                            else true
                        case (
                              ProductCaseClassRepresentation.ProdUplcConstr(tagA, fieldsA),
                              ProductCaseClassRepresentation.ProdUplcConstr(tagB, fieldsB)
                            ) =>
                            tagA == tagB && fieldsCompatibleTracked(fieldsA, fieldsB, seen, pos)
                        case _ =>
                            val compatible = inR.isCompatibleOn(SIRType.FreeUnificator, outR, pos)
                            // Debug: check for TypeVarRepresentation(Fixed) vs Constant mismatch
                            (inR, outR) match
                                case (
                                      TypeVarRepresentation(SIRType.TypeVarKind.Fixed),
                                      PrimitiveRepresentation.Constant
                                    ) | (
                                      PrimitiveRepresentation.Constant,
                                      TypeVarRepresentation(SIRType.TypeVarKind.Fixed)
                                    ) =>
                                    System.err.println(
                                      s"[DEBUG fieldsCompatibleTracked] Checking compatibility:"
                                    )
                                    System.err.println(s"  inR = $inR")
                                    System.err.println(s"  outR = $outR")
                                    System.err.println(s"  compatible = $compatible")
                                case _ => // no debug
                            compatible
                }
    }

    /** Mutable proxy for self-referential sum representations. Like TypeProxy for SIRType — allows
      * circular references in recursive types (e.g., List tail field references the parent List's
      * SumUplcConstr). Set `ref` after constructing the real representation.
      */
    class SumReprProxy(var ref: SumCaseClassRepresentation)
        extends SumCaseClassRepresentation(false, false) {

        // `ref` is set by `buildSumUplcConstr` before it returns, and no external caller
        // observes the proxy mid-build (all field reprs are computed via
        // `typeGenerator(paramType).defaultRepresentation(paramType)`, which doesn't
        // compare against the partially-built selfProxy). So every method below delegates
        // directly to `ref` — no null-guards needed.

        override def defaultUni(semanticType: SIRType)(using LoweringContext): DefaultUni =
            ref.defaultUni(semanticType)

        override def isCompatibleOn(
            tp: SIRType,
            repr: LoweredValueRepresentation,
            pos: SIRPosition
        )(using lctx: LoweringContext): Boolean =
            ref.isCompatibleOn(tp, repr, pos)

        override def isCompatibleWithType(tp: SIRType)(using LoweringContext): Boolean =
            ref.isCompatibleWithType(tp)

        override def doc: Doc = ref.doc

        /** Cycle-detecting delegation to `ref`. `seen` tracks proxy *instances* (identity-keyed
          * via default `equals`/`hashCode`) already being rendered on this call stack; on
          * revisit, emit `SumProxy(cycle)` as the stable placeholder — the common case for
          * self-recursive types like `List`'s tail field.
          */
        override def stableKeyInternal(seen: Set[SumReprProxy]): String =
            if seen.contains(this) then "SumProxy(cycle)"
            else s"SumProxy(${ref.stableKeyInternal(seen + this)})"
    }

}

sealed trait ProductCaseClassRepresentation(val isPackedData: Boolean, val isDataCentric: Boolean)
    extends LoweredValueRepresentation {

    override def isCompatibleWithType(tp: SIRType)(using LoweringContext): Boolean = {
        SIRType.isProd(tp)
    }

}

object ProductCaseClassRepresentation {

    case object PackedDataList
        extends ProductCaseClassRepresentation(true, true)
        with PackedDataDefaultUni {
        override def uplcType(semanticType: SIRType)(using LoweringContext): SIRType =
            SIRType.Data.tp
    }

    case object ProdDataList extends ProductCaseClassRepresentation(false, true) {
        override def uplcType(semanticType: SIRType)(using LoweringContext): SIRType =
            SIRType.List(SIRType.Data.tp)
        override def defaultUni(semanticType: SIRType)(using LoweringContext): DefaultUni =
            DefaultUni.Apply(DefaultUni.ProtoList, DefaultUni.Data)
    }

    case object PackedDataMap
        extends ProductCaseClassRepresentation(true, true)
        with PackedDataDefaultUni {
        override def isCompatibleWithType(tp: SIRType)(using LoweringContext): Boolean = {
            SIRType.retrieveConstrDecl(tp) match
                case Left(_) => false
                case Right(constrDecl) =>
                    constrDecl.name == "scalus.cardano.onchain.plutus.prelude.AssocMap" || constrDecl.name == "scalus.cardano.onchain.plutus.prelude.SortedMap"
        }
        override def uplcType(semanticType: SIRType)(using LoweringContext): SIRType =
            SIRType.Data.tp
    }

    /** Data.Unconstr will give us a pair from data and index of the constructor.
      */
    case object ProdDataConstr
        extends ProductCaseClassRepresentation(true, true)
        with PackedDataDefaultUni {

        override def isCompatibleOn(
            tp: SIRType,
            repr: LoweredValueRepresentation,
            pos: SIRPosition
        )(using
            LoweringContext
        ): Boolean =
            repr match {
                case ProdDataConstr           => true
                case TypeVarRepresentation(_) => true
                case other                    => false
            }
        override def uplcType(semanticType: SIRType)(using LoweringContext): SIRType =
            SIRType.Data.tp
    }

    case object PairIntDataList extends ProductCaseClassRepresentation(false, true) {
        override def defaultUni(semanticType: SIRType)(using LoweringContext): DefaultUni =
            DefaultUni.Apply(
              DefaultUni.Apply(DefaultUni.ProtoPair, DefaultUni.Integer),
              DefaultUni.Apply(DefaultUni.ProtoList, DefaultUni.Data)
            )
    }

    /** BuiltinPair with parameterized component representations.
      *
      * `PairData` is the common case where both components are `PackedData` (i.e.,
      * `BuiltinPair[Data, Data]`). In the future, native component representations like
      * `ProdBuiltinPair(Constant, Constant)` for `BuiltinPair[Integer, ByteString]` are possible.
      */
    case class ProdBuiltinPair(
        fstRepr: LoweredValueRepresentation,
        sndRepr: LoweredValueRepresentation
    ) extends ProductCaseClassRepresentation(
          false,
          fstRepr.isDataCentric && sndRepr.isDataCentric
        ) {

        override def isCompatibleWithType(tp: SIRType)(using LoweringContext): Boolean = {
            ProdBuiltinPair.isPairOrTuple2(tp)
        }

        override def uplcType(semanticType: SIRType)(using lctx: LoweringContext): SIRType = {
            val (fstSemType, sndSemType) = ProdBuiltinPair.extractPairComponentTypes(semanticType)
            SIRType.BuiltinPair(fstRepr.uplcType(fstSemType), sndRepr.uplcType(sndSemType))
        }

        override def defaultUni(semanticType: SIRType)(using lctx: LoweringContext): DefaultUni = {
            val (fstSemType, sndSemType) = ProdBuiltinPair.extractPairComponentTypes(semanticType)
            DefaultUni.Pair(fstRepr.defaultUni(fstSemType), sndRepr.defaultUni(sndSemType))
        }

        override def isCompatibleOn(
            tp: SIRType,
            repr: LoweredValueRepresentation,
            pos: SIRPosition
        )(using lctx: LoweringContext): Boolean =
            repr match {
                case ProdBuiltinPair(otherFst, otherSnd) =>
                    val (fstType, sndType) = ProdBuiltinPair.extractPairComponentTypes(tp)
                    fstRepr.isCompatibleOn(fstType, otherFst, pos) && sndRepr.isCompatibleOn(
                      sndType,
                      otherSnd,
                      pos
                    )
                case tvr: TypeVarRepresentation =>
                    if tvr.isBuiltin then true
                    else
                        val resolved = lctx.typeGenerator(tp).defaultTypeVarReperesentation(tp)
                        resolved.isCompatibleOn(tp, this, pos)
                case _ => false
            }

        override def stableKeyInternal(
            seen: Set[SumCaseClassRepresentation.SumReprProxy]
        ): String =
            s"ProdBuiltinPair(${fstRepr.stableKeyInternal(seen)},${sndRepr.stableKeyInternal(seen)})"
    }

    object ProdBuiltinPair {
        @tailrec
        def isPairOrTuple2(tp: SIRType): Boolean =
            tp match
                case SIRType.CaseClass(decl, typeArgs, _) =>
                    decl.name == "scalus.uplc.builtin.BuiltinPair"
                    ||
                    decl.name == "scala.Tuple2"
                case SIRType.TypeLambda(params, body) =>
                    isPairOrTuple2(body)
                case SIRType.TypeProxy(ref) =>
                    isPairOrTuple2(ref)
                case _ => false

        @tailrec
        def extractPairComponentTypes(tp: SIRType): (SIRType, SIRType) =
            tp match
                case SIRType.CaseClass(decl, fstType :: sndType :: _, _)
                    if decl.name == "scalus.uplc.builtin.BuiltinPair" || decl.name == "scala.Tuple2" =>
                    (fstType, sndType)
                case SIRType.TypeLambda(_, body) => extractPairComponentTypes(body)
                case SIRType.TypeProxy(ref)      => extractPairComponentTypes(ref)
                case _                           => (SIRType.Data.tp, SIRType.Data.tp)
    }

    /** Parameterized constructor representation where each field has its own representation.
      *
      * UPLC form: `Constr(tag, [v1, v2, ..., vN])` where each `vi` is in `fieldReprs(i)`. Unlike
      * `ProdDataConstr` which forces all fields to Data, this allows native field representations
      * (e.g., `Constr(0, [Integer(42), ByteString(#ff)])`).
      *
      * @param tag
      *   constructor tag (index in the DataDecl's constructor list)
      * @param fieldReprs
      *   representation for each field, in order
      */
    case class ProdUplcConstr(
        tag: Int,
        fieldReprs: scala.List[LoweredValueRepresentation]
    ) extends ProductCaseClassRepresentation(false, false) {

        override def defaultUni(semanticType: SIRType)(using LoweringContext): DefaultUni =
            DefaultUni.BuiltinValue // Native Constr values stored as BuiltinValue in lists

        override def uplcType(semanticType: SIRType)(using LoweringContext): SIRType =
            semanticType

        override def isCompatibleOn(
            tp: SIRType,
            repr: LoweredValueRepresentation,
            pos: SIRPosition
        )(using LoweringContext): Boolean =
            isCompatibleOnTracked(tp, repr, Set.empty, pos)

        private[lowering] def isCompatibleOnTracked(
            tp: SIRType,
            repr: LoweredValueRepresentation,
            seen: Set[SumCaseClassRepresentation.SumReprProxy],
            pos: SIRPosition
        )(using lctx: LoweringContext): Boolean =
            if this.stableKey == repr.stableKey then true
            else
                repr match {
                    case ProdUplcConstr(otherTag, otherFieldReprs) =>
                        tag == otherTag &&
                        SumCaseClassRepresentation.SumUplcConstr.fieldsCompatibleTracked(
                          fieldReprs,
                          otherFieldReprs,
                          seen,
                          pos
                        )
                    case tvr: TypeVarRepresentation =>
                        import SIRType.TypeVarKind.*
                        tvr.kind match
                            case Transparent => true
                            case Fixed       => false
                            case Unwrapped   => fieldsMatchDefaultReprs(tp, pos)
                    case _ => false
                }

        /** Unwrapped compatibility: every fieldRepr matches its field's default repr derived
          * from `tp`'s constructor param types. Returns false if tp can't be resolved to a
          * concrete constructor.
          */
        private def fieldsMatchDefaultReprs(tp: SIRType, pos: SIRPosition)(using
            lctx: LoweringContext
        ): Boolean = {
            val (constrDecl, typeArgs, typeParams): (
                scalus.compiler.sir.ConstrDecl,
                scala.List[SIRType],
                scala.List[SIRType.TypeVar]
            ) = tp match
                case SIRType.CaseClass(cd, tArgs, _) =>
                    (cd, tArgs, cd.typeParams)
                case SIRType.SumCaseClass(decl, tArgs) =>
                    // ProdUplcConstr.tag indexes into decl.constructors
                    if tag < 0 || tag >= decl.constructors.length then return false
                    (decl.constructors(tag), tArgs, decl.typeParams)
                case SIRType.TypeLambda(_, body) =>
                    return fieldsMatchDefaultReprs(body, pos)
                case SIRType.TypeProxy(ref) if ref != null =>
                    return fieldsMatchDefaultReprs(ref, pos)
                case SIRType.Annotated(inner, _) =>
                    return fieldsMatchDefaultReprs(inner, pos)
                case _ => return false
            val typeSubst: Map[SIRType.TypeVar, SIRType] =
                typeParams.zip(typeArgs.map(lctx.resolveTypeVarIfNeeded)).toMap
            fieldReprs.zip(constrDecl.params).forall { case (fr, param) =>
                val fieldType = SIRType.substitute(param.tp, typeSubst, Map.empty)
                val defaultRepr = lctx.typeGenerator(fieldType).defaultRepresentation(fieldType)
                fr == defaultRepr || fr.stableKey == defaultRepr.stableKey
            }
        }

        override def stableKeyInternal(
            seen: Set[SumCaseClassRepresentation.SumReprProxy]
        ): String =
            fieldReprs
                .map(_.stableKeyInternal(seen))
                .mkString(s"ProdUplcConstr($tag,[", ",", "])")
    }

    /** BuiltinArray with parameterized element representation.
      *
      * `ArrayData` is the common case where elements are `PackedData` (i.e., `BuiltinArray[Data]`).
      * In the future, native element representations like `ProdBuiltinArray(Constant)` for
      * `BuiltinArray[Integer]` are possible.
      */
    case class ProdBuiltinArray(elementRepr: LoweredValueRepresentation)
        extends ProductCaseClassRepresentation(false, elementRepr.isDataCentric) {

        override def defaultUni(semanticType: SIRType)(using lctx: LoweringContext): DefaultUni = {
            val elemType =
                ProdBuiltinArray.extractElementType(semanticType).getOrElse(SIRType.Data.tp)
            DefaultUni.Array(elementRepr.defaultUni(elemType))
        }

        override def isCompatibleWithType(tp: SIRType)(using LoweringContext): Boolean = {
            tp match
                case SIRType.BuiltinArray(_) => true
                case _                       => false
        }

        override def isCompatibleOn(
            tp: SIRType,
            repr: LoweredValueRepresentation,
            pos: SIRPosition
        )(using lctx: LoweringContext): Boolean =
            repr match {
                case ProdBuiltinArray(otherElemRepr) =>
                    ProdBuiltinArray.extractElementType(tp) match
                        case Some(elemType) =>
                            elementRepr.isCompatibleOn(elemType, otherElemRepr, pos)
                        case None => elementRepr.isDataCentric && otherElemRepr.isDataCentric
                case tvr: TypeVarRepresentation =>
                    if tvr.isBuiltin then true
                    else
                        val resolved = lctx.typeGenerator(tp).defaultTypeVarReperesentation(tp)
                        resolved.isCompatibleOn(tp, this, pos)
                case _ => false
            }

        override def stableKeyInternal(
            seen: Set[SumCaseClassRepresentation.SumReprProxy]
        ): String =
            s"ProdBuiltinArray(${elementRepr.stableKeyInternal(seen)})"
    }

    object ProdBuiltinArray {
        @tailrec
        def extractElementType(tp: SIRType): Option[SIRType] =
            tp match
                case SIRType.BuiltinArray(elemType) => Some(elemType)
                case SIRType.TypeLambda(_, body)    => extractElementType(body)
                case SIRType.TypeProxy(ref)         => extractElementType(ref)
                case _                              => None
    }

    /** Representation for BuiltinArray packed as Data.List.
      *
      * This is used when the array needs to be Data-compatible (e.g., stored in a case class
      * field). The array is converted to a list and then wrapped in Data.List.
      */
    case object PackedArrayAsList
        extends ProductCaseClassRepresentation(true, true)
        with PackedDataDefaultUni {
        override def isCompatibleWithType(tp: SIRType)(using LoweringContext): Boolean = {
            tp match
                case SIRType.BuiltinArray(_) => true
                case _                       => false
        }
        override def uplcType(semanticType: SIRType)(using LoweringContext): SIRType =
            SIRType.Data.tp
    }

    case class OneElementWrapper(representation: LoweredValueRepresentation)
        extends ProductCaseClassRepresentation(
          representation.isPackedData,
          representation.isDataCentric
        ) {

        override def uplcType(semanticType: SIRType)(using LoweringContext): SIRType =
            val argType = OneElementWrapper.retrieveArgType(semanticType, SIRPosition.empty)
            representation.uplcType(argType)

        override def defaultUni(semanticType: SIRType)(using LoweringContext): DefaultUni =
            SIRType.collectProd(semanticType) match
                case Some(_, _, _) =>
                    val argType =
                        OneElementWrapper.retrieveArgType(semanticType, SIRPosition.empty)
                    representation.defaultUni(argType)
                case None => representation.defaultUni(semanticType)

        override def isCompatibleOn(
            tp: SIRType,
            repr: LoweredValueRepresentation,
            pos: SIRPosition
        )(using
            LoweringContext
        ): Boolean =
            repr match {
                case OneElementWrapper(innerRepr) =>
                    val argType = OneElementWrapper.retrieveArgType(tp, pos)
                    representation.isCompatibleOn(argType, innerRepr, pos)
                case _ => false
            }

        override def stableKeyInternal(
            seen: Set[SumCaseClassRepresentation.SumReprProxy]
        ): String =
            s"OneElementWrapper(${representation.stableKeyInternal(seen)})"
    }

    object OneElementWrapper {

        def retrieveArgType(tp: SIRType, pos: SIRPosition): SIRType = {
            SIRType.collectProd(tp) match {
                case Some(typeParams, constrDecl, typeArgs) =>
                    val typeParamsMap = typeParams.zip(typeArgs).toMap
                    val param = constrDecl.params.head
                    SIRType.substitute(param.tp, typeParamsMap, Map.empty)
                case None =>
                    tp match
                        case _: SIRType.TypeVar     => tp
                        case SIRType.FreeUnificator => tp
                        case SIRType.TypeLambda(params, body) =>
                            retrieveArgType(body, pos)
                        case SIRType.TypeProxy(ref) =>
                            retrieveArgType(ref, pos)
                        case _ =>
                            throw LoweringException(
                              s"OneElementWrapper can be used only with product case classes, but got $tp",
                              pos
                            )
            }
        }

    }

}

case class InOutRepresentationPair(
    inRepr: LoweredValueRepresentation,
    outRepr: LoweredValueRepresentation
)

/** Representation for lambda function. By default, lanbda-s accept default reperesentation for
  * input and output types. But when we pass functions to type-parametrized functions, then calling
  * party does not know about real parameter types and can't use default representation, so pass
  * parameters as packed data.
  *
  * So, we translate higher-order functions to packed data representation when pass as arguments to
  * type-parametrized functions.
  *
  * But some builtin function accept more then one representations, because they poplymorhiocj on
  * plutus level. (i.e. have builtin type variables). For example: makeCons work with SumDataList
  * and SumPairDataList, so we need to reevaluate representation when argument type is known.
  */
case class LambdaRepresentation(
    funTp: SIRType,
    canonicalRepresentationPair: InOutRepresentationPair,
) extends LoweredValueRepresentation {

    override def defaultUni(semanticType: SIRType)(using LoweringContext): DefaultUni =
        DefaultUni.Data // Lambda values can't be stored as UPLC constants

    override def isPackedData: Boolean = false

    override def isDataCentric: Boolean = false

    override def isCompatibleOn(tp: SIRType, repr: LoweredValueRepresentation, pos: SIRPosition)(
        using LoweringContext
    ): Boolean = {
        repr match {
            case comparator: LambdaRepresentation =>
                val (inputType, outputType) = retrieveInputAndOutputType(tp, pos)
                val InOutRepresentationPair(inRepr, outRepr) =
                    reprFun(inputType, pos, canonicalRepresentationPair.inRepr)
                val InOutRepresentationPair(otherInRepr, otherOutRepr) =
                    comparator.reprFun(
                      inputType,
                      pos,
                      comparator.canonicalRepresentationPair.inRepr
                    )
                inRepr.isCompatibleOn(inputType, otherInRepr, pos) &&
                outRepr.isCompatibleOn(outputType, otherOutRepr, pos)
            case tvr: TypeVarRepresentation =>
                tvr.isBuiltin || {
                    val (inputType, outputType) = retrieveInputAndOutputType(tp, pos)
                    val InOutRepresentationPair(inRepr, outRepr) =
                        reprFun(inputType, pos, canonicalRepresentationPair.inRepr)
                    isTypeVarCompatibleOn(inputType, inRepr, pos)
                    && isTypeVarCompatibleOn(outputType, outRepr, pos)
                }
            case _ => false
        }

    }

    /** Resolve the input/output representation pair for a function application.
      *
      * Given the argument type and repr, unifies the type with the function's input type pattern to
      * resolve builtin type variables. Then derives output repr by extracting TypeVar→repr mappings
      * from the argument's type/repr structure.
      *
      * E.g., for `headList: ∀A. BuiltinList[A] → A` applied to `BuiltinList[Int]` with
      * `SumBuiltinList(PackedData)`: type substitution gives `A → Int`, repr extraction gives
      * `A → PackedData`, so output repr = `PackedData`.
      */
    def reprFun(tp: SIRType, pos: SIRPosition, argRepr: LoweredValueRepresentation)(using
        lctx: LoweringContext
    ): InOutRepresentationPair = {
        SIRType.collectPolyOrFun(funTp) match
            case Some((typeVars, input, output)) =>
                val builtinTypeVars = typeVars.filter(_.isBuiltin)
                if typeVars.isEmpty then {
                    canonicalRepresentationPair
                } else
                    // Substitute types and derive representations for all TypeVars.
                    // Builtin TypeVars take repr from the actual argument (via extractTypeVarReprs).
                    // Non-builtin TypeVars use defaultTypeVarRepresentation of the resolved type.
                    val tvGenContext = SIRType.createMinimalTypeVarGenerationContext(
                      0L,
                      List(funTp)
                    )
                    val renamedTp =
                        if SIRType.isTypeVarsUsedIn(typeVars, tp) then
                            val newTpTypevars =
                                typeVars.map(tv => (tv, tvGenContext.freshCopy(tv))).toMap
                            val renamingContext =
                                RenamingTypeVars.makeContext(newTpTypevars, tvGenContext)
                            RenamingTypeVars.inType(tp, renamingContext)
                        else tp
                    SIRUnify.topLevelUnifyType(
                      renamedTp,
                      input,
                      SIRUnify.Env.empty.withUpcasting
                    ) match {
                        case SIRUnify.UnificationSuccess(env, unificator) =>
                            val allSubstitutes = typeVars
                                .map(tv => (tv, env.filledTypes.getOrElse(tv, tv)))
                                .toMap
                            val substitutedInput =
                                SIRType.substitute(input, allSubstitutes, Map.empty)
                            val substitutedOutput =
                                SIRType.substitute(output, allSubstitutes, Map.empty)
                            val (inputTypeVars, _) = SIRType.partitionGround(
                              typeVars,
                              substitutedInput
                            )
                            val newInput =
                                if inputTypeVars.isEmpty then substitutedInput
                                else SIRType.TypeLambda(inputTypeVars, substitutedInput)
                            val (outputTypeVars, _) = SIRType.partitionGround(
                              typeVars,
                              substitutedOutput
                            )
                            val newOutput =
                                if outputTypeVars.isEmpty then substitutedOutput
                                else SIRType.TypeLambda(outputTypeVars, substitutedOutput)
                            // Extract TypeVar→repr mappings from the input type/repr structure.
                            // This tells us what repr each builtin TypeVar has in the actual argument.
                            val reprSubstitutes = extractTypeVarReprs(
                              input,
                              argRepr,
                              builtinTypeVars.toSet
                            )
                            // Compute the representation for a type position, accounting for
                            // TypeVar origins across curried applications.
                            //
                            // Walks three structures in parallel:
                            // - declaredParamType: parameter type from the function definition (may have TypeVars)
                            // - argumentType: the same type after unification substitution (concrete types)
                            // - declaredRepr: representation from canonical pair, carries TypeVar
                            //   origin info from previous applications
                            //
                            val inRepr = resolveCanonicalReprInFun(
                              input,
                              newInput,
                              canonicalRepresentationPair.inRepr,
                              reprSubstitutes,
                              allSubstitutes,
                              pos
                            )
                            val outRepr = resolveCanonicalReprInFun(
                              output,
                              newOutput,
                              canonicalRepresentationPair.outRepr,
                              reprSubstitutes,
                              allSubstitutes,
                              pos
                            )
                            InOutRepresentationPair(inRepr, outRepr)
                        case SIRUnify.UnificationFailure(path, left, right) =>
                            throw LoweringException(
                              s"Can't unify function type $tp with input type $input and output type $output, " +
                                  s"because of unification failure: ${path}.\nLeft: ${left}, Right: ${right}",
                              pos
                            )
                    }
            case None =>
                canonicalRepresentationPair
    }

    /** Resolve TypeVars in a canonical representation based on concrete argument types.
      *
      * Walks the declared type, argument type, and canonical repr in parallel:
      *   1. TypeVar(builtin) → use reprSubstitutes (from extractTypeVarReprs)
      *   2. TypeVar(non-builtin) → defaultTypeVarRepresentation of argument type
      *   3. Fun → recurse into in/out with LambdaRepresentation's in/out reprs
      *   4. TypeLambda → recurse into body
      *   5. Compound reprs (SumBuiltinList, ProdBuiltinPair, etc.) → recursively resolve inner
      *      reprs
      *   6. Otherwise → defaultRepresentation of argument type
      */
    private def resolveCanonicalReprInFun(
        declaredParamType: SIRType,
        argumentType: SIRType,
        declaredRepr: LoweredValueRepresentation,
        reprSubstitutes: Map[SIRType.TypeVar, LoweredValueRepresentation],
        allSubstitutes: Map[SIRType.TypeVar, SIRType],
        pos: SIRPosition
    )(using lctx: LoweringContext): LoweredValueRepresentation = {
        def resolve(
            declaredParamType: SIRType,
            argumentType: SIRType,
            declaredRepr: LoweredValueRepresentation
        ): LoweredValueRepresentation =
            declaredParamType match
                case tv: SIRType.TypeVar if tv.isBuiltin && reprSubstitutes.contains(tv) =>
                    reprSubstitutes(tv)
                case tv: SIRType.TypeVar if !tv.isBuiltin =>
                    lctx.typeGenerator(argumentType).defaultTypeVarReperesentation(argumentType)
                case SIRType.Fun(defIn, defOut) =>
                    (argumentType, declaredRepr) match
                        case (SIRType.Fun(argIn, argOut), lr: LambdaRepresentation) =>
                            val inR =
                                resolve(defIn, argIn, lr.canonicalRepresentationPair.inRepr)
                            val outR =
                                resolve(defOut, argOut, lr.canonicalRepresentationPair.outRepr)
                            LambdaRepresentation(argumentType, InOutRepresentationPair(inR, outR))
                        case (SIRType.TypeLambda(params, argBody), _) =>
                            val inner = resolve(declaredParamType, argBody, declaredRepr)
                            val remaining = params.filter(p =>
                                p.isBuiltin && !allSubstitutes.get(p).exists(_ != p)
                            )
                            inner match
                                case lr: LambdaRepresentation if remaining.nonEmpty =>
                                    LambdaRepresentation(
                                      SIRType.TypeLambda(remaining, lr.funTp),
                                      lr.canonicalRepresentationPair
                                    )
                                case other => other
                        case (SIRType.Fun(_, _), other) =>
                            throw LoweringException(
                              s"resolveCanonicalReprInFun: Fun type but declaredRepr is not LambdaRepresentation: ${other.show}",
                              pos
                            )
                        case _ =>
                            throw LoweringException(
                              s"resolveCanonicalReprInFun: declaredParamType is Fun but argumentType is ${argumentType.show}",
                              pos
                            )
                case SIRType.TypeLambda(_, defBody) =>
                    argumentType match
                        case SIRType.TypeLambda(_, argBody) =>
                            resolve(defBody, argBody, declaredRepr)
                        case _ => resolve(defBody, argumentType, declaredRepr)
                case _ =>
                    declaredRepr match
                        case tvr: TypeVarRepresentation if tvr.isBuiltin =>
                            lctx.typeGenerator(argumentType).defaultRepresentation(argumentType)
                        case tvr: TypeVarRepresentation if !tvr.isBuiltin =>
                            lctx.typeGenerator(argumentType)
                                .defaultTypeVarReperesentation(argumentType)
                        case suc: SumCaseClassRepresentation.SumUplcConstr =>
                            // Try to substitute element reprs from `reprSubstitutes`
                            // (e.g. `List[B]` with B → mapper outputRepr) so the result
                            // list's element repr matches the actual bytes produced by the
                            // body. Falls back to defaultRepresentation if no substitution
                            // can be derived from the TypeVar typeArgs.
                            substituteSumUplcConstrFromReprs(
                              declaredParamType,
                              argumentType,
                              suc,
                              reprSubstitutes
                            ).getOrElse(
                              lctx.typeGenerator(argumentType).defaultRepresentation(argumentType)
                            )
                        case other =>
                            val result = lctx
                                .typeGenerator(argumentType)
                                .defaultRepresentation(argumentType)
                            result

        resolve(declaredParamType, argumentType, declaredRepr)
    }

    /** Substitute reprSubstitutes into a SumUplcConstr's variant fields when their type positions
      * correspond to TypeVars we have actual reprs for.
      *
      * For e.g. `declaredParamType = List[B]`, `declaredRepr = SumUplcConstr(... [Transparent, …])`
      * and `reprSubstitutes = {B → ProdUplcConstr(SolutionEntry [Fixed, …])}`: builds
      * `SumUplcConstr(... [reprSubstitutes(B), …])` so the list's element repr matches what the
      * upstream caller actually produces (preserving Fixed-encoded fields), instead of falling back
      * to the type-default repr (Constant-encoded fields) which would mislabel bytes.
      *
      * Returns None if no substitution can be made (caller should fall back to
      * defaultRepresentation).
      */
    private def substituteSumUplcConstrFromReprs(
        declaredParamType: SIRType,
        argumentType: SIRType,
        suc: SumCaseClassRepresentation.SumUplcConstr,
        reprSubstitutes: Map[SIRType.TypeVar, LoweredValueRepresentation]
    )(using lctx: LoweringContext): Option[SumCaseClassRepresentation.SumUplcConstr] = {
        if reprSubstitutes.isEmpty then return None
        // Extract decl + typeArgs from declaredParamType.
        def extract(tp: SIRType): Option[(DataDecl, scala.List[SIRType])] = tp match
            case SIRType.SumCaseClass(decl, args)      => Some((decl, args))
            case SIRType.CaseClass(_, _, Some(p))      => extract(p)
            case SIRType.TypeLambda(_, body)           => extract(body)
            case SIRType.TypeProxy(ref) if ref != null => extract(ref)
            case SIRType.Annotated(t1, _)              => extract(t1)
            case _                                     => None
        extract(declaredParamType) match
            case None => None
            case Some((decl, typeArgs)) => {
                // Map decl's typeParam name → reprSubstitutes for any typeArg that's a substituted TypeVar.
                val nameToRepr: Map[String, LoweredValueRepresentation] =
                    decl.typeParams
                        .zip(typeArgs)
                        .flatMap {
                            case (param, argTv: SIRType.TypeVar) =>
                                reprSubstitutes.get(argTv).map(r => param.name -> r)
                            case _ => None
                        }
                        .toMap
                if nameToRepr.isEmpty then return None
                // Walk variants: for each constructor field whose param.tp is a TypeVar in nameToRepr,
                // substitute that field's repr. Otherwise keep existing repr.
                val newVariants = suc.variants.map { case (tag, puc) =>
                    val constructor = decl.constructors(tag)
                    val newFieldReprs =
                        puc.fieldReprs.zip(constructor.params).map { case (fr, param) =>
                            param.tp match
                                case tv: SIRType.TypeVar =>
                                    nameToRepr.getOrElse(tv.name, fr)
                                case SIRType.TypeProxy(ref) =>
                                    ref match
                                        case tv: SIRType.TypeVar =>
                                            nameToRepr.getOrElse(tv.name, fr)
                                        case _ => fr
                                case _ => fr
                        }
                    tag -> ProductCaseClassRepresentation.ProdUplcConstr(puc.tag, newFieldReprs)
                }
                Some(SumCaseClassRepresentation.SumUplcConstr(newVariants))
            }
    }

    /** Extract TypeVar→repr mappings by walking a type pattern and actual repr in parallel.
      *
      * When a builtin TypeVar is found at a position in the type, the corresponding repr from the
      * actual repr structure is recorded.
      *
      * E.g., type `BuiltinList[A]` with repr `SumBuiltinList(PackedData)`: `A` is at element
      * position → `{A → PackedData}`
      */
    private def extractTypeVarReprs(
        typePattern: SIRType,
        actualRepr: LoweredValueRepresentation,
        builtinTypeVars: Set[SIRType.TypeVar]
    )(using LoweringContext): Map[SIRType.TypeVar, LoweredValueRepresentation] = {
        (typePattern, actualRepr) match
            case (
                  SIRType.SumCaseClass(decl, typeArgs),
                  SumCaseClassRepresentation.SumBuiltinList(elemRepr)
                ) =>
                // List type: element TypeVar → element repr
                typeArgs.headOption match
                    case Some(tv: SIRType.TypeVar) if builtinTypeVars.contains(tv) =>
                        Map(tv -> elemRepr)
                    case _ => Map.empty
            case (
                  SIRType.SumCaseClass(decl, typeArgs),
                  outSum: SumCaseClassRepresentation.SumUplcConstr
                ) =>
                // UplcConstr list: extract element repr from Cons variant's head field
                val elemRepr = outSum.variants.values
                    .find(_.fieldReprs.nonEmpty)
                    .map(_.fieldReprs.head)
                typeArgs.headOption match
                    case Some(tv: SIRType.TypeVar) if builtinTypeVars.contains(tv) =>
                        elemRepr.map(r => Map(tv -> r)).getOrElse(Map.empty)
                    case _ => Map.empty
            case (
                  SIRType.SumCaseClass(decl, typeArgs),
                  SumCaseClassRepresentation.SumPairBuiltinList(keyRepr, valueRepr)
                ) =>
                // Pair list type: element TypeVar → ProdBuiltinPair with actual key/value reprs
                typeArgs.headOption match
                    case Some(tv: SIRType.TypeVar) if builtinTypeVars.contains(tv) =>
                        Map(
                          tv -> ProductCaseClassRepresentation.ProdBuiltinPair(keyRepr, valueRepr)
                        )
                    case _ => Map.empty
            case (
                  SIRType.BuiltinArray(elemType),
                  ProductCaseClassRepresentation.ProdBuiltinArray(elemRepr)
                ) =>
                elemType match
                    case tv: SIRType.TypeVar if builtinTypeVars.contains(tv) =>
                        Map(tv -> elemRepr)
                    case _ => Map.empty
            case (SIRType.Fun(inT, outT), lr: LambdaRepresentation) =>
                extractTypeVarReprs(inT, lr.canonicalRepresentationPair.inRepr, builtinTypeVars) ++
                    extractTypeVarReprs(
                      outT,
                      lr.canonicalRepresentationPair.outRepr,
                      builtinTypeVars
                    )
            case (tv: SIRType.TypeVar, repr) if builtinTypeVars.contains(tv) =>
                Map(tv -> repr)
            case _ => Map.empty
    }

    def isTypeVarCompatibleOn(
        tp: SIRType,
        repr: LoweredValueRepresentation,
        pos: SIRPosition
    )(using LoweringContext): Boolean =
        repr match {
            case TypeVarRepresentation(_) => true
            case othrLambdaRepr @ LambdaRepresentation(otherFunTp, otherCanonicalPair) =>
                val (inputType, outputType) = retrieveInputAndOutputType(tp, pos)
                val InOutRepresentationPair(inRepr, outRepr) =
                    othrLambdaRepr.reprFun(inputType, pos, otherCanonicalPair.inRepr)
                isTypeVarCompatibleOn(inputType, inRepr, pos) &&
                isTypeVarCompatibleOn(outputType, outRepr, pos)
            case _ => repr.isPackedData
        }

    override def isCompatibleWithType(tp: SIRType)(using LoweringContext): Boolean = {
        SIRUnify.topLevelUnifyType(funTp, tp, SIRUnify.Env.empty).isSuccess
    }

    override def doc: Doc = {
        PrettyPrinter.inParens(
          canonicalRepresentationPair.inRepr.doc + Doc.text(
            " -> "
          ) + canonicalRepresentationPair.outRepr.doc
        )
    }

    private def retrieveInputAndOutputType(tp: SIRType, pos: SIRPosition): (SIRType, SIRType) = {
        tp match {
            case SIRType.Fun(in, out)        => (in, out)
            case SIRType.TypeLambda(_, body) => retrieveInputAndOutputType(body, pos)
            case SIRType.TypeProxy(ref)      => retrieveInputAndOutputType(ref, pos)
            case tv: SIRType.TypeVar         => (tv, tv)
            case SIRType.FreeUnificator      => (SIRType.FreeUnificator, SIRType.FreeUnificator)
            case SIRType.TypeNothing         => (SIRType.TypeNothing, SIRType.TypeNothing)
            case _ =>
                throw LoweringException(
                  s"Can't retrieve input type from ${tp.show}, which is not a function type",
                  pos
                )
        }
    }

    override def stableKeyInternal(
        seen: Set[SumCaseClassRepresentation.SumReprProxy]
    ): String =
        s"LambdaRepresentation(${funTp.show},in=${canonicalRepresentationPair.inRepr
                .stableKeyInternal(seen)},out=${canonicalRepresentationPair.outRepr
                .stableKeyInternal(seen)})"
}

sealed trait PrimitiveRepresentation(val isPackedData: Boolean, val isDataCentric: Boolean)
    extends LoweredValueRepresentation {

    override def isCompatibleWithType(tp: SIRType)(using LoweringContext): Boolean = {
        tp match {
            case SIRType.Integer | SIRType.Data() | SIRType.ByteString | SIRType.String |
                SIRType.Boolean | SIRType.Unit | SIRType.BLS12_381_G1_Element |
                SIRType.BLS12_381_G2_Element | SIRType.BLS12_381_MlResult | SIRType.BuiltinValue =>
                true
            case _ => false
        }
    }

}

object PrimitiveRepresentation {
    case object PackedData extends PrimitiveRepresentation(true, true) with PackedDataDefaultUni {
        override def isCompatibleWithType(tp: SIRType)(using LoweringContext): Boolean =
            super.isCompatibleWithType(tp) && tp != SIRType.Unit && tp != SIRType.BLS12_381_MlResult
        override def isCompatibleOn(
            tp: SIRType,
            repr: LoweredValueRepresentation,
            pos: SIRPosition
        )(using LoweringContext): Boolean =
            repr match {
                case PackedData                          => true
                case SumCaseClassRepresentation.DataData => true
                case TypeVarRepresentation(_)            => true
                case _                                   => false
            }
        override def uplcType(semanticType: SIRType)(using LoweringContext): SIRType =
            SIRType.Data.tp
    }

    case object Constant extends PrimitiveRepresentation(false, false) {
        override def defaultUni(semanticType: SIRType)(using LoweringContext): DefaultUni =
            semanticType match
                case SIRType.Integer              => DefaultUni.Integer
                case SIRType.ByteString           => DefaultUni.ByteString
                case SIRType.String               => DefaultUni.String
                case SIRType.Boolean              => DefaultUni.Bool
                case SIRType.Unit                 => DefaultUni.Unit
                case SIRType.BLS12_381_G1_Element => DefaultUni.BLS12_381_G1_Element
                case SIRType.BLS12_381_G2_Element => DefaultUni.BLS12_381_G2_Element
                case SIRType.BLS12_381_MlResult   => DefaultUni.BLS12_381_MlResult
                case SIRType.BuiltinValue         => DefaultUni.Data // Value is always Data
                case _                            => DefaultUni.Data
    }
}

/** TypeVarRepresentation is used for type variables. Usually this is a synonym for some other
  * specific-type representation.
  *   - Transparent: builtin UPLC type variable, can be freely used in any representation
  *   - Fixed: Scala type variable with native representation
  *   - Fixed: Scala type variable that flows into list element position, uses Data representation
  *     (explicit per-element representation requires an `@UplcRepr` annotation).
  */
case class TypeVarRepresentation(kind: SIRType.TypeVarKind) extends LoweredValueRepresentation {

    import SIRType.TypeVarKind

    /** True only for Transparent ("unknown/passthrough"). Unwrapped is concrete — requires
      * conversion at boundaries like Fixed does.
      */
    inline def isBuiltin: Boolean = kind == TypeVarKind.Transparent

    // assume that TypeVarDataRepresentation is a packed data.
    //  (this is not true for lambda, will check this in code. Usually in all places we also known type)
    override def isPackedData: Boolean = kind == TypeVarKind.Fixed

    override def isDataCentric: Boolean = isPackedData

    override def isCompatibleOn(
        tp: SIRType,
        repr: LoweredValueRepresentation,
        pos: SIRPosition
    )(using lctx: LoweringContext): Boolean =
        // Match on (sourceKind, target-as-pair-or-shape). Each kind has distinct
        // semantics; cross-kind TypeVar↔TypeVar combinations are NOT compatible.
        repr match
            case TypeVarRepresentation(otherKind) =>
                (kind, otherKind) match
                    case (TypeVarKind.Transparent, TypeVarKind.Transparent) => true
                    case (TypeVarKind.Unwrapped, TypeVarKind.Unwrapped)     => true
                    case (TypeVarKind.Fixed, TypeVarKind.Fixed)             => true
                    case _                                                  => false
            case other =>
                kind match
                    case TypeVarKind.Transparent =>
                        // Wildcard source — any concrete target accepts (bytes unknown).
                        true
                    case TypeVarKind.Unwrapped =>
                        // Bytes are in concrete-default form. Compatible only with the
                        // defaultRepresentation of the resolved concrete type.
                        val concrete = lctx.resolveTypeVarIfNeeded(tp)
                        repr.isCompatibleOn(
                          concrete,
                          lctx.typeGenerator(concrete).defaultRepresentation(concrete),
                          pos
                        )
                    case TypeVarKind.Fixed =>
                        // Legacy Fixed = Data-wrapped equivalent — compatible with Data forms.
                        val concrete = lctx.resolveTypeVarIfNeeded(tp)
                        repr.isCompatibleOn(
                          concrete,
                          lctx.typeGenerator(concrete).defaultTypeVarReperesentation(concrete),
                          pos
                        )

    override def isCompatibleWithType(tp: SIRType)(using LoweringContext): Boolean = true

    override def uplcType(semanticType: SIRType)(using lctx: LoweringContext): SIRType =
        semanticType match
            case _: SIRType.TypeVar | SIRType.FreeUnificator => semanticType
            case _ =>
                val gen = lctx.typeGenerator(semanticType)
                val resolved =
                    if isBuiltin then gen.defaultRepresentation(semanticType)
                    else gen.defaultTypeVarReperesentation(semanticType)
                resolved.uplcType(semanticType)

    override def defaultUni(semanticType: SIRType)(using lctx: LoweringContext): DefaultUni =
        semanticType match
            case tv: SIRType.TypeVar =>
                lctx.tryResolveTypeVar(tv) match
                    case Some(resolved) =>
                        val gen = lctx.typeGenerator(resolved)
                        gen.defaultRepresentation(resolved).defaultUni(resolved)
                    case None =>
                        DefaultUni.Data
            case SIRType.FreeUnificator => DefaultUni.Data
            case _ =>
                val gen = lctx.typeGenerator(semanticType)
                val resolved =
                    if isBuiltin then gen.defaultRepresentation(semanticType)
                    else gen.defaultTypeVarReperesentation(semanticType)
                resolved.defaultUni(semanticType)

    override def doc: Doc = {
        val suffix = kind match
            case TypeVarKind.Transparent => "(B)"
            case TypeVarKind.Unwrapped   => "(U)"
            case TypeVarKind.Fixed       => "(R)"
        Doc.text("TypeVar") + Doc.text(suffix)
    }

}

object TypeVarRepresentation {

    /** Backward-compatible factory from isBuiltin flag */
    def apply(isBuiltin: Boolean): TypeVarRepresentation =
        TypeVarRepresentation(SIRType.TypeVarKind.fromIsBuiltin(isBuiltin))
}

case object ErrorRepresentation extends LoweredValueRepresentation {
    override def isPackedData: Boolean = false

    override def isDataCentric: Boolean = false

    override def defaultUni(semanticType: SIRType)(using LoweringContext): DefaultUni =
        DefaultUni.Data

    override def isCompatibleWithType(tp: SIRType)(using LoweringContext): Boolean = true

    // Error is unreachable at runtime — compatible with any target repr
    override def isCompatibleOn(
        tp: SIRType,
        repr: LoweredValueRepresentation,
        pos: SIRPosition
    )(using LoweringContext): Boolean = true

}
