package scalus.verify

import scalus.compiler.sir.SIRType
import scalus.compiler.sir.lowering.{LoweredValueRepresentation, LoweringContext, ProductCaseClassRepresentation, SumCaseClassRepresentation}
import scalus.compiler.sir.lowering.typegens.SirTypeUplcGenerator
import scalus.verify.DataEncoding.KeyOrder
import scalus.verify.SirTypes.*
import scalus.verify.uplcblaster.UplcBlaster

/** How the values of a type are laid out as `Data`: their [[DataEncoding]], with the names of the
  * fields and the constructors each part comes from. A path of a [[Limit]] names the parts of a
  * value by them.
  */
private[verify] enum DataLayout {

    /** A part without parts that have names: an integer, a byte string, a string, a Boolean, any
      * `Data`, or an encoding not told here.
      */
    case Plain(plain: DataEncoding)

    /** A case class: `Constr 0` of its fields, each by its name. */
    case Record(fields: List[(String, DataLayout)])

    /** A type of several constructors: `Constr` of a constructor's tag and its fields, each
      * constructor by its name, with its fields by their names.
      */
    case Choice(constructors: List[(String, List[(String, DataLayout)])])

    /** A case class encoded as its one field, `field`. */
    case Wrapper(field: String, inner: DataLayout)

    /** A list. */
    case Collection(items: DataLayout)

    /** A map, with its keys in `order`. */
    case Mapping(keys: DataLayout, values: DataLayout, order: KeyOrder)

    /** A type that contains itself, named `name`: `inner`, in which a part laid out as
      * `Plain(DataEncoding.Again(name))` is the type again, where it repeats.
      */
    case Recursive(name: String, inner: DataLayout)

    /** The encoding of the values laid out so, derived once. */
    lazy val encoding: DataEncoding = this match
        case Plain(encoding) => encoding
        case Record(fields)  => DataEncoding.Constr(List(fields.map(_._2.encoding)))
        case Choice(constructors) =>
            DataEncoding.Constr(constructors.map(_._2.map(_._2.encoding)))
        case Wrapper(_, inner) => inner.encoding
        case Collection(items) => DataEncoding.Items(items.encoding)
        case Mapping(keys, values, order) =>
            DataEncoding.Entries(keys.encoding, values.encoding, order)
        case Recursive(name, inner) => DataEncoding.Recursive(name, inner.encoding)
}

private[verify] object DataLayout {

    /** The layout of the values of `tp`, by the `Data` representation the compiler gives it with
      * the verifier's options ([[UplcBlaster.options]]).
      */
    def of(tp: SIRType): DataLayout = derivation(tp).layout

    /** The type of a part of the values of `tp` that has no `Data` form, if one has none: a
      * function, `Unit`, a value of a Miller loop, or a case class or an enum the compiler
      * represents without `Data` because it holds one of these, in a field or a type argument. Of a
      * case class or an enum the compiler says which (`canBeConvertedToData`); it is not asked of
      * another type, for which it may not answer. A value with such a part is the decoding of no
      * `Data`, so a `Data` variable does not stand for it. Its part's layout is an unknown
      * encoding.
      */
    def withoutDataForm(tp: SIRType): Option[SIRType] = derivation(tp).withoutDataForm

    /** The layout of the values of a type, and the first part of them that has no `Data` form. */
    private final case class Derivation(layout: DataLayout, withoutDataForm: Option[SIRType])

    private def derivation(tp: SIRType): Derivation = {
        def derive: Derivation = {
            given LoweringContext = UplcBlaster.representations
            val parts = new Parts
            val layout = layoutOf(tp, Set.empty, parts)
            // A layout cut off is no layout: its encoding's premise would be as large as the cut.
            if parts.count > maxParts then
                Derivation(
                  Plain(
                    DataEncoding.Unknown(
                      s"the encoding of ${tp.show} has more than $maxParts parts"
                    )
                  ),
                  parts.withoutDataForm
                )
            else Derivation(layout, parts.withoutDataForm)
        }
        derived.getOrElseUpdate(typeKey(tp), derive)
    }

    /** [[derivation]] of each type it was asked about, by its [[SirTypes.typeKey]]. */
    private val derived = scala.collection.concurrent.TrieMap.empty[String, Derivation]

    /** How many parts a layout may have: many more than a type of the ledger has, and an end to a
      * type that contains itself under other arguments each time, which `enclosing` does not tell,
      * and whose parts double at each level.
      */
    private val maxParts = 100_000

    /** The parts of a layout derived so far, the first that has no `Data` form, and the keys
      * ([[SirTypes.recursionKey]]) of the enclosing types that repeat below the part being derived.
      */
    private final class Parts(
        var count: Int = 0,
        var withoutDataForm: Option[SIRType] = None,
        val repeated: scala.collection.mutable.Set[String] = scala.collection.mutable.Set.empty
    )

    /** The name of the encoding of `tp`, of the key `key` ([[SirTypes.recursionKey]]), where it
      * contains itself: its declaration's name, as a Lean name may have it ([[leanSimpleName]]),
      * and its key as a hash, so that two types of one name are told apart, and a type has one name
      * in every check.
      */
    private def recursionName(tp: SIRType, key: String): String = {
        val hash = scalus.utils.Hex
            .bytesToHex(scalus.utils.Utils.sha2_256(key.getBytes("UTF-8")))
            .take(8)
            .toLowerCase
        s"${leanSimpleName(declarationName(tp))}_$hash"
    }

    /** The last part of the declaration name `declaration`, of the letters, digits and `_` of ASCII
      * only, which every Lean name may have; `T` where none is left.
      */
    private[verify] def leanSimpleName(declaration: String): String = {
        val simple =
            declaration.split('.').last.filter(c => c < 128 && (c.isLetterOrDigit || c == '_'))
        if simple.isEmpty then "T" else simple
    }

    /** The layout of the values of `tp`. `enclosing` are the keys of the types it is derived in
      * ([[SirTypes.recursionKey]]), and `parts` counts the parts derived so far.
      */
    private def layoutOf(tp: SIRType, enclosing: Set[String], parts: Parts)(using
        LoweringContext
    ): DataLayout = {
        parts.count += 1
        if parts.count > maxParts then
            Plain(DataEncoding.Unknown(s"the encoding has more than $maxParts parts"))
        else
            unwrap(tp) match
                case SIRType.Integer             => Plain(DataEncoding.Integer)
                case SIRType.ByteString          => Plain(DataEncoding.Bytes)
                case SIRType.String              => Plain(DataEncoding.Text)
                case SIRType.Boolean             => Plain(DataEncoding.Bool)
                case whole if isWholeData(whole) => Plain(DataEncoding.AnyData)
                case none @ (_: SIRType.Fun | _: SIRType.TypeLambda | SIRType.Unit |
                    SIRType.BLS12_381_MlResult) =>
                    withoutDataForm(none, parts)
                // A type that repeats is its encoding again, and the type where it repeats is
                // the encoding of that name: a recursive one.
                case other @ (_: SIRType.CaseClass | _: SIRType.SumCaseClass) =>
                    val key = recursionKey(other)
                    if enclosing.contains(key) then
                        parts.repeated += key
                        Plain(DataEncoding.Again(recursionName(other, key)))
                    else if enclosing.size >= maxNesting then
                        Plain(
                          DataEncoding.Unknown(s"${other.show} nests more than $maxNesting deep")
                        )
                    else
                        val inner = representedLayout(other, enclosing + key, parts)
                        // A type whose encoding is not told is that, whatever repeats in it.
                        val repeats = parts.repeated.remove(key)
                        if repeats && !inner.isInstanceOf[Plain] then
                            Recursive(recursionName(other, key), inner)
                        else inner
                case other =>
                    Plain(DataEncoding.Unknown(s"${other.show} has no Data encoding told here"))
    }

    /** The layout of a part of type `tp` that has no `Data` form, recorded as the first such part
      * where it is.
      */
    private def withoutDataForm(tp: SIRType, parts: Parts): DataLayout = {
        if parts.withoutDataForm.isEmpty then parts.withoutDataForm = Some(tp)
        Plain(DataEncoding.Unknown(s"${tp.show} has no Data form"))
    }

    /** [[layoutOf]] of a case class or a sum type, by its `Data` representation. The compiler is
      * asked whether it gives the type one before it is asked which: it gives none to a type that
      * holds a function, and fails where it is asked for it.
      */
    private def representedLayout(tp: SIRType, inner: Set[String], parts: Parts)(using
        LoweringContext
    ): DataLayout = {
        val generator = SirTypeUplcGenerator(tp)
        if !generator.canBeConvertedToData(tp) then withoutDataForm(tp, parts)
        else representedBy(generator.defaultDataRepresentation(tp), tp, inner, parts)
    }

    /** [[representedLayout]] of a type the compiler represents as `Data` by `representation`. */
    private def representedBy(
        representation: LoweredValueRepresentation,
        tp: SIRType,
        inner: Set[String],
        parts: Parts
    )(using LoweringContext): DataLayout = {
        def unknown(reason: String) = Plain(DataEncoding.Unknown(reason))
        def arguments: List[SIRType] = typeArgumentsOf(tp)
        def layout(of: SIRType): DataLayout = layoutOf(of, inner, parts)
        def mapping(keyType: SIRType, valueType: SIRType, order: KeyOrder) =
            Mapping(layout(keyType), layout(valueType), order)
        def named(constructor: scalus.compiler.sir.ConstrDecl, types: List[SIRType]) =
            constructor.params.map(_.name).zip(types.map(layout))
        representation match {
            case ProductCaseClassRepresentation.OneElementWrapper(_) =>
                unwrap(tp) match
                    case SIRType.CaseClass(constructor, typeArguments, _) =>
                        fieldTypes(constructor, typeArguments) match
                            case List(field) => Wrapper(constructor.params.head.name, layout(field))
                            case _           => unknown(s"${tp.show} does not wrap one field")
                    case _ => unknown(s"${tp.show} does not wrap one field")
            case ProductCaseClassRepresentation.PackedDataMap =>
                (declarationName(tp), arguments) match
                    case (SortedMapName, List(keyType, valueType)) =>
                        val order = unwrap(keyType) match
                            case SIRType.ByteString => KeyOrder.Bytes
                            case SIRType.Integer    => KeyOrder.Integers
                            case _                  => KeyOrder.Unchecked
                        mapping(keyType, valueType, order)
                    case (AssocMapName, List(keyType, valueType)) =>
                        mapping(keyType, valueType, KeyOrder.Any)
                    case _ => unknown(s"${tp.show} is a map of no known keys and values")
            case SumCaseClassRepresentation.SumDataAssocMap =>
                pairTypes(tp) match
                    case Some((keyType, valueType)) => mapping(keyType, valueType, KeyOrder.Any)
                    case None => unknown(s"${tp.show} is a map of no known keys and values")
            case SumCaseClassRepresentation.PackedSumDataList =>
                arguments match
                    case List(element) => Collection(layout(element))
                    case _             => unknown(s"${tp.show} is a list of no known items")
            case ProductCaseClassRepresentation.ProdDataConstr |
                SumCaseClassRepresentation.DataConstr =>
                unwrap(tp) match
                    case SIRType.CaseClass(constructor, typeArguments, None) =>
                        Record(named(constructor, fieldTypes(constructor, typeArguments)))
                    case SIRType.SumCaseClass(declaration, typeArguments) =>
                        val constructors = declaration.constructors.map(constructor =>
                            ownArguments(constructor, typeArguments).map(own =>
                                constructor.name -> named(constructor, fieldTypes(constructor, own))
                            )
                        )
                        if constructors.isEmpty || constructors.contains(None) then
                            unknown(s"the constructors of ${tp.show} are not told here")
                        else Choice(constructors.flatten)
                    case _ => unknown(s"the constructors of ${tp.show} are not told here")
            case other =>
                unknown(s"${tp.show} is represented as ${other.show}")
        }
    }

    private val SortedMapName = "scalus.cardano.onchain.plutus.prelude.SortedMap"
    private val AssocMapName = "scalus.cardano.onchain.plutus.prelude.AssocMap"

    /** The key and value types of a list of pairs: `List[(A, B)]`, `List[BuiltinPair[A, B]]` or
      * `PairList[A, B]`.
      */
    private def pairTypes(tp: SIRType): Option[(SIRType, SIRType)] =
        typeArgumentsOf(tp) match
            case List(element) =>
                typeArgumentsOf(element) match
                    case List(first, second) => Some(first -> second)
                    case _                   => None
            case List(first, second) => Some(first -> second)
            case _                   => None
}
