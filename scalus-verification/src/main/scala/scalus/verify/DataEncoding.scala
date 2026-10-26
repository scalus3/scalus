package scalus.verify

import scalus.compiler.sir.{AnnotationsDecl, SIR, SIRPosition, SIRType}
import scalus.compiler.sir.lowering.{LoweringContext, ProductCaseClassRepresentation, SumCaseClassRepresentation}
import scalus.compiler.sir.lowering.typegens.SirTypeUplcGenerator
import scalus.cardano.onchain.plutus.prelude.List as PList
import scalus.uplc.builtin.{ByteString, Data}
import scalus.verify.SirTypes.*
import scalus.verify.uplcblaster.UplcBlaster

/** How the values of a type are encoded as `Data`: which `Data` is the encoding of a value of the
  * type, as the compiler's `Data` representation of the type says. [[DataEncoding.of]] derives it
  * from a type, recursively.
  *
  * It is told in two forms that agree: a Lean predicate on `Data`, [[leanPredicate]], that holds of
  * the encodings of the type's values, and a check of a `Data` on the JVM, [[outside]]. Where the
  * encoding is [[exact]], the predicate holds of exactly those encodings. Where it is not, as for a
  * type that contains itself, it holds of more `Data` than that, and [[outside]] says which of them
  * it cannot tell.
  *
  * {{{
  * val mint = DataEncoding.of[Mint]
  * mint.outside(someMint.toData)    // None
  * mint.leanDefinition("isMint")    // def isMint (d : Data) : Bool := …
  * }}}
  */
enum DataEncoding {

    /** `I`. */
    case Integer

    /** `B`. */
    case Bytes

    /** `Constr 0 []` or `Constr 1 []`: `false` or `true`. */
    case Bool

    /** Any `Data`: a value of type `Data` is itself. */
    case AnyData

    /** `B` of a string's UTF-8. The Lean predicate does not check the UTF-8. */
    case Text

    /** `Constr`: for each tag, the encodings of that constructor's fields. */
    case Constr(constructors: List[List[DataEncoding]])

    /** `List` of items. */
    case Items(items: DataEncoding)

    /** `Map` of entries, with its keys in `order`. */
    case Entries(keys: DataEncoding, values: DataEncoding, order: DataEncoding.KeyOrder)

    /** An encoding not told here, with the reason. Its Lean predicate holds of every `Data`. */
    case Unknown(reason: String)

    /** Whether the Lean predicate holds of exactly the `Data` encoded so. */
    def exact: Boolean = this match
        case Integer | Bytes | Bool | AnyData => true
        case Text | Unknown(_)                => false
        case Constr(constructors)             => constructors.forall(_.forall(_.exact))
        case Items(items)                     => items.exact
        case Entries(keys, values, order) =>
            keys.exact && values.exact && order != DataEncoding.KeyOrder.Unchecked

    /** Whether every `Data` is encoded so, and the predicate says nothing. */
    def isAnything: Boolean = this == AnyData

    /** A small `Data` encoded so, where this tells one: the value of a variable Lean leaves out of
      * a counterexample. A constructor is the first whose fields have one, so a type that contains
      * itself ends in a constructor without itself.
      */
    private[verify] def example: Option[Data] = this match
        case Integer | AnyData => Some(Data.I(0))
        case Bytes | Text      => Some(Data.B(ByteString.empty))
        case Bool              => Some(Data.Constr(0, PList.Nil))
        case Items(_)          => Some(Data.List(PList.Nil))
        case Entries(_, _, _)  => Some(Data.Map(PList.Nil))
        case Unknown(_)        => None
        case Constr(constructors) =>
            constructors.iterator.zipWithIndex
                .flatMap { (fields, tag) =>
                    val values = fields.map(_.example)
                    Option.when(values.forall(_.isDefined))(
                      Data.Constr(tag, PList.from(values.flatten))
                    )
                }
                .nextOption()

    /** Why `data` is not encoded so, or `None` where it is. A `Data` this cannot tell, under an
      * [[Unknown]] encoding, is not encoded so.
      */
    def outside(data: Data): Option[String] = {
        def no(what: String) = Some(s"${data.toString.take(80)} is no $what")
        (this, data) match
            case (Integer, Data.I(_)) => None
            case (Integer, _)         => no("integer")
            case (Bytes, Data.B(_))   => None
            case (Bytes, _)           => no("byte string")
            case (Text, Data.B(bytes)) =>
                Option.when(!DataEncoding.isUtf8(bytes.bytes))(
                  s"${data.toString.take(80)} is no UTF-8"
                )
            case (Text, _) => no("string")
            case (Bool, Data.Constr(tag, fields)) if (tag == 0 || tag == 1) && fields.isEmpty =>
                None
            case (Bool, _)            => no("Boolean")
            case (AnyData, _)         => None
            case (Unknown(reason), _) => Some(s"$reason, so its values are not checked here")
            case (Constr(constructors), Data.Constr(tag, fields))
                if tag >= 0 && tag < constructors.size =>
                val types = constructors(tag.toInt)
                val values = fields.toScalaList
                if types.size != values.size then
                    Some(s"constructor $tag has ${types.size} fields, not ${values.size}")
                else values.zip(types).iterator.flatMap((v, t) => t.outside(v)).nextOption()
            case (Constr(_), _) => no("constructor of its type")
            case (Items(items), Data.List(values)) =>
                values.toScalaList.iterator.flatMap(items.outside).nextOption()
            case (Items(_), _) => no("list")
            case (Entries(keys, values, order), Data.Map(entries)) =>
                DataEncoding.entriesOutside(keys, values, order, entries.toScalaList)
            case (Entries(_, _, _), _) => no("map")
    }

    /** The Lean predicate, a `Data → Bool`, that holds of the `Data` encoded so. It names
      * PlutusCore's `Data` as `Data`, as after `open PlutusCore.Data (Data)`, and uses
      * [[DataEncoding.leanDefinitions]].
      */
    def leanPredicate: String = leanPredicate(Iterator.from(0))

    /** A Lean definition of [[leanPredicate]], `def name (d : Data) : Bool := …`, on one line. */
    def leanDefinition(name: String): String = s"def $name (d : Data) : Bool := $leanPredicate d"

    /** [[leanPredicate]], where `fresh` numbers the names it binds. */
    private[verify] def leanPredicate(fresh: Iterator[Int]): String = this match
        case Integer              => "scalusIsI"
        case Bytes | Text         => "scalusIsB"
        case Bool                 => "scalusIsBool"
        case AnyData | Unknown(_) => "(fun _ => true)"
        case Items(items) =>
            val d = s"d${fresh.next()}"
            s"(fun $d => match $d with | Data.List xs => scalusAll ${items.leanPredicate(fresh)} xs | _ => false)"
        case Entries(keys, values, order) =>
            val d = s"d${fresh.next()}"
            s"(fun $d => match $d with | Data.Map ps => ${DataEncoding.leanEntries(keys, values, order, fresh)} ps | _ => false)"
        case Constr(constructors) =>
            val d = s"d${fresh.next()}"
            val tag = s"t${fresh.next()}"
            val fields = s"fs${fresh.next()}"
            val branches = constructors.zipWithIndex.map { (types, index) =>
                val names = types.indices.map(i => s"${fields}_$i")
                val test =
                    if types.isEmpty then s"$fields.isEmpty"
                    else
                        val all = types
                            .zip(names)
                            .map((field, name) => s"${field.leanPredicate(fresh)} $name")
                            .mkString(" && ")
                        s"(match $fields with | [${names.mkString(", ")}] => $all | _ => false)"
                index -> test
            }
            val chain = branches.foldRight("false") { case ((index, test), otherwise) =>
                s"(if $tag == $index then $test else $otherwise)"
            }
            s"(fun $d => match $d with | Data.Constr $tag $fields => $chain | _ => false)"
}

object DataEncoding {

    /** The order of a map's keys: any; strictly ascending byte strings or integers, as a
      * `SortedMap`'s; or an order of keys of another type, which is not checked.
      */
    enum KeyOrder {
        case Any, Bytes, Integers, Unchecked
    }

    /** The encoding of the values of `A`. */
    inline def of[A]: DataEncoding = ${ PropMacro.dataEncoding[A] }

    /** The encoding of the values of `tp`, by the `Data` representation the compiler gives it with
      * the verifier's options ([[UplcBlaster.options]]).
      */
    def of(tp: SIRType): DataEncoding = {
        def derive: DataEncoding = {
            given LoweringContext = UplcBlaster.representations
            val parts = new Parts
            val encoding = encodingOf(tp, Set.empty, parts)
            // An encoding cut off is no encoding: its premise would be as large as the cut.
            if parts.count > maxParts then
                Unknown(s"the encoding of ${tp.show} has more than $maxParts parts")
            else encoding
        }
        derived.getOrElseUpdate(typeKey(tp), derive)
    }

    /** [[of]] of each type it was asked about, by its [[SirTypes.typeKey]]. */
    private val derived = scala.collection.concurrent.TrieMap.empty[String, DataEncoding]

    /** How many parts an encoding may have: many more than a type of the ledger has, and an end to
      * a type that contains itself under other arguments each time, which `enclosing` does not
      * tell, and whose parts double at each level.
      */
    private val maxParts = 100_000

    /** The parts of an encoding derived so far. */
    private final class Parts(var count: Int = 0)

    /** The key of the annotation that marks the type of a statement variable which ranges over the
      * `Data` encodings of its type's values ([[Props.forAllData]]). The annotation holds the
      * variable's decoder, the compiled `(d: Data) => d.to[A]`.
      */
    private val EncodedKey = "scalus.verify.forAllData"

    /** `variable`, ranging over the `Data` encodings of its type's values, which `decoder` reads.
      */
    private[verify] def encodedVariable[A](
        variable: PropExpr.Ident[A],
        decoder: SIR
    ): PropExpr.Ident[A] = {
        val annotations = AnnotationsDecl(SIRPosition.empty, data = Map(EncodedKey -> decoder))
        new PropExpr.Ident[A](
          variable.name,
          variable.id,
          SIRType.Annotated(variable.tp, annotations)
        )
    }

    /** The type of the values and the decoder of a statement variable of type `tp` that ranges over
      * their `Data` encodings, or none for another variable.
      */
    private[verify] def encoded(tp: SIRType): Option[(SIRType, SIR)] = tp match
        case SIRType.Annotated(inner, annotations) =>
            annotations.data.get(EncodedKey).map(inner -> _)
        case _ => None

    /** The type of the values of a statement variable of type `tp`: `tp`, without the mark of a
      * variable over their `Data` encodings ([[encoded]]). Code reads the variable's values with
      * it.
      */
    private[verify] def valueType(tp: SIRType): SIRType = encoded(tp).fold(tp)(_._1)

    /** The Lean definitions that [[DataEncoding.leanPredicate]] uses, each on one line. They name
      * PlutusCore's `Data` as `Data`.
      */
    val leanDefinitions: String = List(
      "def scalusIsI : Data → Bool | Data.I _ => true | _ => false",
      "def scalusIsB : Data → Bool | Data.B _ => true | _ => false",
      "def scalusIsBool : Data → Bool | Data.Constr t [] => t == 0 || t == 1 | _ => false",
      "def scalusAll (p : Data → Bool) : List Data → Bool | [] => true | x :: rest => " +
          "p x && scalusAll p rest",
      "def scalusAllPairs (p q : Data → Bool) : List (Data × Data) → Bool | [] => true " +
          "| (a, b) :: rest => p a && q b && scalusAllPairs p q rest",
      "def scalusAscendingBytes : List (Data × Data) → Bool | [] => true | [(Data.B _, _)] => true " +
          "| (Data.B a, _) :: tail@((Data.B b, _) :: _) => decide (a < b) && " +
          "scalusAscendingBytes tail | _ => false",
      "def scalusAscendingIntegers : List (Data × Data) → Bool | [] => true " +
          "| [(Data.I _, _)] => true | (Data.I a, _) :: tail@((Data.I b, _) :: _) => " +
          "decide (a < b) && scalusAscendingIntegers tail | _ => false"
    ).mkString("\n\n")

    /** The Lean predicate on the entries of a map, a `List (Data × Data)`. */
    private[verify] def leanEntries(
        keys: DataEncoding,
        values: DataEncoding,
        order: KeyOrder,
        fresh: Iterator[Int]
    ): String = {
        val e = s"e${fresh.next()}"
        val ascending = order match
            case KeyOrder.Bytes                    => s" && scalusAscendingBytes $e"
            case KeyOrder.Integers                 => s" && scalusAscendingIntegers $e"
            case KeyOrder.Any | KeyOrder.Unchecked => ""
        s"(fun $e => scalusAllPairs ${keys.leanPredicate(fresh)} ${values.leanPredicate(fresh)} $e$ascending)"
    }

    /** Why `entries` are not entries of a map encoded so, or `None` where they are. */
    private[verify] def entriesOutside(
        keys: DataEncoding,
        values: DataEncoding,
        order: KeyOrder,
        entries: List[(Data, Data)]
    ): Option[String] = {
        val entry = entries.iterator
            .flatMap((key, value) => keys.outside(key).orElse(values.outside(value)))
            .nextOption()
        def ascending(less: (Data, Data) => Option[Boolean]) =
            entries.map(_._1).sliding(2).collectFirst {
                case List(a, b) if !less(a, b).contains(true) =>
                    "the keys are not in ascending order"
            }
        entry.orElse(order match
            case KeyOrder.Any => None
            case KeyOrder.Unchecked =>
                Option.when(entries.size > 1)("the order of the keys is not checked here")
            case KeyOrder.Bytes =>
                ascending {
                    case (Data.B(a), Data.B(b)) =>
                        Some(java.util.Arrays.compareUnsigned(a.bytes, b.bytes) < 0)
                    case _ => None
                }
            case KeyOrder.Integers =>
                ascending {
                    case (Data.I(a), Data.I(b)) => Some(a < b)
                    case _                      => None
                })
    }

    /** The encoding of the values of `tp` as `Data`. `enclosing` are the types it is derived in,
      * and `parts` counts the parts derived so far.
      */
    private def encodingOf(tp: SIRType, enclosing: Set[String], parts: Parts)(using
        LoweringContext
    ): DataEncoding = {
        parts.count += 1
        if parts.count > maxParts then Unknown(s"the encoding has more than $maxParts parts")
        else
            unwrap(tp) match
                case SIRType.Integer             => Integer
                case SIRType.ByteString          => Bytes
                case SIRType.String              => Text
                case SIRType.Boolean             => Bool
                case whole if isWholeData(whole) => AnyData
                case other @ (_: SIRType.CaseClass | _: SIRType.SumCaseClass) =>
                    if enclosing.contains(other.show) then Unknown(s"${other.show} contains itself")
                    else if enclosing.size >= maxNesting then
                        Unknown(s"${other.show} nests more than $maxNesting deep")
                    else representedEncoding(other, enclosing + other.show, parts)
                case other => Unknown(s"${other.show} has no Data encoding told here")
    }

    /** [[encodingOf]] of a case class or a sum type, by its `Data` representation. */
    private def representedEncoding(tp: SIRType, inner: Set[String], parts: Parts)(using
        LoweringContext
    ): DataEncoding = {
        def arguments: List[SIRType] = typeArgumentsOf(tp)
        def encoding(of: SIRType): DataEncoding = encodingOf(of, inner, parts)
        def entries(keyType: SIRType, valueType: SIRType, order: KeyOrder) =
            Entries(encoding(keyType), encoding(valueType), order)
        SirTypeUplcGenerator.defaultDataRepresentation(tp) match
            case ProductCaseClassRepresentation.OneElementWrapper(_) =>
                fieldsOf(tp) match
                    case Some(List(field)) => encoding(field)
                    case _                 => Unknown(s"${tp.show} does not wrap one field")
            case ProductCaseClassRepresentation.PackedDataMap =>
                (declarationName(tp), arguments) match
                    case (SortedMapName, List(keyType, valueType)) =>
                        val order = unwrap(keyType) match
                            case SIRType.ByteString => KeyOrder.Bytes
                            case SIRType.Integer    => KeyOrder.Integers
                            case _                  => KeyOrder.Unchecked
                        entries(keyType, valueType, order)
                    case (AssocMapName, List(keyType, valueType)) =>
                        entries(keyType, valueType, KeyOrder.Any)
                    case _ => Unknown(s"${tp.show} is a map of no known keys and values")
            case SumCaseClassRepresentation.SumDataAssocMap =>
                pairTypes(tp) match
                    case Some((keyType, valueType)) => entries(keyType, valueType, KeyOrder.Any)
                    case None => Unknown(s"${tp.show} is a map of no known keys and values")
            case SumCaseClassRepresentation.PackedSumDataList =>
                arguments match
                    case List(element) => Items(encoding(element))
                    case _             => Unknown(s"${tp.show} is a list of no known items")
            case ProductCaseClassRepresentation.ProdDataConstr |
                SumCaseClassRepresentation.DataConstr =>
                val constructors: List[Option[List[SIRType]]] = unwrap(tp) match
                    case SIRType.CaseClass(constructor, typeArguments, None) =>
                        List(Some(fieldTypes(constructor, typeArguments)))
                    case SIRType.SumCaseClass(declaration, typeArguments) =>
                        declaration.constructors.map(constructor =>
                            ownArguments(constructor, typeArguments).map(fieldTypes(constructor, _))
                        )
                    case _ => Nil
                if constructors.isEmpty || constructors.contains(None) then
                    Unknown(s"the constructors of ${tp.show} are not told here")
                else Constr(constructors.flatten.map(_.map(encoding)))
            case representation =>
                Unknown(s"${tp.show} is represented as ${representation.show}")
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

    /** The types of the fields of a case class, or none for a type that is not one. */
    private def fieldsOf(tp: SIRType): Option[List[SIRType]] = unwrap(tp) match
        case SIRType.CaseClass(constructor, arguments, _) =>
            Some(fieldTypes(constructor, arguments))
        case _ => None

    /** Whether `bytes` are UTF-8. */
    private def isUtf8(bytes: Array[Byte]): Boolean = {
        val decoder = java.nio.charset.StandardCharsets.UTF_8
            .newDecoder()
            .onMalformedInput(java.nio.charset.CodingErrorAction.REPORT)
            .onUnmappableCharacter(java.nio.charset.CodingErrorAction.REPORT)
        // A decoder tells malformed input only by throwing.
        try { decoder.decode(java.nio.ByteBuffer.wrap(bytes)); true }
        catch case _: java.nio.charset.CharacterCodingException => false
    }
}
