package scalus.verify

import scalus.compiler.sir.{AnnotationsDecl, SIR, SIRPosition, SIRType}
import scalus.uplc.Constant
import scalus.cardano.onchain.plutus.prelude.List as PList
import scalus.uplc.builtin.{ByteString, Data}

/** How the values of a type are encoded as `Data`: which `Data` is the encoding of a value of the
  * type, as the compiler's `Data` representation of the type says. [[DataEncoding.of]] derives it
  * from a type, recursively.
  *
  * It is told in two forms that agree: a Lean predicate on `Data`, [[leanPredicate]], that holds of
  * the encodings of the type's values, and a check of a `Data` on the JVM, [[outside]]. Where the
  * encoding is [[exact]], the predicate holds of exactly those encodings. Where it is not, as for a
  * string, whose UTF-8 the predicate does not check, it holds of more `Data` than that, and
  * [[outside]] says which of them it cannot tell. A type that contains itself is a `Recursive`
  * encoding, whose predicate is a recursive Lean definition, exact where its other parts are.
  *
  * {{{
  * val mint = DataEncoding.of[Mint]
  * mint.outside(someMint.toData)    // None
  * // A Lean file of the predicate: what it uses, and then the definition.
  * DataEncoding.leanDefinitions     // def scalusIsI … and the other few
  * mint.leanRecursions              // the walks of its lists and maps
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

    /** The encoding of a type that contains itself, named `name`: `body`, in which [[Again]] of the
      * name is the type again, where it repeats. Its Lean predicate is a recursive definition
      * ([[leanRecursions]]), so it is exact where the rest of `body` is.
      */
    case Recursive(name: String, body: DataEncoding)

    /** The type of the enclosing [[Recursive]] encoding named `name`, again. */
    case Again(name: String)

    /** Whether the Lean predicate holds of exactly the `Data` encoded so. */
    def exact: Boolean = this match
        case Integer | Bytes | Bool | AnyData => true
        case Text | Unknown(_)                => false
        case Recursive(_, body)               => body.exact
        case Again(_)                         => true
        case Constr(constructors)             => constructors.forall(_.forall(_.exact))
        case Items(items)                     => items.exact
        case Entries(keys, values, order) =>
            keys.exact && values.exact && order != DataEncoding.KeyOrder.Unchecked

    /** Whether every `Data` is encoded so, and the predicate says nothing. */
    def isAnything: Boolean = this == AnyData

    /** Whether the Lean predicate holds of every `Data`: of [[AnyData]], and of an [[Unknown]]
      * encoding, which it does not check.
      */
    private[verify] def holdsOfEvery: Boolean = this match
        case AnyData | Unknown(_) => true
        case _                    => false

    /** Why the encoding is not told, where it is not as a whole: an [[Unknown]]. The body of a
      * [[Recursive]] one never is, as it has the type again.
      */
    private[verify] def unknownReason: Option[String] = this match
        case Unknown(reason) => Some(reason)
        case _               => None

    /** A small `Data` encoded so, where this tells one: the value of a variable Lean leaves out of
      * a counterexample. A constructor is the first whose fields have one, so a type that contains
      * itself ends in a constructor without itself.
      */
    private[verify] def example: Option[Data] = exampleIn(Map.empty)

    /** [[example]], where the [[Recursive]] encodings of the names in `bound` enclose this one: a
      * repeat of one of them is its example. Within its own encoding a type again has none, so a
      * constructor without it ends the value.
      */
    private[verify] def exampleIn(bound: Map[String, DataEncoding]): Option[Data] = this match
        case Integer | AnyData => Some(Data.I(0))
        case Bytes | Text      => Some(Data.B(ByteString.empty))
        case Bool              => Some(Data.Constr(0, PList.Nil))
        case Items(_)          => Some(Data.List(PList.Nil))
        case Entries(_, _, _)  => Some(Data.Map(PList.Nil))
        case Unknown(_)        => None
        // A constructor without the type again ends the value.
        case Recursive(name, body) => body.exampleIn(bound - name)
        case Again(name)           => bound.get(name).flatMap(_.exampleIn(bound - name))
        case Constr(constructors) =>
            constructors.iterator.zipWithIndex
                .flatMap { (fields, tag) =>
                    val values = fields.map(_.exampleIn(bound))
                    Option.when(values.forall(_.isDefined))(
                      Data.Constr(tag, PList.from(values.flatten))
                    )
                }
                .nextOption()

    /** Why `data` is not encoded so, or `None` where it is. A `Data` this cannot tell, under an
      * `Unknown` encoding, is not encoded so.
      *
      * The parts are checked in a loop, not by recursion: a type that contains itself is as deep as
      * its `Data`, which may be deeper than a thread's stack.
      */
    def outside(data: Data): Option[String] = {
        val pending = DataEncoding.Pending()
        pending.later(this, data, Map.empty)
        pending.run()
    }

    /** Why `data`, but for its parts, is not encoded so, where the [[Recursive]] encodings of the
      * names in `bound` enclose this one. The checks of its parts go to `pending`, to be made in
      * their order.
      */
    private def outsideIn(
        data: Data,
        bound: Map[String, DataEncoding],
        pending: DataEncoding.Pending
    ): Option[String] = {
        def no(what: String) = Some(s"${data.toString.take(80)} is no $what")
        (this, data) match
            case (recursive @ Recursive(name, body), _) =>
                body.outsideIn(data, bound + (name -> recursive), pending)
            case (Again(name), _) =>
                bound.get(name) match
                    case Some(recursive) => recursive.outsideIn(data, bound, pending)
                    case None            => Some(s"the type $name is again outside its encoding")
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
                else
                    pending.later(types.zip(values), bound)
                    None
            case (Constr(_), _) => no("constructor of its type")
            case (Items(items), Data.List(values)) =>
                pending.later(values.toScalaList.map(items -> _), bound)
                None
            case (Items(_), _) => no("list")
            case (Entries(keys, values, order), Data.Map(entries)) =>
                val all = entries.toScalaList
                // The order of the keys is told after the entries.
                pending.later(() => DataEncoding.orderOutside(order, all.map(_._1)))
                pending.later(
                  all.flatMap((key, value) => List(keys -> key, values -> value)),
                  bound
                )
                None
            case (Entries(_, _, _), _) => no("map")
    }

    /** The Lean predicate, a `Data → Bool`, that holds of the `Data` encoded so. It names
      * PlutusCore's `Data` as `Data`, as after `open PlutusCore.Data (Data)`, and uses
      * [[DataEncoding.leanDefinitions]] and the recursive definitions of [[leanRecursions]].
      *
      * It takes no function as an argument: a list or a map is checked by a walk of its own, a
      * recursive definition of [[leanRecursions]]. Blaster gives the solver a function argument as
      * a lambda with quantified axioms, on which Z3 did not finish a check of a script context in
      * 15 minutes; the same check with a walk of each list answered in under a second.
      */
    def leanPredicate: String = renderedPredicate

    /** [[leanPredicate]], rendered once: a walk's name is a digest of its items' predicate, and an
      * encoding inside another is asked for it at every level.
      */
    private lazy val renderedPredicate: String = leanPredicate(Iterator.from(0))

    /** A Lean definition of [[leanPredicate]], `def name (d : Data) : Bool := …`, on one line. It
      * comes after [[DataEncoding.leanDefinitions]] and the recursive definitions it uses: those of
      * [[DataEncoding.leanRecursions]] of all the encodings of a file, which define each once.
      */
    def leanDefinition(name: String): String = s"def $name (d : Data) : Bool := $leanPredicate d"

    /** The recursive Lean definitions that [[leanPredicate]] uses, each after those it uses: a walk
      * of each kind of list or map in this encoding, and `mutual` blocks of the types that contain
      * themselves. None for an encoding without a list, a map or such a type. A file of several
      * encodings has [[DataEncoding.leanRecursions]] of them all, which writes each once.
      */
    def leanRecursions: List[String] = DataEncoding.leanRecursions(List(this))

    /** [[leanPredicate]], where `fresh` numbers the names it binds. */
    private[verify] def leanPredicate(fresh: Iterator[Int]): String = this match
        case Integer              => "scalusIsI"
        case Bytes | Text         => "scalusIsB"
        case Bool                 => "scalusIsBool"
        case AnyData | Unknown(_) => "(fun _ => true)"
        // A type that contains itself is its recursive definition, of which this is the name.
        case Recursive(name, _) => DataEncoding.leanName(name)
        case Again(name)        => DataEncoding.leanName(name)
        case Items(items) =>
            val d = s"d${fresh.next()}"
            s"(fun $d => match $d with | Data.List xs => ${DataEncoding.leanItems(items)} xs | _ => false)"
        case Entries(keys, values, order) =>
            val d = s"d${fresh.next()}"
            s"(fun $d => match $d with | Data.Map ps => ${DataEncoding.leanEntries(keys, values, order, fresh)} ps | _ => false)"
        case Constr(constructors) =>
            val d = s"d${fresh.next()}"
            val tag = s"t${fresh.next()}"
            val fields = s"fs${fresh.next()}"
            val chain = DataEncoding.constructorTests(tag, fields, constructors) { (field, name) =>
                s"${field.leanPredicate(fresh)} $name"
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
      * the verifier's options ([[scalus.verify.uplcblaster.UplcBlaster.options]]).
      */
    def of(tp: SIRType): DataEncoding = DataLayout.of(tp).encoding

    /** The key of the annotation that marks the type of a statement variable which ranges over the
      * `Data` encodings of its type's values ([[Props.forAllData]]). The annotation holds the
      * variable's decoder, the compiled `(d: Data) => d.to[A]`.
      */
    private val EncodedKey = "scalus.verify.forAllData"

    /** The key of the annotation that holds the limits of a statement variable over the `Data`
      * encodings of its type's values ([[Props.forAllDataLimited]]), as `Data` ([[Limits.toData]]).
      * A variable without limits has none.
      */
    private val LimitsKey = "scalus.verify.limits"

    /** `variable`, ranging over the `Data` encodings of its type's values, which `decoder` reads,
      * that meet `limits`.
      */
    private[verify] def encodedVariable[A](
        variable: PropExpr.Ident[A],
        decoder: SIR,
        limits: Limits = Limits.none
    ): PropExpr.Ident[A] = {
        val limited = Option.when(limits.all.nonEmpty)(
          LimitsKey -> SIR.Const(
            Constant.Data(limits.toData),
            SIRType.Data(),
            AnnotationsDecl.empty
          )
        )
        val annotations =
            AnnotationsDecl(SIRPosition.empty, data = Map(EncodedKey -> decoder) ++ limited)
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

    /** The limits of a statement variable of type `tp` over the `Data` encodings of its values
      * ([[Props.forAllDataLimited]]): none for a variable without them, or of another type.
      */
    private[verify] def limits(tp: SIRType): Limits = tp match
        case SIRType.Annotated(_, annotations) =>
            annotations.data.get(LimitsKey) match
                case Some(SIR.Const(Constant.Data(data), _, _)) => Limits.fromData(data)
                case _                                          => Limits.none
        case _ => Limits.none

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
        s"(fun $e => ${leanPairs(keys, values)} $e${ascendingKeys(order, e)})"
    }

    /** The Lean predicate on a list of `items`, a `List Data → Bool`: the name of its walk, which
      * [[leanRecursions]] defines, or a constant where the items' predicate holds of every `Data`.
      * The name is that of the walk's text, so that two encodings of one file share it.
      */
    private[verify] def leanItems(items: DataEncoding): String = {
        if items.holdsOfEvery then "(fun _ => true)"
        else s"scalusAll_${digest(items.leanPredicate)}"
    }

    /** The Lean predicate on the entries of a map of `keys` and `values`, a
      * `List (Data × Data) → Bool`, as [[leanItems]] is of a list: without the order of its keys.
      */
    private[verify] def leanPairs(keys: DataEncoding, values: DataEncoding): String = {
        if keys.holdsOfEvery && values.holdsOfEvery then "(fun _ => true)"
        else s"scalusAllPairs_${digest(s"${keys.leanPredicate} ${values.leanPredicate}")}"
    }

    /** The walk named [[leanItems]] of a list of `items`, whose predicate does not hold of every
      * `Data`.
      */
    private def itemsWalk(items: DataEncoding): String = {
        val walk = leanItems(items)
        s"def $walk : List Data → Bool | [] => true | x :: rest => ${items.leanPredicate} x && " +
            s"$walk rest"
    }

    /** The walk named [[leanPairs]] of the entries of a map of `keys` and `values`, not both of
      * whose predicates hold of every `Data`.
      */
    private def pairsWalk(keys: DataEncoding, values: DataEncoding): String = {
        val walk = leanPairs(keys, values)
        s"def $walk : List (Data × Data) → Bool | [] => true | (k, v) :: rest => " +
            s"${keys.leanPredicate} k && ${values.leanPredicate} v && $walk rest"
    }

    /** Sixteen hexadecimal digits, 64 bits, of the SHA-256 of `text`: the part of a Lean name that
      * tells it apart, of a walk by what it checks and of a type that contains itself by its key
      * ([[DataLayout]]). [[leanRecursions]] fails on two walks of one name.
      */
    private[verify] def digest(text: String): String = {
        scalus.utils.Hex
            .bytesToHex(scalus.utils.Utils.sha2_256(text.getBytes("UTF-8")))
            .take(16)
            .toLowerCase
    }

    /** Why `entries` are not entries of a map encoded so, or `None` where they are. */
    private[verify] def entriesOutside(
        keys: DataEncoding,
        values: DataEncoding,
        order: KeyOrder,
        entries: List[(Data, Data)]
    ): Option[String] = {
        entries.iterator
            .flatMap((key, value) => keys.outside(key).orElse(values.outside(value)))
            .nextOption()
            .orElse(orderOutside(order, entries.map(_._1)))
    }

    /** Why `keys`, the keys of a map in their order, are not in `order`, or `None` where they are.
      */
    private def orderOutside(order: KeyOrder, keys: List[Data]): Option[String] = {
        def ascending(less: (Data, Data) => Option[Boolean]) =
            keys.sliding(2).collectFirst {
                case List(a, b) if !less(a, b).contains(true) =>
                    "the keys are not in ascending order"
            }
        order match
            case KeyOrder.Any => None
            case KeyOrder.Unchecked =>
                Option.when(keys.size > 1)("the order of the keys is not checked here")
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
                }
    }

    /** The checks of [[DataEncoding.outside]] still to make, the next one last, which [[run]] makes
      * in a loop: a check of a part leaves those of its own parts here.
      */
    private final class Pending {
        private val checks = scala.collection.mutable.ArrayBuffer.empty[() => Option[String]]

        /** A check, to be made after those of the parts left before it. */
        def later(check: () => Option[String]): Unit = checks += check

        /** The checks of `parts`, each `Data` by its encoding, to be made in their order. */
        def later(parts: List[(DataEncoding, Data)], bound: Map[String, DataEncoding]): Unit = {
            parts.reverseIterator.foreach((encoding, part) =>
                checks += (() => encoding.outsideIn(part, bound, this))
            )
        }

        /** The check of `data` by `encoding`. */
        def later(encoding: DataEncoding, data: Data, bound: Map[String, DataEncoding]): Unit =
            later(List(encoding -> data), bound)

        /** The first reason a check gives, making them until one does or none is left. */
        def run(): Option[String] = {
            var reason: Option[String] = None
            while reason.isEmpty && checks.nonEmpty do reason = checks.remove(checks.length - 1)()
            reason
        }
    }

    /** The Lean name of the recursive definition of the encoding of a type that contains itself,
      * named `name` ([[Recursive]]).
      */
    private[verify] def leanName(name: String): String = s"scalusRec_$name"

    /** The recursive Lean definitions that the predicates of `encodings` use, each once and after
      * those it uses: the walks of their lists and maps ([[leanItems]], [[leanPairs]]), and the
      * `mutual` blocks of their types that contain themselves ([[Recursive]]). A Lean file of
      * several definitions ([[DataEncoding.leanDefinition]]) has them once, after
      * [[leanDefinitions]].
      */
    def leanRecursions(encodings: Iterable[DataEncoding]): List[String] = {
        val recursions = new LeanRecursions
        encodings.foreach(recursions.reach)
        recursions.blocks.values.toList
    }

    /** The tests of a constructor of `constructors`, whose tag and list of fields a `match` named
      * `tag` and `fields`: for each, its fields, each by the test `field` gives of its encoding and
      * its name, and `false` for a tag that none has.
      */
    private def constructorTests(
        tag: String,
        fields: String,
        constructors: List[List[DataEncoding]]
    )(field: (DataEncoding, String) => String): String = {
        val branches = constructors.zipWithIndex.map { (types, index) =>
            val names = types.indices.map(i => s"${fields}_$i")
            val test =
                if types.isEmpty then s"$fields.isEmpty"
                else
                    val all = types.zip(names).map(field.tupled).mkString(" && ")
                    s"(match $fields with | [${names.mkString(", ")}] => $all | _ => false)"
            index -> test
        }
        branches.foldRight("false") { case ((index, test), otherwise) =>
            s"(if $tag == $index then $test else $otherwise)"
        }
    }

    /** The test that the keys of the entries `entries` are in `order`, after `&&`, or nothing for
      * an order that is not checked.
      */
    private def ascendingKeys(order: KeyOrder, entries: String): String = order match
        case KeyOrder.Bytes                    => s" && scalusAscendingBytes $entries"
        case KeyOrder.Integers                 => s" && scalusAscendingIntegers $entries"
        case KeyOrder.Any | KeyOrder.Unchecked => ""

    /** The recursive definitions: the walks of lists and maps, by their names, and the `mutual`
      * blocks, by the name of the type each is for, each after those it uses.
      *
      * A walk is of the lists or the maps of one kind of element, whose elements repeat no type
      * around them: `scalusAll_…` of a list, `scalusAllPairs_…` of a map's entries.
      *
      * A type's block defines its predicate, those of the types inside it that repeat it, which
      * call each other, and a walk of each list or map whose elements repeat one of them. Each is
      * written with `match` on a part it took apart, as `match fs with | [a, b] => …`, so that Lean
      * sees every call is of a part of its argument: the recursion is structural, and the
      * definition total. A type inside that repeats no enclosing type has a block of its own,
      * before this one; and a type is defined once, in whichever block meets it first.
      */
    private final class LeanRecursions {
        val blocks = scala.collection.mutable.LinkedHashMap.empty[String, String]

        /** The names of the types whose predicates are defined, in any block. */
        private val defined = scala.collection.mutable.Set.empty[String]

        /** [[free]] of each encoding asked about, by identity: an encoding is asked about once. */
        private val freeNames = new java.util.IdentityHashMap[DataEncoding, Set[String]]()

        /** The encodings reached, by identity: one of many variables of a type, or a part of many
          * types, is reached once.
          */
        private val reached = java.util.Collections.newSetFromMap(
          new java.util.IdentityHashMap[DataEncoding, java.lang.Boolean]()
        )

        /** What each walk written checks, by the walk's name. */
        private val walks = scala.collection.mutable.Map.empty[String, String]

        /** Every definition that the predicate of `encoding` uses, written. */
        def reach(encoding: DataEncoding): Unit = {
            if reached.add(encoding) then
                encoding match
                    case recursive: Recursive     => root(recursive)
                    case Constr(constructors)     => constructors.foreach(_.foreach(reach))
                    case Items(items)             => walkItems(items)
                    case Entries(keys, values, _) => walkPairs(keys, values)
                    case _                        => ()
        }

        /** The name of the walk of a list of `items`, defined after what it uses. */
        def walkItems(items: DataEncoding): String = {
            val name = leanItems(items)
            if !items.holdsOfEvery && unwritten(name, items.leanPredicate) then
                reach(items)
                blocks(s"walk $name") = itemsWalk(items)
            name
        }

        /** The name of the walk of a map's entries of `keys` and `values`, defined after what it
          * uses.
          */
        def walkPairs(keys: DataEncoding, values: DataEncoding): String = {
            val name = leanPairs(keys, values)
            val checks = s"${keys.leanPredicate} ${values.leanPredicate}"
            if !(keys.holdsOfEvery && values.holdsOfEvery) && unwritten(name, checks) then
                reach(keys)
                reach(values)
                blocks(s"walk $name") = pairsWalk(keys, values)
            name
        }

        /** Whether no walk named `name` is written yet, which then is to be, of what `checks`. The
          * name is a digest of what a walk checks, and two walks of one name would be one.
          */
        private def unwritten(name: String, checks: String): Boolean = {
            walks.get(name) match
                case None =>
                    walks(name) = checks
                    true
                case Some(written) if written == checks => false
                case Some(written) =>
                    throw new IllegalStateException(
                      s"two walks of one name, $name: of $written and of $checks"
                    )
        }

        /** The names of the types of [[Again]] in `encoding` that it does not enclose. */
        private def free(encoding: DataEncoding): Set[String] = {
            val known = freeNames.get(encoding)
            if known != null then known
            else
                val names = encoding match
                    case Again(name)              => Set(name)
                    case Recursive(name, body)    => free(body) - name
                    case Constr(constructors)     => constructors.flatten.flatMap(free).toSet
                    case Items(items)             => free(items)
                    case Entries(keys, values, _) => free(keys) ++ free(values)
                    case _                        => Set.empty[String]
                freeNames.put(encoding, names)
                names
        }

        /** The block of `recursive`, a type that repeats no enclosing type, after those it uses. */
        private def root(recursive: Recursive): Unit = {
            if !defined.contains(recursive.name) then
                val block = new Block(recursive.name)
                block.define(recursive)
                blocks(recursive.name) = block.definitions.mkString("mutual\n", "\n", "\nend")
        }

        /** The definitions of the block of the type `rootName`. */
        private final class Block(rootName: String) {
            val definitions = scala.collection.mutable.ArrayBuffer.empty[String]
            private val fresh = Iterator.from(0)
            private def next(prefix: String) = s"$prefix${fresh.next()}"

            /** The predicate of `recursive`, in this block, where no block has it yet. */
            def define(recursive: Recursive): Unit = {
                if defined.add(recursive.name) then
                    val d = next("d")
                    val body = on(d, recursive.body)
                    definitions += s"def ${leanName(recursive.name)} ($d : Data) : Bool := $body"
            }

            /** That `x`, a part taken apart, is encoded so: a Boolean of Lean. A part of no type of
              * this block is tested as the predicates of other types are, after the blocks of the
              * types in it.
              */
            private def on(x: String, encoding: DataEncoding): String = {
                encoding match {
                    case Integer              => s"scalusIsI $x"
                    case Bytes | Text         => s"scalusIsB $x"
                    case Bool                 => s"scalusIsBool $x"
                    case AnyData | Unknown(_) => "true"
                    case Again(name)          => s"${leanName(name)} $x"
                    case recursive @ Recursive(name, _) =>
                        if free(recursive).isEmpty then root(recursive) else define(recursive)
                        s"${leanName(name)} $x"
                    case Constr(constructors) =>
                        val tag = next("t")
                        val fields = next("fs")
                        val chain = constructorTests(tag, fields, constructors)((t, n) => on(n, t))
                        s"(match $x with | Data.Constr $tag $fields => $chain | _ => false)"
                    case Items(items) =>
                        val xs = next("xs")
                        val all =
                            if free(items).nonEmpty then
                                val walk = s"${leanName(rootName)}_items${fresh.next()}"
                                val (y, rest) = (next("y"), next("rest"))
                                definitions += s"def $walk ($xs : List Data) : Bool := " +
                                    s"(match $xs with | [] => true | $y :: $rest => " +
                                    s"${on(y, items)} && $walk $rest)"
                                s"$walk $xs"
                            else s"${walkItems(items)} $xs"
                        s"(match $x with | Data.List $xs => $all | _ => false)"
                    case Entries(keys, values, order) =>
                        val ps = next("ps")
                        val all =
                            if free(keys).nonEmpty || free(values).nonEmpty then
                                val walk = s"${leanName(rootName)}_entries${fresh.next()}"
                                val (k, v, rest) = (next("k"), next("v"), next("rest"))
                                definitions += s"def $walk ($ps : List (Data × Data)) : Bool := " +
                                    s"(match $ps with | [] => true | ($k, $v) :: $rest => " +
                                    s"${on(k, keys)} && ${on(v, values)} && $walk $rest)"
                                s"$walk $ps"
                            else s"${walkPairs(keys, values)} $ps"
                        s"(match $x with | Data.Map $ps => $all${ascendingKeys(order, ps)} | _ => false)"
                }
            }
        }
    }

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
