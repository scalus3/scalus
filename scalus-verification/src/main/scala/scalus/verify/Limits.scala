package scalus.verify

import scalus.cardano.onchain.plutus.prelude.List as PList
import scalus.compiler.sir.SIRType
import scalus.uplc.builtin.{ByteString, Data}

import java.nio.charset.StandardCharsets

/** A step from a value to a part of it, in the path of a [[Limit]]. */
enum Step {

    /** The field `name` of a case class. */
    case Field(name: String)

    /** Each element of a list. */
    case Elements

    /** Each key of a map. */
    case Keys

    /** Each value of a map. */
    case Values
}

/** A limit on the shape of a value: on how many elements its collections have.
  *
  * A collection is a list or a map of the value's type, as the type's `Data` encoding has it. The
  * fields of a case class are none: their number is the type's. Nor is a list in a field of type
  * `Data`, which stands for any `Data`.
  */
enum Limit {

    /** Every collection of the value has at most `max` elements. */
    case MaxCollectionLength(max: Int)

    /** Each collection at `path` has at least `min` elements, and at most `max` where it is given.
      * A path through the elements of a list is to the collection of each of them.
      */
    case Length(path: List[Step], min: Int, max: Option[Int])
}

/** The limits of a value, all of which hold.
  *
  * {{{
  * val limits = Limits.of(
  *   Limit.MaxCollectionLength(2),
  *   Limit.Length(List(Step.Field("txInfo"), Step.Field("inputs")), 1, Some(3))
  * )
  * limits.outside(context.toData, contextType)    // None where the context meets them
  * }}}
  */
final case class Limits(all: List[Limit]) {

    /** These limits and `other`'s. */
    def and(other: Limits): Limits = Limits(all ++ other.all)

    /** The limits as `Data`, which a statement variable's type keeps them as ([[Limits.fromData]]):
      * a list of them, each a `Constr` of its case and its parts.
      */
    private[verify] def toData: Data = {
        def step(step: Step): Data = step match
            case Step.Field(name) =>
                Data.Constr(
                  0,
                  PList(Data.B(ByteString.fromArray(name.getBytes(StandardCharsets.UTF_8))))
                )
            case Step.Elements => Data.Constr(1, PList.Nil)
            case Step.Keys     => Data.Constr(2, PList.Nil)
            case Step.Values   => Data.Constr(3, PList.Nil)
        Data.List(PList.from(all.map {
            case Limit.MaxCollectionLength(max) => Data.Constr(0, PList(Data.I(max)))
            case Limit.Length(path, min, max) =>
                val most =
                    max.fold(Data.Constr(1, PList.Nil))(max => Data.Constr(0, PList(Data.I(max))))
                Data.Constr(1, PList(Data.List(PList.from(path.map(step))), Data.I(min), most))
        }))
    }

    /** The least and the most elements a collection at `path` may have, by the limits of that path
      * and by [[Limit.MaxCollectionLength]]. A collection no path can name, as one in a constructor
      * of an enum, is at `None`, which only the latter limits.
      */
    def bounds(path: Option[List[Step]]): (Int, Option[Int]) = {
        val own = path.toList.flatMap(path =>
            all.collect { case Limit.Length(`path`, min, max) => min -> max }
        )
        val everyCollection = all.collect { case Limit.MaxCollectionLength(max) => max }
        val min = own.map(_._1).maxOption.getOrElse(0)
        min -> (everyCollection ++ own.flatMap(_._2)).minOption
    }

    /** Why the limits do not fit the values of type `tp`, or none where they do: a bound below
      * zero, a path that names no collection of the type, a path into the constructors of an enum,
      * which a limit does not reach yet, or bounds that no number of elements meets.
      */
    def unfit(tp: SIRType): Option[String] = unfit(DataLayout.of(tp))

    /** Why `value`, the `Data` encoding of a value of type `tp`, does not meet the limits, or none
      * where it does. Where the limits do not fit the type, why ([[unfit]]); and where `value` is
      * no encoding of a value of the type, why ([[DataEncoding.outside]]).
      */
    def outside(value: Data, tp: SIRType): Option[String] = {
        val layout = DataLayout.of(tp)
        unfit(layout)
            .orElse(DataEncoding.of(tp).outside(value))
            .orElse(lengthsOutside(layout, value, Some(Nil)))
    }

    private[verify] def unfit(layout: DataLayout): Option[String] = {
        val lengths = all.collect { case length: Limit.Length => length }
        val negative = all.collectFirst {
            case Limit.MaxCollectionLength(max) if max < 0 =>
                s"a collection has at most $max elements, below none"
            case Limit.Length(path, min, max) if min < 0 || max.exists(_ < 0) =>
                s"${Limits.show(path)} has from $min to ${max.fold("any number")(_.toString)} " +
                    "elements, below none"
        }
        def unmet = lengths
            .map(_.path)
            .distinct
            .collectFirst(Function.unlift { path =>
                val (min, max) = bounds(Some(path))
                max.filter(_ < min)
                    .map(max =>
                        s"${Limits.show(path)} has at least $min elements and at most $max, which no " +
                            "number of elements is"
                    )
            })
        negative
            .orElse(
              lengths.iterator.flatMap(length => unreached(layout, length.path, Nil)).nextOption()
            )
            .orElse(unmet)
    }

    /** Why `path`, after `walked`, names no collection of `layout`, or none where it names one. */
    private def unreached(
        layout: DataLayout,
        path: List[Step],
        walked: List[Step]
    ): Option[String] = {
        def at = Limits.show(walked)
        (layout, path) match
            case (DataLayout.Collection(_) | DataLayout.Mapping(_, _, _), Nil) => None
            case (_, Nil) => Some(s"$at is no list or map")
            case (DataLayout.Record(fields), (step @ Step.Field(name)) :: rest) =>
                fields.find(_._1 == name) match
                    case Some((_, field)) => unreached(field, rest, walked :+ step)
                    case None             => Some(s"$at has no field $name")
            case (DataLayout.Wrapper(field, inner), (step @ Step.Field(name)) :: rest)
                if field == name =>
                unreached(inner, rest, walked :+ step)
            case (DataLayout.Collection(items), Step.Elements :: rest) =>
                unreached(items, rest, walked :+ Step.Elements)
            case (DataLayout.Mapping(keys, _, _), Step.Keys :: rest) =>
                unreached(keys, rest, walked :+ Step.Keys)
            case (DataLayout.Mapping(_, values, _), Step.Values :: rest) =>
                unreached(values, rest, walked :+ Step.Values)
            case (DataLayout.Choice(_), _) =>
                Some(s"$at is of several constructors, into which a limit does not reach yet")
            case (DataLayout.Plain(DataEncoding.AnyData), _) =>
                Some(s"$at is any Data, which has no collection of its type")
            case (DataLayout.Plain(DataEncoding.Unknown(reason)), _) =>
                Some(s"$at has an encoding not told here: $reason")
            case (_, step :: _) => Some(s"$at has no ${Limits.show(List(step))}")
    }

    /** The largest value laid out as `layout` that meets the limits, as far as they bound it: each
      * collection with as many elements as it may have, or as few as it must where it has no most,
      * and a collection that no limit bounds empty. The keys of a map of byte strings or of
      * integers are distinct and ascending. A map of keys of another type repeats its key's
      * example, which no map of the ledger does: the value is then one to count steps on, not one
      * of the type. Each other part is its encoding's example. None where a part has none.
      */
    private[verify] def largest(layout: DataLayout): Option[Data] = largest(layout, Some(Nil))

    private def largest(layout: DataLayout, path: Option[List[Step]]): Option[Data] = {
        def all(parts: List[Option[Data]]): Option[List[Data]] =
            Option.when(parts.forall(_.isDefined))(parts.flatten)
        def count = {
            val (least, most) = bounds(path)
            most.getOrElse(least)
        }
        def under(step: Step) = path.map(_ :+ step)
        // Byte strings of four bytes, whose order is their number's.
        def bytes(i: Int) =
            Data.B(ByteString.fromArray(Array(i >>> 24, i >>> 16, i >>> 8, i).map(_.toByte)))
        layout match {
            case DataLayout.Plain(encoding) => encoding.example
            case DataLayout.Record(fields) =>
                all(fields.map((name, field) => largest(field, under(Step.Field(name)))))
                    .map(values => Data.Constr(0, PList.from(values)))
            case DataLayout.Choice(constructors) =>
                constructors.iterator.zipWithIndex
                    .flatMap { case ((_, fields), tag) =>
                        all(fields.map((_, field) => largest(field, None)))
                            .map(values => Data.Constr(tag, PList.from(values)))
                    }
                    .nextOption()
            case DataLayout.Wrapper(name, inner) => largest(inner, under(Step.Field(name)))
            case DataLayout.Collection(items) =>
                if count == 0 then Some(Data.List(PList.Nil))
                else
                    largest(items, under(Step.Elements))
                        .map(item => Data.List(PList.from(List.fill(count)(item))))
            case DataLayout.Mapping(keys, values, order) =>
                if count == 0 then Some(Data.Map(PList.Nil))
                else
                    val distinct = (order, keys) match
                        case (DataEncoding.KeyOrder.Bytes, _) |
                            (_, DataLayout.Plain(DataEncoding.Bytes | DataEncoding.Text)) =>
                            Some((0 until count).map(bytes).toList)
                        case (DataEncoding.KeyOrder.Integers, _) |
                            (_, DataLayout.Plain(DataEncoding.Integer)) =>
                            Some((0 until count).map(i => Data.I(BigInt(i))).toList)
                        case _ => largest(keys, under(Step.Keys)).map(List.fill(count)(_))
                    for
                        keys <- distinct
                        item <- largest(values, under(Step.Values))
                    yield Data.Map(PList.from(keys.map(_ -> item)))
        }
    }

    /** Why the collections of `value`, laid out as `layout`, at `path`, do not meet the limits, or
      * none where they do. `value` is laid out so.
      */
    private def lengthsOutside(
        layout: DataLayout,
        value: Data,
        path: Option[List[Step]]
    ): Option[String] = {
        def inner(layout: DataLayout, value: Data, step: Step) =
            lengthsOutside(layout, value, path.map(_ :+ step))
        def elements(count: Int) = {
            val (min, max) = bounds(path)
            Option.when(count < min || max.exists(count > _)) {
                val where = path.fold("a collection in a constructor of an enum")(Limits.show)
                val noun = if count == 1 then "element" else "elements"
                s"$where has $count $noun, not from $min to ${max.fold("any number")(_.toString)}"
            }
        }
        (layout, value) match {
            case (DataLayout.Record(fields), Data.Constr(_, values)) =>
                fields.iterator
                    .zip(values.toScalaList)
                    .flatMap { case ((name, field), value) =>
                        inner(field, value, Step.Field(name))
                    }
                    .nextOption()
            // A path does not reach into the constructors of an enum ([[unreached]]).
            case (DataLayout.Choice(constructors), Data.Constr(tag, values)) =>
                constructors(tag.toInt)._2.iterator
                    .zip(values.toScalaList)
                    .flatMap { case ((_, field), value) => lengthsOutside(field, value, None) }
                    .nextOption()
            case (DataLayout.Wrapper(name, field), _) => inner(field, value, Step.Field(name))
            case (DataLayout.Collection(items), Data.List(values)) =>
                val all = values.toScalaList
                elements(all.size).orElse(
                  all.iterator.flatMap(inner(items, _, Step.Elements)).nextOption()
                )
            case (DataLayout.Mapping(keys, values, _), Data.Map(entries)) =>
                val all = entries.toScalaList
                elements(all.size).orElse(
                  all.iterator
                      .flatMap((key, item) =>
                          inner(keys, key, Step.Keys).orElse(inner(values, item, Step.Values))
                      )
                      .nextOption()
                )
            case _ => None
        }
    }
}

object Limits {

    /** No limits. */
    val none: Limits = Limits(Nil)

    /** The limits `limits`, all of which hold. */
    def of(limits: Limit*): Limits = Limits(limits.toList)

    /** `n` as a number of elements, of a bound of a limit that a statement writes. */
    def count(n: BigInt): Int = {
        require(n.isValidInt && n >= 0, s"a collection has no $n elements")
        n.toInt
    }

    /** The limits that `data` is of ([[Limits.toData]]). */
    private[verify] def fromData(data: Data): Limits = {
        def malformed = throw new IllegalArgumentException(s"$data is no limits")
        def int(data: Data): Int = data match
            case Data.I(value) if value.isValidInt => value.toInt
            case _                                 => malformed
        def step(data: Data): Step = data match
            case Data.Constr(0, PList.Cons(Data.B(name), PList.Nil)) =>
                Step.Field(new String(name.bytes, StandardCharsets.UTF_8))
            case Data.Constr(1, PList.Nil) => Step.Elements
            case Data.Constr(2, PList.Nil) => Step.Keys
            case Data.Constr(3, PList.Nil) => Step.Values
            case _                         => malformed
        data match
            case Data.List(limits) =>
                Limits(limits.toScalaList.map {
                    case Data.Constr(0, PList.Cons(max, PList.Nil)) =>
                        Limit.MaxCollectionLength(int(max))
                    case Data.Constr(
                          1,
                          PList.Cons(Data.List(path), PList.Cons(min, PList.Cons(most, PList.Nil)))
                        ) =>
                        val max = most match
                            case Data.Constr(0, PList.Cons(max, PList.Nil)) => Some(int(max))
                            case Data.Constr(1, PList.Nil)                  => None
                            case _                                          => malformed
                        Limit.Length(path.toScalaList.map(step), int(min), max)
                    case _ => malformed
                })
            case _ => malformed
    }

    /** A path as it reads: `txInfo.inputs.*.resolved`, where `*` is each element of a list, and
      * `keys` and `values` those of a map.
      */
    def show(path: List[Step]): String =
        if path.isEmpty then "the value"
        else
            path.map {
                case Step.Field(name) => name
                case Step.Elements    => "*"
                case Step.Keys        => "keys"
                case Step.Values      => "values"
            }.mkString(".")
}
