package scalus.prelude

import scalus.Compile
import scalus.builtin.Builtins.*
import scalus.builtin.Data.fromData
import scalus.builtin.{Data, FromData, ToData}
import scala.annotation.tailrec

/** Alternative to `scala.collection.immutable.SortedMap` in onchain code.
  * @tparam A
  *   the type of keys, must be an instance of `Ord`
  * @tparam B
  *   the type of values
  */
case class SortedMap[A, B] private (toList: List[(A, B)])

@Compile
object SortedMap {
    import List.*
    import Option.*

    /** Constructs an empty `SortedMap`.
      *
      * @example
      *   {{{
      *   SortedMap.empty.toList === List.empty
      *   }}}
      */
    def empty[A, B]: SortedMap[A, B] = SortedMap(List.empty)

    /** Constructs a `SortedMap` with a single key-value pair.
      *
      * @param key
      *   the key to insert
      * @param value
      *   the value associated with the key
      * @return
      *   a `SortedMap` containing the single key-value pair
      * @example
      *   {{{
      *   SortedMap.singleton("key", "value").toList === List.single(("key", "value"))
      *   }}}
      */
    def singleton[A, B](key: A, value: B): SortedMap[A, B] = SortedMap(
      List.single((key, value))
    )

    /** Constructs a `SortedMap` in unsafe way from a list of key-value pairs assuming that it's in
      * strictly ascending order without any validation.
      *
      * @param lst
      *   the list of key-value pairs
      * @return
      *   a `SortedMap` containing the key-value pairs from the list
      * @example
      *   {{{
      *   SortedMap.unsafeFromList(List.Cons(("a", 1), List.Cons(("b", 2), List.Nil))).toList === List.Cons(("a", 1), List.Cons(("b", 2), List.Nil))
      *   }}}
      * @see
      *   [[fromList]] or [[fromStrictlyAscendingList]] for safe versions
      */
    def unsafeFromList[A, B](lst: List[(A, B)]): SortedMap[A, B] = SortedMap(lst)

    /** Constructs a `SortedMap` from a list of key-value pairs, ordering it in strictly ascending
      * order, in case when a key is presented multiple times, the first occurrence prevails.
      *
      * @param lst
      *   the list of key-value pairs
      * @return
      *   a `SortedMap` containing the key-value pairs from the list, with unique keys in sorted
      *   order
      * @example
      *   {{{
      *   SortedMap.fromList(List.Cons(("b", 2), List.Cons(("a", 1), List.Nil))).toList === List.Cons(("a", 1), List.Cons(("b", 2), List.Nil))
      *   SortedMap.fromList(List.Cons(("a", 1), List.Cons(("a", 2), List.Nil))).toList === List.Cons(("a", 1), List.Nil)
      *   }}}
      * @see
      *   [[unsafeFromList]] for an unfiltered unsafe version or [[fromStrictlyAscendingList]] for a
      *   faster stricter version
      */
    def fromList[A: Ord, B](lst: List[(A, B)]): SortedMap[A, B] = {
        def insertIfDoesNotExist(lst: List[(A, B)], key: A, value: B): List[(A, B)] = lst match
            case Nil              => single(key, value)
            case Cons(pair, tail) =>
                pair match
                    case (k, v) =>
                        key <=> k match
                            case Order.Less    => Cons((key, value), lst)
                            case Order.Greater => Cons(pair, insertIfDoesNotExist(tail, key, value))
                            case Order.Equal   => lst

        SortedMap(
          lst.foldLeft(List.empty) { (acc, pair) => insertIfDoesNotExist(acc, pair._1, pair._2) }
        )
    }

    /** Constructs a `SortedMap` from a list of key-value pairs, or fails if the list is not in
      * strictly ascending order.
      *
      * @param lst
      *   the list of key-value pairs
      * @return
      *   a `SortedMap` containing the key-value pairs from the list, or fails if the list is not in
      *   strictly ascending order
      * @throws scalus.cardano.onchain.RequirementError
      *   if the list is not in strictly ascending order
      * @example
      *   {{{
      *   SortedMap.fromStrictlyAscendingList(List.Cons(("a", 1), List.Cons(("b", 2), List.Nil))).toList === List.Cons(("a", 1), List.Cons(("b", 2), List.Nil))
      *   SortedMap.fromStrictlyAscendingList(List.Cons(("a", 1), List.Cons(("a", 2), List.Nil))) // throws scalus.cardano.onchain.RequirementError
      *   }}}
      * @see
      *   [[unsafeFromList]] for an unsafe fast or [[fromList]] for a safe slow versions that don't
      *   require strictly ascending order
      */
    def fromStrictlyAscendingList[A: Ord, B](
        lst: List[(A, B)]
    ): SortedMap[A, B] = {
        @tailrec
        def checkStrictlyAscendingOrder(
            lst: List[(A, B)]
        ): Boolean = lst match
            case Nil               => true
            case Cons(pair1, tail) =>
                tail match
                    case Nil            => true
                    case Cons(pair2, _) =>
                        pair1._1 <=> pair2._1 match
                            case Order.Less => checkStrictlyAscendingOrder(tail)
                            case _          => false

        require(checkStrictlyAscendingOrder(lst), "List is not strictly ascending")
        SortedMap(lst)
    }

    /** Merges two `SortedMap`s into a new `SortedMap` containing keys from both maps. if a key is
      * only present in left map, it is wrapped in `These.This`, if only in right map, it is wrapped
      * in `These.That`, if a key is present in both maps, its values are wrapped in `These.These`.
      * The resulting map is sorted by keys in strictly ascending order.
      *
      * This method is useful for combining two maps where you want to keep track of which keys are
      * unique to each map and which keys are shared between
      *
      * @param lhs
      *   the left-hand side `SortedMap`
      * @param rhs
      *   the right-hand side `SortedMap`
      * @return
      *   a new `SortedMap` containing keys from both maps, with values combined into `These`
      * @example
      *   {{{
      *   val map1 = SortedMap.fromList(List.Cons(("a", 1), List.Cons(("b", 2), List.Nil)))
      *   val map2 = SortedMap.fromList(List.Cons(("b", 3), List.Cons(("c", 4), List.Nil)))
      *   SortedMap.union(map1, map2).toList === List.Cons(("a", These.This(1)), List.Cons(("b", These.These(2, 3)), List.Cons(("c", These.That(4)), List.Nil)))
      *   }}}
      */
    def union[A: Ord, B, C](
        lhs: SortedMap[A, B],
        rhs: SortedMap[A, C]
    ): SortedMap[A, These[B, C]] = {
        def go(
            lhs: List[(A, B)],
            rhs: List[(A, C)]
        ): List[(A, These[B, C])] = lhs match
            case Nil                    => rhs.map { pair => (pair._1, These.That(pair._2)) }
            case Cons(lhsPair, lhsTail) =>
                rhs match
                    case Nil => lhs.map { pair => (pair._1, These.This(pair._2)) }
                    case Cons(rhsPair, rhsTail) =>
                        lhsPair match
                            case (lhsKey, lhsValue) =>
                                rhsPair match
                                    case (rhsKey, rhsValue) =>
                                        lhsKey <=> rhsKey match
                                            case Order.Less =>
                                                Cons(
                                                  (lhsKey, These.This(lhsValue)),
                                                  go(lhsTail, rhs)
                                                )
                                            case Order.Greater =>
                                                Cons(
                                                  (rhsKey, These.That(rhsValue)),
                                                  go(lhs, rhsTail)
                                                )
                                            case Order.Equal =>
                                                Cons(
                                                  (lhsKey, These.These(lhsValue, rhsValue)),
                                                  go(lhsTail, rhsTail)
                                                )

        SortedMap(go(lhs.toList, rhs.toList))
    }

    /** Merges two `SortedMap`s into a new `SortedMap` and maps the combined values using a
      * function.
      *
      * Similar to `union` but additionally transforms the values. Keys that exist only in the left
      * map have their values wrapped in `These.This`, keys that exist only in the right map have
      * their values wrapped in `These.That`, and keys that exist in both maps have their values
      * wrapped in `These.These` before being passed to the mapping function.
      *
      * @param lhs
      *   the left-hand side `SortedMap`
      * @param rhs
      *   the right-hand side `SortedMap`
      * @param f
      *   the function to transform the combined values
      * @return
      *   a new `SortedMap` containing merged and transformed values
      * @example
      *   {{{
      *   val map1 = SortedMap.fromList(List.Cons(("a", 1), List.Cons(("b", 2), List.Nil)))
      *   val map2 = SortedMap.fromList(List.Cons(("b", 3), List.Cons(("c", 4), List.Nil)))
      *   val combined = SortedMap.unionMap(map1, map2, {
      *     case These.This(x) => x * 10      // Only in left map
      *     case These.That(y) => y * 100     // Only in right map
      *     case These.These(x, y) => x + y   // In both maps
      *   })
      *   combined === List.Cons(("a", 10), List.Cons(("b", 5), List.Cons(("c", 400), List.Nil)))
      *   }}}
      */
    def unionMap[A: Ord, B, C, D](
        lhs: SortedMap[A, B],
        rhs: SortedMap[A, C],
        f: These[B, C] => D
    ): SortedMap[A, D] = {
        def go(
            lhs: List[(A, B)],
            rhs: List[(A, C)]
        ): List[(A, D)] = lhs match
            case Nil                    => rhs.map { pair => (pair._1, f(These.That(pair._2))) }
            case Cons(lhsPair, lhsTail) =>
                rhs match
                    case Nil => lhs.map { pair => (pair._1, f(These.This(pair._2))) }
                    case Cons(rhsPair, rhsTail) =>
                        lhsPair match
                            case (lhsKey, lhsValue) =>
                                rhsPair match
                                    case (rhsKey, rhsValue) =>
                                        lhsKey <=> rhsKey match
                                            case Order.Less =>
                                                Cons(
                                                  (lhsKey, f(These.This(lhsValue))),
                                                  go(lhsTail, rhs)
                                                )
                                            case Order.Greater =>
                                                Cons(
                                                  (rhsKey, f(These.That(rhsValue))),
                                                  go(lhs, rhsTail)
                                                )
                                            case Order.Equal =>
                                                Cons(
                                                  (lhsKey, f(These.These(lhsValue, rhsValue))),
                                                  go(lhsTail, rhsTail)
                                                )

        SortedMap(go(lhs.toList, rhs.toList))
    }

    /** Provides an `Eq` instance for `SortedMap[A, B]` where both key and value types are instances
      * of `Eq`.
      */
    given sortedMapEq[A: Eq, B: Eq]: Eq[SortedMap[A, B]] =
        (lhs: SortedMap[A, B], rhs: SortedMap[A, B]) => lhs.toList === rhs.toList

    /** Provides an `Ord` instance for `SortedMap[A, B]` where both key and value types are
      * instances of `Ord`.
      */
    given sortedMapOrd[A: Ord, B: Ord]: Ord[SortedMap[A, B]] =
        (lhs: SortedMap[A, B], rhs: SortedMap[A, B]) => lhs.toList <=> rhs.toList

    /** Provides a `FromData` instance for `SortedMap[A, B]` where both key and value types are
      * instances of `FromData`. There is no validation that the keys are in strictly ascending
      * order
      */
    given sortedMapFromData[A: FromData, B: FromData]: FromData[SortedMap[A, B]] =
        (d: Data) =>
            def loop(
                ls: scalus.builtin.BuiltinList[scalus.builtin.BuiltinPair[Data, Data]]
            ): scalus.prelude.List[(A, B)] =
                if ls.isEmpty then Nil
                else
                    val pair = ls.head
                    Cons(
                      (fromData[A](pair.fst), fromData[B](pair.snd)),
                      loop(ls.tail)
                    )
            SortedMap(loop(unMapData(d)))

    /** Provides a validating `FromData` instance for `SortedMap[A, B]` where both key and value
      * types are instances of `FromData`. This method ensures that the keys are in strictly
      * ascending order using `Ord[A]` instance for keys.
      *
      * @throws scalus.cardano.onchain.RequirementError
      *   if the keys are not in strictly ascending order
      */
    def sortedMapFromDataWithValidation[A: FromData: Ord, B: FromData]: FromData[SortedMap[A, B]] =
        (d: Data) =>
            def loop(
                ls: scalus.builtin.BuiltinList[scalus.builtin.BuiltinPair[Data, Data]]
            ): scalus.prelude.List[(A, B)] =
                if ls.isEmpty then Nil
                else
                    val pair = ls.head
                    Cons(
                      (fromData[A](pair.fst), fromData[B](pair.snd)),
                      loop(ls.tail)
                    )
            SortedMap.fromStrictlyAscendingList(loop(unMapData(d)))

    /** Provides a `ToData` instance for `SortedMap[A, B]` where both key and value types are
      * instances of `ToData`.
      */
    given sortedMapToData[A: ToData, B: ToData]: ToData[SortedMap[A, B]] =
        (a: SortedMap[A, B]) => {
            def go(
                a: List[(A, B)]
            ): scalus.builtin.BuiltinList[scalus.builtin.BuiltinPair[Data, Data]] =
                a match {
                    case Nil               => mkNilPairData()
                    case Cons(tuple, tail) =>
                        tuple match {
                            case (a, b) =>
                                mkCons(
                                  scalus.builtin
                                      .BuiltinPair(summon[ToData[A]](a), summon[ToData[B]](b)),
                                  go(tail)
                                )
                        }
                }

            mapData(go(a.toList))
        }

    extension [A, B](self: SortedMap[A, B])
        /** Checks if the `SortedMap` is empty.
          *
          * @return
          *   `true` if the map is empty, `false` otherwise
          * @example
          *   {{{
          *   SortedMap.empty.isEmpty === true
          *   SortedMap.singleton("key", "value").isEmpty === false
          *   }}}
          */
        inline def isEmpty: Boolean = self.toList.isEmpty

        /** Checks if the `SortedMap` is non-empty.
          *
          * @return
          *   `true` if the map is non-empty, `false` otherwise
          * @example
          *   {{{
          *   SortedMap.empty.nonEmpty === false
          *   SortedMap.singleton("key", "value").nonEmpty === true
          *   }}}
          */
        inline def nonEmpty: Boolean = self.toList.nonEmpty

        /** Returns the number of key-value pairs in the `SortedMap`.
          *
          * @return
          *   the number of key-value pairs in the map
          * @example
          *   {{{
          *   SortedMap.empty.length === 0
          *   SortedMap.singleton("key", "value").length === 1
          *   }}}
          */
        inline def length: BigInt = self.toList.length

        /** Returns the size of the `SortedMap`, which is the same as its length.
          *
          * @return
          *   the size of the map
          * @example
          *   {{{
          *   SortedMap.empty.size === 0
          *   SortedMap.singleton("key", "value").size === 1
          *   }}}
          */
        inline def size: BigInt = length

        /** Returns a list of keys in the `SortedMap`.
          *
          * @return
          *   a list containing all keys in the map
          * @example
          *   {{{
          *   SortedMap.empty.keys === List.empty
          *   SortedMap.singleton("key", "value").keys === List.single("key")
          *   SortedMap.fromList(List.Cons(("a", 1), List.Cons(("b", 2), List.Nil))).keys === List.Cons("a", List.Cons("b", List.Nil))
          *   }}}
          */
        def keys: List[A] = self.toList.map { case (k, _) => k }

        /** Returns a list of values in the `SortedMap`.
          *
          * @return
          *   a list containing all values in the map
          * @example
          *   {{{
          *   SortedMap.empty.values === List.empty
          *   SortedMap.singleton("key", "value").values === List.single("value")
          *   SortedMap.fromList(List.Cons(("a", 1), List.Cons(("b", 2), List.Nil))).values === List.Cons(1, List.Cons(2, List.Nil))
          *   }}}
          */
        def values: List[B] = self.toList.map { case (_, v) => v }

        /** Checks if a predicate holds for all key-value pairs in the `SortedMap`.
          *
          * @param f
          *   the predicate function to check
          * @return
          *   `true` if the predicate holds for all pairs, `false` otherwise
          * @example
          *   {{{
          *   SortedMap.empty.forall(_ => true) === true
          *   SortedMap.singleton("key", "value").forall(_._1 === "foo") === false
          *   SortedMap.singleton("key", "value").forall(_._1 === "key") === true
          *   SortedMap.fromList(List.Cons(("a", 1), List.Cons(("b", 2), List.Nil))).forall(_._2 > 0) === true
          *   }}}
          */
        def forall(f: ((A, B)) => Boolean): Boolean = self.toList.forall(f)

        /** Checks if a predicate holds for at least one key-value pair in the `SortedMap`.
          *
          * @param f
          *   the predicate function to check
          * @return
          *   `true` if the predicate holds for at least one pair, `false` otherwise
          * @example
          *   {{{
          *   SortedMap.empty.exists(_ => true) === false
          *   SortedMap.singleton("key", "value").exists(_._1 === "foo") === false
          *   SortedMap.singleton("key", "value").exists(_._1 === "key") === true
          *   SortedMap.fromList(List.Cons(("a", 1), List.Cons(("b", 2), List.Nil))).exists(_._2 > 1) === true
          *   }}}
          */
        def exists(f: ((A, B)) => Boolean): Boolean = self.toList.exists(f)

        /** Maps the values of the `SortedMap` using a function.
          *
          * @param f
          *   the function to apply to each value
          * @return
          *   a new `SortedMap` with the same keys and transformed values
          * @example
          *   {{{
          *   SortedMap.singleton("key", 1).mapValues(_ + 1).toList === List.single(("key", 2))
          *   SortedMap.fromList(List.Cons(("a", 1), List.Cons(("b", 2), List.Nil))).mapValues(_ * 2).toList === List.Cons(("a", 2), List.Cons(("b", 4), List.Nil))
          *   }}}
          */
        def mapValues[C](f: B => C): SortedMap[A, C] = SortedMap(
          self.toList.map((k, v) => (k, f(v)))
        )

        /** Filters the keys of the `SortedMap` based on a predicate.
          *
          * @param predicate
          *   the predicate function to apply to each key
          * @return
          *   a new `SortedMap` containing only the key-value pairs where the key satisfies the
          *   predicate
          * @example
          *   {{{
          *   SortedMap.fromList(List.Cons(("a", 1), List.Cons(("b", 2), List.Nil))).filterKeys(_ === "a").toList === List.Cons(("a", 1), List.Nil)
          *   }}}
          */
        def filterKeys(predicate: A => Boolean): SortedMap[A, B] =
            SortedMap(self.toList.filter { case (k, _) =>
                predicate(k)
            })

        /** Filters the key-value pairs of the `SortedMap` based on a predicate.
          *
          * @param predicate
          *   the predicate function to apply to each key-value pair
          * @return
          *   a new `SortedMap` containing only the key-value pairs that satisfy the predicate
          * @example
          *   {{{
          *   SortedMap.fromList(List.Cons(("a", 1), List.Cons(("b", 2), List.Nil))).filter(_._2 > 1).toList === List.Cons(("b", 2), List.Nil)
          *   }}}
          */
        def filter(predicate: ((A, B)) => Boolean): SortedMap[A, B] =
            SortedMap(self.toList.filter(predicate))

        /** Filters the key-value pairs of the `SortedMap` based on a negated predicate.
          *
          * @param predicate
          *   the predicate function to apply to each key-value pair, negated
          * @return
          *   a new `SortedMap` containing only the key-value pairs that do not satisfy the
          *   predicate
          * @example
          *   {{{
          *   SortedMap.fromList(List.Cons(("a", 1), List.Cons(("b", 2), List.Nil))).filterNot(_._2 > 1).toList === List.Cons(("a", 1), List.Nil)
          *   }}}
          */
        def filterNot(predicate: ((A, B)) => Boolean): SortedMap[A, B] =
            SortedMap(self.toList.filterNot(predicate))

        /** Optionally returns the first key-value pair that satisfies a predicate.
          *
          * @param predicate
          *   the predicate function to apply to each key-value pair
          * @return
          *   an `Option` containing the first key-value pair that satisfies the predicate, or
          *   `None` if no such pair exists
          * @example
          *   {{{
          *   SortedMap.fromList(List.Cons(("a", 1), List.Cons(("b", 2), List.Nil))).find(_._1 === "b") === Some(("b", 2))
          *   SortedMap.fromList(List.Cons(("a", 1), List.Cons(("b", 2), List.Nil))).find(_._1 === "c") === None
          *   }}}
          */
        def find(predicate: ((A, B)) => Boolean): Option[(A, B)] = self.toList.find(predicate)

        /** Finds the first key-value pair that satisfies a predicate and maps it to a new type.
          *
          * @param predicate
          *   the predicate function to apply to each key-value pair
          * @return
          *   an `Option` containing the result of mapping the first key-value pair that satisfies
          *   the predicate, or `None` if no such pair exists
          * @example
          *   {{{
          *   SortedMap.fromList(List.Cons(("a", 1), List.Cons(("b", 2), List.Nil))).findMap {
          *     case ("b", v) => Some(v + 1)
          *     case _        => None
          *   } === Some(3)
          *
          *   SortedMap.fromList(List.Cons(("a", 1), List.Cons(("b", 2), List.Nil))).findMap {
          *     case ("c", v) => Some(v + 1)
          *     case _        => None
          *   } === None
          *
          *   SortedMap.empty.findMap(_ => Some(1)) === None
          *   }}}
          */
        def findMap[C](
            predicate: ((A, B)) => Option[C]
        ): Option[C] = self.toList.findMap(predicate)

        /** Folds the `SortedMap` from the left, combining key-value pairs into a single value.
          *
          * @param init
          *   the initial value to start folding from
          * @param combiner
          *   the function to combine the accumulated value with each key-value pair
          * @return
          *   the final accumulated value after folding over all key-value pairs
          * @example
          *   {{{
          *   SortedMap.fromList(List.Cons(("a", 1), List.Cons(("b", 2), List.Nil))).foldLeft(0)(_ + _._2) === 3
          *   }}}
          */
        def foldLeft[C](init: C)(combiner: (C, (A, B)) => C): C = {
            self.toList match
                case Nil              => init
                case Cons(pair, tail) =>
                    SortedMap(tail).foldLeft(combiner(init, pair))(combiner)
        }

        /** Folds the `SortedMap` from the right, combining key-value pairs into a single value.
          *
          * @param init
          *   the initial value to start folding from
          * @param combiner
          *   the function to combine the accumulated value with each key-value pair
          * @return
          *   the final accumulated value after folding over all key-value pairs
          * @example
          *   {{{
          *   SortedMap.fromList(List.Cons(("a", 1), List.Cons(("b", 2), List.Nil))).foldLeft(0)(_ + _._2) === 3
          *   }}}
          */
        def foldRight[C](init: C)(combiner: ((A, B), C) => C): C =
            self.toList.foldRight(init) { (pair, acc) => combiner(pair, acc) }

    extension [A: Ord, B](self: SortedMap[A, B])
        /** Optionally returns the value associated with a key.
          *
          * @param key
          *   the key value
          * @return
          *   an option value containing the value associated with `key` in this map, or `None` if
          *   none exists.
          * @example
          *   {{{
          *   SortedMap.empty.get("key") === None
          *   SortedMap.singleton("key", "value").get("key") === Some("value")
          *   SortedMap.fromList(List.Cons(("a", 1), List.Cons(("b", 2), List.Nil))).get("a") === Some(1)
          *   SortedMap.fromList(List.Cons(("a", 1), List.Cons(("b", 2), List.Nil))).get("c") === None
          *   }}}
          */
        def get(key: A): Option[B] = {
            @tailrec
            def go(lst: List[(A, B)]): Option[B] = lst match
                case Nil              => None
                case Cons(pair, tail) =>
                    pair match
                        case (k, v) =>
                            key <=> k match
                                case Order.Less    => None
                                case Order.Greater => go(tail)
                                case Order.Equal   => Some(v)

            go(self.toList)
        }

        /** Retrieves the value associated with a key, or fails with a custom message if the key is
          * not presented.
          *
          * @param key
          *   the key to retrieve the value for
          * @param message
          *   the custom error message to use if the key is not found
          * @return
          *   the value associated with the key
          * @throws NoSuchElementException
          *   if the key is not present in the map
          * @example
          *   {{{
          *   SortedMap.singleton("key", "value").getOrFail("key") === "value"
          *   SortedMap.fromList(List.Cons(("a", 1), List.Cons(("b", 2), List.Nil))).getOrFail("a") === 1
          *   SortedMap.fromList(List.Cons(("a", 1), List.Cons(("b", 2), List.Nil))).getOrFail("c") // throws NoSuchElementException
          *   SortedMap.empty.getOrFail("key") // throws NoSuchElementException
          *   }}}
          */
        inline def getOrFail(
            key: A,
            inline message: String = "SortedMap doesn't contain searching key"
        ): B = get(key).getOrFail(message)

        /** Retrieves the value associated with a key, or fails if the key is not present.
          *
          * @param key
          *   the key to retrieve the value for
          * @return
          *   the value associated with the key
          * @throws NoSuchElementException
          *   if the key is not present in the map
          * @example
          *   {{{
          *   SortedMap.singleton("key", "value").at("key") === "value"
          *   SortedMap.fromList(List.Cons(("a", 1), List.Cons(("b", 2), List.Nil))).at("a") === 1
          *   SortedMap.fromList(List.Cons(("a", 1), List.Cons(("b", 2), List.Nil))).at("c") // throws NoSuchElementException
          *   SortedMap.empty.at("key") // throws NoSuchElementException
          *   }}}
          */
        def at(key: A): B = get(key).getOrFail("Undefined key in SortedMap.at")

        /** Checks if the `SortedMap` contains a key.
          *
          * @param key
          *   the key to check for existence
          * @return
          *   `true` if the map contains the key, `false` otherwise
          * @example
          *   {{{
          *   SortedMap.empty.contains("key") === false
          *   SortedMap.singleton("key", "value").contains("key") === true
          *   SortedMap.fromList(List.Cons(("a", 1), List.Cons(("b", 2), List.Nil))).contains("a") === true
          *   SortedMap.fromList(List.Cons(("a", 1), List.Cons(("b", 2), List.Nil))).contains("c") === false
          *   }}}
          */
        def contains(key: A): Boolean = get(key).isDefined

        /** Insert a key-value pair into the `SortedMap`, maintaining the sorted order without
          * duplication. * If the key already exists, it updates the value. * @param key the key to
          * insert
          * @param value
          *   the value associated with the key * @return a new `SortedMap` with the key-value pair
          *   inserted
          * @example
          *   {{{
          *   SortedMap.empty.insert("key", "value") === SortedMap.singleton("key", "value")
          *   SortedMap.singleton("key", "value").insert("key", "newValue") === SortedMap.singleton("key", "newValue")
          *   SortedMap.fromList(List.Cons(("a", 1), List.Cons(("b", 2), List.Nil))).insert("c", 3).toList === List.Cons(("a", 1), List.Cons(("b", 2), List.Cons(("c", 3), List.Nil)))
          *   }}}
          */
        def insert(key: A, value: B): SortedMap[A, B] = {
            def go(lst: List[(A, B)]): List[(A, B)] = lst match
                case Nil              => single(key, value)
                case Cons(pair, tail) =>
                    pair match
                        case (k, v) =>
                            key <=> k match
                                case Order.Less    => Cons((key, value), lst)
                                case Order.Greater => Cons(pair, go(tail))
                                case Order.Equal   => Cons((key, value), tail)

            SortedMap(go(self.toList))
        }

        /** Deletes a key-value pair from the `SortedMap` by key.
          *
          * @param key
          *   the key to delete
          * @return
          *   a new `SortedMap` with the key-value pair removed, if it existed
          * @example
          *   {{{
          *   SortedMap.empty[String, BigInt].delete("key") === SortedMap.empty
          *   SortedMap.fromList(List.Cons(("a", 1), List.Cons(("b", 2), List.Nil))).delete("a").toList === List.Cons(("b", 2), List.Nil)
          *   SortedMap.fromList(List.Cons(("a", 1), List.Cons(("b", 2), List.Nil))).delete("c").toList === List.Cons(("a", 1), List.Cons(("b", 2), List.Nil))
          *   }}}
          */
        def delete(key: A): SortedMap[A, B] = {
            def go(m: SortedMap[A, B]): SortedMap[A, B] = m.toList match
                case Nil              => m
                case Cons(pair, tail) =>
                    pair match
                        case (k, v) =>
                            key <=> k match
                                case Order.Less    => m
                                case Order.Greater =>
                                    // TODO: check that cons is use PairList representation.
                                    SortedMap(Cons(pair, go(SortedMap(tail)).toList))
                                case Order.Equal => SortedMap(tail)
            go(self)
        }

}
