package scalus.prelude

import scalus.Compile
import scalus.Ignore
import scalus.builtin.Builtins.*
import scalus.builtin.Data.fromData
import scalus.builtin.{BuiltinPair, Data, FromData, ToData}
import scala.annotation.tailrec
import scala.collection.mutable

enum List[+A]:
    case Nil extends List[Nothing]
    case Cons(head: A, tail: List[A]) extends List[A]

@Compile
object List {
    import Option.*

    /** Returns an empty list.
      *
      * This is an inline function that creates a new empty list of the specified type.
      *
      * @tparam A
      *   The type of elements this empty list would contain.
      * @return
      *   An empty list of type `List[A]`.
      */
    inline def empty[A]: List[A] = List.Nil

    /** Creates a list with a single element */
    def single[A](a: A): List[A] = Cons(a, List.Nil)

    /** Creates a list from a variable number of arguments.
      *
      * This method allows creating a list by simply providing the elements. Works only offchain.
      *
      * @tparam A
      *   The type of elements in the list.
      * @param args
      *   The elements to include in the list.
      * @return
      *   A list containing all provided arguments.
      * @example
      *   {{{
      *   List("a", "b", "c") === Cons("a", Cons("b", Cons("c", Nil)))
      *   List() === Nil
      *   }}}
      */
    def apply[A](args: A*): List[A] = args.list

    /** Creates a list from any Scala iterable collection.
      *
      * Works only offchain.
      *
      * @tparam A
      *   The type of elements in the list.
      * @param i
      *   The iterable collection to convert to a list.
      * @return
      *   A list containing all elements from the provided iterable.
      * @example
      *   {{{
      *   List.from(Vector(BigInt(1), BigInt(2), BigInt(3))) === Cons(BigInt(1), Cons(BigInt(2), Cons(BigInt(3), Nil)))
      *   List.from(Iterator.empty) === Nil
      *   }}}
      */
    @Ignore
    def from[A](i: IterableOnce[A]): List[A] = i.iterator.foldRight(empty[A]) { case (a, b) =>
        Cons(a, b)
    }

    /** Creates a list from a Java iterable collection.
      *
      * Works only offchain.
      *
      * @tparam A
      *   The type of elements in the list.
      * @param i
      *   The Java iterable collection to convert to a list.
      * @return
      *   A list containing all elements from the provided Java iterable.
      * @example
      *   {{{
      *   import java.util.Arrays
      *   val javaList = Arrays.asList("a", "b", "c")
      *   List.from(javaList) === Cons("a", Cons("b", Cons("c", Nil)))
      *
      *   val emptyJavaList = new java.util.ArrayList[String]()
      *   List.from(emptyJavaList) === Nil
      *   }}}
      */
    @Ignore
    def from[A](i: java.lang.Iterable[A]): List[A] =
        import scala.jdk.CollectionConverters.*
        from(i.asScala)

    /** Creates a list containing a range of `BigInt` values, inclusive of both endpoints.
      *
      * This method creates a list containing all integers from `from` up to and including `to`. If
      * `from` is greater than `to`, an empty list is returned.
      *
      * @param from
      *   The starting value of the range (inclusive).
      * @param to
      *   The ending value of the range (inclusive).
      * @return
      *   A list containing all `BigInt` values from `from` to `to`, inclusive.
      * @example
      *   {{{
      *   List.range(1, 3) === Cons(BigInt(1), Cons(BigInt(2), Cons(BigInt(3), Nil)))
      *   List.range(5, 3) === Nil
      *   List.range(0, 0) === Cons(BigInt(0), Nil)
      *   }}}
      */
    def range(from: BigInt, to: BigInt): List[BigInt] =
        if from <= to then Cons(from, range(from + 1, to))
        else Nil

    /** Creates a list containing a range of `BigInt` values, inclusive of the start but exclusive
      * of the end.
      *
      * This method creates a list containing all integers from `from` up to but not including `to`.
      * If `from` is greater than or equal to `to`, an empty list is returned.
      *
      * @param from
      *   The starting value of the range (inclusive).
      * @param to
      *   The ending value of the range (exclusive).
      * @return
      *   A list containing all `BigInt` values from `from` to `to-1`, inclusive.
      * @example
      *   {{{
      *   List.rangeUntil(1, 4) === Cons(BigInt(1), Cons(BigInt(2), Cons(BigInt(3), Nil)))
      *   List.rangeUntil(5, 5) === Nil
      *   List.rangeUntil(5, 3) === Nil
      *   }}}
      */
    def rangeUntil(from: BigInt, to: BigInt): List[BigInt] =
        if from < to then Cons(from, rangeUntil(from + 1, to))
        else Nil

    /** Creates a list by repeating a value a specified number of times.
      *
      * If `times` is less than or equal to 0, an empty list is returned.
      *
      * @param value
      *   The value to repeat in the list.
      * @param times
      *   The number of times to repeat the value.
      * @tparam A
      *   The type of the value to repeat.
      * @return
      *   A list containing the specified value repeated `times` times.
      * @example
      *   {{{
      *   List.fill("a", 3) === Cons("a", Cons("a", Cons("a", Nil)))
      *   List.fill(true, 0) === Nil
      *   List.fill("x", -1) === Nil
      *   }}}
      */
    def fill[A](value: A, times: BigInt): List[A] =
        if 0 < times then Cons(value, fill(value, times - 1))
        else Nil

    /** Combines two lists element-wise using the provided function.
      *
      * The resulting list will have the length of the shorter of the two input lists.
      *
      * @param a
      *   The first list.
      * @param b
      *   The second list.
      * @param f
      *   A function that takes one element from each list and produces a result.
      * @tparam A
      *   The element type of the first list.
      * @tparam B
      *   The element type of the second list.
      * @tparam C
      *   The element type of the resulting list.
      * @return
      *   A list containing the results of applying function `f` to corresponding elements of lists
      *   `a` and `b`.
      * @example
      *   {{{
      *   val list1 = Cons(BigInt(1), Cons(BigInt(2), Nil))
      *   val list2 = Cons(BigInt(3), Cons(BigInt(4), Nil))
      *   List.map2(list1, list2)(_ + _) === Cons(BigInt(4), Cons(BigInt(6), Nil))
      *   List.map2(List.empty[BigInt], list1)(_ + _) === Nil
      *   }}}
      */
    def map2[A, B, C](a: List[A], b: List[B])(f: (A, B) => C): List[C] =
        a match
            case Cons(h1, t1) =>
                b match
                    case Cons(h2, t2) => Cons(f(h1, h2), map2(t1, t2)(f))
                    case Nil          => Nil
            case Nil => Nil

    /** Provides a `ToData` instance for `List[A]` where value type is instances of `ToData`. */
    given listToData[A: ToData]: ToData[scalus.prelude.List[A]] =
        (a: scalus.prelude.List[A]) => {
            def loop(a: scalus.prelude.List[A]): scalus.builtin.BuiltinList[Data] =
                a match
                    case scalus.prelude.List.Nil => mkNilData()
                    case scalus.prelude.List.Cons(head, tail) =>
                        mkCons(summon[ToData[A]](head), loop(tail))

            listData(loop(a))
        }

    given listPairToData[A: ToData, B: ToData]
        : ToData[scalus.prelude.List[scalus.builtin.BuiltinPair[A, B]]] =
        (a: scalus.prelude.List[scalus.builtin.BuiltinPair[A, B]]) => {
            def loop(
                a: scalus.prelude.List[scalus.builtin.BuiltinPair[A, B]]
            ): scalus.builtin.BuiltinList[BuiltinPair[Data, Data]] =
                a match
                    case scalus.prelude.List.Nil => scalus.builtin.Builtins.mkNilPairData()
                    case scalus.prelude.List.Cons(head, tail) =>
                        mkCons(
                          mkPairData(summon[ToData[A]](head.fst), summon[ToData[B]](head.snd)),
                          loop(tail)
                        )

            mapData(loop(a))
        }

    /** Provides a `FromData` instance for `List[A]` where value type is instances of `FromData`. */
    given listFromData[A: FromData]: FromData[scalus.prelude.List[A]] = (d: Data) =>
        def loop(ls: scalus.builtin.BuiltinList[Data]): scalus.prelude.List[A] =
            if ls.isEmpty then List.Nil
            else List.Cons(fromData[A](ls.head), loop(ls.tail))
        loop(unListData(d))

    @Ignore
    given listPairsFromData[A: FromData, B: FromData]
        : FromData[scalus.prelude.List[scalus.builtin.BuiltinPair[A, B]]] =
        (d: Data) =>
            def loop(
                ls: scalus.builtin.BuiltinList[BuiltinPair[Data, Data]]
            ): scalus.prelude.List[scalus.builtin.BuiltinPair[A, B]] =
                if ls.isEmpty then List.Nil
                else
                    List.Cons(
                      scalus.builtin
                          .BuiltinPair(fromData[A](ls.head.fst), fromData[B](ls.head.snd)),
                      loop(ls.tail)
                    )
            loop(unMapData(d))

    /** Provides an `Eq` instance for `List[A]` where value type is instances of `Eq`. */
    given listEq[A: Eq]: Eq[List[A]] = (lhs: List[A], rhs: List[A]) =>
        lhs match
            case Nil =>
                rhs match
                    case Nil        => true
                    case Cons(_, _) => false
            case Cons(headLhs, tailLhs) =>
                rhs match
                    case Nil                    => false
                    case Cons(headRhs, tailRhs) => headLhs === headRhs && tailLhs === tailRhs

    /** Provides an `Ord` instance for `List[A]` where value type is instances of `Ord`. */
    given listOrd[A: Ord]: Ord[List[A]] = (lhs: List[A], rhs: List[A]) =>
        lhs match
            case Nil =>
                rhs match
                    case Nil        => Order.Equal
                    case Cons(_, _) => Order.Less
            case Cons(headLhs, tailLhs) =>
                rhs match
                    case Nil => Order.Greater
                    case Cons(headRhs, tailRhs) =>
                        val order = headLhs <=> headRhs
                        if order.nonEqual then order else tailLhs <=> tailRhs

    given showList[T: Show]: Show[List[T]] = (xs: List[T]) => {
        @tailrec
        def go(start: String, rest: List[T]): String = rest match {
            case List.Nil => start
            case List.Cons(head, tail) =>
                go(appendString(appendString(start, head.show), ", "), tail)
        }
        appendString("[", appendString(go("", xs), "]"))
    }

    extension [A](elem: A) {

        /** Alias on prepended */
        inline def +:[B >: A](list: List[B]): List[B] = list.prepended(elem)
    }

    extension [A: Ord](self: List[A]) {

        /** Sorts the list using the quicksort algorithm.
          *
          * This method sorts the elements of the list in ascending order based on the provided
          * `Ord[A]` instance.
          *
          * @return
          *   A new list containing the elements of the original list sorted in ascending order.
          * @example
          *   {{{
          *   List(BigInt(3), BigInt(1), BigInt(2)).quicksort === Cons(BigInt(1), Cons(BigInt(2), Cons(BigInt(3), Nil)))
          *   List.empty[BigInt].quicksort === Nil
          *   }}}
          */
        def quicksort: List[A] =
            self match
                case List.Nil => List.Nil
                case List.Cons(head, tail) =>
                    val before = tail.filter { elem => (elem <=> head).isLess }.quicksort
                    val after = tail.filter { elem => !(elem <=> head).isLess }.quicksort
                    before ++ after.prepended(head)
    }

    extension [A](self: List[List[A]]) {

        /** Flattens a list of lists into a single list.
          *
          * This method concatenates all inner lists into a single list.
          *
          * @return
          *   A new list containing all elements from the inner lists.
          * @example
          *   {{{
          *   val list = Cons(Cons(BigInt(1), Cons(BigInt(2), Nil)), Cons(Cons(BigInt(3), Cons(BigInt(4), Nil)), Nil))
          *   list.flatten === Cons(BigInt(1), Cons(BigInt(2), Cons(BigInt(3), Cons(BigInt(4), Nil))))
          *   List.empty[List[BigInt]].flatten === Nil
          *   }}}
          */
        def flatten: List[A] = self.foldRight(List.empty[A]) { (innerList, acc) =>
            innerList ++ acc
        }
    }

    extension [A](self: List[A]) {

        /** Alias for at */
        inline def !!(idx: BigInt): A = at(idx)

        /** Checks if the list is empty.
          *
          * @return
          *   `true` if the list is empty, `false` otherwise.
          * @example
          *   {{{
          *   List.empty[BigInt].isEmpty   === true
          *   Cons(BigInt(1), Nil).isEmpty === false
          *   }}}
          */
        def isEmpty: Boolean = self match
            case Nil        => true
            case Cons(_, _) => false

        /** Checks if the list is not empty.
          *
          * @return
          *   `true` if the list contains at least one element, `false` otherwise.
          * @example
          *   {{{
          *   List.empty[BigInt].nonEmpty    === false
          *   Cons(BigInt(1), Nil).nonEmpty  === true
          *   }}}
          */
        inline def nonEmpty: Boolean = !isEmpty

        /** Checks if the list contains an element at the specified index.
          *
          * @param index
          *   The zero-based BigInt index to check.
          * @return
          *   `true` if the list contains an element at the specified index, `false` otherwise.
          * @example
          *   {{{
          *   List.empty[BigInt].isDefinedAt(BigInt(0)) === false
          *
          *   val list: List[BigInt] = Cons(BigInt(1), Cons(BigInt(2), Cons(BigInt(3), Nil)))
          *   list.isDefinedAt(BigInt(1)) === true
          *   list.isDefinedAt(BigInt(3)) === false
          *   list.isDefinedAt(BigInt(-1)) === false
          *   }}}
          */
        def isDefinedAt(index: BigInt): Boolean = get(index).isDefined

        /** Retrieves the element at the specified index in the list.
          *
          * @param index
          *   The zero-based BigInt index of the element to retrieve.
          * @return
          *   The element at the specified index.
          * @throws `NoSuchElementException`
          *   if the index is out of bounds.
          * @example
          *   {{{
          *   List.empty[BigInt].at(BigInt(0)) // throws NoSuchElementException
          *
          *   val list: List[BigInt] = Cons(BigInt(1), Cons(BigInt(2), Cons(BigInt(3), Nil)))
          *   list.at(BigInt(1)) === BigInt(2)
          *   list.at(BigInt(3)) // throws NoSuchElementException
          *   list.at(BigInt(-1)) // throws NoSuchElementException
          *   }}}
          */
        def at(index: BigInt): A = get(index).getOrFail("Index out of bounds in List.at")

        /** Retrieves the element at the specified index in the list.
          *
          * @param index
          *   The zero-based index of the element to retrieve.
          * @return
          *   An `Option` containing the element at the specified index, or `None` if the index is
          *   out of bounds.
          * @example
          *   {{{
          *   List.empty[BigInt].get(0) === None
          *
          *   val list: List[BigInt] = Cons(BigInt(1), Cons(BigInt(2), Cons(BigInt(3), Nil)))
          *   list.get(1) === Some(BigInt(2))
          *   list.get(3) === None
          *   list.get(-1) === None
          *   }}}
          */
        def get(index: BigInt): Option[A] = {
            if index < 0 then None
            else
                @tailrec
                def go(lst: List[A], currentIndex: BigInt): Option[A] = lst match
                    case Nil => None
                    case Cons(head, tail) =>
                        if currentIndex == index then Some(head)
                        else go(tail, currentIndex + 1)

                go(self, 0)
        }

        /** Checks if the list contains the specified element.
          *
          * @param elem
          *   The element to check for in the list.
          * @tparam B
          *   The type of the element, which must be a supertype of `A`.
          * @return
          *   `true` if the list contains the element, `false` otherwise.
          * @example
          *   {{{
          *   List.empty[BigInt].contains(BigInt(2)) === false
          *
          *   val list: List[BigInt] = Cons(BigInt(1), Cons(BigInt(2), Cons(BigInt(3), Nil)))
          *   list.contains(BigInt(2)) === true
          *   list.contains(BigInt(4)) === false
          *   list.contains(BigInt(-1)) === false
          *   }}}
          */
        def contains[B >: A](elem: B)(using eq: Eq[B]): Boolean = find(_ === elem).isDefined

        /** Groups the elements of this list by the keys returned by the specified function.
          *
          * @param keyExtractor
          *   A function that extracts the key from each element.
          * @tparam K
          *   The type of the keys.
          * @return
          *   An `SortedMap` mapping each key to a list of elements that have that key.
          * @example
          *   {{{
          *   List.empty[BigInt].groupBy(_ % 2) === SortedMap.empty
          *
          *   val list: List[BigInt] = Cons(1, Cons(2, Cons(3, Cons(4, Nil))))
          *   list.groupBy(_ % 2) === SortedMap.fromList((BigInt(0), Cons(2, Cons(4, Nil))), (BigInt(1), Cons(1, Cons(3, Nil))))
          *   }}}
          */
        def groupBy[K: Ord](keyExtractor: A => K): SortedMap[K, List[A]] =
            groupMap(keyExtractor)(identity)

        /** Groups the elements of this list by the keys returned by the key extractor function and
          * transforms each element using the value extractor function.
          *
          * @param keyExtractor
          *   A function that extracts the key from each element.
          * @param valueExtractor
          *   A function that transforms each element before collecting into groups.
          * @tparam K
          *   The type of the keys.
          * @tparam B
          *   The type of the transformed elements.
          * @return
          *   An `SortedMap` mapping each key to a list of transformed elements that have that key.
          * @example
          *   {{{
          *   List.empty[BigInt].groupMap(_ % 2)(_ * 2) === SortedMap.empty
          *
          *   val list: List[BigInt] = Cons(1, Cons(2, Cons(3, Cons(4, Nil))))
          *   list.groupMap(_ % 2)(_ * 2) === SortedMap.fromList((BigInt(0), Cons(4, Cons(8, Nil))), (BigInt(1), Cons(2, Cons(6, Nil))))
          *   }}}
          */
        def groupMap[K: Ord, B](
            keyExtractor: A => K
        )(valueExtractor: A => B): SortedMap[K, List[B]] = {
            @tailrec
            def go(list: List[A], acc: SortedMap[K, List[B]]): SortedMap[K, List[B]] =
                list match
                    case Nil => acc
                    case Cons(head, tail) =>
                        val key = keyExtractor(head)
                        val value = valueExtractor(head)
                        acc.get(key) match
                            case None =>
                                val newAcc = acc.insert(key, List.single(value))
                                go(tail, newAcc)
                            case Some(lst) =>
                                val newLst = lst.prepended(value)
                                val newAcc = acc.insert(key, newLst)
                                go(tail, newAcc)

            go(self, SortedMap.empty).mapValues { _.reverse }
        }

        /** Groups elements by the keys returned by the key extractor function, transforms each
          * element using the value extractor function, and combines values with the same key using
          * the reducer function.
          *
          * @param keyExtractor
          *   A function that extracts the key from each element.
          * @param valueExtractor
          *   A function that transforms each element before reduction.
          * @param reducer
          *   A function that combines two values with the same key.
          * @tparam K
          *   The type of the keys.
          * @tparam B
          *   The type of the transformed elements.
          * @return
          *   An `SortedMap` mapping each key to the reduced value of all elements with that key.
          * @example
          *   {{{
          *   List.empty[BigInt].groupMapReduce(_ % 2)(identity)(_ + _) === SortedMap.empty
          *
          *   val list: List[BigInt] = Cons(1, Cons(2, Cons(3, Cons(4, Nil))))
          *   list.groupMapReduce(_ % 2)(identity)(_ + _) === SortedMap.fromList((BigInt(0), BigInt(6)), (BigInt(1), BigInt(4)))
          *   }}}
          */
        def groupMapReduce[K: Ord, B](
            keyExtractor: A => K
        )(valueExtractor: A => B)(reducer: (B, B) => B): SortedMap[K, B] = {
            @tailrec
            def go(list: List[A], acc: SortedMap[K, B]): SortedMap[K, B] =
                list match
                    case Nil => acc
                    case Cons(head, tail) =>
                        val key = keyExtractor(head)
                        val value = valueExtractor(head)
                        acc.get(key) match
                            case None =>
                                val newAcc = acc.insert(key, value)
                                go(tail, newAcc)
                            case Some(oldValue) =>
                                val newValue = reducer(oldValue, value)
                                val newAcc = acc.insert(key, newValue)
                                go(tail, newAcc)

            go(self, SortedMap.empty)
        }

        /** Zips this list with another list, producing a list of pairs.
          *
          * The resulting list will have the length of the shorter of the two lists.
          *
          * @param other
          *   The other list to zip with.
          * @tparam B
          *   The type of elements in the other list.
          * @return
          *   A list of pairs, where each pair contains an element from this list and an element
          *   from the other list at the same index.
          * @example
          *   {{{
          *   List(BigInt(1), BigInt(2)).zip(List(BigInt(3), BigInt(4))) === Cons((BigInt(1), BigInt(3)), Cons((BigInt(2), BigInt(4)), Nil))
          *   List.empty[BigInt].zip(List(BigInt(1), BigInt(2))) === Nil
          *   }}}
          */
        def zip[B](other: List[B]): List[(A, B)] = self match
            case Nil => Nil
            case Cons(selfHead, selfTail) =>
                other match
                    case Nil => Nil
                    case Cons(otherHead, otherTail) =>
                        Cons((selfHead, otherHead), selfTail.zip(otherTail))

        /** Prepends an element to the list.
          *
          * @param elem
          *   The element to prepend.
          * @tparam B
          *   The type of the element, which must be a supertype of `A`.
          * @return
          *   A new list with the element prepended.
          * @example
          *   {{{
          *   List.empty[BigInt].prepended(BigInt(1)) === Cons(BigInt(1), Nil)
          *
          *   val list: List[BigInt] = Cons(BigInt(2), Cons(BigInt(3), Nil))
          *   list.prepended(BigInt(1)) === Cons(BigInt(1), Cons(BigInt(2), Cons(BigInt(3), Nil)))
          *   }}}
          */
        inline def prepended[B >: A](elem: B): List[B] = Cons(elem, self)

        /** Prepends all elements of another list to this list.
          *
          * @param other
          *   The list whose elements will be prepended.
          * @tparam B
          *   The type of the elements in the other list, which must be a supertype of `A`.
          * @return
          *   A new list with all elements of `other` prepended to this list.
          * @example
          *   {{{
          *   List.empty[BigInt].prependedAll(List(BigInt(1), BigInt(2))) === Cons(BigInt(1), Cons(BigInt(2), Nil))
          *
          *   val list: List[BigInt] = Cons(BigInt(3), Nil)
          *   list.prependedAll(List(BigInt(1), BigInt(2))) === Cons(BigInt(1), Cons(BigInt(2), Cons(BigInt(3), Nil)))
          *   }}}
          */
        inline def prependedAll[B >: A](other: List[B]): List[B] = {
            def go(lst: List[B]): List[B] = lst match
                case Nil              => self
                case Cons(head, tail) => Cons(head, go(tail))

            if self.nonEmpty then go(other) else other
        }

        /** Alias on prependedAll */
        inline def ++:[B >: A](other: List[B]): List[B] = other.prependedAll(self)

        /** Appends an element to the end of the list.
          *
          * @param elem
          *   The element to append.
          * @tparam B
          *   The type of the element, which must be a supertype of `A`.
          * @return
          *   A new list with the element appended.
          * @example
          *   {{{
          *   List.empty[BigInt].appended(BigInt(1)) === Cons(BigInt(1), Nil)
          *
          *   val list: List[BigInt] = Cons(BigInt(1), Cons(BigInt(2), Nil))
          *   list.appended(BigInt(3)) === Cons(BigInt(1), Cons(BigInt(2), Cons(BigInt(3), Nil)))
          *   }}}
          */
        def appended[B >: A](elem: B): List[B] = self match
            case Nil              => List.single(elem)
            case Cons(head, tail) => Cons(head, tail.appended(elem))

        /** Alias for appended. */
        inline def :+[B >: A](elem: B): List[B] = appended(elem)

        /** Appends all elements of another list to this list.
          *
          * @param other
          *   The list whose elements will be appended.
          * @tparam B
          *   The type of the elements in the other list, which must be a supertype of `A`.
          * @return
          *   A new list with all elements of `other` appended to this list.
          * @example
          *   {{{
          *   List.empty[BigInt].appendedAll(List(BigInt(1), BigInt(2))) === Cons(BigInt(1), Cons(BigInt(2), Nil))
          *
          *   val list: List[BigInt] = Cons(BigInt(1), Cons(BigInt(2), Nil))
          *   list.appendedAll(List(BigInt(3), BigInt(4))) === Cons(BigInt(1), Cons(BigInt(2), Cons(BigInt(3), Cons(BigInt(4), Nil))))
          *   }}}
          */
        def appendedAll[B >: A](other: List[B]): List[B] = other.prependedAll(self)

        /** Alias for appendedAll. */
        inline def :++[B >: A](other: List[B]): List[B] = appendedAll(other)

        /** Alias for appendedAll. */
        inline def concat[B >: A](other: List[B]): List[B] = appendedAll(other)

        /** Alias for appendedAll. */
        inline def ++[B >: A](other: List[B]): List[B] = concat(other)

        /** Applies a function to each element of the list, producing a new list with the results.
          *
          * @param mapper
          *   A function that takes an element of type `A` and returns a value of type `B`.
          * @return
          *   A new list where each element is the result of applying `mapper` to the corresponding
          *   element of the original list.
          * @example
          *   {{{
          *   List.empty[BigInt].map(_ * 2) === Nil
          *
          *   val list: List[BigInt] = Cons(BigInt(1), Cons(BigInt(2), Cons(BigInt(3), Nil)))
          *   val result = list.map(_ * 2)
          *   result === Cons(BigInt(2), Cons(BigInt(4), .Cons(BigInt(6), Nil)))
          *   }}}
          */
        def map[B](mapper: A => B): List[B] =
            foldRight(List.empty[B]) { (head, tail) => Cons(mapper(head), tail) }

        /** Applies a function to each element of the list, producing a new list with the results.
          *
          * This method is similar to `map`, but it allows the function to return a list for each
          * element, effectively flattening the result.
          *
          * @param mapper
          *   A function that takes an element of type `A` and returns a list of type `List[B]`.
          * @return
          *   A new list containing all elements produced by applying `mapper` to each element of
          *   the original list and flattening the results.
          * @example
          *   {{{
          *   List.empty[BigInt].flatMap(x => List(x, x + 1)) === Nil
          *
          *   val list: List[BigInt] = Cons(BigInt(1), Cons(BigInt(2), Nil))
          *   val result = list.flatMap(x => List(x, x + 1))
          *   result === Cons(BigInt(1), Cons(BigInt(2), Cons(BigInt(2), Cons(BigInt(3), Nil))))
          *   }}}
          */
        def flatMap[B](mapper: A => List[B]): List[B] =
            foldRight(List.empty[B]) { (head, tail) => mapper(head) ++ tail }

        /** Filters the elements of the list based on a predicate.
          *
          * @param predicate
          *   A function that takes an element of type `A` and returns `true` if the element should
          *   be included in the resulting list, or `false` otherwise.
          * @return
          *   A new list containing only the elements that satisfy the predicate.
          * @example
          *   {{{
          *   List.empty[BigInt].filter(_ % 2 == 1) === Nil
          *
          *   val list: List[BigInt] = Cons(BigInt(1), Cons(BigInt(2), Cons(BigInt(3), Nil)))
          *   val filtered = list.filter(_ % 2 == 1)
          *   filtered === Cons(BigInt(1), Cons(BigInt(3), Nil))
          *   }}}
          */
        def filter(predicate: A => Boolean): List[A] =
            foldRight(List.empty[A]) { (head, tail) =>
                if predicate(head) then Cons(head, tail) else tail
            }

        /** Filters the elements of the list based on a predicate that returns `false` for elements
          * to be excluded.
          *
          * @param predicate
          *   A function that takes an element of type `A` and returns `true` if the element should
          *   be excluded from the resulting list, or `false` otherwise.
          * @return
          *   A new list containing only the elements that do not satisfy the predicate.
          * @example
          *   {{{
          *   List.empty[BigInt].filterNot(_ % 2 == 1) === Nil
          *
          *   val list: List[BigInt] = Cons(BigInt(1), Cons(BigInt(2), Cons(BigInt(3), Nil)))
          *   val filtered = list.filterNot(_ % 2 == 1)
          *   filtered === Cons(BigInt(2), Nil)
          *   }}}
          */
        def filterNot(predicate: A => Boolean): List[A] = filter(!predicate(_))

        /** Filters the elements of the list based on a predicate that returns an `Option[B]`.
          *
          * If the predicate returns `None`, the element is excluded from the resulting list. If it
          * returns `Some(value)`, the value is included in the resulting list.
          *
          * @param predicate
          *   A function that takes an element of type `A` and returns an `Option[B]`.
          * @tparam B
          *   The type of the values to be included in the resulting list.
          * @return
          *   A new list containing only the elements for which the predicate returned
          *   `Some(value)`.
          * @example
          *   {{{
          *   List.empty[BigInt].filterMap(x => if x % 2 == 1 then Some(x) else None) === Nil
          *
          *   val list: List[BigInt] = Cons(BigInt(1), Cons(BigInt(2), Cons(BigInt(3), Nil)))
          *   val filtered = list.filterMap(x => if x % 2 == 1 then Some(x) else None)
          *   filtered === Cons(BigInt(1), Cons(BigInt(3), Nil))
          *   }}}
          */
        def filterMap[B](predicate: A => Option[B]): List[B] =
            foldRight(List.empty[B]) { (head, tail) =>
                predicate(head) match
                    case None        => tail
                    case Some(value) => Cons(value, tail)
            }

        /** Finds the first element in the list that satisfies the given predicate.
          *
          * @param predicate
          *   A function that takes an element of type `A` and returns `true` if the element matches
          *   the condition.
          * @return
          *   An `Option` containing the first element that satisfies the predicate, or `None` if no
          *   such element exists.
          * @example
          *   {{{
          *   List.empty[BigInt].find(_ > 1) === None
          *
          *   val list: List[BigInt] = Cons(BigInt(1), Cons(BigInt(2), Cons(BigInt(3), Nil)))
          *   list.find(_ > 1) === Some(BigInt(2))
          *   list.find(_ > 3) === None
          *   }}}
          */
        @tailrec
        def find(predicate: A => Boolean): Option[A] = self match
            case Nil              => None
            case Cons(head, tail) => if predicate(head) then Some(head) else tail.find(predicate)

        /** Finds the first element in the list that, when passed to the provided mapper function
          * returns non-`None` or `None` otherwise.
          *
          * @param mapper
          *   A function that takes an element of type `A` and returns an `Option[B]`.
          * @return
          *   An `Option[B]` containing the first non-`None` result from applying the mapper to the
          *   elements of the list, or `None` if no such element exists.
          * @example
          *   {{{
          *   List.empty[String].findMap(str => if str.length >= 3 then Some(BigInt(str.length)) else None) === None
          *
          *   val list: List[String] = Cons("a", Cons("bb", Cons("ccc", Nil)))
          *   list.findMap(str => if str.length >= 2 then Some(BigInt(str.length)) else None) === Some(BigInt(2))
          *   list.findMap(str => if str.length >= 4 then Some(BigInt(str.length)) else None) === None
          *   }}}
          */
        @tailrec
        def findMap[B](mapper: A => Option[B]): Option[B] = self match
            case Nil => None
            case Cons(head, tail) =>
                val result = mapper(head)
                if result.isDefined then result else tail.findMap(mapper)

        /** Performs a left fold on the list.
          *
          * @param init
          *   The initial value to start the fold with.
          * @param combiner
          *   A function that combines the accumulated value and the current element.
          * @tparam B
          *   The type of the accumulated value.
          * @return
          *   The result of applying the combiner function to all elements of the list, starting
          *   with the initial value.
          * @example
          *   {{{
          *   List.empty[BigInt].foldLeft(BigInt(0))(_ + _) === BigInt(0)
          *
          *   val list: List[BigInt] = Cons(BigInt(1), Cons(BigInt(2), Cons(BigInt(3), Nil)))
          *   val sum = list.foldLeft(BigInt(0))(_ + _)
          *   sum === BigInt(6)
          *   }}}
          */
        @tailrec
        def foldLeft[B](init: B)(combiner: (B, A) => B): B = self match
            case Nil              => init
            case Cons(head, tail) => tail.foldLeft(combiner(init, head))(combiner)

        /** Performs a right fold on the list.
          * @param init
          *   The initial value to start the fold with.
          * @param combiner
          *   A function that combines the current element and the accumulated value.
          * @param B
          *   The type of the accumulated value.
          * @return
          *   The result of applying the combiner function to all elements of the list, starting
          *   with the initial value.
          * @example
          *   {{{
          *   List.empty[BigInt].foldRight(BigInt(0))(_ + _) === BigInt(0)
          *
          *   val list: List[BigInt] = Cons(BigInt(1), Cons(BigInt(2), Cons(BigInt(3), Nil)))
          *   val sum = list.foldRight(BigInt(0))(_ + _)
          *   sum === BigInt(6)
          *   }}}
          */
        def foldRight[B](init: B)(combiner: (A, B) => B): B = self match
            case Nil              => init
            case Cons(head, tail) => combiner(head, tail.foldRight(init)(combiner))

        /** Checks if the list contains an element that satisfies the given predicate.
          * @param predicate
          *   A function that takes an element of type `A` and returns `true` if the element matches
          *   the condition.
          * @return
          *   `true` if there is at least one element in the list that satisfies the predicate,
          *   `false` otherwise.
          * @example
          *   {{{
          *   List.empty[BigInt].exists(_ > 1) === false
          *
          *   val list: List[BigInt] = Cons(BigInt(1), Cons(BigInt(2), Cons(BigInt(3), Nil)))
          *   list.exists(_ > 1) === true
          *   list.exists(_ > 3) === false
          *   }}}
          */
        def exists(predicate: A => Boolean): Boolean = find(predicate).isDefined

        /** Checks if all elements in the list satisfy the given predicate.
          *
          * @param predicate
          *   A function that takes an element of type `A` and returns `true` if the element matches
          *   the condition.
          * @return
          *   `true` if all elements in the list satisfy the predicate, `false` otherwise.
          * @example
          *   {{{
          *   List.empty[BigInt].forall(_ > 1) === true
          *
          *   val list: List[BigInt] = Cons(BigInt(1), Cons(BigInt(2), Cons(BigInt(3), Nil)))
          *   list.forall(_ > 0) === true
          *   list.forall(_ > 2) === false
          *   }}}
          */
        @tailrec
        def forall(predicate: A => Boolean): Boolean = self match
            case Nil              => true
            case Cons(head, tail) => if predicate(head) then tail.forall(predicate) else false

        /** Counts the number of elements in the list that satisfy the given predicate.
          * @param p
          *   A function that takes an element of type `A` and returns `true` if the element matches
          *   the condition.
          * @return
          *   The number of elements in the list that satisfy the predicate as a `BigInt`.
          * @example
          *   {{{
          *   List.empty[BigInt].count(_ > 1) === BigInt(0)
          *
          *   val list: List[BigInt] = Cons(BigInt(1), Cons(BigInt(2), Cons(BigInt(3), Nil)))
          *   list.count(_ > 1) === BigInt(2)
          *   }}}
          */
        def count(p: A => Boolean): BigInt = foldLeft(BigInt(0)) { (counter, elem) =>
            if p(elem) then counter + 1 else counter
        }

        /** Finds the index of the first occurrence of the specified element in the list.
          *
          * @param elem
          *   The element to search for in the list.
          * @tparam B
          *   The type of the element being searched for, which must be a supertype of `A`.
          * @return
          *   The index of the first occurrence of the element, or BigInt(-1) if the element is not
          *   found.
          * @example
          *   {{{
          *   List.empty[BigInt].indexOf(BigInt(2)) === BigInt(-1)
          *
          *   val list: List[BigInt] = Cons(BigInt(1), Cons(BigInt(2), Cons(BigInt(3), Nil)))
          *   list.indexOf(BigInt(2)) === BigInt(1)
          *   list.indexOf(BigInt(4)) === BigInt(-1)
          *   list.indexOf(BigInt(-1)) === BigInt(-1)
          *   }}}
          */
        def indexOf[B >: A](elem: B)(using eq: Eq[B]): BigInt = indexOfOption(elem).getOrElse(-1)

        /** Finds the index of the first occurrence of the specified element in the list.
          *
          * @param elem
          *   The element to search for in the list.
          * @tparam B
          *   The type of the element being searched for, which must be a supertype of `A`.
          * @return
          *   An `Option` containing the index of the first occurrence of the element, or `None` if
          *   the element is not found.
          * @example
          *   {{{
          *   List.empty[BigInt].indexOfOption(BigInt(2)) === None
          *
          *   val list: List[BigInt] = Cons(BigInt(1), Cons(BigInt(2), Cons(BigInt(3), Nil)))
          *   list.indexOfOption(BigInt(2)) === Some(BigInt(1))
          *   list.indexOfOption(BigInt(4)) === None
          *   list.indexOfOption(BigInt(-1)) === None
          *   }}}
          */
        def indexOfOption[B >: A](elem: B)(using eq: Eq[B]): Option[BigInt] = {
            @tailrec
            def go(lst: List[A], index: BigInt): Option[BigInt] = lst match
                case Nil => None
                case Cons(head, tail) =>
                    if head === elem then Some(index) else go(tail, index + 1)

            go(self, BigInt(0))
        }

        /** Returns the last element of the list or throws an exception if the list is empty.
          *
          * @return
          *   The last element of the list.
          * @throws NoSuchElementException
          *   If the list is empty.
          * @example
          *   {{{
          *   val list: List[BigInt] = Cons(BigInt(1), Cons(BigInt(2), Cons(BigInt(3), Nil)))
          *   list.last === BigInt(3)
          *   List.empty[BigInt].last // throw NoSuchElementException
          *   }}}
          */
        def last: A = lastOption.getOrFail("last of empty list")

        /** Returns the last element of the list as an [[Option]].
          *
          * @return
          *   An `Option` containing the last element of the list, or `None` if the list is empty.
          * @example
          *   {{{
          *   List.empty[BigInt].lastOption === None
          *
          *   val list: List[BigInt] = Cons(BigInt(1), Cons(BigInt(2), Cons(BigInt(3), Nil)))
          *   list.lastOption === Some(BigInt(3))
          *   List.empty[BigInt].lastOption === None
          *   }}}
          */
        @tailrec
        def lastOption: Option[A] = self match
            case Nil               => None
            case Cons(value, tail) => if tail.isEmpty then Some(value) else tail.lastOption

        /** Returns the number of elements in the list.
          *
          * @return
          *   The number of elements in the list as a `BigInt`.
          * @example
          *   {{{
          *   List.empty[BigInt].length === BigInt(0)
          *
          *   val list: List[BigInt] = Cons(BigInt(1), Cons(BigInt(2), Cons(BigInt(3), Nil)))
          *   list.length === BigInt(3)
          *   }}}
          */
        def length: BigInt = foldLeft(BigInt(0)) { (counter, _) => counter + 1 }

        /** Alias for `length`. */
        inline def size: BigInt = length

        /** Returns the first element of the list or throws an exception if the list is empty.
          *
          * @return
          *   The first element of the list.
          * @throws NoSuchElementException
          *   If the list is empty.
          * @example
          *   {{{
          *   val list: List[BigInt] = Cons(BigInt(1), Cons(BigInt(2), Cons(BigInt(3), Nil)))
          *   list.head === BigInt(1)
          *   List.empty[BigInt].head // throw NoSuchElementException
          *   }}}
          */
        def head: A = headOption.getOrFail("head of empty list")

        /** Returns the first element of the list as an [[Option]].
          *
          * @return
          *   An `Option` containing the first element of the list, or `None` if the list is empty.
          * @example
          *   {{{
          *   List.empty[BigInt].headOption === None
          *
          *   val list: List[BigInt] = Cons(BigInt(1), Cons(BigInt(2), Cons(BigInt(3), Nil)))
          *   list.headOption === Some(1)
          *   List.empty[BigInt].headOption === None
          *   }}}
          */
        def headOption: Option[A] = {
            self match
                case Nil            => None
                case Cons(value, _) => Some(value)
        }

        /** Returns a list consisting of all elements except the first element of this list or
          * throws an exception if the list is empty.
          *
          * @return
          *   A list containing all elements except the first.
          * @throws NoSuchElementException
          *   If the list is empty.
          * @example
          *   {{{
          *   val list: List[BigInt] = Cons(BigInt(1), Cons(BigInt(2), Cons(BigInt(3), Nil)))
          *   list.tail === Cons(BigInt(2), Cons(BigInt(3), Nil))
          *   List.empty[BigInt].tail // throw NoSuchElementException
          *   }}}
          */
        def tail: List[A] = self match
            case Nil           => throw new NoSuchElementException("tail of empty list")
            case Cons(_, rest) => rest

        /** Returns a list consisting of all elements except the first `skip` elements of this list.
          *
          * @param skip
          *   The number of elements to drop from the beginning of the list
          * @return
          *   A new list with the first `skip` elements removed, or an empty list if `skip` is
          *   greater than or equal to the list's length
          * @example
          *   {{{
          *   List.empty[BigInt].drop(2) === Nil
          *
          *   val list: List[BigInt] = Cons(BigInt(1), Cons(BigInt(2), Cons(BigInt(3), Nil)))
          *   list.drop(2) === Cons(BigInt(3), Nil)
          *   list.drop(0) === list
          *   list.drop(4) === Nil
          *   }}}
          */
        @tailrec
        def drop(skip: BigInt): List[A] =
            if skip <= 0 then self
            else
                self match
                    case Nil           => Nil
                    case Cons(_, tail) => tail.drop(skip - 1)

        /** Returns a list consisting of all elements except the last `skip` elements of this list.
          *
          * @param skip
          *   The number of elements to drop from the end of the list
          * @return
          *   A new list with the last `skip` elements removed, or an empty list if `skip` is
          *   greater than or equal to the list's length
          * @example
          *   {{{
          *   List.empty[BigInt].dropRight(2) === Nil
          *
          *   val list: List[BigInt] = Cons(BigInt(1), Cons(BigInt(2), Cons(BigInt(3), Nil)))
          *   list.dropRight(2) === Cons(BigInt(1), Nil)
          *   list.dropRight(0) === list
          *   list.dropRight(4) === Nil
          *   }}}
          */
        def dropRight(skip: BigInt): List[A] =
            if skip <= 0 then self
            else
                foldRight((List.empty[A], skip)) { (head, acc) =>
                    if acc._2 > 0 then (Nil, acc._2 - 1)
                    else (Cons(head, acc._1), acc._2)
                }._1

        /** Drops elements from the beginning of the list as long as they satisfy the predicate.
          *
          * @param predicate
          *   A function that takes an element and returns `true` if it should be dropped from the
          *   result
          * @return
          *   A new list containing all elements from the first element that does not satisfy the
          *   predicate until the end of the list
          * @example
          *   {{{
          *   List.empty[BigInt].dropWhile(_ < 3) === Nil
          *
          *   val list: List[BigInt] = Cons(BigInt(1), Cons(BigInt(2), Cons(BigInt(3), Nil)))
          *   list.dropWhile(_ < 3) === Cons(BigInt(3), Nil)
          *   list.dropWhile(_ < 1) === list
          *   list.dropWhile(_ < 4) === Nil
          *   }}}
          */
        def dropWhile(predicate: A => Boolean): List[A] = self match
            case Nil => Nil
            case Cons(head, tail) =>
                if predicate(head) then tail.dropWhile(predicate)
                else self

        /** Deletes the first occurrence of the specified element from the list.
          *
          * @param elem
          *   The element to delete from the list
          * @param eq
          *   An instance of `Eq[B]` used to compare elements for equality
          * @tparam B
          *   The type of the element being deleted, which must be a supertype of `A`
          * @return
          *   A new list with the first occurrence of the specified element removed, or the original
          *   list if the element is not found
          * @example
          *   {{{
          *   List.empty[BigInt].deleteFirst(BigInt(2)) === Nil
          *
          *   val list: List[BigInt] = Cons(BigInt(1), Cons(BigInt(2), Cons(BigInt(2), Nil)))
          *   list.deleteFirst(BigInt(2)) === Cons(BigInt(1), Cons(BigInt(2), Nil))
          *   list.deleteFirst(BigInt(3)) === list
          *   }}}
          */
        def deleteFirst[B >: A](elem: B)(using eq: Eq[B]): List[A] = {
            def go(lst: List[A]): List[A] = lst match
                case Nil => Nil
                case Cons(head, tail) =>
                    if head === elem then tail
                    else Cons(head, go(tail))

            go(self)
        }

        /** Takes the first `count` elements from the list.
          *
          * @param count
          *   The number of elements to take from the beginning of the list
          * @return
          *   A new list containing the first `count` elements, or an empty list if `count` is less
          *   than or equal to 0
          * @example
          *   {{{
          *   List.empty[BigInt].take(2) === Nil
          *
          *   val list: List[BigInt] = Cons(BigInt(1), Cons(BigInt(2), Cons(BigInt(3), Nil)))
          *   list.take(2) === Cons(BigInt(1), Cons(BigInt(2), Nil))
          *   list.take(0) === Nil
          *   list.take(4) === list
          *   }}}
          */
        def take(count: BigInt): List[A] =
            if count <= 0 then Nil
            else
                self match
                    case Nil              => Nil
                    case Cons(head, tail) => Cons(head, tail.take(count - 1))

        /** Takes the last `count` elements from the list.
          *
          * @param count
          *   The number of elements to take from the end of the list
          * @return
          *   A new list containing the last `count` elements, or an empty list if `count` is less
          *   than or equal to 0
          * @example
          *   {{{
          *   List.empty[BigInt].takeRight(2) === Nil
          *
          *   val list: List[BigInt] = Cons(BigInt(1), Cons(BigInt(2), Cons(BigInt(3), Nil)))
          *   list.takeRight(2) === Cons(BigInt(2), Cons(BigInt(3), Nil))
          *   list.takeRight(0) === Nil
          *   list.takeRight(4) === list
          *   }}}
          */
        def takeRight(count: BigInt): List[A] =
            if count <= 0 then Nil
            else
                foldRight((List.empty[A], BigInt(0))) { (head, acc) =>
                    if acc._2 >= count then acc
                    else (Cons(head, acc._1), acc._2 + 1)
                }._1

        /** Takes elements from the beginning of the list as long as they satisfy the predicate.
          *
          * @param predicate
          *   A function that takes an element and returns `true` if it should be included in the
          *   result
          * @return
          *   A new list containing elements from the beginning of the list until an element is
          *   found that does not satisfy the predicate
          * @example
          *   {{{
          *   List.empty[BigInt].takeWhile(_ < 3) === Nil
          *
          *   val list: List[BigInt] = Cons(BigInt(1), Cons(BigInt(2), Cons(BigInt(3), Nil)))
          *   list.takeWhile(_ < 3) === Cons(BigInt(1), Cons(BigInt(2), Nil))
          *   list.takeWhile(_ < 1) === Nil
          *   list.takeWhile(_ < 4) === list
          *   }}}
          */
        def takeWhile(predicate: A => Boolean): List[A] = self match
            case Nil => Nil
            case Cons(head, tail) =>
                if predicate(head) then Cons(head, tail.takeWhile(predicate))
                else Nil

        /** Returns a new list with duplicate elements removed, keeping only the first occurrence of
          * each element.
          *
          * @param eq
          *   An instance of `Eq[B]` used to compare elements for equality
          * @tparam B
          *   The type of elements being compared, which must be a supertype of `A`
          * @return
          *   A new list containing only distinct elements in their original order
          * @example
          *   {{{
          *   List.empty[BigInt].distinct === Nil
          *
          *   val list: List[BigInt] = Cons(BigInt(1), Cons(BigInt(2), Cons(BigInt(1), Cons(BigInt(3), Nil))))
          *   list.distinct === Cons(BigInt(1), Cons(BigInt(2), Cons(BigInt(3), Nil)))
          *   }}}
          */
        def distinct[B >: A](using eq: Eq[B]): List[A] =
            foldLeft(List.empty[A]) { (acc, elem) =>
                if acc.exists(_ === elem) then acc
                else Cons(elem, acc)
            }.reverse

        /** Returns a new list containing elements from this list that do not appear in the other
          * list.
          *
          * @param other
          *   The list whose elements should be removed from this list
          * @param eq
          *   An instance of `Eq[B]` used to compare elements for equality
          * @tparam B
          *   The type of elements in the other list, which must be a supertype of `A`
          * @return
          *   A new list containing elements from this list that do not appear in the other list
          * @example
          *   {{{
          *   List.empty[BigInt].diff(List(BigInt(1), BigInt(2))) === Nil
          *
          *   val list: List[BigInt] = Cons(BigInt(1), Cons(BigInt(2), Cons(BigInt(3), Nil)))
          *   list.diff(List(BigInt(2))) === Cons(BigInt(1), Cons(BigInt(3), Nil))
          *   list.diff(Nil) === list
          *   list.diff(list) === Nil
          *   }}}
          */
        @tailrec
        def diff[B >: A](other: List[B])(using eq: Eq[B]): List[A] = {
            if isEmpty then Nil
            else
                other match
                    case Nil              => self
                    case Cons(head, tail) => (deleteFirst(head): List[A]).diff(tail)
        }

        /** Returns a new list containing all elements except the last element of this list or
          * throws an exception if the list is empty.
          *
          * @return
          *   A new list containing all elements except the last.
          * @throws NoSuchElementException
          *   If the list is empty.
          * @example
          *   {{{
          *   val list: List[BigInt] = Cons(BigInt(1), Cons(BigInt(2), Cons(BigInt(3), Nil)))
          *   list.init === Cons(BigInt(1), Cons(BigInt(2), Nil))
          *   List.empty[BigInt].init // throw NoSuchElementException
          *   }}}
          */
        inline def init: List[A] =
            if isEmpty then throw new NoSuchElementException("init of empty list") else dropRight(1)

        /** Returns a new list with elements in reverse order.
          *
          * @return
          *   A new list containing the same elements but in reverse order.
          * @example
          *   {{{
          *   List.empty[BigInt].reverse === Nil
          *
          *   val list: List[BigInt] = Cons(BigInt(1), Cons(BigInt(2), Cons(BigInt(3), Nil)))
          *   list.reverse === Cons(BigInt(3), Cons(BigInt(2), Cons(BigInt(1), Nil)))
          *   }}}
          */
        def reverse: List[A] = foldLeft(List.empty[A]) { (acc, elem) => Cons(elem, acc) }

        /** Applies the given function to each element of the list.
          *
          * @param f
          *   The function to apply to each element.
          * @example
          *   {{{
          *   }}}
          */
        @tailrec
        def foreach(f: A => Unit): Unit = self match
            case Nil              => ()
            case Cons(head, tail) => f(head); tail.foreach(f)

        /** Converts the list to a Scala sequence (`scala.Seq`).
          *
          * This method is only available offchain.
          *
          * @return
          *   A `scala.Seq[A]` containing all the elements from this list in the same order.
          * @example
          *   {{{
          *   List.empty[BigInt].asScala === scala.Seq.empty[BigInt]
          *
          *   val list: List[BigInt] = Cons(BigInt(1), Cons(BigInt(2), Cons(BigInt(3), Nil)))
          *   list.asScala === scala.Seq(BigInt(1), BigInt(2), BigInt(3))
          *   }}}
          */
        @Ignore
        def asScala: scala.Seq[A] =
            val buf = mutable.ListBuffer.empty[A]
            for e <- self do buf.addOne(e)
            buf.toList

        /** Converts this list to a [[scala.collection.immutable.List]].
          *
          * This method allows you to convert a `List[A]` to a Scala standard library
          * `scala.collection.immutable.List[A]`. This is an offchain-only operation.
          *
          * @return
          *   A `scala.collection.immutable.List[A]` containing all the elements from this list in
          *   the same order.
          */
        @Ignore
        def toScalaList: scala.collection.immutable.List[A] =
            val buf = mutable.ListBuffer.empty[A]
            for e <- self do buf.addOne(e)
            buf.toList
    }
}
