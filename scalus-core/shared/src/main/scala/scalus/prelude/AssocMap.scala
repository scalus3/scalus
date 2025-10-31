package scalus.prelude

import scalus.Compile
import scalus.builtin.Builtins.*
import scalus.builtin.Data.fromData
import scalus.builtin.{Data, FromData, ToData}
import scala.annotation.tailrec

case class AssocMap[A, B](toList: List[(A, B)])

@Compile
object AssocMap {
    import List.*
    import Option.*

    def empty[A, B]: AssocMap[A, B] = AssocMap(List.empty[(A, B)])
    def singleton[A, B](key: A, value: B): AssocMap[A, B] = AssocMap(List.single((key, value)))
    inline def unsafeFromList[A, B](lst: List[(A, B)]): AssocMap[A, B] = AssocMap(lst)

    def fromList[A: Eq, B](lst: List[(A, B)]): AssocMap[A, B] = AssocMap(
      lst.foldLeft(List.empty) { (acc, elem) =>
          if acc.exists(_._1 === elem._1) then acc
          else Cons(elem, acc)
      }
    )

    given AssocMapFromData[A: FromData: Eq, B: FromData]: FromData[AssocMap[A, B]] =
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
            AssocMap.unsafeFromList(loop(unMapData(d)))

    given assocMapToData[A: ToData, B: ToData]: ToData[AssocMap[A, B]] =
        (a: AssocMap[A, B]) => {
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

    extension [A, B](self: AssocMap[A, B])
        inline def isEmpty: Boolean = self.toList.isEmpty
        inline def nonEmpty: Boolean = self.toList.nonEmpty
        inline def length: BigInt = self.toList.length
        inline def size: BigInt = length
        def keys: List[A] = self.toList.map { case (k, _) => k }
        def values: List[B] = self.toList.map { case (_, v) => v }
        def mapValues[C](f: B => C): AssocMap[A, C] = AssocMap(self.toList.map((k, v) => (k, f(v))))
        def forall(f: ((A, B)) => Boolean): Boolean = self.toList.forall(f)
        def exists(f: ((A, B)) => Boolean): Boolean = self.toList.exists(f)

        def filterKeys(predicate: A => Boolean): AssocMap[A, B] = AssocMap(self.toList.filter {
            case (k, _) => predicate(k)
        })

        def filter(predicate: ((A, B)) => Boolean): AssocMap[A, B] =
            AssocMap(self.toList.filter(predicate))

        def filterNot(predicate: ((A, B)) => Boolean): AssocMap[A, B] =
            AssocMap(self.toList.filterNot(predicate))

        def find(predicate: ((A, B)) => Boolean): Option[(A, B)] = {
            @tailrec
            def go(lst: List[(A, B)]): Option[(A, B)] = lst match
                case Nil              => None
                case Cons(pair, tail) =>
                    if predicate(pair) then Some(pair) else go(tail)

            go(self.toList)
        }

        def foldLeft[C](init: C)(combiner: (C, (A, B)) => C): C =
            self.toList.foldLeft(init) { (acc, pair) => combiner(acc, pair) }

        def foldRight[C](init: C)(combiner: ((A, B), C) => C): C =
            self.toList.foldRight(init) { (pair, acc) => combiner(pair, acc) }

    extension [A: Eq, B](self: AssocMap[A, B])
        /** Optionally returns the value associated with a key.
          *
          * @param key
          *   the key value
          * @return
          *   an option value containing the value associated with `key` in this map, or `None` if
          *   none exists.
          */
        def get(key: A): Option[B] = {
            @tailrec
            def go(lst: List[(A, B)]): Option[B] = lst match
                case Nil              => None
                case Cons(pair, tail) =>
                    pair match
                        case (k, v) => if k === key then Some(v) else go(tail)

            go(self.toList)
        }

        def contains(key: A): Boolean = get(key).isDefined

        def insert(key: A, value: B): AssocMap[A, B] = {
            def go(lst: List[(A, B)]): List[(A, B)] = lst match
                case Nil =>
                    Cons((key, value), Nil)
                case Cons(pair, tail) =>
                    pair match
                        case (k, v) =>
                            if k === key then Cons((key, value), tail)
                            else Cons(pair, go(tail))

            AssocMap(go(self.toList))
        }

        def delete(key: A): AssocMap[A, B] = {
            def go(lst: List[(A, B)]): List[(A, B)] = lst match
                case Nil              => Nil
                case Cons(pair, tail) =>
                    pair match
                        case (k, v) =>
                            if k === key then tail else Cons(pair, go(tail))

            AssocMap(go(self.toList))
        }

    def union[A: Eq, B, C](
        lhs: AssocMap[A, B],
        rhs: AssocMap[A, C]
    ): AssocMap[A, These[B, C]] = {
        def go(lst: List[(A, B)]): List[(A, These[B, C])] = lst match
            case Nil              => Nil
            case Cons(pair, tail) =>
                pair match
                    case (k, v) =>
                        val optionR = rhs.get(k)
                        val these = optionR match
                            case None    => These.This(v)
                            case Some(r) => These.These(v, r)
                        Cons((k, these), go(tail))

        val lhs1 = go(lhs.toList) // all left with corresponding right

        val rhsNotInLhs =
            rhs.toList.filter { case (a, c) => !lhs.toList.exists(p => p._1 === a) }

        val rhsThat = rhsNotInLhs.map { case (k, v) => (k, These.That(v)) }
        AssocMap(lhs1.appendedAll(rhsThat))
    }

    given assocMapEq[A: Eq, B: Eq]: Eq[AssocMap[A, B]] =
        (lhs: AssocMap[A, B], rhs: AssocMap[A, B]) =>
            lhs.size === rhs.size && lhs.toList.forall { case (key, lhsValue) =>
                rhs.get(key) match
                    case None           => false
                    case Some(rhsValue) => lhsValue === rhsValue
            }
}
