package scalus.cardano.onchain.plutus.prelude

import scalus.compiler.{Compile, Ignore}
import scalus.uplc.builtin.Builtins.*
import scalus.uplc.builtin.Data.{fromData, FromData, ToData}
import scalus.uplc.builtin.{BuiltinList, BuiltinPair, Data}

/** A list of key-value pairs that stays in the UPLC `BuiltinPair` representation, avoiding costly
  * per-element conversions between `PairData` and `ConstrData` that occur when using `List[(A, B)]`
  * with generic `map`/`filter`.
  *
  * On JVM, `PairList` behaves like a regular list of tuples. On-chain, the compiler lowers it to
  * the same `SumDataPairList` representation as `List[(A, B)]`, so conversions via `toList` /
  * `toPairList` are zero-cost (no UPLC code generated).
  *
  * Pair-specific operations like `mapValues` operate directly on `fstPair`/`sndPair` builtins,
  * yielding ~3x fewer builtin operations per element compared to `List.map` on tuples.
  *
  * @tparam A
  *   the type of the first element (key)
  * @tparam B
  *   the type of the second element (value)
  */
enum PairList[+A, +B]:
    case PairNil extends PairList[Nothing, Nothing]
    case PairCons(head: (A, B), tail: PairList[A, B]) extends PairList[A, B]

@Compile
object PairList {
    import Option.*
    import List.*

    inline def empty[A, B]: PairList[A, B] = PairNil

    inline def single[A, B](a: A, b: B): PairList[A, B] = PairCons((a, b), PairNil)

    @Ignore
    def from[A, B](it: IterableOnce[(A, B)]): PairList[A, B] =
        it.iterator.foldRight(empty[A, B]) { case (pair, acc) => PairCons(pair, acc) }

    extension [A, B](self: PairList[A, B])

        /** Returns the first element of this `PairList`.
          *
          * @return
          *   The first key-value pair.
          * @throws NoSuchElementException
          *   If the `PairList` is empty.
          * @example
          *   {{{
          *   PairList.single("a", 1).head === ("a", 1)
          *   }}}
          */
        def head: (A, B) = self match
            case PairNil           => throw new NoSuchElementException("head of empty list")
            case PairCons(head, _) => head

        /** Returns all elements except the first.
          *
          * @return
          *   The tail of the `PairList`.
          * @throws NoSuchElementException
          *   If the `PairList` is empty.
          */
        def tail: PairList[A, B] = self match
            case PairNil           => throw new NoSuchElementException("tail of empty list")
            case PairCons(_, tail) => tail

        /** Converts this `PairList` to a `List[(A, B)]`.
          *
          * On-chain this is a zero-cost operation (noop in UPLC) because both types share the same
          * `SumDataPairList` representation.
          */
        def toList: List[(A, B)] = self match
            case PairNil              => List.Nil
            case PairCons(head, tail) => List.Cons(head, tail.toList)

        def isEmpty: Boolean = self match
            case PairNil => true
            case _       => false

        def nonEmpty: Boolean = !self.isEmpty

        def length: BigInt = self match
            case PairNil           => BigInt(0)
            case PairCons(_, tail) => BigInt(1) + tail.length

        /** Maps values while keeping keys unchanged.
          *
          * On-chain this is significantly cheaper than `List.map` on tuples because it uses
          * `fstPair`/`sndPair` builtins directly (~4 ops/element vs ~12).
          */
        def mapValues[C](f: B => C): PairList[A, C] = self match
            case PairNil                => PairNil
            case PairCons((k, v), tail) => PairCons((k, f(v)), tail.mapValues(f))

        def map[C, D](f: ((A, B)) => (C, D)): PairList[C, D] = self match
            case PairNil              => PairNil
            case PairCons(head, tail) => PairCons(f(head), tail.map(f))

        def filter(p: ((A, B)) => Boolean): PairList[A, B] = self match
            case PairNil => PairNil
            case PairCons(head, tail) =>
                if p(head) then PairCons(head, tail.filter(p))
                else tail.filter(p)

        def filterNot(p: ((A, B)) => Boolean): PairList[A, B] =
            self.filter(x => !p(x))

        def foldLeft[C](init: C)(f: (C, (A, B)) => C): C = self match
            case PairNil              => init
            case PairCons(head, tail) => tail.foldLeft(f(init, head))(f)

        def foldRight[C](init: C)(f: ((A, B), C) => C): C = self match
            case PairNil              => init
            case PairCons(head, tail) => f(head, tail.foldRight(init)(f))

        def forall(p: ((A, B)) => Boolean): Boolean = self match
            case PairNil              => true
            case PairCons(head, tail) => p(head) && tail.forall(p)

        def exists(p: ((A, B)) => Boolean): Boolean = self match
            case PairNil              => false
            case PairCons(head, tail) => p(head) || tail.exists(p)

        def find(p: ((A, B)) => Boolean): Option[(A, B)] = self match
            case PairNil => None
            case PairCons(head, tail) =>
                if p(head) then Some(head)
                else tail.find(p)

        def findMap[C](f: ((A, B)) => Option[C]): Option[C] = self match
            case PairNil => None
            case PairCons(head, tail) =>
                f(head) match
                    case Some(v) => Some(v)
                    case None    => tail.findMap(f)

        def prepended(elem: (A, B)): PairList[A, B] = PairCons(elem, self)

        def ++(other: PairList[A, B]): PairList[A, B] = self match
            case PairNil              => other
            case PairCons(head, tail) => PairCons(head, tail ++ other)

        @Ignore
        def asScala: scala.collection.immutable.List[(A, B)] =
            self.foldRight(scala.Nil: scala.collection.immutable.List[(A, B)]) { case (pair, acc) =>
                pair :: acc
            }

    given pairListToData[A: ToData, B: ToData]: ToData[PairList[A, B]] =
        (a: PairList[A, B]) => {
            def loop(a: PairList[A, B]): BuiltinList[BuiltinPair[Data, Data]] =
                a match
                    case PairNil => mkNilPairData()
                    case PairCons((k, v), tail) =>
                        mkCons(mkPairData(summon[ToData[A]](k), summon[ToData[B]](v)), loop(tail))
            mapData(loop(a))
        }

    given pairListFromData[A: FromData, B: FromData]: FromData[PairList[A, B]] =
        (d: Data) =>
            def loop(ls: BuiltinList[BuiltinPair[Data, Data]]): PairList[A, B] =
                if ls.isEmpty then PairNil
                else PairCons((fromData[A](ls.head.fst), fromData[B](ls.head.snd)), loop(ls.tail))
            loop(unMapData(d))

    given pairListEq[A: Eq, B: Eq]: Eq[PairList[A, B]] =
        (lhs: PairList[A, B], rhs: PairList[A, B]) =>
            lhs match
                case PairNil =>
                    rhs match
                        case PairNil        => true
                        case PairCons(_, _) => false
                case PairCons(headLhs, tailLhs) =>
                    rhs match
                        case PairNil => false
                        case PairCons(headRhs, tailRhs) =>
                            headLhs === headRhs && tailLhs === tailRhs

    /** Converts a `List[(A, B)]` to a `PairList[A, B]`.
      *
      * On-chain this is a zero-cost operation (noop in UPLC) because both types share the same
      * `SumDataPairList` representation.
      */
    extension [A, B](self: List[(A, B)])
        def toPairList: PairList[A, B] = self match
            case Nil              => PairNil
            case Cons(head, tail) => PairCons(head, tail.toPairList)
}
