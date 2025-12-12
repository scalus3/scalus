package scalus.builtin

import scala.collection.immutable

/** A builtin list type corresponding to the Plutus Core builtin list.
  *
  * This is the Scalus equivalent of `BuiltinList` in Plutus. It represents an immutable
  * singly-linked list that can be used in Plutus smart contracts.
  *
  * The list supports basic operations: checking emptiness, accessing head/tail, and prepending
  * elements. These operations correspond directly to Plutus Core builtin functions.
  *
  * @tparam A
  *   the element type
  */
enum BuiltinList[+A]:
    private case Nil extends BuiltinList[Nothing]

    private case Cons(h: A, tl: BuiltinList[A]) extends BuiltinList[A]

    /** Checks if the list is empty and never fails. */
    def isEmpty: Boolean = this match
        case Nil => true
        case _   => false

    /** Returns the first element of the list.
      * @throws NoSuchElementException
      *   if the list is empty
      */
    def head: A = this match
        case Cons(h, _) => h
        case _          => throw new NoSuchElementException("head of empty list")

    /** Returns all elements except the first.
      * @throws NoSuchElementException
      *   if the list is empty
      */
    def tail: BuiltinList[A] = this match
        case Cons(_, t) => t
        case _          => throw new NoSuchElementException("tail of empty list")

    /** Prepends an element to the list and never fails. */
    def ::[B >: A](x: B): BuiltinList[B] = Cons(x, this)

    /** Converts to a Scala immutable List. */
    def toList: immutable.List[A] = this match
        case Nil        => immutable.Nil
        case Cons(h, t) => h :: t.toList

/** Companion object for [[BuiltinList]]. */
object BuiltinList:
    /** Creates an empty BuiltinList. */
    def empty[A]: BuiltinList[A] = Nil

    /** Creates a BuiltinList from varargs. */
    def apply[A](xs: A*): BuiltinList[A] = xs.foldRight(empty[A])(_ :: _)

    /** Creates a BuiltinList from any IterableOnce. */
    def from[A](xs: IterableOnce[A]): BuiltinList[A] = xs.iterator.foldRight(empty[A])(_ :: _)

/** A builtin pair type corresponding to the Plutus Core builtin pair.
  *
  * @param fst
  *   the first element
  * @param snd
  *   the second element
  */
case class BuiltinPair[A, B](fst: A, snd: B):
    override def toString = "(" + fst + ", " + snd + ")"
