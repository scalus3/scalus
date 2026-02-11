package scalus.uplc.builtin

import scala.collection.immutable

/** A builtin list type corresponding to the Plutus Core builtin list.
  *
  * This is the Scalus equivalent of `BuiltinList` in Plutus. It represents an immutable
  * singly-linked list that can be used in Plutus smart contracts.
  *
  * The list supports basic operations: checking emptiness, accessing head/tail, and prepending
  * elements. These operations correspond directly to Plutus Core builtin functions.
  *
  * ==UPLC Builtin Operations==
  *
  * The following UPLC builtins operate on lists:
  *   - `mkCons` - Prepend element (corresponds to `::`)
  *   - `headList` - Get first element (corresponds to `head`)
  *   - `tailList` - Get all but first (corresponds to `tail`)
  *   - `nullList` - Check if empty (corresponds to `isEmpty`)
  *   - `chooseList` - Pattern match on empty vs non-empty
  *   - `dropList` - Drop first n elements (Plutus V4+)
  *   - `mkNilData` - Create empty list of Data
  *   - `mkNilPairData` - Create empty list of Data pairs
  *
  * @note
  *   For higher-level list operations like `map`, `filter`, `fold`, etc., use
  *   [[scalus.cardano.onchain.plutus.prelude.List]] which provides a rich set of combinators.
  *
  * @tparam A
  *   the element type
  *
  * @see
  *   [[scalus.cardano.onchain.plutus.prelude.List]] for higher-level list operations
  * @since Plutus
  *   V1
  */
enum BuiltinList[+A]:
    case Nil extends BuiltinList[Nothing]

    case Cons(h: A, tl: BuiltinList[A]) extends BuiltinList[A]

    /** Checks if the list is empty.
      *
      * Corresponds to UPLC builtin `nullList`.
      *
      * @return
      *   true if the list has no elements
      */
    def isEmpty: Boolean = this match
        case Nil => true
        case _   => false

    /** Returns the first element of the list.
      *
      * Corresponds to UPLC builtin `headList`.
      *
      * @return
      *   the first element
      * @throws NoSuchElementException
      *   if the list is empty
      */
    def head: A = this match
        case Cons(h, _) => h
        case _          => throw new NoSuchElementException("head of empty list")

    /** Returns all elements except the first.
      *
      * Corresponds to UPLC builtin `tailList`.
      *
      * @return
      *   a new list without the first element
      * @throws NoSuchElementException
      *   if the list is empty
      */
    def tail: BuiltinList[A] = this match
        case Cons(_, t) => t
        case _          => throw new NoSuchElementException("tail of empty list")

    /** Prepends an element to the list.
      *
      * Corresponds to UPLC builtin `mkCons`.
      *
      * @param x
      *   the element to prepend
      * @return
      *   a new list with x as the first element
      */
    def ::[B >: A](x: B): BuiltinList[B] = Cons(x, this)

    /** Converts to a Scala immutable List.
      *
      * This is an offchain operation for interoperability with Scala collections.
      */
    def toList: immutable.List[A] = this match
        case Nil        => immutable.Nil
        case Cons(h, t) => h :: t.toList

/** Companion object for [[BuiltinList]] providing factory methods. */
object BuiltinList:

    /** Creates an empty BuiltinList.
      *
      * For lists of Data, prefer using `Builtins.mkNilData()` onchain.
      */
    def empty[A]: BuiltinList[A] = Nil

    /** Creates a BuiltinList from varargs. */
    def apply[A](xs: A*): BuiltinList[A] = xs.foldRight(empty[A])(_ :: _)

    /** Creates a BuiltinList from any IterableOnce. */
    def from[A](xs: IterableOnce[A]): BuiltinList[A] = xs.iterator.foldRight(empty[A])(_ :: _)

/** A builtin pair type corresponding to the Plutus Core builtin pair.
  *
  * Pairs are polymorphic 2-tuples used throughout Plutus for composite data.
  *
  * ==UPLC Builtin Operations==
  *
  * The following UPLC builtins operate on pairs:
  *   - `fstPair` - Extract the first element (corresponds to `fst`)
  *   - `sndPair` - Extract the second element (corresponds to `snd`)
  *   - `mkPairData` - Create a pair of Data values
  *
  * @param fst
  *   the first element
  * @param snd
  *   the second element
  *
  * @tparam A
  *   type of the first element
  * @tparam B
  *   type of the second element
  *
  * @since Plutus
  *   V1
  */
case class BuiltinPair[A, B](fst: A, snd: B):
    override def toString = "(" + fst + ", " + snd + ")"
