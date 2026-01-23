package scalus.cardano.onchain.plutus.prelude

import scalus.Compile
import scalus.Ignore
import scalus.uplc.builtin.Builtins.*
import scalus.uplc.builtin.Data.{fromData, toData}
import scalus.uplc.builtin.{Data, FromData, ToData}

/** Alternative to `scala.Option` in onchain code.
  *
  * Options [[Eq]] and [[Ord]] are defined in terms of the contained `A` elements.
  * @tparam A
  *   the type of the element contained in the [[Option]]
  */
enum Option[+A]:
    // note, that order of cases matters, as it is used in serialization
    case Some(value: A) extends Option[A]
    case None extends Option[Nothing]

@Compile
object Option {

    /** Constructs an Option from a value. If the value is null, returns None, otherwise
      * Some(value).
      *
      * @return
      *   Some(value) if value != null, None if value == null
      *
      * @example
      *   {{{
      *   Option.apply(null) == Option.None
      *   Option.apply("1").get == "1"
      *   }}}
      */
    @Ignore
    inline def apply[A](x: A): Option[A] = if x == null then None else Some(x)

    /** Returns an empty Option instance.
      *
      * @return
      *   an empty Option instance (None)
      *
      * @example
      *   {{{
      *   Option.empty == Option.apply(null)
      *   }}}
      */
    inline def empty[A]: Option[A] = None

    given optionEq[A](using eq: Eq[A]): Eq[Option[A]] = (a: Option[A], b: Option[A]) =>
        a match
            case None =>
                b match
                    case None    => true
                    case Some(_) => false
            case Some(value) =>
                b match
                    case None         => false
                    case Some(value2) => value === value2

    // empties
    given emptyOptionEq: Eq[Option[Nothing]] = (_, _) => true

    given optionOrd[A](using ord: Ord[A]): Ord[Option[A]] = (a: Option[A], b: Option[A]) =>
        a match
            case None =>
                b match
                    case None    => Order.Equal
                    case Some(_) => Order.Less
            case Some(value1) =>
                b match
                    case None         => Order.Greater
                    case Some(value2) => value1 <=> value2

    given optionFromData[A: FromData]: FromData[Option[A]] = (d: Data) =>
        val pair = unConstrData(d)
        if pair.fst == BigInt(0) then Option.Some(fromData[A](pair.snd.head))
        else Option.None

    given optionToData[A: ToData]: ToData[Option[A]] =
        (a: Option[A]) => {
            a match {
                case Option.Some(v) =>
                    constrData(0, mkCons(v.toData, mkNilData()))
                case Option.None => constrData(1, mkNilData())
            }
        }

    extension [A](self: Option[Option[A]]) {

        /** Returns the inner Option if this Option contains one, otherwise returns None.
          *
          * @return
          *   the inner Option if this Option contains a Some, otherwise None
          *
          * @example
          *   {{{
          *   Option.Some(Option.Some("hello")).flatten == Option.Some("hello")
          *   Option.Some(Option.None).flatten == Option.None
          *   Option.None.flatten == Option.None
          *   }}}
          */
        def flatten: Option[A] = self match
            case None    => None
            case Some(a) => a
    }

    extension [A](self: Option[A]) {

        /** Checks if this Option is empty.
          *
          * @return
          *   true if this Option is None, false if it contains a value
          *
          * @example
          *   {{{
          *   Option.empty.isEmpty == true
          *   Option.apply("1").isEmpty == false
          *   Option.apply(null).isEmpty == true
          *   }}}
          */
        def isEmpty: Boolean = self match
            case None    => true
            case Some(_) => false

        /** Checks if this Option contains a value.
          *
          * @return
          *   true if this Option contains a value, false if it is None
          *
          * @example
          *   {{{
          *   Option.empty.nonEmpty == false
          *   Option.apply("1").nonEmpty == true
          *   Option.apply(null).nonEmpty == false
          *   }}}
          */
        inline def nonEmpty: Boolean = !isEmpty

        /** Checks if this Option contains a value.
          *
          * @return
          *   true if this Option contains a value, false if it is None
          *
          * @see
          *   [[nonEmpty]]
          */
        inline def isDefined: Boolean = nonEmpty

        /** Returns the value if this Option contains one, otherwise throws an exception.
          *
          * @param message
          *   message to include in the exception if this Option is None
          * @return
          *   the value contained in this Option
          * @throws NoSuchElementException
          *   if this Option is None
          *
          * @example
          *   {{{
          *   Option.Some("hello").getOrFail() == "hello"
          *
          *   try {
          *     Option.None.getOrFail("custom message")
          *   } catch {
          *     case e: NoSuchElementException => e.getMessage == "custom message"
          *   }
          *   }}}
          */
        inline def getOrFail(inline message: String = "None.getOrFail"): A = self match
            case None        => throw new NoSuchElementException(message)
            case Some(value) => value

        /** Throws an exception if this Option is None, otherwise does nothing.
          *
          * @param message
          *   message to include in the exception if this Option is None
          * @throws NoSuchElementException
          *   if this Option is None
          *
          * @example
          *   {{{
          *   Option.Some("hello") orFail "Should not throw" == ()
          *
          *   try {
          *     Option.None orFail "custom message"
          *   } catch {
          *     case e: NoSuchElementException => e.getMessage == "custom message"
          *   }
          *   }}}
          */
        inline infix def orFail(inline message: String = "None.orFail"): Unit = self match
            case None    => throw new NoSuchElementException(message)
            case Some(_) => ()

        /** Returns the value if this Option contains one, otherwise throws an exception.
          *
          * @return
          *   the value contained in this Option
          * @throws NoSuchElementException
          *   if this Option is None
          *
          * @example
          *   {{{
          *   Option.Some("hello").get == "hello"
          *
          *   try {
          *     Option.None.get
          *   } catch {
          *     case e: NoSuchElementException => e.getMessage == "None.get"
          *   }
          *   }}}
          *
          * @see
          *   [[getOrFail]]
          */
        def get: A = getOrFail("None.get")

        /** Returns the value if this Option contains one, otherwise returns the default value.
          *
          * @param default
          *   the value to return if this Option is None
          * @return
          *   the value of this Option if it contains one, otherwise the default value
          *
          * @example
          *   {{{
          *   Option.Some("hello").getOrElse("world") == "hello"
          *   Option.None.getOrElse("world") == "world"
          *   }}}
          */
        def getOrElse[B >: A](default: B): B = self match
            case None    => default
            case Some(a) => a

        /** Returns this Option if it contains a value, otherwise returns the alternative Option.
          *
          * @param alternative
          *   the Option to return if this Option is None
          * @return
          *   this Option if it contains a value, otherwise the alternative Option
          *
          * @example
          *   {{{
          *   Option.Some("hello").orElse(Option.Some("world")) == Option.Some("hello")
          *   Option.None.orElse(Option.Some("world")) == Option.Some("world")
          *   Option.None.orElse(Option.None) == Option.None
          *   }}}
          */
        def orElse[B >: A](alternative: Option[B]): Option[B] = self match
            case None    => alternative
            case Some(a) => self

        /** Applies a function to the value inside this Option.
          *
          * @param mapper
          *   the function to apply to the value of this Option
          * @return
          *   a Some containing the result if this Option contains a value, otherwise None
          *
          * @example
          *   {{{
          *   Option.Some(BigInt(5)).map(_ * BigInt(2)) == Option.Some(BigInt(10))
          *   Option.None.map(_ * BigInt(2)) == Option.None
          *   Option.Some("hello").map(_.toUpperCase) == Option.Some("HELLO")
          *   }}}
          *
          * @see
          *   [[flatMap]]
          */
        def map[B](mapper: A => B): Option[B] = self match
            case None    => None
            case Some(a) => Some(mapper(a))

        /** Applies a function to the value inside this Option and returns the result directly.
          *
          * @param mapper
          *   the function to apply to the value of this Option, returning an Option
          * @return
          *   the Option returned by the mapper function if this Option contains a value, otherwise
          *   None
          *
          * @example
          *   {{{
          *   Option.Some(BigInt(5)).flatMap(x => if x > BigInt(0) then Option.Some(x * BigInt(2)) else Option.None) == Option.Some(BigInt(10))
          *   Option.Some(BigInt(-1)).flatMap(x => if x > BigInt(0) then Option.Some(x * BigInt(2)) else Option.None) == Option.None
          *   Option.None.flatMap(x => Option.Some(x * BigInt(2))) == Option.None
          *   }}}
          *
          * @see
          *   [[map]]
          */
        def flatMap[B](mapper: A => Option[B]): Option[B] = self match
            case None    => None
            case Some(a) => mapper(a)

        /** Returns this Option if its value satisfies the given condition, otherwise returns None.
          *
          * @param predicate
          *   the condition to test the value against
          * @return
          *   this Option if it contains a value that satisfies the predicate, otherwise None
          *
          * @example
          *   {{{
          *   Option.Some(BigInt(5)).filter(_ > BigInt(3)) == Option.Some(BigInt(5))
          *   Option.Some(BigInt(2)).filter(_ > BigInt(3)) == Option.None
          *   Option.None.filter(_ > BigInt(3)) == Option.None
          *   }}}
          *
          * @see
          *   [[filterNot]]
          */
        def filter(predicate: A => Boolean): Option[A] = self match
            case None    => None
            case Some(a) => if predicate(a) then self else None

        /** Returns this Option if its value does not satisfy the given condition, otherwise returns
          * None.
          *
          * @param predicate
          *   the condition to test the value against
          * @return
          *   this Option if it contains a value that does not satisfy the predicate, otherwise None
          *
          * @example
          *   {{{
          *   Option.Some(BigInt(2)).filterNot(_ > BigInt(3)) == Option.Some(BigInt(2))
          *   Option.Some(BigInt(5)).filterNot(_ > BigInt(3)) == Option.None
          *   Option.None.filterNot(_ > 3) == Option.None
          *   }}}
          *
          * @see
          *   [[filter]]
          */
        def filterNot(predicate: A => Boolean): Option[A] = filter(!predicate(_))

        /** Checks if this Option contains the specified element.
          *
          * @param elem
          *   the element to check for
          * @param eq
          *   the equality comparison to use
          * @return
          *   true if this Option contains a value equal to elem, false otherwise
          *
          * @example
          *   {{{
          *   Option.Some("hello").contains("hello") == true
          *   Option.Some("hello").contains("world") == false
          *   Option.None.contains("hello") == false
          *   }}}
          */
        def contains[B >: A](elem: B)(using eq: Eq[B]): Boolean = self match
            case None    => false
            case Some(a) => a === elem

        /** Checks if this Option contains a value that satisfies the given condition.
          *
          * @param p
          *   the condition to test the value against
          * @return
          *   true if this Option contains a value that satisfies the condition, false otherwise
          *
          * @example
          *   {{{
          *   Option.Some(BigInt(5)).exists(_ > BigInt(3)) == true
          *   Option.Some(BigInt(2)).exists(_ > BigInt(3)) == false
          *   Option.None.exists(_ > BigInt(3)) == false
          *   }}}
          */
        def exists(p: A => Boolean): Boolean = self match
            case None    => false
            case Some(a) => p(a)

        /** Checks if this Option is empty or its value satisfies the given condition.
          *
          * @param p
          *   the condition to test the value against
          * @return
          *   true if this Option is empty or contains a value that satisfies the condition, false
          *   otherwise
          *
          * @example
          *   {{{
          *   Option.Some(BigInt(5)).forall(_ > BigInt(3)) == true
          *   Option.Some(BigInt(2)).forall(_ > BigInt(3)) == false
          *   Option.None.forall(_ > BigInt(3)) == true
          *   }}}
          */
        def forall(p: A => Boolean): Boolean = self match
            case None    => true
            case Some(a) => p(a)

        /** Returns this Option if its value satisfies the given condition, otherwise returns None.
          *
          * @param p
          *   the condition to test the value against
          * @return
          *   this Option if it contains a value that satisfies the condition, otherwise None
          *
          * @example
          *   {{{
          *   Option.Some(BigInt(5)).find(_ > BigInt(3)) == Option.Some(BigInt(5))
          *   Option.Some(BigInt(2)).find(_ > BigInt(3)) == Option.None
          *   Option.None.find(_ > BigInt(3)) == Option.None
          *   }}}
          *
          * @see
          *   [[filter]]
          */
        inline def find(p: A => Boolean): Option[A] = filter(p)

        /** Converts this Option to a scala.Option.
          *
          * @return
          *   a scala.Some if this Option contains a value, scala.None otherwise
          *
          * @example
          *   {{{
          *   Option.Some("hello").asScala == scala.Some("hello")
          *   Option.None.asScala == scala.None
          *   }}}
          */
        @Ignore
        def asScala: scala.Option[A] = self match
            case None    => scala.None
            case Some(a) => scala.Some(a)
    }
}
