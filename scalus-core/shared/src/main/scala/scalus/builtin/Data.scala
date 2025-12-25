package scalus.builtin

import scalus.builtin.Data.{B, Constr, I, List, Map}
import scalus.prelude.List as PList

import scala.compiletime.asMatchable

/** The universal data type for Plutus smart contracts.
  *
  * `Data` is the core serializable type used throughout Cardano for representing:
  *   - Script datums attached to UTxOs
  *   - Redeemers passed to validators
  *   - Arbitrary structured data in smart contracts
  *
  * It corresponds to the `BuiltinData` type in Plutus Core and can represent any algebraic data
  * type through its five constructors:
  *
  *   - [[Data.Constr]] - Constructor application with tag and arguments (for sum/product types)
  *   - [[Data.Map]] - Key-value mappings
  *   - [[Data.List]] - Ordered sequences
  *   - [[Data.I]] - Arbitrary-precision integers
  *   - [[Data.B]] - Byte strings
  *
  * ==Encoding Convention==
  *
  * Scalus uses the standard Plutus Data encoding:
  *   - Sum types use `Constr` with constructor index as tag
  *   - Product types (case classes) use `Constr` with fields as arguments
  *   - `Option[A]` encodes as `Constr(0, [])` for `None`, `Constr(1, [value])` for `Some`
  *   - `Boolean` encodes as `Constr(0, [])` for `false`, `Constr(1, [])` for `true`
  *
  * ==UPLC Builtin Operations==
  *
  * The following UPLC builtins operate on Data:
  *   - `chooseData` - Pattern match on Data constructor
  *   - `constrData`, `mapData`, `listData`, `iData`, `bData` - Construct Data values
  *   - `unConstrData`, `unMapData`, `unListData`, `unIData`, `unBData` - Deconstruct Data values
  *   - `equalsData` - Structural equality comparison
  *   - `serialiseData` - CBOR serialization (Plutus V2+)
  *
  * @example
  *   {{{
  *   // Encoding a simple value
  *   val intData: Data = Data.I(42)
  *
  *   // Encoding a product type (pair)
  *   val pairData: Data = Data.Constr(0, List(Data.I(1), Data.I(2)))
  *
  *   // Encoding a sum type (Option)
  *   val someData: Data = Data.Constr(1, List(Data.I(42)))  // Some(42)
  *   val noneData: Data = Data.Constr(0, List.Nil)          // None
  *   }}}
  *
  * @see
  *   [[scalus.builtin.ToData]] for automatic encoding to Data
  * @see
  *   [[scalus.builtin.FromData]] for automatic decoding from Data
  * @see
  *   [[https://plutus.cardano.intersectmbo.org/resources/plutus-core-spec.pdf Plutus Core Specification]]
  */
sealed abstract class Data {
    private[scalus] def showDebug: String = this match {
        case Constr(constr, args) =>
            s"<$constr, [${args.toScalaList.map(_.showDebug).mkString(", ")}]>"
        case Map(values) =>
            s"{${values.toScalaList.map { case (k, v) => s"${k.showDebug}: ${v.showDebug}" }.mkString(", ")}}"
        case List(values) =>
            s"[${values.toScalaList.map(_.showDebug).mkString(", ")}]"
        case I(value) =>
            s"$value"
        case B(value) =>
            s"\"${value.toHex}\""
    }

    override def toString: String = showDebug
}

/** Companion object for [[Data]] providing constructors and type class instances.
  *
  * Use the [[ToData]] and [[FromData]] type classes for automatic conversion between Scala types
  * and Data.
  */
object Data extends DataApi:

    /** Type class for converting values to Data representation. */
    type ToData[A] = scalus.builtin.ToData[A]

    extension [A: ToData](a: A)
        /** Converts this value to its Data representation using the ToData type class. */
        inline def toData: Data = summon[ToData[A]](a)

    extension (inline data: Data)
        /** Converts this Data to type A using the FromData type class.
          * @throws Exception
          *   if the Data structure doesn't match the expected type
          */
        inline def to[A](using inline ev: FromData[A]): A = ev(data)

    /** Type class for converting Data back to typed values. */
    type FromData[A] = scalus.builtin.FromData[A]

    /** Converts Data to type A using the FromData type class.
      *
      * @param data
      *   the Data to convert
      * @return
      *   the converted value
      * @throws Exception
      *   if the Data structure doesn't match the expected type
      */
    inline def fromData[A](inline data: Data)(using inline ev: scalus.builtin.FromData[A]): A = ev(
      data
    )

    /** Constructor application - represents sum types and product types.
      *
      * This is the primary way to encode algebraic data types in Plutus:
      *   - For sum types, `constr` is the constructor index (0, 1, 2, ...)
      *   - For product types (case classes), `constr` is typically 0
      *   - `args` contains the constructor's field values
      *
      * Corresponds to UPLC builtins `constrData` and `unConstrData`.
      *
      * @param constr
      *   the constructor index (must be non-negative)
      * @param args
      *   the constructor arguments as a list of Data values
      *
      * @example
      *   {{{
      *   // A pair (Int, String)
      *   Constr(0, List(I(42), B(ByteString.fromString("hello"))))
      *
      *   // Either.Left(10)
      *   Constr(0, List(I(10)))
      *
      *   // Either.Right("x")
      *   Constr(1, List(B(ByteString.fromString("x"))))
      *   }}}
      */
    case class Constr(constr: BigInt, args: PList[Data]) extends Data {
        assert(constr >= 0, s"Constructor must be non-negative, got $constr")
    }

    /** Key-value mapping - represents associative data structures.
      *
      * Used to encode `Map[K, V]` types where both keys and values are Data. Note that duplicate
      * keys are allowed (this is a list of pairs, not a true map).
      *
      * Corresponds to UPLC builtins `mapData` and `unMapData`.
      *
      * @param values
      *   list of key-value pairs
      *
      * @example
      *   {{{
      *   // A map from integers to strings
      *   Map(List((I(1), B(hex"aa")), (I(2), B(hex"bb"))))
      *   }}}
      */
    case class Map(values: PList[(Data, Data)]) extends Data {
        override def hashCode(): Int = values.toScalaList.toSet.hashCode()
        override def equals(x: Any): Boolean = x.asMatchable match {
            case Map(otherValues) => values.toScalaList.toSet == otherValues.toScalaList.toSet
            case _                => false
        }
    }

    /** Ordered sequence - represents list types.
      *
      * Used to encode `List[A]` types where elements are Data.
      *
      * Corresponds to UPLC builtins `listData` and `unListData`.
      *
      * @param values
      *   the list elements
      *
      * @example
      *   {{{
      *   // A list of integers [1, 2, 3]
      *   List(PList(I(1), I(2), I(3)))
      *   }}}
      */
    case class List(values: PList[Data]) extends Data:
        override def toString: String =
            s"List(${values.toScalaList.map(v => v.toString + "::").mkString}Nil)"

    /** Arbitrary-precision integer.
      *
      * Corresponds to UPLC builtins `iData` and `unIData`.
      *
      * @param value
      *   the integer value
      *
      * @example
      *   {{{
      *   I(42)
      *   I(-1000000000000000000L)
      *   }}}
      */
    case class I(value: BigInt) extends Data

    /** Byte string - represents binary data.
      *
      * Used to encode `ByteString`, `PubKeyHash`, `CurrencySymbol`, `TokenName`, etc.
      *
      * Corresponds to UPLC builtins `bData` and `unBData`.
      *
      * @param value
      *   the byte string
      *
      * @example
      *   {{{
      *   B(ByteString.fromHex("deadbeef"))
      *   B(ByteString.empty)
      *   }}}
      */
    case class B(value: ByteString) extends Data:
        override def toString: String = s"B(\"${value.toHex}\")"

    /** The unit value encoded as Data.
      *
      * Equivalent to `Constr(0, Nil)` - an empty constructor with tag 0.
      */
    val unit: Data = Constr(0, PList.Nil)
