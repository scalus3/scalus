package scalus.builtin

/** A builtin array type corresponding to the Plutus Core builtin array (CIP-0156).
  *
  * Arrays provide O(1) indexed access to elements, complementing linked lists which have O(n)
  * access time. This makes arrays ideal for random access patterns.
  *
  * Arrays are created from lists using `listToArray` and provide efficient indexed access via
  * `indexArray` and `multiIndexArray`.
  *
  * ==UPLC Builtin Operations==
  *
  * The following UPLC builtins operate on arrays (available in Plutus V4+):
  *   - `listToArray` - Convert a list to an array
  *   - `lengthOfArray` - Get the number of elements
  *   - `indexArray` - Access element at index (O(1))
  *   - `multiIndexArray` - Access multiple elements by indices
  *
  * @tparam A
  *   the element type
  *
  * @see
  *   [[https://github.com/cardano-foundation/CIPs/tree/master/CIP-0156 CIP-156]]
  * @since Plutus
  *   V4
  */
class BuiltinArray[A] private (array: IndexedSeq[A]) {

    /** Returns the number of elements in the array.
      *
      * Corresponds to UPLC builtin `lengthOfArray`.
      *
      * @return
      *   the array length
      */
    def length: BigInt = array.length

    /** Accesses the element at the given index.
      *
      * Corresponds to UPLC builtin `indexArray`.
      *
      * @param n
      *   the zero-based index
      * @return
      *   the element at index n
      * @throws IndexOutOfBoundsException
      *   if n is out of bounds
      */
    def apply(n: BigInt): A = array(n.intValue)
}

/** Companion object for [[BuiltinArray]] providing factory methods. */
object BuiltinArray {

    /** Creates an empty BuiltinArray. */
    def empty[A]: BuiltinArray[A] = new BuiltinArray(IndexedSeq.empty)

    /** Creates a BuiltinArray from varargs. */
    def apply[A](xs: A*): BuiltinArray[A] = new BuiltinArray(xs.toIndexedSeq)

    /** Creates a BuiltinArray from any IterableOnce. */
    def from[A](xs: IterableOnce[A]): BuiltinArray[A] = new BuiltinArray(xs.iterator.toIndexedSeq)

    /** Creates a BuiltinArray from a BuiltinList.
      *
      * Corresponds to UPLC builtin `listToArray`.
      *
      * @param a
      *   the source list
      * @return
      *   a new array containing all elements from the list
      */
    def fromList[A](a: BuiltinList[A]): BuiltinArray[A] = new BuiltinArray(a.toList.toIndexedSeq)
}
