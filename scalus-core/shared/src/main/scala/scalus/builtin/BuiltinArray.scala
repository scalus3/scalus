package scalus.builtin

class BuiltinArray[A] private (array: IndexedSeq[A]) {
    def length: BigInt = array.length
    def apply(n: BigInt): A = array(n.intValue)
}
object BuiltinArray {
    def empty[A]: BuiltinArray[A] = new BuiltinArray(IndexedSeq.empty)
    def apply[A](xs: A*): BuiltinArray[A] = new BuiltinArray(xs.toIndexedSeq)
    def from[A](xs: IterableOnce[A]): BuiltinArray[A] = new BuiltinArray(xs.iterator.toIndexedSeq)
    def fromList[A](a: BuiltinList[A]): BuiltinArray[A] = new BuiltinArray(a.toList.toIndexedSeq)
}
