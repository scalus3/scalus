package scalus.builtin

case class BuiltinArray[A](array: Vector[A]) {
    def length: BigInt = array.length
    def apply(n: BigInt): A = array(n.intValue)
}
object BuiltinArray {
    def empty[A]: BuiltinArray[A] = BuiltinArray(Vector.empty)
    def apply[A](xs: A*): BuiltinArray[A] = BuiltinArray(xs.toVector)
    def from[A](xs: IterableOnce[A]): BuiltinArray[A] = BuiltinArray(xs.iterator.toVector)
    def fromList[A](a: BuiltinList[A]): BuiltinArray[A] = BuiltinArray(a.toList.toVector)
}
