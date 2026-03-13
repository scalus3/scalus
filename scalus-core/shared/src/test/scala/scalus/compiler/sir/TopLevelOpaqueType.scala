package scalus.compiler.sir.toplevelope

import scalus.compiler.Compile

opaque type TopMyInt = BigInt

@Compile
object TopMyInt {
    def apply(x: BigInt): TopMyInt = x
    extension (x: TopMyInt) def add(y: TopMyInt): TopMyInt = x + y
}
