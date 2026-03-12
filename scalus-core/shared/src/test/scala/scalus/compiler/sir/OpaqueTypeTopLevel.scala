package scalus.compiler.sir

import scalus.compiler.Compile

package opaquetypes {

    @Compile
    object TopMyInt {
        opaque type T = BigInt

        def apply(x: BigInt): T = x

        extension (x: T) def add(y: T): T = x + y
    }
}
