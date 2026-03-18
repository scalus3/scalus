package scalus.compiler.sir.toplevelope

import scalus.compiler.Compile

opaque type TopMyInt = BigInt

@Compile
object TopMyInt {
    def apply(x: BigInt): TopMyInt = x
    extension (x: TopMyInt)
        def add(y: TopMyInt): TopMyInt = x + y
        inline def value: BigInt = x
}

// Test: a separate @Compile object that uses the top-level opaque type
// This mirrors the pattern where BitcoinHelpers uses BlockHeader from another scope
@Compile
object TopMyIntOps {
    def double(x: TopMyInt): TopMyInt = x.add(x)
}

// TODO: enable after fixing $asInstanceOf$ handling in @Compile objects for inline opaque extensions
// Mirrors BitcoinHelpers.blockHeaderHash(blockHeader: BlockHeader)
// where blockHeader.bytes returns the underlying ByteString via inline extension
// @Compile
// object TopMyIntUnwrap {
//     def unwrap(x: TopMyInt): BigInt = x.value
// }
