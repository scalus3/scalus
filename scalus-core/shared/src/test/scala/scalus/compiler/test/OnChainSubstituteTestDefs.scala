package scalus.compiler.test

import scalus.compiler.Compile
import scalus.compiler.{Ignore, OnChainSubstitute}
import scalus.uplc.builtin.Data

/** Substitute @Compile object for ContextTxInfo: on-chain, ContextTxInfo is just Data (TxInfo). */
@Compile
object ContextTxInfoOps {

    /** On-chain, ContextTxInfo is Data. For this test, fee simply unwraps I(n). */
    def fee(self: Data): BigInt = self.toI
}

/** Trait with @OnChainSubstitute: on-chain, ContextTxInfo is Data. */
@Compile
@OnChainSubstitute(ContextTxInfoOps)
trait ContextTxInfo {
    @Ignore def fee: BigInt
}

/** Substitute @Compile object for SimpleContext: each method gets `self: Data` as first param. */
@Compile
object SubstOps {
    def getValue(self: Data): BigInt = self.toI
    def add(self: Data, x: BigInt): BigInt = self.toI + x
    def txInfo(self: Data): Data = self
}

/** Trait with @OnChainSubstitute: on-chain calls redirect to SubstOps. */
@Compile
@OnChainSubstitute(SubstOps)
trait SimpleContext {
    @Ignore def getValue: BigInt
    @Ignore def add(x: BigInt): BigInt
    @Ignore def txInfo: ContextTxInfo
}
