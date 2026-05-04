package scalus.compiler.test

import scalus.compiler.Compile

/** User code that calls SimpleContext trait methods — plugin redirects to SubstOps. */
@Compile
object UseSimpleContext {
    def run(ctx: SimpleContext): BigInt = {
        val v = ctx.getValue
        ctx.add(v)
    }

    /** Chains through nested substituted type: ctx.txInfo returns ContextTxInfo, then .fee */
    def getFee(ctx: SimpleContext): BigInt = {
        val info = ctx.txInfo
        info.fee
    }
}
