package scalus.cardano.node.emulator

import org.scalatest.funsuite.AnyFunSuite

class ChainConfigTest extends AnyFunSuite {

    test("instant and manual carry the defaults of the spec table") {
        // spec [CF-2]
        assert(ChainConfig.instant.blockProduction == BlockProduction.Instant)
        assert(ChainConfig.manual.blockProduction == BlockProduction.Manual)
        for config <- Seq(ChainConfig.instant, ChainConfig.manual) do
            assert(config.awaitTx == AwaitTxPolicy.ProduceBlock)
            assert(config.orphanTxs == OrphanTxPolicy.ReturnToMempool)
            assert(config.securityParam == 2160L)
    }

    test("each with method changes its own field and nothing else") {
        // spec [CF-3]
        val base = ChainConfig.instant
        assert(base.withBlockProduction(BlockProduction.Manual) == ChainConfig.manual)
        val waitOnly = base.withAwaitTx(AwaitTxPolicy.WaitOnly)
        assert(waitOnly.awaitTx == AwaitTxPolicy.WaitOnly)
        assert(waitOnly.withAwaitTx(AwaitTxPolicy.ProduceBlock) == base)
        val drop = base.withOrphanTxs(OrphanTxPolicy.Drop)
        assert(drop.orphanTxs == OrphanTxPolicy.Drop)
        assert(drop.withOrphanTxs(OrphanTxPolicy.ReturnToMempool) == base)
        val k = base.withSecurityParam(5L)
        assert(k.securityParam == 5L)
        assert(k.withSecurityParam(2160L) == base)
    }

    test("a security parameter below 1 is rejected") {
        // spec section 5: securityParam is at least 1
        assertThrows[IllegalArgumentException](ChainConfig.instant.withSecurityParam(0L))
    }

    test("there is no constructor or copy with default parameters") {
        // spec [CF-1]: Java and JS cannot use Scala default parameters
        assertDoesNotCompile(
          "ChainConfig(BlockProduction.Manual, AwaitTxPolicy.WaitOnly, OrphanTxPolicy.Drop, 1L)"
        )
        assertDoesNotCompile("ChainConfig.instant.copy(securityParam = 3L)")
        assertCompiles("ChainConfig.instant.withSecurityParam(3L)")
    }
}
