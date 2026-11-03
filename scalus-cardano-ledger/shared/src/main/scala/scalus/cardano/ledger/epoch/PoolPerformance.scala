package scalus.cardano.ledger.epoch

/** How the reward calculation rates the pools, ledger-rules spec [ES-1], [ES-1a].
  *
  * In the `Instant` preset each tx makes one block, at any slot, so the blocks made are far below
  * the expected blocks and `Measured` rewards are close to 0. `Ideal`, the default, pays what a
  * node where every pool is perfect pays.
  */
enum PoolPerformance {

    /** Every pool with stake earns with an apparent performance of 1 and `eta = 1`; the blocks made
      * are not read, spec [RW-4], [RW-15].
      */
    case Ideal

    /** The ledger's formulas on the blocks made in the epoch before: a pool with no block earns
      * nothing, spec [RW-3], [RW-14], [RW-15].
      */
    case Measured
}

object PoolPerformance {

    // Java names a case of a Scala 3 enum only through the module class, so these forward as
    // static methods, `PoolPerformance.ideal()`, as BlockIssuer does, ledger-rules spec [ES-1a].

    /** [[Ideal]], for Java. */
    def ideal: PoolPerformance = Ideal

    /** [[Measured]], for Java. */
    def measured: PoolPerformance = Measured
}
