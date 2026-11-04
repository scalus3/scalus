package scalus.cardano.ledger.epoch

import scalus.InteropApi

/** The Scala.js half of [[NewEpochState]]: empty. The JVM copy carries the Java getters; a
  * JavaScript caller reads the ledger state through `getLedgerState` of the JS Emulator.
  */
private[epoch] trait NewEpochStatePlatform extends InteropApi { self: NewEpochState => }

/** The Scala.js half of [[PoolDistr]]: empty, as [[NewEpochStatePlatform]]. */
private[epoch] trait PoolDistrPlatform extends InteropApi { self: PoolDistr => }

/** The Scala.js half of [[NonMyopic]]: empty, as [[NewEpochStatePlatform]]. */
private[epoch] trait NonMyopicPlatform extends InteropApi { self: NonMyopic => }
