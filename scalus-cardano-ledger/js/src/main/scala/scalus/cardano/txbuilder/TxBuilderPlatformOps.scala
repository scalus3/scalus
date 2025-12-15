package scalus.cardano.txbuilder

/** JS platform-specific operations for TxBuilder.
  *
  * On JavaScript, only the async `complete` method (returning `Future`) is available. The blocking
  * overload with `timeout` parameter is JVM-only.
  */
private trait TxBuilderPlatformOps { self: TxBuilder => }
