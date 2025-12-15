package scalus.cardano.txbuilder

/** JS platform-specific operations for TxBuilder.
  *
  * On JavaScript, blocking operations are not available. Use `completeAsync` instead of `complete`.
  */
private trait TxBuilderPlatformOps { self: TxBuilder => }
