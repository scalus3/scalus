package scalus.cardano.node

/** JS platform-specific operations for AsyncProvider.
  *
  * On JavaScript, blocking operations like `toSync`, `awaitConfirmation`, and `submitAndWait` are
  * not available. Use the async methods directly instead.
  */
private trait AsyncProviderPlatformOps { self: AsyncProvider => }
