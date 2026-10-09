package scalus.builtin

/** JS-specific backward compatibility exports for scalus.builtin.
  *
  * NodeJsPlatformSpecific has been moved to [[scalus.uplc.builtin.NodeJsPlatformSpecific]]. This
  * object provides deprecated aliases for migration.
  */
@deprecated("use scalus.uplc.builtin.NodeJsPlatformSpecific instead", "1.3.0")
object NodeJsPlatformSpecific extends scalus.uplc.builtin.NodeJsPlatformSpecific
