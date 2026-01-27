package scalus.builtin

/** Backward compatibility for JVM-specific builtin types.
  */
object JVMPlatformSpecificBackwardCompat {
    private val version = "0.14.2"

    @deprecated("Use scalus.uplc.builtin.JVMPlatformSpecific instead", version)
    type JVMPlatformSpecific = scalus.uplc.builtin.JVMPlatformSpecific
}

// Re-export at package level
@deprecated("Use scalus.uplc.builtin.JVMPlatformSpecific instead", "0.14.2")
type JVMPlatformSpecific = scalus.uplc.builtin.JVMPlatformSpecific
