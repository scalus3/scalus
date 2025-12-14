package scalus.testing

package object kit {
    @deprecated("Use scalus.cardano.node.Emulator instead", "0.13.0")
    type NodeEmulator = scalus.cardano.node.Emulator

    @deprecated("Use scalus.cardano.node.Emulator instead", "0.13.0")
    val NodeEmulator = scalus.cardano.node.Emulator
}
