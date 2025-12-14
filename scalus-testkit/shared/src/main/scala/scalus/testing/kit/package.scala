package scalus.testing

package object kit {
    @deprecated("Use scalus.cardano.node.NodeEmulator instead", "0.9.0")
    type NodeEmulator = scalus.cardano.node.NodeEmulator

    @deprecated("Use scalus.cardano.node.NodeEmulator instead", "0.9.0")
    val NodeEmulator = scalus.cardano.node.NodeEmulator
}
