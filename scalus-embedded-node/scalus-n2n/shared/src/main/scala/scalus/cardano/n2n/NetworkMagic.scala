package scalus.cardano.n2n

/** Cardano network magic — the 32-bit identifier that distinguishes mainnet from each testnet. Sent
  * in the handshake's version-data so peers on different networks fail fast with a version refusal.
  * Opaque `Long` to prevent mixing with other numeric identifiers (slot, epoch, etc.) while still
  * accepting the full `u32` range the wire defines.
  */
opaque type NetworkMagic = Long

object NetworkMagic {

    /** Wrap a raw magic value. Rejects out-of-range values — the wire format is `u32`
      * (0..0xFFFFFFFF).
      */
    def apply(value: Long): NetworkMagic = {
        require(
          value >= 0L && value <= 0xffffffffL,
          s"NetworkMagic must fit in u32 (0..${0xffffffffL}), got $value"
        )
        value
    }

    val Mainnet: NetworkMagic = 764824073L
    val Preview: NetworkMagic = 2L
    val Preprod: NetworkMagic = 1L

    /** Yaci devkit devnet default magic. Placeholder — confirmed against the running container in
      * Phase 9's integration suite.
      */
    val YaciDevnet: NetworkMagic = 42L

    extension (m: NetworkMagic) def value: Long = m
}
