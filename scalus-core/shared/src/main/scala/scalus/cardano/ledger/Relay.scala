package scalus.cardano.ledger

import io.bullet.borer.NullOptions.given
import io.bullet.borer.*
import scalus.uplc.builtin.ByteString

/** Represents a relay for a stake pool in the Cardano blockchain.
  *
  * A relay is a network endpoint for a stake pool that can be one of three types:
  *   - SingleHostAddr: An IP address with optional port
  *   - SingleHostName: A DNS name with optional port
  *   - MultiHostName: A DNS name for SRV lookup
  */
enum Relay {

    /** A relay specified by an IP address.
      *
      * @param port
      *   Optional port number
      * @param ipv4
      *   Optional IPv4 address (4 bytes)
      * @param ipv6
      *   Optional IPv6 address (16 bytes)
      */
    case SingleHostAddr(port: Option[Int], ipv4: Option[ByteString], ipv6: Option[ByteString])

    /** A relay specified by a DNS name.
      *
      * @param port
      *   Optional port number
      * @param dnsName
      *   The DNS name (A or AAAA record)
      */
    case SingleHostName(port: Option[Int], dnsName: String)

    /** A relay specified by a DNS name for SRV lookup.
      *
      * @param dnsName
      *   The DNS name (SRV record)
      */
    case MultiHostName(dnsName: String)
}

object Relay {

    /** Validates that a DNS name is at most 128 characters.
      */
    private def validateDnsName(name: String): Unit = {
        require(name.length <= 128, s"DNS name must be at most 128 characters, got ${name.length}")
    }

    /** CBOR Encoder for Relay. Encodes as a tagged array based on the relay type.
      */
    given Encoder[Relay] = new Encoder[Relay] {
        def write(w: Writer, value: Relay): Writer = value match {
            case Relay.SingleHostAddr(port, ipv4, ipv6) =>
                w.writeArrayOpen(4)
                    .writeInt(0) // Tag for SingleHostAddr

                // Write port or null
                w.write(port)

                // Write IPv4 or null
                w.write(ipv4)

                // Write IPv6 or null
                w.write(ipv6)

                w.writeArrayClose()

            case Relay.SingleHostName(port, dnsName) =>
                w.writeArrayOpen(3)
                    .writeInt(1) // Tag for SingleHostName

                // Write port or null
                w.write(port)
                // Write DNS name
                w.writeString(dnsName)
                    .writeArrayClose()

            case Relay.MultiHostName(dnsName) =>
                w.writeArrayOpen(2)
                    .writeInt(2) // Tag for MultiHostName
                    .writeString(dnsName)
                    .writeArrayClose()
        }
    }

    /** CBOR Decoder for Relay. Decodes from a tagged array based on the relay type.
      */
    given Decoder[Relay] = new Decoder[Relay] {
        def read(r: Reader): Relay = {
            r.readArrayHeader()
            val tag = r.readInt()

            val relay = tag match {
                case 0 => // SingleHostAddr
                    // Read port
                    val port = r.read[Option[Int]]()

                    // Read IPv4
                    val ipv4 = r.read[Option[ByteString]]()

                    // Read IPv6
                    val ipv6 = r.read[Option[ByteString]]()

                    Relay.SingleHostAddr(port, ipv4, ipv6)

                case 1 => // SingleHostName
                    // Read port
                    val port = r.read[Option[Int]]()

                    // Read DNS name
                    val dnsName = r.readString()
                    validateDnsName(dnsName)

                    Relay.SingleHostName(port, dnsName)

                case 2 => // MultiHostName
                    // Read DNS name
                    val dnsName = r.readString()
                    validateDnsName(dnsName)

                    Relay.MultiHostName(dnsName)

                case _ =>
                    throw new IllegalArgumentException(s"Invalid Relay tag: $tag")
            }

            relay
        }
    }
}
