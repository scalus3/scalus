package scalus.cardano.n2n.keepalive

import io.bullet.borer.{Decoder, Encoder, Reader, Writer}

/** KeepAlive mini-protocol messages (protocol id 8), per the ouroboros-network CDDL
  * (`keep-alive.cddl`):
  *
  * {{{
  * keepAliveMessage     = msgKeepAlive / msgKeepAliveResponse / msgDone
  * msgKeepAlive         = [0, word16]   ; cookie
  * msgKeepAliveResponse = [1, word16]   ; echo
  * msgDone              = [2]
  * }}}
  *
  * Cookies are `u16`: the initiator sends a cookie, the responder echoes it. Mismatch is a
  * wire-protocol violation that fires the connection root.
  */
sealed trait KeepAliveMessage

object KeepAliveMessage {

    /** Initiator-side heartbeat. `cookie` is in the u16 range (0..65535). */
    final case class MsgKeepAlive(cookie: Int) extends KeepAliveMessage {
        require(cookie >= 0 && cookie <= 0xffff, s"cookie must be u16, got $cookie")
    }

    /** Responder's echo of the initiator's cookie. */
    final case class MsgKeepAliveResponse(cookie: Int) extends KeepAliveMessage {
        require(cookie >= 0 && cookie <= 0xffff, s"cookie must be u16, got $cookie")
    }

    /** Terminates the protocol for this connection. Sent best-effort on clean shutdown. */
    case object MsgDone extends KeepAliveMessage

    // ----------------------------------------------------------------------------------------
    // CBOR codec — tag-first variant, hand-written.
    // ----------------------------------------------------------------------------------------

    given Encoder[KeepAliveMessage] with
        def write(w: Writer, m: KeepAliveMessage): Writer = m match {
            case MsgKeepAlive(cookie)         => w.writeArrayHeader(2).writeInt(0).writeInt(cookie)
            case MsgKeepAliveResponse(cookie) => w.writeArrayHeader(2).writeInt(1).writeInt(cookie)
            case MsgDone                      => w.writeArrayHeader(1).writeInt(2)
        }

    given Decoder[KeepAliveMessage] with
        def read(r: Reader): KeepAliveMessage = {
            val arrLen = r.readArrayHeader().toInt
            r.readInt() match {
                case 0 if arrLen == 2 => MsgKeepAlive(r.readInt())
                case 1 if arrLen == 2 => MsgKeepAliveResponse(r.readInt())
                case 2 if arrLen == 1 => MsgDone
                case other =>
                    r.validationFailure(s"unexpected keepAliveMessage tag=$other arrLen=$arrLen")
            }
        }
}
