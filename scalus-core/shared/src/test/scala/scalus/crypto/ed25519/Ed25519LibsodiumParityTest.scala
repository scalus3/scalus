package scalus.crypto.ed25519

import org.scalatest.funsuite.AnyFunSuite
import scalus.uplc.builtin.platform

/** Scalus must accept exactly the Ed25519 signatures that the Cardano node accepts. The node
  * verifies with libsodium 1.0.18; the fixture holds libsodium's verdict for every vector.
  */
class Ed25519LibsodiumParityTest extends AnyFunSuite {
    test("fixture is complete") {
        assert(LibsodiumVerdicts.all.size == 930)
        assert(LibsodiumVerdicts.all.count(_.accept) == 45)
    }

    test("verifyEd25519Signature gives libsodium's verdict on every vector") {
        val wrong =
            LibsodiumVerdicts.mismatches(v => platform.verifyEd25519Signature(v.pk, v.msg, v.sig))
        if wrong.nonEmpty then fail(s"${wrong.size} mismatches:\n${wrong.mkString("\n")}")
    }

    test("Ed25519Signer.verify gives libsodium's verdict on every vector") {
        val signer = summon[Ed25519Signer]
        val wrong = LibsodiumVerdicts.mismatches(v =>
            signer.verify(
              VerificationKey.unsafeFromByteString(v.pk),
              v.msg,
              Signature.unsafeFromByteString(v.sig)
            )
        )
        if wrong.nonEmpty then fail(s"${wrong.size} mismatches:\n${wrong.mkString("\n")}")
    }
}
