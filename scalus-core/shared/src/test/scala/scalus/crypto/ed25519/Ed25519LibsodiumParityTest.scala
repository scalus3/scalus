package scalus.crypto.ed25519

import org.scalatest.funsuite.AnyFunSuite
import scalus.uplc.builtin.{platform, ByteString}

/** Scalus must accept exactly the Ed25519 signatures that the Cardano node accepts. The node
  * verifies with libsodium 1.0.18; the fixture holds libsodium's verdict for every vector.
  */
class Ed25519LibsodiumParityTest extends AnyFunSuite {
    private case class Vector(
        source: String,
        id: String,
        pk: ByteString,
        msg: ByteString,
        sig: ByteString,
        accept: Boolean
    ) {
        def name: String = s"$source#$id"
    }

    private val vectors: Seq[Vector] = {
        val path = "scalus-core/shared/src/test/resources/ed25519/libsodium-verdicts.tsv"
        new String(platform.readFile(path), "UTF-8").linesIterator
            .filterNot(line => line.isEmpty || line.startsWith("#"))
            .map { line =>
                val Array(source, id, pk, msg, sig, verdict) = line.split('\t'): @unchecked
                Vector(
                  source,
                  id,
                  ByteString.fromHex(pk),
                  ByteString.fromHex(msg),
                  ByteString.fromHex(sig),
                  verdict == "accept"
                )
            }
            .toSeq
    }

    private def mismatches(verify: Vector => Boolean): Seq[String] =
        vectors.collect {
            case v if verify(v) != v.accept =>
                s"${v.name}: libsodium ${if v.accept then "accepts" else "rejects"}"
        }

    test("fixture is complete") {
        assert(vectors.size == 930)
        assert(vectors.count(_.accept) == 45)
    }

    test("verifyEd25519Signature gives libsodium's verdict on every vector") {
        val wrong = mismatches(v => platform.verifyEd25519Signature(v.pk, v.msg, v.sig))
        if wrong.nonEmpty then fail(s"${wrong.size} mismatches:\n${wrong.mkString("\n")}")
    }

    test("Ed25519Signer.verify gives libsodium's verdict on every vector") {
        val signer = summon[Ed25519Signer]
        val wrong = mismatches(v =>
            signer.verify(
              VerificationKey.unsafeFromByteString(v.pk),
              v.msg,
              Signature.unsafeFromByteString(v.sig)
            )
        )
        if wrong.nonEmpty then fail(s"${wrong.size} mismatches:\n${wrong.mkString("\n")}")
    }
}
