package scalus.crypto.ed25519

import org.scalatest.funsuite.AnyFunSuite
import scalus.uplc.builtin.{platform, ByteString}

/** Scalus must accept exactly the Ed25519 signatures that the Cardano node accepts. The node
  * verifies with libsodium 1.0.18; the fixture `resources/ed25519/libsodium-verdicts.tsv` holds
  * libsodium's verdict for every vector.
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

    private lazy val lines: Seq[String] = {
        val path = "scalus-core/shared/src/test/resources/ed25519/libsodium-verdicts.tsv"
        new String(platform.readFile(path), "UTF-8").linesIterator.toSeq
    }

    private val Header = """# rows=(\d+) accepts=(\d+)""".r

    private lazy val vectors: Seq[Vector] =
        lines
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

    /** Fails the test, listing every vector where `verify` disagrees with libsodium. */
    private def assertParity(name: String, verify: Vector => Boolean): Unit = {
        val wrong = vectors.collect {
            case v if verify(v) != v.accept =>
                s"${v.name}: libsodium ${if v.accept then "accepts" else "rejects"}"
        }
        if wrong.nonEmpty then
            fail(s"$name: ${wrong.size} mismatches with libsodium:\n${wrong.mkString("\n")}")
    }

    test("fixture matches its '# rows=N accepts=M' header") {
        val (rows, accepts) = lines
            .collectFirst { case Header(rows, accepts) => (rows.toInt, accepts.toInt) }
            .getOrElse(fail("libsodium-verdicts.tsv has no '# rows=N accepts=M' header"))
        assert(vectors.size == rows)
        assert(vectors.count(_.accept) == accepts)
    }

    test("verifyEd25519Signature gives libsodium's verdict on every vector") {
        assertParity(
          "verifyEd25519Signature",
          v => platform.verifyEd25519Signature(v.pk, v.msg, v.sig)
        )
    }

    test("Ed25519Signer.verify gives libsodium's verdict on every vector") {
        val signer = summon[Ed25519Signer]
        assertParity(
          "Ed25519Signer.verify",
          v =>
              signer.verify(
                VerificationKey.unsafeFromByteString(v.pk),
                v.msg,
                Signature.unsafeFromByteString(v.sig)
              )
        )
    }
}
