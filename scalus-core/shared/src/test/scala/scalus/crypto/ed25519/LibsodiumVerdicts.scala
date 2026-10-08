package scalus.crypto.ed25519

import scalus.uplc.builtin.{platform, ByteString}

/** Ed25519 vectors with libsodium's verdict: see resources/ed25519/libsodium-verdicts.tsv. */
object LibsodiumVerdicts {
    case class Vector(
        source: String,
        id: String,
        pk: ByteString,
        msg: ByteString,
        sig: ByteString,
        accept: Boolean
    ) {
        def name: String = s"$source#$id"
    }

    lazy val all: Seq[Vector] = {
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

    /** The vectors where `verify` disagrees with libsodium. */
    def mismatches(verify: Vector => Boolean): Seq[String] =
        all.collect {
            case v if verify(v) != v.accept =>
                s"${v.name}: libsodium ${if v.accept then "accepts" else "rejects"}"
        }
}
