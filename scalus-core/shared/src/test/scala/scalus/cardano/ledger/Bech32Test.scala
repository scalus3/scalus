package scalus.cardano.ledger

import org.scalatest.Inspectors
import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers

import scala.util.control.NonFatal

class Bech32Test extends AnyFunSuite with Matchers with Inspectors {

    // https://github.com/bitcoin/bips/blob/master/bip-0173.mediawiki#test-vectors
    val testData: Seq[String] = Seq(
      "A12UEL5L",
      "an83characterlonghumanreadablepartthatcontainsthenumber1andtheexcludedcharactersbio1tt5tgs",
      "abcdef1qpzry9x8gf2tvdw0s3jn54khce6mua7lmqqqxw",
      "11qqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqc8247j",
      "split1checkupstagehandshakeupstreamerranterredcaperred2y9e3w"
    )

    test("Bech32 decode") {
        forAll(testData) { s =>
            try Bech32.decode(s)
            catch
                case NonFatal(e) =>
                    fail(s"Expected to decode $s, but got exception: ${e.getMessage}")

        }
    }

    test("Bech32 must round trip") {
        forAll(testData) { s =>
            val decoded = Bech32.decode(s)
            assert(Bech32.encode(decoded.hrp, decoded.data) == s.toLowerCase)
        }
    }
}
