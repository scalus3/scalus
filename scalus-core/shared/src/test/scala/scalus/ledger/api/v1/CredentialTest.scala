package scalus.cardano.onchain.plutus.v1

import org.scalatest.funsuite.AnyFunSuite
import scalus.cardano.ledger.{Coin, ExUnits}
import scalus.cardano.onchain.OnchainError
import scalus.testing.kit.EvalTestKit
import scalus.uplc.builtin.ByteString.hex

class CredentialTest extends AnyFunSuite with EvalTestKit {

    test("scriptHashOrFail") {
        assert(
          (Credential.ScriptCredential(hex"aa"): Credential).scriptHashOrFail("expected script")
              == hex"aa"
        )
        assertThrows[OnchainError](
          (Credential.PubKeyCredential(PubKeyHash(hex"bb")): Credential)
              .scriptHashOrFail("expected script")
        )

        assertEvalEq(
          (Credential.ScriptCredential(hex"aa"): Credential).scriptHashOrFail("expected script"),
          hex"aa"
        )
        assertEvalFailsWithMessage[OnchainError]("expected script")(
          (Credential.PubKeyCredential(PubKeyHash(hex"bb")): Credential)
              .scriptHashOrFail("expected script")
        )
    }

    test("budget: scriptHashOrFail") {
        assertEvalWithBudgetAndFee(
          (c: Credential) => c.scriptHashOrFail("expected script"),
          Credential.ScriptCredential(hex"aa"),
          hex"aa",
          ExUnits(memory = 2660, steps = 795867),
          Coin(211)
        )
    }

    test("budget: pubKeyHashOrFail") {
        assertEvalWithBudgetAndFee(
          (c: Credential) => c.pubKeyHashOrFail("expected key"),
          Credential.PubKeyCredential(PubKeyHash(hex"bb")),
          PubKeyHash(hex"bb"),
          ExUnits(memory = 2660, steps = 795867),
          Coin(211)
        )
    }

    test("pubKeyHashOrFail") {
        assert(
          (Credential.PubKeyCredential(PubKeyHash(hex"bb")): Credential)
              .pubKeyHashOrFail("expected key") == PubKeyHash(hex"bb")
        )
        assertThrows[OnchainError](
          (Credential.ScriptCredential(hex"aa"): Credential).pubKeyHashOrFail("expected key")
        )

        assertEvalEq(
          (Credential.PubKeyCredential(PubKeyHash(hex"bb")): Credential)
              .pubKeyHashOrFail("expected key"),
          PubKeyHash(hex"bb")
        )
        assertEvalFailsWithMessage[OnchainError]("expected key")(
          (Credential.ScriptCredential(hex"aa"): Credential).pubKeyHashOrFail("expected key")
        )
    }
}
