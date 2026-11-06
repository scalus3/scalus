package scalus.cardano.onchain.plutus.v1

import org.scalatest.funsuite.AnyFunSuite
import scalus.testing.budget.BudgetTable
import scalus.cardano.onchain.OnchainError
import scalus.testing.kit.EvalTestKit
import scalus.uplc.builtin.ByteString.hex

class CredentialTest extends AnyFunSuite with EvalTestKit {
    override protected def budgetTable: BudgetTable = CredentialTestBudgets

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
        assertEvalEq(hex"aa"): (c: Credential) =>
            c.scriptHashOrFail("expected script")
        .at(Credential.ScriptCredential(hex"aa"))
    }

    test("budget: pubKeyHashOrFail") {
        assertEvalEq(PubKeyHash(hex"bb")): (c: Credential) =>
            c.pubKeyHashOrFail("expected key")
        .at(Credential.PubKeyCredential(PubKeyHash(hex"bb")))
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
