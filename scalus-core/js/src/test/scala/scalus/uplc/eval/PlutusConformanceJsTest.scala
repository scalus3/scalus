package scalus.uplc.eval

/** JS-specific Plutus Conformance tests.
  *
  * BLS12-381 goes through `@noble`, not blst, so the DST-length skips the JVM and Native suites
  * carry do not apply here: this suite runs the whole corpus.
  */
class PlutusConformanceJsTest extends PlutusConformanceTest {

    /** Guards the conformance claims published in `scalus-cardano-ledger/js/src/main/npm/README.md`
      * and on scalus.org.
      *
      * The prose names no counts, so a corpus update does not make it wrong. It claims three
      * things, and this test pins each of them: every case runs (none skipped), the flat-encoded
      * form runs too, and every case the reference evaluates successfully has a budget to compare
      * exactly.
      */
    test("published conformance claims are still accurate") {
        assert(
          ignoredCases.isEmpty,
          s"the JS build now skips ${ignoredCases.size} case(s), but the README says none are " +
              s"skipped: ${ignoredCases.keys.mkString(", ")}"
        )
        assert(
          runsFlatCases && discoveredFlatCases.nonEmpty,
          "the README and the site say the flat-encoded cases run too"
        )
        // expectationOf fails the test for a case that expects a term but has no budget file, which
        // would break the claim that every successful case has its budget compared exactly.
        val termCases = discoveredCases.count(expectationOf(_) == CaseExpectation.TermAndBudget)
        assert(termCases > 0, "no case has a term and budget to compare")
    }
}
