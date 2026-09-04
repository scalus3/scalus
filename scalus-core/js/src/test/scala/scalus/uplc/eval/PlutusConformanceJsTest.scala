package scalus.uplc.eval

/** JS-specific Plutus Conformance tests.
  *
  * BLS12-381 goes through `@noble`, not blst, so the DST-length skips the JVM and Native suites
  * carry do not apply here: this suite runs the whole corpus.
  */
class PlutusConformanceJsTest extends PlutusConformanceTest {

    /** Guards the numbers published in `scalus-cardano-ledger/js/src/main/npm/README.md` and on
      * scalus.org.
      *
      * "999 of 999, none skipped" is the strongest claim Scalus makes to a JavaScript audience, and
      * prose rots silently. The split matters as much as the total: only the term-bearing cases
      * have their execution budget compared with the reference, and the published wording says how
      * many those are. Pinning the total alone let two different wrong splits get published, so
      * every number the prose states is pinned here. If the corpus moves, this fails in the file
      * whose author is best placed to update the README and the site in the same commit.
      */
    test("published conformance counts are still accurate") {
        assert(
          discoveredCases.size == 999,
          s"corpus has ${discoveredCases.size} evaluation cases; the README and the site say 999"
        )
        assert(
          ignoredCases.isEmpty,
          s"the JS build now skips ${ignoredCases.size} case(s), but the README says none are " +
              s"skipped: ${ignoredCases.keys.mkString(", ")}"
        )
        val counts = discoveredCases.groupBy(expectationOf).view.mapValues(_.size).toMap
        def count(expectation: CaseExpectation): Int = counts.getOrElse(expectation, 0)
        assert(
          count(CaseExpectation.TermAndBudget) == 724,
          s"${count(CaseExpectation.TermAndBudget)} cases have their term and budget asserted; " +
              s"the README and the site say 724"
        )
        assert(
          count(CaseExpectation.EvaluationFailure) == 220,
          s"${count(CaseExpectation.EvaluationFailure)} cases expect 'evaluation failure'; the " +
              s"README and the site say 220"
        )
        assert(
          count(CaseExpectation.ParseError) == 55,
          s"${count(CaseExpectation.ParseError)} cases expect 'parse error'; the README and the " +
              s"site say 55"
        )
    }
}
