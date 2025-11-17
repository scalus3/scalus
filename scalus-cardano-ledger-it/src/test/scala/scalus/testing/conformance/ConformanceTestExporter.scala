package scalus.testing.conformance

import com.github.plokhotnyuk.jsoniter_scala.core.*
import scalus.testing.conformance.ConformanceTestSchema.*
import scalus.testing.conformance.CardanoLedgerVectorImporter.*

import java.nio.file.{Files, Path}
import scala.util.{Failure, Success, Try}

/** Utilities for exporting and converting conformance test data
  *
  * Provides tools for:
  * - Exporting test results to various formats
  * - Converting between test data formats
  * - Generating test suite summaries
  * - Aggregating results from multiple test runs
  */
object ConformanceTestExporter {

  /** Export test suite to JSON file
    *
    * @param suite
    *   The conformance test suite to export
    * @param outputPath
    *   Path to output JSON file
    */
  def exportTestSuite(suite: ConformanceTestSuite, outputPath: Path): Unit = {
    val json = writeToString(suite, WriterConfig.withIndentionStep(2))
    Files.write(outputPath, json.getBytes(java.nio.charset.StandardCharsets.UTF_8))
    println(s"Test suite exported to: $outputPath")
  }

  /** Export test results to JSON file
    *
    * @param results
    *   Test results to export
    * @param outputPath
    *   Path to output JSON file
    */
  def exportResults(results: ConformanceTestResults, outputPath: Path): Unit = {
    val json = writeToString(results, WriterConfig.withIndentionStep(2))
    Files.write(outputPath, json.getBytes(java.nio.charset.StandardCharsets.UTF_8))
    println(s"Test results exported to: $outputPath")
  }

  /** Generate summary report from test results
    *
    * @param results
    *   Test results to summarize
    * @return
    *   Summary report as formatted string
    */
  def generateSummaryReport(results: ConformanceTestResults): String = {
    val sb = new StringBuilder

    sb.append("=" * 80 + "\n")
    sb.append("CONFORMANCE TEST SUMMARY REPORT\n")
    sb.append("=" * 80 + "\n\n")

    // Overall statistics
    sb.append("Overall Results:\n")
    sb.append(f"  Total Tests:    ${results.totalTests}%5d\n")
    sb.append(f"  Passed:         ${results.passedTests}%5d (${results.passRate}%5.1f%%)\n")
    sb.append(f"  Failed:         ${results.failedTests}%5d\n")
    sb.append(f"  Skipped:        ${results.skippedTests}%5d\n")
    sb.append(f"  Total Time:     ${results.totalTimeMs}%5d ms\n")
    sb.append(f"  Average Time:   ${results.averageTimeMs}%5.1f ms/test\n\n")

    // Test vector statistics
    sb.append("Test Vector Statistics:\n")
    sb.append(f"  Total Vectors:  ${results.statistics.totalVectors}%5d\n")
    sb.append(f"  Success Cases:  ${results.statistics.successVectors}%5d\n")
    sb.append(f"  Failure Cases:  ${results.statistics.failureVectors}%5d\n")
    sb.append(f"  Categories:     ${results.statistics.categoryCounts.size}%5d\n")
    sb.append(f"  Tags:           ${results.statistics.tagCounts.size}%5d\n\n")

    // Top categories
    sb.append("Top Test Categories:\n")
    results.statistics.categoryCounts.toSeq
      .sortBy(-_._2)
      .take(10)
      .foreach { case (cat, count) =>
        sb.append(f"  $cat%-60s: $count%5d\n")
      }
    sb.append("\n")

    // Tag distribution
    sb.append("Tag Distribution:\n")
    results.statistics.tagCounts.toSeq.sortBy(-_._2).foreach { case (tag, count) =>
      sb.append(f"  $tag%-20s: $count%5d\n")
    }
    sb.append("\n")

    // Failed tests (if any)
    if (results.failedTests > 0) {
      sb.append(s"Failed Tests (showing first 20):\n")
      results.results
        .filter(!_.passed)
        .take(20)
        .foreach { result =>
          sb.append(s"  ${result.testId}\n")
          result.errors.take(2).foreach(err => sb.append(s"    - $err\n"))
        }
      if (results.failedTests > 20) {
        sb.append(s"  ... and ${results.failedTests - 20} more\n")
      }
      sb.append("\n")
    }

    sb.append("=" * 80 + "\n")
    sb.toString
  }

  /** Convert raw test vectors to conformance test suite
    *
    * @param vectorsPath
    *   Path to directory containing raw test vectors
    * @param outputPath
    *   Path to output JSON file for test suite
    * @param suiteName
    *   Name for the test suite
    * @param genesis
    *   Genesis configuration to use for all tests
    */
  def convertVectorsToTestSuite(
      vectorsPath: Path,
      outputPath: Path,
      suiteName: String,
      genesis: GenesisConfig
  ): Unit = {
    println(s"Converting test vectors from: $vectorsPath")

    val vectors = loadAllVectors(vectorsPath)
    val stats = computeStats(vectors)

    println(s"Loaded ${vectors.size} test vectors")
    println(stats.report)

    // Convert vectors to test cases
    val testCases = vectors.map { case (path, vector) =>
      val testId = path.getFileName.toString
      toTransactionTestCase(testId, vector, genesis)
    }

    // Create test suite
    val suite = ConformanceTestSuite(
      metadata = TestSuiteMetadata(
        name = suiteName,
        version = "1.0",
        description = s"Converted from ${vectors.size} test vectors",
        source = "cardano-ledger",
        generatedAt = Some(java.time.Instant.now().toString),
        generatedBy = Some("Scalus ConformanceTestExporter")
      ),
      transactionTests = testCases,
      blockTests = List.empty
    )

    // Export suite
    exportTestSuite(suite, outputPath)
    println(s"Converted ${testCases.size} test cases")
  }

  /** Aggregate results from multiple test runs
    *
    * @param resultsPaths
    *   Paths to result JSON files
    * @return
    *   Aggregated results
    */
  def aggregateResults(resultsPaths: List[Path]): ConformanceTestResults = {
    val allResults = resultsPaths.flatMap { path =>
      Try(ConformanceTestRunner.loadResults(path)) match {
        case Success(results) => Some(results)
        case Failure(e) =>
          println(s"Warning: Failed to load results from $path: ${e.getMessage}")
          None
      }
    }

    if (allResults.isEmpty) {
      throw new IllegalArgumentException("No valid result files found")
    }

    // Aggregate all results
    val totalTests = allResults.map(_.totalTests).sum
    val passedTests = allResults.map(_.passedTests).sum
    val failedTests = allResults.map(_.failedTests).sum
    val skippedTests = allResults.map(_.skippedTests).sum
    val totalTime = allResults.map(_.totalTimeMs).sum
    val allTestResults = allResults.flatMap(_.results)

    // Use statistics from first result (they should be the same)
    val stats = allResults.head.statistics

    ConformanceTestResults(
      totalTests = totalTests,
      passedTests = passedTests,
      failedTests = failedTests,
      skippedTests = skippedTests,
      totalTimeMs = totalTime,
      results = allTestResults,
      statistics = stats
    )
  }

  /** Export results to CSV format
    *
    * @param results
    *   Test results to export
    * @param outputPath
    *   Path to output CSV file
    */
  def exportResultsToCSV(results: ConformanceTestResults, outputPath: Path): Unit = {
    val csv = new StringBuilder
    csv.append("TestID,Passed,ExecutionTimeMs,ErrorCount\n")

    results.results.foreach { result =>
      csv.append(s"${result.testId},")
      csv.append(s"${result.passed},")
      csv.append(s"${result.executionTimeMs},")
      csv.append(s"${result.errors.size}\n")
    }

    Files.write(outputPath, csv.toString.getBytes(java.nio.charset.StandardCharsets.UTF_8))
    println(s"Results exported to CSV: $outputPath")
  }

  /** Generate HTML report from test results
    *
    * @param results
    *   Test results to report
    * @param outputPath
    *   Path to output HTML file
    */
  def exportResultsToHTML(results: ConformanceTestResults, outputPath: Path): Unit = {
    val html = new StringBuilder

    html.append("<!DOCTYPE html>\n")
    html.append("<html>\n<head>\n")
    html.append("<title>Conformance Test Results</title>\n")
    html.append("<style>\n")
    html.append("body { font-family: Arial, sans-serif; margin: 20px; }\n")
    html.append("h1 { color: #333; }\n")
    html.append(".summary { background: #f5f5f5; padding: 15px; margin: 10px 0; }\n")
    html.append(
      ".passed { background: #d4edda; padding: 10px; margin: 5px 0; border-left: 4px solid #28a745; }\n"
    )
    html.append(
      ".failed { background: #f8d7da; padding: 10px; margin: 5px 0; border-left: 4px solid #dc3545; }\n"
    )
    html.append("table { border-collapse: collapse; width: 100%; }\n")
    html.append("th, td { border: 1px solid #ddd; padding: 8px; text-align: left; }\n")
    html.append("th { background-color: #4CAF50; color: white; }\n")
    html.append("</style>\n")
    html.append("</head>\n<body>\n")

    html.append("<h1>Conformance Test Results</h1>\n")

    // Summary
    html.append("<div class='summary'>\n")
    html.append(s"<h2>Summary</h2>\n")
    html.append(s"<p><strong>Total Tests:</strong> ${results.totalTests}</p>\n")
    html.append(
      s"<p><strong>Passed:</strong> ${results.passedTests} (${results.passRate}%)</p>\n"
    )
    html.append(s"<p><strong>Failed:</strong> ${results.failedTests}</p>\n")
    html.append(s"<p><strong>Total Time:</strong> ${results.totalTimeMs} ms</p>\n")
    html.append(s"<p><strong>Average Time:</strong> ${results.averageTimeMs} ms/test</p>\n")
    html.append("</div>\n")

    // Results table
    html.append("<h2>Test Results</h2>\n")
    html.append("<table>\n")
    html.append("<tr><th>Test ID</th><th>Status</th><th>Time (ms)</th><th>Errors</th></tr>\n")

    results.results.foreach { result =>
      val status = if (result.passed) "PASSED" else "FAILED"
      val rowClass = if (result.passed) "passed" else "failed"
      html.append(s"<tr class='$rowClass'>\n")
      html.append(s"<td>${result.testId}</td>\n")
      html.append(s"<td>$status</td>\n")
      html.append(s"<td>${result.executionTimeMs}</td>\n")
      html.append(s"<td>${result.errors.mkString("; ")}</td>\n")
      html.append("</tr>\n")
    }

    html.append("</table>\n")
    html.append("</body>\n</html>\n")

    Files.write(outputPath, html.toString.getBytes(java.nio.charset.StandardCharsets.UTF_8))
    println(s"HTML report exported to: $outputPath")
  }
}
