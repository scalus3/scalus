package scalus.examples.setbench

import scalus.compiler.Options
import scalus.uplc.PlutusV3

private given Options = Options.release

lazy val Mpf16Contract = PlutusV3.compile(SetBenchMpf16Validator.validate)
lazy val Mpf16oContract = PlutusV3.compile(SetBenchMpf16oValidator.validate)
lazy val Mpf64Contract = PlutusV3.compile(SetBenchMpf64Validator.validate)
lazy val Mpf16bContract = PlutusV3.compile(SetBenchMpf16bValidator.validate)
lazy val Mpf64bContract = PlutusV3.compile(SetBenchMpf64bValidator.validate)
lazy val AccContract = PlutusV3.compile(SetBenchAccValidator.validate)
lazy val AmtContract = PlutusV3.compile(SetBenchAmtValidator.validate)
lazy val Amt4Contract = PlutusV3.compile(SetBenchAmt4Validator.validate)
