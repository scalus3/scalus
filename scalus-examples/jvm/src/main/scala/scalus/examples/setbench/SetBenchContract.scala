package scalus.examples.setbench

import scalus.compiler.Options
import scalus.uplc.PlutusV3

private given Options = Options.release

lazy val Mpf16oContract = PlutusV3.compile(SetBenchMpf16oValidator.validate)
lazy val Mpf16oLightContract = PlutusV3.compile(SetBenchMpf16oLightValidator.validate)
lazy val Mpf16bContract = PlutusV3.compile(SetBenchMpf16bValidator.validate)
lazy val Mpf16bLightContract = PlutusV3.compile(SetBenchMpf16bLightValidator.validate)
lazy val AccContract = PlutusV3.compile(SetBenchAccValidator.validate)
lazy val ImtContract = PlutusV3.compile(SetBenchImtValidator.validate)
