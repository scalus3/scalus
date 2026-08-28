package tsfixtures

import scala.scalajs.js.annotation.*

/** Members exported under a JavaScript name that differs from the Scala one. */
@JSExportTopLevel("Renames")
class Renames {

    /** Evaluates a script. */
    @JSExport("evaluate")
    def evaluateScript(hex: String): String = hex

    /** Evaluates a script against a budget. */
    @JSExport("evaluate")
    def evaluateScript(hex: String, budget: Double): String = hex

    /** Two differently named Scala methods share one exported name. */
    @JSExport("run")
    def runNumber(x: Double): Double = x

    /** The string arm of `run`. */
    @JSExport("run")
    def runString(x: String): String = x

    /** Exported under its own name and under an alias. */
    @JSExport
    @JSExport("aliased")
    def both(): Double = 1.0

    /** A field exported under a shorter name. */
    @JSExport("ver")
    val version: String = "1.0.0"

    /** Not exported at all. */
    def internal(): Double = 0.0
}

/** @JSExportAll implies a bare @JSExport on every public member; an alias adds to it. */
@JSExportTopLevel("RenamesAll")
@JSExportAll
class RenamesAll {

    /** Reachable as `plain` and as `extra`. */
    @JSExport("extra")
    def plain(): Double = 1.0
}
