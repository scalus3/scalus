package scalus.compiler.plugin

import dotty.tools.dotc.core.Contexts.Context
import dotty.tools.dotc.plugins.{PluginPhase, StandardPlugin}

/** Phase registration for the Scala 3.5+ series (3.8.x).
  *
  * `StandardPlugin.init` is deprecated since 3.5.0 in favour of `initialize`, which can access the
  * compiler `Context`. The shared [[Plugin]] supplies the phases via [[scalusPhases]].
  */
trait PluginCompat extends StandardPlugin {

    protected def scalusPhases(options: List[String]): List[PluginPhase]

    override def initialize(options: List[String])(using Context): List[PluginPhase] =
        scalusPhases(options)
}
