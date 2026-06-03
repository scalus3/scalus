package scalus.compiler.plugin

import dotty.tools.dotc.plugins.{PluginPhase, StandardPlugin}

/** Phase registration for the Scala 3.3.x LTS series.
  *
  * On the LTS, `StandardPlugin.init` is the only registration hook (`initialize` was added in
  * 3.5.0). The shared [[Plugin]] supplies the phases via [[scalusPhases]].
  */
trait PluginCompat extends StandardPlugin {

    protected def scalusPhases(options: List[String]): List[PluginPhase]

    override def init(options: List[String]): List[PluginPhase] = scalusPhases(options)
}
