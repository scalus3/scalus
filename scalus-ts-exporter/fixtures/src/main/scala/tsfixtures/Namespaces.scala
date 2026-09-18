package tsfixtures

import scalus.interop.TsName

import scala.scalajs.js
import scala.scalajs.js.annotation.*

/** An options interface and its factory share one TypeScript export name. */
@TsName("EvaluationOptions")
trait JsEvaluationOptions extends js.Object {
    val plutusVersion: Int
}

@JSExportTopLevel("EvaluationOptions")
object EvaluationOptionsFactory {
    @JSExport
    def plutusV3(): JsEvaluationOptions = new JsEvaluationOptions {
        val plutusVersion: Int = 3
    }
}

/** A function can also share a name with an interface, including through an alias. */
@TsName("CallableOptions")
trait JsCallableOptions extends js.Object {
    val enabled: Boolean
}

object CallableOptionsFactory {
    @JSExportTopLevel("CallableOptions")
    @JSExportTopLevel("LegacyCallableOptions")
    def make(value: Boolean): JsCallableOptions = new JsCallableOptions {
        val enabled: Boolean = value
    }
}
