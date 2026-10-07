package scalus.uplc.eval

import scalus.cardano.ledger.ScriptCache
import scalus.interop.TsIgnore
import scalus.utils.scalajs.internal.*

import scala.scalajs.js
import scala.scalajs.js.annotation.JSExportTopLevel

/** Plutus scripts kept across evaluations, so a script that many transactions carry or reference is
  * hashed and decoded once. Give one cache to several `TxEvaluator`s through their `scriptCache`
  * option, for example one evaluator per slot configuration.
  *
  * When the cache is full, the least recently used script is dropped.
  *
  * ```ts
  * const scripts = new ScriptCache(64)
  * const ev = new TxEvaluator({ slotConfig, costModels, protocolMajorVersion: 11, scriptCache: scripts })
  * ```
  *
  * @param maxEntries
  *   how many scripts to keep, a positive integer
  * @throws TypeError
  *   if `maxEntries` is not a positive integer
  */
@JSExportTopLevel("ScriptCache")
class JScriptCache(maxEntries: Double) extends js.Object {

    @TsIgnore private[eval] final val underlying: ScriptCache = {
        val n = intOf(maxEntries, "maxEntries")
        if n <= 0 then typeError(s"maxEntries must be positive, got $n")
        new ScriptCache(n)
    }

    /** How many scripts the cache holds. */
    def size: Int = underlying.size
}

private[eval] object JScriptCache {

    /** The cache an options field holds, or null when it is absent. Throws a `TypeError` for
      * anything else.
      */
    def of(value: js.Any): ScriptCache | Null =
        if js.isUndefined(value) || value == null then null
        else if value.isInstanceOf[JScriptCache] then value.asInstanceOf[JScriptCache].underlying
        else typeError("scriptCache must be a ScriptCache")
}
