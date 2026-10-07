package scalus.cardano.ledger

import scalus.uplc.builtin.ByteString

import java.util

/** Keeps Plutus scripts across transactions, so a script that many transactions carry or reference
  * is hashed and decoded once.
  *
  * A [[PlutusScript]] caches its hash and its decoded program itself, but every decoded transaction
  * brings new script instances, so without a cache each evaluation hashes and decodes every script
  * again. [[intern]] hands back the first instance seen with the same language and bytes, and with
  * it the work already done on it. One cache can serve several evaluators, for example one per slot
  * configuration.
  *
  * The least recently used script is dropped when the cache is full. Safe to share between threads.
  *
  * @param maxEntries
  *   how many scripts to keep
  */
final class ScriptCache(maxEntries: Int) {
    require(maxEntries > 0, s"maxEntries must be positive, got $maxEntries")

    private val entries =
        new util.LinkedHashMap[(Language, ByteString), PlutusScript](16, 0.75f, true) {
            override def removeEldestEntry(
                eldest: util.Map.Entry[(Language, ByteString), PlutusScript]
            ): Boolean = this.size() > maxEntries
        }

    /** The cached instance of `script`, or `script` itself, now cached, if none is. Native scripts
      * are returned as they are.
      */
    def intern(script: Script): Script = script match
        case plutus: PlutusScript =>
            val key = (plutus.language, plutus.script)
            synchronized {
                val cached = entries.get(key)
                if cached != null then cached
                else
                    entries.put(key, plutus)
                    plutus
            }
        case other => other

    /** How many scripts the cache holds. */
    def size: Int = synchronized(entries.size())
}
