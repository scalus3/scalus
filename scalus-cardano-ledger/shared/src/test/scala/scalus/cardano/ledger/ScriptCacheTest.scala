package scalus.cardano.ledger

import org.scalatest.funsuite.AnyFunSuite
import scalus.uplc.builtin.ByteString

class ScriptCacheTest extends AnyFunSuite {

    private def bytes(hex: String) = ByteString.fromHex(hex)

    test("an equal script comes back as the first instance, with its hash already computed") {
        val cache = new ScriptCache(4)
        val first = Script.PlutusV3(bytes("4e4d01000033222220051200120011"))
        val hash = first.scriptHash
        val again = Script.PlutusV3(bytes("4e4d01000033222220051200120011"))
        assert(again ne first)
        assert(cache.intern(first) eq first)
        assert(cache.intern(again) eq first)
        assert(cache.intern(again).scriptHash == hash)
        assert(cache.size == 1)
    }

    test("the same bytes in another language are another script") {
        val cache = new ScriptCache(4)
        val v2 = Script.PlutusV2(bytes("4e4d01000033222220051200120011"))
        val v3 = Script.PlutusV3(bytes("4e4d01000033222220051200120011"))
        assert(cache.intern(v2) eq v2)
        assert(cache.intern(v3) eq v3)
        assert(v2.scriptHash != v3.scriptHash)
        assert(cache.size == 2)
    }

    test("the least recently used script is dropped when the cache is full") {
        val cache = new ScriptCache(2)
        val a = Script.PlutusV3(bytes("01"))
        val b = Script.PlutusV3(bytes("02"))
        cache.intern(a)
        cache.intern(b)
        cache.intern(Script.PlutusV3(bytes("01"))) // a is now the most recently used
        cache.intern(Script.PlutusV3(bytes("03"))) // drops b
        assert(cache.size == 2)
        assert(cache.intern(Script.PlutusV3(bytes("01"))) eq a)
        assert(cache.intern(Script.PlutusV3(bytes("02"))) ne b)
    }

    test("native scripts pass through uncached") {
        val cache = new ScriptCache(4)
        val native = Script.Native(Timelock.TimeStart(0))
        assert(cache.intern(native) eq native)
        assert(cache.size == 0)
    }

    test("a cache must hold at least one script") {
        assertThrows[IllegalArgumentException](new ScriptCache(0))
    }
}
