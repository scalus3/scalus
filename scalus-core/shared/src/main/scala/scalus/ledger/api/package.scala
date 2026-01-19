package scalus.ledger

package object api {

    type ScriptContext = v1.ScriptContext | v2.ScriptContext | v3.ScriptContext

    object ScriptContext {
        def foldMap[T](sc: ScriptContext)(
            f1: v1.ScriptContext => T,
            f2: v2.ScriptContext => T,
            f3: v3.ScriptContext => T
        ): T = sc match {
            case sc1: v1.ScriptContext => f1(sc1)
            case sc2: v2.ScriptContext => f2(sc2)
            case sc3: v3.ScriptContext => f3(sc3)
        }
    }

}
