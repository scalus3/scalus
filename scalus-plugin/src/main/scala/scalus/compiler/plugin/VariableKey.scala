package scalus.compiler.plugin

import dotty.tools.dotc.core.Symbols.Symbol

/** Key for variable bindings that distinguishes shadowed variables by their symbols */
case class VariableKey(displayName: String, symbolId: Option[Int]) {
    override def toString: String = symbolId match {
        case Some(id) => s"$displayName-$id"
        case None     => displayName
    }

    def varName: String = toString

}

object VariableKey {

    @scala.annotation.targetName("applyFromSymbol")
    def apply(name: String, symbol: Option[Symbol]): VariableKey =
        VariableKey(name, symbol.map(_.id))

    def fromName(name: String): VariableKey = VariableKey(name, None: Option[Int])
}
