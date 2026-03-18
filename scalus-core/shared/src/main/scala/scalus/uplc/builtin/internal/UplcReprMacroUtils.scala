package scalus.uplc.builtin.internal

import scala.quoted.*

/** Shared macro utility for extracting `@UplcRepr` annotation values at compile time.
  *
  * Returns the `UplcRepresentation` case object name (e.g. "ProductCaseOneElement", "SumCase") by
  * pattern-matching the annotation's AST node.
  */
private[scalus] object UplcReprMacroUtils {

    def getUplcRepr(using Quotes)(symbol: quotes.reflect.Symbol): Option[String] = {
        import quotes.reflect.*
        symbol
            .getAnnotation(Symbol.requiredClass("scalus.compiler.UplcRepr"))
            .flatMap {
                case Apply(_, scala.List(arg)) =>
                    arg match
                        case Select(_, name) => Some(name)
                        case Ident(name)     => Some(name)
                        case _               => None
                case _ => None
            }
    }

    def getUplcRepr[A: Type](using Quotes): Option[String] = {
        import quotes.reflect.*
        getUplcRepr(TypeRepr.of[A].dealias.widen.typeSymbol)
    }

    /** Resolve opaque type alias via TypeRef.translucentSuperType. Returns the underlying type if
      * the type is an opaque alias, or the original type unchanged. Also handles AppliedType where
      * the type constructor is an opaque alias (e.g., opaque type Queue[A] = List[A]).
      */
    def resolveOpaqueAlias(using Quotes)(tpe: quotes.reflect.TypeRepr): quotes.reflect.TypeRepr = {
        import quotes.reflect.*
        tpe match
            case tr: TypeRef if tr.isOpaqueAlias =>
                tr.translucentSuperType
            case AppliedType(tycon, args) =>
                tycon match
                    case tr: TypeRef if tr.isOpaqueAlias =>
                        tr.translucentSuperType.appliedTo(args)
                    case _ => tpe
            case _ => tpe
    }
}
