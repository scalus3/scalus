package scalus.tsexport

import scala.quoted.*

/** Maps Scala `TypeRepr`s to [[TsType]] following Scala.js export semantics.
  *
  * @param knownNames
  *   typeSymbol.fullName -> emitted TS name, for exported/chased declarations
  * @param chase
  *   called when an unknown js.Object subtype is referenced; returns its future TS name and
  *   registers it for interface emission, or None if the type is excluded from the export
  */
class TypeMapper[Q <: Quotes & Singleton](using val quotes: Q)(
    knownNames: Map[String, String],
    chase: quotes.reflect.Symbol => Option[String]
) {
    import quotes.reflect.*
    import TsType.*

    private val jsObjectSym = Symbol.requiredClass("scala.scalajs.js.Object")

    private val ground: Map[String, TsType] = Map(
      "scala.Boolean" -> Named("boolean"),
      "scala.Byte" -> Named("number"),
      "scala.Short" -> Named("number"),
      "scala.Int" -> Named("number"),
      "scala.Float" -> Named("number"),
      "scala.Double" -> Named("number"),
      "java.lang.String" -> Named("string"),
      "scala.Unit" -> Named("void"),
      "scala.Null" -> Named("null"),
      "scala.Nothing" -> Named("never"),
      "scala.Any" -> Named("any"),
      "scala.scalajs.js.BigInt" -> Named("bigint"),
      "scala.scalajs.js.Dynamic" -> Named("any"),
      "scala.scalajs.js.Any" -> Named("any"),
      "scala.scalajs.js.Object" -> Named("object"),
      "scala.scalajs.js.Date" -> Named("Date"),
      "scala.scalajs.js.RegExp" -> Named("RegExp")
    )

    private def err(context: String, message: String): Left[ExportError, TsType] =
        Left(ExportError(context, message))

    def map(tpe: TypeRepr, context: String): Either[ExportError, TsType] = {
        val dealiased = tpe.dealias
        dealiased match
            case AnnotatedType(underlying, _) => map(underlying, context)
            case ByNameType(underlying)       => map(underlying, context)
            case OrType(_, _)                 => mapUnion(flattenOr(dealiased), context)
            case AndType(_, _)                => mapIntersect(flattenAnd(dealiased), context)
            case AppliedType(base, args)      => mapApplied(base, args, context, dealiased)
            case _                            => mapSimple(dealiased, context)
    }

    private def flattenOr(tpe: TypeRepr): List[TypeRepr] = tpe match
        case OrType(a, b) => flattenOr(a.dealias) ++ flattenOr(b.dealias)
        case other        => List(other)

    private def flattenAnd(tpe: TypeRepr): List[TypeRepr] = tpe match
        case AndType(a, b) => flattenAnd(a.dealias) ++ flattenAnd(b.dealias)
        case other         => List(other)

    /** An intersection maps to a TypeScript intersection. Every branch is mapped, so a branch that
      * is not exportable is reported rather than one side being silently picked.
      */
    private def mapIntersect(
        branches: List[TypeRepr],
        context: String
    ): Either[ExportError, TsType] = {
        val mapped = branches.foldLeft[Either[ExportError, List[TsType]]](Right(Nil)) {
            (acc, branch) => for { ts <- acc; t <- map(branch, context) } yield ts :+ t
        }
        mapped.map(_.distinct match
            case single :: Nil => single
            case many          => Intersect(many))
    }

    private def mapUnion(
        branches: List[TypeRepr],
        context: String
    ): Either[ExportError, TsType] = {
        val mapped = branches.foldLeft[Either[ExportError, List[TsType]]](Right(Nil)) {
            (acc, branch) =>
                for
                    ts <- acc
                    t <- branch.typeSymbol.fullName match
                        case "scala.Unit" => Right(Named("undefined"))
                        case "scala.Null" => Right(Named("null"))
                        case _            => map(branch, context)
                yield ts :+ t
        }
        mapped.map { ts =>
            // dedupe preserving order; null/undefined sort last
            val distinct = ts.distinct
            val (special, regular) =
                distinct.partition(t => t == Named("null") || t == Named("undefined"))
            val ordered = regular ++ special.sortBy { case Named(n) => n; case _ => "" }
            ordered match
                case single :: Nil => single
                case many          => Union(many)
        }
    }

    private val funRe = raw"scala\.scalajs\.js\.Function(\d+)".r
    private val thisFunRe = raw"scala\.scalajs\.js\.ThisFunction(\d+)".r

    private def mapApplied(
        base: TypeRepr,
        args: List[TypeRepr],
        context: String,
        original: TypeRepr
    ): Either[ExportError, TsType] = {
        def mapAll(ts: List[TypeRepr]): Either[ExportError, List[TsType]] =
            ts.foldLeft[Either[ExportError, List[TsType]]](Right(Nil)) { (acc, t) =>
                for { list <- acc; m <- map(t, context) } yield list :+ m
            }
        base.typeSymbol.fullName match
            case "scala.scalajs.js.Array" =>
                map(args.head, context).map(Arr(_))
            case "scala.scalajs.js.Dictionary" =>
                map(args.head, context).map(Index(_))
            case "scala.scalajs.js.Promise" =>
                mapAll(args).map(Generic("Promise", _))
            case "scala.scalajs.js.UndefOr" =>
                map(args.head, context).map(t => Union(List(t, Named("undefined"))))
            case "scala.scalajs.js.$bar" | "scala.scalajs.js.|" =>
                mapUnion(args.flatMap(a => flattenOr(a.dealias)), context)
            case funRe(_) =>
                mapAll(args).map { mapped =>
                    val params = mapped.init.zipWithIndex.map { case (t, i) =>
                        TsParam(s"arg$i", t, optional = false)
                    }
                    Func(params, mapped.last)
                }
            case thisFunRe(_) =>
                err(context, s"js.ThisFunction is not supported; use js.FunctionN")
            case _ =>
                // A generic declaration referenced with type arguments: resolve the base name
                // the same way a bare reference is resolved, then re-apply the arguments so
                // `Box[String]` emits `Box<string>` and not a bare `Box`.
                mapSimple(original, context).flatMap {
                    case Named(n) => mapAll(args).map(Generic(n, _))
                    case other    => Right(other)
                }
    }

    private def mapSimple(tpe: TypeRepr, context: String): Either[ExportError, TsType] = {
        val sym = tpe.typeSymbol
        val fullName = sym.fullName
        ground.get(fullName) match
            case Some(t) => Right(t)
            case None =>
                if fullName == "scala.Long" then
                    err(
                      context,
                      "type Long has no JavaScript representation; use Double, js.BigInt, or @TsType"
                    )
                else if fullName == "scala.Option" || fullName.startsWith("scala.Option[") then
                    err(context, s"type ${tpe.show} is not exportable; use js.UndefOr instead")
                else if fullName.startsWith(
                      "scala.collection."
                    ) || fullName == "scala.collection.immutable.List"
                then
                    err(
                      context,
                      s"type ${tpe.show} is not exportable; use js.Array or js.Dictionary instead"
                    )
                else if fullName.startsWith("scala.scalajs.js.typedarray.") then
                    Right(Named(sym.name))
                else if knownNames.contains(fullName) then Right(Named(knownNames(fullName)))
                else if sym.isTypeParam then Right(Named(sym.name))
                else if sym.isClassDef && tpe.baseClasses.contains(jsObjectSym) then
                    chase(sym) match
                        case Some(name) => Right(Named(name))
                        case None =>
                            err(
                              context,
                              s"type ${tpe.show} is excluded from the TypeScript export but is " +
                                  "referenced by an exported member"
                            )
                else
                    err(
                      context,
                      s"type ${tpe.show} is not exportable to TypeScript; export it, use a js.* type, or add @TsType"
                    )
    }
}
