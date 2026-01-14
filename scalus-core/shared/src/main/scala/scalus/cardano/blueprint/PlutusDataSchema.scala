package scalus.cardano.blueprint

import com.github.plokhotnyuk.jsoniter_scala.core.{writeToString, JsonValueCodec, WriterConfig}
import com.github.plokhotnyuk.jsoniter_scala.macros.{CodecMakerConfig, JsonCodecMaker}
import PlutusDataSchema.given_JsonValueCodec_PlutusDataSchema

import scala.annotation.tailrec
import scala.quoted.*

/** The description of the data shape of a validator parameter, datum or redemeer.
  *
  * For user types, the schema should generally be [[PlutusDataSchema.derived]] as opposed to
  * manually assembled.
  *
  * If assembled directly, it should be compliant with https://cips.cardano.org/cip/CIP-57.
  *
  * Namely, composite types should be represented as [[DataType.Constructor]]s, with properly
  * indexed fields, tuples of arity >3 should be [[DataType.List]]s with respective items, and pairs
  * should be [[DataType.PairBuiltin]]. See `BlueprintTest` for derivation examples and
  * expectations.
  *
  * @see
  *   [[DataType]] for type description
  *
  * @see
  *   [[scalus.builtin.Data]] for more info about onchain data types
  *
  * @note
  *   the types described by these schemas are used as datums, redeemers and parameters. As such,
  *   only onchain data should be described by `PlutusDataSchema`. Therefore, attempting to derive
  *   schemas for types that cannot be on chain will lead to compile time errors. Generally, if one
  *   can derive [[scalus.builtin.Data.FromData]] for a type, one can also do so for the schema. If
  *   one cannot derive [[scalus.builtin.Data.FromData]], it means that the type cannot exist on
  *   chain, and therefore should not be described with `PlutusDataSchema`
  */
case class PlutusDataSchema(
    dataType: Option[DataType] = None,
    title: Option[String] = None,
    description: Option[String] = None,
    anyOf: Option[List[PlutusDataSchema]] = None,
    index: Option[Int] = None,
    fields: Option[List[PlutusDataSchema]] = None,
    items: Option[List[PlutusDataSchema]] = None
) {

    def show = toJson()

    def toJson(indentation: Int = 2): String =
        writeToString(this, WriterConfig.withIndentionStep(indentation))
}

object PlutusDataSchema {

    /** Derives the schema for a specified type.
      * @tparam T
      *   the type to derive the schema for
      *
      * @note
      *   can only be used for onchain types
      */
    inline def derived[T]: PlutusDataSchema = ${ deriveSchemaImpl[T] }

    private def deriveSchemaImpl[T: Type](using Quotes): Expr[PlutusDataSchema] =
        import quotes.reflect.*

        val tpe = TypeRepr.of[T].dealias.widen
        val symbol = tpe.typeSymbol

        if isUnit(tpe) then '{ PlutusDataSchema(dataType = Some(DataType.UnitBuiltin)) }
        else if isPrimitive(tpe) then deriveForPrimitive(tpe)
        else if isTuple(tpe) then deriveForTuple(tpe)
        else if symbol.flags.is(Flags.Case) && !symbol.flags.is(Flags.Enum) then
            deriveForCaseClass(symbol)
        else if !symbol.flags.is(Flags.Case) && symbol.flags.is(Flags.Enum) then
            deriveForEnumRoot(symbol)
        else if symbol.flags.is(Flags.Case) && symbol.flags.is(Flags.Enum) then
            generateForEnumLeafWithIndex(symbol, 0)
        else report.errorAndAbort(s"Unsupported type for schema generation: ${tpe.show}")

    private def deriveForPrimitive(using
        Quotes
    )(tpe: quotes.reflect.TypeRepr): Expr[PlutusDataSchema] =
        import quotes.reflect.*
        tpe.show match {
            case "scala.Int" | "scala.Long" | "scala.math.BigInt" =>
                '{ PlutusDataSchema(dataType = Some(DataType.Integer)) }
            case "scalus.builtin.ByteString" | "scala.Array[scala.Byte]" |
                "scala.collection.immutable.List[scala.Byte]" =>
                '{ PlutusDataSchema(dataType = Some(DataType.Bytes)) }
            case "scala.Boolean" =>
                '{ PlutusDataSchema(dataType = Some(DataType.BooleanBuiltin)) }
            case "java.lang.String" =>
                '{ PlutusDataSchema(dataType = Some(DataType.StringBuiltin)) }
            case _ if tpe.typeSymbol.name == "List" =>
                '{ PlutusDataSchema(dataType = Some(DataType.List)) }
            case _ if tpe.typeSymbol.name == "Map" =>
                '{ PlutusDataSchema(dataType = Some(DataType.Map)) }
            case _ =>
                report.errorAndAbort(s"Unsupported primitive type: ${tpe.show}")
        }

    private def deriveForCaseClass(using
        Quotes
    )(symbol: quotes.reflect.Symbol): Expr[PlutusDataSchema] =

        val params = getPrimaryConstructorParams(symbol)
        val fieldSchemas = params.map { case (name, typeRepr) =>
            generateFieldSchema(name, typeRepr)
        }

        val fieldsExpr = Expr.ofList(fieldSchemas)

        '{
            PlutusDataSchema(
              dataType = Some(DataType.Constructor),
              title = Some(${ Expr(symbol.name) }),
              fields = Some($fieldsExpr)
            )
        }

    private def deriveForEnumRoot(using
        Quotes
    )(symbol: quotes.reflect.Symbol): Expr[PlutusDataSchema] =
        import quotes.reflect.*

        val cases = symbol.children.filter(_.flags.is(Flags.Case))
        val caseSchemas = cases.zipWithIndex.map { case (caseSymbol, index) =>
            generateForEnumLeafWithIndex(caseSymbol, index)
        }

        val anyOfExpr = Expr.ofList(caseSchemas)

        '{
            PlutusDataSchema(
              title = Some(${ Expr(symbol.name) }),
              anyOf = Some($anyOfExpr)
            )
        }

    private def generateForEnumLeafWithIndex(using
        Quotes
    )(symbol: quotes.reflect.Symbol, index: Int): Expr[PlutusDataSchema] =

        val params = getPrimaryConstructorParams(symbol)

        if params.isEmpty then {
            '{
                PlutusDataSchema(
                  dataType = Some(DataType.Constructor),
                  title = Some(${ Expr(symbol.name) }),
                  index = Some(${ Expr(index) }),
                  fields = Some(List.empty)
                )
            }
        } else {
            val fieldSchemas = params.map { case (name, typeRepr) =>
                generateFieldSchema(name, typeRepr)
            }
            val fieldsExpr = Expr.ofList(fieldSchemas)

            '{
                PlutusDataSchema(
                  dataType = Some(DataType.Constructor),
                  title = Some(${ Expr(symbol.name) }),
                  index = Some(${ Expr(index) }),
                  fields = Some($fieldsExpr)
                )
            }
        }

    private def deriveSchemaForField[T: Type](using Quotes): Expr[PlutusDataSchema] =
        import quotes.reflect.*
        val tpe = TypeRepr.of[T].dealias.widen
        val symbol = tpe.typeSymbol

        if isUnit(tpe) then {
            report.errorAndAbort("Unit type cannot be used as a field")
        } else if isPrimitive(tpe) then {
            deriveForPrimitive(tpe)
        } else if isTuple(tpe) then {
            deriveForTuple(tpe)
        } else if symbol.flags.is(Flags.Case) && !symbol.flags.is(Flags.Enum) then {
            deriveForCaseClass(symbol)
        } else if !symbol.flags.is(Flags.Case) && symbol.flags.is(Flags.Enum) then {
            deriveForEnumRoot(symbol)
        } else if symbol.flags.is(Flags.Case) && symbol.flags.is(Flags.Enum) then {
            generateForEnumLeafWithIndex(symbol, 0)
        } else {
            report.errorAndAbort(s"Unsupported type for schema generation: ${tpe.show}")
        }

    private def generateFieldSchema(using
        Quotes
    )(name: String, tpe: quotes.reflect.TypeRepr): Expr[PlutusDataSchema] =
        import quotes.reflect.*

        if isTuple(tpe) then {
            // Handle tuple fields specially - generate full schema including items
            tpe match {
                case AppliedType(_, args) if args.length == 2 =>
                    val firstItemSchema = deriveSchemaForField(using
                      args(0).asType.asInstanceOf[Type[Any]]
                    )
                    val secondItemSchema = deriveSchemaForField(using
                      args(1).asType.asInstanceOf[Type[Any]]
                    )

                    val itemsExpr = Expr.ofList(List(firstItemSchema, secondItemSchema))

                    '{
                        PlutusDataSchema(
                          dataType = Some(DataType.PairBuiltin),
                          title = Some(${ Expr(name) }),
                          items = Some($itemsExpr)
                        )
                    }
                case _ =>
                    report.errorAndAbort(s"Unsupported tuple type in field: ${tpe.show}")
            }
        } else {
            val dataType = resolveFieldDataType(tpe)

            '{
                PlutusDataSchema(
                  dataType = $dataType,
                  title = Some(${ Expr(name) })
                )
            }
        }

    @tailrec
    private def resolveFieldDataType(using
        Quotes
    )(tpe: quotes.reflect.TypeRepr): Expr[Option[DataType]] =
        import quotes.reflect.*

        if isPrimitive(tpe) then {
            tpe.show match {
                case "scala.Int" | "scala.Long" | "scala.math.BigInt" =>
                    '{ Some(DataType.Integer) }
                case "scala.Array[scala.Byte]" | "scalus.builtin.ByteString" =>
                    '{ Some(DataType.Bytes) }
                case "scala.Boolean" =>
                    '{ Some(DataType.BooleanBuiltin) }
                case "java.lang.String" =>
                    '{ Some(DataType.StringBuiltin) }
                case _ if tpe.typeSymbol.name == "List" =>
                    '{ Some(DataType.List) }
                case _ if tpe.typeSymbol.name == "Map" =>
                    '{ Some(DataType.Map) }
                case _ =>
                    '{ Some(DataType.Constructor) }
            }
        } else if isTuple(tpe) then {
            '{ Some(DataType.PairBuiltin) }
        } else {
            val symbol = tpe.typeSymbol
            if symbol.flags.is(Flags.Case) && !symbol.flags.is(Flags.Enum) then {
                val params = getPrimaryConstructorParams(symbol)
                if params.length == 1 then {
                    val (_, fieldType) = params.head
                    if isPrimitive(fieldType) then {
                        resolveFieldDataType(fieldType)
                    } else {
                        '{ Some(DataType.Constructor) }
                    }
                } else {
                    '{ Some(DataType.Constructor) }
                }
            } else {
                '{ Some(DataType.Constructor) }
            }
        }

    private def isTuple(using Quotes)(tpe: quotes.reflect.TypeRepr): Boolean =
        val symbolName = tpe.typeSymbol.name
        symbolName == "Tuple2"

    private def deriveForTuple(using
        Quotes
    )(tpe: quotes.reflect.TypeRepr): Expr[PlutusDataSchema] =
        import quotes.reflect.*

        tpe match {
            case AppliedType(_, args) if args.length == 2 =>
                val firstItemSchema = deriveSchemaForField(using
                  args(0).asType.asInstanceOf[Type[Any]]
                )
                val secondItemSchema = deriveSchemaForField(using
                  args(1).asType.asInstanceOf[Type[Any]]
                )

                val itemsExpr = Expr.ofList(List(firstItemSchema, secondItemSchema))

                '{
                    PlutusDataSchema(
                      dataType = Some(DataType.PairBuiltin),
                      title = Some("Tuple2"),
                      items = Some($itemsExpr)
                    )
                }
            case _ =>
                report.errorAndAbort(
                  s"Unsupported tuple type: ${tpe.show}. The only currently supported tuple is a pair."
                )
        }

    private def isUnit(using Quotes)(tpe: quotes.reflect.TypeRepr): Boolean =
        tpe.show == "scala.Unit" || tpe.show == "Unit"

    private def isPrimitive(using Quotes)(tpe: quotes.reflect.TypeRepr): Boolean =
        tpe.show match {
            case "scala.Int" | "scala.Long" | "scala.math.BigInt" | "scala.Boolean" |
                "java.lang.String" =>
                true
            case "scala.Array[scala.Byte]" | "scalus.builtin.ByteString"            => true
            case _ if tpe.typeSymbol.name == "List" || tpe.typeSymbol.name == "Map" => true
            case _                                                                  => false
        }

    private def getPrimaryConstructorParams(using
        Quotes
    )(symbol: quotes.reflect.Symbol): List[(String, quotes.reflect.TypeRepr)] =
        import quotes.reflect.*

        val primaryConstructor = symbol.primaryConstructor
        primaryConstructor.paramSymss.flatten.map { param =>
            val paramName = param.name
            val paramType = param.tree match
                case ValDef(_, tpt, _) =>
                    tpt.tpe.dealias.widen
                case _ =>
                    TypeRepr.of[Nothing]

            (paramName, paramType)
        }

    given JsonValueCodec[PlutusDataSchema] =
        JsonCodecMaker.make(CodecMakerConfig.withAllowRecursiveTypes(true))
}
