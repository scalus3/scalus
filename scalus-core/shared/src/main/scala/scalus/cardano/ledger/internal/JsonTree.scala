package scalus.cardano.ledger.internal

import com.github.plokhotnyuk.jsoniter_scala.core.{readFromStream, readFromString, writeToString, JsonReader, JsonValueCodec, JsonWriter, ReaderConfig}

import upickle.core.{ArrVisitor, ObjVisitor, StringVisitor, Visitor}

import java.io.InputStream
import java.math.MathContext
import scala.collection.immutable.VectorMap

/** A JSON value whose numbers stay exact: each holds the `BigDecimal` of its text.
  *
  * `ujson.Value` holds a number as a `Double`, which rounds a decimal such as
  * `0.123456789012345678` and every integer above 2^53. The protocol-parameter codecs need the
  * exact value, and jsoniter-scala ships no tree of its own, so this is the small one they read and
  * write through. Objects keep their keys in document order.
  */
private[ledger] enum JsonTree {
    case Obj(fields: VectorMap[String, JsonTree])
    case Arr(items: Vector[JsonTree])
    case Str(value: String)
    case Num(value: BigDecimal)
    case Bool(value: Boolean)
    case Null

    /** The value of `key`. Fails, naming the key, when this object has none, and with the kind of
      * value this is when it is not an object.
      */
    def apply(key: String): JsonTree = {
        obj.getOrElse(key, throw new NoSuchElementException(s"key not found: $key"))
    }

    /** The value of `key`, if this is an object that has it. */
    def get(key: String): Option[JsonTree] = this match
        case Obj(fields) => fields.get(key)
        case _           => None

    def obj: VectorMap[String, JsonTree] = this match
        case Obj(fields) => fields
        case _           => throw new IllegalArgumentException(s"expected a JSON object, got $kind")

    def arrOpt: Option[Vector[JsonTree]] = this match
        case Arr(items) => Some(items)
        case _          => None

    def num: BigDecimal = this match
        case Num(value) => value
        case _          => throw new IllegalArgumentException(s"expected a JSON number, got $kind")

    /** What sort of JSON value this is, for error messages. */
    def kind: String = this match
        case Obj(_)  => "an object"
        case Arr(_)  => "an array"
        case Str(_)  => "a string"
        case Num(_)  => "a number"
        case Bool(_) => "a boolean"
        case Null    => "null"
}

private[ledger] object JsonTree {

    def obj(fields: (String, JsonTree)*): JsonTree = Obj(VectorMap.from(fields))

    def num(value: Long): JsonTree = Num(BigDecimal(value))

    def parse(json: String): JsonTree = readFromString(json, readerConfig)(using codec)

    def parse(json: InputStream): JsonTree = readFromStream(json, readerConfig)(using codec)

    def render(tree: JsonTree): String = writeToString(tree)(using codec)

    /** Builds the tree [[parse]] builds for the same valid JSON, through ujson's parser, which
      * hands over each number's text. For macros only, which read the repository's own resources:
      * it has none of [[parse]]'s bounds on nesting or on a number's digits and scale, and
      * `JsonTreeTest` checks that the two agree.
      *
      * A macro runs this module's code inside the compiler, on the JVM, against the dependencies of
      * the platform being compiled. jsoniter's Scala Native artifact reads its buffer through
      * Native pointer intrinsics (`ByteArrayAccess`), which throw `UndefinedBehaviorError` on a
      * JVM, so [[parse]] cannot run while `scalus-core` compiles for Native. ujson is plain Scala
      * on every platform. D129
      */
    def parseInMacro(json: String): JsonTree = {
        ujson.CharSequenceParser.transform(json, TextKeepingBuilder)
    }

    /** Builds a [[JsonTree]] from ujson's parse events, a number from its text. */
    private object TextKeepingBuilder extends ujson.JsVisitor[JsonTree, JsonTree] {
        override def visitArray(length: Int, index: Int): ArrVisitor[JsonTree, JsonTree] =
            new ArrVisitor[JsonTree, JsonTree] {
                private val items = Vector.newBuilder[JsonTree]
                override def subVisitor: Visitor[?, ?] = TextKeepingBuilder
                override def visitValue(v: JsonTree, index: Int): Unit = items += v
                override def visitEnd(index: Int): JsonTree = Arr(items.result())
            }

        override def visitJsonableObject(length: Int, index: Int): ObjVisitor[JsonTree, JsonTree] =
            new ObjVisitor[JsonTree, JsonTree] {
                private val fields = VectorMap.newBuilder[String, JsonTree]
                private var key = ""
                override def subVisitor: Visitor[?, ?] = TextKeepingBuilder
                override def visitKey(index: Int): Visitor[?, ?] = StringVisitor
                override def visitKeyValue(v: Any): Unit = key = v.toString
                override def visitValue(v: JsonTree, index: Int): Unit = fields += key -> v
                override def visitEnd(index: Int): JsonTree = Obj(fields.result())
            }

        override def visitNull(index: Int): JsonTree = Null
        override def visitFalse(index: Int): JsonTree = Bool(false)
        override def visitTrue(index: Int): JsonTree = Bool(true)
        override def visitString(s: CharSequence, index: Int): JsonTree = Str(s.toString)

        override def visitFloat64StringParts(
            s: CharSequence,
            decIndex: Int,
            expIndex: Int,
            index: Int
        ): JsonTree = Num(BigDecimal(new java.math.BigDecimal(s.toString)))
    }

    /** Without the hex dump, a parse error is a message and an offset, which is what a caller of
      * `fromBlockfrostJson` can act on.
      */
    private val readerConfig = ReaderConfig.withAppendHexDumpToParseException(false)

    /** jsoniter's default bounds on a number's scale and digits, which no protocol parameter comes
      * near. The `MathContext` is not the default: `DECIMAL128` would round past 34 digits.
      */
    private val ScaleLimit = 6178
    private val DigitsLimit = 308

    /** Nesting deeper than this fails instead of exhausting the stack. Protocol parameters nest
      * three levels.
      */
    private val MaxDepth = 64

    private val codec: JsonValueCodec[JsonTree] = new JsonValueCodec[JsonTree] {
        override def nullValue: JsonTree = null

        override def decodeValue(in: JsonReader, default: JsonTree): JsonTree = read(in, 0)

        override def encodeValue(tree: JsonTree, out: JsonWriter): Unit = write(tree, out)
    }

    private def read(in: JsonReader, depth: Int): JsonTree = {
        if depth > MaxDepth then in.decodeError(s"JSON nested deeper than $MaxDepth levels")
        val token = in.nextToken()
        if token == '"' then
            in.rollbackToken()
            Str(in.readString(null))
        else if token == '{' then readObject(in, depth)
        else if token == '[' then readArray(in, depth)
        else if token == 't' || token == 'f' then
            in.rollbackToken()
            Bool(in.readBoolean())
        else if token == 'n' then in.readNullOrError(Null, "expected a JSON value")
        else
            in.rollbackToken()
            Num(in.readBigDecimal(null, MathContext.UNLIMITED, ScaleLimit, DigitsLimit))
    }

    private def readObject(in: JsonReader, depth: Int): JsonTree = {
        val fields = VectorMap.newBuilder[String, JsonTree]
        if !in.isNextToken('}') then
            in.rollbackToken()
            var more = true
            while more do
                val key = in.readKeyAsString()
                fields += key -> read(in, depth + 1)
                more = in.isNextToken(',')
            if !in.isCurrentToken('}') then in.objectEndOrCommaError()
        Obj(fields.result())
    }

    private def readArray(in: JsonReader, depth: Int): JsonTree = {
        val items = Vector.newBuilder[JsonTree]
        if !in.isNextToken(']') then
            in.rollbackToken()
            var more = true
            while more do
                items += read(in, depth + 1)
                more = in.isNextToken(',')
            if !in.isCurrentToken(']') then in.arrayEndOrCommaError()
        Arr(items.result())
    }

    private def write(tree: JsonTree, out: JsonWriter): Unit = tree match
        case Obj(fields) =>
            out.writeObjectStart()
            for (key, value) <- fields do
                out.writeKey(key)
                write(value, out)
            out.writeObjectEnd()
        case Arr(items) =>
            out.writeArrayStart()
            items.foreach(write(_, out))
            out.writeArrayEnd()
        case Str(value)  => out.writeVal(value)
        case Num(value)  => out.writeVal(value)
        case Bool(value) => out.writeVal(value)
        case Null        => out.writeNull()
}
