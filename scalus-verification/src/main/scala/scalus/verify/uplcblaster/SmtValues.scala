package scalus.verify.uplcblaster

import scalus.cardano.onchain.plutus.prelude.List as PList
import scalus.uplc.builtin.{ByteString, Data}

/** Reads the values of Blaster's counterexamples.
  *
  * Blaster asks Z3 for each variable's value with `(eval x)` and prints Z3's answer as it is, an
  * SMT-LIB term over the sorts Blaster declared, which are named after the Lean definitions. An
  * application is parenthesized, a negative integer is `(- 1)`, a list is built with `List.cons`
  * and ends in `(as List.nil (@List …))`, a pair is `Prod.mk`, and a byte string is
  * `(PlutusCore.ByteString.….ByteString.mk "ABC")`, one character per byte. Constructors of `Data`
  * are qualified, as in `PlutusCore.Data.PlutusCore.DataInternal.Data.I`. A string literal follows
  * SMT-LIB: `""` is a quote, and `\u{…}` is a character by its code point.
  */
private[uplcblaster] object SmtValues {

    private enum Node {
        case Atom(text: String)
        case Text(value: String)
        case Apply(items: List[Node])
    }

    def integer(text: String): Either[String, BigInt] = term(text).flatMap(toInteger)

    def boolean(text: String): Either[String, Boolean] = text.trim match
        case "true"  => Right(true)
        case "false" => Right(false)
        case other   => Left(s"expected a Boolean, got $other")

    def data(text: String): Either[String, Data] = term(text).flatMap(toData)

    private def term(text: String): Either[String, Node] =
        tokens(text).flatMap { tokens =>
            parse(tokens) match
                case Right((node, Nil)) => Right(node)
                case Right((_, rest))   => Left(s"unexpected ${rest.head} after a term in $text")
                case Left(error)        => Left(s"$error in $text")
        }

    private enum Token {
        case Open, Close
        case Word(text: String)
        case Quoted(value: String)
    }

    private def tokens(text: String): Either[String, List[Token]] = {
        val result = List.newBuilder[Token]
        var i = 0
        var error: Option[String] = None
        while i < text.length && error.isEmpty do
            val c = text.charAt(i)
            if c.isWhitespace then i += 1
            else if c == '(' then
                result += Token.Open
                i += 1
            else if c == ')' then
                result += Token.Close
                i += 1
            else if c == '|' then
                val close = text.indexOf('|', i + 1)
                if close < 0 then error = Some("an unterminated quoted symbol")
                else
                    result += Token.Word(text.substring(i + 1, close))
                    i = close + 1
            else if c == '"' then
                quoted(text, i + 1) match
                    case Right((value, next)) =>
                        result += Token.Quoted(value)
                        i = next
                    case Left(message) => error = Some(message)
            else
                val start = i
                while i < text.length && !text.charAt(i).isWhitespace && text.charAt(i) != '(' &&
                    text.charAt(i) != ')'
                do i += 1
                result += Token.Word(text.substring(start, i))
        error.toLeft(result.result())
    }

    /** An SMT-LIB string literal from after its opening quote: its value, and the index after it.
      * `""` stands for a quote, and `\u{d…}` or `\udddd` for the character with that hexadecimal
      * code point; any other character, a backslash included, stands for itself.
      */
    private def quoted(text: String, from: Int): Either[String, (String, Int)] = {
        val escape = """\\u\{([0-9a-fA-F]{1,5})\}|\\u([0-9a-fA-F]{4})""".r
        val value = new StringBuilder
        var i = from
        var end = -1
        while end < 0 && i < text.length do
            val c = text.charAt(i)
            if c == '"' then
                if text.startsWith("\"\"", i) then
                    value += '"'
                    i += 2
                else end = i + 1
            else
                escape.findPrefixMatchOf(text.substring(i)) match
                    case Some(found) =>
                        val digits = Option(found.group(1)).getOrElse(found.group(2))
                        value.appendAll(Character.toChars(Integer.parseInt(digits, 16)))
                        i += found.end
                    case None =>
                        value += c
                        i += 1
        if end < 0 then Left("an unterminated string literal") else Right(value.toString -> end)
    }

    private def parse(tokens: List[Token]): Either[String, (Node, List[Token])] = tokens match
        case Token.Open :: rest =>
            @annotation.tailrec
            def items(
                remaining: List[Token],
                found: List[Node]
            ): Either[String, (Node, List[Token])] = remaining match
                case Token.Close :: after => Right(Node.Apply(found.reverse) -> after)
                case Nil                  => Left("an unclosed parenthesis")
                case _ =>
                    parse(remaining) match
                        case Right((node, after)) => items(after, node :: found)
                        case Left(error)          => Left(error)
            items(rest, Nil)
        case Token.Word(text) :: rest    => Right(Node.Atom(text) -> rest)
        case Token.Quoted(value) :: rest => Right(Node.Text(value) -> rest)
        case Token.Close :: _            => Left("an unexpected closing parenthesis")
        case Nil                         => Left("an empty term")

    private def toInteger(node: Node): Either[String, BigInt] = node match
        case Node.Atom(text) if text.nonEmpty && text.forall(_.isDigit) => Right(BigInt(text))
        case Node.Apply(List(Node.Atom("-"), value))                    => toInteger(value).map(-_)
        case other => Left(s"expected an integer, got $other")

    /** The constructor a qualified name of a `Data` constructor names: `I` for `….Data.I`. */
    private def dataConstructor(name: String): Option[String] =
        name.split('.').takeRight(2) match
            case Array("Data", constructor) => Some(constructor)
            case _                          => None

    private def toData(node: Node): Either[String, Data] = node match
        case Node.Apply(Node.Atom(name) :: arguments) =>
            (dataConstructor(name), arguments) match
                case (Some("I"), List(value)) => toInteger(value).map(Data.I(_))
                case (Some("B"), List(bytes)) => toBytes(bytes).map(Data.B(_))
                case (Some("List"), List(items)) =>
                    toList(items, toData).map(values => Data.List(PList.from(values)))
                case (Some("Map"), List(entries)) =>
                    toList(entries, toPair).map(values => Data.Map(PList.from(values)))
                case (Some("Constr"), List(tag, fields)) =>
                    for
                        index <- toInteger(tag)
                        values <- toList(fields, toData)
                    yield Data.Constr(index, PList.from(values))
                case _ => Left(s"expected a Data value, got $node")
        case other => Left(s"expected a Data value, got $other")

    private def toPair(node: Node): Either[String, (Data, Data)] = node match
        case Node.Apply(List(Node.Atom("Prod.mk"), key, value)) =>
            for k <- toData(key); v <- toData(value) yield k -> v
        case other => Left(s"expected a pair, got $other")

    private def toList[A](node: Node, element: Node => Either[String, A]): Either[String, List[A]] =
        node match
            case Node.Apply(List(Node.Atom("List.cons"), head, tail)) =>
                for h <- element(head); t <- toList(tail, element) yield h :: t
            case Node.Apply(Node.Atom("as") :: Node.Atom("List.nil") :: _) |
                Node.Atom("List.nil") =>
                Right(Nil)
            case other => Left(s"expected a list, got $other")

    /** A byte string, one character per byte. A character above 255 is no byte. */
    private def toBytes(node: Node): Either[String, ByteString] = node match
        case Node.Apply(List(Node.Atom(name), Node.Text(value)))
            if name.endsWith("ByteString.mk") =>
            val codePoints = value.codePoints().toArray
            codePoints.find(_ > 255) match
                case Some(codePoint) =>
                    Left(
                      s"the byte string $value has the character U+${codePoint.toHexString}, no byte"
                    )
                case None => Right(ByteString.fromArray(codePoints.map(_.toByte)))
        case other => Left(s"expected a byte string, got $other")
}
