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
  *
  * Z3 abbreviates a large value: `(let ((a!1 term)) body)` names a subterm that `body`, or a
  * further `let` in it, uses. A long list is printed so, with each nested `let` starting on a new
  * line.
  *
  * Where the model leaves a value, or part of one, unconstrained, Z3 answers with the SMT name of a
  * variable, such as `$0`. Any value serves there, so it reads as a default: `0`, `false`, `I 0`,
  * the empty list or byte string.
  */
private[uplcblaster] object SmtValues {

    private enum Node {
        case Atom(text: String)
        case Text(value: String)
        case Apply(items: List[Node])
    }

    /** Why a term of a counterexample is not read as a value. */
    sealed trait Unreadable {
        def reason: String
    }

    object Unreadable {

        /** The term is not what Z3 prints for a value of the type: the output is not understood. */
        final case class Malformed(reason: String) extends Unreadable

        /** The term is a value of Lean's model that is no value of the type. The model stores a
          * byte string as a `String`, so it has byte strings with a character above 255.
          */
        final case class OutsideType(reason: String) extends Unreadable
    }

    private type Read[A] = Either[Unreadable, A]

    private def malformed[A](reason: String): Read[A] = Left(Unreadable.Malformed(reason))

    /** The value of the term `text`, as `value` reads it. */
    private def read[A](text: String, value: Node => Read[A]): Read[A] =
        term(text).left.map(Unreadable.Malformed(_)).flatMap(value)

    def integer(text: String): Either[Unreadable, BigInt] = read(text, toInteger)

    def boolean(text: String): Either[Unreadable, Boolean] = text.trim match
        case "true"                        => Right(true)
        case "false"                       => Right(false)
        case other if unconstrained(other) => Right(false)
        case other                         => malformed(s"expected a Boolean, got $other")

    /** The SMT name of a variable, which Z3 answers for a value it leaves unconstrained. */
    private def unconstrained(text: String): Boolean = text.startsWith("$")

    def data(text: String): Either[Unreadable, Data] = read(text, toData)

    def bytes(text: String): Either[Unreadable, ByteString] = read(text, toBytes)

    /** Whether `text` is a whole term so far: every parenthesis outside a string literal or a
      * quoted symbol is closed. A value printed over several lines is whole at its last line.
      */
    def complete(text: String): Boolean = {
        var depth = 0
        var quote: Option[Char] = None
        text.foreach { c =>
            quote match
                case Some(closing) => if c == closing then quote = None
                case None =>
                    if c == '"' || c == '|' then quote = Some(c)
                    else if c == '(' then depth += 1
                    else if c == ')' then depth -= 1
        }
        depth <= 0 && quote.isEmpty
    }

    private def term(text: String): Either[String, Node] =
        tokens(text).flatMap { tokens =>
            parse(tokens) match
                case Right((node, Nil)) => expand(node, Map.empty).left.map(e => s"$e in $text")
                case Right((_, rest))   => Left(s"unexpected ${rest.head} after a term in $text")
                case Left(error)        => Left(s"$error in $text")
        }

    /** `node` without its `let`s: each name stands for the term it is bound to. The bindings of one
      * `let` are read in the scope around it, as SMT-LIB defines.
      */
    private def expand(node: Node, names: Map[String, Node]): Either[String, Node] = node match
        case Node.Atom(text) => Right(names.getOrElse(text, node))
        case _: Node.Text    => Right(node)
        case Node.Apply(List(Node.Atom("let"), Node.Apply(bindings), body)) =>
            val bound = bindings.foldLeft[Either[String, Map[String, Node]]](Right(names)) {
                case (Right(scope), Node.Apply(List(Node.Atom(name), value))) =>
                    expand(value, names).map(expanded => scope.updated(name, expanded))
                case (Right(_), other) => Left(s"expected a let binding, got $other")
                case (failed, _)       => failed
            }
            bound.flatMap(expand(body, _))
        case Node.Apply(items) =>
            items
                .foldRight[Either[String, List[Node]]](Right(Nil)) { (item, rest) =>
                    for expanded <- expand(item, names); others <- rest yield expanded :: others
                }
                .map(Node.Apply(_))

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

    private def toInteger(node: Node): Read[BigInt] = node match
        case Node.Atom(text) if unconstrained(text)                     => Right(BigInt(0))
        case Node.Atom(text) if text.nonEmpty && text.forall(_.isDigit) => Right(BigInt(text))
        case Node.Apply(List(Node.Atom("-"), value))                    => toInteger(value).map(-_)
        case other => malformed(s"expected an integer, got $other")

    /** The constructor a qualified name of a `Data` constructor names: `I` for `….Data.I`. */
    private def dataConstructor(name: String): Option[String] =
        name.split('.').takeRight(2) match
            case Array("Data", constructor) => Some(constructor)
            case _                          => None

    private def toData(node: Node): Read[Data] = node match
        case Node.Atom(text) if unconstrained(text) => Right(Data.I(0))
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
                        // Lean's `Data` takes any integer for the tag of a constructor.
                        _ <- Either.cond(
                          index >= 0,
                          (),
                          Unreadable.OutsideType(s"a Data constructor has the tag $index, below 0")
                        )
                        values <- toList(fields, toData)
                    yield Data.Constr(index, PList.from(values))
                case _ => malformed(s"expected a Data value, got $node")
        case other => malformed(s"expected a Data value, got $other")

    private def toPair(node: Node): Read[(Data, Data)] = node match
        case Node.Atom(text) if unconstrained(text) => Right(Data.I(0) -> Data.I(0))
        case Node.Apply(List(Node.Atom("Prod.mk"), key, value)) =>
            for k <- toData(key); v <- toData(value) yield k -> v
        case other => malformed(s"expected a pair, got $other")

    private def toList[A](node: Node, element: Node => Read[A]): Read[List[A]] =
        node match
            case Node.Atom(text) if unconstrained(text) => Right(Nil)
            case Node.Apply(List(Node.Atom("List.cons"), head, tail)) =>
                for h <- element(head); t <- toList(tail, element) yield h :: t
            case Node.Apply(Node.Atom("as") :: Node.Atom("List.nil") :: _) |
                Node.Atom("List.nil") =>
                Right(Nil)
            case other => malformed(s"expected a list, got $other")

    /** A byte string, one character per byte. A character above 255 is no byte. */
    private def toBytes(node: Node): Read[ByteString] = node match
        case Node.Atom(text) if unconstrained(text) => Right(ByteString.empty)
        case Node.Apply(List(Node.Atom(name), Node.Text(value)))
            if name.endsWith("ByteString.mk") =>
            val codePoints = value.codePoints().toArray
            codePoints.find(_ > 255) match
                case Some(codePoint) =>
                    Left(
                      Unreadable.OutsideType(
                        s"the byte string $value has the character U+${codePoint.toHexString}, " +
                            "no byte"
                      )
                    )
                case None => Right(ByteString.fromArray(codePoints.map(_.toByte)))
        case other => malformed(s"expected a byte string, got $other")
}
