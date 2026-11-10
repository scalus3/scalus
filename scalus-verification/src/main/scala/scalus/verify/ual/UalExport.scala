package scalus.verify.ual

import scalus.utils.{Hex, Utils}
import scalus.verify.uplcblaster.UplcBlaster.{UalFunction, UalStatement}

import java.nio.charset.StandardCharsets
import java.nio.file.{Files, Path}

/** Statements as documents of the Universal Annotation Language (UAL 1.0,
  * https://github.com/input-output-hk/UniversalAnnotationLanguage): what a producer of the language
  * emits for a consumer to check.
  *
  * A statement is a property over compiled functions, the programs of its tests
  * ([[scalus.verify.uplcblaster.UplcBlaster.ualStatement]]). [[bundle]] makes of the properties:
  *
  *   - `assurance.json`, the detached assurance document: the properties, the Lean definitions they
  *     use as formal fragments, and the functions, each with its compiled code and the SHA-256 of
  *     it;
  *   - `checking-<property>.json`, a checking context for each property, with its steps and the
  *     functions it is about, where the environment of a consumer is given;
  *   - `annotations.ual`, the same as the annotations of the language, to be read.
  *
  * It emits claims and no evidence: that a statement was proved here, by another checker's form of
  * it, is no evidence of the language, which a consumer makes by checking the documents.
  *
  * What is not there yet:
  *
  *   - Every property is of functions (`kind: function`), none of a script of the blueprint: the
  *     programs proved are compiled with the tactic's options, and are not the published script.
  *   - The blueprint is referred to as it is given. The language asks for the compiled-interface
  *     dialect of CIP-57, which Scalus's blueprints are not in.
  *   - A checking context names the environment of the consumer that is to check it, a manifest
  *     that consumer writes. Without one, no context is written, and the steps of a property are in
  *     its annotation only.
  */
object UalExport {

    val languageVersion = "1.0"
    val languageUri = "https://github.com/input-output-hk/UniversalAnnotationLanguage"
    val assuranceSchema = "https://cips.cardano.org/cips/cipXXXX/schemas/assurance-v2.json"
    val contextSchema = "https://cips.cardano.org/cips/cipXXXX/schemas/checking-context.json"
    val stepCheckProfile = "https://cips.cardano.org/cips/cipXXXX/profiles/uplc-step-check/v1"

    /** The semantics of the builtins the checks here run with: the default of the Lean model. */
    val semanticsVariant = "E"

    /** What an assurance document says of itself. `created` is a date, as `2026-11-10`. */
    final case class Preamble(
        title: String,
        description: String,
        version: String,
        authors: List[String],
        created: String,
        license: Option[String]
    )

    /** A file a document refers to: where it is, from the document, and its bytes, which the
      * reference has the SHA-256 of, where they are known.
      */
    final case class Artifact(uri: String, bytes: Option[Array[Byte]])

    /** A property: its id, of letters, digits, `_` and `-`; what it says, in words; and the
      * statement.
      */
    final case class Property(id: String, text: String, statement: UalStatement)

    /** The documents of an export, by their file names. */
    final case class Bundle(files: List[(String, String)]) {

        /** Writes the documents into `directory`. */
        def writeTo(directory: Path): Unit = {
            Files.createDirectories(directory)
            files.foreach((name, text) =>
                Files.writeString(directory.resolve(name), text, StandardCharsets.UTF_8)
            )
        }
    }

    /** `name`, a statement's, as the id of a property: its letters, digits, `_` and `-`, with `_`
      * for every other character.
      */
    def propertyId(name: String): String =
        name.map(c => if c < 128 && (c.isLetterOrDigit || c == '_' || c == '-') then c else '_')

    /** The documents of `properties`: the assurance document, the annotations, and, where
      * `environment` is given, a checking context for each property.
      */
    def bundle(
        preamble: Preamble,
        blueprint: Artifact,
        environment: Option[Artifact],
        properties: List[Property]
    ): Bundle = {
        val ids = properties.map(_.id)
        require(ids.distinct == ids, s"two properties of one id: ${ids.diff(ids.distinct)}")
        val contexts = environment.toList.flatMap(manifest =>
            properties.map(property => property.id -> render(context(property, manifest)))
        )
        val assurance = render(document(preamble, blueprint, properties, contexts.toMap))
        Bundle(
          ("assurance.json" -> assurance) ::
              contexts.map((id, text) => contextFile(id) -> text) :::
              List("annotations.ual" -> annotations(properties))
        )
    }

    private def contextFile(id: String): String = s"checking-$id.json"

    private def sha256(bytes: Array[Byte]): String =
        Hex.bytesToHex(Utils.sha2_256(bytes)).toLowerCase

    private def digest(hash: String): Json =
        Json.Obj("alg" -> Json.Str("sha256"), "digest" -> Json.Str(hash))

    private def reference(artifact: Artifact): Json = Json.Obj(
      (("uri" -> Json.Str(artifact.uri)) ::
          artifact.bytes.toList.map(bytes => "hash" -> digest(sha256(bytes))))*
    )

    private def interface(function: UalFunction): List[(String, Json)] = List(
      "serialization" -> Json.Str("cbor-flat"),
      "plutusVersion" -> Json.Str("v3"),
      "arguments" -> Json.Arr(function.arguments.map(argument => Json.Raw(argument.schema))),
      "result" -> Json.Raw("""{"dataType":"#boolean"}""")
    )

    private def context(property: Property, environment: Artifact): Json = Json.Obj(
      "$schema" -> Json.Str(contextSchema),
      "profile" -> Json.Str(stepCheckProfile),
      "environment" -> reference(environment),
      "execution" -> Json.Obj(
        "semanticsVariant" -> Json.Str(semanticsVariant),
        "budget" -> Json.Obj(
          "kind" -> Json.Str("cek-steps"),
          "steps" -> Json.Num(property.statement.steps)
        ),
        "acceptance" -> Json.Str("evaluation")
      ),
      "targets" -> Json.Arr(
        property.statement.functions.toList.map(function =>
            Json.Obj(
              "function" -> Json.Str(function.id),
              "functionHash" -> digest(function.hash),
              "functionInterface" -> Json.Obj(interface(function)*)
            )
        )
      )
    )

    private def document(
        preamble: Preamble,
        blueprint: Artifact,
        properties: List[Property],
        contexts: Map[String, String]
    ): Json = {
        val fragments = properties
            .filter(_.statement.predicates.nonEmpty)
            .map(property =>
                Json.Obj(
                  "id" -> Json.Str(property.id),
                  "language" -> Json.Str("ual"),
                  "source" -> Json.Str(property.statement.predicates)
                )
            )
        val claims = properties.map { property =>
            val uses =
                if property.statement.predicates.isEmpty then Nil
                else List("uses" -> Json.Arr(List(Json.Str(property.id))))
            val formal = Json.Obj(
              (("language" -> Json.Str("ual")) :: uses :::
                  List("source" -> Json.Str(property.statement.formal)))*
            )
            val fields = List(
              "id" -> Json.Str(property.id),
              "scope" -> Json.Obj(
                "functions" -> Json.Arr(
                  property.statement.functions.toList.map(f => Json.Str(f.id))
                )
              ),
              "statement" -> Json.Obj("text" -> Json.Str(property.text), "formal" -> formal)
            ) ::: (if contexts.contains(property.id) then
                       List("checkingContext" -> Json.Str(property.id))
                   else Nil)
            Json.Obj(fields*)
        }
        val functions = properties
            .flatMap(_.statement.functions)
            .map(function =>
                function.id -> Json.Obj(
                  (List(
                    "compiledCode" -> Json
                        .Str(Hex.bytesToHex(function.program.cborEncoded).toLowerCase),
                    "hash" -> digest(function.hash)
                  ) ::: interface(function))*
                )
            )
        val checking = contexts.toList
            .sortBy(_._1)
            .map((id, text) =>
                id -> reference(
                  Artifact(contextFile(id), Some(text.getBytes(StandardCharsets.UTF_8)))
                )
            )
        Json.Obj(
          (List(
            "$schema" -> Json.Str(assuranceSchema),
            "preamble" -> Json.Obj(
              (List(
                "title" -> Json.Str(preamble.title),
                "description" -> Json.Str(preamble.description),
                "version" -> Json.Str(preamble.version),
                "authors" -> Json.Arr(preamble.authors.map(Json.Str(_))),
                "created" -> Json.Str(preamble.created)
              ) ::: preamble.license.toList.map(license => "license" -> Json.Str(license)))*
            ),
            "blueprint" -> reference(blueprint),
            "languages" -> Json.Obj(
              "ual" -> Json.Obj(
                "name" -> Json.Str("Universal Annotation Language"),
                "version" -> Json.Str(languageVersion),
                "uri" -> Json.Str(languageUri)
              )
            ),
            "formalFragments" -> Json.Arr(fragments),
            "properties" -> Json.Arr(claims)
          ) ::: (if checking.isEmpty then Nil
                 else List("checkingContexts" -> Json.Obj(checking*))) :::
              List("functions" -> Json.Obj(functions*)))*
        )
    }

    /** The properties as the annotations of the language, in the comment delimiters of Scala: an
      * `ONCHAIN` for each function, a `PREDICATE` of the definitions a property uses, and the
      * `PROPERTY`.
      */
    def annotations(properties: List[Property]): String =
        properties
            .map { property =>
                val statement = property.statement
                val onchain = statement.functions.toList.map { function =>
                    val signature =
                        (function.arguments.map(_.annotation) :+ "{ Bool : asNative }")
                            .mkString(" -> ")
                    s"/*@ ONCHAIN [kind: function] [version: PlutusV3] " +
                        s"[steps: ${statement.steps}, semantics: $semanticsVariant]\n" +
                        s"    ${function.id} :: $signature\n@*/"
                }
                val predicate =
                    if statement.predicates.isEmpty then Nil
                    else List(s"/*@ PREDICATE\n${statement.predicates}\n@*/")
                val scope = statement.functions.map(_.id).mkString(" ")
                val claim = s"/*@ PROPERTY [scope: $scope] ${property.id}\n" +
                    s"    ${quoted(property.text)}\n  : ${statement.formal}\n@*/"
                (onchain ::: predicate ::: List(claim)).mkString("\n\n")
            }
            .mkString("", "\n\n", "\n")

    private def quoted(text: String): String =
        "\"" + text.replace("\\", "\\\\").replace("\"", "\\\"") + "\""

    /** JSON, as much of it as the documents have. */
    private enum Json {
        case Obj(fields: (String, Json)*)
        case Arr(items: List[Json])
        case Str(value: String)
        case Num(value: Long)

        /** JSON that is written already. */
        case Raw(text: String)
    }

    private def string(value: String): String = {
        val text = new StringBuilder("\"")
        value.foreach {
            case '"'           => text ++= "\\\""
            case '\\'          => text ++= "\\\\"
            case '\n'          => text ++= "\\n"
            case '\r'          => text ++= "\\r"
            case '\t'          => text ++= "\\t"
            case c if c < 0x20 => text ++= f"\\u${c.toInt}%04x"
            case c             => text += c
        }
        text += '"'
        text.toString
    }

    private def render(json: Json): String = render(json, 0) + "\n"

    private def render(json: Json, depth: Int): String = {
        val indent = "  " * (depth + 1)
        val close = "  " * depth
        json match
            case Json.Str(value) => string(value)
            case Json.Num(value) => value.toString
            case Json.Raw(text)  => text
            case Json.Arr(Nil)   => "[]"
            case Json.Arr(items) =>
                items
                    .map(item => indent + render(item, depth + 1))
                    .mkString("[\n", ",\n", s"\n$close]")
            case Json.Obj() => "{}"
            case Json.Obj(fields*) =>
                fields
                    .map((name, value) => s"$indent${string(name)}: ${render(value, depth + 1)}")
                    .mkString("{\n", ",\n", s"\n$close}")
    }
}
