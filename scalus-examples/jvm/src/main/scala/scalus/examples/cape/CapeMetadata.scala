package scalus.examples.cape

/** Generates CAPE-compliant metadata.json content.
  *
  * See
  * https://github.com/IntersectMBO/UPLC-CAPE/blob/main/submissions/TEMPLATE/metadata.schema.json
  */
object CapeMetadata {
    val SourceRepository = "https://github.com/scalus3/scalus"

    /** The `Options` every CAPE contract compiles with: release, without the Scalus tag. */
    val CompilationFlags = "Options.releaseUntagged"

    /** The commit of the compiler that compiled the submission. The `v<version>` tag names it only
      * when the build runs at that tag; a later checkout compiles with its own plugin and
      * optimizer, so the checkout's own commit is recorded.
      */
    def compilerCommit(head: String, versionTag: Option[String]): String =
        versionTag.filter(_ == head).getOrElse(head)

    def render(
        version: String,
        compilerCommit: String,
        date: String,
        sourceCommit: String,
        notes: String
    ): String = {
        val compilationConfig = ujson.Obj(
          "optimization_level" -> "release",
          "target" -> "uplc",
          "flags" -> ujson.Arr(CompilationFlags),
          // The release options target van Rossem: every scenario uses builtin casing or batch-6
          // builtins, which need protocol version 11.
          "min_protocol_version" -> 11
        )
        ujson.write(
          ujson.Obj(
            "compiler" -> ujson.Obj(
              "name" -> "Scalus",
              "version" -> version,
              "commit_hash" -> compilerCommit
            ),
            "compilation_config" -> compilationConfig,
            "contributors" -> ujson.Arr(
              ujson.Obj(
                "name" -> "Alexander Nemish",
                "organization" -> "Lantr",
                "contact" -> "@nau"
              )
            ),
            "submission" -> ujson.Obj(
              "date" -> date,
              "source_available" -> true,
              "source_repository" -> SourceRepository,
              "source_commit_hash" -> sourceCommit,
              "implementation_notes" -> notes
            )
          ),
          indent = 2
        ) + "\n"
    }
}
