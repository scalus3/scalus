package scalus.examples.cape

/** Generates CAPE-compliant metadata.json content.
  *
  * See
  * https://github.com/IntersectMBO/UPLC-CAPE/blob/main/submissions/TEMPLATE/metadata.schema.json
  */
object CapeMetadata {
    val SourceRepository = "https://github.com/scalus3/scalus"

    def render(
        version: String,
        compilerCommit: String,
        date: String,
        sourceCommit: String,
        notes: String,
        minPlutusVersion: Option[String] = None
    ): String = {
        val compilationConfig = ujson.Obj(
          "optimization_level" -> "release",
          "target" -> "uplc",
          "flags" -> ujson.Arr("Options.release")
        )
        minPlutusVersion.foreach(v => compilationConfig("min_plutus_version") = v)
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
