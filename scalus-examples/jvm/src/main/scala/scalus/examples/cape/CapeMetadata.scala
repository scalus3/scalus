package scalus.examples.cape

/** Generates CAPE-compliant metadata.json content.
  *
  * See
  * https://github.com/IntersectMBO/UPLC-CAPE/blob/main/submissions/TEMPLATE/metadata.schema.json
  */
object CapeMetadata {
    def apply(
        version: String,
        date: String,
        sourceUrl: String,
        notes: String
    ): String =
        s"""|{
            |  "compiler": {
            |    "name": "Scalus",
            |    "version": "$version"
            |  },
            |  "compilation_config": {
            |    "optimization_level": "release",
            |    "target": "uplc",
            |    "flags": ["Options.release"]
            |  },
            |  "submission": {
            |    "date": "${date}",
            |    "source_available": true,
            |    "source_repository": "$sourceUrl",
            |    "implementation_notes": "$notes"
            |  }
            |}""".stripMargin
}
