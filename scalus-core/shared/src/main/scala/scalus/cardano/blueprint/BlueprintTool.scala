package scalus.cardano.blueprint

import scala.jdk.CollectionConverters.*

/** JSON-level blueprint operations used by the Scalus sbt plugin.
  *
  * The plugin (Scala 2.12/3, running inside sbt) calls these reflectively across a `URLClassLoader`
  * boundary, so the signatures use only `java.*` and `String` types. Do not rename or change
  * signatures without updating `ScalusSbtPlugin`.
  */
object BlueprintTool {

    /** Returns `json` with the top-level `scalus.scalaVersion` extension key stamped in,
      * overwriting any previous stamp. See [[ScalusInfo]] for why this lives at the root.
      */
    def stampScalaVersion(json: String, scalaVersion: String): String =
        Blueprint
            .fromJson(json)
            .copy(scalus = Some(ScalusInfo(Some(scalaVersion))))
            .toJson()

    /** Merges per-contract blueprint documents into one CIP-57 document (Aiken-style
      * `plutus.json`): all validators concatenated in input order under a project-level preamble.
      *
      * Compiler info is carried over from the first input that has it (all inputs of one build
      * share the same Scalus version). `plutusVersion` is set only when all inputs agree.
      *
      * @param jsons
      *   per-contract blueprint JSON documents, in the desired validator order
      * @param title
      *   project-level title (typically the sbt project name)
      * @param version
      *   project-level version (typically the sbt project version)
      * @param scalaVersion
      *   the Scala toolchain that compiled the contracts, recorded as `scalus.scalaVersion`
      */
    def aggregate(
        jsons: java.util.List[String],
        title: String,
        version: String,
        scalaVersion: String
    ): String = {
        val bps = jsons.asScala.toSeq.map(Blueprint.fromJson)
        val plutusVersions = bps.flatMap(_.preamble.plutusVersion).distinct
        val preamble = Preamble(
          title = title,
          version = Some(version),
          compiler = bps.flatMap(_.preamble.compiler).headOption,
          plutusVersion = if plutusVersions.length == 1 then plutusVersions.headOption else None
        )
        Blueprint(
          preamble,
          bps.flatMap(_.validators),
          Some(ScalusInfo(Some(scalaVersion)))
        ).toJson()
    }
}
