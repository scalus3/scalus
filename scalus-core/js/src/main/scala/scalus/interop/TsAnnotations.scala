package scalus.interop

import scala.annotation.StaticAnnotation

/** Overrides the TypeScript type emitted by scalus-ts-exporter for the annotated member, parameter,
  * or field - e.g. `@TsType("\"key\" | \"script\"")`. The string is emitted verbatim.
  */
final class TsType(val tsType: String) extends StaticAnnotation

/** Overrides the TypeScript declaration name emitted by scalus-ts-exporter for the annotated class
  * or trait - e.g. `@TsName("SubmitResult")` on `trait JSubmitResult`.
  */
final class TsName(val name: String) extends StaticAnnotation

/** Excludes the annotated member from the TypeScript definitions emitted by scalus-ts-exporter. The
  * member still exists at JS runtime; it is just not part of the typed surface - use for
  * Scala-facing members of js.Object classes (e.g. methods taking java.time.Instant).
  */
final class TsIgnore extends StaticAnnotation
