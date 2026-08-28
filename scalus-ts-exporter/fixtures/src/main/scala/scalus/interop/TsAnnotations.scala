package scalus.interop

import scala.annotation.StaticAnnotation

/** Overrides the TypeScript type emitted by scalus-ts-exporter for the annotated member, parameter,
  * or field - e.g. `@TsType("\"key\" | \"script\"")`. The string is emitted verbatim.
  *
  * Same-FQN local copy so the fixtures project does not depend on scalus-core.
  */
final class TsType(val tsType: String) extends StaticAnnotation

/** Overrides the TypeScript declaration name emitted by scalus-ts-exporter for the annotated class
  * or trait - e.g. `@TsName("SubmitResult")` on `trait JSubmitResult`.
  *
  * Same-FQN local copy so the fixtures project does not depend on scalus-core.
  */
final class TsName(val name: String) extends StaticAnnotation

/** Excludes the annotated member from the TypeScript definitions emitted by scalus-ts-exporter.
  * Same-FQN local copy so the fixtures project does not depend on scalus-core.
  */
final class TsIgnore extends StaticAnnotation
