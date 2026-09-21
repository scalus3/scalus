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

/** Excludes the annotated member from the TypeScript definitions emitted by scalus-ts-exporter.
  *
  * This affects the `.d.ts` and nothing else: the member is still an export root, so everything it
  * reaches stays in `scalus.js`. For a Scala-facing-only member, prefer an extension method on the
  * companion object, which is not a root. See `docs/internal/JS_BUNDLE_SIZE.md`.
  */
final class TsIgnore extends StaticAnnotation
