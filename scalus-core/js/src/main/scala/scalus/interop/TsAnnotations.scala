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
  * This affects the emitted `.d.ts` and nothing else. The member is still exported to JavaScript,
  * and it is still a linker export root: everything it reaches stays in `scalus.js`. Hiding a
  * member from the typed surface therefore does not make it free. A `SlotConfig` member returning a
  * `java.time.Instant` was annotated this way and kept the ~800 KB IANA timezone database in the
  * bundle; the fix was to move it off the exported class, not to hide it. If a member is
  * Scala-facing only, prefer an extension method on the companion object, which Scala finds without
  * an import and the linker does not treat as a root. See `docs/internal/JS_BUNDLE_SIZE.md`.
  */
final class TsIgnore extends StaticAnnotation
