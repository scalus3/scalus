package scalus

/** Marker for per-platform interop surface traits.
  *
  * Java/JS-facing members that would degrade the Scala API live in per-platform traits mixed into
  * the shared (or platform) type; every such trait extends this marker so the interop surface is
  * mechanically enumerable by tests.
  *
  * See `docs/superpowers/specs/2026-07-11-cross-language-interop-style-guide-design.md`.
  */
trait InteropApi
