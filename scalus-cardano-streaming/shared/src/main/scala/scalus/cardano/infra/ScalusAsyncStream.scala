package scalus.cardano.infra

/** Typeclass describing how the embedded data node pushes events into a
  * subscriber's stream representation.
  *
  * Instances are provided by per-library adapter modules (fs2, pekko-streams,
  * ox). The set of required methods is not yet fixed — this is an initial
  * API draft. Expected capabilities:
  *
  *   - construct a stream from a push-based source (the data node emits;
  *     the adapter wraps it in the target stream type)
  *   - carry a bounded buffer / backpressure policy
  *   - propagate completion and error signals
  *
  * Methods will be added alongside the first concrete instance.
  */
trait ScalusAsyncStream[S[_]] {}
