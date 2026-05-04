package scalus

import scala.annotation.Annotation

final class ScalusDebug(val debugLevel: Int) extends Annotation

/** This is a marker trait for the compiler plugin to compile derivations of the instances of the
  * type classes.
  * @see
  *   scalus.prelude.ToData, scalus.prelude.FromData
  */
trait CompileDerivations
