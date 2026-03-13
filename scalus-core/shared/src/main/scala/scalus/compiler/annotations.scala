package scalus.compiler

import scala.annotation.Annotation
import scala.annotation.StaticAnnotation

final class Compile extends Annotation
final class Ignore extends Annotation

/** Marks a trait/class as having an on-chain substitute object.
  *
  * When the Scalus plugin compiles a method call on a type annotated with `@OnChainSubstitute`, it
  * redirects the call to the substitute `@Compile object`. The substitute object must define
  * methods matching the original trait's methods, with an additional `self` first parameter.
  *
  * On-chain, `self` receives the actual qualifier value (cast via `typeProxy` if `selfAs` is
  * specified). Off-chain, the trait works as normal OOP — the annotation is ignored.
  *
  * @param substitute
  *   A `@Compile object` providing on-chain implementations for each trait method.
  * @param selfAs
  *   The on-chain type for `self`. The plugin inserts a zero-cost `typeProxy` cast from the trait
  *   type to `selfAs`. Defaults to `Nothing` (no cast, `self` keeps the original type).
  *
  * Example:
  * {{{
  * @OnChainSubstitute(OnChainCellOps, classOf[ScriptContext])
  * trait CellContext {
  *   def requireSignedBy(pkh: PubKeyHash): Unit
  * }
  *
  * @Compile
  * object OnChainCellOps {
  *   def requireSignedBy(self: ScriptContext, pkh: PubKeyHash): Unit =
  *       require(self.txInfo.isSignedBy(pkh), "missing signature")
  * }
  * }}}
  */
final class OnChainSubstitute(substitute: AnyRef, selfAs: Class[?] = classOf[Nothing])
    extends StaticAnnotation

/** Marker trait for annotations that should be propagated to SIR Module metadata.
  *
  * The compiler plugin finds all annotations on `@Compile` objects whose class extends this trait
  * and serializes their arguments generically into `Module.anns.data`. Adding a new annotation
  * subclass requires zero plugin changes.
  *
  * Keys in `anns.data` are `"annotationClassName"` for simple args or `"annotationClassName:key"`
  * for Map entries.
  */
trait SIRModuleAnnotation extends StaticAnnotation

/** Type-safe specification of UPLC type representations.
  *
  * This enum defines how a type should be represented in UPLC (Untyped Plutus Lambda Calculus).
  * Used with the [[UplcRepr]] annotation to customize type representation at compile time.
  */
sealed trait UplcRepresentation

object UplcRepresentation {
    // Simple representations (no parameters)
    case object ProductCase extends UplcRepresentation
    case object SumCase extends UplcRepresentation
    case object Map extends UplcRepresentation
    case object SumDataList extends UplcRepresentation
    case object SumPairDataList extends UplcRepresentation
    case object Data extends UplcRepresentation
    case object BuiltinArray extends UplcRepresentation

    // One-element wrapper - inner type derived from first constructor parameter
    case object ProductCaseOneElement extends UplcRepresentation
}

/** Specifies the UPLC representation for a case class or enum.
  *
  * When applied to a type, this annotation directs the Scalus compiler to use the specified
  * representation instead of the default structural inference.
  *
  * @param repr
  *   The representation from [[UplcRepresentation]]
  *
  * @example
  *   {{{
  *   @UplcRepr(UplcRepresentation.ProductCaseOneElement)
  *   case class PubKeyHash(hash: ByteString)
  *
  *   @UplcRepr(UplcRepresentation.Map)
  *   case class AssocMap[K, V](inner: List[(K, V)])
  *   }}}
  */
final class UplcRepr(repr: UplcRepresentation) extends StaticAnnotation
