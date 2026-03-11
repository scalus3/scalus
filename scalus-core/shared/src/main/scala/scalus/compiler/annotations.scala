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
