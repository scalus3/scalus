package scalus.cardano.blueprint

/** A single-contract blueprint provider.
  *
  * Extend this in an object that defines one compiled contract with its blueprint. The compiler
  * plugin discovers implementations and the `blueprint` sbt task prints the JSON.
  *
  * {{{
  * object MyContract extends Contract {
  *     private given Options = Options.release
  *     lazy val compiled = PlutusV3.compile(MyValidator.validate)
  *     lazy val blueprint = Blueprint.plutusV3[Datum, Redeemer](
  *         title = "My Contract",
  *         description = "...",
  *         version = "1.0.0",
  *         license = None,
  *         compiled = compiled
  *     )
  * }
  * }}}
  */
trait Contract {
    def blueprint: Blueprint

    /** Returns the blueprint as a 2-space-indented JSON string. */
    final def blueprintJson: String = blueprint.toJson(2)
}
