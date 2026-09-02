package scalus.compiler.sir

/** Moved to [[scalus.compiler.sir.transform.StaticArgumentTransformation]], alongside the other
  * SIR-to-SIR passes. This alias forwards to the new location so existing callers keep compiling
  * and linking.
  */
@deprecated("use scalus.compiler.sir.transform.StaticArgumentTransformation instead", "1.1.1")
object StaticArgumentTransformation {

    /** Forwards to [[scalus.compiler.sir.transform.StaticArgumentTransformation.SatSuffix]]. */
    val SatSuffix: String = transform.StaticArgumentTransformation.SatSuffix

    /** Forwards to [[scalus.compiler.sir.transform.StaticArgumentTransformation.apply]]. */
    def apply(sir: SIR): SIR = transform.StaticArgumentTransformation(sir)
}
