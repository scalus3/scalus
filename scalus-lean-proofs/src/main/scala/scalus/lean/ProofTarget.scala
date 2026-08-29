package scalus.lean

import scalus.uplc.Program

/** One compiled UPLC program that the Lean proof suite reasons about.
  *
  * @param name
  *   lower_snake_case; becomes `<name>.flat` on disk and a camelCase Lean identifier
  * @param program
  *   the compiled program, always built with [[ProofTargets.options]]
  * @param arity
  *   how many integer arguments the program takes
  * @param samples
  *   argument lists paired with the value the program must produce, used both as a JVM-side test
  *   and as the generated Lean differential checks
  */
final case class ProofTarget(
    name: String,
    program: Program,
    arity: Int,
    samples: Seq[(Seq[BigInt], BigInt)]
) {

    /** The Lean identifier for this target, e.g. `math_gcd` becomes `mathGcd`. */
    def leanName: String = ProofTarget.leanNameOf(name)
}

object ProofTarget {

    /** Converts a lower_snake_case target name to a camelCase Lean identifier. */
    def leanNameOf(name: String): String = {
        val parts = name.split('_').filter(_.nonEmpty)
        (parts.head +: parts.tail.map(p => p.head.toUpper +: p.tail)).mkString
    }
}
