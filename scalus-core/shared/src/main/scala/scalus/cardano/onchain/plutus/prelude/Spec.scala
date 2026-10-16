package scalus.cardano.onchain.plutus.prelude

import scalus.cardano.onchain.SpecificationError
import scalus.compiler.Compile

/** The contract of a function, written in its body, next to the code it describes: what its callers
  * owe, and what it guarantees.
  *
  * {{{
  * def clamp(x: BigInt, lo: BigInt, hi: BigInt): BigInt = {
  *     Spec.expects(lo <= hi)
  *     (if x < lo then lo else if x > hi then hi else x).ensuring(r => lo <= r && r <= hi)
  * }
  *
  * inline override def spend(datum: Option[Data], redeemer: Data, tx: TxInfo, ref: TxOutRef): Unit = {
  *     Spec.ensures(tx.isSignedBy(configOf(datum).beneficiary))
  *     ...
  * }
  * }}}
  *
  * A specification is not a check of the script. The clauses are removed before the code is
  * lowered, so a script's bytes and hash are the same with and without them, and they cost nothing
  * on chain. They are kept in the function's SIR, where a verifier reads them: the guarantees are
  * proved about the compiled code, and each caller is checked to establish `expects`.
  *
  * Use [[require]] for a condition the script must enforce against any transaction: an entry point
  * has no caller to rely on. A validator's handler states what it guarantees with [[ensures]]: what
  * holds of every transaction the script accepts.
  */
@Compile
object Spec {

    /** What every caller establishes, and the function may assume. Written first in the body.
      * Off-chain, where the same code runs as Scala, it is checked and throws a
      * [[scalus.cardano.onchain.SpecificationError]].
      */
    def expects(condition: Boolean): Unit =
        if condition then () else throw new SpecificationError("a precondition does not hold")

    /** What holds of the function's parameters wherever it returns. Written at the head of the
      * body, with the `expects` clauses. For a validator's handler, which returns nothing, this is
      * the guarantee: where the script succeeds, `condition` holds.
      *
      * It is not evaluated off-chain: at the head of the body it is not known yet whether the
      * function returns.
      */
    inline def ensures(inline condition: Boolean): Unit = holdsOnReturn(_ => condition)

    /** The function behind [[ensures]], which keeps the condition unevaluated. */
    def holdsOnReturn(condition: Unit => Boolean): Unit = ()

    extension [A](body: A) {

        /** What the function guarantees of its result, where it returns one. Applied to the body's
          * last expression, or to the whole body. It takes the place of `Predef`'s `ensuring`,
          * which the Scalus compiler does not read. Off-chain it is checked.
          *
          * The expression it is applied to is typed on its own, without the function's result type:
          * a branch that is the literal `0` needs to be written `BigInt(0)`.
          */
        def ensuring(condition: A => Boolean): A =
            if condition(body) then body
            else throw new SpecificationError("a postcondition does not hold")
    }
}
