package scalus.uplc.eval

import scalus.compiler.compile
import scalus.compiler.sir.SIR
import scalus.uplc.builtin.Builtins.*

/** A sample validator defined in a separate file.
  *
  * Used by CekSourcePosTest to verify that CEK machine errors report source positions pointing to
  * this file, not to the test file.
  */
object SampleValidator {

    /** A validator that requires the input to be positive. Throws on non-positive input. */
    val positiveValidator: SIR = compile { (x: BigInt) =>
        if x > BigInt(0) then x
        else throw new Exception("input must be positive")
    }

    /** A validator that always fails. */
    val alwaysFails: SIR = compile {
        throw new Exception("always fails")
    }

    /** Calls headList on an empty list — triggers a BuiltinError. */
    val headOfEmptyList: SIR = compile {
        headList(mkNilData())
    }

    /** Incomplete pattern match — only handles Cons, not Nil. */
    val incompleteCaseMatch: SIR = compile {
        (lst: scalus.cardano.onchain.plutus.prelude.List[BigInt]) =>
            (lst: @unchecked) match
                case scalus.cardano.onchain.plutus.prelude.List.Cons(h, _) => h
    }

    /** A validator that uses require to check a condition. */
    val requirePositive: SIR = compile { (x: BigInt) =>
        scalus.cardano.onchain.plutus.prelude.require(x > BigInt(0), "input must be positive")
        x
    }
}
