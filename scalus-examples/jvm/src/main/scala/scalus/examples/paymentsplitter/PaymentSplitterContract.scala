package scalus.examples.paymentsplitter

import scalus.cardano.blueprint.{Blueprint, Contract}
import scalus.compiler.Options
import scalus.cardano.onchain.plutus.prelude.List
import scalus.uplc.PlutusV3
import scalus.uplc.builtin.{ByteString, Data}

/** Naive payment splitter: split payouts equally among a fixed list of payees. */
object NaivePaymentSplitterContract extends Contract {
    private given Options = Options.release
    lazy val compiled = PlutusV3.compile(NaivePaymentSplitterValidator.validate)
    lazy val blueprint = Blueprint.plutusV3[List[ByteString], Unit, Unit](
      title = "Naive Payment Splitter",
      description =
          "Split payouts equally among a list of specified payees (naive implementation). " +
              "Parameterized by the payee public key hashes; datum and redeemer are unused.",
      version = "1.0.0",
      license = Some("Apache-2.0"),
      // DataParameterizedValidator applies the payee list parameter as Data on the UPLC level; the
      // cast only re-labels the phantom type for schema derivation.
      compiled = compiled.asInstanceOf[PlutusV3[List[ByteString] => Data => Unit]]
    )
}

/** Optimized payment splitter using the stake-validator (withdraw-zero) pattern. */
object OptimizedPaymentSplitterContract extends Contract {
    private given Options = Options.release
    lazy val compiled = PlutusV3.compile(OptimizedPaymentSplitterValidator.validate)
    lazy val blueprint = Blueprint.plutusV3[List[ByteString], Unit, SplitVerificationRedeemer](
      title = "Optimized Payment Splitter",
      description =
          "Split payouts equally among a list of specified payees (optimized with the stake " +
              "validator pattern). Parameterized by the payee public key hashes; the split is " +
              "verified once in the reward endpoint.",
      version = "1.0.0",
      license = Some("Apache-2.0"),
      // DataParameterizedValidator applies the payee list parameter as Data on the UPLC level; the
      // cast only re-labels the phantom type for schema derivation.
      compiled = compiled.asInstanceOf[PlutusV3[List[ByteString] => Data => Unit]]
    )
}
