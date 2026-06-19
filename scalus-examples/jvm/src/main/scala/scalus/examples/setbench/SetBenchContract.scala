package scalus.examples.setbench

import scalus.cardano.blueprint.{Blueprint, Contract}
import scalus.compiler.Options
import scalus.uplc.PlutusV3

private given Options = Options.release

/** Merkle-Patricia-forest set membership benchmark (optimized representation). */
object Mpf16oContract extends Contract {
    lazy val compiled = PlutusV3.compile(SetBenchMpf16oValidator.validate)
    lazy val blueprint = Blueprint.plutusV3[SetBenchDatum, SetBenchRedeemer](
      title = "Set benchmark — MPF-16o",
      description =
          "Set membership benchmark backed by a Merkle Patricia Forest (optimized 16-ary nodes).",
      version = "1.0.0",
      license = Some("Apache-2.0"),
      compiled = compiled
    )
}

/** Merkle-Patricia-forest set membership benchmark (optimized, light proofs). */
object Mpf16oLightContract extends Contract {
    lazy val compiled = PlutusV3.compile(SetBenchMpf16oLightValidator.validate)
    lazy val blueprint = Blueprint.plutusV3[SetBenchDatum, SetBenchRedeemer](
      title = "Set benchmark — MPF-16o-light",
      description =
          "Set membership benchmark backed by a Merkle Patricia Forest (optimized 16-ary nodes, " +
              "light proof encoding).",
      version = "1.0.0",
      license = Some("Apache-2.0"),
      compiled = compiled
    )
}

/** Merkle-Patricia-forest set membership benchmark (basic representation). */
object Mpf16bContract extends Contract {
    lazy val compiled = PlutusV3.compile(SetBenchMpf16bValidator.validate)
    lazy val blueprint = Blueprint.plutusV3[SetBenchDatum, SetBenchRedeemer](
      title = "Set benchmark — MPF-16b",
      description =
          "Set membership benchmark backed by a Merkle Patricia Forest (basic 16-ary nodes).",
      version = "1.0.0",
      license = Some("Apache-2.0"),
      compiled = compiled
    )
}

/** Merkle-Patricia-forest set membership benchmark (basic, light proofs). */
object Mpf16bLightContract extends Contract {
    lazy val compiled = PlutusV3.compile(SetBenchMpf16bLightValidator.validate)
    lazy val blueprint = Blueprint.plutusV3[SetBenchDatum, SetBenchRedeemer](
      title = "Set benchmark — MPF-16b-light",
      description =
          "Set membership benchmark backed by a Merkle Patricia Forest (basic 16-ary nodes, light " +
              "proof encoding).",
      version = "1.0.0",
      license = Some("Apache-2.0"),
      compiled = compiled
    )
}

/** Cryptographic-accumulator set membership benchmark. */
object AccContract extends Contract {
    lazy val compiled = PlutusV3.compile(SetBenchAccValidator.validate)
    lazy val blueprint = Blueprint.plutusV3[SetBenchDatum, AccWithdrawRedeemer](
      title = "Set benchmark — accumulator",
      description = "Set membership benchmark backed by a cryptographic accumulator.",
      version = "1.0.0",
      license = Some("Apache-2.0"),
      compiled = compiled
    )
}

/** Indexed-Merkle-tree set membership benchmark. */
object ImtContract extends Contract {
    lazy val compiled = PlutusV3.compile(SetBenchImtValidator.validate)
    lazy val blueprint = Blueprint.plutusV3[ImtDatum, ImtRedeemer](
      title = "Set benchmark — indexed Merkle tree",
      description = "Set membership benchmark backed by an indexed Merkle tree (IMT).",
      version = "1.0.0",
      license = Some("Apache-2.0"),
      compiled = compiled
    )
}
