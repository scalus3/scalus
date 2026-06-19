package scalus.examples.linkedlist

import scalus.cardano.blueprint.{Blueprint, Contract}
import scalus.compiler.Options
import scalus.uplc.PlutusV3
import scalus.uplc.builtin.Data

/** Blueprint and compiled script for the on-chain linked-list contract. */
object LinkedListContract extends Contract {
    private given Options = Options.release
    lazy val compiled = PlutusV3.compile(LinkedListValidator.validate)
    lazy val blueprint = Blueprint.plutusV3[ListConfig, ListAction](
      title = "On-chain linked list",
      description =
          "Sorted on-chain associative linked list parameterized by a ListConfig. Init/Insert/" +
              "Remove operations maintain ordering and node-pointer integrity via the minting " +
              "policy; spending guards continuation of node UTxOs.",
      version = "1.0.0",
      license = Some("Apache-2.0"),
      // DataParameterizedValidator applies the ListConfig parameter as Data on the UPLC level; the
      // cast only re-labels the phantom type for schema derivation.
      compiled = compiled.asInstanceOf[PlutusV3[ListConfig => Data => Unit]]
    )
}
