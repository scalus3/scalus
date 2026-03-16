package scalus.examples

import scalus.*
import scalus.uplc.builtin.Data
import scalus.compiler.Options
import scalus.cardano.onchain.plutus.v3.*
import scalus.patterns.{Factory, FactoryAction, ProductDatum}
import scalus.cardano.onchain.plutus.prelude.*
import scalus.cardano.onchain.plutus.v3.Validator
import scalus.uplc.PlutusV3

/** Factory Pattern Example — a combined minting + spending validator.
  *
  * '''Minting (factory):'''
  *   - `Create(tag, seedUtxo)`: mints a product NFT (one-shot via seed UTxO) and locks a product
  *     UTxO at this script's address
  *   - `Destroy`: burns a product NFT
  *
  * '''Spending (product):'''
  *   - Consuming a product UTxO requires burning the NFT and signing by the creator
  *
  * The spending validator address is the same as the minting policy hash (single script), so the
  * `spendingScriptHash` parameter passed to `validateCreate` is the policy's own hash.
  *
  * @see
  *   [[https://github.com/blockchain-unica/rosetta-smart-contracts/tree/main/contracts/factory]]
  */
@Compile
object FactoryExample extends Validator {

    inline override def mint(redeemer: Data, policyId: PolicyId, tx: TxInfo): Unit = {
        val action = redeemer.to[FactoryAction]
        action match
            case FactoryAction.Create(tag, seedUtxo) =>
                // Use first signatory as the creator
                val creator = tx.signatories.head
                Factory.validateCreate(
                  creator = creator,
                  tag = tag,
                  seedUtxo = seedUtxo,
                  policyId = policyId,
                  spendingScriptHash = policyId, // same script for minting and spending
                  tx = tx
                )
            case FactoryAction.Destroy =>
                val creator = tx.signatories.head
                Factory.validateDestroy(
                  creator = creator,
                  policyId = policyId,
                  tx = tx
                )
    }

    inline override def spend(
        datum: Option[Data],
        redeemer: Data,
        tx: TxInfo,
        ownRef: TxOutRef
    ): Unit = {
        val productDatum = datum.getOrFail("Datum required").to[ProductDatum]

        // Derive the factory policy from our own script address
        val ownInput = tx.findOwnInputOrFail(ownRef)
        val factoryPolicyId =
            ownInput.resolved.address.credential.scriptOption
                .getOrFail("Own address must be Script")

        Factory.validateSpend(
          datum = productDatum,
          factoryPolicyId = factoryPolicyId,
          ownInputValue = ownInput.resolved.value,
          tx = tx
        )
    }
}

private object FactoryCompilation {
    private given compilerOptions: Options = Options.release
    lazy val contract = PlutusV3.compile(FactoryExample.validate)
}

lazy val FactoryExampleContract = FactoryCompilation.contract
