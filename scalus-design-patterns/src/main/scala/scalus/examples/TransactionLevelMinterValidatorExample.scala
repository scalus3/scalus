package scalus.examples

import scalus.*
import scalus.uplc.builtin.{ByteString, Data, FromData}
import scalus.uplc.builtin.ByteString.utf8
import scalus.compiler.Options
import scalus.cardano.onchain.plutus.v3.*
import scalus.patterns.TransactionLevelMinterValidator
import scalus.cardano.onchain.plutus.prelude.*
import scalus.uplc.PlutusV3

@Compile
object TransactionLevelMinterValidatorExample extends Validator {
    case class SampleSpendRedeemer(
        ownIndex: BigInt,
        burn: Boolean
    )

    case class SampleMintRedeemer(maxUtxosToSpend: BigInt)

    given FromData[SampleSpendRedeemer] = FromData.derived
    given FromData[SampleMintRedeemer] = FromData.derived

    /** Sample spend logic on how to use the provided interface. Here we are passing script's own
      * hash as the expected minting policy.
      */
    inline override def spend(
        datum: Option[Data],
        redeemer: Redeemer,
        tx: TxInfo,
        ownRef: TxOutRef
    ): Unit = {
        val sampleSpendRedeemer = redeemer.to[SampleSpendRedeemer]
        // Grabbing spending UTxO based on the provided index.
        val input = tx.inputs.get(sampleSpendRedeemer.ownIndex).getOrFail("Undefined ownIndex")
        val ownCredential = input.resolved.address.credential
        val outRef = input.outRef

        // Validating that the found UTxO is in fact the spending UTxO.
        require(ownRef === outRef)

        // Getting the validator's script hash.
        val ownHash = ownCredential.scriptOption.getOrFail("Own address must be Script")

        /** Utilizing the design pattern, where the underlying logic expects a single "BEACON" token
          * to be either burnt or minted.
          */
        TransactionLevelMinterValidator.spend(
          minterScriptHash = ownHash,
          minterRedeemerValidator = _.to[SampleMintRedeemer].maxUtxosToSpend > 0,
          minterTokensValidator = tnQtyDict => {
              val (tokenName, mintQuantity) = tnQtyDict.toList.head
              require(tokenName === utf8"BEACON")
              if sampleSpendRedeemer.burn then mintQuantity === BigInt(-1)
              else mintQuantity === BigInt(1)
          },
          txInfo = tx
        )
    }

    /** Sample mint logic that benefits from this design pattern. This example expects a specific
      * number of inputs to be spent in each transaction.
      */
    inline override def mint(redeemer: Redeemer, policyId: PolicyId, tx: TxInfo): Unit = {
        val sampleMintRedeemer = redeemer.to[SampleMintRedeemer]
        val scriptInputsCount = tx.inputs.foldRight(BigInt(0)) { (input, acc) =>
            input.resolved.address.credential match
                case Credential.ScriptCredential(validatorHash) =>
                    if validatorHash === policyId then acc + 1 else acc
                case _ => acc
        }

        require(scriptInputsCount === sampleMintRedeemer.maxUtxosToSpend)
    }
}

private object TransactionLevelMinterCompilation:
    private given txLevelMinterOptions: Options = Options.release
    lazy val contract = PlutusV3.compile(TransactionLevelMinterValidatorExample.validate)

lazy val TransactionLevelMinterValidatorExampleContract = TransactionLevelMinterCompilation.contract
