package scalus.examples

import scalus.*
import scalus.uplc.builtin.Data
import scalus.compiler.Options
import scalus.cardano.onchain.plutus.v3.*
import scalus.patterns.StakeValidator
import scalus.cardano.onchain.plutus.prelude.*
import scalus.uplc.PlutusV3

/** Example for a validator that requires a withdrawal from its script for each spend. Note that
  * depending on an external script is typically more performant.
  */
@Compile
object StakeValidatorExample extends Validator {
    inline override def spend(
        datum: Option[Data],
        redeemer: Redeemer,
        tx: TxInfo,
        ownRef: TxOutRef
    ): Unit = {
        val ownCredential = tx.findOwnInputOrFail(ownRef).resolved.address.credential
        val ownWithdrawal = ownCredential.scriptOption.getOrFail("Own address must be Script")

        StakeValidator.spend(
          withdrawalScriptHash = ownWithdrawal,
          withdrawalRedeemerValidator = (redeemer, lovelace) => lovelace === BigInt(0),
          txInfo = tx
        )
    }

    inline override def reward(redeemer: Redeemer, stakingKey: Credential, tx: TxInfo): Unit = {
        StakeValidator.withdraw(
          withdrawalValidator = (redeemer, validatorHash, txInfo) => true,
          redeemer = redeemer,
          credential = stakingKey,
          txInfo = tx
        )
    }
}

private object StakeValidatorCompilation:
    private given stakeValidatorOptions: Options = Options.release
    lazy val contract = PlutusV3.compile(StakeValidatorExample.validate)

lazy val StakeValidatorExampleContract = StakeValidatorCompilation.contract
