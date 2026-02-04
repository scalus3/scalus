package scalus.cardano.onchain.plutus.v3

import scalus.Compile
import scalus.uplc.builtin.Data
import scalus.cardano.onchain.plutus.v1.{Credential, PolicyId}
import scalus.cardano.onchain.plutus.prelude.*

@Compile
trait Validator {

    inline def validate(scData: Data): Unit = {
        validateScriptContext(scData.to[ScriptContext])
    }

    inline def validateScriptContext(sc: ScriptContext): Unit = {
        sc.scriptInfo match
            case ScriptInfo.MintingScript(policyId) =>
                mint(sc.redeemer, policyId, sc.txInfo)
            case ScriptInfo.SpendingScript(txOutRef, datum) =>
                spend(datum, sc.redeemer, sc.txInfo, txOutRef)
            case ScriptInfo.RewardingScript(credential) =>
                reward(sc.redeemer, credential, sc.txInfo)
            case ScriptInfo.CertifyingScript(index, cert) =>
                certify(sc.redeemer, cert, sc.txInfo)
            case ScriptInfo.VotingScript(voter) =>
                vote(sc.redeemer, voter, sc.txInfo)
            case ScriptInfo.ProposingScript(index, procedure) =>
                propose(procedure, sc.txInfo)
    }

    inline def spend(
        datum: Option[Data],
        redeemer: Data,
        tx: TxInfo,
        ownRef: TxOutRef
    ): Unit
    // = {
    //    // send the script to the blockchain
    //    fail("Empty Validator.spend")
    // }

    inline def mint(
        redeemer: Data,
        policyId: PolicyId,
        tx: TxInfo
    ): Unit
    // = {
    //    fail("Empty Validator.mint")
    // }

    inline def reward(
        redeemer: Data,
        stakingKey: Credential,
        tx: TxInfo
    ): Unit
    // = {
    //    fail("Empty Validator.reward")
    // }

    inline def certify(
        redeemer: Data,
        cert: TxCert,
        tx: TxInfo
    ): Unit
    // = {
    //    fail("Empty Validator.certify")
    // }

    inline def vote(
        redeemer: Data,
        voter: Voter,
        tx: TxInfo
    ): Unit
    // =
    // {
    //    fail("Empty Validator.vote")
    // }

    inline def propose(
        proposalProcedure: ProposalProcedure,
        tx: TxInfo
    ): Unit
    // = {
    //    fail("Empty Validator.propose")
    // }

}

// erased [not available yet]
@Compile
trait ParameterizedValidator[A] {

    inline def validate(param: A)(scData: Data): Unit = {
        val sc = scData.to[ScriptContext]
        sc.scriptInfo match
            case ScriptInfo.MintingScript(policyId) =>
                mint(param, sc.redeemer, policyId, sc.txInfo)
            case ScriptInfo.SpendingScript(txOutRef, datum) =>
                spend(param, datum, sc.redeemer, sc.txInfo, txOutRef)
            case ScriptInfo.RewardingScript(credential) =>
                reward(param, sc.redeemer, credential, sc.txInfo)
            case ScriptInfo.CertifyingScript(index, cert) =>
                certify(param, sc.redeemer, cert, sc.txInfo)
            case ScriptInfo.VotingScript(voter) =>
                vote(param, sc.redeemer, voter, sc.txInfo)
            case ScriptInfo.ProposingScript(index, procedure) =>
                propose(param, procedure, sc.txInfo)
    }

    inline def spend(
        param: A,
        datum: Option[Data],
        redeemer: Data,
        tx: TxInfo,
        ownRef: TxOutRef
    ): Unit
    // = {
    // send the script to the blockchain
    // fail("Empty Validator.spend")
    // }

    inline def mint(
        param: A,
        redeemer: Data,
        policyId: PolicyId,
        tx: TxInfo
    ): Unit
    // = {
    //    fail("Empty Validator.mint")
    // }

    inline def reward(
        param: A,
        redeemer: Data,
        stakingKey: Credential,
        tx: TxInfo
    ): Unit
    // =
    // {
    //    fail("Empty Validator.reward")
    // }

    inline def certify(
        param: A,
        redeemer: Data,
        cert: TxCert,
        tx: TxInfo
    ): Unit
    // = {
    //    fail("Empty Validator.certify")
    // }

    inline def vote(
        param: A,
        redeemer: Data,
        voter: Voter,
        tx: TxInfo
    ): Unit
    // = {
    //    fail("Empty Validator.vote")
    // }

    inline def propose(
        param: A,
        proposalProcedure: ProposalProcedure,
        tx: TxInfo
    ): Unit = {
        fail("Empty Validator.propose")
    }

}

/** Validator, parametrized by Data. Keep
  *
  * Needed for compatibility with solutions which use existing uplc scripts and apply Data as
  * parameter on UPLC level.
  */
@Compile
trait DataParameterizedValidator {

    inline def validate(param: Data)(scData: Data): Unit = {
        val sc = scData.to[ScriptContext]
        sc.scriptInfo match
            case ScriptInfo.MintingScript(policyId) =>
                mint(param, sc.redeemer, policyId, sc.txInfo)
            case ScriptInfo.SpendingScript(txOutRef, datum) =>
                spend(param, datum, sc.redeemer, sc.txInfo, txOutRef)
            case ScriptInfo.RewardingScript(credential) =>
                reward(param, sc.redeemer, credential, sc.txInfo)
            case ScriptInfo.CertifyingScript(index, cert) =>
                certify(param, sc.redeemer, cert, sc.txInfo)
            case ScriptInfo.VotingScript(voter) =>
                vote(param, sc.redeemer, voter, sc.txInfo)
            case ScriptInfo.ProposingScript(index, procedure) =>
                propose(param, procedure, sc.txInfo)
    }

    inline def spend(
        param: Data,
        datum: Option[Data],
        redeemer: Data,
        tx: TxInfo,
        ownRef: TxOutRef
    ): Unit
    // = {
    //    // send the script to the blockchain
    //    fail("Empty Validator.spend")
    // }

    inline def mint(
        param: Data,
        redeemer: Data,
        policyId: PolicyId,
        tx: TxInfo
    ): Unit

    inline def reward(
        param: Data,
        redeemer: Data,
        stakingKey: Credential,
        tx: TxInfo
    ): Unit

    inline def certify(
        param: Data,
        redeemer: Data,
        cert: TxCert,
        tx: TxInfo
    ): Unit

    inline def vote(
        param: Data,
        redeemer: Data,
        voter: Voter,
        tx: TxInfo
    ): Unit

    inline def propose(
        param: Data,
        proposalProcedure: ProposalProcedure,
        tx: TxInfo
    ): Unit

}
