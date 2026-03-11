package scalus.utxocells

import scalus.Compile
import scalus.uplc.builtin.ByteString
import scalus.uplc.builtin.Data
import scalus.uplc.builtin.Data.toData
import scalus.cardano.onchain.plutus.v1.{Address, Credential, PolicyId, PosixTime, PubKeyHash, Value}
import scalus.cardano.onchain.plutus.v2
import scalus.cardano.onchain.plutus.v3.*
import scalus.cardano.onchain.plutus.prelude.*

/** On-chain substitute for [[CellContext]]. Receives `ScriptContext` as `self`. */
@Compile
object CellContextOps {

    def txInfo(self: ScriptContext): Data = self.txInfo.toData

    def ownPolicyId(self: ScriptContext): PolicyId =
        self.scriptInfo match
            case ScriptInfo.MintingScript(pid) => pid
            case ScriptInfo.SpendingScript(ownRef, _) =>
                val ownInput = self.txInfo.findOwnInputOrFail(ownRef)
                ownInput.resolved.address.credential match
                    case Credential.ScriptCredential(hash) => hash
                    case _ => fail("CellContext: input is not a script")
            case _ => fail("CellContext: unsupported script info")

    def ownInputValue(self: ScriptContext): Value =
        self.scriptInfo match
            case ScriptInfo.SpendingScript(ownRef, _) =>
                self.txInfo.findOwnInputOrFail(ownRef).resolved.value
            case _ => fail("CellContext: not spending")

    def mint(self: ScriptContext, tokenName: ByteString, amount: BigInt): Unit = {
        val policyId = ownPolicyId(self)
        val mintedTokens = self.txInfo.mint.tokens(policyId)
        require(
          mintedTokens.length === BigInt(1),
          "CellContext: only one token name may be minted under this policy"
        )
        require(
          mintedTokens.keys.head === tokenName,
          "CellContext: unexpected token name minted"
        )
        require(
          mintedTokens.values.head === amount,
          "CellContext: mint quantity mismatch"
        )
    }

    def requireInputToken(
        self: ScriptContext,
        policyId: PolicyId,
        tokenName: ByteString,
        quantity: BigInt
    ): Unit = {
        val ownValue = ownInputValue(self)
        require(
          ownValue.quantityOf(policyId, tokenName) >= quantity,
          "CellContext: required token not present in cell input"
        )
    }

    def setContinuingValue(self: ScriptContext, value: Value): Unit = {
        self.scriptInfo match
            case ScriptInfo.SpendingScript(ownRef, _) =>
                val ownInput = self.txInfo.findOwnInputOrFail(ownRef)
                val scriptHash = ownInput.resolved.address.credential match
                    case Credential.ScriptCredential(hash) => hash
                    case _ => fail("CellContext: input is not a script")
                val outputs = self.txInfo.findOwnScriptOutputs(scriptHash)
                require(outputs.length === BigInt(1), "CellContext: expected one continuing output")
                UtxoCellLib.verifyStakingCredential(
                  ownInput.resolved.address,
                  outputs.head.address
                )
                require(
                  UtxoCellLib.valueGeq(outputs.head.value, value),
                  "CellContext: continuing output value insufficient"
                )
            case _ => fail("CellContext: not spending")
    }
}

/** On-chain substitute for [[CellTxInfo]]. Receives `TxInfo` (as Data) as `self`. */
@Compile
object CellTxInfoOps {

    def outputs(self: Data): Data = self.to[TxInfo].outputs.toData

    def requireSignedBy(self: Data, pkh: PubKeyHash): Unit =
        require(self.to[TxInfo].isSignedBy(pkh), "CellContext: missing required signature")

    def requireValidAfter(self: Data, time: PosixTime): Unit =
        require(
          self.to[TxInfo].getValidityStartTime >= time,
          "CellContext: validity range not after required time"
        )

    def requireValidBefore(self: Data, time: PosixTime): Unit =
        self.to[TxInfo].validRange.to.boundType match
            case IntervalBoundType.Finite(t) =>
                require(t <= time, "CellContext: validity range not before required time")
            case _ => fail("CellContext: no finite upper bound on validity range")
}

/** On-chain substitute for [[CellOutputs]]. Receives `List[TxOut]` (as Data) as `self`.
  *
  * WARNING (V005): Each `add` call independently searches all tx outputs via `exists`. If the
  * transition calls `add` multiple times with the same address and overlapping value, a single
  * output can satisfy multiple checks (double satisfaction). For transitions producing multiple
  * outputs to the same address, use `UtxoCellLib.verifyOutputs` for V005-safe sequential matching.
  */
@Compile
object CellOutputsOps {

    def add(self: Data, address: Address, value: Value): Unit = {
        val found = self.to[List[v2.TxOut]].exists { out =>
            out.address === address && UtxoCellLib.valueGeq(out.value, value)
        }
        require(found, "CellContext: expected output not found in transaction")
    }
}
