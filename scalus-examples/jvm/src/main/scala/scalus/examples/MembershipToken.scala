package scalus.examples

import scalus.compiler.Compile

import scalus.*
import scalus.compiler.Options
import scalus.uplc.PlutusV3
import scalus.uplc.builtin.Builtins.unBData
import scalus.uplc.builtin.{ByteString, Data}
import scalus.uplc.builtin.Data.{FromData, ToData}
import scalus.cardano.onchain.plutus.prelude.*
import scalus.cardano.onchain.plutus.v1.Value.getLovelace
import scalus.cardano.onchain.plutus.v2.OutputDatum
import scalus.cardano.onchain.plutus.v3.*
import scalus.cardano.onchain.plutus.v3.ParameterizedValidator

// ============================================================================
// DATA MODELS
// ============================================================================

/** Datum stored with the deposit UTxO at the script's spending address.
  *
  * @param depositor
  *   public key hash of the member who deposited
  */
case class MembershipDatum(depositor: ByteString) derives FromData, ToData

@Compile
object MembershipDatum {
    given Eq[MembershipDatum] = Eq.derived
}

/** Redeemer for the membership token validator.
  *
  *   - Mint: provide a Merkle proof to mint a membership token + create deposit
  *   - Burn: burn the membership token and reclaim the deposit
  */
enum MembershipRedeemer derives FromData, ToData:
    case Mint(proof: Data)
    case Burn

@Compile
object MembershipRedeemer

// ============================================================================
// VALIDATOR
// ============================================================================

/** Membership token validator parameterized by the Merkle root.
  *
  * The Merkle root is baked into the script at deployment time. This single validator handles both
  * minting and spending purposes:
  *
  * '''Minting:'''
  *   - On `Mint(proof)`: verifies the signer is in the Merkle tree, mints exactly 1 token with
  *     tokenName = signer's pkh, and requires a deposit UTxO at the script address.
  *   - On `Burn`: verifies exactly 1 token is burned (quantity = -1).
  *
  * '''Spending (deposit withdrawal):'''
  *   - On `Burn`: verifies the membership token (policyId = own script hash, tokenName = depositor)
  *     is burned, releasing the deposit.
  */
@Compile
object MembershipTokenValidator extends ParameterizedValidator[ByteString] {
    import scalus.cardano.onchain.plutus.crypto.tree.MerkleTree

    inline override def mint(
        merkleRoot: ByteString,
        redeemer: Data,
        policyId: PolicyId,
        txInfo: TxInfo
    ): Unit = {
        val action = redeemer.to[MembershipRedeemer]
        action match
            case MembershipRedeemer.Mint(proofData) =>
                require(txInfo.signatories.length > BigInt(0), "No signatories")
                val signer = txInfo.signatories.head
                val proof = unBData(proofData)
                MerkleTree.verifyMembership(merkleRoot, signer.hash, proof)

                // Verify exactly 1 token minted with tokenName = signer's pkh
                require(
                  txInfo.mint.quantityOf(policyId, signer.hash) === BigInt(1),
                  "Must mint exactly 1 membership token"
                )

                // Verify only one token name under this policy
                val allMinted = txInfo.mint.flatten.filter { case (pid, _, _) =>
                    pid === policyId
                }
                require(allMinted.length === BigInt(1), "Only one token allowed per mint")

                // Verify deposit UTxO at script address
                val scriptCred = Credential.ScriptCredential(policyId)
                val depositOutputs =
                    txInfo.findOwnOutputsByCredential(scriptCred)
                require(depositOutputs.length === BigInt(1), "Expected one deposit output")
                val depositOut = depositOutputs.head
                require(
                  depositOut.value.getLovelace >= BigInt(2_000_000),
                  "Deposit must be at least 2 ADA"
                )
                val depositDatum = depositOut.datum match
                    case OutputDatum.OutputDatum(d) => d.to[MembershipDatum]
                    case _                          => fail("Expected inline datum on deposit")
                require(
                  depositDatum.depositor === signer.hash,
                  "Deposit datum must reference the signer"
                )

            case MembershipRedeemer.Burn =>
                // Verify exactly 1 token burned
                val allMinted = txInfo.mint.flatten.filter { case (pid, _, _) =>
                    pid === policyId
                }
                require(allMinted.length === BigInt(1), "Expected exactly one burn entry")
                allMinted.foreach { case (_, _, qty) =>
                    require(qty === BigInt(-1), "Must burn exactly 1 token")
                }
    }

    inline override def spend(
        merkleRoot: ByteString,
        datum: Option[Data],
        redeemer: Data,
        txInfo: TxInfo,
        txOutRef: TxOutRef
    ): Unit = {
        val action = redeemer.to[MembershipRedeemer]
        action match
            case MembershipRedeemer.Burn =>
                val depositDatum = datum.getOrFail("Expected datum").to[MembershipDatum]
                val ownInput = txInfo.findOwnInputOrFail(txOutRef).resolved
                val policyId = ownInput.address.credential match
                    case Credential.ScriptCredential(hash) => hash
                    case _                                 => fail("Expected script credential")

                // Verify the membership token is burned
                require(
                  txInfo.mint.quantityOf(policyId, depositDatum.depositor) === BigInt(-1),
                  "Membership token must be burned"
                )
            case _ => fail("Only Burn allowed for spending")
    }
}

// ============================================================================
// CONTRACT COMPILATION
// ============================================================================

private object MembershipTokenCompilation:
    private given Options = Options.release
    lazy val contract = PlutusV3.compile(MembershipTokenValidator.validate)

lazy val MembershipTokenContract = MembershipTokenCompilation.contract
