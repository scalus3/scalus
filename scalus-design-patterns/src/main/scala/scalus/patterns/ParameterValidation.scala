package scalus.patterns

import scalus.*
import scalus.uplc.builtin.{ByteString, Data}
import scalus.cardano.ledger.{Script, ScriptHash}
import scalus.cardano.onchain.plutus.v3.{Address, Credential, TxOut, ValidatorHash}
import scalus.cardano.onchain.plutus.prelude.*
import scalus.uplc.DeBruijnedProgram

/** Parameter Validation Pattern
  *
  * Enables verification that script instances are legitimate instantiations of parameterized
  * scripts with specific parameter values.
  *
  * Use Cases:
  *   - Minting Policy → Spending Script: A minting policy needs to verify that tokens are only sent
  *     to a spending script parameterized with specific values (e.g., correct royalty address)
  *   - Multi-Script Coordination: A coordinator script verifies that helper scripts have the
  *     expected parameters
  *   - Factory Pattern: A factory mints tokens that reference scripts with verified parameters
  *
  * How Script Hashing Works in Cardano:
  * {{{
  * script_hash = blake2b_224(language_tag ++ cbor_encoded_program)
  *
  * Language tags:
  *   - PlutusV1: 0x01
  *   - PlutusV2: 0x02
  *   - PlutusV3: 0x03
  *
  * When parameters are applied:
  *   parameterized_program = base_program $ param1 $ param2 ...
  *   parameterized_hash = blake2b_224(language_tag ++ cbor(parameterized_program))
  * }}}
  *
  * @see
  *   [[https://github.com/Anastasia-Labs/aiken-design-patterns/blob/develop/lib/aiken-design-patterns/parameter-validation.ak]]
  */
object ParameterValidation {

    /** Compute script hash for a PlutusV3 program with applied parameters.
      *
      * @param baseProgram
      *   The compiled script before parameter application
      * @param params
      *   Parameters to apply (in order)
      * @return
      *   The script hash of the parameterized script
      */
    def computeScriptHashV3(baseProgram: DeBruijnedProgram, params: Data*): ScriptHash = {
        val parameterized = params.foldLeft(baseProgram)(_ $ _)
        Script.PlutusV3(ByteString.unsafeFromArray(parameterized.cborEncoded)).scriptHash
    }

    /** Compute script hash for a PlutusV2 program with applied parameters.
      *
      * @param baseProgram
      *   The compiled script before parameter application
      * @param params
      *   Parameters to apply (in order)
      * @return
      *   The script hash of the parameterized script
      */
    def computeScriptHashV2(baseProgram: DeBruijnedProgram, params: Data*): ScriptHash = {
        val parameterized = params.foldLeft(baseProgram)(_ $ _)
        Script.PlutusV2(ByteString.unsafeFromArray(parameterized.cborEncoded)).scriptHash
    }

    /** Compute script hash for a PlutusV1 program with applied parameters.
      *
      * @param baseProgram
      *   The compiled script before parameter application
      * @param params
      *   Parameters to apply (in order)
      * @return
      *   The script hash of the parameterized script
      */
    def computeScriptHashV1(baseProgram: DeBruijnedProgram, params: Data*): ScriptHash = {
        val parameterized = params.foldLeft(baseProgram)(_ $ _)
        Script.PlutusV1(ByteString.unsafeFromArray(parameterized.cborEncoded)).scriptHash
    }
}

/** On-chain helpers for parameter validation.
  *
  * These functions are compiled to UPLC and can be used in validators to verify that addresses or
  * credentials correspond to expected parameterized scripts.
  */
@Compile
object ParameterValidationOnChain {

    /** Verify that a credential matches the expected script hash.
      *
      * Use this in minting policies to verify tokens go to the correct parameterized spending
      * script.
      *
      * @param credential
      *   The credential to verify
      * @param expectedHash
      *   The expected script hash
      */
    inline def verifyScriptCredential(
        credential: Credential,
        expectedHash: ValidatorHash
    ): Unit = {
        val actualHash = credential.scriptOption.getOrFail(ExpectedScriptCredential)
        require(actualHash === expectedHash, ScriptHashMismatch)
    }

    /** Verify that an address has the expected script credential.
      *
      * @param address
      *   The address to verify
      * @param expectedHash
      *   The expected script hash
      */
    inline def verifyAddressScript(
        address: Address,
        expectedHash: ValidatorHash
    ): Unit = {
        verifyScriptCredential(address.credential, expectedHash)
    }

    /** Find outputs to a specific script hash.
      *
      * @param outputs
      *   The list of transaction outputs
      * @param scriptHash
      *   The script hash to search for
      * @return
      *   List of outputs sent to the specified script
      */
    inline def findOutputsToScript(
        outputs: List[TxOut],
        scriptHash: ValidatorHash
    ): List[TxOut] = {
        val scriptCred = Credential.ScriptCredential(scriptHash)
        outputs.filter(_.address.credential === scriptCred)
    }

    /** Check if a credential is a script credential with the expected hash.
      *
      * @param credential
      *   The credential to check
      * @param expectedHash
      *   The expected script hash
      * @return
      *   true if the credential is a script credential matching the expected hash
      */
    inline def isExpectedScript(
        credential: Credential,
        expectedHash: ValidatorHash
    ): Boolean = {
        credential.scriptOption match
            case Option.Some(hash) => hash === expectedHash
            case Option.None       => false
    }

    inline val ExpectedScriptCredential = "Expected script credential, got pub key"
    inline val ScriptHashMismatch = "Script hash does not match expected"
}
