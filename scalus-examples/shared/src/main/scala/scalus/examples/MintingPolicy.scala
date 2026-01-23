package scalus.examples

import scalus.uplc.builtin.Builtins.*
import scalus.uplc.builtin.ByteString.*
import scalus.uplc.builtin.Data.fromData
import scalus.uplc.builtin.{ByteString, Data}
import scalus.compiler.sir.TargetLoweringBackend
import scalus.compiler.{compile, Options}
import scalus.cardano.onchain.plutus.v1.*
import scalus.cardano.onchain.plutus.prelude.*
import scalus.cardano.onchain.plutus.prelude.Option.*
import scalus.{Compile, Ignore}

case class TxInInfoTxOutRefOnly(txInInfoOutRef: TxOutRef)

case class MintingContext(inputs: List[TxOutRef], minted: Value, ownSymbol: PolicyId)

@Compile
object MintingPolicy {
    import ScriptPurpose.*

    given Data.FromData[TxInInfoTxOutRefOnly] = (d: Data) =>
        new TxInInfoTxOutRefOnly(d.toConstr.snd.head.to[TxOutRef])

    protected final val hoskyMintTxOutRef = TxOutRef(
      TxId(hex"1ab6879fc08345f51dc9571ac4f530bf8673e0d798758c470f9af6f98e2f3982"),
      0
    )
    protected final val hoskyMintTxOut = TxOut(
      address = Address(
        Credential.PubKeyCredential(
          PubKeyHash(
            hex"61822dde476439a526070f36d3d1667ad099b462c111cd85e089f5e7f6"
          )
        ),
        None
      ),
      Value.lovelace(BigInt("10000000")),
      None
    )

    val simpleCtxDeserializer: Data => MintingContext = (ctxData: Data) => {
        val ctx = ctxData.to[ScriptContext]
        val txInfo = ctx.txInfo
        val txInfoInputs = txInfo.inputs
        val minted = txInfo.mint
        val purpose = ctx.purpose
        val ownSymbol = purpose match
            case Minting(curSymbol) => curSymbol
            case Spending(txOutRef) => throw new RuntimeException("PS")
            case Rewarding(stakingCred) =>
                throw new RuntimeException("PR")
            case Certifying(cert) => throw new RuntimeException("PC")
        new MintingContext(
          txInfoInputs.map(_.outRef),
          minted,
          ownSymbol
        )
    }

    val optimizedCtxDeserializer: Data => MintingContext = (ctxData: Data) => {
        val txInfoData = ctxData.field[ScriptContext](_.txInfo)
        val txInfoInputs = txInfoData.field[TxInfo](_.inputs).to[List[TxInInfoTxOutRefOnly]]
        val minted = txInfoData.field[TxInfo](_.mint).to[Value]
        val ownSymbol =
            val purpose = ctxData.field[ScriptContext](_.purpose)
            val pair = purpose.toConstr
            val tag = pair.fst
            val args = pair.snd
            if tag == BigInt(0) then args.head.toByteString
            else throw new Exception("P")
        new MintingContext(
          txInfoInputs.map(_.txInInfoOutRef),
          minted,
          ownSymbol
        )
    }
    /* Here we use a custom ScriptContext deserializer
       to avoid deserializing from Data fields that are not used in the script.
       This saves us more than 1000 bytes of the script size.
     */
    def mintingPolicyScript(deserializer: Data => MintingContext)(
        txId: ByteString,
        txOutIdx: BigInt,
        tokensToMint: SortedMap[ByteString, BigInt]
    ) = (redeemer: Unit, ctxData: Data) => {
        deserializer(ctxData) match
            case MintingContext(txOutRefs, minted, ownSymbol) =>
                val mintedTokens = minted.toSortedMap.get(ownSymbol) match
                    case Some(mintedTokens) => mintedTokens
                    case None =>
                        throw new Exception("T")

                val checkSpendsTxOut = txOutRefs.find { case TxOutRef(txOutRefTxId, txOutRefIdx) =>
                    txOutRefTxId.hash == txId && txOutRefIdx == txOutIdx
                }

                inline def check(b: Boolean, inline msg: String): Unit =
                    if b then () else throw new Exception(msg)
                checkSpendsTxOut match
                    // If the transaction spends the TxOut, then it's a minting transaction
                    case Some(input) => check(Value.equalsAssets(mintedTokens, tokensToMint), "M")
                    // Otherwise, it's a burn transaction
                    case None =>
                        // check burned
                        val burned =
                            mintedTokens.toList.forall { case (tokenName, amount) =>
                                lessThanInteger(amount, BigInt(0))
                            }
                        check(burned, "B")
    }

    @Ignore
    given Options = Options(
      targetLoweringBackend = TargetLoweringBackend.SirToUplcV3Lowering
    )

    @Ignore
    val compiledOptimizedMintingPolicyScript = compile(
      mintingPolicyScript(optimizedCtxDeserializer)
    )

    @Ignore
    val compiledMintingPolicyScript = compile(
      mintingPolicyScript(simpleCtxDeserializer)
    )

    // val cbor = Cbor.encode(flatEncoded).toByteArray
    // val cborHex = Utils.bytesToHex(Cbor.encode(flatEncoded).toByteArray)
    // val doubleCborHex = Utils.bytesToHex(Cbor.encode(cbor).toByteArray)
}

@Compile
object MintingPolicyV2 {
    import scalus.cardano.onchain.plutus.v2.*
    import ScriptPurpose.*

    val simpleCtxV2Deserializer: Data => MintingContext = (ctxData: Data) => {
        val ctx = fromData[ScriptContext](ctxData)
        val txInfo = ctx.txInfo
        val txInfoInputs = txInfo.inputs
        val minted = txInfo.mint
        val purpose = ctx.purpose
        val ownSymbol = purpose match
            case Minting(curSymbol) => curSymbol
            case Spending(txOutRef) => throw new RuntimeException("PS")
            case Rewarding(stakingCred) =>
                throw new RuntimeException("PR")
            case Certifying(cert) => throw new RuntimeException("PC")
        new MintingContext(
          txInfoInputs.map(_.outRef),
          minted,
          ownSymbol
        )
    }

    @Ignore
    val compiledMintingPolicyScriptV2 = compile(
      MintingPolicy.mintingPolicyScript(simpleCtxV2Deserializer)
    )
}

object MintingPolicySimpleBackend {
    // Use SimpleSirToUplcLowering backend explicitly
    given Options = Options(
      targetLoweringBackend = TargetLoweringBackend.ScottEncodingLowering
    )

    val compiledOptimizedMintingPolicyScript = compile(
      MintingPolicy.mintingPolicyScript(MintingPolicy.optimizedCtxDeserializer)
    )
}
