package scalus.examples.oneshotmint

import scalus.uplc.builtin.Builtins.*
import scalus.uplc.builtin.ByteString.*
import scalus.uplc.builtin.Data.fromData
import scalus.uplc.builtin.{ByteString, Data}
import scalus.compiler.sir.TargetLoweringBackend
import scalus.compiler.{compile, Options}
import scalus.cardano.onchain.plutus.v1.*
import scalus.cardano.onchain.plutus.prelude.*
import scalus.cardano.onchain.plutus.prelude.Option.*
import scalus.compiler.{Compile, Ignore}

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
    ) = (redeemer: Unit, ctxData: Data) => mint(deserializer(ctxData), txId, txOutIdx, tokensToMint)

    /** The policy, on the minting context a deserializer reads. It is a one-shot policy: the
      * transaction that spends the output `txId#txOutIdx`, the seed, mints `tokensToMint`, and any
      * other transaction can only burn the policy's tokens. The ledger lets an output be spent
      * once, so the tokens are minted once.
      */
    inline def mint(
        context: MintingContext,
        txId: ByteString,
        txOutIdx: BigInt,
        tokensToMint: SortedMap[ByteString, BigInt]
    ): Unit = {
        // What holds of every transaction the policy accepts. These clauses are its
        // specification, not checks: they are no part of the script, and
        // `MintingPolicyVerificationTest` proves them about the compiled policy.

        // 1. Only the seed mints. A transaction that does not spend the seed output burns each of
        //    the policy's tokens that it names.
        Spec.ensures(spendsSeed(context, txId, txOutIdx) || burnsOnly(context))

        // 2. The transaction that spends the seed output mints exactly `tokensToMint`: each of its
        //    tokens in its quantity, and no other token of the policy.
        Spec.ensures(!spendsSeed(context, txId, txOutIdx) || mintsExactly(context, tokensToMint))

        context match
            case MintingContext(txOutRefs, minted, ownSymbol) =>
                val mintedTokens = minted.toSortedMap.get(ownSymbol) match
                    case Some(mintedTokens) => mintedTokens
                    case None =>
                        throw new Exception("T")

                val checkSpendsTxOut = txOutRefs.find { case TxOutRef(txOutRefTxId, txOutRefIdx) =>
                    txOutRefTxId.hash == txId && txOutRefIdx == txOutIdx
                }

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

    inline def check(b: Boolean, inline msg: String): Unit =
        if b then () else throw new Exception(msg)

    // What the `Spec` clauses of `mint` speak of. Only the clauses use these functions, and they
    // are removed from the script with them.

    /** Whether the transaction spends the seed output `txId#txOutIdx`. */
    def spendsSeed(context: MintingContext, txId: ByteString, txOutIdx: BigInt): Boolean =
        context.inputs.exists(ref => ref.id.hash == txId && ref.idx == txOutIdx)

    /** The quantities of the policy's own tokens in the transaction's mint. */
    def ownTokens(context: MintingContext): SortedMap[ByteString, BigInt] =
        context.minted.toSortedMap.get(context.ownSymbol).getOrFail(NoOwnTokens)

    /** Whether the transaction burns each of the policy's tokens that it names: every quantity is
      * negative.
      */
    def burnsOnly(context: MintingContext): Boolean =
        ownTokens(context).toList.forall { case (_, quantity) => quantity < BigInt(0) }

    /** Whether the transaction mints `tokens` and nothing else of the policy: each token has the
      * same quantity in the mint as in `tokens`, where a token that is not named has none.
      */
    def mintsExactly(context: MintingContext, tokens: SortedMap[ByteString, BigInt]): Boolean = {
        val minted = ownTokens(context)
        tokens.toList.forall { case (name, quantity) => quantityOf(minted, name) == quantity } &&
        minted.toList.forall { case (name, quantity) => quantity == quantityOf(tokens, name) }
    }

    /** The quantity of the token `name` in `tokens`, zero where it is not named. */
    def quantityOf(tokens: SortedMap[ByteString, BigInt], name: ByteString): BigInt =
        tokens.get(name).getOrElse(BigInt(0))

    inline val NoOwnTokens = "The mint names no token of the policy"

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
    // Use the ScottEncodingLowering backend explicitly
    given Options = Options(
      targetLoweringBackend = TargetLoweringBackend.ScottEncodingLowering
    )

    val compiledOptimizedMintingPolicyScript = compile(
      MintingPolicy.mintingPolicyScript(MintingPolicy.optimizedCtxDeserializer)
    )
}
