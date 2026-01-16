package scalus.cardano.txbuilder

import io.bullet.borer.Cbor
import monocle.syntax.all.*
import monocle.{Focus, Lens}
import org.scalacheck.Arbitrary.arbitrary
import org.scalacheck.{Arbitrary, Gen}
import org.scalatest.funsuite.AnyFunSuite
import org.scalatestplus.scalacheck.ScalaCheckPropertyChecks
import scalus.builtin.Data.toData
import scalus.builtin.{ByteString, Data}
import scalus.cardano.address.Network.{Mainnet, Testnet}
import scalus.cardano.address.ShelleyDelegationPart.{Key, Null}
import scalus.cardano.address.{Network, ShelleyAddress, ShelleyPaymentPart}
import scalus.cardano.ledger.ArbitraryInstances.given
import scalus.cardano.ledger.Certificate.UnregCert
import scalus.cardano.ledger.DatumOption.Inline
import scalus.cardano.ledger.Timelock.AllOf
import scalus.cardano.ledger.TransactionOutput.Babbage
import scalus.cardano.ledger.TransactionWitnessSet.given
import scalus.cardano.ledger.{Mint as TxBodyMint, *}
import scalus.cardano.txbuilder.*
import scalus.cardano.txbuilder.Datum.DatumInlined
import scalus.cardano.txbuilder.RedeemerPurpose.{ForCert, ForMint}
import scalus.cardano.txbuilder.ScriptSource.*
import scalus.cardano.txbuilder.SomeBuildError.SomeStepError
import scalus.cardano.txbuilder.StepError.*
import scalus.testing.kit.Party.Alice
import scalus.testing.kit.TestUtil.genAdaOnlyPubKeyUtxo
import scalus.cardano.txbuilder.TransactionBuilder.{build, Context, ResolvedUtxos}
import scalus.cardano.txbuilder.TransactionBuilderStep.{Mint, *}
import scalus.prelude.List as PList
import scalus.|>

import scala.collection.immutable.SortedMap

class TransactionBuilderTest extends AnyFunSuite, ScalaCheckPropertyChecks {

    given CardanoInfo = CardanoInfo.mainnet

    /** Test that the builder steps fail with the expected error
      *
      * @param label
      * @param steps
      * @param error
      */
    def testBuilderStepsFail(
        label: String,
        steps: Seq[TransactionBuilderStep],
        error: StepError
    ): Unit =
        test(label) {
            val res = TransactionBuilder.build(Mainnet, steps)
            res match {
                case Left(SomeBuildError.SomeStepError(e, context)) => assert(e == error)
                case other =>
                    fail(
                      s"Expected the transaction building to fail with $error, got $other instead"
                    )
            }
        }

    def testBuilderSteps(
        label: String,
        steps: Seq[TransactionBuilderStep],
        expected: ContextTuple
    ): Unit =
        test(label) {
            val res = TransactionBuilder.build(Mainnet, steps)
            assert(res.map(_.toTuple) == Right(expected))
        }

    val pkhUtxo = Utxo(input = input1, output = pkhOutput)
    val skhUtxo = Utxo(input1, skhOutput)

    val ns: Script.Native = Script.Native(AllOf(IndexedSeq.empty))
    val nsSigners: Set[ExpectedSigner] =
        Gen.listOf(arbitrary[AddrKeyHash]).sample.get.toSet.map(ExpectedSigner(_))

    val nsWitness = NativeScriptWitness(NativeScriptValue(ns), nsSigners)

    val script2Signers: Set[ExpectedSigner] =
        Gen.listOf(arbitrary[AddrKeyHash]).sample.get.toSet.map(ExpectedSigner(_))

    val plutusScript2Witness =
        ThreeArgumentPlutusScriptWitness(
          PlutusScriptValue(script2),
          Data.List(PList.Nil),
          DatumInlined,
          script2Signers
        )

    private def withScriptAddr(scriptHash: ScriptHash, utxo: Utxo): Utxo = {
        val newOutput = utxo.output match
            case o: Babbage =>
                o.copy(address =
                    ShelleyAddress(Mainnet, ShelleyPaymentPart.Script(scriptHash), Null)
                )
            case _ =>
                Babbage(
                  address = ShelleyAddress(Mainnet, ShelleyPaymentPart.Script(scriptHash), Null),
                  value = utxo.output.value,
                  datumOption = None,
                  scriptRef = None
                )
        Utxo(utxo.input, newOutput)
    }

    // A Utxo at the address for script 1
    val script1Utxo: Utxo = {
        val utxo = genAdaOnlyPubKeyUtxo(Alice).sample.get
        val withAddr = withScriptAddr(script1.scriptHash, utxo)
        val newOutput = withAddr.output match
            case o: Babbage => o.copy(datumOption = Some(Inline(Data.List(PList.Nil))))
            case _          => throw new IllegalStateException("Expected Babbage output")
        Utxo(withAddr.input, newOutput)
    }

    // A Utxo at the address for script 2
    val script2Utxo: Utxo = {
        val utxo = genAdaOnlyPubKeyUtxo(Alice).sample.get
        withScriptAddr(script2.scriptHash, utxo)
    }

    // Expected Signers for the plutus script1 ref witness
    val psRefWitnessExpectedSigners: Set[ExpectedSigner] =
        Gen.listOf(arbitrary[AddrKeyHash]).sample.get.toSet.map(ExpectedSigner(_))

    private def withRefScript(script: Script, utxo: Utxo): Utxo = {
        val newOutput = utxo.output match
            case o: Babbage => o.copy(scriptRef = Some(ScriptRef(script)))
            case _ =>
                Babbage(
                  address = utxo.output.address,
                  value = utxo.output.value,
                  datumOption = None,
                  scriptRef = Some(ScriptRef(script))
                )
        Utxo(utxo.input, newOutput)
    }

    // A utxo carrying a reference script for script 1
    val utxoWithScript1ReferenceScript: Utxo = {
        val utxo = genAdaOnlyPubKeyUtxo(Alice).sample.get
        withRefScript(script1, utxo)
    }

    // A utxo carrying a reference script for script 2
    val utxoWithScript2ReferenceStep: ReferenceOutput = {
        val utxo = genAdaOnlyPubKeyUtxo(Alice).sample.get
        ReferenceOutput(withRefScript(script2, utxo))
    }

    val plutusScript1RefWitness = ThreeArgumentPlutusScriptWitness(
      PlutusScriptAttached,
      Data.List(PList.Nil),
      DatumInlined,
      psRefWitnessExpectedSigners
    )

    val plutusScript1RefSpentWitness = ThreeArgumentPlutusScriptWitness(
      PlutusScriptAttached,
      Data.List(PList.Nil),
      DatumInlined,
      psRefWitnessExpectedSigners
    )

    val plutusScript2RefWitness = ThreeArgumentPlutusScriptWitness(
      PlutusScriptAttached,
      Data.List(PList.Nil),
      DatumInlined,
      psRefWitnessExpectedSigners
    )

    ///////////////////////////////////////////////////////////////
    // Group: "SpendOutput"
    ///////////////////////////////////////////////////////////////

    val spendPkhUtxoStep = TransactionBuilderStep.Spend(pkhUtxo, PubKeyWitness)
    val pubKeyInput1Expected: ContextTuple =
        Context.empty(Mainnet).toTuple
            |> transactionL
                .andThen(txBodyL.refocus(_.inputs))
                .replace(TaggedSortedSet(input1))
            |> expectedSignersL
                .modify(
                  _ + ExpectedSigner(
                    spendPkhUtxoStep.utxo.output.address.keyHashOption.get.asInstanceOf[AddrKeyHash]
                  )
                )
            |> resolvedUtxosL.modify((r: ResolvedUtxos) => ResolvedUtxos(r.utxos + pkhUtxo.toTuple))

    testBuilderSteps(
      label = "PKH Output",
      steps = List(Spend(pkhUtxo, PubKeyWitness)),
      expected = pubKeyInput1Expected
    )

    testBuilderStepsFail(
      label = "PKH output x2",
      steps = List(Spend(pkhUtxo, PubKeyWitness), Spend(pkhUtxo, PubKeyWitness)),
      error = InputAlreadyExists(pkhUtxo.input, Spend(pkhUtxo, PubKeyWitness))
    )

    testBuilderStepsFail(
      label = "PKH output with wrong witness #1",
      steps = List(Spend(pkhUtxo, nsWitness)),
      error = WrongOutputType(WitnessKind.ScriptBased, pkhUtxo, Spend(pkhUtxo, nsWitness))
    )

    testBuilderStepsFail(
      label = "PKH output with wrong witness #2",
      steps = List(Spend(pkhUtxo, plutusScript1RefSpentWitness)),
      error = WrongOutputType(
        WitnessKind.ScriptBased,
        pkhUtxo,
        Spend(pkhUtxo, plutusScript1RefSpentWitness)
      )
    )

    testBuilderStepsFail(
      label = "SKH output with wrong witness #1",
      steps = List(Spend(skhUtxo, nsWitness)),
      error = IncorrectScriptHash(ns, scriptHash1, Spend(skhUtxo, nsWitness))
    )

    testBuilderStepsFail(
      label = "SKH output with wrong witness #2",
      steps = List(Spend(skhUtxo, plutusScript2Witness)),
      error = IncorrectScriptHash(script2, scriptHash1, Spend(skhUtxo, plutusScript2Witness))
    )

    // ============================================================================
    // Try to spend output with wrong network in address
    // ============================================================================

    val pkhUtxoTestNet =
        Utxo(
          input = input0,
          output = Babbage(
            address = ShelleyAddress(
              network = Testnet,
              payment = pkhOutputPaymentPart,
              delegation = Null
            ),
            value = Value.zero,
            datumOption = None,
            scriptRef = None
          )
        )

    testBuilderStepsFail(
      label = "Try to spend output with wrong network in address",
      steps = List(Spend(pkhUtxo, PubKeyWitness), Spend(pkhUtxoTestNet, PubKeyWitness)),
      error = WrongNetworkId(pkhUtxoTestNet.output.address, Spend(pkhUtxoTestNet, PubKeyWitness))
    )

    test("Spend with delayed redeemer sees transaction inputs") {
        val redeemerBuilder: Transaction => Data = { tx =>
            val inputCount = tx.body.value.inputs.toSeq.size
            Data.Constr(0, PList(Data.I(inputCount)))
        }

        val delayedSpend = Spend(
          utxo = script1Utxo,
          witness = ThreeArgumentPlutusScriptWitness(
            scriptSource = PlutusScriptValue(script1),
            redeemerBuilder = redeemerBuilder,
            datum = DatumInlined,
            additionalSigners = Set.empty
          )
        )

        val steps = Seq(
          Spend(pkhUtxo, PubKeyWitness),
          delayedSpend,
          Send(
            Babbage(
              address = pkhOutput.address,
              value = Value(Coin(1_000_000L)),
              datumOption = None,
              scriptRef = None
            )
          )
        )

        val result = TransactionBuilder.build(Mainnet, steps)
        val context = result.toOption.get
        val scriptRedeemer = context.redeemers.collectFirst {
            case DetachedRedeemer(datum, RedeemerPurpose.ForSpend(input))
                if input == script1Utxo.input =>
                datum
        }
        scriptRedeemer match {
            case Some(Data.Constr(0, PList.Cons(Data.I(inCount), PList.Nil))) =>
                assert(inCount == 2)
            case other => fail("unreachable")
        }
    }

    // ============================================================================
    // Spending with ref script from referenced utxo
    // ============================================================================

    testBuilderSteps(
      label = "Spending with ref script from referenced utxo",
      steps = List(
        ReferenceOutput(utxo = utxoWithScript1ReferenceScript),
        Spend(utxo = script1Utxo, witness = plutusScript1RefWitness)
      ),
      expected = Context.empty(Mainnet).toTuple
          |> (transactionL >>> txInputsL)
              .replace(TaggedSortedSet(script1Utxo.input))
          |> (transactionL >>> txReferenceInputsL)
              .replace(TaggedSortedSet(utxoWithScript1ReferenceScript.input))
          |> (transactionL >>> txRequiredSignersL)
              .replace(TaggedSortedSet.from(psRefWitnessExpectedSigners.map(_.hash)))
          |> (transactionL >>> txRedeemersL)
              .replace(redeemers(unitRedeemer(RedeemerTag.Spend, 0)))
          |> expectedSignersL.replace(psRefWitnessExpectedSigners)
          |> resolvedUtxosL
              .replace(
                fromRight(
                  ResolvedUtxos.empty.addUtxos(Seq(script1Utxo, utxoWithScript1ReferenceScript))
                )
              )
          |> ctxRedeemersL
              .replace(
                List(unitDRedeemer(RedeemerPurpose.ForSpend(script1Utxo.input)))
              )
    )

    // ============================================================================
    // Spending with ref script from consumed utxo
    // ============================================================================

    testBuilderSteps(
      label = "Spending with ref script from consumed utxo",
      steps = List(
        Spend(utxo = utxoWithScript1ReferenceScript),
        Spend(utxo = script1Utxo, witness = plutusScript1RefSpentWitness)
      ),
      expected = {
          val ctx1 = Context.empty(Mainnet).toTuple
              |> (transactionL >>> txInputsL)
                  // We spend two inputs: the script1Utxo (at the script address), and the UTxO carrying the reference
                  // script at the Pubkey Address
                  .replace(
                    TaggedSortedSet(utxoWithScript1ReferenceScript.input, script1Utxo.input)
                  )
              |> (transactionL >>> txRequiredSignersL)
                  // We add the required signers for script1
                  .replace(
                    TaggedSortedSet.from(psRefWitnessExpectedSigners.map(_.hash))
                  )
              |> expectedSignersL
                  // Add the expected signers for the script and the expected signer for spending the utxo with the script
                  .replace(
                    psRefWitnessExpectedSigners + ExpectedSigner(
                      utxoWithScript1ReferenceScript.output.address.keyHashOption.get
                          .asInstanceOf[AddrKeyHash]
                    )
                  )
              |> resolvedUtxosL
                  .replace(
                    fromRight(
                      ResolvedUtxos.empty.addUtxos(Seq(script1Utxo, utxoWithScript1ReferenceScript))
                    )
                  )
              |> ctxRedeemersL.replace(
                List(unitDRedeemer(RedeemerPurpose.ForSpend(script1Utxo.input)))
              )

          // Now we need to determine which order the inputs were in. Because the pubkey hash is randomly
          // generated, and because the inputs are in sorted order, it may come before or after the
          // script input.
          val redeemerIndex: Int = ctx1 |> (transactionL >>> txInputsL).get |>
              ((inputs: TaggedSortedSet[TransactionInput]) =>
                  inputs.toSeq.indexOf(script1Utxo.input)
              )

          ctx1 |> (transactionL >>> txRedeemersL)
              .replace(redeemers(unitRedeemer(RedeemerTag.Spend, redeemerIndex)))
      }
    )

    // Script1Utxo is utxo at the script1 address, while the witness passed
    // denotes a utxo carrying script2 in its scriptRef.
    testBuilderStepsFail(
      label = "Script Output with mismatched script ref in spent utxo",
      steps = List(Spend(utxo = script1Utxo, witness = plutusScript2RefWitness)),
      error =
          AttachedScriptNotFound(script1.scriptHash, Spend(script1Utxo, plutusScript2RefWitness))
    )

    // ================================================================
    // Subgroup: Signature tracking
    // ================================================================

    test("SpendOutput.additionalSignersUnsafe works for pubkey") {
        // Check that the transaction step adds the correct signer
        val tx = build(Mainnet, List(spendPkhUtxoStep))
        assert(
          tx.map(_.expectedSigners) ==
              Right(
                Set(
                  ExpectedSigner(
                    spendPkhUtxoStep.utxo.output.address.keyHashOption.get
                        .asInstanceOf[AddrKeyHash]
                  )
                )
              )
        )
    }

    test("Signers works for NS spend") {
        val txInput = arbitrary[TransactionInput].sample.get

        val step =
            TransactionBuilderStep.Spend(
              utxo = Utxo(
                txInput,
                Babbage(
                  address = ShelleyAddress(Mainnet, ShelleyPaymentPart.Script(ns.scriptHash), Null),
                  value = Value.zero,
                  datumOption = None,
                  scriptRef = None
                )
              ),
              witness = nsWitness
            )

        // Signers are what we expect for a transaction built with this step
        assert(
          build(Mainnet, List(step)).map(_.expectedSigners) ==
              Right(step.witness.asInstanceOf[NativeScriptWitness].additionalSigners)
        )
    }

    test("Signers work for PS spend") {
        val txInput = arbitrary[TransactionInput].sample.get

        val step =
            TransactionBuilderStep.Spend(
              utxo = Utxo(
                txInput,
                Babbage(
                  address =
                      ShelleyAddress(Mainnet, ShelleyPaymentPart.Script(script2.scriptHash), Null),
                  value = Value.zero,
                  datumOption = Some(Inline(Data.List(PList.Nil))),
                  scriptRef = None
                )
              ),
              witness = plutusScript2Witness
            )

        val built = fromRight(build(Mainnet, List(step)))

        // Signers are what we expect for a transaction built with this step
        assert(
          built.expectedSigners ==
              step.witness.asInstanceOf[ThreeArgumentPlutusScriptWitness].additionalSigners
        )

        // signers are added to the `requiredSigners` field in tx body
        val obtained =
            built.toTuple |> transactionL.andThen(txBodyL).refocus(_.requiredSigners).get |> (s =>
                s.toSet.toSet
            )

        val expected = step.witness
            .asInstanceOf[ThreeArgumentPlutusScriptWitness]
            .additionalSigners
            .map(_.hash)

        assert(obtained == expected)
    }

    // =======================================================================
    // Group: "Pay"
    // =======================================================================

    testBuilderSteps(
      label = "Pay #1",
      steps = List(Send(pkhOutput)),
      expected = Context.empty(Mainnet).toTuple
          |> transactionL
              .andThen(txBodyL.refocus(_.outputs))
              .replace(IndexedSeq(Sized(pkhOutput)))
    )

    // =======================================================================
    // Group: "MintAsset"
    // =======================================================================

    //     testBuilderSteps "#1" [ Pay pkhOutput ] $
    //      anyNetworkTx # _body <<< _outputs .~ [ pkhOutput ]
    // NOTE (dragospe, 2025-09-24): upstream, this test is the same as pay #1. I've modified it.
    testBuilderSteps(
      label = "MintAsset #1",
      steps = List(
        TransactionBuilderStep.Mint(
          scriptHash = scriptHash1,
          assetName = AssetName(ByteString.fromHex("deadbeef")),
          amount = 1L,
          witness = TwoArgumentPlutusScriptWitness(
            PlutusScriptValue(script1),
            redeemer = Data.List(PList.Nil),
            additionalSigners = Set.empty
          )
        )
      ),
      expected = Context.empty(Mainnet).toTuple |>
          // replace mint
          transactionL
              .andThen(txBodyL)
              .refocus(_.mint)
              .replace(
                Some(
                  TxBodyMint(
                    MultiAsset(
                      SortedMap.from(
                        List(
                          scriptHash1 -> SortedMap.from(
                            List(AssetName(ByteString.fromHex("deadbeef")) -> 1L)
                          )
                        )
                      )
                    )
                  )
                )
              )
          |>
          // add script witness
          transactionL
              .refocus(_.witnessSet.plutusV1Scripts)
              .replace(TaggedSortedStrictMap(script1))
          |>
          // add redeemer
          transactionL
              .refocus(_.witnessSet.redeemers)
              .replace(redeemers(unitRedeemer(RedeemerTag.Mint, 0)))
          |>
          ctxRedeemersL.replace(
            List(
              unitDRedeemer(
                ForMint(
                  ScriptHash.fromHex("36137e3d612d23a644283f10585958085aa255bdae4076fcefe414b6")
                )
              )
            )
          )
    )

    val mintSigners = Set(ExpectedSigner(Arbitrary.arbitrary[AddrKeyHash].sample.get))

    // Mint the given amount of tokens from script 1
    def mintScript1(amount: Long, redeemer: Data = Data.List(PList.Nil)): Mint =
        Mint(
          scriptHash = scriptHash1,
          assetName = AssetName.empty,
          amount = amount,
          witness = TwoArgumentPlutusScriptWitness(
            scriptSource = PlutusScriptValue(script1),
            redeemer = redeemer,
            additionalSigners = mintSigners
          )
        )

    {
        val mint0Step = mintScript1(0)
        testBuilderStepsFail(
          label = "Mint 0 directly",
          steps = List(mint0Step),
          error = CannotMintZero(scriptHash1, AssetName.empty, mint0Step)
        )
    }

    testBuilderSteps(
      label = "Mint 0 via reciprocal mint/burn",
      steps = List(mintScript1(5), mintScript1(-5)),
      expected =
          // NOTE: In the case of reciprocal mint/burns, we don't strip script witnesses or signatures because
          // we don't currently track the purposes associated with these objects.
          Context.empty(Mainnet).toTuple
              |> transactionL
                  .refocus(_.witnessSet.plutusV1Scripts)
                  .modify(s => TaggedSortedStrictMap.from(s.toSet + script1))
              |> (transactionL >>> txBodyL
                  .refocus(_.requiredSigners))
                  .replace(TaggedSortedSet.from(mintSigners.map(_.hash)))
              |> expectedSignersL.replace(mintSigners)
    )

    testBuilderSteps(
      label = "Mint 0 via reciprocal mint/burn with different redeemers",
      steps = List(mintScript1(5), mintScript1(-5, Data.List(PList(Data.List(PList.Nil))))),
      expected =
          // NOTE: In the case of reciprocal mint/burns, we don't strip script witnesses or signatures because
          // we don't currently track the purposes associated with these objects.
          Context.empty(Mainnet).toTuple
              |> transactionL
                  .refocus(_.witnessSet.plutusV1Scripts)
                  .modify(s => TaggedSortedStrictMap.from(s.toSet + script1))
              |> (transactionL >>> txBodyL
                  .refocus(_.requiredSigners))
                  .replace(TaggedSortedSet.from(mintSigners.map(_.hash)))
              |> expectedSignersL.replace(mintSigners)
    )

    testBuilderSteps(
      label = "Monoidal mint with same policy id but different redeemers",
      steps = List(mintScript1(1), mintScript1(1, Data.List(PList(Data.List(PList.Nil))))),
      expected = Context.empty(Mainnet).toTuple
          |> (transactionL >>> txBodyL.refocus(_.mint))
              .replace(Some(TxBodyMint(MultiAsset.from((scriptHash1, AssetName.empty, 2L)))))
          |> transactionL
              .refocus(_.witnessSet.plutusV1Scripts)
              .modify(s => TaggedSortedStrictMap.from(s.toSet + script1))
          |> (transactionL >>> txBodyL
              .refocus(_.requiredSigners))
              .replace(TaggedSortedSet.from(mintSigners.map(_.hash)))
          |> expectedSignersL.replace(mintSigners)
          |> transactionL
              .refocus(_.witnessSet.redeemers)
              .replace(
                Some(
                  KeepRaw(
                    Redeemers(
                      Redeemer(
                        tag = RedeemerTag.Mint,
                        index = 0,
                        data = Data.List(PList(Data.List(PList.Nil))),
                        exUnits = ExUnits.zero
                      )
                    )
                  )
                )
              )
          |> ctxRedeemersL.replace(
            List(
              DetachedRedeemer(
                datum = Data.List(PList(Data.List(PList.Nil))),
                purpose = ForMint(scriptHash1)
              )
            )
          )
    )

    testBuilderSteps(
      label = "Mint/burn monoid",
      steps = List(mintScript1(1), mintScript1(1), mintScript1(-5)),
      expected = Context.empty(Mainnet).toTuple
          |> (transactionL >>> txBodyL.refocus(_.mint))
              .replace(Some(TxBodyMint(MultiAsset.from((scriptHash1, AssetName.empty, -3L)))))
          |> transactionL
              .refocus(_.witnessSet.plutusV1Scripts)
              .modify(s => TaggedSortedStrictMap.from(s.toSet + script1))
          |> (transactionL >>> txBodyL
              .refocus(_.requiredSigners))
              .replace(TaggedSortedSet.from(mintSigners.map(_.hash)))
          |> expectedSignersL.replace(mintSigners)
          |> transactionL
              .refocus(_.witnessSet.redeemers)
              .replace(
                Some(
                  KeepRaw(
                    Redeemers(
                      Redeemer(
                        tag = RedeemerTag.Mint,
                        index = 0,
                        data = Data.List(PList.Nil),
                        exUnits = ExUnits.zero
                      )
                    )
                  )
                )
              )
          |> ctxRedeemersL.replace(
            List(DetachedRedeemer(datum = Data.List(PList.Nil), purpose = ForMint(scriptHash1)))
          )
    )

    test("Mint with delayed redeemer sees transaction inputs") {
        val redeemerBuilder: Transaction => Data = { tx =>
            tx.body.value.inputs.toSeq.size.toData
        }

        val steps = Seq(
          Spend(pkhUtxo, PubKeyWitness),
          Mint(
            scriptHash = scriptHash1,
            assetName = AssetName.fromHex("deadbeef"),
            amount = 1,
            witness = TwoArgumentPlutusScriptWitness(
              PlutusScriptValue(script1),
              redeemerBuilder = redeemerBuilder,
              additionalSigners = Set.empty
            )
          ),
          Send(TransactionOutput(pkhOutput.address, Value.ada(1)))
        )

        val result = TransactionBuilder.build(Mainnet, steps)
        val context = result.toOption.get
        val mintRedeemer = context.redeemers.collectFirst {
            case DetachedRedeemer(datum, RedeemerPurpose.ForMint(hash)) if hash == scriptHash1 =>
                datum
        }
        assert(mintRedeemer.contains(1.toData)) // 1 input from pkhUtxo
    }

    test("Mint with static redeemer (backwards compatible)") {
        val staticRedeemer = Data.List(PList.Nil)

        val steps = Seq(
          Mint(
            scriptHash = scriptHash1,
            assetName = AssetName.fromHex("deadbeef"),
            amount = 1,
            witness = TwoArgumentPlutusScriptWitness(
              PlutusScriptValue(script1),
              redeemer = staticRedeemer, // Uses backwards compatible apply
              additionalSigners = Set.empty
            )
          )
        )

        val result = TransactionBuilder.build(Mainnet, steps)
        assert(result.isRight)
    }

    // ================================================================
    // Subgroup: reference utxos
    // ================================================================

    // // TODO: write the same test for a ref native script. Since we don't use then Hydrozoa I decided to skip it
    // test("Referencing a script utxo with Plutus script adds the utxo to resolvedUtxos") {
    //     val steps = List(SpendOutput(utxo = script1Utxo, witness = Some(plutusScript1RefWitness)))
    //     val built = fromRight(TransactionBuilder.build(Mainnet, steps))
    //     assertEquals(
    //       obtained = built.toTuple |> resolvedUtxosL.get,
    //       Set(script1Utxo, utxoWithScript1ReferenceScript)
    //     )
    // }

    test("Referencing a utxo adds the utxo to resolvedUtxos and tx body") {
        val steps = List(ReferenceOutput(utxo = script1Utxo))
        val built = fromRight(TransactionBuilder.build(Mainnet, steps))
        assert(
          (built.toTuple |> resolvedUtxosL.get) == ResolvedUtxos(Map(script1Utxo.toTuple))
        )

        assert(
          (built.toTuple |> transactionL.andThen(txBodyL).refocus(_.referenceInputs).get) ==
              TaggedSortedSet.from(List(script1Utxo.input))
        )
    }

    // ================================================================
    // Subgroup: collateral inputs
    // ================================================================

    test("Adding a utxo as collateral adds the utxo to resolvedUtxos and tx body") {
        val steps = List(AddCollateral(utxo = pkhUtxo))
        val built = fromRight(TransactionBuilder.build(Mainnet, steps))
        assert(
          (built.toTuple |> resolvedUtxosL.get) == ResolvedUtxos(Map(pkhUtxo.toTuple))
        )

        assert(
          (built.toTuple |> transactionL.andThen(txBodyL).refocus(_.collateralInputs).get) ==
              TaggedSortedSet.from(List(pkhUtxo.input))
        )
    }

    testBuilderStepsFail(
      label = "A script based utxo can't be used as a collateral",
      steps = List(AddCollateral(utxo = script1Utxo)),
      error = CollateralNotPubKey(script1Utxo, AddCollateral(script1Utxo))
    )

    // Test that collateral with tokens is now allowed (per Babbage spec CIP-40)
    test("Adding a utxo with tokens as collateral succeeds") {
        // Create a pubkey UTxO with tokens
        val pkhUtxoWithTokens = Utxo(
          input = input1,
          output = pkhOutput.copy(
            value = Value(
              Coin(5_000_000L),
              MultiAsset.asset(script1.scriptHash, AssetName.fromString("TestToken"), 100L)
            )
          )
        )
        val steps = List(AddCollateral(utxo = pkhUtxoWithTokens))
        val built = fromRight(TransactionBuilder.build(Mainnet, steps))

        // Verify the UTXO was added to resolvedUtxos
        assert(built.resolvedUtxos.utxos.contains(pkhUtxoWithTokens.input))

        // Verify the collateral input was added to the transaction body
        assert(
          built.transaction.body.value.collateralInputs.toSet.contains(pkhUtxoWithTokens.input)
        )
    }

    // Test that SetCollateralReturn step sets the return address in context
    test("SetCollateralReturn step sets the return address in context") {
        val returnAddress = pkhOutput.address
        val steps = List(
          AddCollateral(utxo = pkhUtxo),
          SetCollateralReturn(returnAddress = returnAddress)
        )
        val built = fromRight(TransactionBuilder.build(Mainnet, steps))

        // Verify the collateral return address was set
        assert(built.collateralReturnAddress == Some(returnAddress))
    }

    // Test that SetCollateralReturn with wrong network fails
    testBuilderStepsFail(
      label = "SetCollateralReturn with wrong network fails",
      steps = List(
        AddCollateral(utxo = pkhUtxo),
        SetCollateralReturn(
          returnAddress = ShelleyAddress(
            network = Testnet, // Wrong network
            payment = pkhOutputPaymentPart,
            delegation = Null
          )
        )
      ),
      error = WrongNetworkId(
        ShelleyAddress(
          network = Testnet,
          payment = pkhOutputPaymentPart,
          delegation = Null
        ),
        SetCollateralReturn(
          ShelleyAddress(
            network = Testnet,
            payment = pkhOutputPaymentPart,
            delegation = Null
          )
        )
      )
    )

    // Test that collateral with tokens but insufficient ADA returns error
    test("ensureCollateralReturn fails when collateral has tokens but insufficient ADA") {
        // Create a UTxO with tokens but minimal ADA (just 1 ADA which is not enough for collateral + return)
        val pkhUtxoWithTokensLowAda = Utxo(
          input = input1,
          output = pkhOutput.copy(
            value = Value(
              Coin(1_000_000L), // 1 ADA - not enough for both collateral and minAda return
              MultiAsset.asset(script1.scriptHash, AssetName.fromString("TestToken"), 100L)
            )
          )
        )

        // Build a context with this collateral
        val steps = List(AddCollateral(utxo = pkhUtxoWithTokensLowAda))
        val ctx = fromRight(TransactionBuilder.build(Mainnet, steps))

        // Set a fee that requires significant collateral
        val txWithFee = TransactionBuilder.modifyBody(
          ctx.transaction,
          _.copy(fee = Coin(500_000L)) // 0.5 ADA fee -> requires ~0.75 ADA collateral (150%)
        )

        // Call ensureCollateralReturn - should fail because:
        // - Collateral has tokens (must return them)
        // - But 1 ADA - 0.75 ADA = 0.25 ADA left for return, which is less than minAda (~1 ADA)
        val result = TransactionBuilder.ensureCollateralReturn(
          txWithFee,
          ctx.resolvedUtxos.utxos,
          None,
          CardanoInfo.mainnet.protocolParams
        )

        result match {
            case Left(
                  TxBalancingError.InsufficientCollateralForReturn(totalAda, required, minAda)
                ) =>
                // Verify error details make sense
                assert(totalAda.value == 1_000_000L, s"Expected 1 ADA total, got ${totalAda.value}")
                assert(required.value > 0, "Required collateral should be positive")
                assert(minAda.value > 0, "MinAda for return should be positive")
                assert(
                  totalAda.value < required.value + minAda.value,
                  s"Total ADA (${totalAda.value}) should be < required (${required.value}) + minAda (${minAda.value})"
                )
            case Left(other) =>
                fail(s"Expected InsufficientCollateralForReturn, got $other")
            case Right(_) =>
                fail("Expected error but got success")
        }
    }

    // =======================================================================
    // Group: "Deregister"
    // =======================================================================

    testBuilderSteps(
      label = "Deregister script",
      steps = List(
        IssueCertificate(
          cert = Certificate.UnregCert(Credential.ScriptHash(script1.scriptHash), coin = None),
          witness = TwoArgumentPlutusScriptWitness(
            PlutusScriptValue(script1),
            Data.List(PList.Nil),
            Set.empty
          )
        )
      ),
      expected = Context.empty(Mainnet).toTuple |>
          transactionL
              .refocus(_.witnessSet.plutusV1Scripts)
              .replace(TaggedSortedStrictMap(script1)) |>
          transactionL
              .refocus(_.witnessSet.redeemers)
              .replace(redeemers(unitRedeemer(RedeemerTag.Cert, 0)))
          |>
          transactionL
              .andThen(txBodyL)
              .refocus(_.certificates)
              .replace(
                TaggedOrderedStrictSet(
                  Certificate.UnregCert(Credential.ScriptHash(script1.scriptHash), coin = None)
                )
              )
          |>
          ctxRedeemersL.replace(
            List(
              unitDRedeemer(
                ForCert(
                  UnregCert(
                    Credential.ScriptHash(
                      ScriptHash.fromHex("36137e3d612d23a644283f10585958085aa255bdae4076fcefe414b6")
                    ),
                    None
                  )
                )
              )
            )
          )
    )

    val witness =
        TwoArgumentPlutusScriptWitness(PlutusScriptValue(script1), Data.List(PList.Nil), Set.empty)

    testBuilderStepsFail(
      label = "Deregistering stake credential with unneeded witness fails",
      steps = List(IssueCertificate(UnregCert(pubKeyHashCredential1, coin = None), witness)),
      error = UnneededDeregisterWitness(
        pubKeyHashCredential1,
        witness,
        IssueCertificate(UnregCert(pubKeyHashCredential1, coin = None), witness)
      )
    )

    {
        val wrongWitnessStep = IssueCertificate(
          cert = UnregCert(Credential.ScriptHash(script2.scriptHash), coin = None),
          witness = TwoArgumentPlutusScriptWitness(
            PlutusScriptValue(script1),
            Data.List(PList.Nil),
            Set.empty
          )
        )
        testBuilderStepsFail(
          label = "deregistering stake credential with wrong witness fails",
          steps = List(wrongWitnessStep),
          error = IncorrectScriptHash(script1, script2.scriptHash, wrongWitnessStep)
        )
    }

    // =======================================================================
    // Group: "Modify Aux Data"
    // =======================================================================
    testBuilderSteps(
      label = "ModifyAuxData: id",
      steps = List(ModifyAuxiliaryData(identity)),
      expected = Context.empty(Mainnet).toTuple
    )

}

// ===========================================================================
// Test Helpers
// ===========================================================================

def redeemers(rs: Redeemer*) = Some(KeepRaw(Redeemers(rs*)))

def unitRedeemer(tag: RedeemerTag, index: Int) = Redeemer(
  tag = tag,
  index = index,
  data = Data.List(PList.Nil),
  exUnits = ExUnits.zero
)

def unitDRedeemer(purpose: RedeemerPurpose) = DetachedRedeemer(
  datum = Data.List(PList.Nil),
  purpose = purpose
)

def transactionL: Lens[ContextTuple, Transaction] = Focus[ContextTuple](_._1)
def ctxRedeemersL: Lens[ContextTuple, Seq[DetachedRedeemer]] = Focus[ContextTuple](_._2)
def networkL: Lens[ContextTuple, Network] = Focus[ContextTuple](_._3)
def expectedSignersL: Lens[ContextTuple, Set[ExpectedSigner]] = Focus[ContextTuple](_._4)
def resolvedUtxosL: Lens[ContextTuple, ResolvedUtxos] = Focus[ContextTuple](_._5)

// ===========================================================================
// Common Test Data
// ===========================================================================

val unitRedeemer: Redeemer =
    Redeemer(
      tag = RedeemerTag.Spend,
      index = 0,
      data = ByteString.fromHex("").toData,
      exUnits = ExUnits.zero
    )

val script1: Script.PlutusV1 = {
    val bytes = ByteString.fromHex("4d01000033222220051200120011").bytes
    Cbor.decode(bytes).to[Script.PlutusV1].value
}

val scriptHash1: ScriptHash = script1.scriptHash

val scriptHashCredential1: Credential = Credential.ScriptHash(scriptHash1)

val skhOutput: TransactionOutput.Babbage = Babbage(
  address = ShelleyAddress(
    network = Mainnet,
    payment = ShelleyPaymentPart.Script(scriptHashCredential1.scriptHashOption.get),
    delegation = null
  ),
  value = Value(Coin(5_000_000L)),
  datumOption = None,
  scriptRef = None
)

val pubKeyHashCredential1: Credential = {
    val bytes: Array[Byte] = Array(57, 3, 16, 58, 231, 6, 129, 67, 155, 84, 118, 254, 245, 159, 67,
      155, 139, 200, 109, 132, 191, 178, 211, 118, 252, 63, 86, 23).map(_.toByte)
    Credential.KeyHash(Hash(ByteString.fromArray(bytes)))
}

val pkhOutputPaymentPart: ShelleyPaymentPart = {
    val bytes: Array[Byte] = Array(243, 63, 250, 132, 253, 242, 10, 0, 52, 67, 165, 226, 118, 142,
      18, 233, 45, 179, 21, 53, 220, 166, 32, 136, 177, 83, 223, 36).map(_.toByte)
    ShelleyPaymentPart.Key(Hash(ByteString.fromArray(bytes)))
}

val pkhOutput: Babbage = Babbage(
  address = ShelleyAddress(
    network = Mainnet,
    payment = pkhOutputPaymentPart,
    delegation = Key(pubKeyHashCredential1.keyHashOption.get.asInstanceOf[StakeKeyHash])
  ),
  value = Value(Coin(5_000_000L)),
  datumOption = None,
  scriptRef = None
)

def mkInput(txId: String, ix: Int): TransactionInput = {
    val txIdBytes: Array[Byte] = ByteString.fromHex(txId).bytes
    Input(
      transactionId = Hash(ByteString.fromArray(txIdBytes)),
      index = ix
    )
}

val input0: TransactionInput =
    mkInput("5d677265fa5bb21ce6d8c7502aca70b9316d10e958611f3c6b758f65ad959996", 0)

val input1: TransactionInput =
    mkInput("5d677265fa5bb21ce6d8c7502aca70b9316d10e958611f3c6b758f65ad959996", 1)

val input2: TransactionInput =
    mkInput("5d677265fa5bb21ce6d8c7502aca70b9316d10e958611f3c6b758f65ad959996", 2)

val script2: Script.PlutusV1 =
    Cbor.decode(ByteString.fromHex("4e4d01000033222220051200120012").bytes)
        .to[Script.PlutusV1]
        .value

val anyNetworkTx: Transaction = Transaction.empty

// See: https://github.com/mlabs-haskell/purescript-cardano-types/blob/348fbbefa8bec5050e8492f5a9201ac5bb17c9d9/test/CSLHex.purs#L109
val testnetTransaction: Transaction =
    txBodyL.refocus(_.networkId).replace(Some(0))(anyNetworkTx)

val testnetContext: ContextTuple =
    Context.empty(Testnet).toTuple |> transactionL.replace(testnetTransaction)

private def fromRight[A, B](e: Either[A, B]): B =
    e match {
        case Right(x)  => x
        case Left(err) => throw new IllegalArgumentException(s"Expected Right but got Left($err)")
    }

// The fields of a Context, to cut down on noise
// Note: delayedRedeemerSpecs is excluded since it contains lambdas that can't be compared
private type ContextTuple = (
    Transaction,
    Seq[DetachedRedeemer],
    Network,
    Set[ExpectedSigner],
    ResolvedUtxos
)
