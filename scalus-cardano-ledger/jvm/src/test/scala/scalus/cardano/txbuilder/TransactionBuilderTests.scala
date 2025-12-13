package scalus.cardano.txbuilder

import io.bullet.borer.Cbor
import monocle.syntax.all.*
import monocle.{Focus, Lens}
import org.scalacheck.Arbitrary.arbitrary
import org.scalacheck.Gen
import org.scalatest.funsuite.AnyFunSuite
import org.scalatestplus.scalacheck.ScalaCheckPropertyChecks
import scalus.builtin.Data.toData
import scalus.builtin.{ByteString, Data}
import scalus.prelude.List as PList
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
import scalus.cardano.txbuilder.TestPeer.Alice
import scalus.cardano.txbuilder.TransactionBuilder.{build, Context, ResolvedUtxos, WitnessKind}
import scalus.cardano.txbuilder.TransactionBuilderStep.{Mint, *}
import scalus.|>

import scala.collection.immutable.SortedMap

class TransactionBuilderTest extends AnyFunSuite, ScalaCheckPropertyChecks {

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

    private def setScriptAddr(
        scriptHash: ScriptHash,
        utxo: (TransactionInput, Babbage)
    ): (TransactionInput, Babbage) =
        utxo.focus(_._2.address)
            .replace(
              ShelleyAddress(
                network = Mainnet,
                payment = ShelleyPaymentPart.Script(scriptHash),
                delegation = Null
              )
            )

    // A Utxo at the address for script 1
    val script1Utxo: Utxo = {
        val utxo = genAdaOnlyPubKeyUtxo(Alice).sample.get
        Utxo(
          setScriptAddr(script1.scriptHash, utxo)
              .focus(_._2.datumOption)
              .replace(Some(Inline(Data.List(PList.Nil))))
        )
    }

    // A Utxo at the address for script 2
    val script2Utxo: Utxo = {
        val utxo = genAdaOnlyPubKeyUtxo(Alice).sample.get
        Utxo(setScriptAddr(script2.scriptHash, utxo))
    }

    // Expected Signers for the plutus script1 ref witness
    val psRefWitnessExpectedSigners: Set[ExpectedSigner] =
        Gen.listOf(arbitrary[AddrKeyHash]).sample.get.toSet.map(ExpectedSigner(_))

    private def setRefScript(
        script: Script,
        utxo: (TransactionInput, Babbage)
    ): (TransactionInput, Babbage) =
        utxo.focus(_._2.scriptRef).replace(Some(ScriptRef(script)))

    // A utxo carrying a reference script for script 1
    val utxoWithScript1ReferenceScript: Utxo = {
        val utxo = genAdaOnlyPubKeyUtxo(Alice).sample.get
        Utxo(setRefScript(script1, utxo))
    }

    // A utxo carrying a reference script for script 2
    val utxoWithScript2ReferenceStep: ReferenceOutput = {
        val utxo = genAdaOnlyPubKeyUtxo(Alice).sample.get
        ReferenceOutput(Utxo(setRefScript(script2, utxo)))
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

    val mintSigners = Set(ExpectedSigner(genAddrKeyHash.sample.get))

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

    testBuilderStepsFail(
      label = "Mint 0 directly",
      steps = List(mintScript1(0)),
      error = CannotMintZero(scriptHash1, AssetName.empty, mintScript1(0))
    )

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
        StakeCredential(pubKeyHashCredential1),
        witness,
        IssueCertificate(UnregCert(pubKeyHashCredential1, coin = None), witness)
      )
    )

    testBuilderStepsFail(
      label = "deregistering stake credential with wrong witness fails",
      steps = List(
        IssueCertificate(
          cert = UnregCert(Credential.ScriptHash(script2.scriptHash), coin = None),
          witness = TwoArgumentPlutusScriptWitness(
            PlutusScriptValue(script1),
            Data.List(PList.Nil),
            Set.empty
          )
        )
      ),
      error = IncorrectScriptHash(
        script1,
        script2.scriptHash,
        IssueCertificate(
          cert = UnregCert(Credential.ScriptHash(script2.scriptHash), coin = None),
          witness = TwoArgumentPlutusScriptWitness(
            PlutusScriptValue(script1),
            Data.List(PList.Nil),
            Set.empty
          )
        )
      )
    )

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

def mkTransactionInput(txId: String, ix: Int): TransactionInput = {
    val txIdBytes: Array[Byte] = ByteString.fromHex(txId).bytes
    TransactionInput(
      transactionId = Hash(ByteString.fromArray(txIdBytes)),
      index = ix
    )
}

val input0: TransactionInput =
    mkTransactionInput("5d677265fa5bb21ce6d8c7502aca70b9316d10e958611f3c6b758f65ad959996", 0)

val input1: TransactionInput =
    mkTransactionInput("5d677265fa5bb21ce6d8c7502aca70b9316d10e958611f3c6b758f65ad959996", 1)

val input2: TransactionInput =
    mkTransactionInput("5d677265fa5bb21ce6d8c7502aca70b9316d10e958611f3c6b758f65ad959996", 2)

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
private type ContextTuple = (
    Transaction,
    Seq[DetachedRedeemer],
    Network,
    Set[ExpectedSigner],
    ResolvedUtxos,
    Seq[DelayedRedeemerSpec]
)
