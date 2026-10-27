package scalus.cardano.ledger.rules

import org.scalatest.funsuite.AnyFunSuite
import scalus.cardano.address.Network
import scalus.cardano.ledger.*
import scalus.serialization.cbor.Cbor
import scalus.uplc.builtin.ByteString
import scalus.uplc.{Constant, Program, ProgramFlatCodec, Term}

/** `Babbage.validateScriptsWellFormed` in cardano-ledger, on a transaction from preprod. */
class ScriptsWellFormedValidatorTest extends AnyFunSuite {

    /** Preprod transaction `aee75c1c595f90f893717d0186a96f09abf28579f08b76e412f327fa36bebf9b`, in
      * block 4,045,972 at protocol version 10. Output 0 carries a Plutus V2 reference script whose
      * 2156 bytes are one 1078-byte CBOR byte string written twice. The ledger reads the first and
      * ignores the rest, as it does for every Plutus V1 and V2 script.
      */
    private val preprodTx: Transaction = Transaction.fromCbor(
      ByteString
          .fromHex(
            "84a300d9010281825820a1b3f7aeac6cd63aa5c02440db29cfd935af7e0feee3808978f119a0dd7aafe50101" +
                "82a300583900380afe50f7959ba7e3af0d6c97cc871cdb27765ec708eb7304e14011a1016e8152e71d381d1d" +
                "07cda1925f6e356c54a9bf8fa9c866c3239b011a00e4e1c003d818590871820259086c590433010100229800" +
                "aba2aba1aba0aab9faab9eaab9dab9a488888896600264653001300800198041804800cdc3a4005300800248" +
                "88966002600460106ea800e26466453001159800980098059baa0028cc004c03cc030dd50014c03cc030dd50" +
                "0248c966002600e601a6ea800626eb4c044c038dd5000c5900c180818069baa3010300d37546020602260226" +
                "0226022602260226022601a6ea80066e1d200448888c966002600c60206ea803226464b30013008301237540" +
                "0319800980b18099baa0019180b980c000c8c05cc060c06000522232598009804180b1baa001899912cc004c" +
                "04a60026eacc014c064dd51802980c9baa0069bae30053019375403f375c600860326ea807d001456600266e" +
                "24c02c030c0240362602530013756600a60326ea8c014c064dd50034dd71802980c9baa01f9bae3004301937" +
                "5403e800a2c80ba2c80b8c068c05cdd5000911192cc004c050c068dd5000c5200089bad301e301b375400280" +
                "c8c966002602860346ea8006298103d87a8000899198008009bab301f301c375400444b30010018a6103d87a" +
                "8000899192cc004cdc8803000c56600266e3c0180062601c66042603e00497ae08a60103d87a800040751330" +
                "04004302300340746eb8c074004c08000501e203232330010010042259800800c5300103d87a8000899192cc" +
                "004cdc8803000c56600266e3c0180062601a66040603c00497ae08a60103d87a800040711330040043022003" +
                "40706eb8c070004c07c00501d4590151800980b1baa3002301637540068b2022329800800cdd6180b18099ba" +
                "a0069bad30163013375401c80088896600200514c103d87a80008acc004c028006260086602e603000497ae0" +
                "8cc00400e6032005337000029000a006404c80b0dd2a400113322598009980080300145660026600200c0051" +
                "4a3133710600600e600a00c808a2c8088c966002600e0031375c602a60246ea801a2b3001300b00189bae301" +
                "53012375400d1598009801800c4dd7180a98091baa0068acc004cdc3a400c0031375c602a60246ea801a26eb" +
                "8c054c048dd500320204040808101018081baa0052232330010013758602e603060306030603060306030603" +
                "0603060286ea800c896600200314a115980099b8f375c603000200714a31330020023019001404c80b100f11" +
                "92cc004c01c00626eb4c054c058c058c058c048dd5001456600260160031375a602a602c602c602c602c6024" +
                "6ea800a2b3001300300189bad3015301630163016301630163012375400515980099b874801800626eb4c054" +
                "c058c058c058c058c048dd500144dd6980a980b180b180b18091baa002404080810102020301037540028b20" +
                "1498059baa0079807801a444b30013004002899192cc004c05400a00b1640486eb4c04c004c03cdd50054566" +
                "002601000515980098079baa00a801c5901045900d201a180698070009b8748000c024dd5001c590070c0200" +
                "04c00cdd5004452689b2b20021590433010100229800aba2aba1aba0aab9faab9eaab9dab9a4888888966002" +
                "64653001300800198041804800cdc3a400530080024888966002600460106ea800e264664530011598009800" +
                "98059baa0028cc004c03cc030dd50014c03cc030dd500248c966002600e601a6ea800626eb4c044c038dd500" +
                "0c5900c180818069baa3010300d375460206022602260226022602260226022601a6ea80066e1d200448888c" +
                "966002600c60206ea803226464b300130083012375400319800980b18099baa0019180b980c000c8c05cc060" +
                "c06000522232598009804180b1baa001899912cc004c04a60026eacc014c064dd51802980c9baa0069bae300" +
                "53019375403f375c600860326ea807d001456600266e24c02c030c0240362602530013756600a60326ea8c01" +
                "4c064dd50034dd71802980c9baa01f9bae30043019375403e800a2c80ba2c80b8c068c05cdd5000911192cc0" +
                "04c050c068dd5000c5200089bad301e301b375400280c8c966002602860346ea8006298103d87a8000899198" +
                "008009bab301f301c375400444b30010018a6103d87a8000899192cc004cdc8803000c56600266e3c0180062" +
                "601c66042603e00497ae08a60103d87a80004075133004004302300340746eb8c074004c08000501e2032323" +
                "30010010042259800800c5300103d87a8000899192cc004cdc8803000c56600266e3c0180062601a66040603" +
                "c00497ae08a60103d87a80004071133004004302200340706eb8c070004c07c00501d4590151800980b1baa3" +
                "002301637540068b2022329800800cdd6180b18099baa0069bad30163013375401c80088896600200514c103" +
                "d87a80008acc004c028006260086602e603000497ae08cc00400e6032005337000029000a006404c80b0dd2a" +
                "400113322598009980080300145660026600200c00514a3133710600600e600a00c808a2c8088c966002600e" +
                "0031375c602a60246ea801a2b3001300b00189bae30153012375400d1598009801800c4dd7180a98091baa00" +
                "68acc004cdc3a400c0031375c602a60246ea801a26eb8c054c048dd500320204040808101018081baa005223" +
                "2330010013758602e6030603060306030603060306030603060286ea800c896600200314a115980099b8f375" +
                "c603000200714a31330020023019001404c80b100f1192cc004c01c00626eb4c054c058c058c058c048dd500" +
                "1456600260160031375a602a602c602c602c602c60246ea800a2b3001300300189bad3015301630163016301" +
                "630163012375400515980099b874801800626eb4c054c058c058c058c058c048dd500144dd6980a980b180b1" +
                "80b18091baa002404080810102020301037540028b201498059baa0079807801a444b30013004002899192cc" +
                "004c05400a00b1640486eb4c04c004c03cdd50054566002601000515980098079baa00a801c5901045900d20" +
                "1a180698070009b8748000c024dd5001c590070c020004c00cdd5004452689b2b2002182583900380afe50f7" +
                "959ba7e3af0d6c97cc871cdb27765ec708eb7304e14011a1016e8152e71d381d1d07cda1925f6e356c54a9bf" +
                "8fa9c866c3239b1b00000002527b12c5021a00041779a100d90102818258209f4c51bcbafefb0357ebc6aee9" +
                "cd60bbf171eca59a84db9db36f32953315e635584071e31ebe92ec9e6ebc5fe66fa7683aec0b33bb8634eb5e" +
                "39733dfa0a1a58993f0ed80b7001673cd0738efc61b1629fbfc3227c6f790e112d666373074e45e400f5f6"
          )
          .bytes
    )

    private def context(protocolMajor: Int): Context = {
        val params = CardanoInfo.preprod.protocolParams
        Context(
          env = UtxoEnv(
            0,
            params.copy(protocolVersion = ProtocolVersion(protocolMajor, 0)),
            CertState.empty,
            Network.Testnet,
            Coin.zero
          ),
          slotConfig = CardanoInfo.preprod.slotConfig
        )
    }

    test("a Plutus V2 reference script with bytes after its CBOR byte string is well formed") {
        assert(
          preprodTx.id == TransactionHash.fromHex(
            "aee75c1c595f90f893717d0186a96f09abf28579f08b76e412f327fa36bebf9b"
          )
        )
        val script = preprodTx.body.value.outputs.head.value.scriptRef.get.script
        assert(script.isInstanceOf[Script.PlutusV2])
        assert(
          script.scriptHash == ScriptHash.fromHex(
            "5272e7132a75d7e314a75b1107b8a76a7fc0c284e48aa0b016251f7a"
          )
        )
        assert(ScriptsWellFormedValidator.validate(context(10), State(), preprodTx).isRight)
    }

    test("Babbage introduced the check: before protocol version 7 a malformed script passes") {
        // `\redeemer ctx -> ()` with one byte after its filler, which no language allows
        val flat = ProgramFlatCodec.unsafeEncodeFlat(
          Program.plutusV1(Term.LamAbs("redeemer", Term.LamAbs("ctx", Term.Const(Constant.Unit))))
        )
        val script = Script.PlutusV1(ByteString.fromArray(Cbor.encode(flat :+ 0.toByte)))
        val tx = Transaction(
          TransactionBody(
            inputs = TaggedSortedSet.empty,
            outputs = IndexedSeq.empty,
            fee = Coin.zero
          ),
          TransactionWitnessSet(
            scripts = Seq(script),
            redeemers = None,
            vkeyWitnesses = Set.empty,
            plutusData = Seq.empty
          )
        )
        assert(ScriptsWellFormedValidator.validate(context(6), State(), tx).isRight)
        assert(ScriptsWellFormedValidator.validate(context(7), State(), tx).isLeft)
    }
}
