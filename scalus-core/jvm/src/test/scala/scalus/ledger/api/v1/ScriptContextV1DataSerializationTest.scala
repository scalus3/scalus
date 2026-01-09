package scalus.ledger.api.v1

import io.bullet.borer.Cbor
import scalus.*
import scalus.builtin.ByteString.*
import scalus.builtin.Data
import scalus.builtin.Data.*
import scalus.cardano.ledger.{Language, MajorProtocolVersion}
import scalus.compiler.sir.TargetLoweringBackend
import scalus.compiler.{compile, Options}
import scalus.ledger.api.v1.Credential.*
import scalus.ledger.api.v2
import scalus.prelude.List.*
import scalus.prelude.Option.*
import scalus.prelude.SortedMap
import scalus.uplc.*
import scalus.uplc.eval.{MachineParams, PlutusVM}
import scalus.utils.Hex.hexToBytes
import scalus.utils.Utils

import scala.language.implicitConversions

class ScriptContextV1DataSerializationTest extends BaseValidatorTest:
    /*
  From this Haskell code:
  let addr = Address (PubKeyCredential (PubKeyHash "\00\01")) (Just (StakingHash (PubKeyCredential (PubKeyHash "aa01"))))
  let txOutRef = TxOutRef (TxId "deadbeef") 2
  let txOut = TxOut addr (Value.singleton "aa" "bb" 3) (Just (DatumHash "dada"))
  let txInfo = TxInfo  {
          txInfoInputs = [],
          txInfoOutputs = [txOut],
          txInfoFee = Value.singleton "aa" "USD" 1,
          txInfoMint = Value.singleton "aa" "EUR" 2,
          txInfoDCert = [],
          txInfoWdrl = [],
          txInfoValidRange = Interval.always,
          txInfoSignatories = [PubKeyHash "signatory"],
          txInfoData = [],
          txInfoId = TxId "11"
        }
  let sc = ScriptContext txInfo (Spending txOutRef)
  print (bs2Hex (Builtins.serialiseData (toBuiltinData sc)))
     */
    val plutusSerializedData =
        "d8799fd8799f809fd8799fd8799fd8799f420001ffd8799fd8799fd8799f4461613031ffffffffa141aaa142626203d8799f4464616461ffffffa141aaa14355534401a141aaa143455552028080d8799fd8799fd87980d87a80ffd8799fd87b80d87a80ffff9f497369676e61746f7279ff80d8799f423131ffffd87a9fd8799fd8799f486465616462656566ff02ffffff"

    /*
  serialisedDeserialiserV1 = B16.encode . Serialise.serialise . fromCompiledCode $ $$(PlutusTx.compile [||unsafeFromBuiltinData @V1.ScriptContext||])
     */
    val deserializeContractV1 =
        "5906f60100003232323232323232323232323232332232323232232323333573466e1cd55cea8012400046644246600200600464646464646464646464646666ae68cdc39aab9d500a480008cccccccccc888888888848cccccccccc00402c02802402001c01801401000c008cd40448c8c8cccd5cd19b8735573aa0049000119910919800801801180e1aba150023016357426ae8940088c98c8094cd5ce01281381189aab9e5001137540026ae854028cd4044048d5d0a804999aa80a3ae501335742a010666aa028eb9404cd5d0a80399a80880e1aba15006335011335501f01d75a6ae854014c8c8c8cccd5cd19b8735573aa00490001199109198008018011919191999ab9a3370e6aae754009200023322123300100300233502275a6ae854008c08cd5d09aba2500223263202933573805205604e26aae7940044dd50009aba150023232323333573466e1cd55cea8012400046644246600200600466a044eb4d5d0a80118119aba135744a004464c6405266ae700a40ac09c4d55cf280089baa001357426ae8940088c98c8094cd5ce01281381189aab9e5001137540026ae854010cd4045d71aba15003335011335501f75c40026ae854008c064d5d09aba2500223263202133573804204603e26ae8940044d5d1280089aba25001135744a00226ae8940044d5d1280089aba25001135744a00226aae7940044dd50009aba150023232323333573466e1d400520062321222230040053014357426aae79400c8cccd5cd19b875002480108c848888c008014c058d5d09aab9e500423333573466e1d400d20022321222230010053012357426aae7940148cccd5cd19b875004480008c848888c00c014dd71aba135573ca00c464c6403866ae7007007806806406005c4d55cea80089baa001357426ae8940088c98c8054cd5ce00a80b80989aab9e5001137540022464460046eb0004c8004d5405088cccd55cf80092804919a80418021aba100230033574400402a46464646666ae68cdc39aab9d5003480008ccc88848ccc00401000c008c8c8c8cccd5cd19b8735573aa0049000119910919800801801180a9aba1500233500e014357426ae8940088c98c8060cd5ce00c00d00b09aab9e5001137540026ae85400cccd5401dd728031aba1500233500a75c6ae84d5d1280111931900a19ab9c014016012135744a00226aae7940044dd5000899aa800bae75a224464460046eac004c8004d5404888c8cccd55cf80112804119a80399aa80a18031aab9d5002300535573ca00460086ae8800c0504d5d080088910010910911980080200189119191999ab9a3370ea0029000119091180100198029aba135573ca00646666ae68cdc3a801240044244002464c6402066ae700400480380344d55cea80089baa001232323333573466e1cd55cea80124000466442466002006004600a6ae854008dd69aba135744a004464c6401a66ae7003403c02c4d55cf280089baa0012323333573466e1cd55cea800a400046eb8d5d09aab9e500223263200b33573801601a01226ea8004488c8c8cccd5cd19b87500148010848880048cccd5cd19b875002480088c84888c00c010c018d5d09aab9e500423333573466e1d400d20002122200223263200e33573801c02001801601426aae7540044dd50009191999ab9a3370ea0029001109100111999ab9a3370ea0049000109100091931900519ab9c00a00c008007135573a6ea80048c8c8c8c8c8cccd5cd19b8750014803084888888800c8cccd5cd19b875002480288488888880108cccd5cd19b875003480208cc8848888888cc004024020dd71aba15005375a6ae84d5d1280291999ab9a3370ea00890031199109111111198010048041bae35742a00e6eb8d5d09aba2500723333573466e1d40152004233221222222233006009008300c35742a0126eb8d5d09aba2500923333573466e1d40192002232122222223007008300d357426aae79402c8cccd5cd19b875007480008c848888888c014020c038d5d09aab9e500c23263201233573802402802001e01c01a01801601426aae7540104d55cf280189aab9e5002135573ca00226ea80048c8c8c8c8cccd5cd19b875001480088ccc888488ccc00401401000cdd69aba15004375a6ae85400cdd69aba135744a00646666ae68cdc3a80124000464244600400660106ae84d55cf280311931900599ab9c00b00d009008135573aa00626ae8940044d55cf280089baa001232323333573466e1d400520022321223001003375c6ae84d55cf280191999ab9a3370ea004900011909118010019bae357426aae7940108c98c8020cd5ce00400500300289aab9d5001137540022244464646666ae68cdc39aab9d5002480008cd54024c018d5d0a80118029aba135744a004464c6401066ae700200280184d55cf280089baa00149924103505431001122123300100300212001112323001001223300330020020011"
    val deserializeContractV2 =
        "5907490100003232323232323232323232323233223232323232232323333573466e1cd55cea80124000466442466002006004646464646464646464646464646666ae68cdc39aab9d500c480008cccccccccccc88888888888848cccccccccccc00403403002c02802402001c01801401000c008cd404c050d5d0a80619a80980a1aba1500b33501301535742a014666aa02eeb94058d5d0a804999aa80bbae501635742a01066a02603c6ae85401cccd5405c07dd69aba150063232323333573466e1cd55cea801240004664424660020060046464646666ae68cdc39aab9d5002480008cc8848cc00400c008cd40a5d69aba15002302a357426ae8940088c98c80b0cd5ce01601681509aab9e5001137540026ae854008c8c8c8cccd5cd19b8735573aa004900011991091980080180119a814bad35742a00460546ae84d5d1280111931901619ab9c02c02d02a135573ca00226ea8004d5d09aba2500223263202833573805005204c26aae7940044dd50009aba1500533501375c6ae854010ccd5405c06c8004d5d0a801999aa80bbae200135742a004603a6ae84d5d1280111931901219ab9c024025022135744a00226ae8940044d5d1280089aba25001135744a00226ae8940044d5d1280089aba25001135744a00226ae8940044d55cf280089baa00135742a004601a6ae84d5d1280111931900b19ab9c016017014135573ca00226ea800448c88c008dd6000990009aa80a111999aab9f0012500a233500930043574200460066ae880080548c8c8cccd5cd19b8735573aa004900011991091980080180118061aba150023005357426ae8940088c98c8050cd5ce00a00a80909aab9e5001137540024646464646666ae68cdc39aab9d5004480008cccc888848cccc00401401000c008c8c8c8cccd5cd19b8735573aa0049000119910919800801801180a9aba1500233500f014357426ae8940088c98c8064cd5ce00c80d00b89aab9e5001137540026ae854010ccd54021d728039aba150033232323333573466e1d4005200423212223002004357426aae79400c8cccd5cd19b875002480088c84888c004010dd71aba135573ca00846666ae68cdc3a801a400042444006464c6403666ae7006c07006406005c4d55cea80089baa00135742a00466a016eb8d5d09aba2500223263201533573802a02c02626ae8940044d5d1280089aab9e500113754002266aa002eb9d6889119118011bab00132001355011223233335573e0044a010466a00e66442466002006004600c6aae754008c014d55cf280118021aba200301313574200222440042442446600200800624464646666ae68cdc3a800a40004642446004006600a6ae84d55cf280191999ab9a3370ea0049001109100091931900819ab9c01001100e00d135573aa00226ea80048c8c8cccd5cd19b875001480188c848888c010014c01cd5d09aab9e500323333573466e1d400920042321222230020053009357426aae7940108cccd5cd19b875003480088c848888c004014c01cd5d09aab9e500523333573466e1d40112000232122223003005375c6ae84d55cf280311931900819ab9c01001100e00d00c00b135573aa00226ea80048c8c8cccd5cd19b8735573aa004900011991091980080180118029aba15002375a6ae84d5d1280111931900619ab9c00c00d00a135573ca00226ea80048c8cccd5cd19b8735573aa002900011bae357426aae7940088c98c8028cd5ce00500580409baa001232323232323333573466e1d4005200c21222222200323333573466e1d4009200a21222222200423333573466e1d400d2008233221222222233001009008375c6ae854014dd69aba135744a00a46666ae68cdc3a8022400c4664424444444660040120106eb8d5d0a8039bae357426ae89401c8cccd5cd19b875005480108cc8848888888cc018024020c030d5d0a8049bae357426ae8940248cccd5cd19b875006480088c848888888c01c020c034d5d09aab9e500b23333573466e1d401d2000232122222223005008300e357426aae7940308c98c804ccd5ce00980a00880800780700680600589aab9d5004135573ca00626aae7940084d55cf280089baa0012323232323333573466e1d400520022333222122333001005004003375a6ae854010dd69aba15003375a6ae84d5d1280191999ab9a3370ea0049000119091180100198041aba135573ca00c464c6401866ae700300340280244d55cea80189aba25001135573ca00226ea80048c8c8cccd5cd19b875001480088c8488c00400cdd71aba135573ca00646666ae68cdc3a8012400046424460040066eb8d5d09aab9e500423263200933573801201400e00c26aae7540044dd500089119191999ab9a3370ea00290021091100091999ab9a3370ea00490011190911180180218031aba135573ca00846666ae68cdc3a801a400042444004464c6401466ae7002802c02001c0184d55cea80089baa0012323333573466e1d40052002212200223333573466e1d40092000212200123263200633573800c00e00800626aae74dd5000a4c92010350543100120011123230010012233003300200200101"

    val scalusDeserializerV1SIR = compile { (d: Data) => fromData[ScriptContext](d) }
    val scalusDeserializerV2SIR = compile { (d: Data) =>
        fromData[v2.ScriptContext](d)
    }
    val scalusDeserializerV1 = scalusDeserializerV1SIR.toUplc(generateErrorTraces = false)
    val scalusDeserializerV2 = scalusDeserializerV2SIR.toUplc(generateErrorTraces = false)

    val address = Address(
      PubKeyCredential(PubKeyHash(hex"0001")),
      Some(
        StakingCredential.StakingHash(PubKeyCredential(PubKeyHash(hex"61613031")))
      )
    )
    val signatories = Cons(PubKeyHash(hex"7369676e61746f7279"), Nil)
    val txInfo = TxInfo(
      inputs = Nil,
      outputs = Cons(
        TxOut(
          address,
          Value(hex"aa", hex"6262", 3),
          Some(hex"64616461")
        ),
        Nil
      ),
      fee = Value(hex"AA", hex"555344", 1),
      mint = Value(hex"AA", hex"455552", 2),
      dcert = Nil,
      withdrawals = Nil,
      validRange = Interval.always,
      signatories = signatories,
      data = Nil,
      id = TxId(hex"3131")
    )
    val scriptContextV1 = ScriptContext(
      txInfo,
      ScriptPurpose.Spending(TxOutRef(TxId(hex"6465616462656566"), 2))
    )

    val scriptContextV2 = v2.ScriptContext(
      v2.TxInfo(
        inputs = Nil,
        referenceInputs = Nil,
        outputs = Nil,
        fee = txInfo.fee,
        mint = txInfo.mint,
        dcert = txInfo.dcert,
        withdrawals = SortedMap.empty,
        validRange = txInfo.validRange,
        signatories = txInfo.signatories,
        data = SortedMap.empty,
        redeemers = SortedMap.empty,
        id = txInfo.id
      ),
      ScriptPurpose.Spending(TxOutRef(TxId(hex"6465616462656566"), 2))
    )

    val machineParamsV1 =
        MachineParams.defaultParamsFor(Language.PlutusV1, MajorProtocolVersion.vasilPV)
    val machineParamsV2 =
        MachineParams.defaultParamsFor(Language.PlutusV2, MajorProtocolVersion.vasilPV)

    // in Data-based backend, to/from Data is no-op, so use simple backend
    given Options = Options(
      targetLoweringBackend = TargetLoweringBackend.ScottEncodingLowering,
      generateErrorTraces = true,
      optimizeUplc = false,
      debug = false
    )

    test("ScriptContext is the same as in Plutus") {
        // byte array to hex string

        def encodeAsHexString(d: Data) = Utils.bytesToHex(Cbor.encode(d).toByteArray)

        val plutusData = Cbor.decode(plutusSerializedData.hexToBytes).to[Data].value
        val plutusScriptContext = fromData[ScriptContext](plutusData)
        assert(plutusScriptContext == scriptContextV1)
        assert(plutusData == scriptContextV1.toData)
        assert(encodeAsHexString(scriptContextV1.toData) == plutusSerializedData)
    }

    test("deserialize ScriptContext V1 using Plutus") {
        given PlutusVM = PlutusVM.makePlutusV1VM(machineParamsV1)
        val program = Program.fromCborHex(deserializeContractV1)
        val applied = program $ scriptContextV1.toData

        assertSameResult(Expected.SuccessSame)(applied)
        try applied.evaluate
        catch case e: Throwable => fail(e)

        assertThrows[Exception]((program $ scriptContextV2.toData).evaluate)
    }

    test("deserialize ScriptContext V1 using Scalus") {

        import scalus.uplc.TermDSL.given
        given PlutusVM = PlutusVM.makePlutusV1VM(machineParamsV1)
        val applied = Program((1, 0, 0), scalusDeserializerV1 $ scriptContextV1.toData)

        assertSameResult(Expected.SuccessSame)(applied)
        try applied.evaluate
        catch case e: Throwable => fail(e)

        assertThrows[Exception](
          Program((1, 0, 0), scalusDeserializerV1 $ scriptContextV2.toData).evaluate
        )
    }

    test("deserialize ScriptContext V2 using Plutus") {
        given PlutusVM = PlutusVM.makePlutusV2VM(machineParamsV2)
        val program = Program.fromCborHex(deserializeContractV2)
        val applied = program $ scriptContextV2.toData
        try applied.evaluate
        catch case e: Throwable => fail(e)

        assertThrows[Exception]((program $ scriptContextV1.toData).evaluate)
    }

    test("deserialize ScriptContext V2 using Scalus") {

        import scalus.uplc.TermDSL.given
        val applied = Program((1, 0, 0), scalusDeserializerV2 $ scriptContextV2.toData)

        assertSameResult(Expected.SuccessSame)(applied)
        given PlutusVM = PlutusVM.makePlutusV2VM(machineParamsV2)
        try applied.evaluate
        catch case e: Throwable => fail(e)

        assertThrows[Exception](
          Program((1, 0, 0), scalusDeserializerV2 $ scriptContextV1.toData).evaluate
        )
    }
