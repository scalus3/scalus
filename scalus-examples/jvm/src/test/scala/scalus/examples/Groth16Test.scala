package scalus.examples
import org.scalatest.funsuite.AnyFunSuite
import scalus.*
import scalus.builtin.ByteString.hex
import scalus.builtin.Data.toData
import scalus.builtin.Data
import scalus.compiler.sir.TargetLoweringBackend
import scalus.compiler.{compile, Options}
import scalus.examples.Groth16.*
import scalus.prelude.List
import scalus.uplc.TermDSL.given
import scalus.uplc.eval.PlutusVM
import scalus.uplc.{Constant, Term}

import scala.language.implicitConversions

class Groth16Test extends AnyFunSuite:
    // Common verification key used in factorial tests (n! problem)
    private val factorialVK = SnarkVerificationKey(
      nPublic = 2,
      vkAlpha =
          hex"8e3d9e248feda194cb6fa0a3b64fd2a380cb5e94836bf8148bf97ebcbb5819d9a78f63102f0293c104bcbb2f810d8eb4",
      vkBeta =
          hex"8cd68a7186a908212680a0234d8210c20328f8fb3ce1d69c9aec9330a5802d6cfaf6d7cf3176133221c19188590cb4141874ea7bbfcb9872931e115d882c46b90c3dcbcee10062d1c9b9b0a691d7bec7d2735f06495c7f71dea210e55b2782df",
      vkGamma =
          hex"93e02b6052719f607dacd3a088274f65596bd0d09920b61ab5da61bbdc7f5049334cf11213945d57e5ac7d055d042b7e024aa2b2f08f0a91260805272dc51051c6e47ad4fa403b02b4510b647ae3d1770bac0326a805bbefd48056c8c121bdb8",
      vkDelta =
          hex"938231fcec443fbdeb1079ff126b8f69bd8579ffe82d39923214d4345395beee60200288fa20c97ae50f3212131b6f8802af2f9f515c65af6a9a6c294c738590104376a0af44731d6699db6a286608774243f7d1dddc4605eb340e65e15060a5",
      vkAlphaBeta = List(
        hex"93e02b6052719f607dacd3a088274f65596bd0d09920b61ab5da61bbdc7f5049334cf11213945d57e5ac7d055d042b7e024aa2b2f08f0a91260805272dc51051c6e47ad4fa403b02b4510b647ae3d1770bac0326a805bbefd48056c8c121bdb8"
      ),
      vkIC = List(
        hex"b5813c90d3455acb8608fdf66e8601f24ef048f3fdf9384862d77c72cb5cddfde2b307976b1b0319c42ba985f94be60a",
        hex"a41a0e6370c054be0f9acce08e97f6d5702d9daa9e9a934e8a377f553593baa1c58062adc73d63558653422d54d6f50c",
        hex"8e02a87c519c3145f984d25fdf769f74fbc36626c385d4554fb4bc1d7a12cbf669d40f257023b8b3c9a31e631aa8f981"
      )
    )

    given Options = Options(
      targetLoweringBackend = TargetLoweringBackend.SirToUplcV3Lowering,
      generateErrorTraces = true,
      optimizeUplc = true,
      debug = false
    )

    private val validator =
        val sir = compile((vk: Data, proof: Data, public: Data) =>
            grothVerify(vk.to[SnarkVerificationKey], proof.to[Proof], public.to[List[BigInt]])
        )
        // println(s"Compiled Groth16 validator script: \n ${sir.pretty.render(100)}")
        // val lv = sir.toLoweredValue()
        // println(s"Compiled Groth16 validator script (LoweredValue): \n ${lv.pretty.render(100)}")
        val uplc = sir.toUplc()
        // println(s"Compiled Groth16 validator script (UPLC): \n ${uplc.pretty.render(100)}")
        uplc

    test("verify factorial proof - 3! = 6") {
        val proof = Proof(
          piA =
              hex"8b84d092731c653b1accdda79c51e3f5d289bed7311189d927deadef0470e437e6d1d400634726512a79a015867424e3",
          piB =
              hex"92cb1c125816e4b522c7f430a5d74a61116b6189de7b2341f040194c02f10d9ef0cf081f4029444a65ea74e69d98b1cf08d3864087d5d2dee2ed6ab102f9b78e65d341f0824341a9fc25d0ea9dacccc5d355b4eddb0057949370a19c47135b0e",
          piC =
              hex"a4ef633c858a3ff194db50eacdf715f7296fb3d1202c54b543284e9656b69aa90f33ac0e2572d3ab847b88268dcd1f7e"
        )
        val publicValues = List(BigInt(561), BigInt(3))

        assert(checkGroth16Verify(factorialVK, proof, publicValues))
    }

    test("detect invalid factorial proof - 3! = 6 (tampered)") {
        val proof = Proof(
          piA =
              hex"a4ef633c858a3ff194db50eacdf715f7296fb3d1202c54b543284e9656b69aa90f33ac0e2572d3ab847b88268dcd1f7e",
          piB =
              hex"92cb1c125816e4b522c7f430a5d74a61116b6189de7b2341f040194c02f10d9ef0cf081f4029444a65ea74e69d98b1cf08d3864087d5d2dee2ed6ab102f9b78e65d341f0824341a9fc25d0ea9dacccc5d355b4eddb0057949370a19c47135b0e",
          piC =
              hex"8b84d092731c653b1accdda79c51e3f5d289bed7311189d927deadef0470e437e6d1d400634726512a79a015867424e3"
        )
        val publicValues = List(BigInt(561), BigInt(3))

        assert(!checkGroth16Verify(factorialVK, proof, publicValues))
    }

    test("verify factorial proof - 7! = 5040") {
        val proof = Proof(
          piA =
              hex"98ca847cc04a6f67ac85a628521450323d7aa5335d4c2c48e9780b659cf7ea8ece2d0b305c9ff9dcfb3e548d61bbaebe",
          piB =
              hex"a8e6ba4dbce6aa84de8ca1cd39d42353fcac89fe8cb800e728ada3ca4ae3b07baa68f76b9e4fa73eebf78cc609fa85d6166a3b69cd08cc59f2ff36e52dfdf231540a4212fdd4a142504c76066bddea342dd0183b2b11ed62cfc1497189a4db52",
          piC =
              hex"8bc8cc3f11483138cc55d5f0389e67231f9e8465e5cb4a5a668e6e298d5c4febb2a18e86881c84dd03c5d33db65af272"
        )
        val publicValues = List(BigInt(8827), BigInt(7))

        assert(checkGroth16Verify(factorialVK, proof, publicValues))
    }

    test("detect invalid factorial proof - 7! = 5040 (tampered)") {
        val proof = Proof(
          piA =
              hex"8bc8cc3f11483138cc55d5f0389e67231f9e8465e5cb4a5a668e6e298d5c4febb2a18e86881c84dd03c5d33db65af272",
          piB =
              hex"a8e6ba4dbce6aa84de8ca1cd39d42353fcac89fe8cb800e728ada3ca4ae3b07baa68f76b9e4fa73eebf78cc609fa85d6166a3b69cd08cc59f2ff36e52dfdf231540a4212fdd4a142504c76066bddea342dd0183b2b11ed62cfc1497189a4db52",
          piC =
              hex"8e02a87c519c3145f984d25fdf769f74fbc36626c385d4554fb4bc1d7a12cbf669d40f257023b8b3c9a31e631aa8f981"
        )
        val publicValues = List(BigInt(8827), BigInt(7))

        assert(!checkGroth16Verify(factorialVK, proof, publicValues))
    }

    // Mastermind game verification key
    private val mastermindVK = SnarkVerificationKey(
      nPublic = 8,
      vkAlpha =
          hex"910df641298bf37a09468eb8eefe36cd9333d2698abee298b25521570d19a04c7482b35ed0392a8275ced89cbfa44f39",
      vkBeta =
          hex"a13e94f2ba50b23446d56fb039cbd357875acbab92ccff7471e7ca79d2748684e6c4c322dce042d33bbfb23bb22cc57d187ca5634ddad5994f1f9a2c5d9d98d8c1916d767bc9aa23de1549a12640aef88543223ac5828d0520007c140f9e8716",
      vkGamma =
          hex"93e02b6052719f607dacd3a088274f65596bd0d09920b61ab5da61bbdc7f5049334cf11213945d57e5ac7d055d042b7e024aa2b2f08f0a91260805272dc51051c6e47ad4fa403b02b4510b647ae3d1770bac0326a805bbefd48056c8c121bdb8",
      vkDelta =
          hex"8d835235db743dab2ec2817dc2545aaf4eab5997ce1df83195733c6f7ca9619593f66d06153b294f9dc9bf4e45232534084068327fdbcc5f6bdc07bf4dd6cdd8962c75112842fd7e92b2f13da4c565c0fcff2818e34b768856dfa8041bed7b9e",
      vkAlphaBeta = List(
        hex"93e02b6052719f607dacd3a088274f65596bd0d09920b61ab5da61bbdc7f5049334cf11213945d57e5ac7d055d042b7e024aa2b2f08f0a91260805272dc51051c6e47ad4fa403b02b4510b647ae3d1770bac0326a805bbefd48056c8c121bdb8"
      ),
      vkIC = List(
        hex"aae31c7beb5a2f92b3f9be7ff278c841dc15769244c5cb81b95858fcdfad34621a86f956e1de57cb104050302cc8ae09",
        hex"b721c142b4c9c1de3570dd30d6ec1fc009deef3319ecd0c681c37301eab8fa9e625d4457b3376de26c83d3a6dc63b290",
        hex"b1e0a4b9324277450efd02db4b7d0451a6d556f6bd61fef2df9a7ce7fd321b27e8a0e5943967b1f82856aa87a40f17ba",
        hex"ad3f4b41f48a13eceda136882dfc5cc543cab7714a0abdedf7cc85fa58b4b2c69c1a8f74bc0d89901f2a4ab717a3e412",
        hex"84e0a2709079a6c2b0ce1d3e19242d7a729e19936e8a9a7297055545048d1d7d14bd62baecb8bdd6b33809368461a5c9",
        hex"b33e6b2bfc9f29ef766f6167bf6b3873e073eb79a55153b1ebba15187593273bf64ef7e155d1118535bab8d16676c27e",
        hex"89ce67829659a866237178e52ecbfaeba754a9db3bcc097aa230cd0af9daae18346dc51685ca4ccf650094698a7115c9",
        hex"8c6420bca64925df0c00b45eba87e142b04a12f86013e78c3a97c73922910b94e0647c8d60721d5749e5f5fe739dcb28",
        hex"9142ab2d492fbd93323976cc16057301f4f078b273898fbf98e2c0c6aded69f54b0beae793855599d106400de6c85947"
      )
    )

    test("verify mastermind proof - valid game state") {
        val proof = Proof(
          piA =
              hex"972132cdfb134c36e0160ca01747460cffd948d70b0a8437d7f339ac505dada7823309187d60059af3c088f76f6d69aa",
          piB =
              hex"840cc65995a95c5a9d639324f56202ffe280be5d8a34301f8e78d6d5844fb549918f310a4f81b5863d4ac9e37be0f4a4131f10fbd82429378e00bcc3574d11ffd86db203271a050ec1e53c62e8cf3a025d4e84962ae56c21d9c2949943c8b602",
          piC =
              hex"8a957cd1045dda7f31f3747253317aa63a3f590a8423ec8665495e41b23326bd76ae6bbc3e7137b1a9452d4ccb6050b9"
        )

        val salt =
            BigInt("391047073361247658225232481261784502922906942967577347518894619662429381081")
        val publicValues = List(
          salt, // Salt value
          BigInt(4),
          BigInt(3),
          BigInt(2),
          BigInt(1),
          BigInt(0),
          BigInt(4), // Game state
          salt // Salt value again
        )

        assert(checkGroth16Verify(mastermindVK, proof, publicValues))
    }

    test("detect invalid mastermind proof - tampered game state") {
        val proof = Proof(
          piA =
              hex"972132cdfb134c36e0160ca01747460cffd948d70b0a8437d7f339ac505dada7823309187d60059af3c088f76f6d69aa",
          piB =
              hex"840cc65995a95c5a9d639324f56202ffe280be5d8a34301f8e78d6d5844fb549918f310a4f81b5863d4ac9e37be0f4a4131f10fbd82429378e00bcc3574d11ffd86db203271a050ec1e53c62e8cf3a025d4e84962ae56c21d9c2949943c8b602",
          piC =
              hex"8a957cd1045dda7f31f3747253317aa63a3f590a8423ec8665495e41b23326bd76ae6bbc3e7137b1a9452d4ccb6050b9"
        )

        val salt =
            BigInt("391047073361247658225232481261784502922906942967577347518894619662429381081")
        val publicValues = List(
          salt, // Salt value
          BigInt(3),
          BigInt(3),
          BigInt(2),
          BigInt(1),
          BigInt(0),
          BigInt(4), // Tampered game state
          salt // Salt value again
        )

        assert(!checkGroth16Verify(mastermindVK, proof, publicValues))
    }

    test("verify mastermind proof - alternative valid game state") {
        val proof = Proof(
          piA =
              hex"81a68d6aed63ad830e02fff4cbfd6201083f4d46e5d8758077322f3de4ae7a3b01280251da823952bc16e98abb5377c8",
          piB =
              hex"a24d1cd8bede8af638f30316695cc9880db776c7e7d54b4cf27f1483ad10c36e1ec99f113975cc80a49223d8ddd6efc709d8656db16dc1d3db65017329aa82bba4102e256380f60fa72c880a4f862049f3d2ae199ae09a496275a7b2c7c38f79",
          piC =
              hex"a82269de78d22f8e128c9d87c132f79eb228c2caa2927557c4e090d0473d02e4bbf149a21b87a3fde01f02a768fcf76b"
        )

        val salt =
            BigInt("391047073361247658225232481261784502922906942967577347518894619662429381081")
        val publicValues = List(
          salt,
          BigInt(1),
          BigInt(1),
          BigInt(1),
          BigInt(4),
          BigInt(2),
          BigInt(0),
          salt
        )

        assert(checkGroth16Verify(mastermindVK, proof, publicValues))
    }

    test("detect invalid mastermind proof - tampered proof") {
        val proof = Proof(
          piA =
              hex"a82269de78d22f8e128c9d87c132f79eb228c2caa2927557c4e090d0473d02e4bbf149a21b87a3fde01f02a768fcf76b",
          piB =
              hex"a24d1cd8bede8af638f30316695cc9880db776c7e7d54b4cf27f1483ad10c36e1ec99f113975cc80a49223d8ddd6efc709d8656db16dc1d3db65017329aa82bba4102e256380f60fa72c880a4f862049f3d2ae199ae09a496275a7b2c7c38f79",
          piC =
              hex"81a68d6aed63ad830e02fff4cbfd6201083f4d46e5d8758077322f3de4ae7a3b01280251da823952bc16e98abb5377c8"
        )

        val salt =
            BigInt("391047073361247658225232481261784502922906942967577347518894619662429381081")
        val publicValues = List(
          salt,
          BigInt(1),
          BigInt(1),
          BigInt(1),
          BigInt(4),
          BigInt(2),
          BigInt(0),
          salt
        )

        assert(!checkGroth16Verify(mastermindVK, proof, publicValues))
    }

    test("detect invalid mastermind proof - wrong salt value") {
        val proof = Proof(
          piA =
              hex"82d169d4f21b733c86aa39ab15e7076d237136e17493ea9102cef0277c467d47a24e6a046e537ca0a5baac62793039bb",
          piB =
              hex"9976e719144213a044a90090573e98b97f6103d47bd0d5c26264271e853621f32f7ed0c3f76ab90f71fe7e6699e9992f114dfac4a3f50305c7f94cd4e82298e63f82a69e941c26685a52ffb8abe88ee87755e2418561ef5ba06e509e0645a249",
          piC =
              hex"b1626dfbe3c41792412e5aef1221c1a0d6b5b90088eb3a9a8fbcdbabade123f8ccbea73b78d95fffa671d5a7a5115994"
        )

        // Using different salt value
        val publicValues = List(
          BigInt(235),
          BigInt(1),
          BigInt(4),
          BigInt(3),
          BigInt(2),
          BigInt(3),
          BigInt(2),
          BigInt("56789976643453433521111")
        )

        assert(!checkGroth16Verify(mastermindVK, proof, publicValues))
    }

    private def checkGroth16Verify(
        vk: SnarkVerificationKey,
        proof: Proof,
        public: List[BigInt]
    ): Boolean =
        given PlutusVM = PlutusVM.makePlutusV3VM()
        val offchainResult = grothVerify(vk, proof, public)
        val onchainResultTerm =
            val applied =
                validator $ vk.toData $ proof.toData $ public.toData
            applied.evaluate
        val Term.Const(Constant.Bool(onchainResult)) = onchainResultTerm: @unchecked
        assert(
          onchainResult == offchainResult,
          s"Offchain: $offchainResult, Onchain: $onchainResult"
        )
        onchainResult
