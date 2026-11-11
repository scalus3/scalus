package scalus.verify.uplcblaster

import org.scalatest.funsuite.AnyFunSuite
import scalus.cardano.onchain.plutus.prelude
import scalus.cardano.onchain.plutus.prelude.SortedMap
import scalus.cardano.onchain.plutus.v1.{Interval, PubKeyHash}
import scalus.cardano.onchain.plutus.v3.ScriptContext
import scalus.compiler.Compile
import scalus.uplc.builtin.{ByteString, Data, FromData, ToData}
import scalus.uplc.builtin.Data.toData
import scalus.verify.lean.LeanServer.{Message, Result, Severity}
import scalus.verify.lean.LeanTypes

import scala.concurrent.duration.*

/** A case class of the types without a declaration of their own, and a field Lean has a keyword
  * for.
  */
case class TypedRecord(amount: BigInt, owner: ByteString, open: Boolean, note: String, from: Data)
    derives FromData,
      ToData

@Compile
object TypedRecord

/** An enum with a constructor without fields, and one of a list and a sorted map. */
enum TypedChoice derives FromData, ToData:
    case Empty
    case Holding(items: prelude.List[TypedRecord], tokens: SortedMap[ByteString, BigInt])

@Compile
object TypedChoice

/** The Lean types of Scala types ([[LeanTypes]]): what is declared, and that Lean takes it and
  * encodes values as Scalus does.
  */
class LeanTypesTest extends AnyFunSuite with LeanProofsSuite {
    import proofs.*
    import DataEncodingTest.{hex, sampleContext}

    test("a case class is a structure of its fields, with its encoding") {
        val types = LeanTypes.of(List(LeanTypes.typeOf[TypedRecord]))
        val text = types.declarations.mkString("\n\n")
        assert(
          text.contains(
            """structure TypedRecord where
              |  amount : Integer
              |  owner : ByteString
              |  «open» : Bool
              |  note : ByteString
              |  «from» : Data
              |  deriving DecidableEq, Repr""".stripMargin
          ),
          text
        )
        assert(
          text.contains(
            """def TypedRecord.toData (x : TypedRecord) : Data :=
              |  Data.Constr 0 [(Data.I x.amount), (Data.B x.owner), (scalusBoolData x.«open»), (Data.B x.note), x.«from»]""".stripMargin
          ),
          text
        )
        val codec = types.codec(LeanTypes.typeOf[TypedRecord]).getOrElse(fail("no codec"))
        assert(codec.leanType == "TypedRecord")
        assert(codec.toData("r") == "(TypedRecord.toData r)")
        // Every value of the structure is well formed, and a string's UTF-8 is not said.
        assert(codec.wellFormed.isEmpty)
        assert(!codec.exact)
    }

    test("a sorted map has to be ascending, and so has what holds one") {
        val types = LeanTypes.of(List(LeanTypes.typeOf[TypedChoice]))
        val text = types.declarations.mkString("\n\n")
        assert(
          text.contains(
            """inductive TypedChoice where
              |  | Empty
              |  | Holding (items : (List TypedRecord)) (tokens : (List (ByteString × Integer)))
              |  deriving DecidableEq, Repr""".stripMargin
          ),
          text
        )
        assert(text.contains("def SortedMap_ByteString_Integer.ascending"), text)
        // With the previous key as an argument: Lean-blaster does not return on recursion on a
        // sublist bound by an as-pattern (Lean-blaster#463).
        assert(!text.contains("@("), text)
        val codec = types.codec(LeanTypes.typeOf[TypedChoice]).getOrElse(fail("no codec"))
        assert(codec.wellFormed.map(_("c")).contains("(TypedChoice.wellFormed c)"))
        // A type is declared once, however many types hold it.
        assert("structure TypedRecord where".r.findAllIn(text).size == 1)
    }

    test("a type that contains itself is Data that its encoding's predicate holds of") {
        val types = LeanTypes.of(List(LeanTypes.typeOf[EncodingRose]))
        val codec = types.codec(LeanTypes.typeOf[EncodingRose]).getOrElse(fail("no codec"))
        assert(codec.leanType == "Data")
        assert(codec.toData("r") == "r")
        assert(codec.wellFormed.map(_("r")).exists(_.startsWith("(scalusRec_EncodingRose_")))
        assert(codec.exact)
    }

    test("a type bound to a Lean type that exists is not declared, nor what only it is made of") {
        val theirs = LeanTypes.Codec(
          "TypedRecord",
          "Their.Record",
          x => s"(IsData.toData $x)",
          d => s"(IsData.fromData $d)",
          Some(x => s"(Their.valid $x)"),
          exact = false
        )
        val types = LeanTypes.of(
          List(LeanTypes.typeOf[TypedChoice]),
          Map("scalus.verify.uplcblaster.TypedRecord" -> theirs)
        )
        val text = types.declarations.mkString("\n\n")
        assert(!text.contains("structure TypedRecord"), text)
        assert(text.contains("| Holding (items : (List Their.Record))"), text)
        assert(text.contains("| x :: xs => (IsData.toData x) :: List_TypedRecord.toData xs"), text)
        assert(
          text.contains("| x :: xs => (Their.valid x) && List_TypedRecord.wellFormed xs"),
          text
        )
        assert(types.codec(LeanTypes.typeOf[TypedRecord]).contains(theirs))
    }

    test(
      "a module that is generated, or a proof by hand, is no part of what a kept result rests on"
    ) {
        import java.nio.file.Files
        import scalus.verify.lean.LeanWorkspace
        val workspace = Files.createTempDirectory("scalus-lean-types").resolve("Vault")
        Files.createDirectories(workspace.resolve("Vault").resolve("Proofs"))
        Files.writeString(workspace.resolve("lean-toolchain"), "leanprover/lean4:v4.24.0\n")
        Files.writeString(workspace.resolve("Vault.lean"), "import ScalusProofs.Run\n")
        val byHand = workspace.resolve("Vault").resolve("Proofs").resolve("Clamp.lean")
        Files.writeString(byHand, "theorem one : 1 = 1 := rfl\n")
        val own = workspace.resolve("Vault").resolve("Lemmas.lean")
        Files.writeString(own, "theorem one : 1 = 1 := rfl\n")
        val before = LeanWorkspace.environment(workspace)

        val (module, file) = LeanWorkspace.generated(workspace, "Types")
        assert(module == "Vault.Generated.Types")
        assert(file == workspace.resolve("Vault").resolve("Generated").resolve("Types.lean"))
        val text = LeanTypes.of(List(LeanTypes.typeOf[TypedRecord])).module(module)
        assert(LeanWorkspace.write(file, text))
        // The same text is not written again.
        assert(!LeanWorkspace.write(file, text))
        assert(LeanWorkspace.environment(workspace) == before)

        // A proof written by hand is not either: a check imports none of the workspace's modules.
        Files.writeString(byHand, "theorem two : 2 = 2 := rfl\n")
        assert(LeanWorkspace.environment(workspace) == before)

        // Another module of the workspace's package is.
        Files.writeString(own, "theorem two : 2 = 2 := rfl\n")
        assert(LeanWorkspace.environment(workspace) != before)
    }

    test("a statement is a Lean module, of which a proof by hand is a theorem") {
        import java.nio.file.Files
        import scalus.cardano.onchain.plutus.prelude.Math
        import scalus.verify.{FunctionDef, FunctionTable}
        import scalus.verify.Props.*
        val clamp = FunctionDef(Math.clamp)
        val inRange = forAll[BigInt, BigInt, BigInt]((x, lo, hi) =>
            (lo <= hi) ==>: callRef(clamp.ref, (x, lo, hi))(r => lo <= r && r <= hi)
        )
        val lowered = UplcBlaster.lower(inRange, FunctionTable(clamp)).fold(fail(_), identity)
        val module = "Vault.Generated.clamp_in_range"
        // In a workspace the programs are at paths from its directory; here, in a directory of
        // the test, by their whole paths.
        val directory = Files.createTempDirectory("scalus-statement-module")
        val written =
            UplcBlaster.statementModule(lowered, 120, 400000, module, directory.toString)
        assert(
          written.programs.map(_._1) == Vector(s"$directory/Leaf0.flat", s"$directory/Leaf1.flat")
        )
        written.programs.foreach((path, program) =>
            Files.writeString(java.nio.file.Path.of(path), UplcBlaster.programHex(program))
        )
        assert(written.text.contains(s"namespace $module"), written.text)
        assert(written.text.contains("def statement : Prop :=\n  (∀ (x0 : Integer)"), written.text)
        // The header says the programs and the budget, so the text changes with them, and a
        // module is held to the code by them.
        assert(UplcBlaster.statementModuleOf(written.text) == Some(120 -> lowered.programHashes))
        assert(UplcBlaster.statementModuleOf("def statement : Prop := True").isEmpty)
        assert(!written.text.contains("#blaster"))

        requireLean()
        val proof = s"theorem clamp_in_range : $module.statement := by\n" +
            s"  unfold $module.statement\n  blaster\n"
        leanServer.check(
          written.text + "\nset_option warn.sorry false\n" + proof,
          Some(5.minutes)
        ) match
            case Result.Finished(messages) =>
                val errors = messages.filter(_.severity == Severity.Error)
                assert(errors.isEmpty, errors.map((m: Message) => s"${m.line}: ${m.text}"))
            case other => fail(s"Lean did not check the statement: $other")
    }

    test("Lean takes the types, and encodes values as Scalus does") {
        val module = "Scalus.LeanTypesTest"
        val all = List(
          LeanTypes.typeOf[ScriptContext],
          LeanTypes.typeOf[TypedChoice],
          LeanTypes.typeOf[EncodingRose],
          LeanTypes.typeOf[prelude.Option[BigInt]],
          LeanTypes.typeOf[Interval],
          LeanTypes.typeOf[PubKeyHash],
          LeanTypes.typeOf[SortedMap[ByteString, BigInt]]
        )
        val types = LeanTypes.of(all)
        def name[A](tp: scalus.compiler.sir.SIRType): String =
            types.codec(tp).getOrElse(fail(s"no codec of ${tp.show}")).id

        val record = TypedRecord(BigInt(-7), hex("aa"), true, "note", Data.I(1))
        val holding = TypedChoice.Holding(
          prelude.List(record, record.copy(amount = BigInt(8), open = false)),
          SortedMap.fromList(prelude.List((hex("aa"), BigInt(1)), (hex("bb"), BigInt(2))))
        )
        val descending = Data.Constr(
          1,
          prelude.List(
            Data.List(prelude.List.Nil),
            Data.Map(prelude.List((Data.B(hex("bb")), Data.I(2)), (Data.B(hex("aa")), Data.I(1))))
          )
        )
        val rose = EncodingRose(1, prelude.List(EncodingRose(2, prelude.List.Nil)))
        val context = name(LeanTypes.typeOf[ScriptContext])
        val choice = name(LeanTypes.typeOf[TypedChoice])
        val option = name(LeanTypes.typeOf[prelude.Option[BigInt]])
        val hash = name(LeanTypes.typeOf[PubKeyHash])
        val interval = name(LeanTypes.typeOf[Interval])
        assert(context == "ScriptContext" && choice == "TypedChoice")
        assert(option == "Option_Integer")

        /** `data` is read as a well-formed value of the type `id`, whose `Data` it is. */
        def value(id: String, data: Data): List[String] = {
            val d = UplcBlaster.leanData(data)
            List(
              s"example : $id.isEncoding $d = true",
              s"example : (match $id.fromData $d with " +
                  s"| some x => decide ($id.toData x = $d) | none => false) = true"
            )
        }
        def noValue(id: String, data: Data): List[String] =
            List(s"example : $id.isEncoding ${UplcBlaster.leanData(data)} = false")

        val owner = PubKeyHash(hex("11" * 28))
        val examples = (
          value(context, sampleContext.toData) ++
              noValue(context, Data.I(0)) ++
              value(choice, holding.toData) ++
              value(choice, TypedChoice.Empty.toData) ++
              // Its shape is that of a value, and its keys do not ascend.
              noValue(choice, descending) ++
              List(
                s"example : ($choice.fromData ${UplcBlaster.leanData(descending)}).isSome = true"
              ) ++
              value(option, prelude.Option.Some(BigInt(3)).toData) ++
              value(option, (prelude.Option.None: prelude.Option[BigInt]).toData) ++
              noValue(option, Data.Constr(2, prelude.List.Nil)) ++
              value(hash, owner.toData) ++
              noValue(hash, Data.I(1)) ++
              value(interval, Interval.always.toData) ++
              value(interval, Interval.after(BigInt(5)).toData) ++
              // A value written in Lean, by the names of its parts, is the Data Scalus gives.
              List(
                s"example : $hash.toData { hash := ${UplcBlaster.leanData(owner.toData).stripPrefix("(Data.B ").stripSuffix(")")} } = " +
                    UplcBlaster.leanData(owner.toData),
                s"example : $option.toData (.Some 3) = " +
                    UplcBlaster.leanData(prelude.Option.Some(BigInt(3)).toData),
                // A type that contains itself is Data, well formed where it is a value's.
                s"example : ${types.codec(LeanTypes.typeOf[EncodingRose]).get.wellFormed.get(UplcBlaster.leanData(rose.toData))} = true",
                s"example : ${types.codec(LeanTypes.typeOf[EncodingRose]).get.wellFormed.get("(Data.I 0)")} = false"
              )
        ).map(_ + " := by native_decide")

        val text = types.module(module) + "\n" +
            List(
              s"namespace $module",
              "open PlutusCore.Integer (Integer)",
              "open PlutusCore.ByteString (ByteString)",
              "open PlutusCore.Data (Data)"
            ).mkString("\n") + "\n\n" + examples.mkString("\n\n") + s"\n\nend $module\n"
        leanServer.check(text, Some(10.minutes)) match
            case Result.Finished(messages) =>
                val errors = messages.filter(_.severity == Severity.Error)
                assert(errors.isEmpty, errors.map((m: Message) => s"${m.line}: ${m.text}"))
                // The module is to build without a warning, as the workspace it is written to.
                val warnings = messages.filter(_.severity == Severity.Warning)
                assert(warnings.isEmpty, warnings.map((m: Message) => s"${m.line}: ${m.text}"))
            case other => fail(s"Lean did not check the types: $other")
    }
}
