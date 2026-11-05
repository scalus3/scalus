package scalus.verify.uplcblaster

import org.scalatest.funsuite.AnyFunSuite
import scalus.*
import scalus.cardano.onchain.plutus.prelude
import scalus.cardano.onchain.plutus.v1.Value
import scalus.cardano.onchain.plutus.v2.TxOut
import scalus.compiler.Compile
import scalus.uplc.builtin.{FromData, ToData}
import scalus.verify.*
import scalus.verify.Props.*
import scalus.verify.uplcblaster.MappingPlan.{Mapping, Quantifier, Reason}

/** A class with a function field: it has no `Data` form. */
case class PlanFunction(f: BigInt => BigInt)

@Compile
object PlanFunction

/** A class with a string field: its encoding is not exact, as the UTF-8 of the string is not
  * checked by its premise, while its fields are.
  */
case class PlanNamed(name: String, amount: BigInt) derives FromData, ToData

@Compile
object PlanNamed

/** A class whose type argument is not the type of a field. */
case class PlanPhantom[A](n: BigInt)

@Compile
object PlanPhantom

/** A class with a field of a phantom function: the compiler gives it no `Data` form, though no part
  * of its values is a function.
  */
case class PlanPhantomFunction(p: PlanPhantom[BigInt => BigInt], m: BigInt)

@Compile
object PlanPhantomFunction

/** A tree of names: it contains itself, and its strings are not told exactly as `Data`. */
enum PlanNamedTree derives FromData, ToData:
    case Leaf(name: String)
    case Node(left: PlanNamedTree, right: PlanNamedTree)

@Compile
object PlanNamedTree

/** A tree with a function in its leaves: it contains itself, and has no `Data` form. */
enum PlanFunctionTree:
    case Node(left: PlanFunctionTree, right: PlanFunctionTree)
    case Leaf(f: BigInt => BigInt)

@Compile
object PlanFunctionTree

/** The plan of how a statement's variables reach Lean ([[MappingPlan]]): which nodes of a
  * variable's type are decided, how the statement maps them today, and that the plan is what the
  * Lean variables are.
  */
class MappingPlanTest extends AnyFunSuite {
    import MappingPlanTest.given

    private def lowered(prop: Prop): UplcBlaster.Lowered =
        UplcBlaster.lower(prop, FunctionTable.empty).fold(reason => fail(reason), identity)

    /** `prop` lowered with the nodes at `paths` mapped as `Data` ([[MappingPlan.Fixed]]). */
    private def asData(prop: Prop, paths: String*): Either[String, UplcBlaster.Lowered] =
        UplcBlaster.lower(prop, FunctionTable.empty, MappingPlan.Fixed.asData(paths*))

    private def loweredAsData(prop: Prop, paths: String*): UplcBlaster.Lowered =
        asData(prop, paths*).fold(reason => fail(reason), identity)

    /** The binders' names, as the statement writes them. */
    private def names(lowered: UplcBlaster.Lowered): List[String] =
        lowered.binders.map(binder => PropIdentity.written(binder.name))

    /** The plan's nodes, with each variable named as the statement writes it. */
    private def written(plan: MappingPlan): List[(String, Mapping)] =
        plan.nodes.map(node => PropIdentity.written(node.path) -> node.mapping)

    test("a forAll variable of a type without case classes or enums has no decided node") {
        assert(lowered(forAll[BigInt](x => x == x)).plan.nodes.isEmpty)
        assert(
          lowered(forAll[prelude.List[BigInt]](xs => xs.isEmpty || !xs.isEmpty)).plan.nodes.isEmpty
        )
    }

    test("a forAll variable is mapped by fields, at every case class and enum of its type") {
        val owned = lowered(forAll[BlasterOwned](o => o.amount == o.amount))
        assert(written(owned.plan) == List("o" -> Mapping.ByFields, "o.owner" -> Mapping.ByFields))
        assert(
          owned.binders.map(binder => PropIdentity.written(binder.name)) ==
              List("o.owner.key", "o.amount")
        )
        assert(
          owned.plan.nodes.map(_.reason).distinct == List(Reason.Quantified(Quantifier.ForAll)),
          owned.plan.show
        )
        assert(owned.plan.show.linesIterator.next().startsWith("o: "), owned.plan.show)
        // An enum is decided; its constructors' fields here are integers, which are not.
        assert(
          written(lowered(forAll[BlasterShape](s => denotes(s))).plan) ==
              List("s" -> Mapping.ByFields)
        )
        // So is a variable of an exists, and its reason says so.
        val pair = lowered(exists[BlasterPair](p => p.a == p.b))
        assert(written(pair.plan) == List("p" -> Mapping.ByFields))
        assert(pair.plan.nodes.map(_.reason) == List(Reason.Quantified(Quantifier.Exists)))
    }

    test("a variable over Data encodings is one Data node, whatever its type") {
        val owned = lowered(forAllData[BlasterOwned](o => o.amount == o.amount))
        assert(written(owned.plan) == List("o" -> Mapping.AsData))
        assert(owned.binders.size == 1, owned.binders)
        assert(owned.plan.nodes.map(_.reason) == List(Reason.OverEncodings(Quantifier.ForAll)))
        // A parameter written with @UplcRepr(UplcRepresentation.Data) is one too.
        assert(
          written(lowered(forAll((p: BlasterPairData) => p.a == p.a)).plan) ==
              List("p" -> Mapping.AsData)
        )
        // And a variable with limits, of a list, which is no case class: the limits are why.
        val limited = lowered(
          forAllDataLimited[prelude.List[BigInt]](xs => xs.length <= 2)(xs =>
              xs.length >= BigInt(0)
          )
        )
        assert(written(limited.plan) == List("xs" -> Mapping.AsData))
        assert(limited.plan.nodes.map(_.reason) == List(Reason.Limited))
        assert(limited.binders.size == 1, limited.binders)
    }

    test("two variables of one name are mapped each as its own quantifier says") {
        // One inline helper used twice can give two variables one name. Here one is over the Data
        // encodings of BlasterOwned, the other by its fields, under the same name.
        val byFields = forAll[BlasterOwned](x => true)
        forAllData[BlasterOwned](x => true) match
            case Prop.Forall(encoded, body) =>
                val plain = byFields match
                    case Prop.Forall(ident, _) => ident
                    case other                 => fail(s"no quantifier: $other")
                val renamed = forAllSIR(encoded.copy(name = plain.name), body)
                val both = lowered(Prop.And(byFields, renamed))
                assert(
                  written(both.plan) == List(
                    "x" -> Mapping.ByFields,
                    "x.owner" -> Mapping.ByFields,
                    "x" -> Mapping.AsData
                  ),
                  both.plan.show
                )
                // The nodes of each tell the two apart, and so does the plan as it is shown.
                assert(both.plan.nodes.map(_.variable) == List(0, 0, 1), both.plan.show)
                assert(
                  both.plan.show.linesIterator.map(_.takeWhile(_ != ':')).toList ==
                      List("x#1", "x#1.owner", "x#2"),
                  both.plan.show
                )
                assert(both.binders.size == 3, both.binders)
            case other => fail(s"no quantifier: $other")
    }

    test("the variable of an existsLet is none: the body is applied to the witness") {
        val witnessed = forAll[BigInt](x => existsLet(BlasterPair(x, x))(p => p.a == x))
        assert(lowered(witnessed).plan.nodes.isEmpty)
    }

    test("a class with a function field is refused at that field, as before") {
        val refused = UplcBlaster.lower(
          forAll[prelude.Option[PlanFunction]](o => denotes(o)),
          FunctionTable.empty
        )
        // The refusal of the field's type, a function: not of the class, nor an exception.
        assert(
          refused.left.exists(reason =>
              reason.contains("binder is not supported, only BigInt, Boolean, ByteString")
          ),
          refused
        )
    }

    test("of a TxOut, every case class and enum is a decided node, and a list or a map is none") {
        val out = lowered(forAll[TxOut](out => denotes(out)))
        val cred = "out.address.stakingCredential.Some.value.StakingHash.cred"
        assert(
          written(out.plan) == List(
            "out" -> Mapping.ByFields,
            "out.address" -> Mapping.ByFields,
            "out.address.credential" -> Mapping.ByFields,
            // A PubKeyHash, a class of one field, is a node too.
            "out.address.credential.PubKeyCredential.hash" -> Mapping.ByFields,
            "out.address.stakingCredential" -> Mapping.ByFields,
            "out.address.stakingCredential.Some.value" -> Mapping.ByFields,
            cred -> Mapping.ByFields,
            s"$cred.PubKeyCredential.hash" -> Mapping.ByFields,
            "out.datum" -> Mapping.ByFields,
            "out.referenceScript" -> Mapping.ByFields
          ),
          out.plan.show
        )
        // By fields, the 17 Lean variables of docs/design/verification-details/uplc-blaster.md,
        // "Mapping a variable".
        val staking = "out.address.stakingCredential"
        assert(
          out.binders.map(binder => PropIdentity.written(binder.name)) == List(
            "out.address.credential.case",
            "out.address.credential.PubKeyCredential.hash.hash",
            "out.address.credential.ScriptCredential.hash",
            s"$staking.case",
            s"$staking.Some.value.case",
            s"$staking.Some.value.StakingHash.cred.case",
            s"$staking.Some.value.StakingHash.cred.PubKeyCredential.hash.hash",
            s"$staking.Some.value.StakingHash.cred.ScriptCredential.hash",
            s"$staking.Some.value.StakingPtr.a",
            s"$staking.Some.value.StakingPtr.b",
            s"$staking.Some.value.StakingPtr.c",
            "out.value",
            "out.datum.case",
            "out.datum.OutputDatumHash.datumHash",
            "out.datum.OutputDatum.datum",
            "out.referenceScript.case",
            "out.referenceScript.Some.value"
          )
        )
    }

    test("a fixed plan maps a node below the root as one Data variable, named by its path") {
        // The plan between the two of docs/design/verification-details/uplc-blaster.md, "Nodes":
        // the address by fields, and its staking credential as Data, in 10 Lean variables.
        val staking = "out.address.stakingCredential"
        val out = loweredAsData(forAll[TxOut](out => denotes(out)), staking)
        assert(
          names(out) == List(
            "out.address.credential.case",
            "out.address.credential.PubKeyCredential.hash.hash",
            "out.address.credential.ScriptCredential.hash",
            staking,
            "out.value",
            "out.datum.case",
            "out.datum.OutputDatumHash.datumHash",
            "out.datum.OutputDatum.datum",
            "out.referenceScript.case",
            "out.referenceScript.Some.value"
          )
        )
        // The node is in the plan, as Data, for the plan's reason; nothing below it is.
        assert(
          written(out.plan) == List(
            "out" -> Mapping.ByFields,
            "out.address" -> Mapping.ByFields,
            "out.address.credential" -> Mapping.ByFields,
            "out.address.credential.PubKeyCredential.hash" -> Mapping.ByFields,
            staking -> Mapping.AsData,
            "out.datum" -> Mapping.ByFields,
            "out.referenceScript" -> Mapping.ByFields
          ),
          out.plan.show
        )
        assert(
          out.plan.nodes.map(_.reason).distinct ==
              List(Reason.Quantified(Quantifier.ForAll), Reason.ByFixedPlan)
        )
        assert(
          out.plan.nodes.find(_.mapping == Mapping.AsData).forall(_.reason == Reason.ByFixedPlan)
        )
        // The Data variable is restricted to the encodings of an Option[StakingCredential], and
        // the map of the Value by its own premise, as the doc's p3 and p4.
        val premises = out.predicates.zipWithIndex.collect { case (Some(_), at) => names(out)(at) }
        assert(premises == Vector(staking, "out.value"), out.predicates)
        assert(out.predicates(3).exists(_.contains("Data.Constr")), out.predicates(3))
        // Each test decodes it where it builds the value.
        val (variable, decoder) = DataEncoding
            .encoded(out.binders(3).tp)
            .getOrElse(fail(s"not a Data variable: ${out.binders(3).tp.show}"))
        assert(variable.show.contains("StakingCredential"), variable.show)
        assert(decoder.pretty.render(100).contains("UniversalDataConversion$.fromData"))
    }

    test("a fixed plan can map the root of a forAll variable as Data") {
        val owned = loweredAsData(forAll[BlasterOwned](o => o.amount == o.amount), "o")
        assert(names(owned) == List("o"))
        assert(written(owned.plan) == List("o" -> Mapping.AsData))
        assert(owned.plan.nodes.map(_.reason) == List(Reason.ByFixedPlan))
        // A node the plan names that the statement does not have is an error, not a plan by fields.
        asData(forAll[BlasterOwned](o => o.amount == o.amount), "o.amount", "o.ownr") match
            case Left(reason) =>
                assert(reason.startsWith("the fixed plan names o.amount, o.ownr, which"), reason)
            case Right(lowered) => fail(s"expected a refusal, got ${lowered.plan.show}")
        // So is one inside a node it maps as Data, which it is not asked about.
        asData(forAll[BlasterOwned](o => true), "o", "o.owner") match
            case Left(reason) =>
                assert(reason.startsWith("the fixed plan names o.owner, which"), reason)
                assert(reason.contains("inside a node that is one Data variable"), reason)
            case Right(lowered) => fail(s"expected a refusal, got ${lowered.plan.show}")
    }

    test(
      "a Data node whose encoding is not exact is a variable only where it stands for every value"
    ) {
        // As Data, a string is a byte string whose UTF-8 the premise does not check: the variable
        // has more values than PlanNamed. A forAll in a positive position stands for every value.
        assert(asData(forAll[PlanNamed](n => n.amount == n.amount), "n").isRight)
        // An exists claims one, which could be none of the type's.
        asData(exists[PlanNamed](n => n.amount == BigInt(1)), "n") match
            case Left(reason)   => assert(reason.contains("stands for every value"), reason)
            case Right(lowered) => fail(s"expected a refusal, got ${lowered.plan.show}")
        // By fields, the string is a Lean String, but the test builds the value in its Data
        // representation, which encodes the string with encodeUtf8: that is refused as well
        // (input-output-hk/Lean-blaster#389), whichever the quantifier.
        val byFields =
            UplcBlaster.lower(exists[PlanNamed](n => n.amount == BigInt(1)), FunctionTable.empty)
        assert(byFields.left.exists(_.contains("encodeUtf8")), byFields)
    }

    test("a node with a part that has no Data form cannot be Data") {
        // A value of PlanFunction is no decoding of any Data: its field is a function.
        val refused = asData(forAll[prelude.Option[PlanFunction]](o => denotes(o)), "o.Some.value")
        assert(
          refused.left.exists(reason =>
              reason.startsWith("o.Some.value: a ") && reason.contains("has no Data form")
          ),
          refused
        )
        // So is the Option around it, at the root.
        assert(
          asData(forAll[prelude.Option[PlanFunction]](o => denotes(o)), "o").left
              .exists(_.contains("has no Data form"))
        )
        // A class whose type argument is a function, but none of its fields, has a Data form: its
        // values are their integers. The compiler gives none to a class with a field of it, as it
        // looks into the field's type arguments: so a node of that class is refused, as the
        // compiler says, and its representation is not asked for.
        val phantom = forAll[PlanPhantomFunction](c => c.m == c.m)
        assert(UplcBlaster.lower(phantom, FunctionTable.empty).isRight)
        assert(asData(phantom, "c.p").isRight)
        val refusedPhantom = asData(phantom, "c")
        assert(refusedPhantom.left.exists(_.contains("has no Data form")), refusedPhantom)
    }

    test("rule 1: a type that contains itself is Data where it repeats") {
        val tree = forAll[BlasterTree](t => denotes(t))
        val cut = lowered(tree)
        assert(names(cut) == List("t.case", "t.Node.left", "t.Node.right"))
        assert(
          written(cut.plan) == List(
            "t" -> Mapping.ByFields,
            "t.Node.left" -> Mapping.AsData,
            "t.Node.right" -> Mapping.AsData
          ),
          cut.plan.show
        )
        assert(
          cut.plan.nodes.map(_.reason) == List(
            Reason.Quantified(Quantifier.ForAll),
            Reason.ContainsItself,
            Reason.ContainsItself
          ),
          cut.plan.show
        )
        // A fixed plan of the same nodes is the same check: the reasons are not in its text.
        val fixed = loweredAsData(tree, "t.Node.left", "t.Node.right")
        assert(fixed.binders == cut.binders)
        assert(fixed.predicates == cut.predicates)
        assert(fixed.programHashes == cut.programHashes)
        // A forAll in a positive position stands for every value, also in a conclusion, and so
        // does an exists in a negative one.
        assert(UplcBlaster.lower(Prop(true) ==> tree, FunctionTable.empty).isRight)
        assert(
          UplcBlaster.lower(!exists[BlasterTree](t => denotes(t)), FunctionTable.empty).isRight
        )
    }

    test("a type that contains itself is a variable also where it asks for a witness") {
        // Its premise is a recursive predicate, exact: a witness Lean finds is a tree.
        val tree = forAll[BlasterTree](t => denotes(t))
        val asking = List(
          exists[BlasterTree](t => denotes(t)),
          !tree,
          tree ==> Prop(true),
          tree <=> Prop(true),
          Prop(true) <=> exists[BlasterTree](t => denotes(t))
        )
        for prop <- asking do
            val lowering = UplcBlaster.lower(prop, FunctionTable.empty)
            assert(lowering.isRight, lowering)
        val cut = lowered(exists[BlasterTree](t => denotes(t)))
        val nodes = names(cut).zip(cut.predicates)
        assert(
          nodes.collect { case (name, Some(premise)) =>
              name -> premise.startsWith("scalusRec_")
          } ==
              List("t.Node.left" -> true, "t.Node.right" -> true),
          cut.predicates
        )
        // A part that is not told exactly, as a string, still makes it more values than its type.
        assert(
          UplcBlaster
              .lower(exists[PlanNamedTree](t => denotes(t)), FunctionTable.empty)
              .left
              .exists(_.contains("stands for every value"))
        )
    }

    test("a list or a map whose type contains itself through it has the recursive premise") {
        // The children of a rose are a list of roses: by fields, `r.children` is one Lean variable
        // of a list, whose type repeats at its items. Its premise is the recursive predicate of
        // the list's Data, so an exists over it is sound.
        val rose = lowered(exists[EncodingRose](r => r.children.isEmpty))
        val premises = names(rose).zip(rose.predicates).toMap
        assert(
          premises("r.children").exists(p =>
              p.contains("scalusRec_") && p.contains("Data.List xs")
          ),
          rose.predicates
        )
        assert(rose.recursions.nonEmpty)
        // And a map, the files of a directory, with its keys in order.
        val dir = lowered(exists[EncodingDir](d => d.files.isEmpty))
        assert(
          names(dir).zip(dir.predicates).toMap.apply("d.files").exists(_.contains("Data.Map xs")),
          dir.predicates
        )
        assert(dir.recursions.exists(_.contains("scalusAscendingBytes")), dir.recursions)
    }

    test("rule 1: a type that contains itself and has no Data form is refused") {
        UplcBlaster.lower(forAll[PlanFunctionTree](t => denotes(t)), FunctionTable.empty) match
            case Left(reason) =>
                assert(reason.startsWith("t.Node.left: a "), reason)
                assert(reason.contains("contains itself"), reason)
                assert(reason.contains("has no Data form"), reason)
            case Right(lowered) => fail(s"expected a refusal, got ${lowered.plan.show}")
    }

    test("a fixed plan tells two variables of one name apart by their places") {
        val byFields = forAll[BlasterOwned](x => true)
        val both = Prop.And(byFields, byFields)
        // The second variable's owner, the first's by fields.
        val second = new MappingPlan.Fixed(
          Set("x.owner"),
          (variable, path, _) => Option.when(variable == 1 && path == "x.owner")(Mapping.AsData)
        )
        val lowered = UplcBlaster
            .lower(both, FunctionTable.empty, second)
            .fold(reason => fail(reason), identity)
        assert(
          lowered.plan.nodes.map(node => node.variable -> node.mapping) == List(
            0 -> Mapping.ByFields,
            0 -> Mapping.ByFields,
            1 -> Mapping.ByFields,
            1 -> Mapping.AsData
          ),
          lowered.plan.show
        )
        assert(names(lowered) == List("x.owner.key", "x.amount", "x.owner", "x.amount"))
    }
}

object MappingPlanTest {

    /** `Value`'s constructor is private, so its instance is not derived: it is a map, one Lean
      * variable.
      */
    given Quantifiable[Value] = Quantifiable.marker
}
