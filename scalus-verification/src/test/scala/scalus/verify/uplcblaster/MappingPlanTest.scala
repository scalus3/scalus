package scalus.verify.uplcblaster

import org.scalatest.funsuite.AnyFunSuite
import scalus.*
import scalus.cardano.onchain.plutus.prelude
import scalus.cardano.onchain.plutus.v1.Value
import scalus.cardano.onchain.plutus.v2.TxOut
import scalus.compiler.Compile
import scalus.verify.*
import scalus.verify.Props.*
import scalus.verify.uplcblaster.MappingPlan.{Mapping, Quantifier, Reason}

/** A class with a function field: it has no `Data` form. */
case class PlanFunction(f: BigInt => BigInt)

@Compile
object PlanFunction

/** The plan of how a statement's variables reach Lean ([[MappingPlan]]): which nodes of a
  * variable's type are decided, how the statement maps them today, and that the plan is what the
  * Lean variables are.
  */
class MappingPlanTest extends AnyFunSuite {
    import MappingPlanTest.given

    private def lowered(prop: Prop): UplcBlaster.Lowered =
        UplcBlaster.lower(prop, FunctionTable.empty).fold(reason => fail(reason), identity)

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
                // The nodes of each tell the two apart.
                assert(both.plan.nodes.map(_.variable) == List(0, 0, 1), both.plan.show)
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
}

object MappingPlanTest {

    /** `Value`'s constructor is private, so its instance is not derived: it is a map, one Lean
      * variable.
      */
    given Quantifiable[Value] = Quantifiable.marker
}
