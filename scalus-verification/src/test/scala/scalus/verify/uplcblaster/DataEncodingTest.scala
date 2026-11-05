package scalus.verify.uplcblaster

import org.scalatest.funsuite.AnyFunSuite
import scalus.cardano.onchain.plutus.prelude
import scalus.cardano.onchain.plutus.prelude.SortedMap
import scalus.cardano.onchain.plutus.v1.{Address, Credential, PubKeyHash, Value}
import scalus.cardano.onchain.plutus.v2.{OutputDatum, TxOut}
import scalus.cardano.onchain.plutus.v3.{ScriptContext, ScriptInfo, TxId, TxInInfo, TxInfo, TxOutRef}
import scalus.compiler.Compile
import scalus.compiler.sir.SIRType
import scalus.uplc.Constant
import scalus.uplc.builtin.{ByteString, Data, FromData, ToData}
import scalus.uplc.builtin.Data.toData
import scalus.verify.{DataEncoding, DataLayout, FunctionTable, Limit, Limits, Prop, SirTypes}
import scalus.verify.Props.*
import scalus.verify.lean.LeanServer.{Message, Result, Severity}

import scala.concurrent.duration.*
import scala.util.{Random, Try}

/** A type that contains itself through a list. */
case class EncodingRose(label: BigInt, children: prelude.List[EncodingRose])
    derives FromData,
      ToData

@Compile
object EncodingRose

/** A type that contains itself through the values of a map. */
case class EncodingDir(files: SortedMap[ByteString, EncodingDir]) derives FromData, ToData

@Compile
object EncodingDir

/** Two types that contain each other. */
enum EncodingChain derives FromData, ToData:
    case End
    case Link(next: EncodingStep)

@Compile
object EncodingChain

case class EncodingStep(n: BigInt, rest: EncodingChain) derives FromData, ToData

@Compile
object EncodingStep

/** A tree whose children are a forest, which contains the tree: each repeats the other. */
case class EncodingTree(label: BigInt, children: EncodingForest) derives FromData, ToData

@Compile
object EncodingTree

enum EncodingForest derives FromData, ToData:
    case Empty
    case Grove(tree: EncodingTree, rest: EncodingForest)

@Compile
object EncodingForest

/** A case class of two fields, to nest a type deep. */
case class EncodingWrap[A](a: A, n: BigInt)

@Compile
object EncodingWrap

type EncodingWrap4[A] = EncodingWrap[EncodingWrap[EncodingWrap[EncodingWrap[A]]]]
type EncodingWrap16[A] = EncodingWrap4[EncodingWrap4[EncodingWrap4[EncodingWrap4[A]]]]

/** A type that contains itself, beside a part 16 types deep. */
case class EncodingDeep(next: prelude.Option[EncodingDeep], extra: EncodingWrap16[BigInt])

@Compile
object EncodingDeep

/** A type with a field of an enum that holds the type, one of whose constructors has a type
  * argument its enum does not pass on: an enum whose constructors are not told.
  */
case class EncodingHolder(held: EncodingHeld[BigInt], n: BigInt)

@Compile
object EncodingHolder

enum EncodingHeld[A]:
    case Holds(holder: EncodingHolder)
    case Other[B](b: B) extends EncodingHeld[BigInt]

@Compile
object EncodingHeld

/** Two types that contain each other, where the smallest part needs the type around it: its owner.
  */
enum EncodingOwner derives FromData, ToData:
    case Owns(part: EncodingPart)
    case Nobody

@Compile
object EncodingOwner

case class EncodingPart(parts: prelude.List[EncodingPart], owner: EncodingOwner)
    derives FromData,
      ToData

@Compile
object EncodingPart

class DataEncodingTest extends AnyFunSuite with LeanProofsSuite {
    import proofs.*
    import DataEncodingTest.*

    test("the encoding of a type is the Data that its values are") {
        assert(DataEncoding.of[BigInt] == DataEncoding.Integer)
        assert(DataEncoding.of[ByteString] == DataEncoding.Bytes)
        assert(DataEncoding.of[Boolean] == DataEncoding.Bool)
        assert(DataEncoding.of[Data] == DataEncoding.AnyData)
        // A type that wraps one field is that field.
        assert(DataEncoding.of[PubKeyHash] == DataEncoding.Bytes)
        // A wrapper of a list is its list's encoding: the compiler is asked of the list's type
        // whether its items have a Data form.
        assert(
          DataEncoding.of[prelude.Varargs[BigInt]] == DataEncoding.Items(DataEncoding.Integer)
        )
        assert(DataEncoding.of[prelude.List[BigInt]] == DataEncoding.Items(DataEncoding.Integer))
        val outRef = DataEncoding.of[TxOutRef]
        assert(outRef.exact, outRef)
        assert(outRef.outside(sampleOutRef.toData).isEmpty)
        assert(outRef.outside(Data.Constr(0, prelude.List(Data.I(1), Data.I(1)))).nonEmpty)
        // An enum is a constructor of each of its cases.
        val option = DataEncoding.of[prelude.Option[BigInt]]
        assert(option.outside(prelude.Option.Some(BigInt(5)).toData).isEmpty)
        assert(option.outside((prelude.Option.None: prelude.Option[BigInt]).toData).isEmpty)
        assert(option.outside(Data.Constr(2, prelude.List.Nil)).nonEmpty)
    }

    test("a type cut off where it nests too deep has a name of its own, and so its own premise") {
        def names(encoding: DataEncoding): Set[String] = encoding match
            case DataEncoding.Recursive(name, body)    => names(body) + name
            case DataEncoding.Constr(constructors)     => constructors.flatten.flatMap(names).toSet
            case DataEncoding.Items(items)             => names(items)
            case DataEncoding.Entries(keys, values, _) => names(keys) ++ names(values)
            case _                                     => Set.empty
        val deep = DataEncoding.of[EncodingDeep]
        assert(deep.exact, deep)
        // Under 16 more types, the part 16 deep in it is cut off, and is any Data.
        val nested = DataEncoding.of[EncodingWrap16[EncodingDeep]]
        assert(!nested.exact)
        assert(names(deep).size == 1, names(deep))
        val name = names(deep).head
        assert(names(nested) == Set(s"${name}_cut"), names(nested))
        // Whichever is met first, each has its definition, the exact one of the exact layout.
        val recursions = DataEncoding.leanRecursions(List(nested, deep)).mkString("\n")
        assert(recursions.contains(s"def scalusRec_$name ("), recursions)
        assert(recursions.contains(s"def scalusRec_${name}_cut ("), recursions)
    }

    test("the largest value within limits has a part whose smallest value needs the type around") {
        val owner = UplcBlaster
            .lower(forAllData[EncodingOwner](_ => true), FunctionTable.empty)
            .fold(
              reason => fail(reason),
              lowered => DataEncoding.valueType(lowered.binders.head.tp)
            )
        // A part of one part, each owned by nobody: the smallest owner, a constructor without the
        // part again, ends it.
        val smallest = EncodingPart(prelude.List.Nil, EncodingOwner.Nobody)
        val largest = EncodingOwner.Owns(EncodingPart(prelude.List(smallest), EncodingOwner.Nobody))
        assert(
          Limits.of(Limit.MaxCollectionLength(1)).largest(DataLayout.of(owner)) ==
              Some(largest.toData)
        )
    }

    test("a type whose repeat is in a part not told does not contain itself") {
        val holder = DataEncoding.of[EncodingHolder]
        assert(!holder.isInstanceOf[DataEncoding.Recursive], holder)
        assert(holder.leanRecursions.isEmpty, holder.leanRecursions)
    }

    test("a value as deep as a type that contains itself is checked in a loop, not on the stack") {
        // A chain of 100000 links, which a check by recursion would need far more stack for.
        val deep = (1 to 100000).foldLeft(Data.Constr(0, prelude.List.Nil): Data) { (rest, n) =>
            Data.Constr(1, prelude.List(Data.Constr(0, prelude.List(Data.I(n), rest))))
        }
        val tp = UplcBlaster
            .lower(forAllData[EncodingChain](_ => true), FunctionTable.empty)
            .fold(
              reason => fail(reason),
              lowered => DataEncoding.valueType(lowered.binders.head.tp)
            )
        def onSmallStack[A](body: => A): A = {
            var result: Option[Either[Throwable, A]] = None
            val thread = new Thread(
              null,
              () =>
                  result = Some(
                    try Right(body)
                    catch case e: Throwable => Left(e)
                  ),
              "small-stack",
              1L << 20
            )
            thread.start()
            thread.join()
            result.get.fold(e => fail(s"the check failed with $e"), identity)
        }
        assert(onSmallStack(DataEncoding.of[EncodingChain].outside(deep)).isEmpty)
        assert(onSmallStack(Limits.of(Limit.MaxCollectionLength(1)).outside(deep, tp)).isEmpty)
        // A rose of one child each, 100000 deep, repeats through a field and a list, and is
        // checked in time proportional to its depth: no path is carried down a repeat.
        val rose = (1 to 100000).foldLeft(EncodingRose(0, prelude.List.Nil).toData) { (child, n) =>
            Data.Constr(0, prelude.List(Data.I(n), Data.List(prelude.List(child))))
        }
        val roseType = UplcBlaster
            .lower(forAllData[EncodingRose](_ => true), FunctionTable.empty)
            .fold(
              reason => fail(reason),
              lowered => DataEncoding.valueType(lowered.binders.head.tp)
            )
        val started = System.nanoTime()
        assert(
          onSmallStack(Limits.of(Limit.MaxCollectionLength(1)).outside(rose, roseType)).isEmpty
        )
        assert((System.nanoTime() - started) / 1e9 < 30, "the check of the rose's lengths")
        // And a part deep down that is not of the type is told.
        val broken = Data.Constr(
          1,
          prelude.List(Data.Constr(0, prelude.List(Data.B(ByteString.empty), deep)))
        )
        assert(
          onSmallStack(DataEncoding.of[EncodingChain].outside(broken))
              .exists(_.contains("is no integer"))
        )
    }

    test("a type that contains itself is a recursive encoding, exact, checked to any depth") {
        val tree = DataEncoding.of[BlasterTree]
        tree match
            case DataEncoding.Recursive(name, DataEncoding.Constr(constructors)) =>
                assert(
                  constructors == List(
                    Nil,
                    List(DataEncoding.Again(name), DataEncoding.Again(name))
                  ),
                  tree
                )
            case other => fail(s"expected a recursive encoding, got $other")
        assert(tree.exact, tree)
        // The value Lean leaves out is the smallest, a constructor without the type again.
        assert(tree.example.contains(BlasterTree.Leaf.toData))
        val leaf = BlasterTree.Leaf
        assert(tree.outside(BlasterTree.Node(BlasterTree.Node(leaf, leaf), leaf).toData).isEmpty)
        // Two levels below the root, an integer is no tree.
        val deep = Data.Constr(
          1,
          prelude.List(Data.Constr(1, prelude.List(Data.I(0), leaf.toData)), leaf.toData)
        )
        assert(tree.outside(deep).nonEmpty, deep)
        // Through a list, through a map's values, and two types that contain each other.
        val rose = DataEncoding.of[EncodingRose]
        assert(rose.exact, rose)
        val sapling = EncodingRose(1, prelude.List(EncodingRose(2, prelude.List.Nil)))
        assert(rose.outside(sapling.toData).isEmpty)
        assert(rose.outside(EncodingRose(1, prelude.List.Nil).toData).isEmpty)
        val withLeafInt =
            Data.Constr(0, prelude.List(Data.I(1), Data.List(prelude.List(Data.I(2)))))
        assert(rose.outside(withLeafInt).nonEmpty)
        val dir = DataEncoding.of[EncodingDir]
        assert(dir.exact, dir)
        val home = EncodingDir(SortedMap.singleton(hex("aa"), EncodingDir(SortedMap.empty)))
        assert(dir.outside(home.toData).isEmpty)
        val chain = DataEncoding.of[EncodingChain]
        assert(chain.exact, chain)
        val two = EncodingChain.Link(
          EncodingStep(1, EncodingChain.Link(EncodingStep(2, EncodingChain.End)))
        )
        assert(chain.outside(two.toData).isEmpty)
        // An encoding around a type that contains itself uses its definition, written once.
        val optionalTree = DataEncoding.of[prelude.Option[BlasterTree]]
        assert(optionalTree.exact, optionalTree)
        assert(optionalTree.leanRecursions.size == 1, optionalTree.leanRecursions)
        assert(optionalTree.leanRecursions == tree.leanRecursions)
        // A type without one has none.
        assert(DataEncoding.of[TxOutRef].leanRecursions.isEmpty)
        // Types that repeat each other are defined once each, in one block, whichever is met first.
        val bothWays = DataEncoding
            .leanRecursions(List(DataEncoding.of[EncodingTree], DataEncoding.of[EncodingForest]))
            .mkString("\n")
        for name <- List("EncodingTree", "EncodingForest") do
            assert(s"def scalusRec_${name}_".r.findAllIn(bothWays).size == 1, bothWays)
        // A repeat is told by what the type is made of, through proxies, which all show as Proxy.
        def typeOf(prop: Prop): SIRType = UplcBlaster
            .lower(prop, FunctionTable.empty)
            .fold(
              reason => fail(reason),
              lowered => DataEncoding.valueType(lowered.binders.head.tp)
            )
        val pairs = SIRType.TypeProxy(typeOf(forAllData[BlasterPair](_ => true)))
        val owners = SIRType.TypeProxy(typeOf(forAllData[BlasterOwner](_ => true)))
        assert(pairs.show == owners.show)
        assert(SirTypes.recursionKey(pairs) != SirTypes.recursionKey(owners))
        // A Lean name has the ASCII letters and digits of the type's name.
        assert(DataLayout.leanSimpleName("scalus.examples.Tree$1") == "Tree1")
        assert(
          DataLayout.leanSimpleName("scalus.examples.\u0414\u0435\u0440\u0435\u0432\u043e") == "T"
        )
    }

    test("a limit of every collection reaches every level of a type that contains itself") {
        val limited = UplcBlaster
            .lower(
              forAllDataLimited[EncodingRose](r => maxCollectionLength(r) <= 1)(r =>
                  r.label == r.label
              ),
              FunctionTable.empty
            )
            .fold(reason => fail(reason), identity)
        val binder = limited.binders.head
        def outside(rose: EncodingRose) =
            UplcBlaster.outsideType(Constant.Data(rose.toData), binder.tp)
        val leaf = (n: BigInt) => EncodingRose(n, prelude.List.Nil)
        assert(
          outside(EncodingRose(1, prelude.List(EncodingRose(2, prelude.List(leaf(3)))))).isEmpty
        )
        // Two grandchildren, below where the type repeats: outside the limit, on the JVM.
        val bushy = EncodingRose(1, prelude.List(EncodingRose(2, prelude.List(leaf(3), leaf(4)))))
        assert(outside(bushy).nonEmpty)
        // The guard does not walk into a repeat, so it checks less than the limit says: the
        // variable is one only where it stands for every value.
        val negated = UplcBlaster.lower(
          !forAllDataLimited[EncodingRose](r => maxCollectionLength(r) <= 1)(r =>
              r.label == r.label
          ),
          FunctionTable.empty
        )
        assert(negated.left.exists(_.contains("stands for every value")), negated)
        // The largest value within the limit, where the search starts, ends where the type repeats.
        val valueType = DataEncoding.valueType(binder.tp)
        assert(DataEncoding.limits(binder.tp).largest(DataLayout.of(valueType)).isDefined)
    }

    test("the encoding agrees with FromData and ToData, on Data near the type's and far from it") {
        // FromData reads what it needs and checks no more, so a value it reads is one of the
        // type's where ToData gives the Data back. Every such Data is one the premise admits, and
        // every Data it admits is one: the premise holds of exactly the encodings of the values.
        // FromData of a SortedMap checks neither the order of its keys nor that they differ, so
        // it reads Data that is no SortedMap's: of a type with one, only the second is held.
        val random = new Random(20261104)
        agrees[BlasterTree](DataEncoding.of[BlasterTree], random, laxFromData = false)
        agrees[EncodingRose](DataEncoding.of[EncodingRose], random, laxFromData = false)
        agrees[EncodingChain](DataEncoding.of[EncodingChain], random, laxFromData = false)
        agrees[BlasterShape](DataEncoding.of[BlasterShape], random, laxFromData = false)
        agrees[prelude.Option[BlasterTree]](
          DataEncoding.of[prelude.Option[BlasterTree]],
          random,
          laxFromData = false
        )
        agrees[TxOutRef](DataEncoding.of[TxOutRef], random, laxFromData = false)
        agrees[EncodingDir](DataEncoding.of[EncodingDir], random, laxFromData = true)
    }

    test("the encoding of a ledger's script context holds of a script context") {
        val encoding = DataEncoding.of[ScriptContext]
        assert(encoding.outside(sampleContext.toData).isEmpty)
        assert(encoding.outside(Data.I(0)).nonEmpty)
        // A script info of no constructor of its type.
        val data = sampleContext.toData match
            case Data.Constr(tag, prelude.List.Cons(info, prelude.List.Cons(redeemer, _))) =>
                Data.Constr(tag, prelude.List(info, redeemer, Data.Constr(9, prelude.List.Nil)))
            case other => fail(s"a script context is $other")
        assert(encoding.outside(data).nonEmpty)
        // The keys of its maps of credentials and of voters are in an order that is not checked:
        // one key is in order, two are not told.
        assert(!encoding.exact)
        val withdrawals = SortedMap.fromList(
          prelude.List(
            (Credential.PubKeyCredential(owner), BigInt(3)),
            (Credential.ScriptCredential(policy), BigInt(4))
          )
        )
        val twoWithdrawals =
            sampleContext.copy(txInfo = sampleContext.txInfo.copy(withdrawals = withdrawals))
        assert(
          encoding.outside(twoWithdrawals.toData) ==
              Some("the order of the keys is not checked here")
        )
    }

    test("forAllData of a script context reads the Data the node gives a script") {
        // Its decoder comes with the data declarations of the context's types.
        val fee = forAllData[ScriptContext](ctx => ctx.txInfo.fee == ctx.txInfo.fee)
        UplcBlaster.lower(fee, FunctionTable.empty) match
            case Right(lowered) => assert(lowered.binders.size == 1, lowered.binders)
            case Left(reason)   => fail(reason)
        proven(fee, 600)
    }

    test("the Lean definition of an encoding is Lean's, and holds of the same Data") {
        requireLean()
        // Two of them share the tree, and two types repeat each other.
        val recursive = List(
          "isTree" -> DataEncoding.of[BlasterTree],
          "isOptionalTree" -> DataEncoding.of[prelude.Option[BlasterTree]],
          "isRose" -> DataEncoding.of[EncodingRose],
          "isDir" -> DataEncoding.of[EncodingDir],
          "isChain" -> DataEncoding.of[EncodingChain],
          "isForestTree" -> DataEncoding.of[EncodingTree]
        )
        val definitions = List(
          DataEncoding.leanDefinitions,
          DataEncoding.of[ScriptContext].leanDefinition("isScriptContext"),
          DataEncoding.of[SortedMap[ByteString, BigInt]].leanDefinition("isTokens"),
          // The recursive definitions of all of them, each type's once, which Lean has to see as
          // structural, and then the definitions that use them.
          DataEncoding
              .leanRecursions(recursive.map(_._2))
              .mkString("\n\n")
        ).mkString("\n\n") + "\n\n" +
            recursive.map((name, encoding) => encoding.leanDefinition(name)).mkString("\n\n")
        val leaf = BlasterTree.Leaf
        val deepTree = BlasterTree.Node(BlasterTree.Node(leaf, leaf), leaf).toData
        val notATree = Data.Constr(
          1,
          prelude.List(Data.Constr(1, prelude.List(Data.I(0), leaf.toData)), leaf.toData)
        )
        val rose = EncodingRose(1, prelude.List(EncodingRose(2, prelude.List.Nil))).toData
        val dir = EncodingDir(SortedMap.singleton(hex("aa"), EncodingDir(SortedMap.empty))).toData
        val chain = EncodingChain.Link(EncodingStep(1, EncodingChain.End)).toData
        val brokenChain =
            Data.Constr(1, prelude.List(Data.Constr(0, prelude.List(Data.I(1), Data.I(0)))))
        val forestTree = EncodingTree(
          1,
          EncodingForest.Grove(EncodingTree(2, EncodingForest.Empty), EncodingForest.Empty)
        ).toData
        val ascending = Data.Map(
          prelude.List((Data.B(hex("aa")), Data.I(1)), (Data.B(hex("bb")), Data.I(2)))
        )
        val descending = Data.Map(
          prelude.List((Data.B(hex("bb")), Data.I(2)), (Data.B(hex("aa")), Data.I(1)))
        )
        val examples = List(
          s"example : isScriptContext ${UplcBlaster.leanData(sampleContext.toData)} = true",
          "example : isScriptContext (Data.I 0) = false",
          s"example : isTokens ${UplcBlaster.leanData(ascending)} = true",
          s"example : isTokens ${UplcBlaster.leanData(descending)} = false",
          s"example : isTree ${UplcBlaster.leanData(deepTree)} = true",
          s"example : isTree ${UplcBlaster.leanData(notATree)} = false",
          s"example : isRose ${UplcBlaster.leanData(rose)} = true",
          s"example : isDir ${UplcBlaster.leanData(dir)} = true",
          s"example : isChain ${UplcBlaster.leanData(chain)} = true",
          s"example : isChain ${UplcBlaster.leanData(brokenChain)} = false",
          s"example : isOptionalTree ${UplcBlaster.leanData(prelude.Option.Some(BlasterTree.Node(leaf, leaf)).toData)} = true",
          s"example : isForestTree ${UplcBlaster.leanData(forestTree)} = true",
          s"example : isForestTree ${UplcBlaster.leanData(Data.Constr(0, prelude.List(Data.I(1), Data.I(0))))} = false"
        ).map(_ + " := by native_decide")
        val text = (header :: definitions :: examples).mkString("\n\n") + "\n"
        leanServer.check(text, Some(5.minutes)) match
            case Result.Finished(messages) =>
                val errors = messages.filter(_.severity == Severity.Error)
                assert(errors.isEmpty, errors.map((m: Message) => s"${m.line}: ${m.text}"))
            case other => fail(s"Lean did not check the definitions: $other")
    }
}

object DataEncodingTest {

    private def hex(text: String): ByteString = ByteString.fromHex(text)

    /** `encoding`, of the type `A`, held to what FromData and ToData of `A` say of `Data`: on Data
      * that `encoding` gives, on such Data changed in one part, and on Data of no type. Every Data
      * that `encoding` admits is one that FromData reads as a value from which ToData gives it
      * back; and, but where `laxFromData`, as FromData of a SortedMap, which checks no order of the
      * keys, every Data that FromData reads so is one that `encoding` admits.
      */
    private def agrees[A](encoding: DataEncoding, random: Random, laxFromData: Boolean)(using
        from: FromData[A],
        to: ToData[A]
    ): Unit = {
        def isValue(data: Data): Boolean = Try(from(data)).toOption.exists(to(_) == data)
        val fromEncoding = List.fill(200)(sample(encoding, random, Map.empty, 4))
        val changed = fromEncoding.map(change(_, random))
        val any = List.fill(200)(anyData(random, 3))
        for data <- fromEncoding ++ changed ++ any do
            val admitted = encoding.outside(data).isEmpty
            if admitted then assert(isValue(data), s"admitted Data is no value of the type: $data")
            if !laxFromData && isValue(data) then
                assert(admitted, s"a value's Data is not admitted: $data")
        // Data the encoding gives, which the generator below takes for its own.
        assert(
          fromEncoding.forall(encoding.outside(_).isEmpty),
          fromEncoding.find(encoding.outside(_).nonEmpty)
        )
    }

    /** Data that `encoding` admits, of at most about `depth` levels of a type that contains itself;
      * `bound` are the enclosing recursive encodings.
      */
    private def sample(
        encoding: DataEncoding,
        random: Random,
        bound: Map[String, DataEncoding],
        depth: Int
    ): Data = {
        import DataEncoding.*
        encoding match
            case Integer              => Data.I(BigInt(random.nextInt(7) - 3))
            case Bytes | Text         => Data.B(ByteString.fromString(random.nextInt(9).toString))
            case Bool                 => Data.Constr(random.nextInt(2), prelude.List.Nil)
            case AnyData | Unknown(_) => anyData(random, 2)
            case recursive @ Recursive(name, body) =>
                sample(body, random, bound + (name -> recursive), depth)
            case Again(name) =>
                if depth <= 0 then bound(name).exampleIn(bound - name).get
                else sample(bound(name), random, bound, depth - 1)
            case Constr(constructors) =>
                val tag = random.nextInt(constructors.size)
                Data.Constr(
                  tag,
                  prelude.List.from(constructors(tag).map(sample(_, random, bound, depth)))
                )
            case Items(items) =>
                Data.List(
                  prelude.List.from(
                    List.fill(random.nextInt(3))(sample(items, random, bound, depth))
                  )
                )
            case Entries(keys, values, order) =>
                val entries = List.fill(random.nextInt(3))(
                  sample(keys, random, bound, depth) -> sample(values, random, bound, depth)
                )
                val ordered = order match
                    case KeyOrder.Bytes =>
                        val distinct = entries.distinctBy(_._1).collect { case (k @ Data.B(b), v) =>
                            (b, k, v)
                        }
                        distinct
                            .sortWith((a, b) =>
                                java.util.Arrays.compareUnsigned(a._1.bytes, b._1.bytes) < 0
                            )
                            .map((_, k, v) => k -> v)
                    case KeyOrder.Integers =>
                        entries
                            .distinctBy(_._1)
                            .collect { case (k @ Data.I(i), v) => (i, k, v) }
                            .sortBy(_._1)
                            .map((_, k, v) => k -> v)
                    case KeyOrder.Any | KeyOrder.Unchecked => entries
                Data.Map(prelude.List.from(ordered))
    }

    /** `data` with one of its parts, chosen at random, replaced with Data of no type. */
    private def change(data: Data, random: Random): Data = {
        def parts(d: Data): Int = d match
            case Data.Constr(_, fields) => 1 + fields.toScalaList.map(parts).sum
            case Data.List(items)       => 1 + items.toScalaList.map(parts).sum
            case Data.Map(entries) => 1 + entries.toScalaList.map((k, v) => parts(k) + parts(v)).sum
            case _                 => 1
        var at = random.nextInt(parts(data))
        def go(d: Data): Data =
            if at == 0 then
                at -= 1
                anyData(random, 1)
            else
                at -= 1
                d match
                    case Data.Constr(tag, fields) =>
                        Data.Constr(tag, prelude.List.from(fields.toScalaList.map(go)))
                    case Data.List(items) => Data.List(prelude.List.from(items.toScalaList.map(go)))
                    case Data.Map(entries) =>
                        Data.Map(
                          prelude.List.from(entries.toScalaList.map((k, v) => go(k) -> go(v)))
                        )
                    case other => other
        go(data)
    }

    /** Data of no type, of at most `depth` levels. */
    private def anyData(random: Random, depth: Int): Data =
        random.nextInt(if depth <= 0 then 2 else 5) match
            case 0 => Data.I(BigInt(random.nextInt(5) - 2))
            case 1 => Data.B(ByteString.fromString(random.nextInt(9).toString))
            case 2 =>
                Data.Constr(
                  random.nextInt(3),
                  prelude.List.from(List.fill(random.nextInt(3))(anyData(random, depth - 1)))
                )
            case 3 =>
                Data.List(
                  prelude.List.from(List.fill(random.nextInt(3))(anyData(random, depth - 1)))
                )
            case _ =>
                Data.Map(
                  prelude.List.from(
                    List.fill(random.nextInt(3))(
                      anyData(random, depth - 1) -> anyData(random, depth - 1)
                    )
                  )
                )

    /** What a check of the definitions starts with: PlutusCore's `Data`, as `Data`. */
    private val header = List(
      "import ScalusProofs.Run",
      "open PlutusCore.Integer (Integer)",
      "open PlutusCore.ByteString (ByteString)",
      "open PlutusCore.Data (Data)"
    ).mkString("\n")

    private val owner = PubKeyHash(hex("11" * 28))
    private val policy = hex("33" * 28)
    private val sampleOutRef = TxOutRef(TxId(hex("22" * 32)), BigInt(1))

    private val output = TxOut(
      Address(Credential.PubKeyCredential(owner), prelude.Option.None),
      Value.lovelace(BigInt(10)),
      OutputDatum.OutputDatum(Data.I(5))
    )

    /** A context of a minting, with an input, an output, a signatory and a withdrawal. */
    private val sampleContext = ScriptContext(
      TxInfo(
        inputs = prelude.List(TxInInfo(sampleOutRef, output)),
        outputs = prelude.List(output),
        fee = BigInt(2),
        mint = Value(policy, hex("01"), BigInt(1)),
        withdrawals = SortedMap.singleton(Credential.PubKeyCredential(owner), BigInt(3)),
        signatories = prelude.List(owner),
        id = TxId(hex("44" * 32))
      ),
      scriptInfo = ScriptInfo.MintingScript(policy)
    )
}
