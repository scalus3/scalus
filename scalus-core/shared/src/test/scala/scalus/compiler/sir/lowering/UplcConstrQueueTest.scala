package scalus.compiler.sir.lowering

import org.scalatest.funsuite.AnyFunSuite
import scalus.*
import scalus.compiler.{compile, Compile}
import scalus.compiler.Options
import scalus.cardano.onchain.plutus.prelude.*
import scalus.uplc.{Constant, Term}
import scalus.compiler.{UplcRepr, UplcRepresentation}
import scalus.uplc.eval.{PlutusVM, Result}

@UplcRepr(UplcRepresentation.UplcConstr)
case class Item(value: BigInt, extra: BigInt)

@Compile
object QueueModule {
    given Eq[Item] = (a: Item, b: Item) => a.value === b.value && a.extra === b.extra

    def mkItem(v: BigInt, e: BigInt): Item = Item(v, e)

    // Specialized queue (no type parameter)
    opaque type Queue = List[Item]

    def emptyQueue: Queue = List.empty[Item]

    extension (self: Queue)
        def toList: List[Item] = self
        def isEmpty: Boolean = List.isEmpty(self)
        def appendFront(item: Item): Queue = List.prepended(self)(item)
        def appendAllFront(list: List[Item]): Queue = list ++ self
        def removeFront: Queue = List.tail(self)
        def head: Item = List.head(self)

    // makeStarts-like: BuiltinList[BigInt].flatMap → List[Item] (UplcConstr)
    def makeItems(size: BigInt): List[Item] =
        val it = List.range(1, size)
        it.flatMap { x => it.map { y => Item(x, y) } }

    // Full makeStarts-like: flatMap → length → fill → zip → map
    def makeItemsFull(size: BigInt): List[Item] =
        val it = List.range(1, size)
        val l = it.flatMap { x => it.map { y => Item(x, y) } }
        val length = l.length
        require(length == size * size)
        List.fill(BigInt(1) - length, length).zip(l).map { pair =>
            Item(pair._1, pair._2.value)
        }

    def processQueue(queue: Queue): BigInt =
        if queue.isEmpty then BigInt(0)
        else queue.head.value + processQueue(queue.removeFront)

    // depthSearch-like with function parameters
    def depthSearch(
        depth: BigInt,
        queue: Queue,
        grow: Item => List[Item],
        done: Item => Boolean
    ): Queue = {
        if depth === BigInt(0) || queue.isEmpty then emptyQueue
        else if done(queue.head) then
            depthSearch(depth - 1, queue.removeFront, grow, done).appendFront(queue.head)
        else depthSearch(depth - 1, queue.removeFront.appendAllFront(grow(queue.head)), grow, done)
    }
}

class UplcConstrQueueTest extends AnyFunSuite {

    private given PlutusVM = PlutusVM.makePlutusV3VM()
    private given Options = Options()

    test("specialized Queue with flatMap-produced List[Item]") {
        val sir = compile {
            // range(1,3) = [1,2,3], flatMap produces 9 items
            // sum of values: 3*(1+2+3) = 18
            val items = QueueModule.makeItems(BigInt(3))
            val queue = QueueModule.emptyQueue.appendAllFront(items)
            QueueModule.processQueue(queue)
        }
        val result = sir.toUplc().evaluateDebug
        result match
            case Result.Success(Term.Const(Constant.Integer(v), _), _, _, _) =>
                assert(v == 18, s"Expected 18, got $v")
            case Result.Failure(ex, _, _, _) =>
                fail(s"Specialized queue failed: $ex")
            case other => fail(s"Unexpected: $other")
    }

    test("makeItemsFull with zip+map pattern") {
        val sir = compile {
            // makeItemsFull: flatMap → length → fill → zip → map
            val items = QueueModule.makeItemsFull(BigInt(3))
            items.length
        }
        val result = sir.toUplc().evaluateDebug
        result match
            case Result.Success(Term.Const(Constant.Integer(v), _), _, _, _) =>
                assert(v == 9, s"Expected 9, got $v")
            case Result.Failure(ex, _, _, _) =>
                fail(s"makeItemsFull failed: $ex")
            case other => fail(s"Unexpected: $other")
    }

    test("depthSearch with specialized Queue") {
        val sir = compile {
            val items = QueueModule.makeItems(BigInt(3))
            val queue = QueueModule.emptyQueue.appendAllFront(items)
            // grow: duplicate each item; done: value > 5
            val result = QueueModule.depthSearch(
              BigInt(2),
              queue,
              item => List(Item(item.value + 1, item.extra)),
              item => item.value > BigInt(5)
            )
            result.toList.length
        }
        val result = sir.toUplc().evaluateDebug
        result match
            case Result.Success(Term.Const(Constant.Integer(v), _), _, _, _) =>
                info(s"depthSearch result length: $v")
                assert(v >= 0, s"Expected non-negative, got $v")
            case Result.Failure(ex, _, _, _) =>
                fail(s"depthSearch failed: $ex")
            case other => fail(s"Unexpected: $other")
    }

    test("filterMap accessing field - bug reproduction") {
        val sir = compile {
            // Bug: Item constructed in map, then field accessed in filterMap
            // The Item in the list has TypeVarRepresentation(Fixed) for value field,
            // but lambda parameter in filterMap gets Constant representation
            val items = List.range(1, 4).map { x => Item(x, x * 2) }
            val filtered = items.filterMap { item =>
                if item.value === BigInt(2) then Option.Some(item.extra) else Option.None
            }
            filtered.length
        }
        val result = sir.toUplc().evaluateDebug
        result match
            case Result.Success(Term.Const(Constant.Integer(v), _), _, _, _) =>
                assert(v == 1, s"Expected 1, got $v")
            case Result.Failure(ex, _, _, _) =>
                fail(s"filterMap field access failed: $ex")
            case other => fail(s"Unexpected: $other")
    }
}
