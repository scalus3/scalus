package scalus.benchmarks

import scalus.*
import scalus.builtin.Builtins.{multiplyInteger, remainderInteger}
import scalus.prelude.*
import scalus.uplc.*
import scalus.uplc.eval.*
import org.scalatest.funsuite.AnyFunSuite
import scalus.testing.kit.ScalusTest

class KnightsTest extends AnyFunSuite, ScalusTest:
    import KnightsTest.{*, given}

    given scalus.Compiler.Options = scalus.Compiler.Options(
      targetLoweringBackend = scalus.Compiler.TargetLoweringBackend.SirToUplcV3Lowering,
      generateErrorTraces = true,
      optimizeUplc = true,
      debug = false
    )

    val printComparison = true

    test("100_4x4") {
        val sir = Compiler.compile {
            val result = runKnights(100, 4)
            val expected: Solution = List.empty
            require(result === expected)
        }
        // val lw = sir.toLoweredValue()
        // println(s"Lowered value: ${lw.pretty.render(100)}")
        val result = sir.toUplcOptimized(false).evaluateDebug

        val scalusBudget =
            if summon[
                  scalus.Compiler.Options
                ].targetLoweringBackend == scalus.Compiler.TargetLoweringBackend.SirToUplcV3Lowering
            then ExBudget(ExCPU(93104_669030L), ExMemory(329_188074L))
            else if summon[
                  scalus.Compiler.Options
                ].targetLoweringBackend == scalus.Compiler.TargetLoweringBackend.SirToUplc110Lowering
            then ExBudget(ExCPU(44783_358238L), ExMemory(247_807177L))
            else {
                // actually we don't know, need recheck
                ExBudget(ExCPU(44783_358238L), ExMemory(247_807177L))
            }

        assert(result.isSuccess)
        assert(result.budget == scalusBudget)

        compareBudgetWithReferenceValue(
          testName = "KnightsTest.100_4x4",
          scalusBudget = scalusBudget,
          refBudget = ExBudget(ExCPU(54958_831939L), ExMemory(160_204421L)),
          isPrintComparison = printComparison
        )
    }

    test("100_6x6") {
        val result = Compiler
            .compile {
                val result = runKnights(100, 6)

                val expected: Solution = List(
                  (
                    0,
                    ChessSet(
                      size = 6,
                      moveNumber = 36,
                      start = Option.Some((1, 1)),
                      visited = List(
                        // format: off
                        (3, 2), (5, 3), (6, 1), (4, 2), (3, 4), (2, 6), (4, 5), (6, 6), (5, 4),
                        (6, 2), (4, 1), (2, 2), (1, 4), (3, 3), (2, 1), (1, 3), (2, 5), (4, 6),
                        (6, 5), (4, 4), (5, 2), (6, 4), (5, 6), (3, 5), (1, 6), (2, 4), (1, 2),
                        (3, 1), (4, 3), (5, 1), (6, 3), (5, 5), (3, 6), (1, 5), (2, 3), (1, 1)
                        // format: on
                      )
                    )
                  ),
                  (
                    0,
                    ChessSet(
                      size = 6,
                      moveNumber = 36,
                      start = Option.Some((1, 1)),
                      visited = List(
                        // format: off
                        (3, 2), (5, 3), (6, 1), (4, 2), (3, 4), (2, 2), (4, 1), (6, 2), (5, 4),
                        (6, 6), (4, 5), (2, 6), (1, 4), (3, 3), (2, 1), (1, 3), (2, 5), (4, 6),
                        (6, 5), (4, 4), (5, 2), (6, 4), (5, 6), (3, 5), (1, 6), (2, 4), (1, 2),
                        (3, 1), (4, 3), (5, 1), (6, 3), (5, 5), (3, 6), (1, 5), (2, 3), (1, 1)
                        // format: on
                      )
                    )
                  ),
                  (
                    0,
                    ChessSet(
                      size = 6,
                      moveNumber = 36,
                      start = Option.Some((1, 1)),
                      visited = List(
                        // format: off
                        (3, 2), (5, 3), (6, 1), (4, 2), (3, 4), (2, 2), (1, 4), (2, 6), (4, 5),
                        (6, 6), (5, 4), (6, 2), (4, 1), (3, 3), (2, 1), (1, 3), (2, 5), (4, 6),
                        (6, 5), (4, 4), (5, 2), (6, 4), (5, 6), (3, 5), (1, 6), (2, 4), (1, 2),
                        (3, 1), (4, 3), (5, 1), (6, 3), (5, 5), (3, 6), (1, 5), (2, 3), (1, 1)
                        // format: on
                      )
                    )
                  ),
                  (
                    0,
                    ChessSet(
                      size = 6,
                      moveNumber = 36,
                      start = Option.Some((1, 1)),
                      visited = List(
                        // format: off
                        (3, 2), (5, 3), (6, 1), (4, 2), (3, 4), (2, 6), (1, 4), (2, 2), (4, 1),
                        (6, 2), (5, 4), (6, 6), (4, 5), (3, 3), (2, 1), (1, 3), (2, 5), (4, 6),
                        (6, 5), (4, 4), (5, 2), (6, 4), (5, 6), (3, 5), (1, 6), (2, 4), (1, 2),
                        (3, 1), (4, 3), (5, 1), (6, 3), (5, 5), (3, 6), (1, 5), (2, 3), (1, 1)
                        // format: on
                      )
                    )
                  ),
                )

                require(result === expected)
            }
            .toUplcOptimized(false)
            .evaluateDebug

        val scalusBudget =
            summon[scalus.Compiler.Options].targetLoweringBackend match
                case scalus.Compiler.TargetLoweringBackend.SirToUplcV3Lowering =>
                    ExBudget(ExCPU(232462_574079L), ExMemory(848_238459L))
                case scalus.Compiler.TargetLoweringBackend.SirToUplc110Lowering =>
                    ExBudget(ExCPU(115775_218834L), ExMemory(645_799142L))
                case _ =>
                    throw new IllegalStateException("Unsupported target lowering backend")
        if !result.isSuccess then println(s"Result:  $result")
        assert(result.isSuccess)
        assert(result.budget == scalusBudget)

        compareBudgetWithReferenceValue(
          testName = "KnightsTest.100_6x6",
          scalusBudget = scalusBudget,
          refBudget = ExBudget(ExCPU(131954_064320L), ExMemory(292_216349L)),
          isPrintComparison = printComparison
        )
    }

    test("100_8x8") {
        val result = Compiler
            .compile {
                val result = runKnights(100, 8)

                import scalus.prelude.List.*
                val expected: Solution = Cons(
                  (
                    0,
                    ChessSet(
                      size = 8,
                      moveNumber = 64,
                      start = Option.Some((1, 1)),
                      visited = List(
                        // format: off
                        (3, 2), (4, 4), (5, 6), (6, 4), (8, 5), (7, 7), (6, 5), (8, 4), (7, 2),
                        (5, 3), (3, 4), (4, 6), (5, 8), (6, 6), (4, 5), (3, 7), (1, 8), (2, 6),
                        (4, 7), (5, 5), (6, 3), (5, 1), (4, 3), (3, 5), (5, 4), (7, 3), (8, 1),
                        (6, 2), (4, 1), (2, 2), (1, 4), (3, 3), (2, 5), (1, 3), (2, 1), (4, 2),
                        (6, 1), (8, 2), (7, 4), (8, 6), (7, 8), (5, 7), (3, 8), (1, 7), (3, 6),
                        (2, 8), (1, 6), (2, 4), (1, 2), (3, 1), (5, 2), (7, 1), (8, 3), (7, 5),
                        (8, 7), (6, 8), (7, 6), (8, 8), (6, 7), (4, 8), (2, 7), (1, 5), (2, 3),
                        (1, 1)
                        // format: on
                      )
                    )
                  ),
                  Cons(
                    (
                      0,
                      ChessSet(
                        size = 8,
                        moveNumber = 64,
                        start = Option.Some((1, 1)),
                        visited = List(
                          // format: off
                          (3, 2), (4, 4), (5, 6), (7, 7), (8, 5), (6, 4), (7, 2), (8, 4), (6, 5),
                          (5, 3), (3, 4), (4, 6), (5, 8), (6, 6), (4, 5), (3, 7), (1, 8), (2, 6),
                          (4, 7), (5, 5), (6, 3), (5, 1), (4, 3), (3, 5), (5, 4), (7, 3), (8, 1),
                          (6, 2), (4, 1), (2, 2), (1, 4), (3, 3), (2, 5), (1, 3), (2, 1), (4, 2),
                          (6, 1), (8, 2), (7, 4), (8, 6), (7, 8), (5, 7), (3, 8), (1, 7), (3, 6),
                          (2, 8), (1, 6), (2, 4), (1, 2), (3, 1), (5, 2), (7, 1), (8, 3), (7, 5),
                          (8, 7), (6, 8), (7, 6), (8, 8), (6, 7), (4, 8), (2, 7), (1, 5), (2, 3),
                          (1, 1)
                          // format: on
                        )
                      )
                    ),
                    Cons(
                      (
                        0,
                        ChessSet(
                          size = 8,
                          moveNumber = 64,
                          start = Option.Some((1, 1)),
                          visited = List(
                            // format: off
                            (3, 2), (4, 4), (6, 5), (8, 4), (7, 2), (5, 3), (3, 4), (4, 6), (5, 8),
                            (7, 7), (5, 6), (6, 4), (8, 5), (6, 6), (4, 5), (3, 7), (1, 8), (2, 6),
                            (4, 7), (5, 5), (6, 3), (5, 1), (4, 3), (3, 5), (5, 4), (7, 3), (8, 1),
                            (6, 2), (4, 1), (2, 2), (1, 4), (3, 3), (2, 5), (1, 3), (2, 1), (4, 2),
                            (6, 1), (8, 2), (7, 4), (8, 6), (7, 8), (5, 7), (3, 8), (1, 7), (3, 6),
                            (2, 8), (1, 6), (2, 4), (1, 2), (3, 1), (5, 2), (7, 1), (8, 3), (7, 5),
                            (8, 7), (6, 8), (7, 6), (8, 8), (6, 7), (4, 8), (2, 7), (1, 5), (2, 3),
                            (1, 1),
                            // format: on
                          )
                        )
                      ),
                      Nil
                    )
                  )
                )

                require(result === expected)
            }
            .toUplcOptimized(false)
            .evaluateDebug

        val scalusBudget = {
            summon[scalus.Compiler.Options].targetLoweringBackend match {
                case scalus.Compiler.TargetLoweringBackend.SirToUplcV3Lowering =>
                    ExBudget(ExCPU(462613_329705L), ExMemory(1705_972553L))
                case scalus.Compiler.TargetLoweringBackend.SirToUplc110Lowering =>
                    ExBudget(ExCPU(235822_700067L), ExMemory(1315_097779L))
                case scalus.Compiler.TargetLoweringBackend.SimpleSirToUplcLowering =>
                    ExBudget(ExCPU(235822_700067L), ExMemory(1315_097779L))
            }
        }
        assert(result.isSuccess)
        assert(result.budget == scalusBudget)

        compareBudgetWithReferenceValue(
          testName = "KnightsTest.100_8x8",
          scalusBudget = scalusBudget,
          refBudget = ExBudget(ExCPU(270266_226527L), ExMemory(540_217437L)),
        )
    }

end KnightsTest

@Compile
object KnightsTest:
    enum Direction:
        case UL, UR, DL, DR, LU, LD, RU, RD

    val directions: List[Direction] = {
        import Direction.*
        List.Cons(
          UL,
          List.Cons(
            UR,
            List.Cons(
              DL,
              List.Cons(DR, List.Cons(LU, List.Cons(LD, List.Cons(RU, List.Cons(RD, List.Nil)))))
            )
          )
        )
    }

    type Tile = (BigInt, BigInt)

    extension (self: Tile)
        def move(direction: Direction): Tile =
            val (x, y) = self
            import Direction.*
            direction match
                case UL => (x - 1, y - 2)
                case UR => (x + 1, y - 2)
                case DL => (x - 1, y + 2)
                case DR => (x + 1, y + 2)
                case LU => (x - 2, y - 1)
                case LD => (x - 2, y + 1)
                case RU => (x + 2, y - 1)
                case RD => (x + 2, y + 1)

    end extension

    type Solution = List[(BigInt, ChessSet)]

    case class ChessSet(
        size: BigInt,
        moveNumber: BigInt,
        start: Option[Tile],
        visited: List[Tile]
    )

    def createBoard(size: BigInt, initSquare: Tile): ChessSet =
        ChessSet(
          size = size,
          moveNumber = BigInt(1),
          start = Option.Some(initSquare),
          visited = List.single(initSquare)
        )

    def startTour(tile: Tile, size: BigInt): ChessSet =
        require(remainderInteger(size, BigInt(2)) === BigInt(0))
        createBoard(size, tile)

    given Eq[ChessSet] =
        (lhs: ChessSet, rhs: ChessSet) =>
            lhs.size === rhs.size &&
                lhs.moveNumber === rhs.moveNumber &&
                lhs.start === rhs.start &&
                lhs.visited === rhs.visited

    given Ord[ChessSet] = (lhs: ChessSet, rhs: ChessSet) => lhs.visited <=> rhs.visited

    extension (self: ChessSet)
        def addPiece(tile: Tile): ChessSet =
            ChessSet(
              size = self.size,
              moveNumber = self.moveNumber + 1,
              start = self.start,
              visited = self.visited.prepended(tile)
            )

        def firstPiece: Tile = self.start.get
        def lastPiece: Tile = self.visited.head

        def deleteFirst: ChessSet =
            extension [A](self: List[A])
                def secondLast: Option[A] =
                    self.reverse match
                        case List.Nil => fail()
                        case List.Cons(_, tail) =>
                            tail match
                                case List.Nil            => Option.None
                                case List.Cons(value, _) => Option.Some(value)

            end extension

            val newVisited = self.visited.init

            ChessSet(
              size = self.size,
              moveNumber = self.moveNumber - 1,
              start = self.visited.secondLast,
              visited = newVisited
            )

        def isSquareFree(tile: Tile): Boolean = !self.visited.contains(tile)

        def canMoveTo(tile: Tile): Boolean =
            val (x, y) = tile
            val size = self.size
            x >= 1 && x <= size && y >= 1 && y <= size && isSquareFree(tile)

        def canMove(direction: Direction): Boolean = canMoveTo(lastPiece.move(direction))
        def moveKnight(direction: Direction): ChessSet = addPiece(lastPiece.move(direction))
        def possibleMoves: List[Direction] = directions.filter(canMove)
        def allDescend: List[ChessSet] = possibleMoves.map(moveKnight)

        def descAndNo: Solution = allDescend.map { item =>
            (item.deleteFirst.possibleMoves.length, item)
        }

        def singleDescend: List[ChessSet] =
            descAndNo.filterMap { item =>
                val (moves, board) = item
                if moves === BigInt(1) then Option.Some(board) else Option.empty
            }

        def isDeadEnd: Boolean = possibleMoves.isEmpty
        def canJumpFirst: Boolean = deleteFirst.canMoveTo(firstPiece)

        def descendants: List[ChessSet] = {
            if canJumpFirst && addPiece(firstPiece).isDeadEnd then List.empty
            else
                val singles = singleDescend
                singles match
                    case List.Nil              => descAndNo.quicksort.map { _._2 }
                    case List.Cons(head, tail) => if tail.isEmpty then singles else List.empty
        }

        def isTourFinished: Boolean =
            self.moveNumber === multiplyInteger(self.size, self.size) && canJumpFirst

    end extension

    case class Queue[A](list: List[A])

    def emptyQueue[A]: Queue[A] = Queue(List.empty[A])

    extension [A](self: Queue[A])
        def toList: List[A] = self.list
        def isEmpty: Boolean = toList.isEmpty
        def appendFront(item: A): Queue[A] = Queue(toList.prepended(item))
        def appendAllFront(list: List[A]): Queue[A] = Queue(list ++ toList)
        def removeFront: Queue[A] = Queue(toList.tail)
        def head: A = toList.head

    end extension

    def isDone(item: (BigInt, ChessSet)): Boolean = item._2.isTourFinished

    def grow(item: (BigInt, ChessSet)): List[(BigInt, ChessSet)] =
        val (x, board) = item
        board.descendants.map { (x + 1, _) }

    def makeStarts(size: BigInt): List[(BigInt, ChessSet)] =
        val it = List.range(1, size)
        val l = it.flatMap { x => it.map { y => startTour((x, y), size) } }
        val length = l.length
        require(length == size * size)
        List.fill(1 - length, length).zip(l)

    def root(size: BigInt): Queue[(BigInt, ChessSet)] =
        emptyQueue[(BigInt, ChessSet)].appendAllFront(makeStarts(size))

    def depthSearch[A](
        depth: BigInt,
        queue: Queue[A],
        grow: A => List[A],
        done: A => Boolean
    ): Queue[A] = {
        if depth === BigInt(0) || queue.isEmpty then emptyQueue[A]
        else if done(queue.head) then
            depthSearch(depth - 1, queue.removeFront, grow, done).appendFront(queue.head)
        else depthSearch(depth - 1, queue.removeFront.appendAllFront(grow(queue.head)), grow, done)
    }

    def runKnights(depth: BigInt, boardSize: BigInt): Solution =
        depthSearch(depth, root(boardSize), grow, isDone).toList

end KnightsTest
