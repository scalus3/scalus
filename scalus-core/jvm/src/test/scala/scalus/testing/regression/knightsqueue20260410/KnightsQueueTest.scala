package scalus.testing.regression.knightsqueue20260410

import org.scalatest.funsuite.AnyFunSuite
import scalus.*
import scalus.compiler.{compile, Compile, Options, UplcRepr, UplcRepresentation}
import scalus.cardano.onchain.plutus.prelude.*
import scalus.cardano.ledger.MajorProtocolVersion
import scalus.compiler.sir.TargetLoweringBackend
import scalus.uplc.{Constant, Term}
import scalus.uplc.eval.{PlutusVM, Result}
import scalus.uplc.builtin.Builtins.{multiplyInteger, remainderInteger}

/** Minimized reproduction of KnightsTest crash with specialized Queue. */
@Compile
object KnightsQueueRepro:

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

    @UplcRepr(UplcRepresentation.UplcConstr)
    case class Tile(x: BigInt, y: BigInt)

    given Eq[Tile] = Eq.derived
    given Ord[Tile] = (lhs: Tile, rhs: Tile) => (lhs.x <=> rhs.x) ifEqualThen (lhs.y <=> rhs.y)

    @UplcRepr(UplcRepresentation.UplcConstr)
    case class ChessSet(
        size: BigInt,
        moveNumber: BigInt,
        start: Option[Tile],
        @UplcRepr(UplcRepresentation.UplcConstr)
        visited: List[Tile]
    )

    given Eq[ChessSet] = Eq.derived
    given Ord[ChessSet] = (lhs: ChessSet, rhs: ChessSet) => lhs.visited <=> rhs.visited

    @UplcRepr(UplcRepresentation.UplcConstr)
    case class SolutionEntry(depth: BigInt, board: ChessSet)

    given Eq[SolutionEntry] = Eq.derived
    given Ord[SolutionEntry] = (lhs: SolutionEntry, rhs: SolutionEntry) =>
        (lhs.depth <=> rhs.depth) ifEqualThen (lhs.board <=> rhs.board)

    def createBoard(size: BigInt, initSquare: Tile): ChessSet =
        ChessSet(
          size = size,
          moveNumber = BigInt(1),
          start = Option.Some(initSquare),
          visited = List.single(initSquare)
        )

    extension (self: Tile)
        def move(direction: Direction): Tile =
            import Direction.*
            direction match
                case UL => Tile(self.x - 1, self.y - 2)
                case UR => Tile(self.x + 1, self.y - 2)
                case DL => Tile(self.x - 1, self.y + 2)
                case DR => Tile(self.x + 1, self.y + 2)
                case LU => Tile(self.x - 2, self.y - 1)
                case LD => Tile(self.x - 2, self.y + 1)
                case RU => Tile(self.x + 2, self.y - 1)
                case RD => Tile(self.x + 2, self.y + 1)
    end extension

    extension (self: ChessSet)
        def lastPiece: Tile = self.visited.head
        def canMove(direction: Direction): Boolean = lastPiece.x > BigInt(0)
        def possibleMoves: List[Direction] = directions.filter(canMove)

    end extension

    type Solution = List[SolutionEntry]

    def startTour(tile: Tile, size: BigInt): ChessSet =
        require(remainderInteger(size, BigInt(2)) === BigInt(0))
        createBoard(size, tile)

    def makeStarts(size: BigInt): List[SolutionEntry] =
        val it = List.range(1, size)
        val l = it.flatMap { x => it.map { y => startTour(Tile(x, y), size) } }
        val length = l.length
        require(length == size * size)
        List.fill(1 - length, length).zip(l).map { pair =>
            SolutionEntry(pair._1, pair._2)
        }

    opaque type Queue = List[SolutionEntry]

    def emptyQueue: Queue = List.empty[SolutionEntry]

    extension (self: Queue)
        def toList: List[SolutionEntry] = self
        def isEmpty: Boolean = List.isEmpty(self)
        def appendFront(item: SolutionEntry): Queue = List.prepended(self)(item)
        def appendAllFront(list: List[SolutionEntry]): Queue = list ++ self
        def removeFront: Queue = List.tail(self)
        def head: SolutionEntry = List.head(self)

    def root(size: BigInt): Queue =
        emptyQueue.appendAllFront(makeStarts(size))

    def depthSearch(
        depth: BigInt,
        queue: Queue,
        grow: SolutionEntry => List[SolutionEntry],
        done: SolutionEntry => Boolean
    ): Queue = {
        if depth === BigInt(0) || queue.isEmpty then emptyQueue
        else if done(queue.head) then
            depthSearch(depth - 1, queue.removeFront, grow, done).appendFront(queue.head)
        else depthSearch(depth - 1, queue.removeFront.appendAllFront(grow(queue.head)), grow, done)
    }

    def isDone(item: SolutionEntry): Boolean = item.depth > BigInt(5)

    def grow(item: SolutionEntry): List[SolutionEntry] =
        item.board.possibleMoves.map { _ => SolutionEntry(item.depth + 1, item.board) }

    def run(depth: BigInt, boardSize: BigInt): List[SolutionEntry] =
        depthSearch(depth, root(boardSize), grow, isDone).toList

end KnightsQueueRepro

class KnightsQueueTest extends AnyFunSuite {

    private given PlutusVM = PlutusVM.makePlutusV3VM(MajorProtocolVersion.vanRossemPV)
    private given Options = Options(
      targetLoweringBackend = TargetLoweringBackend.SirToUplcV3Lowering,
      targetProtocolVersion = MajorProtocolVersion.vanRossemPV,
      generateErrorTraces = true,
      optimizeUplc = true,
      debug = false
    )

    test("visited.head on ChessSet with @UplcRepr visited field") {
        val sir = compile {
            // Minimal: create ChessSet, call lastPiece (= visited.head)
            val board = KnightsQueueRepro.createBoard(BigInt(4), KnightsQueueRepro.Tile(1, 1))
            board.lastPiece.x
        }
        val result = sir.toUplc().evaluateDebug
        result match
            case Result.Success(Term.Const(Constant.Integer(v), _), _, _, _) =>
                assert(v == 1, s"Expected 1, got $v")
            case Result.Failure(ex, _, _, _) =>
                fail(s"KnightsQueueRepro failed: $ex")
            case other => fail(s"Unexpected: $other")
    }
}
