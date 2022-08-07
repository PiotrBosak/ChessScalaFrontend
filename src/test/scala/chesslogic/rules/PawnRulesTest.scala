package chesslogic.rules

import chesslogic.board.File.*
import chesslogic.board.Position
import chesslogic.board.Rank.*
import chesslogic.game.FullGame
import chesslogic.rules.PawnRules.{ getPossibleAttacks, getPossibleMoves }
import org.scalatest.flatspec.AnyFlatSpec

class PawnRulesTest extends AnyFlatSpec {
  private val game = FullGame()

  "Pawn on A2 before moving" should "have 2 possible moves" in {
    val startingBoard = game.currentBoard
    val position      = Position(A, Two)
    assertResult(2)(getPossibleMoves(position, startingBoard).size)
  }
  "Pawn on A2 after moving to A3" should "have 1 possible move" in {
    val from      = Position(A, Two)
    val to        = Position(A, Three)
    val afterMove = game.makeMoveWithoutTurn(from, to)
    assert(afterMove.isDefined)
    assertResult(1)(getPossibleMoves(to, afterMove.get.currentBoard).size)

  }

  "Pawn on A2" can "capture pawn on B7" in {
    val startingPosition = Position(A, Two)
    val firstPos         = Position(A, Four)
    val secondPos        = Position(A, Five)
    val thirdPos         = Position(A, Six)
    val fourthPosition   = Position(B, Seven)
    val afterFirst       = game.makeMoveWithoutTurn(startingPosition, firstPos).get
    val afterSecond      = afterFirst.makeMoveWithoutTurn(firstPos, secondPos).get
    val afterThird       = afterSecond.makeMoveWithoutTurn(secondPos, thirdPos).get
    assertResult(1)(getPossibleAttacks(thirdPos, afterThird.currentBoard).size)
    val afterFourth =
      afterThird.makeMoveWithoutTurn(thirdPos, fourthPosition).get
    assertResult(2)(
      getPossibleAttacks(fourthPosition, afterFourth.currentBoard).size
    )
    assertResult(31)(afterFourth.currentBoard.tiles.values.count(_.hasPiece))

  }

  "Pawn on A2" can "make le passant" in {
    val first =
      game.makeMoveWithoutTurn(Position(D, Two), Position(D, Four)).get
    val second =
      first.makeMoveWithoutTurn(Position(D, Four), Position(D, Five)).get
    val third =
      second.makeMoveWithoutTurn(Position(E, Seven), Position(E, Five)).get
    assertResult(1)(
      PawnRules.getPossibleAttacks(Position(D, Five), third.currentBoard).size
    )
    val afterLePassant =
      third.makeMoveWithoutTurn(Position(D, Five), Position(E, Six)).get
    assertResult(31)(
      afterLePassant.currentBoard.tiles.count(t => t._2.currentPiece.isDefined)
    )
  }

  "Pawn on A2" should "not be able to do le passant" in {
    val first =
      game.makeMoveWithoutTurn(Position(D, Two), Position(D, Four)).get
    val second =
      first.makeMoveWithoutTurn(Position(D, Four), Position(D, Five)).get
    val third =
      second.makeMoveWithoutTurn(Position(E, Seven), Position(E, Six)).get
    val fourth =
      third.makeMoveWithoutTurn(Position(E, Six), Position(E, Five)).get
    assertResult(0)(
      PawnRules.getPossibleAttacks(Position(D, Five), fourth.currentBoard).size
    )
  }

}
