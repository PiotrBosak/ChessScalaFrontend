package chesslogic.rules

import chesslogic.board.File.*
import chesslogic.board.Position
import chesslogic.board.Rank.*
import chesslogic.game.FullGame
import chesslogic.rules.BishopRules.getPossibleMoves
import org.scalatest.flatspec.AnyFlatSpec

class BishopRulesTest extends AnyFlatSpec {
  val game = FullGame()

  "Bishop on C1" should "have 0 moves possible at the start" in {
    val startingBoard = game.currentBoard
    val position      = Position(C, One)
    assertResult(0)(getPossibleMoves(position, startingBoard).size)
  }
  "After pawn D2 moves Bishop on C1" should "have 5 possible moves" in {
    val afterPawnMove =
      game.makeMoveWithoutTurn(Position(D, Two), Position(D, Four)).get
    assertResult(5)(
      getPossibleMoves(Position(C, One), afterPawnMove.currentBoard).size
    )

  }
  "Bishop on C1" should "have 8 possible moves after moving to F4" in {
    val afterPawnMove =
      game.makeMoveWithoutTurn(Position(D, Two), Position(D, Four)).get
    val afterFirstBishop =
      afterPawnMove.makeMoveWithoutTurn(Position(C, One), Position(F, Four)).get
    assertResult(8)(
      getPossibleMoves(Position(F, Four), afterFirstBishop.currentBoard).size
    )

  }

}
