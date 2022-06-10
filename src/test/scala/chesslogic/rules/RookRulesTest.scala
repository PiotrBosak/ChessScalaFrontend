package chesslogic.rules

import chesslogic.board.File.*
import chesslogic.board.Position
import chesslogic.board.Rank.*
import chesslogic.game.FullGame
import chesslogic.rules.RookRules.{getPossibleAttacks, getPossibleMoves}
import org.scalatest.flatspec.AnyFlatSpec

class RookRulesTest extends AnyFlatSpec {
  val game = FullGame()

  "Rook on A1" should "have 0 moves possible at the start" in {
    val startingBoard = game.currentBoard
    val position      = Position(A, One)
    assertResult(0)(getPossibleMoves(position, startingBoard).size)
  }
  "After pawn move, Rook on A1, after 1 move" should "have 9 possible moves" in {
    val afterPawn =
      game.makeMoveWithoutTurn(Position(A, Two), Position(A, Four)).get
    val afterFirstRookMove =
      afterPawn.makeMoveWithoutTurn(Position(A, One), Position(A, Three)).get
    assertResult(9)(
      getPossibleMoves(Position(A, Three), afterFirstRookMove.currentBoard).size
    )

  }
  "After capturing pawn on D7, Rook " should "have  2 possible attacks and 5 moves" in {
    val afterPawn =
      game.makeMoveWithoutTurn(Position(A, Two), Position(A, Four)).get
    val afterFirstRookMove =
      afterPawn.makeMoveWithoutTurn(Position(A, One), Position(A, Three)).get
    val afterSecondRookMove = afterFirstRookMove
      .makeMoveWithoutTurn(Position(A, Three), Position(D, Three))
      .get
    val afterAttack = afterSecondRookMove
      .makeMoveWithoutTurn(Position(D, Three), Position(D, Seven))
      .get
    assertResult(3)(
      getPossibleAttacks(Position(D, Seven), afterAttack.currentBoard).size
    )
    assertResult(4)(
      getPossibleMoves(Position(D, Seven), afterAttack.currentBoard).size
    )
  }

}
