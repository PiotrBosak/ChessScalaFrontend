package chesslogic.rules

import chesslogic.board.File.*
import chesslogic.board.Position
import chesslogic.board.Rank.*
import chesslogic.game.FullGame
import org.scalatest.flatspec.AnyFlatSpec

class KnightRulesTest extends AnyFlatSpec {
  private val game: FullGame = FullGame()

  "Knight on B8" should "have 2 possible moves" in {
    val startingBoard = game.currentBoard
    assertResult(2)(
      KnightRules.getPossibleMoves(Position(B, Eight), startingBoard).size
    )
  }

  "Knight on G1" can "capture be able to capture pawn on F7" in {
    val first =
      game.makeMoveWithoutTurn(Position(G, One), Position(F, Three)).get
    val second =
      first.makeMoveWithoutTurn(Position(F, Three), Position(E, Five)).get
    val third =
      second.makeMoveWithoutTurn(Position(E, Five), Position(F, Seven)).get
    assertResult(31)(third.currentBoard.tiles.values.count(_.hasPiece))
    assertResult(2)(
      KnightRules
        .getPossibleAttacks(Position(F, Seven), third.currentBoard)
        .size
    )

  }

}
