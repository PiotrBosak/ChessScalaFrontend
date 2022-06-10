package chesslogic.rules

import chesslogic.board.File.*
import chesslogic.board.Position
import chesslogic.board.Rank.*
import chesslogic.game.FullGame
import org.scalatest.flatspec.AnyFlatSpec

class GameTest extends AnyFlatSpec {
  private val game = FullGame()

  "white pawn" should "not be able to move twice" in {
    val afterFirstMove = game.makeMove(Position(A, Two), Position(A, Three)).get
    assert(afterFirstMove.makeMove(Position(A, Two), Position(A, Four)).isEmpty)
  }

}
