package chesslogic.rules

import chesslogic.Color.*
import chesslogic.board.File.*
import chesslogic.board.Position
import chesslogic.board.Rank.*
import chesslogic.game.FullGame
import chesslogic.pieces.Piece.*
import org.scalatest.flatspec.AnyFlatSpec

class KingRulesTest extends AnyFlatSpec {
  private val game = FullGame()
  "King on E1" should "have 0 moves" in {
    val startingBoard = game.currentBoard
    assertResult(0)(
      KingRules.getPossibleMoves(Position(E, One), startingBoard).size
    )
  }

  "King on E8" should "capture pawn on F7 and have 5 possible attacks" in {
    val afterPawn =
      game.makeMoveWithoutTurn(Position(E, Two), Position(E, Four)).get
    val first =
      afterPawn.makeMoveWithoutTurn(Position(E, One), Position(E, Two)).get
    val second =
      first.makeMoveWithoutTurn(Position(E, Two), Position(E, Three)).get
    val third =
      second.makeMoveWithoutTurn(Position(E, Three), Position(F, Four)).get
    val fourth =
      third.makeMoveWithoutTurn(Position(F, Four), Position(F, Five)).get
    val fifth =
      fourth.makeMoveWithoutTurn(Position(G, Seven), Position(G, Five)).get
    val sixth =
      fifth.makeMoveWithoutTurn(Position(F, Five), Position(G, Five)).get
    assertResult(0)(
      KingRules.getPossibleAttacks(Position(G, Five), sixth.currentBoard).size
    )
    assertResult(8)(
      KingRules.getPossibleMoves(Position(G, Five), sixth.currentBoard).size
    )
    assertResult(31)(sixth.currentBoard.tiles.values.count(_.hasPiece))

  }

  "King on E8" can "make castling after queen,bishop and knight are gone" in {
    val afterPawn =
      game.makeMoveWithoutTurn(Position(D, Two), Position(D, Four)).get
    val afterBishop =
      afterPawn.makeMoveWithoutTurn(Position(C, One), Position(H, Six)).get
    val afterQueen =
      afterBishop.makeMoveWithoutTurn(Position(D, One), Position(D, Three)).get
    val afterKnight =
      afterQueen.makeMoveWithoutTurn(Position(B, One), Position(A, Three)).get
    assertResult(3)(
      KingRules
        .getPossibleMoves(Position(E, One), afterKnight.currentBoard)
        .size
    )
    val afterCastling =
      afterKnight.makeMoveWithoutTurn(Position(E, One), Position(B, One)).get
    val rookTile = afterCastling.currentBoard.getTile(Position(C, One))
    assert(
      rookTile.currentPiece.isDefined && rookTile.currentPiece.get
        .isInstanceOf[Rook]
    )
  }

  "King" should "not be able to do castling when he is checked" in {
    val afterPawn =
      game.makeMoveWithoutTurn(Position(D, Two), Position(D, Four)).get
    val afterBishop =
      afterPawn.makeMoveWithoutTurn(Position(C, One), Position(H, Six)).get
    val afterQueen =
      afterBishop.makeMoveWithoutTurn(Position(D, One), Position(D, Three)).get
    val afterKnight =
      afterQueen.makeMoveWithoutTurn(Position(B, One), Position(A, Three)).get
    val afterBlackPawn =
      afterKnight.makeMoveWithoutTurn(Position(C, Seven), Position(C, Six)).get
    val afterCheck = afterBlackPawn
      .makeMoveWithoutTurn(Position(D, Eight), Position(A, Five))
      .get
    assert(CheckAndMateRules.isKingChecked(afterCheck.currentBoard, White))
    assert(
      afterCheck.makeMoveWithoutTurn(Position(E, One), Position(B, One)).isEmpty
    )
  }

}
