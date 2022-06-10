package chesslogic.board

import chesslogic.*
import chesslogic.Color.*
import chesslogic.board.File.*
import chesslogic.board.Rank.*
import chesslogic.pieces.Piece.*
import org.scalatest.flatspec.AnyFlatSpec

class BoardFactoryTest extends AnyFlatSpec {
  val board: Board = Board()

  "Board" should " have 64 tiles, 32 for each color" in {
    assert(board.tiles.values.size == 64)
    assert(board.tiles.values.count(_.color == Black) == 32)
    assert(board.tiles.values.count(_.color == White) == 32)
  }
  "Board" should "have 32 empty tiles at the beginning" in {
    assert(board.tiles.values.count(_.currentPiece.isEmpty) == 32)
  }

  "Board" should "have 16 pawns, 8 for each color" in {
    assert(
      board.tiles.values
        .map(_.currentPiece)
        .count(p => p.isDefined && p.get.isInstanceOf[Pawn]) == 16
    )
    assert(
      board.tiles.values
        .map(_.currentPiece)
        .count(p => p.isDefined && p.get.isInstanceOf[Pawn] && p.get.color == Black) == 8
    )
    assert(
      board.tiles.values
        .map(_.currentPiece)
        .count(p => p.isDefined && p.get.isInstanceOf[Pawn] && p.get.color == White) == 8
    )
  }
  "Board" should "have 2 black Knights and 2 white Knights" in {
    assert(
      board.tiles.values
        .map(_.currentPiece)
        .count(p => p.isDefined && p.get.isInstanceOf[Knight] && p.get.color == White) == 2
    )
    assert(
      board.tiles.values
        .map(_.currentPiece)
        .count(p => p.isDefined && p.get.isInstanceOf[Knight] && p.get.color == Black) == 2
    )
  }

  "Board" should "have a white rook at A1 and a black bishop at F8" in {
    assert(
      board.tiles.values.find(t => t.position.file == A && t.position.rank == One).get.currentPiece.get == Rook(White)
    )
    assert(
      board.tiles.values.find(t => t.position.file == F && t.position.rank == Eight).get.currentPiece.get == Bishop(
        Black
      )
    )
  }
}
