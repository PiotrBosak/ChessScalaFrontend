package chesslogic.board

import chesslogic.*
import chesslogic.pieces.Piece.*
import chesslogic.Color.*
import chesslogic.pieces.Piece
import chesslogic.board.File.*
import chesslogic.board.Rank.*
import scala.annotation.tailrec

case object BoardFactory {
  def apply(): Map[Position, Tile] = {

    createTilesWithBlackPawns() ++
      createTilesWithWhiteRooks() ++
      createTilesWithBlackRooks() ++
      createTilesWithWhiteKnights() ++
      createTilesWithBlackKnights() ++
      createTilesWithWhiteBishops() +
      createTileWithBlackKing() ++
      createTilesWithNoPieces() ++
      createTilesWithWhitePawns() ++
      createTilesWithBlackBishops() +
      createTileWithWhiteQueen() +
      createTileWithBlackQueen() +
      createTileWithWhiteKing()

  }

  private def createTilesWithBlackBishops(): Map[Position, Tile] = Map(
    Position(C, Eight) -> Tile(Position(C, Eight), Some(Bishop(Black))),
    Position(F, Eight) -> Tile(Position(F, Eight), Some(Bishop(Black)))
  ).view.toMap

  private def createTilesWithBlackKnights(): Map[Position, Tile] = Map(
    Position(B, Eight) -> Tile(Position(B, Eight), Some(Knight(Black))),
    Position(G, Eight) -> Tile(Position(G, Eight), Some(Knight(Black)))
  ).view.toMap

  private def createTilesWithBlackRooks(): Map[Position, Tile] = Map(
    Position(A, Eight) -> Tile(Position(A, Eight), Some(Rook(Black))),
    Position(H, Eight) -> Tile(Position(H, Eight), Some(Rook(Black)))
  ).view.toMap

  private def createTilesWithWhiteBishops(): Map[Position, Tile] = Map(
    Position(C, One) -> Tile(Position(C, One), Some(Bishop(White))),
    Position(F, One) -> Tile(Position(F, One), Some(Bishop(White)))
  ).view.toMap

  private def createTilesWithWhiteKnights(): Map[Position, Tile] = Map(
    Position(B, One) -> Tile(Position(B, One), Some(Knight(White))),
    Position(G, One) -> Tile(Position(G, One), Some(Knight(White)))
  ).view.toMap

  private def createTilesWithWhiteRooks(): Map[Position, Tile] = Map(
    Position(A, One) -> Tile(Position(A, One), Some(Rook(White))),
    Position(H, One) -> Tile(Position(H, One), Some(Rook(White)))
  ).view.toMap

  private def createTileWithBlackKing(): (Position, Tile) =
    Position(E, Eight) -> Tile(Position(E, Eight), Some(King(Black)))

  private def createTileWithBlackQueen(): (Position, Tile) =
    Position(D, Eight) -> Tile(Position(D, Eight), Some(Queen(Black)))

  private def createTileWithWhiteKing(): (Position, Tile) =
    Position(E, One) -> Tile(Position(E, One), Some(King(White)))

  private def createTileWithWhiteQueen(): (Position, Tile) =
    Position(D, One) -> Tile(Position(D, One), Some(Queen(White)))

  private def createTilesWithNoPieces(): Map[Position, Tile] = {
    @tailrec
    def aux(row: Int, map: Map[Position, Tile]): Map[Position, Tile] =
      if (row > 6) map
      else aux(row + 1, map ++ createTiles(row, None))

    aux(3, Map())

  }

  private def createTilesWithBlackPawns(): Map[Position, Tile] =
    createTiles(7, Some(Pawn(Black)))

  private def createTilesWithWhitePawns(): Map[Position, Tile] =
    createTiles(2, Some(Pawn(White)))

  private def createTiles(rank: Int, piece: Option[Piece]): Map[Position, Tile] = {
    @tailrec
    def createTilesInColumn(file: Int, map: Map[Position, Tile]): Map[Position, Tile] = {
      if (file > 8) map
      else if ((rank + file) % 2 == 0)
        createTilesInColumn(
          file + 1,
          map + (Position(File.fromIntUnsafe(file), Rank.fromIntUnsafe(rank)) -> Tile(
            Position(File.fromIntUnsafe(file), Rank.fromIntUnsafe(rank)),
            piece
          ))
        )
      else
        createTilesInColumn(
          file + 1,
          map + (Position(File.fromIntUnsafe(file), Rank.fromIntUnsafe(rank)) -> Tile(
            Position(File.fromIntUnsafe(file), Rank.fromIntUnsafe(rank)),
            piece
          ))
        )
    }

    createTilesInColumn(1, Map())
  }
}
