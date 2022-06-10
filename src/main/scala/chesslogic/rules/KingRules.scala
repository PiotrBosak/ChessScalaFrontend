package chesslogic.rules

import chesslogic.board.{ Board, MoveType, Position }
import chesslogic.pieces.Piece.*
import chesslogic.board.File.*
import chesslogic.board.File
import chesslogic.board.Rank.*
import chesslogic.board.Rank
import chesslogic.rules.CheckAndMateRules.isKingChecked
import chesslogic.rules.RulesForKingAndKnight.getAllMoves

object KingRules extends MovingRules[King] {
  private val combinations = (for {
    x <- -1 to 1
    y <- -1 to 1
  } yield (x, y)).distinct.toList

  override def getPossibleMoves(position: Position, board: Board): List[(MoveType, Position)] = {
    val castlingMoves =
      List(castlingMove(position, board, isLeftRook = true), castlingMove(position, board, isLeftRook = false))
        .collect { case Some((moveType, position)) =>
          (moveType, position)
        }

    (getAllMoves(position, board, combinations)._1).map(p => (MoveType.Normal, p)) ++ castlingMoves
  }

  override def getPossibleAttacks(position: Position, board: Board): List[(MoveType, Position)] =
    getAllMoves(position, board, combinations)._2.map(p => (MoveType.Normal, p))

  private def castlingMove(position: Position, board: Board, isLeftRook: Boolean): Option[(MoveType, Position)] = {
    val rookFile            = if isLeftRook then A else H
    val newKingPositionFile = if isLeftRook then B else G
    for {
      _ <- Some(())
      kingTile     = board.getTile(position) if !kingTile.hasMoved
      kingPosition = kingTile.position
      rookPosition = Position(rookFile, kingPosition.rank)
      rookTile <- getRookTileOption(kingPosition, board, rookPosition.file)
      if (areTilesClear(kingPosition, rookTile.position, board) &&
        !isKingChecked(board, kingTile.currentPiece.get.color))
      newKingPosition = Position(newKingPositionFile, rookTile.position.rank)
    } yield (MoveType.Castling, newKingPosition)
  }

  private def areTilesClear(from: Position, to: Position, board: Board): Boolean = {
    val tiles = board.tiles.values.filter(tile => {
      val p = tile.position
      p.rank == from.rank && isBetween(p.file.toNumber, from, to)
    })
    tiles.forall(!_.hasPiece)
  }

  private def isBetween(file: Int, from: Position, to: Position): Boolean =
    val smaller = Math.min(from.file.toNumber, to.file.toNumber)
    val bigger  = Math.max(from.file.toNumber, to.file.toNumber)
    (smaller + 1 until bigger).contains(file)

  private def getRookTileOption(position: Position, board: Board, file: File) =
    for {
      _ <- Some(())
      tile         = board.getTile(position)
      rookPosition = Position(file, tile.position.rank)
      rookTile     = board.getTile(rookPosition) if !rookTile.hasMoved
    } yield rookTile

}
