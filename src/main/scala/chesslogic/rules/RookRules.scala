package chesslogic.rules

import chesslogic.Color
import chesslogic.board.{Board, MoveType, Position}
import chesslogic.pieces.Piece.*
import chesslogic.rules.RulesForMultiTileMoves.{
  getViablePositionForAttacks,
  getViablePositionsForMoves
}

object RookRules extends MovingRules[Rook] {
  override def getPossibleAttacks(
      position: Position,
      board: Board
  ): List[(MoveType, Position)] =
    (for {
      _ <- Some(())
      tile = board.getTile(position)
      piece <- tile.currentPiece
    } yield List(
      getTopAttack(board, position, piece.color),
      getLeftAttack(board, position, piece.color),
      getRightAttack(board, position, piece.color),
      getDownAttack(board, position, piece.color)
    ).collect { case Some(position) => (MoveType.Normal, position) })
      .getOrElse(Nil)

  override def getPossibleMoves(
      position: Position,
      board: Board
  ): List[(MoveType, Position)] =
    (getDownMoves(position, board) ++ getTopMoves(position, board) ++
      getLeftMoves(position, board) ++ getRightMoves(position, board)).map(p =>
      (MoveType.Normal, p)
    )

  private def getRightMoves(position: Position, board: Board): List[Position] =
    getViablePositionsForMoves(board, position, 0, 1)

  private def getLeftMoves(position: Position, board: Board): List[Position] =
    getViablePositionsForMoves(board, position, 0, -1)

  private def getTopMoves(position: Position, board: Board): List[Position] =
    getViablePositionsForMoves(board, position, 1, 0)

  private def getDownMoves(position: Position, board: Board): List[Position] =
    getViablePositionsForMoves(board, position, -1, 0)

  private def getRightAttack(
      board: Board,
      position: Position,
      color: Color
  ): Option[Position] =
    getViablePositionForAttacks(board, position, 0, 1, color)

  private def getDownAttack(
      board: Board,
      position: Position,
      color: Color
  ): Option[Position] =
    getViablePositionForAttacks(board, position, -1, 0, color)

  private def getLeftAttack(
      board: Board,
      position: Position,
      color: Color
  ): Option[Position] =
    getViablePositionForAttacks(board, position, 0, -1, color)

  private def getTopAttack(
      board: Board,
      position: Position,
      color: Color
  ): Option[Position] =
    getViablePositionForAttacks(board, position, 1, 0, color)

}
