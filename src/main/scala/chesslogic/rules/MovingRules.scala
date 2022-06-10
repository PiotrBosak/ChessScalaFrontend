package chesslogic.rules

import chesslogic.board.{ Board, MoveType, Position, Tile }

trait MovingRules[Piece] {
  def getPossibleMoves(position: Position, board: Board): List[(MoveType, Position)]
  def getPossibleAttacks(position: Position, board: Board): List[(MoveType, Position)]
}
