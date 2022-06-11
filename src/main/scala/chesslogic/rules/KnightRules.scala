package chesslogic.rules

import chesslogic.board.{Board, MoveType, Position}
import chesslogic.pieces.Piece.*
import chesslogic.rules.RulesForKingAndKnight.getAllMoves

object KnightRules extends MovingRules[Knight] {
  private val combinations: List[(Int, Int)] =
    List((2, 1), (1, 2), (2, -1), (1, -2), (-2, 1), (-2, -1), (-1, -2), (-1, 2))

  override def getPossibleMoves(
      position: Position,
      board: Board
  ): List[(MoveType, Position)] =
    getAllMoves(position, board, combinations)._1.map(p => (MoveType.Normal, p))

  override def getPossibleAttacks(
      position: Position,
      board: Board
  ): List[(MoveType, Position)] =
    getAllMoves(position, board, combinations)._2.map(p => (MoveType.Attack, p))

}
