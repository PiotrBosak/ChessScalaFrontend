package chesslogic.rules

import scala.util.Try
import chesslogic.board.{ Board, Position }

protected object RulesForKingAndKnight {

  def getAllMoves(
      position: Position,
      board: Board,
      combinations: List[(Int, Int)]
  ): (List[Position], List[Position]) = {
    val tile = board.getTile(position)
    val possiblePositions = combinations
      .map(t => Try(Position(position.file.advanceUnsafe(t._2), position.rank.advanceUnsafe(t._1))).toOption)
      .map(op => op.map(t => board.getTile(t)))
      .collect { case Some(tile) => tile }
    val attackingPiece = tile.currentPiece.get
    val moves          = possiblePositions.filter(!_.hasPiece).map(_.position)
    val attacks = possiblePositions.foldLeft(List.empty[Position])((acc, t) => {
      t.currentPiece match {
        case Some(piece) =>
          if (piece.color != attackingPiece.color) t.position :: acc
          else acc
        case None => acc
      }
    })
    (moves, attacks)
  }
}
