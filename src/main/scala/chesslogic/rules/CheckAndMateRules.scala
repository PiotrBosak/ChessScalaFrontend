package chesslogic.rules

import chesslogic.Color
import chesslogic.board.Board
import chesslogic.game.FullGame
import chesslogic.pieces.Piece.*

object CheckAndMateRules {
  def isKingChecked(board: Board, kingColor: Color): Boolean =
    isKingAttacked(board, kingColor)

  private def isKingAttacked(board: Board, kingColor: Color): Boolean = {
    val kingTile = board.tiles.values
      .find(t =>
        t.currentPiece match {
          case Some(piece) => piece.isInstanceOf[King] && piece.color == kingColor
          case None        => false
        }
      )
      .getOrElse(throw new RuntimeException("king should never be captured!!!"))

    val possibleAttacks = board.tiles.values
      .collect(tile => {
        tile.currentPiece match {
          case Some(piece) if piece.color != kingColor => (piece, tile.position)
        }
      })
      .flatMap(tuple => board.getMovesForPiece(tuple._1, tuple._2))
      .toList

    possibleAttacks.map(_._2).contains(kingTile.position)
  }

  def cannotBeDefended(kingColor: Color, game: FullGame): Boolean = {
    val board = game.currentBoard
    val tilesWithPieces = board.tiles.values
      .collect(t => {
        t.currentPiece match {
          case Some(piece) if piece.color == kingColor => (t, piece)
        }
      })
      .toList
    val allPossibleMoves = tilesWithPieces.map(tuple => {
      (tuple._1.position, board.getMovesForPiece(tuple._2, tuple._1.position))
    })
    val allPossibleScenarios = for {
      moveTuple <- allPossibleMoves
      moveFrom = moveTuple._1
      moveTo  <- moveTuple._2
      newGame <- game.makeMoveWithoutTurn(moveFrom, moveTo._2)
    } yield newGame

    allPossibleScenarios.forall(game => isKingAttacked(game.currentBoard, kingColor))

  }

  def isKingMated(kingColor: Color, game: FullGame): Boolean =
    isKingAttacked(game.currentBoard, kingColor) && cannotBeDefended(kingColor, game)

}
