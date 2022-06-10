package chesslogic.game

import cats.data.NonEmptyList
import chesslogic.Color.*
import cats.syntax.all.*
import cats.*
import cats.derived.semiauto.derived
import chesslogic.board.{Board, Move, Position}
import chesslogic.game.FullGame.Turn
import chesslogic.game.FullGame.Turn.{BlackTurn, WhiteTurn}
import io.circe.Codec

case class FullGame(
    gameHistory: NonEmptyList[Board] = NonEmptyList.one(Board()),
    turn: Turn = WhiteTurn
) {

  val currentBoard: Board = gameHistory.head

  def makeMove(from: Position, to: Position): Option[FullGame] = {
    val currentBoard        = gameHistory.head
    val possibleMovesOption = currentBoard.getPossibleMoves(from)
    for {
      possibleMoves   <- possibleMovesOption
      (moveType, pos) <- possibleMoves.find(p => p._2 == to)
      tileToMove = currentBoard.getTile(to)
      tileFrom   = currentBoard.getTile(from)
      attackingPiece <- tileFrom.currentPiece
      isColorCorrect =
        if (attackingPiece.color == White) turn == WhiteTurn
        else turn == BlackTurn
      newBoard <- currentBoard.getBoardAfterMove(
        moveType,
        tileFrom,
        tileToMove
      ) if isColorCorrect
    } yield FullGame(newBoard :: this.gameHistory, turn = turn.changeTurn)
  }

  def makeMoveWithoutTurn(from: Position, to: Position): Option[FullGame] = {
    val currentBoard        = gameHistory.head
    val possibleMovesOption = currentBoard.getPossibleMoves(from)
    for {
      possibleMoves   <- possibleMovesOption
      (moveType, pos) <- possibleMoves.find(p => p._2 == to)
      tileToMove = currentBoard.getTile(to)
      tileFrom   = currentBoard.getTile(from)
      newBoard <- currentBoard.getBoardAfterMove(
        moveType,
        tileFrom,
        tileToMove
      )
    } yield FullGame(newBoard :: this.gameHistory, turn = turn.changeTurn)

  }

}

object FullGame {

  enum Turn derives Codec.AsObject:
    case WhiteTurn
    case BlackTurn

    def changeTurn: Turn = this match
      case WhiteTurn => BlackTurn
      case BlackTurn => WhiteTurn

}
