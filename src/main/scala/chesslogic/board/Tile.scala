package chesslogic.board

import cats.implicits.*
import chesslogic.Color.*
import chesslogic.*
import chesslogic.pieces.Piece
import cats.derived.semiauto.*
import io.circe.Codec

case class Tile(position: Position, currentPiece: Option[Piece], hasMoved: Boolean = false) derives Codec.AsObject {

  def isEmpty: Boolean = currentPiece.isEmpty

  def hasPiece: Boolean = currentPiece.isDefined

  val color: Color =
    if ((position.file.toNumber + position.rank.toNumber) % 2 == 0) White
    else Black
  println("Helo")

  def isPieceColorDifferent(another: Option[Tile]): Boolean =
    another.isDefined && another.get.currentPiece.isDefined && another.get.currentPiece.get.color != this.color

  def isPieceColorDifferent(another: Tile): Boolean = isPieceColorDifferent(Some(another))

}
