package myorg

import cats.effect.IO
import tyrian.*
import tyrian.Html.*

import scala.scalajs.js.annotation.*

@JSExportTopLevel("TyrianApp")
object Chessfronttyrian extends TyrianApp[Msg, Model]:

  def init(flags: Map[String, String]): (Model, Cmd[IO, Msg]) =
    (0, Cmd.None)

  def update(model: Model): Msg => (Model, Cmd[IO, Msg]) =
    case Msg.Increment => (model + 1, Cmd.None)
    case Msg.Decrement => (model - 1, Cmd.None)

  def view(model: Model): Html[Msg] =
    div(
      button(onClick(Msg.Decrement))(
        ""
//        board.findTile(Position(A, Eight)).toString
      ),
      div(model.toString),
      button(onClick(Msg.Increment))("+")
    )

  def subscriptions(model: Model): Sub[IO, Msg] =
    Sub.None

type Model = Int
//final case class Model(
//    pickedPosition: Option[Position],
//    board: Board
//)

//private def getPieceImage(piece: Piece): Attribute =
//  piece match
//    case Pawn(White)   => src := "dist/static/whitePawn.png"
//    case Pawn(Black)   => src := "dist/static/blackPawn.png"
//    case Bishop(White) => src := "dist/static/whiteBishop.png"
//    case Bishop(Black) => src := "dist/static/blackBishop.png"
//    case Queen(White)  => src := "dist/static/whiteQueen.png"
//    case Queen(Black)  => src := "dist/static/blackQueen.png"
//    case King(White)   => src := "dist/static/whiteKing.png"
//    case King(Black)   => src := "dist/static/blackKing.png"
//    case Rook(White)   => src := "dist/static/whiteRook.png"
//    case Rook(Black)   => src := "dist/static/blackRook.png"
//    case Knight(White) => src := "dist/static/whiteKnight.png"
//    case Knight(Black) => src := "dist/static/blackKnight.png"

enum Msg:
  case Increment, Decrement
