package myorg

import cats.effect.IO
import myorg.utils.HtmlUtils.*
import chesslogic.pieces.Piece
import chesslogic.pieces.Piece.*
import chesslogic.Color
import chesslogic.Color.*
import chesslogic.board.{Board, Position, Rank, Tile, File}
import chesslogic.board.Rank.*
import chesslogic.board.File.*
import tyrian.*
import tyrian.Html.*
import myorg.Model
import myorg.Msg
import myorg.Msg.*

import scala.scalajs.js.annotation.*

@JSExportTopLevel("TyrianApp")
object Chessfronttyrian extends TyrianApp[Msg, Model] {

  private def initModel: Model = Model(None, Board())

  def init(flags: Map[String, String]): (Model, Cmd[IO, Msg]) =
    (initModel, Cmd.None)

  def update(model: Model): Msg => (Model, Cmd[IO, Msg]) =
    case Select(position) =>
      (
        model.copy(pickedPosition = Some(position)),
        Cmd.None
      )
    case Deselect =>
      (
        model.copy(pickedPosition = None),
        Cmd.None
      )

  def view(model: Model): Html[Msg] = {
    val byRank =
      model.board.tiles.values.toList
        .groupBy(_.position.rank)
        .toList
        .sortBy { case (rank, _) => rank }(summon[Ordering[Rank]].reverse)
        .map { case (rank, tiles) => (rank, tiles.sorted) }

    div(
      div(
        byRank.map { case (_, tiles) =>
          renderRank(model, tiles)
        }
      )
    )
  }

  def subscriptions(model: Model): Sub[IO, Msg] =
    Sub.None

  private def renderRank(
      model: Model,
      tiles: List[Tile]
  ): Html[Msg] = {
    val possibleAttacks =
      model.pickedPosition.fold(Nil)(model.board.possibleValidMoves)
    tr(tiles.map(renderTile(pickedPosition, _)))
  }

  private def renderTile(
      tile: Tile
      pickedPosition: Option[Position],
    po
  ): Html[Msg] = {


    val color = (tile.color, pickedPosition) match

      case (_, Some(position)) if position == tile.position =>
        attribute("style", "background-color: blue")
      case (White, _) => attribute("style", "background-color: white")
      case (Black, _) => attribute("style", "background-color: black")
    td(
      attribute("width", "100"),
      attribute("height", "100"),
      color,
      onClick(
        if (pickedPosition.contains(tile.position)) Msg.Deselect
        else Msg.Select(tile.position)
      )
    )(maybeElem(tile.currentPiece)(pieceImage))
  }

  private def pieceImage(piece: Piece): Html[Msg] = {

    val imageSource = piece match
      case Pawn(White)   => "assets/pieces/whitePawn.png"
      case Pawn(Black)   => "assets/pieces/blackPawn.png"
      case Bishop(White) => "assets/pieces/whiteBishop.png"
      case Bishop(Black) => "assets/pieces/blackBishop.png"
      case Queen(White)  => "assets/pieces/whiteQueen.png"
      case Queen(Black)  => "assets/pieces/blackQueen.png"
      case King(White)   => "assets/pieces/whiteKing.png"
      case King(Black)   => "assets/pieces/blackKing.png"
      case Rook(White)   => "assets/pieces/whiteRook.png"
      case Rook(Black)   => "assets/pieces/blackRook.png"
      case Knight(White) => "assets/pieces/whiteKnight.png"
      case Knight(Black) => "assets/pieces/blackKnight.png"

    img(
      src := imageSource,
      attribute("width", "100"),
      attribute("height", "100")
    )
  }
}

final case class Model(
    pickedPosition: Option[Position],
    board: Board
)

enum Msg:
  case Select(position: Position)
  case Deselect
