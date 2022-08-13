package myorg.pages

import cats.effect.IO
import myorg.utils.HtmlUtils.*
import chesslogic.pieces.Piece
import chesslogic.pieces.Piece.*
import chesslogic.Color
import chesslogic.Color.*
import chesslogic.board.{ Board, File, Position, Rank, Tile }
import chesslogic.board.Rank.*
import chesslogic.board.File.*
import tyrian.*
import tyrian.Html.*
import chesslogic.board.MoveType.*
import myorg.Msg.*

import scala.scalajs.js.annotation.*
import chesslogic.board.MoveType
import chesslogic.game.FullGame.Turn
import myorg.SelectionState.*
import myorg.pages.Register.Model
import myorg.algebras.UserAlg.*
import myorg.pages.Register.Msg

@JSExportTopLevel("TyrianApp")
object MainPage extends TyrianApp[Msg, Model] {
  def update(model: Model): Msg => (Model, Cmd[IO, Msg]) =
    msg => Register.update(msg,model)

  def init(flags: Map[String, String]): (Model, Cmd[cats.effect.IO, Msg]) = (Register.init(),Cmd.None)
  def subscriptions(model: Model): Sub[cats.effect.IO, Msg] = Sub.None
  def view(model: Model): Html[Msg] = Register.view(model)

}
