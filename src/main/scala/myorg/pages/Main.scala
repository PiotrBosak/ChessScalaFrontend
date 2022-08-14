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
import myorg.Route
import tyrian.Html.*
import chesslogic.board.MoveType.*
import myorg.pages.Login as LoginPage
import myorg.pages.Register as RegisterPage
import myorg.pages.Main.Model.*
import scala.scalajs.js.annotation.*
import chesslogic.board.MoveType
import chesslogic.game.FullGame.Turn
import Main.Model
import Main.Msg
import myorg.SelectionState.*
import myorg.algebras.UserAlg.*
import tyrian.Navigation.Result

@JSExportTopLevel("TyrianApp")
object Main extends TyrianApp[Msg, Model] {

  val address = "localhost:8080"

  enum Model {
    case Login(model: LoginPage.Model)
    case Register(model: RegisterPage.Model)
  }

  enum Msg {
    case NoOp
    case SwitchPage(page: Page)
  }

  enum Page {
    case Login
    case Register
  }
  def update(model: Model): Msg => (Model, Cmd[IO, Msg]) =
    msg => msg match {
      case Msg.SwitchPage(Page.Register) => 
        (Register(RegisterPage.init()), Cmd.None)
      case Msg.SwitchPage(Page.Login) =>
        (Login(LoginPage.init()), Cmd.None)
      case _ => (model, Cmd.None)

    }

  def init(flags: Map[String, String]): (Model, Cmd[cats.effect.IO, Msg]) =
    (Model.Login(LoginPage.init()), Cmd.None)
  def subscriptions(model: Model): Sub[cats.effect.IO, Msg] =
    Navigation.onLocationHashChange(change => {
      change match {
        case Result.HashChange(_, _, _, newFragment) =>
          Route.fromString(newFragment) match {
            case Some(Route.Register) => Msg.SwitchPage(Page.Register)
            case Some(Route.Login)    => Msg.SwitchPage(Page.Login)
            case _                    => Msg.NoOp
          }
      }

    })
  def view(model: Model): Html[Msg] = {
    model match {
      case Login(model) => LoginPage.view(model).map(_ => Msg.NoOp)
      case Register(model)  => RegisterPage.view(model).map(_ => Msg.NoOp)
    }

  }

}
