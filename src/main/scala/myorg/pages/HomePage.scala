package myorg.pages
import myorg.algebras.StartGameAlg
import tyrian.*
import domain.*
import tyrian.Html.*
import myorg.misc.Session
import myorg.Route
import cats.syntax.all.*
import cats.data.*
import domain.user.*
import HomePage.*
import myorg.pages.Commons.*
import myorg.pages.Commons.Problem.*

object HomePage {

  def update[F[_]: StartGameAlg](msg: Msg, model: Model): (Model, Cmd[F, Msg]) = {
    (model, Cmd.None)
  }

  enum Msg {
    case EmptyForNow
  }

  final case class Model()

  def init(): Model = Model()

  def view(model: Model): Html[Msg] = {
    div(
      text("Hello")
    )
  }

}
