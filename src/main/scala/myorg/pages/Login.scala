package myorg.pages

import myorg.misc.Session
import tyrian.Cmd
import tyrian.*
import tyrian.Html.*
import cats.data.*
import cats.data.Validated.*
import cats.syntax.all.*
import Login.Msg.*
import myorg.algebras.UserAlg
import tyrian.Html
import domain.user.*
import myorg.Route
import myorg.pages.Commons.Problem
import myorg.pages.Commons.Problem.*

object Login {
  def update[F[_]: UserAlg](msg: Msg, model: Model): (Model, Cmd[F, Msg]) = {
    msg match {
      case Msg.EnteredUsername(username) =>
        (model.copy(form = model.form.copy(username = username)), Cmd.None)
      case NoOp => (model, Cmd.None)
      case Msg.EnteredPassword(password) =>
        (model.copy(form = model.form.copy(password = password)), Cmd.None)
      case Msg.SubmittedForm =>
        println("sumitted")
        validateForm(model.form) match {
          case Valid(loginData) =>
            (
              model,
              Cmd.Run(UserAlg[F].login(loginData))(_ => Msg.SubmittedForm)
            ) // the second argument is wrong, dunno what to do just yet
          case Invalid(problems) =>
            (model.copy(problems = problems.toList), Cmd.None)

        }
      case Msg.GotSession(_) => (model, Cmd.None)

    }
  }

  private def validateForm(form: Form): ValidatedNel[Problem, LoginData] = {
    (
      UserName.make(form.username).toValidNel(InvalidEntry(ValidatedField.Username, "Username is too short")),
      Password.make(form.password).toValidNel(InvalidEntry(ValidatedField.Password, "Invalid password"))
    )
      .mapN(LoginData.apply)

  }

  def init(): Model = Model(List(), Form("", ""))

  def view(model: Model): Html[Msg] = {
    div(attribute("class", "cred-page"))(
      div(
        attribute("class", "row")
      )(
        div(attribute("class", "col-md-6 offset-md-3 col-xs-12"))(
          h1(attribute("class", "text-cs-center"))(text("Log in")),
          p(attribute("class", "text-cs-center"))(
            a(Route.Register.href)(
              text("No account? Sign up")
            )
          ),
          ul(attribute("class", "error-messages"))(
            model.problems.map(_.renderError)
          ),
          viewForm(model.form)
        )
      )
    )
  }

  private def viewForm(form: Form): Html[Msg] = {
    Html.form(onEvent("submit", e => { e.preventDefault(); Msg.NoOp }))(
      input(
        `class`     := "form-control form-control-lg",
        placeholder := "Username",
        Html.onInput(EnteredUsername.apply),
        Property("value", form.username)
      ),
      input(
        `class`     := "form-control form-control-lg",
        placeholder := "Password",
        `type`      := "password",
        Html.onInput(EnteredPassword.apply),
        Property("value", form.username)
      ),
      button(onClick(Msg.SubmittedForm))(text("Sign in"))
    )
  }

  enum ValidatedField {
    case Username
    case Password
  }
  final case class Form(username: String = "", password: String = "")
  final case class Model(problems: List[Problem] = List(), form: Form)
  enum Msg {
    case EnteredUsername(username: String)
    case EnteredPassword(password: String)
    case SubmittedForm
    case GotSession(session: Session)
    case NoOp
  }
}
