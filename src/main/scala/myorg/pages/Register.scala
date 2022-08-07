package myorg.pages

import myorg.algebras.UserAlg
import tyrian.*
import tyrian.Html.*
import myorg.Router
import Register.Msg.*

object Register {

  def update[F[_]: UserAlg](msg: Msg, model: Model): (Model, Cmd[F, Msg]) = {
    msg match
      case Msg.EnteredUsername(username) => (model, Cmd.None)
      case Msg.EnteredEmail(email)       => (model, Cmd.None)
      case Msg.EnteredPassword(password) => (model, Cmd.None)
      case Msg.SubmittedForm             => (model, Cmd.None)
  }

  private def init(): Model = {
    Model(List(), Form("", "", ""))
  }

  def view(model: Model): Html[Msg] = {
    div(
      attribute("class", "cred-page")
    )(
      div(
        attribute("class", "row")
      )(
        div(attribute("class", "col-md-6 offset-md-3 col-xs-12"))(
          h1(attribute("class", "text-cs-center"))(text("Sign up")),
          p(attribute("class", "text-cs-center"))(
            a(Router.Login.href)(
              text("Have an account?")
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
    Html.form(
      input(
        `class`     := "form-control form-control-lg",
        placeholder := "Username",
        Html.onInput(EnteredUsername.apply),
        Property("value", form.username)
      ),
      input(
        `class`     := "form-control form-control-lg",
        placeholder := "Email",
        Html.onInput(EnteredUsername.apply),
        Property("value", form.username)
      ),
      input(
        `class`     := "form-control form-control-lg",
        placeholder := "Password",
        `type`      := "password",
        Html.onInput(EnteredUsername.apply),
        Property("value", form.username)
      )
    )

  }

  final case class Model(
      problems: List[Problem],
      form: Form
  )

  final case class Form(email: String, username: String, password: String)

  enum Problem {
    case InvalidEntry(field: ValidatedField, errorDescription: String)
    case ServerError(description: String)

    def renderError[Msg]: Html[Msg] = this match {
      case InvalidEntry(_, errorDescription) => li(errorDescription)
      case ServerError(description)          => li(description)
    }
  }

  enum ValidatedField {
    case Username, Email, Password
  }

  enum Msg {
    case EnteredUsername(username: String)
    case EnteredEmail(email: String)
    case EnteredPassword(password: String)
    case SubmittedForm
  }

}
