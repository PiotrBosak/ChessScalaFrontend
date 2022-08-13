package myorg.pages
import tyrian.*
import domain.*
import myorg.algebras.UserAlg
import tyrian.Html.*
import myorg.misc.Session
import myorg.Router
import cats.syntax.all.*
import cats.data.*
import cats.data.Validated.*
import Register.Msg.*
import domain.user.*
import myorg.pages.Register.Problem.*

object Register {

  def update[F[_]: UserAlg](msg: Msg, model: Model): (Model, Cmd[F, Msg]) = {
    println(msg)
    msg match
      case Msg.EnteredUsername(username) => (model, Cmd.None)
      case Msg.EnteredEmail(email)       => (model, Cmd.None)
      case NoOp                          => (model, Cmd.None)
      case Msg.EnteredPassword(password) =>
        (model, Cmd.None)
      case Msg.SubmittedForm =>
        println("sumitted")
        validateForm(model.form) match {
          case Valid(registrationData) =>
            (
              model,
              Cmd.Run(UserAlg[F].register(registrationData))(_ => Msg.SubmittedForm)
            ) // the second argument is wrong, dunno what to do just yet
          case Invalid(problems) =>
            (model.copy(problems = problems.toList), Cmd.None)

        }
      case Msg.GotSession(_) => (model, Cmd.None)
  }

  def init(): Model = {
    Model(List(), Form("aaa", "", "a"))
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
    Html.form(onEvent("submit", e => { e.preventDefault(); Msg.NoOp }))(
      input(
        `class`     := "form-control form-control-lg",
        placeholder := "Username",
        Html.onInput(EnteredUsername.apply),
        Property("value", form.username)
      ),
      input(
        `class`     := "form-control form-control-lg",
        placeholder := "Email",
        Html.onInput(EnteredEmail.apply),
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

  private def validateForm(form: Form): ValidatedNel[Problem, RegistrationData] = {
    (
      UserName.make(form.username).toValidNel(InvalidEntry(ValidatedField.Username, "Username is too short")),
      Email.make(form.email).toValidNel(InvalidEntry(ValidatedField.Email, "Invalid email")),
      Password.make(form.password).toValidNel(InvalidEntry(ValidatedField.Password, "Invalid password"))
    )
      .mapN(RegistrationData.apply)

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
    case GotSession(session: Session)
    case NoOp
  }

}
