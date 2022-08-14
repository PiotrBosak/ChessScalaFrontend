package myorg.pages
import tyrian.Html.*
import tyrian.Html
object Commons {
  
  enum Problem {
    case InvalidEntry[Field](field: Field, errorDescription: String)
    case ServerError(description: String)

    def renderError[Msg]: Html[Msg] = this match {
      case InvalidEntry(_, errorDescription) => li(errorDescription)
      case ServerError(description)          => li(description)
    }
}
}
