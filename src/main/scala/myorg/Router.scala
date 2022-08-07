package myorg
import tyrian.*
import tyrian.Html.*

enum Router {
  case Home, Login, Logout, Register

  private def routeToString: String = this match {
    case Home     => "home"
    case Login    => "login"
    case Logout   => "logout"
    case Register => "register"
  }

  def href[Msg]: Attr[Msg] = attribute("href", routeToString)
}
