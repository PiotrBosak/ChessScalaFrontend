package myorg
import tyrian.*
import tyrian.Html.*

enum Route {
  case Home, Login, Logout, Register

  private def routeToString: String = this match {
    case Home     => "#home"
    case Login    => "#login"
    case Logout   => "#logout"
    case Register => "#register"
  }

  def href[Msg]: Attr[Msg] = attribute("href", routeToString)
}
object Route {
  def fromString(s: String): Option[Route] = s match {
    case "#home" => Some(Home)
    case "#login" => Some(Login)
    case "#logout" => Some(Logout)
    case "#register" => Some(Register)
    case _ => None
  }
}
