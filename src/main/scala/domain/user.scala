package domain
import java.util.UUID
import cats.implicits.*
object user {
  final case class UserId(value: UUID)
  final case class UserName private (value: String)
  object UserName {
    def make(s: String): Option[UserName] =
      if (s.length > 3)
        UserName(s).some
      else None

  }
  final case class Password private (value: String)
  final case class Email private (value: String)
  final case class JwtToken(value: String)
  final case class Profile(arg: UserName, token: JwtToken)

}
