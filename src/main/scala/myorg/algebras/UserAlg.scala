package myorg.algebras

import domain.user.{ Email, JwtToken, Password, UserName }
import domain.user.Profile

trait UserAlg[F[_]] {

  def login(username: UserName, password: Password): F[Option[Profile]]

  def register(username: UserName, email: Email, password: Password): F[Option[Profile]]

  def logout(jwt: JwtToken): F[Unit]

}
