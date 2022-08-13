package myorg.algebras

import domain.user.*
import domain.user.Profile
import cats.effect.IO

trait UserAlg[F[_]] {

  def login(username: UserName, password: Password): F[Option[Profile]]

  def register(registrationData: RegistrationData): F[Option[Profile]]

  def logout(jwt: JwtToken): F[Unit]

}
object UserAlg {
  def apply[F[_]](using userAlg: UserAlg[F]): UserAlg[F] = summon
  given UserAlg[IO] = new UserAlg[IO] {

    def login(username: UserName, password: Password): IO[Option[Profile]] = IO.pure(None)
    def register(registrationData: RegistrationData): IO[Option[Profile]] = IO.pure(None)
    def logout(jwt: JwtToken): IO[Unit] = IO.unit

  }
}
