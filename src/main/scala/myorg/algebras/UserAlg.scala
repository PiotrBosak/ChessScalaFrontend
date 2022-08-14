package myorg.algebras

import domain.user.*
import domain.user.Profile
import org.http4s.ember.client.EmberClientBuilder
import cats.effect.IO
import myorg.pages.Main

trait UserAlg[F[_]] {

  def login(loginData: LoginData): F[Option[Profile]]

  def register(registrationData: RegistrationData): F[Option[Profile]]

  def logout(jwt: JwtToken): F[Unit]

}
object UserAlg {
  def apply[F[_]](using userAlg: UserAlg[F]): UserAlg[F] = summon
  given UserAlg[IO] = new UserAlg[IO] {

    def login(loginData: LoginData): IO[Option[Profile]] = ???
      // EmberClientBuilder.default[IO].build.use { client => 
      //   client.expect[JwtToken](Main.address)
      // }

    def register(registrationData: RegistrationData): IO[Option[Profile]] = IO.pure(None)
    def logout(jwt: JwtToken): IO[Unit] = IO.unit

  }
}
