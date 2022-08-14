package myorg.algebras

import StartGameResult.*
import scala.concurrent.duration.Duration
import domain.game.GameId
trait StartGameAlg[F[_]] {

  def startMultiGame: F[Unit]
  def pollGame: F[PollGameResult]

}
object StartGameResult {

  enum PollGameResult {
    case NotFoundYet(timeout: Duration)
    case FoundGame(gameId: GameId)
  }
}
