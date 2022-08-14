package domain

import java.util.UUID
import chesslogic.board.Position.apply
import chesslogic.board.Position
import domain.user.UserId

object game {
 
  final case class MakeMove(from: Position, to: Position)
  final case class GameId(value: UUID)

  final case class GameData(userId: UserId, gameId: GameId)
}
