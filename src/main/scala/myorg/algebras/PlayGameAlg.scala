package myorg.algebras
import PlayGameAlg.*
import domain.game.*

trait PlayGameAlg[F[_]] {

  def move(makeMove: MakeMove, gameData: GameData): F[MakeMoveResult]
  def proposeDraw(gameData: GameData): F[ProposeDrawResult]
  def answerDrawProposal(gameData: GameData): F[DrawProposalAnswerResult]
  def fortfeit(gameData: GameData): F[FortfeitResult]

}

object PlayGameAlg {


  enum MakeMoveResult {
    case ValidMove
    case InvalidMove
  }

  enum ProposeDrawResult {
    case Accepted
    case Rejected
  }
  enum DrawProposalAnswerResult {
    case ProposalAnswerSuccessful
    case ProposalAnswerFailed
  }
  enum FortfeitResult {
    case FortfeitSuccessful
    case FortfeitFailed
  }

}
