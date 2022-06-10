package chesslogic.rules

import chesslogic.Color
import chesslogic.Color.*
import chesslogic.board.Rank.*
import chesslogic.board.File.*
import chesslogic.board.{Board, Move, MoveType, Position, Tile}
import chesslogic.pieces.Piece.*
import chesslogic.board.MoveType
import cats.implicits.*

//todo it should be a typeclass, that way it doesn't make much sense
object PawnRules extends MovingRules[Pawn] {
  override def getPossibleAttacks(
      position: Position,
      board: Board
  ): List[(MoveType, Position)] =
    List(
      getLeftAttack(position, board),
      getRightAttack(position, board),
      lePassant(position, board)
    )
      .collect { case Some((moveType, position)) => (moveType, position) }

  override def getPossibleMoves(
      position: Position,
      board: Board
  ): List[(MoveType, Position)] =
    List(oneTileMove(position, board), twoTileMove(position, board))
      .collect { case Some((moveType, tile)) => (moveType, tile.position) }

  private def getRightAttack(
      position: Position,
      board: Board
  ): Option[(MoveType, Position)] =
    for {
      _ <- Option(())
      tile = board.getTile(position)
      piece           <- tile.currentPiece
      rightAttackTile <- getRightAttackTile(position, board, piece.color)
      attackedPiece   <- rightAttackTile.currentPiece
      if attackedPiece.color != piece.color
    } yield (MoveType.Normal, rightAttackTile.position)
  private def getRightAttackTile(
      position: Position,
      board: Board,
      color: Color
  ): Option[Tile] =
    color match
      case White =>
        (position.file.advance(1), position.rank.advance(1))
          .mapN(Position.apply)
          .map(board.getTile)
      case Black =>
        (position.file.advance(1), position.rank.advance(-1))
          .mapN(Position.apply)
          .map(board.getTile)

  private def lePassant(
      position: Position,
      board: Board
  ): Option[(MoveType, Position)] =
    (lePassantGet(position, board, isLeft = true) orElse lePassantGet(
      position,
      board,
      isLeft = false
    ))
      .map(p => (MoveType.LePassant, p))

  private def lePassantGet(
      position: Position,
      board: Board,
      isLeft: Boolean
  ): Option[Position] = {
    val difference = if (isLeft) -1 else 1
    for {
      _ <- Some(())
      tile = board.getTile(position)
      piece <- tile.currentPiece
      attackingColor = piece.color
      lastMove <- board.previousMove
      _ = s"before checking, previous move was: $lastMove"
      positionToLeft <- position.file
        .advance(difference)
        .map(file => position.copy(file = file))
      if checkTileForLePassant(lastMove, attackingColor, positionToLeft, board)
      _              = "Checking successful"
      rankDifference = if (attackingColor == White) 1 else -1
      positionToMove <- positionToLeft.rank
        .advance(rankDifference)
        .map(rank => positionToLeft.copy(rank = rank))
    } yield positionToMove
  }

  private def checkTileForLePassant(
      move: Move,
      attackingColor: Color,
      attackingTilePosition: Position,
      board: Board
  ): Boolean = {
    val startingEnemyRank      = if (attackingColor == White) Seven else Two
    val isRowTheSame           = move.to.rank == attackingTilePosition.rank
    val isPreviousStartCorrect = move.from.rank == startingEnemyRank
    val isColumnTheSame        = move.to.file == attackingTilePosition.file

    val isEnemyPawn = board.findTile(move.to).currentPiece match {
      case Some(piece: Pawn) => piece.color != attackingColor
      case _                 => false
    }
    isRowTheSame && isPreviousStartCorrect && isEnemyPawn && isColumnTheSame

  }

  private def getLeftAttack(
      position: Position,
      board: Board
  ): Option[(MoveType, Position)] =
    for {
      _ <- Option(())
      tile = board.getTile(position)
      piece          <- tile.currentPiece
      leftAttackTile <- getLeftAttackTile(position, board, piece.color)
      attackedPiece  <- leftAttackTile.currentPiece
      if attackedPiece.color != piece.color
    } yield (MoveType.Normal, leftAttackTile.position)
  private def getLeftAttackTile(
      position: Position,
      board: Board,
      color: Color
  ): Option[Tile] =
    color match
      case White =>
        (position.file.advance(-1), position.rank.advance(1))
          .mapN(Position.apply)
          .map(board.getTile)
      case Black =>
        (position.file.advance(-1), position.rank.advance(-1))
          .mapN(Position.apply)
          .map(board.getTile)

  private def oneTileMove(
      position: Position,
      board: Board
  ): Option[(MoveType, Tile)] =
    for {
      _ <- Option(())
      tile = board.getTile(position)
      piece <- tile.currentPiece
      color = piece.color
      nextTile <- getNextPawnTile(tile.position, color, board, 1)
      if nextTile.currentPiece.isEmpty
    } yield (MoveType.Normal, nextTile)

  private def getNextPawnTile(
      position: Position,
      pieceColor: Color,
      board: Board,
      amount: Int
  ): Option[Tile] =
    pieceColor match {
      case White =>
        position.rank
          .advance(amount)
          .map(rank => position.copy(rank = rank))
          .map(board.getTile(_))
      case Black =>
        position.rank
          .advance(-amount)
          .map(rank => position.copy(rank = rank))
          .map(board.getTile(_))
    }

  private def twoTileMove(
      position: Position,
      board: Board
  ): Option[(MoveType, Tile)] = {
    val pieceTileOption = board.getTile(position)
    for {
      _ <- Some(())
      tile = pieceTileOption if !tile.hasMoved
      piece <- tile.currentPiece
      color = piece.color
      nextTile <- getNextPawnTile(tile.position, color, board, 2)
      if nextTile.currentPiece.isEmpty
    } yield (MoveType.TwoTileMove, nextTile)
  }

}
