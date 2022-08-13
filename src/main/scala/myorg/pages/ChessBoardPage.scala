package myorg

import cats.effect.IO
import myorg.utils.HtmlUtils.*
import chesslogic.pieces.Piece
import chesslogic.pieces.Piece.*
import chesslogic.Color
import chesslogic.Color.*
import chesslogic.board.{ Board, File, Position, Rank, Tile }
import chesslogic.board.Rank.*
import chesslogic.board.File.*
import tyrian.*
import tyrian.Html.*
import myorg.Model
import myorg.Msg
import chesslogic.board.MoveType.*
import myorg.Msg.*

import chesslogic.board.MoveType
import chesslogic.game.FullGame.Turn
import myorg.SelectionState.*

object ChessBoardPage {

  private def initModel: Model = Model(
    SelectionState.Unselected,
    Board(),
    Turn.WhiteTurn,
    PlayerColor.White
  )

  def init(flags: Map[String, String]): (Model, Cmd[IO, Msg]) =
    (initModel, Cmd.None)

  def update(model: Model): Msg => (Model, Cmd[IO, Msg]) = { case Select(position) =>
    updateSelection(model, position)
  }

  private def updateSelection(model: Model, selectedPosition: Position): (Model, Cmd[IO, Msg]) = {

    val selectedPiece = model.board.findTile(selectedPosition).currentPiece
    if (selectedPiece.exists(pieceMatchesTurn(_, model.turn)))
      model.state match {
        case Selected(position, _) if position == selectedPosition =>
          (model.copy(state = Unselected), Cmd.None)
        case Selected(position, possibleMoves) if !possibleMoves.map(_._2).contains(position) =>
          val possibleMoves = model.board.possibleValidMoves(selectedPosition)
          selectNewPosition(model, selectedPosition)
        case Unselected =>
          selectNewPosition(model, selectedPosition)
        case Selected(position, possibleMoves) =>
          makeMove(position, selectedPosition, model, possibleMoves)
      }
    else
      (model, Cmd.SideEffect(IO(println("You can't move like that"))))

  }

  private def pieceMatchesTurn(piece: Piece, turn: Turn): Boolean = {
    (piece.color, turn) match {
      case (Color.White, Turn.WhiteTurn) => true
      case (Color.Black, Turn.BlackTurn) => true
      case _                             => false
    }
  }

  private def selectNewPosition(model: Model, selectedPosition: Position) = {
    val possibleMoves = model.board.possibleValidMoves(selectedPosition)
    (model.copy(state = Selected(selectedPosition, possibleMoves)), Cmd.None)
  }

  private def makeMove(
      currentPosition: Position,
      selectedPosition: Position,
      model: Model,
      possibleMoves: List[(MoveType, Position)]): (Model, Cmd[IO, Msg]) = {
    val boardAfterMove =
      possibleMoves
        .find { case (_, position) => selectedPosition == position }
        .flatMap { case (moveType, selected) => model.board.getBoardAfterMove(moveType, currentPosition, selected) }

    boardAfterMove match {
      case Some(board) =>
        (
          model.copy(
            board = board,
            state = Unselected,
            turn = model.turn.changeTurn
          ),
          Cmd.None
        )
      case None =>
        val possibleMoves = model.board.possibleValidMoves(selectedPosition)
        (
          model.copy(state = Selected(selectedPosition, possibleMoves)),
          Cmd.None
        )
    }
  }

  def view(model: Model): Html[Msg] = {
    val byRank =
      model.board.tiles.values.toList
        .groupBy(_.position.rank)
        .toList
        .sortBy { case (rank, _) => rank }(summon[Ordering[Rank]].reverse)
        .map { case (rank, tiles) => (rank, tiles.sorted) }

    div(
      div(
        byRank.map { case (_, tiles) =>
          renderRankAAA(tilesWithTypes(tiles, model))
        }
      )
    )
  }

  def subscriptions(model: Model): Sub[IO, Msg] =
    Sub.None

  private def tilesWithTypes(
      tiles: List[Tile],
      model: Model
  ): List[(Tile, TileType)] = {
    val possibleMoves = model.state match {
      case Selected(_, moves) => moves
      case _                  => Nil
    }
    tiles.map(tile => (tile, findTileType(tile, model, possibleMoves)))
  }

  private def findTileType(
      tile: Tile,
      model: Model,
      possibleMoves: List[(MoveType, Position)]
  ): TileType = {
    model.state match {
      case Selected(position, _) if position == tile.position =>
        TileType.Selected
      case _ =>
        if (possibleMoves.exists {
            case (Normal | TwoTileMove | Castling, position) =>
              position == tile.position
            case _ => false
          })
          TileType.PossibleMove
        else if (possibleMoves.exists {
            case (Attack | EnPassant, position) => position == tile.position
            case _                              => false
          })
          TileType.PossibleAttack
        else
          TileType.Normal
    }
  }

  private def renderRankAAA(
      tiles: List[(Tile, TileType)]
  ): Html[Msg] = {
    tr(tiles.map { case (tile, tileType) =>
      renderTile(tile, tileType)
    })
  }

  private def renderTile(
      tile: Tile,
      tileType: TileType
  ): Html[Msg] = {

    val hello = (tile.color, tileType) match
      case (_, TileType.Selected) =>
        attribute("style", "background-color: blue")
      case (_, TileType.PossibleAttack) =>
        attribute("style", "background-color: red")
      case (_, TileType.PossibleMove) =>
        attribute("style", "background-color: lime")
      case (White, TileType.Normal) =>
        attribute("style", "background-color: yellow")
      case (Black, TileType.Normal) =>
        attribute("style", "background-color: black")
    td(
      attribute("width", "100"),
      attribute("height", "100"),
      hello,
      onClick(Msg.Select(tile.position))
    )(maybeElem(tile.currentPiece)(pieceImage))
  }

  private def pieceImage(piece: Piece): Html[Msg] = {

    val imageSource = piece match
      case Pawn(White)   => "assets/pieces/whitePawn.png"
      case Pawn(Black)   => "assets/pieces/blackPawn.png"
      case Bishop(White) => "assets/pieces/whiteBishop.png"
      case Bishop(Black) => "assets/pieces/blackBishop.png"
      case Queen(White)  => "assets/pieces/whiteQueen.png"
      case Queen(Black)  => "assets/pieces/blackQueen.png"
      case King(White)   => "assets/pieces/whiteKing.png"
      case King(Black)   => "assets/pieces/blackKing.png"
      case Rook(White)   => "assets/pieces/whiteRook.png"
      case Rook(Black)   => "assets/pieces/blackRook.png"
      case Knight(White) => "assets/pieces/whiteKnight.png"
      case Knight(Black) => "assets/pieces/blackKnight.png"

    img(
      src := imageSource,
      attribute("width", "100"),
      attribute("height", "100")
    )
  }
}

final case class Model(
    state: SelectionState,
    board: Board,
    turn: Turn,
    playerColor: PlayerColor
)

enum PlayerColor {
  case White
  case Black
}
enum SelectionState {
  case Unselected
  case Selected(position: Position, possibleMoves: List[(MoveType, Position)])
}

enum Msg {
  case Select(position: Position)
}

enum TileType:
  case Normal, Selected, PossibleMove, PossibleAttack
