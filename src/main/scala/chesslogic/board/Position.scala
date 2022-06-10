package chesslogic.board

import io.circe.syntax.*
import math.Ordered.orderingToOrdered
import scala.math.Ordering
import cats.derived.semiauto.*
import chesslogic.board.Rank.{Eight, Five, Four, One, Seven, Six, Three, Two}
import io.circe.Codec
import cats.kernel.{Eq, Order}

import scala.collection.SeqView.Sorted
import scala.math.Ordering
import scala.util.Try

final case class Position(file: File, rank: Rank) derives Codec.AsObject

object Position:
  given Ordering[Position] =
    (x: Position, y: Position) => {
      import File.*
      import Rank.*
      val compareRank = x.rank.compare(y.rank)
      if (compareRank != 0)
        compareRank
      else x.file.compare(y.file)
    }

enum Rank:
  case One
  case Two
  case Three
  case Four
  case Five
  case Six
  case Seven
  case Eight

  def toNumber: Int = this match
    case Rank.One   => 1
    case Rank.Two   => 2
    case Rank.Three => 3
    case Rank.Four  => 4
    case Rank.Five  => 5
    case Rank.Six   => 6
    case Rank.Seven => 7
    case Rank.Eight => 8

  def advanceUnsafe(n: Int): Rank = Rank.fromIntUnsafe(toNumber + n)
  def advance(n: Int): Option[Rank] = Try(
    Rank.fromIntUnsafe(toNumber + n)
  ).toOption

end Rank

object Rank:
  def fromIntUnsafe(rank: Int): Rank = rank match
    case 1 => One
    case 2 => Two
    case 3 => Three
    case 4 => Four
    case 5 => Five
    case 6 => Six
    case 7 => Seven
    case 8 => Eight
    case _ => throw new RuntimeException("invalid rank")

  given Ordering[Rank] = (x: Rank, y: Rank) => x.toNumber - y.toNumber

enum File:
  case A
  case B
  case C
  case D
  case E
  case F
  case G
  case H

  def toNumber: Int = this match
    case File.A => 1
    case File.B => 2
    case File.C => 3
    case File.D => 4
    case File.E => 5
    case File.F => 6
    case File.G => 7
    case File.H => 8

  def advanceUnsafe(n: Int): File = File.fromIntUnsafe(toNumber + n)
  def advance(n: Int): Option[File] = Try(
    File.fromIntUnsafe(toNumber + n)
  ).toOption

end File

object File:
  def fromIntUnsafe(file: Int): File = file match
    case 1 => A
    case 2 => B
    case 3 => C
    case 4 => D
    case 5 => E
    case 6 => F
    case 7 => G
    case 8 => H
    case _ => throw new RuntimeException("invalid file")

  given Ordering[File] = (x: File, y: File) => x.toNumber - y.toNumber

final case class Move(from: Position, to: Position, moveType: MoveType)
    derives Codec.AsObject
enum MoveType:
  case Normal
  case TwoTileMove
  case Castling
  case LePassant
