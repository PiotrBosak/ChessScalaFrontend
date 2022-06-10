package chesslogic

import cats.syntax.all.*
import cats.*
import cats.derived.semiauto.derived
import io.circe.Codec

enum Color derives Codec.AsObject:
  case White
  case Black
