package com.evolution.project.domain.user

import io.circe.generic.JsonCodec
import io.circe.syntax.EncoderOps
import io.circe.{Encoder, Json}

import java.util.UUID

sealed trait UserError extends Throwable
object UserError {
  case object UserValidationError extends UserError
  case object UserNotFoundError extends UserError
  case object NegativeBalance extends UserError
}

sealed abstract case class User private (id: UUID, name: String, balance: Int)
object User {
  def of(id: UUID, name: String, balance: Int): Either[UserError, User] = {
    if (name.nonEmpty && balance >= 0)  Right(new User(id, name, balance) {})
    else                                Left(UserError.UserValidationError)
  }

  implicit val UserEncoder: Encoder[User] =
    Encoder.instance { (user: User) =>
      Json.obj(
        "id" -> user.id.asJson,
        "name" -> user.name.asJson,
        "balance" -> user.balance.asJson
      )
    }
}

@JsonCodec
case class UserParams(name: String, balance: Int)
