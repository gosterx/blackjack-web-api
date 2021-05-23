package com.evolution.project.database

import com.evolution.project.domain.user.{User, UserError}
import doobie._

import java.util.UUID

object Mappers {

  implicit val uuidMeta: Meta[UUID] = Meta[String].timap(UUID.fromString)(_.toString)
  implicit val userRead: Read[Either[UserError, User]] = Read[(UUID, String, Int)].map{
    case (id, name, balance) => User.of(id, name, balance)
  }
}
