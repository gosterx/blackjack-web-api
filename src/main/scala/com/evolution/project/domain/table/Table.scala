package com.evolution.project.domain.table

import com.evolution.project.domain.user.User
import io.circe.generic.JsonCodec
import io.circe.generic.auto.exportEncoder
import io.circe.syntax.EncoderOps
import io.circe.{Encoder, Json}

case class Table (name: String, maxPlayersCount: Int, status: Status, players: List[User])
object Table {

  implicit val TableEncoder: Encoder[Table] =
    Encoder.instance{ (table: Table) =>
      Json.obj(
        "name" -> table.name.asJson,
        "maxPlayersCount" -> table.maxPlayersCount.asJson,
        "status" -> table.status.asJson,
        "players" -> table.players.asJson,
      )
    }
}

@JsonCodec
case class TableParams(name: String, maxPlayersCount: Int)
