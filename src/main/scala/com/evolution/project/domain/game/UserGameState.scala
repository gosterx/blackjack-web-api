package com.evolution.project.domain.game

import com.evolution.project.domain.deck.Card
import io.circe.generic.auto.exportEncoder
import io.circe.syntax.EncoderOps
import io.circe.{Encoder, Json}


case class UserGameState(cards: List[Card], score: Int)
object UserGameState {
  implicit val UserGameStateEncoder: Encoder[UserGameState] =
    Encoder.instance { (userGameState: UserGameState) =>
      Json.obj(
        "cards" -> userGameState.cards.asJson,
        "score" -> userGameState.score.asJson
      )
    }
}