package com.evolution.project.domain.game

import com.evolution.project.domain.deck.Card
import io.circe.generic.auto.exportEncoder
import io.circe.syntax.EncoderOps
import io.circe.{Encoder, Json}

case class DealerGameState(cards: List[Card], score: Int)
object DealerGameState {
  implicit val DealerGameStateEncoder: Encoder[DealerGameState] =
    Encoder.instance { (dealerGameState: DealerGameState) =>
      Json.obj(
        "cards" -> dealerGameState.cards.asJson,
        "score" -> dealerGameState.score.asJson
      )
    }
}