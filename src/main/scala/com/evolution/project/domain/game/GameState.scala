package com.evolution.project.domain.game

import com.evolution.project.domain.user.User
import io.circe.generic.auto.exportEncoder
import io.circe.syntax.EncoderOps
import io.circe.{Encoder, Json}

case class GameState(turn: User, betState: Map[String, Int], userGameState: Map[String, UserGameState], dealerGameState: DealerGameState, result: Map[String, GameResult])
object GameState {
  implicit val GameStateEncoder: Encoder[GameState] =
    Encoder.instance { (gameState: GameState) =>
      Json.obj(
        "turn" -> gameState.turn.asJson,
        "betState" -> gameState.betState.toList.asJson,
        "userGameState" -> gameState.userGameState.toList.asJson,
        "dealerGameState" -> gameState.dealerGameState.asJson,
        "result"          -> gameState.result.toList.asJson
      )
    }
}