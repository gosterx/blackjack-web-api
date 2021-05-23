package com.evolution.project.router

import cats.effect.IO
import com.evolution.project.service.GameService
import org.http4s.HttpRoutes
import org.http4s.dsl.io._

object GameRouter {

  def of(gameService: GameService[IO]): HttpRoutes[IO] = {
    import org.http4s.circe.CirceEntityCodec._

    HttpRoutes.of[IO]{

      case GET -> Root / tableName / "game" =>
        for {
          _            <- IO(println(tableName))
          game         <- gameService.getGameByTableName(tableName)
          gameState    <- game.getGameState
          _            <- IO(println(gameState))
          response     <- Ok(gameState)
        } yield response
    }
  }

}
