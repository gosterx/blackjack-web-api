package com.evolution.project.router

import cats.effect.IO
import com.evolution.project.domain.game.Game
import com.evolution.project.domain.user.UserJoinParams
import com.evolution.project.repository.UserRepository
import com.evolution.project.service.{GameService, TableService}
import org.http4s.HttpRoutes
import org.http4s.circe.CirceEntityCodec.circeEntityDecoder
import org.http4s.dsl.io._

object TableSelectionRouter {

  def of(tableService: TableService[IO], userRepository: UserRepository[IO], gameService: GameService[IO]): HttpRoutes[IO] = {

    HttpRoutes.of[IO]{

      case req@POST -> Root / "user" / "join" / "table" =>
        for {
          startP <- req.as[UserJoinParams]
          userE   <- userRepository.getUser(startP.userName)
          response       <- userE match {
            case Left(_) => BadRequest()
            case Right(value) => tableService.join(value, startP.tableName) *> {
              for {
                table <- tableService.getTableByName(startP.tableName)
                _     <- IO(println(table))
                _     <- if (table.maxPlayersCount == table.players.length) {
                  for {
                    game <- Game.of[IO](table.players, userRepository)
                    _    <- gameService.addGame(table.name, game)
                  } yield ()
                } else IO()
              } yield ()
            } *> Ok()
          }
        } yield response

    }
  }

}
