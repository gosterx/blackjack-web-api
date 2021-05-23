package com.evolution.project.router

import cats.effect.IO
import com.evolution.project.domain.game.{BettingParams, HitParams}
import com.evolution.project.service.GameService
import org.http4s.HttpRoutes
import org.http4s.dsl.io._

object BettingRouter {

  def of(gameService: GameService[IO]): HttpRoutes[IO] = {
    import org.http4s.circe.CirceEntityCodec._

    HttpRoutes.of[IO]{

      case req@ POST -> Root / "bet" =>
        for {
          bettingP <- req.as[BettingParams]
          _        <- IO(println(bettingP))
          game     <- gameService.getGameByTableName(bettingP.tableName)
          response     <- game.doBet(bettingP.userName, bettingP.bet) *> Ok()
        } yield response

      case req@ POST -> Root / "hit" =>
        for {
          hitP  <- req.as[HitParams]
          game  <- gameService.getGameByTableName(hitP.tableName)
          response <- game.doHit(hitP.userName) *> Ok()
        } yield response

      case req@ POST -> Root / "resign" =>
        for {
          hitP  <- req.as[HitParams]
          game  <- gameService.getGameByTableName(hitP.tableName)
          response <- game.doResign(hitP.userName) *> Ok()
        } yield response
    }
  }

}
