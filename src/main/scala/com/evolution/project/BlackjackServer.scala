package com.evolution.project


import cats.effect.{ExitCode, IO, IOApp}
import cats.implicits.toSemigroupKOps
import com.evolution.project.database.DBTransactor
import com.evolution.project.repository.UserRepository
import com.evolution.project.router._
import com.evolution.project.service.{GameService, TableService}
import org.http4s.HttpApp
import org.http4s.implicits.http4sKleisliResponseSyntaxOptionT
import org.http4s.server.blaze.BlazeServerBuilder

import scala.concurrent.ExecutionContext

object BlackjackServer extends IOApp {

  def routers(userRepo: UserRepository[IO], tableService: TableService[IO], gameService: GameService[IO]): HttpApp[IO] = {
    UserRouter.of(userRepo) <+>
      TableRouter.of(tableService) <+>
      TableSelectionRouter.of(tableService, userRepo, gameService) <+>
      GameRouter.of(gameService) <+>
      BettingRouter.of(gameService)
  }.orNotFound

  override def run(args: List[String]): IO[ExitCode] = {
    DBTransactor.make[IO]
      .use { tx =>
        for {
          userRepo <- UserRepository.of[IO](tx)
          tableService <- TableService.of[IO]
          gameService  <- GameService.of[IO]()
          _ <- httpServer(routers(userRepo, tableService, gameService))
        } yield ()
      }.as(ExitCode.Success)
  }

  def httpServer(routes: HttpApp[IO]): IO[Unit] =
    BlazeServerBuilder[IO](ExecutionContext.global)
      .bindHttp(port = 9001, host = "localhost")
      .withHttpApp(routes)
      .serve
      .compile
      .drain
}

