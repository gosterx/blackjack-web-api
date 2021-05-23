package com.evolution.project.service

import cats.Monad
import cats.effect.concurrent.Ref
import cats.effect.{IO, Sync}
import cats.syntax.all._
import com.evolution.project.domain.game.Game
import com.evolution.project.domain.user.UserError

trait GameService[F[_]] {
  def addGame(tableName: String, game: Game[IO]): F[Unit]
  def getGameByTableName(tableName: String): F[Game[IO]]
}

object GameService {

  def of[F[_]: Sync](): F[GameService[F]] = {
    Ref.of(Map.empty[String, Game[IO]]).map { games =>
      new GameService[F] {
        override def addGame(tableName: String, game: Game[IO]): F[Unit] =
          for {
            _ <- games.update(map => map.updated(tableName, game))
          } yield ()

        override def getGameByTableName(tableName: String): F[Game[IO]] =
          for {
            games  <- games.get
            _      <- Sync[F].delay(println(games))
            game  <- games.get(tableName) match {
              case Some(value) => Monad[F].pure(value)
              case None =>        Sync[F].raiseError(UserError.UserValidationError)
            }
          } yield game
      }
    }
  }

}
