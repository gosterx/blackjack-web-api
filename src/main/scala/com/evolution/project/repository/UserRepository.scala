package com.evolution.project.repository

import cats.Monad
import cats.effect.Sync
import cats.syntax.all._
import com.evolution.project.database.Mappers._
import com.evolution.project.domain.user.{User, UserError, UserParams}
import com.evolution.project.effects.GenUUID
import doobie.implicits._
import doobie.util.transactor.Transactor

trait UserRepository[F[_]] {
  def getUser(name: String): F[Either[UserError, User]]
  def createUser(user: UserParams): F[Either[UserError, User]]
  def updateBalance(name: String, newBalance: Int): F[Either[UserError, User]]
}

object UserRepository {
  def of[F[_]: Sync](transactor: Transactor[F]): F[UserRepository[F]] = Sync[F].pure(new UserRepository[F] {

    override def getUser(name: String): F[Either[UserError, User]] =
      fr"SELECT id, name, balance FROM users WHERE name = $name"
        .query[Either[UserError, User]].option.transact(transactor)
        .map{
          case Some(value) => value match {
            case Left(value) => value.asLeft
            case Right(user) => user.asRight
          }
          case None => UserError.UserNotFoundError.asLeft
        }

    override def createUser(userP: UserParams): F[Either[UserError, User]] =
      for {
        userId <- GenUUID[F].random
        userE  <- Sync[F].pure(User.of(userId, userP.name, userP.balance))
        user   <- userE match {
          case Left(value) => Sync[F].raiseError(value)
          case Right(value) => Monad[F].pure(value)
        }
        _ <- sql"INSERT INTO users (id, name, balance) VALUES (${userId.toString}, ${user.name}, ${user.balance})"
          .update.run
          .transact(transactor)
      } yield userE

    override def updateBalance(name: String, newBalance: Int): F[Either[UserError, User]] =
      for {
        balance   <- if (newBalance >= 0) Monad[F].pure(newBalance)
        else Sync[F].raiseError(UserError.NegativeBalance)
        userE     <- getUser(name)
        user      <- userE match {
          case Left(_) => Sync[F].raiseError(UserError.UserNotFoundError)
          case Right(value) => Monad[F].pure(User.of(value.id, value.name, balance))
        }
        _          <- sql"UPDATE users SET balance = ${balance.toString} WHERE name = $name"
          .update
          .run
          .transact(transactor)
      } yield user
  })

}
