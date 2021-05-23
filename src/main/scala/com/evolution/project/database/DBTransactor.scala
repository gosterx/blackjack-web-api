package com.evolution.project.database

import cats.effect.{Async, Blocker, ContextShift, Resource}
import com.evolution.project.database.DBConfig.{dbDriverName, dbPwd, dbUrl, dbUser}
import doobie.hikari.HikariTransactor
import doobie.{ExecutionContexts, Transactor}

object DBTransactor {
  def make[F[_]: ContextShift: Async]: Resource[F, Transactor[F]] =
    for {
      ce <- ExecutionContexts.fixedThreadPool[F](10)
      be <- Blocker[F]
      xa <- HikariTransactor.newHikariTransactor[F](
        driverClassName = dbDriverName,
        url = dbUrl,
        user = dbUser,
        pass = dbPwd,
        connectEC = ce, // await connection on this EC
        blocker = be, // execute JDBC operations on this EC
      )
    } yield xa
}
