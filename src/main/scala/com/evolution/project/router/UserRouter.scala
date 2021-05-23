package com.evolution.project.router

import cats.effect.IO
import com.evolution.project.domain.user.UserParams
import com.evolution.project.repository.UserRepository
import org.http4s.HttpRoutes
import org.http4s.dsl.impl.Root
import org.http4s.dsl.io._

object UserRouter {

  def of(userRepo: UserRepository[IO]): HttpRoutes[IO] = {
    import io.circe.generic.auto._
    import org.http4s.circe.CirceEntityCodec._

    HttpRoutes.of[IO] {

      case GET -> Root / "get" / "user" / name =>
        userRepo.getUser(name).flatMap {
          case Left(_) => NotFound()
          case Right(value) => Ok(value)
        }

      case req@POST -> Root / "create" / "user" =>
        for {
          userP <- req.as[UserParams]
          responseE <- userRepo.createUser(userP)
          response <- responseE match {
            case Left(error) => BadRequest(error)
            case Right(value) => Ok(value)
          }
        } yield response

      case req@POST -> Root / "update" / "balance" =>
        for {
          userP <- req.as[UserParams]
          responseE <- userRepo.updateBalance(userP.name, userP.balance)
          response <- responseE match {
            case Left(error) => BadRequest(error)
            case Right(value) => Ok(value)
          }
        } yield response

    }
  }

}

