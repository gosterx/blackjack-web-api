package com.evolution.project.router

import cats.effect.IO
import com.evolution.project.domain.table.{Status, Table, TableParams}
import com.evolution.project.service.TableService
import org.http4s.HttpRoutes
import org.http4s.dsl.io._

object TableRouter {

  def of(tableService: TableService[IO]): HttpRoutes[IO] = {
    import org.http4s.circe.CirceEntityCodec._

    HttpRoutes.of[IO]{
      case GET -> Root / "get" / "tables" =>
        tableService.getTables.flatMap(tables => Ok(tables))

      case req@ POST -> Root / "create" / "table" =>
        for {
          tableP <- req.as[TableParams]
          table = Table(tableP.name, tableP.maxPlayersCount, Status.Waiting, Nil)
          response     <- tableService.addTable(table) *> Ok()
        } yield response
    }
  }

}

