package com.evolution.project.service

import cats.effect.Sync
import cats.effect.concurrent.Ref
import cats.syntax.all._
import com.evolution.project.domain.table.{Status, Table}
import com.evolution.project.domain.user.User

trait TableService[F[_]] {
  def addTable(newTable: Table): F[Unit]
  def getTables: F[List[Table]]
  def getTableByName(name: String): F[Table]
  def join(user: User, tableName: String): F[Unit]
}

object TableService {

  def of[F[_]: Sync]: F[TableService[F]] =
    Ref.of(List(Table("table1", 3, Status.Waiting, Nil))).map { state =>

      new TableService[F] {

        override def getTables: F[List[Table]] =
          for {
            tables <- state.get
          } yield tables

        override def addTable(newTable: Table): F[Unit] =
          state.update(x => x ::: List(newTable))

        override def join(user: User, tableName: String): F[Unit] =
          state.update { list =>
            list.map{ table =>
              if (table.name == tableName) Table(table.name, table.maxPlayersCount, table.status, table.players ::: List(user))
              else table
            }
          }

        override def getTableByName(name: String): F[Table] =
          for {
            tables <- state.get
          } yield tables.filter(t => t.name == name).head
      }
    }
}
