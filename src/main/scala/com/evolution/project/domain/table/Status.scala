package com.evolution.project.domain.table

sealed trait Status
object Status {
  case object Waiting extends Status
  case object Playing extends Status
}
