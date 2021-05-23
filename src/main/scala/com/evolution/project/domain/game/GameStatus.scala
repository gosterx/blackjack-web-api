package com.evolution.project.domain.game

sealed trait GameStatus
object GameStatus{
  case object Playing extends GameStatus
  case object Finished extends GameStatus
}
