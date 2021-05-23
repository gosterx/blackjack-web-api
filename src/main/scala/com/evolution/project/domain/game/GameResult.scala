package com.evolution.project.domain.game

sealed trait GameResult
object GameResult {
  case object Win extends GameResult
  case object Lose extends GameResult
  case object Draw extends GameResult
}
