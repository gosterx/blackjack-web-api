package com.evolution.project.effects

import cats.effect.Sync

import scala.util.Random

trait GenNumber[F[_]] {
  def random(max: Int): F[Int]
}

object GenNumber {
  def apply[F[_]: GenNumber]: GenNumber[F] = implicitly

  implicit def syncGenNumber[F[_]: Sync]: GenNumber[F] = new GenNumber[F] {
    override def random(max: Int): F[Int] =
      Sync[F].delay(Random.nextInt(max))
  }
}