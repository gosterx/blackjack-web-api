package com.evolution.project.domain.deck

sealed trait Suit
object Suit {
  case object Heart   extends Suit
  case object Diamond extends Suit
  case object Club    extends Suit
  case object Spade   extends Suit

  val suits: List[Suit] = List(Heart, Diamond, Club, Spade)
}