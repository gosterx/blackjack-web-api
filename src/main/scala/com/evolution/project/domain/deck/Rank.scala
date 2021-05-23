package com.evolution.project.domain.deck

sealed trait Rank
object Rank {
  case object Ace       extends Rank
  case object Two       extends Rank
  case object Three     extends Rank
  case object Four      extends Rank
  case object Five      extends Rank
  case object Six       extends Rank
  case object Seven     extends Rank
  case object Eight     extends Rank
  case object Nine      extends Rank
  case object Ten       extends Rank
  case object Jack      extends Rank
  case object Queen     extends Rank
  case object King      extends Rank

  val ranks: List[Rank] = List(Ace, Two, Three, Four, Five, Six, Seven, Eight, Nine, Ten, Jack, Queen, King)
}

