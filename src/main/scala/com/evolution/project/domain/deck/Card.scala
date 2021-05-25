package com.evolution.project.domain.deck

case class Card(rank: Rank, suit: Suit)

object Card {
  def initDeck: List[Card] =
    for {
      rank <- Rank.ranks
      suit <- Suit.suits
    } yield Card(rank, suit)

  def getCardsScore(cards: List[Card]): Int = {
    val scores: Int = cards.map(getSingleCardScore).sum
    if (scores > 21) {
      val aceCount = cards.count(_.rank == Rank.Ace)
      if (aceCount > 1) {
        scores - 10 * (aceCount - 1)
      } else if (aceCount == 1) {
        scores - 10
      } else {
        scores
      }
    } else {
      scores
    }
  }

  def getSingleCardScore(card: Card): Int = {
    card.rank match {
      case Rank.Ace => 11
      case Rank.Two => 2
      case Rank.Three => 3
      case Rank.Four => 4
      case Rank.Five => 5
      case Rank.Six => 6
      case Rank.Seven => 7
      case Rank.Eight => 8
      case Rank.Nine => 9
      case Rank.Ten => 10
      case Rank.Jack => 10
      case Rank.Queen => 10
      case Rank.King => 10
    }
  }

}