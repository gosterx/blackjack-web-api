package com.evolution.project.domain.game

import cats.effect.Sync
import cats.effect.concurrent.Ref
import cats.syntax.all._
import com.evolution.project.domain.deck.{Card, Rank, Suit}
import com.evolution.project.domain.user.User
import com.evolution.project.repository.UserRepository
import com.evolution.project.domain.deck.Card._
import com.evolution.project.effects.GenNumber

import scala.util.Random

trait Game[F[_]] {
  def getGameState: F[GameState]

  def doBet(userName: String, bet: Int): F[Unit]

  def placeInitialCards: F[List[Unit]]

  def doResign(userName: String): F[Unit]

  def doHit(userName: String): F[Unit]
}

object Game {

  def of[F[_] : Sync](players: List[User], userRepository: UserRepository[F]): F[Game[F]] = {


    def getUserAfter(user: User): User = {
      val index = players.indexOf(user)
      if (index == players.length - 1) players.head
      else players(index + 1)
    }


    def getRandomCard(deck: List[Card]): Card = deck(Random.nextInt(deck.length - 1))

    for {
      gameStatus <- Ref.of(0)
      result <- Ref.of(Map.empty[String, GameResult])
      deck <- Ref.of(initDeck)
      turn <- Ref.of(players.head)
      betState <- Ref.of(Map.empty[String, Int])
      startDealerGameState <- Ref.of(DealerGameState(Nil, 0))
      dealerGameState <- Ref.of(DealerGameState(Nil, 0))
      userGameState <- Ref.of(Map.empty[String, UserGameState])
    } yield new Game[F] {

      override def doBet(userName: String, bet: Int): F[Unit] =
        for {
          turnUser <- turn.get
          _ <-
            if (userName == turnUser.name)
              betState.update(_.updated(userName, bet)) *> turn.update(getUserAfter)
            else Sync[F].pure()
          turnUser1 <- turn.get
          _ <-
            if (turnUser1.name == players.head.name) placeInitialCards *> placeDealerInitialCards
            else Sync[F].pure()
        } yield ()

      override def getGameState: F[GameState] =
        for {
          gameStatus <- gameStatus.get
          turn <- turn.get
          bets <- betState.get
          userState <- userGameState.get
          result <- result.get
          dealerState <- gameStatus match {
            case 0 => startDealerGameState.get
            case _ => dealerGameState.get
          }
        } yield GameState(turn, bets, userState, dealerState, result)

      override def placeInitialCards: F[List[Unit]] =
        players.map(user => {
          for {
            firstCard <- getCardFromDeck
            secondCard <- getCardFromDeck
            hand: List[Card] = List(firstCard, secondCard)
            userState = UserGameState(hand, getCardsScore(hand))
            _ <- userGameState.update(map => map.updated(user.name, userState))
          } yield ()
        }).sequence

      def getCardFromDeck: F[Card] =
        for {
          deckNow <- deck.get
          number  <- GenNumber[F].random(deckNow.length)
          card    <- Sync[F].delay(deckNow(number - 1))
          _       <- deck.update(_.filter(_ != card))
        } yield card

      def placeDealerInitialCards: F[Unit] =
        for {
          firstCard  <- getCardFromDeck
          secondCard <- getCardFromDeck
          hand: List[Card] = List(firstCard, secondCard)
          dealerState = DealerGameState(hand, getCardsScore(hand))
          startDealerState = DealerGameState(List(firstCard), getCardsScore(List(firstCard)))
          _ <- dealerGameState.update(_ => dealerState)
          _ <- startDealerGameState.update(_ => startDealerState)
        } yield ()

      def placeDealerFinishCards: F[Unit] =
        for {
          dealerState <- dealerGameState.get
          _ <-
            if (dealerState.score < 16) {
              for {
                card <- getCardFromDeck
                _ <- dealerGameState.update(state => state.copy(cards = state.cards ::: List(card), score = getCardsScore(state.cards ::: List(card))))
                _ <- placeDealerFinishCards
              } yield ()
            } else gameStatus.update(_ => 1)
        } yield ()

      def pullResults: F[Unit] = {
        for {
          usersGameState <- userGameState.get
          dealerState <- dealerGameState.get
          _ <- usersGameState.map(state =>
            if ((state._2.score <= 21 && dealerState.score > 21) || (state._2.score <= 21 && dealerState.score <= 21 && state._2.score > dealerState.score)) {
              result.update(_.updated(state._1, GameResult.Win))
            } else if ((state._2.score > 21 && dealerState.score <= 21) || (state._2.score <= 21 && dealerState.score <= 21 && state._2.score < dealerState.score)) {
              result.update(_.updated(state._1, GameResult.Lose))
            } else result.update(_.updated(state._1, GameResult.Draw))
          ).toList.sequence
        } yield ()
      }

      override def doResign(userName: String): F[Unit] = {
        for {
          userE <- userRepository.getUser(userName)
          _ <- userE match {
            case Left(_) => Sync[F].pure()
            case Right(value) => turn.update(_ => getUserAfter(value))
          }
          turnUser <- turn.get
          _ <- if (turnUser.name == players.head.name) placeDealerFinishCards *> pullResults else Sync[F].pure()
        } yield ()
      }

      override def doHit(userName: String): F[Unit] =
        for {
          turnUser <- turn.get
          usersGameState <- userGameState.get
          deckNow <- deck.get
          _ <-
            if (userName == turnUser.name) {
              usersGameState.get(userName) match {
                case Some(value) =>
                  val newCard = getRandomCard(deckNow)
                  userGameState.update(_.updated(userName, value.copy(cards = value.cards ::: List(newCard), score = getCardsScore(value.cards ::: List(newCard))))) *>
                    deck.update(_.filter(_ != newCard)) *> {
                    if (getCardsScore(value.cards ::: List(newCard)) > 21) {
                      doResign(userName)
                    } else Sync[F].pure()
                  }
                case None => Sync[F].pure()
              }
            } else Sync[F].pure()

        } yield ()

    }
  }

}
