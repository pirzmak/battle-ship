package game

import game.models.Field
import game.player.{AIPlayer, HumanPlayer, Player}
import game.ship.{Ship, ShipType}
import game.view.{BoardViewModel, GameUI, PlayerViewModel}

import scala.annotation.tailrec
import scala.util.Random

class Game(player1: Player,
           player2: Player) {

  def start(): Unit = {
    val turn = Random.nextBoolean()
    val firstPlayer = selectPlayer(turn)
    val secondPlayer = selectPlayer(!turn)
    val firstPlayerWithShips = allocateShips(firstPlayer, Ship.shipTypes)
    val secondPlayerWithShips = allocateShips(secondPlayer, Ship.shipTypes)

    val (winner, loser) = mainLoop(firstPlayerWithShips, secondPlayerWithShips)
    GameUI.showWinner(toViewModel(winner, showShips = true), toViewModel(loser, showShips = true))
  }

  @tailrec
  private def mainLoop(currentPlayer: Player, secondPlayer: Player): (Player, Player)= {
    if(currentPlayer.isLose) {
      (secondPlayer, currentPlayer)
    } else if(secondPlayer.isLose) {
      (currentPlayer, secondPlayer)
    } else {
      val shot = currentPlayer match {
        case player: AIPlayer =>
          player.getShotField(
            secondPlayer.board.hits,
            secondPlayer.board.miss,
            secondPlayer.board.sankShipsFields)
        case player: HumanPlayer =>
          GameUI.getShotCoordinates(toViewModel(player, showShips = true), toViewModel(secondPlayer, showShips = false))
      }

      val (nextActivePlayer, nextWaitingPlayer) = attack(currentPlayer, secondPlayer, shot)

      mainLoop(nextActivePlayer, nextWaitingPlayer)
    }
  }

  private def attack(currentPlayer: Player, otherPlayer: Player, shot: Field): (Player, Player) = {
    val shotPlayer = otherPlayer.selectField(shot) match {
      case Left(error) =>
        GameUI.printMessage(error.msg, currentPlayer.nick)
        mainLoop(currentPlayer, otherPlayer)._1
      case Right(player) =>
        player
    }
    val isHit = otherPlayer.board.hits != shotPlayer.board.hits
    if (isHit) {
      (currentPlayer, shotPlayer)
    } else {
      (shotPlayer, currentPlayer)
    }
  }

  private def selectPlayer(player1Turn: Boolean): Player =
    if(player1Turn) player1 else player2

  @tailrec
  private def allocateShips(current: Player, ships: Seq[ShipType]): Player = {
    ships match {
      case _ if current.allShipsAllocated => current
      case Nil => current
      case ship :: rest =>
        val position = GameUI.getShipPositionFromPlayer(toViewModel(current, showShips = true), ship)
        current.allocateShipOnBoard(position) match {
          case Left(error) =>
            GameUI.printMessage(error.msg, current.nick)
            allocateShips(current, ships)
          case Right(player) =>
            allocateShips(player, rest)
        }
    }
  }

  private def toViewModel(player: Player, showShips: Boolean): PlayerViewModel = {
    PlayerViewModel(player.nick,
      BoardViewModel(player.board.miss, player.board.hits, if(showShips) player.board.shipFields else Seq.empty))
  }
}
