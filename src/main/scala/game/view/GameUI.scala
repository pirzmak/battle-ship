package game.view

import game.board.Board
import game.models.Field
import game.ship.Direction.Direction
import game.ship.{Direction, Ship, ShipType}

object GameUI {
  def printMessage(playerName: String, msg: String): Unit = {
    println(playerName + ": " + msg)
  }

  def getShotCoordinates(player: PlayerViewModel, opponent: PlayerViewModel): Field = {
    showBoard(player)
    showBoard(opponent)
    getFieldFromPlayer(player.nick, "shot")
  }

  def getShipPositionFromPlayer(player: PlayerViewModel, ship: ShipType): Ship = {
    showBoard(player)
    println("No allocated ship:")
    Ship.shipTypes.filterNot(player.board.ships.contains).foreach(s => println(s.name + " " + ViewModel.shipField * s.size))
    println()
    println("Allocate ship [" + ship.name + "] ")
    game.ship.Ship(ship, getFieldFromPlayer(player.nick, "ship"), getDirection(player.nick))
  }

  private def getDirection(playerName: String): Direction = {
    println(s"$playerName: Set direction: v - vertical h - horizontal")

    var direction = scala.io.StdIn.readChar()
    while(direction != 'v' && direction != 'h')
      direction = scala.io.StdIn.readChar()

    if(direction == 'v') Direction.Vertical else Direction.Horizontal
  }

  private def showBoard(viewModel: PlayerViewModel): Unit = {
    println("=========" + viewModel.nick + "=========")
    Board.getAllFields.foreach(p => {
      if (p.x == 0) println()
      print(getPositionSign(viewModel.board, p)  + " ")
    })
    println()
    println()
  }

  def showWinner(winner: PlayerViewModel, loser: PlayerViewModel): Unit = {
    println("Winner : " + winner.nick)
    println()
    showBoard(winner)
    showBoard(loser)
  }

  @scala.annotation.tailrec
  private def getFieldFromPlayer(playerName: String, element: String): Field = {
    println(s"$playerName: Get $element coordinates: x y")

    val position = scala.io.StdIn.readLine().split(" ")
    try {
      Field(position(0).toInt, position(1).toInt)
    } catch {
      case _: Throwable =>
        getFieldFromPlayer(playerName, element)
    }
  }

  private def getPositionSign(board: BoardViewModel, position: Field): Char =
    if (board.miss.contains(position)) {
      ViewModel.missField
    } else if (board.hits.contains(position)) {
      ViewModel.hitField
    } else if (board.ships.contains(position)) {
      ViewModel.shipField
    } else {
      ViewModel.emptyField
    }
}
