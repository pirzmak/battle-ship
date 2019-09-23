package game.player

import game.GameException
import game.board.Board
import game.models.Field
import game.ship.Ship

import scala.util.Random

trait Player {
  val nick: String
  val board: Board

  def copyT(nick: String = this.nick, board: Board = this.board): Player

  def allocateShipOnBoard(ship: Ship): Either[GameException, Player] = this.board.addShip(ship).map(b => this.copyT(board = b))

  def selectField(field: Field): Either[GameException, Player] = this.board.selectField(field).map(b => this.copyT(board = b))

  def allocateShipsRandom(): Player = this.copyT(board = this.board.setAllShipsRandom())

  def allShipsAllocated: Boolean = board.allShipsOnBoard

  def isLose: Boolean = board.allShipsSank
}

case class HumanPlayer(nick: String,
                       board: Board = Board()) extends Player {
  def copyT(nick: String = this.nick, board: Board = this.board): Player = this.copy(nick, board)
}

case class AIPlayer(nick: String,
                    board: Board = Board()) extends Player {
  def copyT(nick: String = this.nick, board: Board = this.board): Player = this.copy(nick, board)

  def getShotField(opponentHitFields: Seq[Field],
                   opponentMissFields: Seq[Field],
                   opponentSankShipsFields: Seq[Field]): Field = {
    val markedFields = opponentHitFields ++ opponentMissFields
    val notSankShipHitPositions = opponentHitFields.find(!opponentSankShipsFields.contains(_))

    notSankShipHitPositions match {
      case Some(position) =>
        getSupposedShipPosition(position, opponentHitFields, markedFields).getOrElse(randomField(markedFields))
      case None => randomField(markedFields)
    }
  }

  private def getSupposedShipPosition(hit: Field, hitPositions: Seq[Field], markedFields: Seq[Field]): Option[Field] = {
    val neighbours = Seq(hit.left, hit.right, hit.top, hit.bottom).
      filter(p => Board.fieldInBoard(p) && !markedFields.contains(p))

    Random.shuffle(neighbours).headOption
  }

  private def randomField(markedFields: Seq[Field]): Field = {
    Random.shuffle(Board.getAllFields.filter(!markedFields.contains(_))).head
  }
}


