package game.board

import game.ship.{Direction, Ship, ShipType}
import game._
import game.models.Field
import game.ship.Direction.Direction

import scala.annotation.tailrec
import scala.util.Random

case class Board(ships: Seq[Ship] = Seq.empty,
                 miss: Seq[Field] = Seq.empty) {
  val hits: Seq[Field] = ships.flatMap(_.hits)
  val shipFields: Seq[Field] = ships.flatMap(_.occupiedFields)
  val sankShipsFields: Seq[Field] = ships.filter(_.isSink).flatMap(_.occupiedFields)
  val allShipsOnBoard: Boolean = ships.length == Ship.shipTypes.length
  val allShipsSank: Boolean = sankShipsFields == shipFields

  def setAllShipsRandom(): Board =
    setShipsRandom(Ship.shipTypes, this)

  def addShip(ship: Ship): Either[GameException, Board] = {
    validateShipPosition(ship) match {
      case Some(exc) => Left(exc)
      case None => Right(this.copy(ships = ship +: ships))
    }
  }

  def selectField(check: Field): Either[GameException, Board] = {
    validateField(check) match {
      case Some(exc) => Left(exc)
      case None =>
        if(isHit(check)) {
          Right(this.copy(ships = ships.map(s => if(s.isHit(check)) s.copy(hits = check +: s.hits) else s)))
        } else {
          Right(this.copy(miss = check +: miss))
        }
    }
  }

  private def validateShipPosition(ship: Ship): Option[GameException] = {
    if(!isShipInBoard(ship))
      Some(ShipNotInBoardException())
    else if(!isOnFreeField(ship.occupiedFields, this))
      Some(ShipOnOccupiedPositionException())
    else
      None
  }

  private def validateField(field: Field): Option[GameException] = {
    if(!Board.fieldInBoard(field))
      Some(FieldNotInBoardException())
    else if(isFieldChecked(field))
      Some(FieldAlreadyMarkedException())
    else
      None
  }

  @tailrec
  private def setShipsRandom(ships: Seq[ShipType], board: Board): Board =
    ships match {
      case Nil => board
      case s :: tail =>
        val randomPosition = Field(Random.nextInt(Board.columns), Random.nextInt(Board.rows))
        val randomVertical = if(Random.nextBoolean()) Direction.Vertical else Direction.Horizontal
        val ship = findValidShipPosition(game.ship.Ship(s, randomPosition, randomVertical), board, randomVertical)
        setShipsRandom(tail, board.addShip(ship).right.get)
    }

  @tailrec
  private def findValidShipPosition(ship: Ship, board: Board, lastDirection: Direction): Ship = {
    if(isShipInBoard(ship) && isOnFreeField(ship.occupiedFields, board)) {
      ship
    } else if(ship.direction == lastDirection) {
      findValidShipPosition(ship.copy(direction = Direction.opposed(ship.direction)), board, lastDirection)
    } else {
      findValidShipPosition(ship.copy(position = getNewShipPosition(ship)), board, Direction.opposed(lastDirection))
    }
  }

  private def getNewShipPosition(ship: Ship): Field = {
    if(ship.sternPosition.x >= Board.columns) {
      Field(0, ship.position.y + 1)
    } else if(ship.sternPosition.y >= Board.rows) {
      Field(0, 0)
    } else {
      Field(ship.position.x + 1, ship.position.y)
    }
  }

  private def isShipInBoard(ship: Ship): Boolean =
    Board.fieldInBoard(ship.bowPosition) && Board.fieldInBoard(ship.sternPosition)

  private def isOnFreeField(shipPosition: Seq[Field], board: Board): Boolean =
    shipPosition.forall(!board.shipFields.contains(_))

  private def isFieldChecked(field: Field): Boolean =
    (hits ++ miss).contains(field)

  private def isHit(field: Field): Boolean =
    shipFields.contains(field)
}

object Board {
  val rows = 11
  val columns = 11

  def getAllFields: Seq[Field] =
    for(y <- 0 to columns; x <- 0 to rows) yield Field(x, y)

  def fieldInBoard(field: Field): Boolean =
    field.x >= 0 && field.x <= Board.columns &&
      field.y >= 0 && field.y <= Board.rows
}
