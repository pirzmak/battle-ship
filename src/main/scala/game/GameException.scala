package game

trait GameException {
  val msg: String
}
case class ShipNotInBoardException(msg: String = "Try to set ship on incorrect position") extends GameException
case class ShipOnOccupiedPositionException(msg: String = "Try to set ship on already occupied position") extends GameException
case class FieldNotInBoardException(msg: String = "Try to mark out off the board field") extends GameException
case class FieldAlreadyMarkedException(msg: String = "Try to mark already marked field") extends GameException
