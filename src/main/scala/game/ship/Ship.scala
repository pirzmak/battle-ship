package game.ship

import game.models.Field
import game.ship.Direction.Direction


case class Ship(shipType: ShipType,
                position: Field,
                direction: Direction,
                hits: Seq[Field] = Seq.empty) {
  val isSink: Boolean = hits.length == shipType.size

  val bowPosition: Field = position
  val sternPosition: Field = position.copy(
    x = if(direction == Direction.Horizontal) position.x + shipType.size - 1 else position.x,
    y = if(direction == Direction.Vertical) position.y + shipType.size - 1 else position.y)

  val occupiedFields: Seq[Field] = if(direction == Direction.Vertical) {
    (bowPosition.y to sternPosition.y).map(Field(position.x, _)) }
  else {
    (bowPosition.x to sternPosition.x).map(Field(_, position.y))
  }

  def isHit(shot: Field): Boolean =
    occupiedFields.contains(shot)
}

object Ship {
  val shipTypes: Seq[ShipType] = Seq(
    ShipType("Carrier", 5),
    ShipType("Battleship", 4),
    ShipType("Submarine", 3),
    ShipType("Cruiser", 3),
    ShipType("Destroyer", 2)
  )
}
