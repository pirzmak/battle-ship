package game.ship

case class ShipType(name: String, size: Int)

object Direction extends Enumeration {
  type Direction = Value
  val Vertical, Horizontal = Value

  def opposed(direction: Direction): Direction = {
    if(direction == Vertical) Horizontal else Vertical
  }
}