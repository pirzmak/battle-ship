package game.models

case class Field(x: Int, y: Int) {
  def left: Field =
    copy(x = x - 1)
  def right: Field =
    copy(x = x + 1)
  def top: Field =
    copy(y = y + 1)
  def bottom: Field =
    copy(y = y - 1)
}
