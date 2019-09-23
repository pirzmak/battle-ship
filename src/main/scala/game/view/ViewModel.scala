package game.view

import game.models.Field


case class PlayerViewModel(nick: String, board: BoardViewModel)

case class BoardViewModel(miss: Seq[Field],
                          hits: Seq[Field],
                          ships: Seq[Field])

object ViewModel {
  val missField: Char = 'O'
  val hitField: Char = 'X'
  val shipField: Char = '$'
  val emptyField: Char = '#'
}
