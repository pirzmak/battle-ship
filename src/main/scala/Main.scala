import game.Game
import game.player.{AIPlayer, HumanPlayer, Player}

object Main {
  def main(args: Array[String]): Unit = {

    @scala.annotation.tailrec
    def options(args: List[String], players: List[Player]): (Player, Player) = {
      args match {
        case Nil if players.length == 2 =>
          (players.head, players.last)
        case Nil =>
          options(Nil, HumanPlayer("Player" + (players.length + 1)) :: players)
        case nick :: "-ai" :: rest =>
          options(rest, AIPlayer(nick).allocateShipsRandom() :: players)
        case nick :: "-r" :: rest =>
          options(rest, HumanPlayer(nick).allocateShipsRandom() :: players)
        case "-ai" :: rest =>
          options(rest, AIPlayer("Player" + (players.length + 1)).allocateShipsRandom() :: players)
        case "-r" :: rest =>
          options(rest, HumanPlayer("Player" + (players.length + 1)).allocateShipsRandom() :: players)
        case nick :: rest =>
          options(rest, HumanPlayer(nick) :: players)
      }
    }
    val (player1, player2) = options(args.toList, List.empty)
    val game = new Game(player1, player2)
    
    game.start()
  }
}
