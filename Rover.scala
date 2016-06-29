object Rover {
  val left  = Map('N' -> 'W', 'W' -> 'S', 'S' -> 'E', 'E' -> 'N')
  val right = Map('N' -> 'E', 'E' -> 'S', 'S' -> 'W', 'W' -> 'N')

  val turns = Map('L' -> left, 'R' -> right)

  val moves = Map('N' -> (0,  1), 'E' -> ( 1, 0),
                  'S' -> (0, -1), 'W' -> (-1, 0))

  case class Position(x: Int, y: Int, dir: Char)

  def command(pos: Position, cmd: Char): Position = (pos, cmd) match {
    case (Position(x, y, dir), 'M')  => val (xOffset, yOffset) = moves(dir)
                                        Position(x + xOffset, y + yOffset, dir)
    case (Position(x, y, dir), turn) => Position(x, y, turns(turn)(dir))
  }

  def main(args: Array[String]): Unit = {
    val initialPosition = Position(0, 0, 'N')
    val result = args(0).filter("LRM" contains _).foldLeft(initialPosition)(command)
    println(result)
  }
}
