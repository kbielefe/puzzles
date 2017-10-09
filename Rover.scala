object Rover {
  val left  = Map('N' -> 'W', 'W' -> 'S', 'S' -> 'E', 'E' -> 'N')
  val right = Map('N' -> 'E', 'E' -> 'S', 'S' -> 'W', 'W' -> 'N')

  val turns = Map('L' -> left, 'R' -> right)

  val xOff = Map('N' -> 0, 'S' -> 0, 'E' -> 1, 'W' -> -1)
  val yOff = Map('E' -> 0, 'W' -> 0, 'N' -> 1, 'S' -> -1)

  case class Position(x: Int, y: Int, dir: Char)

  def command(pos: Position, cmd: Char): Position = (pos, cmd) match {
    case (Position(x, y, dir), 'M')  => Position(x + xOff(dir), y + yOff(dir), dir)
    case (Position(x, y, dir), turn) => Position(x, y, turns(turn)(dir))
  }

  def main(args: Array[String]): Unit = {
    val initialPosition = Position(0, 0, 'N')
    val commands = args.lift(0) getOrElse "" map (_.toUpper) filter ("LRM" contains _)
    val result = commands.foldLeft(initialPosition)(command)
    println(result)
  }
}
