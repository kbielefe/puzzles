object Sudoku {
  type Square  = (Int, Int)
  type Squares = Set[Square]
  type Puzzle  = Map[Square,Set[Int]]

  def findPeers(square: Square): Squares = {
    val (row, col) = square
    val inRow = (1 to 9).map{(row,_)}.toSet
    val inCol = (1 to 9).map{(_,col)}.toSet
    val inBlock = for {
      bRow <- row / 3 + 1 to row / 3 + 3
      bCol <- col / 3 + 1 to col / 3 + 3
    } yield (bRow, bCol)
    inRow ++ inCol ++ inBlock.toSet - square
  }

  def readPuzzle(lines: Iterator[String]): Puzzle = {
    lines drop 1 // heading
    (1 to 9).foldLeft(Map[Square,Set[Int]]()){(puzzle, row) =>
      val line = lines.next
      val digitsCols = line.zipWithIndex.map{case (x,y) => (x.asDigit,y+1)}

      digitsCols.foldLeft(puzzle){case (puzzle, (num, col)) =>
        val entry = if (num == 0) ((1 to 9).toSet - num) else Set(num)
        puzzle + ((row, col) -> entry)
      }
    }
  }

  def printSquares(squares: Squares): Unit = {
    for {
      row <- 1 to 9
      col <- 1 to 9
    } {
      if (squares contains (row, col)) print("X") else print("\u00b7")
      if (col % 3 == 0 && col != 9) print ("\u2502")
      if (col == 9) println()
      if (row %3 == 0 && row != 9 && col == 9) println("\u2500\u2500\u2500\u253c\u2500\u2500\u2500\u253c\u2500\u2500\u2500")
    }
  }

  def main(args: Array[String]): Unit = {
    import scala.io.Source
    val lines = Source.fromFile("sudoku.txt").getLines
    val puzzle = readPuzzle(lines)

    printSquares(findPeers((1,1)))
  }
}
