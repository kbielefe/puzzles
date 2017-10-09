import scala.io.Source

/*
 * These both work on the principle of keeping a running sum of the minimum.
 * For each element of the triangle, it keeps a running sum of itself and the
 * minimum value of the two values above and to either side of it.  Then the
 * answer is just a minimum of running sum at the bottom row.
 * For the rectangle, it keeps a running sum of the shortest distances to the
 * top and bottom points, testing paths that cut through the middle or not.
 */

object CodingChallenge12 {
  def square_path(file: String): Int = {
    val lines = Source.fromFile(file).getLines
    val intLines = lines map {_ split " " map {_.toInt}} map {case Array(x, y, z) => (x, y, z)}

    val (firstTop, firstBottom, firstMiddle) = intLines.next()
    val initial = (firstTop, firstTop + firstMiddle, firstMiddle)

    val (_, lastBottom, _) = intLines.foldLeft(initial){case ((prevTop, prevBottom, prevMiddle), (top, bottom, middle)) =>
      val newTop    = List(prevBottom + prevMiddle + top,    prevTop    + top).min
      val newBottom = List(prevTop    + prevMiddle + bottom, prevBottom + bottom, prevTop + top + middle).min // last condition handles taking far-right edge of rectangle
      val newMiddle = middle
      (newTop, newBottom, newMiddle)
    }
    lastBottom
  }

  def triangle_path(file: String): Int = {
    val lines = Source.fromFile(file).getLines
    val intLines = lines map {_ split "\\s+" filterNot {_ == ""} map {_.toInt}}

    val top = intLines.next()

    val bottom = intLines.foldLeft(top){case (prev, row) =>
      row.zipWithIndex map {case (num, index) =>
        val aboveIndices = List(index-1, index) filter {i => i >= 0 && i < prev.size}
        val aboveValues = aboveIndices map {prev(_)}
        num + aboveValues.min
      }
    }
    bottom.min
  }

  def main(args: Array[String]) {
    if (args(0) startsWith "square")
      println(s"Shortest path for file ${args(0)} is ${square_path(args(0))}")
    else
      println(s"Shortest path for file ${args(0)} is ${triangle_path(args(0))}")
  }
}
