case class Range(x: (Int, Int), y: (Int, Int)) extends Ordered[Range] {
  private def between(x: Int, lower: Int, upper: Int) =
    x >= lower && x <= upper

  private def overlaps(first: (Int, Int), second: (Int, Int)): Boolean = 
    between(first._1, second._1, second._2) ||
    between(first._2, second._1, second._2) ||
    between(second._1, first._1, first._2) ||
    between(second._2, first._1, first._2)

  private def combine(first: (Int, Int), second: (Int, Int)): (Int, Int) = 
    (first._1 min second._1, first._2 max second._2)

  def overlaps(other: Range): Boolean =
    overlaps(x, other.x) && overlaps(y, other.y)

  def combine(other: Range): Range =
    Range(combine(x, other.x), combine(y, other.y))

  def compare(other: Range) =
    if (x._1 == other.x._1 && x._2 == other.x._2) 0
    else if (x._1 < other.x._1 || (x._1 == other.x._1 && x._2 < other.x._2)) -1
    else 1
}

import scala.annotation.tailrec

@tailrec
def combine(input: List[Range], result: List[Range] = List.empty[Range]): List[Range] = {
  if (input.size < 2)
    return input ++ result

  val (x :: y :: tail) = input
  if (x overlaps y) 
    combine((x combine y) :: tail, result)
  else
    combine(y :: tail, x :: result)
}

val ranges = List(
  Range((0,6),   (34,40)),
  Range((1,7),   (35,41)),
  Range((3,9),   (12,18)),
  Range((2,8),   (36,42)),
  Range((13,19), (22,28)),
  Range((18,29), (23,30)))

val sorted = ranges.sorted
val combined = combine(sorted)
combined foreach println
