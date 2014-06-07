object CodeJam {
  class Parser(filename: String) {
    import scala.io.Source
    private val lines = Source.fromFile(filename).getLines

    def int() = 
      lines.next().toInt

    def intSeq() = 
      lines.next() split " " map (_.toInt)

    def doubleSeq() = 
      lines.next() split " " map (_.toDouble)

    def intSet() = intSeq().toSet

    def countIter[A](f: => A): Iterator[A] = {
      val count = int()
      repeat(count)(f)
    }

    def repeat[A](count: Int)(f: => A): Iterator[A] = {
      Iterator.fill(count)(f)
    }
  }

  def formatCases[A](cases: Iterator[A]) = {
    val indexed = cases.map(_.toString).zipWithIndex
    indexed map {case (output, index) =>
      s"Case #${index + 1}: ${output}"
    }
  }

  def main(args: Array[String] = Array("Sample")) = {
    val filename = args(0).stripSuffix(".in")
    val out = if (args.size > 1 && args(1) == "-f")
        new java.io.PrintStream(filename + ".out")
      else
        System.out

    val parser = new Parser(filename + ".in")
    val testCases = parser.countIter(testCase(parser))
    val output = formatCases(testCases)
    output foreach out.println
  }

  case class State(
    rate: Double, cookies: Double, time: Double,
    c: Double, f: Double, x: Double) {

    def dontBuyWinTime = (x - cookies) / rate

    def buyTime = (c - cookies) / rate

    def buyWinTime = buyTime + x / (rate + f)

    def buy = State(rate + f, 0.0, time + buyTime,
      c, f, x)
  }

  def testCase(parser: Parser): Double = {
    def solve(state: State): Double = {
      if (state.buyWinTime > state.dontBuyWinTime)
        state.dontBuyWinTime + state.time
      else
        solve(state.buy)
    }

    val Array(c, f, x) = parser.doubleSeq()
    val initialState = State(2.0, 0.0, 0.0, c, f, x)

    solve(initialState)
  }
}
