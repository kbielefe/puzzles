object CodeJam {
  class Parser(filename: String) {
    import scala.io.Source
    private val lines = Source.fromFile(filename).getLines

    def int() = 
      lines.next().toInt

    def intSeq() = 
      lines.next() split " " map (_.toInt)

    def intSet() = intSeq().toSet

    def countIter[A](f: => A): Iterator[A] = {
      val count = int()
      repeat(count)(f)
    }

    def repeat[A](count: Int)(f: => A): Iterator[A] = {
      Iterator.fill(count)(f)
    }
  }

  def formatCases(cases: Iterator[String]) = {
    val indexed = cases.zipWithIndex
    indexed map {case (output, index) =>
      s"Case #${index + 1}: ${output}"
    }
  }

  def main(args: Array[String] = Array("Sample")) = {
    val filename = args(0)
    val out = if (args.size > 1 && args(1) == "-f")
        new java.io.PrintStream(filename + ".out")
      else
        System.out

    val parser = new Parser(filename + ".in")
    val testCases = parser.countIter(testCase(parser))
    val output = formatCases(testCases)
    output foreach out.println
  }

  def testCase(parser: Parser): String = {
    def choice() = {
      val row = parser.int()
      val rows = parser.repeat(4)(parser.intSet)
      rows.toVector(row-1)
    }
      
    val choices = parser.repeat(2)(choice)

    val result = choices.next() & choices.next()

    result.size match {
      case 0 => "Volunteer cheated!"
      case 1 => result.head.toString
      case _ => "Bad magician!"
    }
  }
}
