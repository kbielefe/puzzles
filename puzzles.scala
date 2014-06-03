def fibs(a: BigInt = 0, b: BigInt = 1): Stream[BigInt] =
  a #:: fibs(b, a + b)

def triangles(a: BigInt = 1, n: BigInt = 2): Stream[BigInt] =
  a #:: triangles(a + n, n + 1)

object Prime {
  def is(i: Long) =
    if (i == 2) true
    else if ((i & 1) == 0) false
    else prime(i)

  def primes: Stream[Long] = 2 #:: prime3

  private val prime3: Stream[Long] = {
    def next(i: Long): Stream[Long] =
      if (prime(i))
        i #:: next(i + 2)
      else
        next(i + 2) // tail
    3 #:: next(5)
  }

  private def prime(i: Long) =
    prime3 takeWhile (math.sqrt(i).>= _) forall { i % _ != 0 }

  private def divides(d : Int, n : Int) = {
    (n % d) == 0
  }

  private def ld(n : Int) : Int = {
    ldf(2, n)
  }

  private def ldf(k : Int, n : Int) : Int = {
    if (divides(k, n)) k
    else if ((k*k) > n) n
    else ldf((k+1), n)
  }

  def factors(n : Int) : List[Int] = n match {
    case 1 => Nil;
    case _ => {
      val p = ld(n)
      p :: factors(n / p)
    }
  }

  def divisorCount(n: Int): Int =
    factors(n).groupBy((x) => x).values.map(_.size + 1).product
}

val collatzLengthCache = scala.collection.mutable.HashMap.empty[BigInt, BigInt]

def collatzLength(n: BigInt, initialN: BigInt, length: BigInt = 1): BigInt = {
  if (collatzLengthCache contains n) {
    val result = length + collatzLengthCache(n) - 1
    collatzLengthCache += ((initialN, result))
    result
  }
  else if (n == 1) {
    collatzLengthCache += ((initialN, length))
    length
  }
  else if (n % 2 == 0)
    collatzLength(n / 2, initialN, length + 1)
  else
    collatzLength(3 * n + 1, initialN, length + 1)
}

def countPaths(pos: (Int, Int)): Int = pos match {
  case (0,  0) => 1
  case (_, -1) => 0
  case (-1, _) => 0
  case (x,  y) => {
    countPaths((x-1, y)) +
    countPaths((x, y-1))
  }
}
