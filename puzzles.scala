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
}
