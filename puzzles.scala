import scala.annotation.tailrec

def fibs(a: BigInt = 0, b: BigInt = 1): Stream[BigInt] =
  a #:: fibs(b, a + b)

def triangles(a: BigInt = 1, n: BigInt = 2): Stream[BigInt] =
  a #:: triangles(a + n, n + 1)

object Prime {
  def is(i: Long) =
    if (i == 2) true
    else if (i < 2) false
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

  def divisors(n: Int): Set[Int] = {
    val primeFactors = factors(n)
    divisors(primeFactors, primeFactors.size, Set(1))
  }

  private def divisors(primeFactors: List[Int], size: Int, result: Set[Int]): Set[Int] = {
    if (size == 0)
      return result
    val combos = primeFactors.combinations(size)
    val newDivisors = combos.map(_.product).toSet
    divisors(primeFactors, size - 1, result ++ newDivisors)
  }
}

object Collatz {
  private val cache = scala.collection.mutable.HashMap.empty[BigInt, BigInt]

  def getLength(n: BigInt, initialN: BigInt, length: BigInt = 1): BigInt = {
    if (cache contains n) {
      val result = length + cache(n) - 1
      cache += ((initialN, result))
      result
    }
    else if (n == 1) {
      cache += ((initialN, length))
      length
    }
    else if (n % 2 == 0)
      getLength(n / 2, initialN, length + 1)
    else
      getLength(3 * n + 1, initialN, length + 1)
  }
}

object Paths {
  private val cache = scala.collection.mutable.HashMap.empty[(Int, Int), BigInt]

  def count(pos: (Int, Int)): BigInt = {
    if (cache contains pos)
      return cache(pos)

    pos match {
      case (0,  0) => 1
      case (_, -1) => 0
      case (-1, _) => 0
      case (x,  y) => {
        val result = count((x-1, y)) + count((x, y-1))
        cache += ((pos, result))
        result
      }
    }
  }
}

object WordNumbers {
  private val digitWords = Array("", "one", "two", "three", "four", "five", "six",
    "seven", "eight", "nine")

  private val teenWords = Array("ten", "eleven", "twelve", "thirteen", "fourteen", 
    "fifteen", "sixteen", "seventeen", "eighteen", "nineteen")

  private val tensWords = Array("", "ten", "twenty", "thirty", "forty", "fifty", 
    "sixty", "seventy", "eighty", "ninety")

  private def thousands(n: Int) = {
    val digit = n / 1000
    if (digit > 0)
      digitWords(digit) ++ " thousand "
    else
      ""
  }

  private def hundreds(n: Int) = {
    val digit = n % 1000 / 100
    if (digit > 0)
      digitWords(digit) ++ " hundred " 
    else
      ""
  }

  private def tens(n: Int) = {
    val digit = n % 100 / 10
    if (digit == 1)
      teenWords(n % 10)
    else if (digit > 0)
      tensWords(digit) ++ "-"
    else
      ""
  }

  private def ones(n: Int) = {
    val tensDigit = n % 100 / 10
    val digit = n % 10
    if (tensDigit != 1)
      digitWords(digit)
    else
      ""
  }

  private def andWord(n: Int) = {
    val tens = n % 100
    if (tens != 0 && n > 99)
      " and "
    else
      ""
  }

  def toWords(n: Int) = {
    thousands(n) ++ hundreds(n) ++ andWord(n) ++ tens(n) ++ ones(n)
  }
}

class PuzzleIntegral[T](value: T)(implicit n: Integral[T]) {
  def digits = value.toString map {_.asDigit}
  def isPrime = Prime.is(n.toLong(value))
}

import scala.language.implicitConversions
implicit def puzzleIntegral[T](value: T)(implicit n: Integral[T]) =
  new PuzzleIntegral[T](value)

@tailrec
def gcd(a: Int, b: Int): Int = if (b == 0) a.abs else gcd(b, a % b)

@tailrec
def fac(n: Int, result: BigInt = 1): BigInt = {
  if (n == 0)
    result
  else
    fac(n - 1, n * result)
}

def romanToInt(roman: String) = {

  val digits = roman map Map(
    'M' -> 1000,
    'D' -> 500,
    'C' -> 100,
    'L' -> 50,
    'X' -> 10,
    'V' -> 5,
    'I' -> 1)

  def inversion(digits: Seq[Int]) =
    digits.size > 1 && digits(0) < digits(1)

  val subtractive = digits sliding 2 filter inversion map (_.head)

  digits.sum - subtractive.sum * 2
}

def intToRoman(number: Int): String = {
  if (number <= 0) return ""

  val conversions = List(
    (1000, "M"),
    (900,  "CM"),
    (500,  "D"),
    (400,  "CD"),
    (100,  "C"),
    (90,   "XC"),
    (50,   "L"),
    (40,   "XL"),
    (10,   "X"),
    (9,    "IX"),
    (5,    "V"),
    (4,    "IV"),
    (1,    "I"))

  val (decimal, roman) = 
    (conversions dropWhile (_._1 > number)).head

  roman + intToRoman(number - decimal)
}

// Return an iterator through all combinations of one element
// from each list, similar to amb operator.
def permute[A](lists: List[List[A]]): Iterator[List[A]] = {
  if (lists.isEmpty) return Iterator.single(Nil)
  val head :: tail = lists
  for (x <- head.iterator; xs <- permute(tail)) yield x :: xs
}

def rotations[A](list: Seq[A], count: Int = 0): List[Seq[A]] = {
  if (count == list.size) return Nil
  val (head, tail) = list.splitAt(count)
  tail ++ head :: rotations(list, count + 1)
}

def digitsToInt(digits: Seq[Int]) =
  digits.foldLeft(0)((total, digit) => total * 10 + digit)

def circular(number: Int) = {
  rotations(number.digits) map digitsToInt forall (_.isPrime)
}

def truncations(number: Int) = {
  val digits = number.digits
  val left = for (i <- 0 until digits.size) yield digitsToInt(digits.slice(i, digits.size))
  val right = for (i <- digits.size-1 to 1 by -1) yield digitsToInt(digits.slice(0, i))
  left ++ right
}

def truncatable(number: Int) = {
  val x = truncations(number)
  val primeTruncations = x filter (_.isPrime)
  x.size == primeTruncations.size
}
