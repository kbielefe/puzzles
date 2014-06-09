import scala.annotation.tailrec

def fibs = new Iterator[BigInt] {
  var a: BigInt = 1
  var b: BigInt = 0
  def hasNext = true
  def next() = {
    val result = a + b
    a = b
    b = result
    result
  }
}

def modFibs = new Iterator[Int] {
  var a: Int = 1
  var b: Int = 0
  def hasNext = true
  def next() = {
    val result = (a + b) % 1000000000
    a = b
    b = result
    result
  }
}

def decimalFibs = new Iterator[BigDecimal] {
  val mc = new java.math.MathContext(32, java.math.RoundingMode.FLOOR)
  var a = BigDecimal(1, mc)
  var b = BigDecimal(0, mc)
  def hasNext = true
  def next() = {
    val result = (a + b)(mc)
    a = b
    b = result
    result
  }
}

def triangles = Iterator.from(1).map((n) => n * (n + 1) /2)

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

def pandigital(number: Int, through: Int = 9) = {
  number.digits.sorted == (1 to through)
}

def pandigitals(through: Int = 9) = {
  (through to 1 by -1).permutations map digitsToInt
}

def commaDelimitedFile(filename: String) = {
  import scala.io.Source
  val line = Source.fromFile(filename).getLines.next()
  line split "," map (_.stripPrefix("\"").stripSuffix("\""))
}

def time[A](f: => A) = {
  val s = System.nanoTime
  val ret = f
  println("time: "+(System.nanoTime-s)/1e6+"ms")
  ret
}

def pandigital(n: ((BigDecimal, Int), Int)) = n match {
  case ((start, end), index) =>
    start.toString.replaceAll("\\.", "").take(9).sorted == "123456789" &&
    end.toString.takeRight(9).sorted == "123456789"
}

// http://www.codecodex.com/wiki/Calculate_an_integer_square_root
def sqrt(number : BigInt) = {
  def next(n : BigInt, i : BigInt) : BigInt = (n + i/n) >> 1
 
  val one = BigInt(1)
 
  var n = one
  var n1 = next(n, number)
     
  while ((n1 - n).abs > one) {
    n = n1
    n1 = next(n, number)
  }
      
  while (n1 * n1 > number) {
    n1 -= one
  }
      
  n1
}

def perfectSquare(number: BigInt): Boolean = {
  if (number < 0)
    return false
  val result = sqrt(number)
  result * result == number
}

// Returns true if has a positive integer solution to the quadratic equation
def naturalQuadratic(a: BigInt, b: BigInt, c: BigInt): Boolean = {
  val discriminant = b * b - 4 * a * c
  if (!perfectSquare(discriminant))
    return false

  val plus = -b + sqrt(discriminant)
  if (plus % (2 * a) == 0 && (plus * a) > 0)
    return true

  val minus = -b - sqrt(discriminant)
  if (minus % (2 * a) == 0 && (minus * a) > 0)
    return true

  false
}

def isPentagonal(n: BigInt) = naturalQuadratic(3, -1, -2*n)

def isHexagonal(n: BigInt) = naturalQuadratic(2, -1, -n)

def isTriangular(n: BigInt) = naturalQuadratic(1, 1, -2*n)

// Figure out better way to do 
// res0 map ((x) => x map ((y) => y.toInt))

// Generates a sequence based on parameters from
// http://www.alpertron.com.ar/QUAD.HTM
def diophantine(x0: BigInt, y0: BigInt, 
  p: BigInt, q: BigInt, k: BigInt, r: BigInt,
  s: BigInt, l: BigInt) = {

  def nextX(prevX: BigInt, prevY: BigInt) = p * prevX + q * prevY + k
  def nextY(prevX: BigInt, prevY: BigInt) = r * prevX + s * prevY + l

  val iter = Iterator.iterate((x0, y0)){case (x, y) => (nextX(x, y), nextY(x, y))}
  iter filter {case (x, y) => x > 0 && y > 0}
}

def triangleAndPentagonal = diophantine(0, 0, -2, -3, -1, -1, -2, 0)

def ncr(n: Int, r: Int) = fac(n) / fac(r) / fac(n-r)

case class Card(rank: Int, suit: Char)

def cardFromString(string: String) = {
  val rank = string(0) match {
    case 'T' => 10
    case 'J' => 11
    case 'Q' => 12
    case 'K' => 13
    case 'A' => 14
    case x => x.asDigit
  }

  val suit = string(1)

  Card(rank, suit)
}

def getHands(line: String) = {
  val cards = line split " " map cardFromString
  cards.splitAt(5)
}

object Score extends Enumeration {
  val HighCard, Pair, TwoPair, ThreeOfAKind, Straight,
    Flush, FullHouse, FourOfAKind, FlushStraight = Value
}

def tiebreaker(hand: Array[Card]) = {
  val groupSizes        = hand.groupBy(_.rank) mapValues (_.size)
  val sortedByRank      = groupSizes.toSeq.sortBy(_._1)
  val sortedByGroupSize = sortedByRank.sortBy(_._2)
  val ranksOnly         = sortedByGroupSize.reverse.map(_._1)
  ranksOnly.toList
}

def evaluateHand(hand: Array[Card]): Score.Value = {
  val sorted   = hand.map(_.rank).sorted

  val flush    = hand.map(_.suit).distinct.size == 1
  val straight = sorted sliding 2 forall ((x) => x(1) - x(0) == 1)

  val groups   = hand.groupBy(_.rank).values.map(_.size)
  def containsGroupOf(n: Int) = groups.exists(_ == n)

  import Score._
  if (flush && straight)         return FlushStraight
  if (containsGroupOf(4))        return FourOfAKind
  if (containsGroupOf(3) && 
      containsGroupOf(2))        return FullHouse
  if (flush)                     return Flush
  if (straight)                  return Straight
  if (containsGroupOf(3))        return ThreeOfAKind
  if (groups.count(_ == 2) == 2) return TwoPair
  if (containsGroupOf(2))        return Pair 
                                 return HighCard
}

def winner(game: (Array[Card], Array[Card])): Boolean = {
  val (hand1, hand2) = game
  val score1 = evaluateHand(hand1).id :: tiebreaker(hand1)
  val score2 = evaluateHand(hand2).id :: tiebreaker(hand2)

  lexCompare(score1, score2)
}

@tailrec
def lexCompare(tie1: List[Int], tie2: List[Int]): Boolean = {
  if (tie1.head > tie2.head)
    return true
  else if (tie2.head > tie1.head)
    return false

  lexCompare(tie1.tail, tie2.tail)
}

import scala.io.Source
val lines = Source.fromFile("poker.txt").getLines
val games = lines map getHands
games.count(winner)
