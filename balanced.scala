def balanced(input: String): Boolean = {
  val balances = input.scanLeft(0){(count, char) => char match {
      case '(' => count + 1
      case ')' => count - 1
      case  _  => count
    }
  }
  balances.last == 0 && !balances.contains(-1)
}

println(balanced("Are (these) ((parentheses) (balanced))?"))
