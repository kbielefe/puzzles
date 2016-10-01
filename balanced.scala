def balanced(input: String): Boolean = {
  val balances = input.scanLeft(0){
      case (count, '(') => count + 1
      case (count, ')') => count - 1
      case (count,  _ ) => count
  }
  balances.last == 0 && !balances.contains(-1)
}

println(balanced("Are (these) ((parentheses) (balanced))?"))
