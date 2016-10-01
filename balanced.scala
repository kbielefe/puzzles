def balanced(input: String): Boolean = {
  val balance = input.foldLeft[Option[Int]](Some(0)){
      case (    Some(0), ')') => None
      case (Some(count), '(') => Some(count + 1)
      case (Some(count), ')') => Some(count - 1)
      case (Some(count),  _ ) => Some(count)
      case (None,         _ ) => None
  }
  balance == Some(0)
}

println(balanced("Are (these) ((parentheses) (balanced))?"))
