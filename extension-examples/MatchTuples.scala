object Tuple {
  def maybeNeg(v: (Int, Boolean)): (Int, Boolean) = {// Type
    v match {
      case (i, false) =>// pattern
        (i, false)// literal
      case (i, true) =>
        (-i, false)
    }
  }
}

